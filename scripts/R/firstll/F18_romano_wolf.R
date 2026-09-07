# ==============================================================================
# F18_romano_wolf.R
# Romano-Wolf step-down multiple-testing correction.
#
# WHY THIS EXISTS
# ---------------
# F11 applies a Benjamini-Hochberg correction across the 80-test family
# (4 outcomes x 4 samples x 5 horizons). BH controls the false discovery rate
# and is valid under positive regression dependency, but it ignores the
# correlation structure entirely and is therefore conservative here: a player's
# ranking-point change at 4, 8 and 12 weeks are strongly autocorrelated, and
# ranking points and Elo move together. Treating those as independent tests
# throws away information.
#
# Romano and Wolf (2005, Econometrica) give a step-down procedure that controls
# the family-wise error rate while exploiting the joint dependence, via a
# bootstrap that resamples the whole vector of test statistics together. Where
# BH answers "what share of my rejections are false", RW answers the stricter
# "what is the chance I made ANY false rejection", but does so with far more
# power than Bonferroni or Holm because the bootstrap knows the tests are
# correlated.
#
# IMPLEMENTATION
# --------------
# The procedure is executed by wildrwolf::rwolf(), the reference R
# implementation of Clarke, Romano and Wolf (2019, Stata Journal), which runs
# a null-imposed wild cluster bootstrap through fwildclusterboot and applies
# the step-down max-t correction across models.
#
# An earlier hand-rolled version of this script drew an independent cluster
# resample inside the specification loop, so the max-t null distribution was
# that of independent statistics rather than the correlated truth, and every
# adjusted p-value was inflated. The error was visible in the output alone:
# Romano-Wolf dominates Holm by construction, yet it returned p_RW = 0.410 for
# a hypothesis whose Holm bound is at most 80 x 0.002 = 0.16. Delegating to a
# tested package removes that class of error.
#
# rwolf() tests ONE named parameter across a list of models, so each of the
# (design x tour x outcome x horizon) hypotheses is estimated as its own
# regression in which the focal horizon interaction is renamed `treat` and the
# remaining four horizon interactions are carried as separate regressors. That
# reparameterisation is algebraically identical to the stacked `got_ll:horizon`
# specification used elsewhere in the paper: the coefficient, standard error
# and raw p-value on `treat` reproduce the corresponding cell of the headline
# table exactly.
#
# Inputs:
#   Data/cleaned/firstll/firstll_gs_est_v2.rds
#   Data/cleaned/firstll/firstll_nongs_est_v2.rds
#
# Outputs:
#   Tables_FirstLL/table_romano_wolf.tex
#   Data/cleaned/firstll/romano_wolf.rds
#   Output_FirstLL/F18_romano_wolf_summary.md
#
# Dependencies: dplyr, fixest, fwildclusterboot, wildrwolf, here
# ==============================================================================

set.seed(20260416)

library(dplyr)
library(fixest)
library(fwildclusterboot)
library(wildrwolf)
library(here)

source(here("scripts", "R", "firstll", "firstll_helpers.R"))
summary_log <- character()

B_RW <- 9999L
PRIMARY_OUTCOMES <- c("points_change", "elo_change", "n_main_draws",
                      "n_matches_250plus")

#' Drop the rows fixest excludes, iterating until the estimation sample is
#' stable, and return the cleaned frame together with the fitted model.
#'
#' fixest silently removes fixed-effect singletons and rows with missing values
#' in covariates the caller did not filter on. boottest() and therefore rwolf()
#' refuse to run on a model whose fixed effects were altered this way, which is
#' the correct behaviour: the bootstrap dgp must be built on exactly the rows
#' the model was fitted to.
fit_stable <- function(fml, d, max_iter = 10L) {
  for (i in seq_len(max_iter)) {
    m <- tryCatch(feols(fml, data = d, cluster = ~player_id),
                  error = function(e) NULL)
    if (is.null(m)) return(NULL)
    used <- obs(m)
    if (length(used) == nrow(d)) return(list(model = m, data = d))
    d <- d[used, , drop = FALSE]
  }
  NULL
}


# ==============================================================================
# LOAD
# ==============================================================================
gs  <- ensure_scaled(readRDS(file.path(FIRSTLL_CLEANED, "firstll_gs_est_v2.rds")))
ngs <- ensure_scaled(readRDS(file.path(FIRSTLL_CLEANED, "firstll_nongs_est_v2.rds")))


# ==============================================================================
# STEP 1: BUILD ONE MODEL PER HYPOTHESIS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 1: FITTING ONE MODEL PER HYPOTHESIS")
message(strrep("=", 70))

h_tag <- function(h) paste0("tr_", gsub("[^A-Za-z0-9]", "", h))

models   <- list()
meta_rows <- list()

for (design in c("GS", "NonGS")) {
  for (tour_label in c("ATP", "WTA")) {
    base <- if (design == "GS") gs[gs$tour == tour_label, ]
            else                ngs[ngs$tour == tour_label, ]
    if (nrow(base) < 30) next
    fe <- if (design == "GS") "slam_year + horizon" else "tourney_id + horizon"
    use_cf <- (design != "GS")
    st <- stack_horizons_full(base)

    # Explicit horizon-treatment dummies. tr_<h> equals got_ll for rows at
    # horizon h and zero elsewhere, so the set of five reproduces the
    # got_ll:horizon interaction basis exactly.
    for (h in HORIZON_LABS) {
      st[[h_tag(h)]] <- as.numeric(st$got_ll) * as.numeric(st$horizon == h)
    }

    for (ob in PRIMARY_OUTCOMES) {
      if (!(ob %in% names(st))) next
      d0 <- st
      d0$.y <- d0[[ob]]
      keep <- !is.na(d0$.y) & !is.na(d0$pre_rank_pts_s) & !is.na(d0$player_age)
      if (use_cf) keep <- keep & !is.na(d0$v_hat)
      d0 <- d0[keep, ]
      if (nrow(d0) < 50) next

      for (h in HORIZON_LABS) {
        d <- d0
        d$treat <- d[[h_tag(h)]]
        others  <- setdiff(vapply(HORIZON_LABS, h_tag, ""), h_tag(h))
        rhs <- paste(c("treat", others), collapse = " + ")
        if (use_cf) rhs <- paste0(rhs, " + v_hat:horizon")
        rhs <- paste0(rhs, " + ", ZPRE_FIRSTLL)
        fml <- as.formula(paste0(".y ~ ", rhs, " | ", fe))

        fit <- fit_stable(fml, d)
        if (is.null(fit)) next
        m  <- fit$model
        ct <- as.data.frame(coeftable(m))
        if (!("treat" %in% rownames(ct))) next

        key <- paste(design, tour_label, ob, h, sep = "|")
        models[[key]] <- m
        meta_rows[[key]] <- data.frame(
          key = key, design = design, tour = tour_label,
          outcome = ob, horizon = h,
          coef  = ct["treat", "Estimate"],
          se    = ct["treat", "Std. Error"],
          tstat = ct["treat", "Estimate"] / ct["treat", "Std. Error"],
          p_raw = ct["treat", "Pr(>|t|)"],
          n_obs = nobs(m),
          stringsAsFactors = FALSE)
      }
    }
  }
}

meta_df <- do.call(rbind, meta_rows)
rownames(meta_df) <- NULL
M <- nrow(meta_df)
message("  ", M, " hypotheses in the family")


# ==============================================================================
# STEP 2: ROMANO-WOLF STEP-DOWN
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 2: ROMANO-WOLF (wildrwolf, B = ", B_RW, ", Webb weights)")
message(strrep("=", 70))

# wildrwolf 0.7.0 ships an argument check that has fallen out of step with
# fwildclusterboot 0.14.3: rwolf() accepts only p_val_type = "two_sided" but
# passes the string unchanged to boottest(), which accepts only "two-tailed".
# Either spelling therefore fails, one at the outer check and one at the inner
# call. Disabling rwolf's own argument validation (and nothing else) lets the
# spelling boottest wants through. Every argument below is supplied literally
# here, so no validation is lost.
rwolf_run <- wildrwolf::rwolf
local_env <- new.env(parent = environment(wildrwolf::rwolf))
local_env$check_arg <- function(...) invisible(NULL)
environment(rwolf_run) <- local_env

rw <- rwolf_run(models = models, param = "treat", B = B_RW,
                p_val_type = "two-tailed", weights_type = "webb",
                nthreads = 1)

# rwolf returns rows in the order the models were supplied.
stopifnot(nrow(rw) == M)
rw_df <- meta_df
rw_df$p_rw <- as.numeric(rw[["RW Pr(>|t|)"]])
rw_df$p_bh <- p.adjust(rw_df$p_raw, method = "BH")

# Sanity check against the Holm bound, which Romano-Wolf must not exceed by
# more than bootstrap noise. This is the diagnostic that caught the earlier
# hand-rolled implementation.
p_holm <- p.adjust(rw_df$p_raw, method = "holm")
viol <- which(rw_df$p_rw > p_holm + 0.02)
if (length(viol) > 0) {
  warning("Romano-Wolf p exceeds the Holm bound for ", length(viol),
          " hypotheses; inspect before citing.")
}

rw_df <- rw_df[order(rw_df$design, rw_df$tour, rw_df$outcome, rw_df$horizon), ]

slog("Romano-Wolf vs Benjamini-Hochberg (family of ", M, " tests):")
n_raw <- sum(rw_df$p_raw < 0.05, na.rm = TRUE)
n_bh  <- sum(rw_df$p_bh  < 0.05, na.rm = TRUE)
n_rw  <- sum(rw_df$p_rw  < 0.05, na.rm = TRUE)
slog(sprintf("  Significant at 5%%: raw=%d, BH q<0.05=%d, RW p<0.05=%d",
             n_raw, n_bh, n_rw))
slog(sprintf("  Max |p_RW - p_Holm| violation: %d hypotheses", length(viol)))

slog("\nTests surviving Romano-Wolf at 5%:")
surv <- rw_df[!is.na(rw_df$p_rw) & rw_df$p_rw < 0.05, ]
if (nrow(surv) == 0) slog("  (none)")
for (i in seq_len(nrow(surv))) {
  r <- surv[i, ]
  slog(sprintf("  %-6s %-4s %-18s %-4s: coef=%8.2f  p_raw=%.3f  p_BH=%.3f  p_RW=%.3f",
               r$design, r$tour, r$outcome, r$horizon, r$coef,
               r$p_raw, r$p_bh, r$p_rw))
}

slog("\nFull family:")
for (i in seq_len(nrow(rw_df))) {
  r <- rw_df[i, ]
  slog(sprintf("  %-6s %-4s %-18s %-4s: coef=%8.2f  p_raw=%.3f  p_BH=%.3f  p_RW=%.3f",
               r$design, r$tour, r$outcome, r$horizon, r$coef,
               r$p_raw, r$p_bh, r$p_rw))
}


# ==============================================================================
# TABLE
# ==============================================================================
oc_label <- function(oc) switch(oc,
  "points_change"     = "Ranking points",
  "elo_change"        = "Elo",
  "n_main_draws"      = "Main draws",
  "n_matches_250plus" = "Matches 250+", oc)

L <- c("\\begin{tabular}{ll*{5}{c}}", "\\toprule",
       paste0("Sample & & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"),
       "\\midrule")
first <- TRUE
for (oc in PRIMARY_OUTCOMES) {
  sub_oc <- rw_df[rw_df$outcome == oc, ]
  if (nrow(sub_oc) == 0) next
  if (!first) L <- c(L, "\\\\[0.3em]")
  first <- FALSE
  L <- c(L, paste0("\\multicolumn{7}{l}{\\textit{Panel: ", oc_label(oc),
                   "}} \\\\", " \\midrule"))
  for (des in c("GS", "NonGS")) {
    for (t in c("ATP", "WTA")) {
      sub <- sub_oc[sub_oc$design == des & sub_oc$tour == t, ]
      if (nrow(sub) == 0) next
      cv <- bv <- rv <- character()
      for (h in HORIZON_LABS) {
        r <- sub[sub$horizon == h, ]
        if (nrow(r) == 0) { cv <- c(cv, ""); bv <- c(bv, ""); rv <- c(rv, "") }
        else {
          cv <- c(cv, paste0(fmt(r$coef[1], 1), add_stars(r$p_raw[1])))
          bv <- c(bv, fmt(r$p_bh[1], 3))
          rv <- c(rv, fmt(r$p_rw[1], 3))
        }
      }
      L <- c(L,
        paste0(des, "-", t, " & $\\hat{\\beta}_h$ & ", paste(cv, collapse = " & "), " \\\\"),
        paste0(" & $q$ (BH) & ", paste(bv, collapse = " & "), " \\\\"),
        paste0(" & $p$ (RW) & ", paste(rv, collapse = " & "), " \\\\[0.2em]"))
    }
  }
}
L <- c(L, "\\bottomrule", "\\end{tabular}")
writeLines(paste(L, collapse = "\n"),
           file.path(FIRSTLL_TABLES, "table_romano_wolf.tex"))
message("  Wrote table_romano_wolf.tex")

saveRDS(rw_df, file.path(FIRSTLL_CLEANED, "romano_wolf.rds"))
writeLines(summary_log, file.path(FIRSTLL_OUTPUT, "F18_romano_wolf_summary.md"))
message("\nSummary saved to Output_FirstLL/F18_romano_wolf_summary.md")
message("DONE.")
