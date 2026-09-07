# ==============================================================================
# F16_smallsample_inference.R
# Small-sample inference upgrades demanded by the R4 methods referee.
#
# The Grand Slam samples are small (N = 120 / 82 pooled; N = 49 / 31 on the
# corrected verified-lottery subsample) and the number of player clusters is
# smaller still. Asymptotic cluster-robust SEs over-reject in that regime.
# This script provides:
#
#   PART A. Cluster counts for every sample used in the paper, so the reader
#           can judge whether asymptotic clustered inference is credible.
#   PART B. Wild cluster bootstrap (WCR) p-values with the null imposed and
#           Webb 6-point weights, for the verified-lottery subsamples and the
#           marginal-impact GS strata.
#   PART C. Minimum detectable effects (MDE) at 80% power for each sample, so
#           null results can be labelled "informative" or "underpowered"
#           rather than left ambiguous.
#
# WILD CLUSTER BOOTSTRAP DETAIL
# -----------------------------
# Following Cameron, Gelbach and Miller (2008) and the recommendations in
# Roodman et al. (2019), we use the restricted (null-imposed) variant, WCR:
#   1. Fit the restricted model with the coefficient of interest set to zero
#      (i.e. omit the treatment term) and store fitted values + residuals.
#   2. For bootstrap replication b, draw one weight per CLUSTER from the Webb
#      6-point distribution {+/-sqrt(1.5), +/-1, +/-sqrt(0.5)}, each w.p. 1/6.
#      Webb weights outperform Rademacher when the cluster count is below ~12,
#      because Rademacher can only generate 2^G distinct draws.
#   3. Build y*_ig = fitted_restricted_ig + w_g * resid_restricted_ig.
#   4. Refit the UNRESTRICTED model on y* and record the cluster-robust
#      t-statistic on the treatment term.
#   5. The symmetric two-sided p-value is the share of |t*| >= |t_observed|.
#
# Inputs:
#   Data/cleaned/firstll/firstll_gs_est_v2.rds
#   Data/cleaned/firstll/firstll_nongs_est_v2.rds
#   Data/cleaned/firstll/ll_lottery_classification.rds
#   Data/cleaned/skeleton_gs_est_v5.rds
#
# Outputs:
#   Tables_FirstLL/table_cluster_counts.tex
#   Tables_FirstLL/table_wildbootstrap_verified.tex
#   Tables_FirstLL/table_mde.tex
#   Data/cleaned/firstll/smallsample_inference.rds
#   Output_FirstLL/F16_smallsample_summary.md
#
# Dependencies: dplyr, fixest, here
# ==============================================================================

set.seed(20260416)

library(dplyr)
library(fixest)
library(fwildclusterboot)
library(here)

source(here("scripts", "R", "firstll", "firstll_helpers.R"))
summary_log <- character()

# 9,999 replications. fwildclusterboot is compiled and handles this in seconds,
# so there is no reason to economise the way a hand-rolled loop had to.
N_BOOT <- 9999L

#' Drop the rows fixest excludes, iterating until the estimation sample is
#' stable, and return the cleaned frame together with the fitted model.
#'
#' fixest silently removes fixed-effect singletons and rows with missing values
#' in covariates the caller did not filter on. boottest() refuses to run on a
#' model whose fixed effects were altered this way, which is the correct
#' behaviour: an earlier hand-rolled bootstrap in this project did NOT check,
#' recycled a shorter fitted/residual vector against a full-length weight
#' vector, and produced p-values below both the asymptotic and the exact
#' randomization p-value. Pre-cleaning here means the model handed to boottest
#' is estimated on exactly the rows it thinks it is.
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
classif <- readRDS(file.path(FIRSTLL_CLEANED, "ll_lottery_classification.rds"))

gs$event_key      <- paste(gs$tour, gs$tourney_id, gs$year, sep = "|")
classif$event_key <- paste(classif$tour, classif$tourney_id, classif$year, sep = "|")

verified_events <- unique(classif$event_key[classif$classification == "lottery_verified"])
gs_ver <- gs[gs$event_key %in% verified_events, ]

message("Loaded. Verified events: ", length(verified_events))


# ==============================================================================
# PART A: CLUSTER COUNTS
# ==============================================================================
message("\n", strrep("=", 70))
message("PART A: CLUSTER COUNTS")
message(strrep("=", 70))

cluster_rows <- list()
add_cluster_row <- function(label, d) {
  if (nrow(d) == 0) return(invisible(NULL))
  cluster_rows[[label]] <<- data.frame(
    sample     = label,
    n_obs      = nrow(d),
    n_treated  = sum(d$got_ll == 1),
    n_control  = sum(d$got_ll == 0),
    n_players  = length(unique(d$player_id)),
    n_events   = length(unique(paste(d$tour, d$tourney_id, d$year))),
    stringsAsFactors = FALSE
  )
}

add_cluster_row("GS-ATP (pooled first-LL)",   gs[gs$tour == "ATP", ])
add_cluster_row("GS-WTA (pooled first-LL)",   gs[gs$tour == "WTA", ])
add_cluster_row("GS-ATP (verified lottery)",  gs_ver[gs_ver$tour == "ATP", ])
add_cluster_row("GS-WTA (verified lottery)",  gs_ver[gs_ver$tour == "WTA", ])
add_cluster_row("NonGS-ATP (first-LL)",       ngs[ngs$tour == "ATP", ])
add_cluster_row("NonGS-WTA (first-LL)",       ngs[ngs$tour == "WTA", ])

cluster_df <- do.call(rbind, cluster_rows)

slog("Cluster counts by sample:")
for (i in seq_len(nrow(cluster_df))) {
  r <- cluster_df[i, ]
  slog(sprintf("  %-28s N=%5d (T=%4d, C=%4d)  players=%4d  events=%3d",
               r$sample, r$n_obs, r$n_treated, r$n_control, r$n_players, r$n_events))
}

# Flag samples where asymptotic clustered inference is questionable.
slog("\nAsymptotic-inference warnings (rule of thumb: <40 clusters marginal, <20 unreliable):")
for (i in seq_len(nrow(cluster_df))) {
  r <- cluster_df[i, ]
  if (r$n_players < 40) {
    lvl <- if (r$n_players < 20) "UNRELIABLE" else "MARGINAL"
    slog(sprintf("  %-28s %d player clusters -> %s; use wild bootstrap",
                 r$sample, r$n_players, lvl))
  }
}

L <- c("\\begin{tabular}{lrrrrr}", "\\toprule",
       "Sample & $N$ & Treated & Control & Players & Events \\\\", "\\midrule")
for (i in seq_len(nrow(cluster_df))) {
  r <- cluster_df[i, ]
  L <- c(L, sprintf("%s & %d & %d & %d & %d & %d \\\\",
                    r$sample, r$n_obs, r$n_treated, r$n_control,
                    r$n_players, r$n_events))
}
L <- c(L, "\\bottomrule", "\\end{tabular}")
writeLines(paste(L, collapse = "\n"),
           file.path(FIRSTLL_TABLES, "table_cluster_counts.tex"))
message("  Wrote table_cluster_counts.tex")


# ==============================================================================
# PART B: WILD CLUSTER BOOTSTRAP
# ==============================================================================
message("\n", strrep("=", 70))
message("PART B: WILD CLUSTER BOOTSTRAP (WCR, Webb weights)")
message(strrep("=", 70))

# Webb six-point weights and the restricted (null-imposed) bootstrap are both
# supplied by fwildclusterboot::boottest(), which is the reference
# implementation of Roodman, Nielsen, MacKinnon and Webb (2019). This replaces
# a hand-rolled loop that had two defects: it did not verify that the fitted
# and residual vectors matched the data frame it was resampling (see
# fit_stable above), and it used the naive share estimator rather than the
# Davidson-MacKinnon (1 + k)/(B + 1) p-value, so it reported 0.000 at a
# replication count where the attainable floor is 1e-4.

#' Wild cluster bootstrap p-value for one horizon-specific treatment coefficient.
#' Returns the asymptotic and bootstrap p-values side by side, plus the
#' bootstrap confidence interval and the cluster count.
wcr_pvalue <- function(data, outcome, horizon, fe_str, cf_term = NULL,
                       n_boot = N_BOOT) {
  d <- data
  d$.y <- d[[outcome]]
  keep <- !is.na(d$.y) & !is.na(d$pre_rank_pts_s) & !is.na(d$player_age)
  if (!is.null(cf_term)) keep <- keep & !is.na(d$v_hat)
  d <- d[keep, ]
  if (nrow(d) < 30) return(NULL)

  target <- paste0("got_ll:horizon", horizon)

  rhs <- "got_ll:horizon"
  if (!is.null(cf_term)) rhs <- paste0(rhs, " + v_hat:horizon")
  rhs <- paste0(rhs, " + ", ZPRE_FIRSTLL)
  fml <- as.formula(paste0(".y ~ ", rhs, " | ", fe_str))

  fit <- fit_stable(fml, d)
  if (is.null(fit)) return(NULL)
  mod <- fit$model
  d   <- fit$data

  ct <- as.data.frame(coeftable(mod))
  if (!(target %in% rownames(ct))) return(NULL)
  b_obs  <- ct[target, "Estimate"]
  se_obs <- ct[target, "Std. Error"]
  p_asy  <- ct[target, "Pr(>|t|)"]

  bt <- tryCatch(
    boottest(mod, param = target, clustid = "player_id",
             B = n_boot, type = "webb", p_val_type = "two-tailed"),
    error = function(e) { message("      boottest failed: ",
                                  conditionMessage(e)); NULL })
  if (is.null(bt)) return(NULL)

  ci <- tryCatch(confint(bt), error = function(e) c(NA_real_, NA_real_))

  list(coef = b_obs, se = se_obs, p_asy = p_asy,
       p_wcr = fwildclusterboot::pval(bt),
       ci_lo = ci[1], ci_hi = ci[2],
       n_clusters = length(unique(d$player_id)), n_boot = n_boot)
}
# Run WCR on the verified-lottery subsample for the two primary outcomes.
wcr_rows <- list()
for (t in c("ATP", "WTA")) {
  sub <- gs_ver[gs_ver$tour == t, ]
  if (nrow(sub) < 30) next
  st <- stack_horizons_full(sub)
  for (ob in c("points_change", "elo_change")) {
    for (h in HORIZON_LABS) {
      message(sprintf("  WCR: verified %s %s %s ...", t, ob, h))
      r <- wcr_pvalue(st, ob, h, "slam_year + horizon", NULL)
      if (is.null(r)) next
      wcr_rows[[paste("verified", t, ob, h, sep = "_")]] <- data.frame(
        sample = paste0("Verified-", t), outcome = ob, horizon = h,
        coef = r$coef, se = r$se, p_asy = r$p_asy, p_wcr = r$p_wcr,
        n_clusters = r$n_clusters, stringsAsFactors = FALSE)
    }
  }
}

# Run WCR on the pooled first-LL GS sample as the comparison benchmark.
for (t in c("ATP", "WTA")) {
  sub <- gs[gs$tour == t, ]
  st <- stack_horizons_full(sub)
  for (ob in c("points_change", "elo_change")) {
    for (h in HORIZON_LABS) {
      message(sprintf("  WCR: pooled %s %s %s ...", t, ob, h))
      r <- wcr_pvalue(st, ob, h, "slam_year + horizon", NULL)
      if (is.null(r)) next
      wcr_rows[[paste("pooled", t, ob, h, sep = "_")]] <- data.frame(
        sample = paste0("Pooled-", t), outcome = ob, horizon = h,
        coef = r$coef, se = r$se, p_asy = r$p_asy, p_wcr = r$p_wcr,
        n_clusters = r$n_clusters, stringsAsFactors = FALSE)
    }
  }
}

wcr_df <- do.call(rbind, wcr_rows)

slog("\nWild cluster bootstrap results (asymptotic vs WCR p-values):")
for (i in seq_len(nrow(wcr_df))) {
  r <- wcr_df[i, ]
  flag <- if (!is.na(r$p_asy) && !is.na(r$p_wcr) &&
              r$p_asy < 0.05 && r$p_wcr >= 0.05) "  <-- LOSES SIGNIFICANCE" else ""
  slog(sprintf("  %-14s %-14s %-4s coef=%8.2f  p_asy=%.3f  p_wcr=%.3f  (G=%d)%s",
               r$sample, r$outcome, r$horizon, r$coef, r$p_asy, r$p_wcr,
               r$n_clusters, flag))
}

make_wcr_table <- function(dfr, focal_outcome) {
  d <- dfr[dfr$outcome == focal_outcome, ]
  L <- c("\\begin{tabular}{ll*{5}{c}}", "\\toprule",
         paste0("Sample & & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"),
         "\\midrule")
  for (s in unique(d$sample)) {
    sub <- d[d$sample == s, ]
    cv <- pa <- pw <- character()
    for (h in HORIZON_LABS) {
      r <- sub[sub$horizon == h, ]
      if (nrow(r) == 0) { cv <- c(cv, ""); pa <- c(pa, ""); pw <- c(pw, "") }
      else {
        cv <- c(cv, fmt(r$coef[1], 1))
        pa <- c(pa, fmt(r$p_asy[1], 3))
        pw <- c(pw, fmt(r$p_wcr[1], 3))
      }
    }
    L <- c(L,
      paste0(s, " & $\\hat{\\beta}_h$ & ", paste(cv, collapse = " & "), " \\\\"),
      paste0(" & $p$ (clustered) & ", paste(pa, collapse = " & "), " \\\\"),
      paste0(" & $p$ (wild boot.) & ", paste(pw, collapse = " & "), " \\\\[0.3em]"))
  }
  L <- c(L, "\\bottomrule", "\\end{tabular}")
  paste(L, collapse = "\n")
}

writeLines(make_wcr_table(wcr_df, "points_change"),
           file.path(FIRSTLL_TABLES, "table_wildbootstrap_verified.tex"))
writeLines(make_wcr_table(wcr_df, "elo_change"),
           file.path(FIRSTLL_TABLES, "table_wildbootstrap_elo.tex"))
message("  Wrote table_wildbootstrap_verified.tex and table_wildbootstrap_elo.tex")


# ==============================================================================
# PART C: MINIMUM DETECTABLE EFFECTS
# ==============================================================================
message("\n", strrep("=", 70))
message("PART C: MINIMUM DETECTABLE EFFECTS")
message(strrep("=", 70))

# MDE at 80% power, 5% two-sided test: MDE = (z_{0.975} + z_{0.80}) * SE
# = 2.802 * SE, using the realised clustered SE from the fitted model.
MDE_MULT <- qnorm(0.975) + qnorm(0.80)

mde_rows <- list()
compute_mde <- function(d, label, outcome, fe_str) {
  st <- stack_horizons_full(d)
  for (h in HORIZON_LABS) {
    sd2 <- st[!is.na(st[[outcome]]) & !is.na(st$pre_rank_pts_s) &
              !is.na(st$player_age), ]
    if (nrow(sd2) < 30) next
    sd2$.y <- sd2[[outcome]]
    fml <- as.formula(paste0(".y ~ got_ll:horizon + ", ZPRE_FIRSTLL, " | ", fe_str))
    m <- tryCatch(feols(fml, data = sd2, cluster = ~player_id),
                  error = function(e) NULL)
    if (is.null(m)) next
    ct <- as.data.frame(coeftable(m))
    tn <- paste0("got_ll:horizon", h)
    if (!(tn %in% rownames(ct))) next
    se <- ct[tn, "Std. Error"]
    mde_rows[[paste(label, outcome, h, sep = "_")]] <<- data.frame(
      sample = label, outcome = outcome, horizon = h,
      se = se, mde = MDE_MULT * se,
      coef = ct[tn, "Estimate"],
      stringsAsFactors = FALSE)
  }
}

for (t in c("ATP", "WTA")) {
  compute_mde(gs[gs$tour == t, ],         paste0("Pooled-", t),   "points_change", "slam_year + horizon")
  compute_mde(gs_ver[gs_ver$tour == t, ], paste0("Verified-", t), "points_change", "slam_year + horizon")
  compute_mde(gs[gs$tour == t, ],         paste0("Pooled-", t),   "elo_change",    "slam_year + horizon")
  compute_mde(gs_ver[gs_ver$tour == t, ], paste0("Verified-", t), "elo_change",    "slam_year + horizon")
}
mde_df <- do.call(rbind, mde_rows)

slog("\nMinimum detectable effects (80% power, 5% two-sided):")
for (i in seq_len(nrow(mde_df))) {
  r <- mde_df[i, ]
  slog(sprintf("  %-14s %-14s %-4s  SE=%7.2f  MDE=%7.1f  (observed coef=%7.1f)",
               r$sample, r$outcome, r$horizon, r$se, r$mde, r$coef))
}

# The key comparison: is the verified-sample MDE larger than the pooled-sample
# point estimate? If yes, a null on the verified sample is uninformative.
slog("\nInformativeness of verified-sample nulls (ranking points):")
for (t in c("ATP", "WTA")) {
  for (h in HORIZON_LABS) {
    pv <- mde_df[mde_df$sample == paste0("Pooled-", t) &
                 mde_df$outcome == "points_change" & mde_df$horizon == h, ]
    vv <- mde_df[mde_df$sample == paste0("Verified-", t) &
                 mde_df$outcome == "points_change" & mde_df$horizon == h, ]
    if (nrow(pv) == 0 || nrow(vv) == 0) next
    verdict <- if (vv$mde[1] > abs(pv$coef[1]))
      "UNDERPOWERED (MDE exceeds pooled estimate; null uninformative)"
    else
      "INFORMATIVE (MDE below pooled estimate; null is evidence)"
    slog(sprintf("  %s %-4s: pooled coef=%6.1f, verified MDE=%6.1f -> %s",
                 t, h, pv$coef[1], vv$mde[1], verdict))
  }
}

L <- c("\\begin{tabular}{ll*{5}{c}}", "\\toprule",
       paste0("Sample & & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"),
       "\\midrule")
for (s in unique(mde_df$sample)) {
  sub <- mde_df[mde_df$sample == s & mde_df$outcome == "points_change", ]
  if (nrow(sub) == 0) next
  cv <- mv <- character()
  for (h in HORIZON_LABS) {
    r <- sub[sub$horizon == h, ]
    if (nrow(r) == 0) { cv <- c(cv, ""); mv <- c(mv, "") }
    else { cv <- c(cv, fmt(r$coef[1], 1)); mv <- c(mv, fmt(r$mde[1], 1)) }
  }
  L <- c(L,
    paste0(s, " & $\\hat{\\beta}_h$ & ", paste(cv, collapse = " & "), " \\\\"),
    paste0(" & MDE (80\\%) & ", paste(mv, collapse = " & "), " \\\\[0.3em]"))
}
L <- c(L, "\\bottomrule", "\\end{tabular}")
writeLines(paste(L, collapse = "\n"), file.path(FIRSTLL_TABLES, "table_mde.tex"))
message("  Wrote table_mde.tex")


# ==============================================================================
# SAVE
# ==============================================================================
saveRDS(list(clusters = cluster_df, wcr = wcr_df, mde = mde_df),
        file.path(FIRSTLL_CLEANED, "smallsample_inference.rds"))
writeLines(summary_log, file.path(FIRSTLL_OUTPUT, "F16_smallsample_summary.md"))
message("\nSummary saved to Output_FirstLL/F16_smallsample_summary.md")
message("DONE.")
