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
# Romano and Wolf (2005, Econometrica; 2016 practitioner note) give a
# step-down procedure that controls the family-wise error rate while
# exploiting the joint dependence, via a bootstrap that resamples the whole
# vector of test statistics together. Where BH answers "what share of my
# rejections are false", RW answers the stricter "what is the chance I made
# ANY false rejection", but does so with far more power than Bonferroni or
# Holm because the bootstrap knows the tests are correlated.
#
# ALGORITHM
# ---------
#   1. Compute the observed t-statistic for each of the M hypotheses.
#   2. Bootstrap B times by resampling PLAYERS with replacement (preserving
#      within-player dependence), recentring each statistic on its observed
#      value to impose the null, and storing the full M-vector of t-stats.
#   3. Order hypotheses by |t| descending. For the j-th hypothesis in that
#      order, the step-down p-value is the share of bootstrap draws whose
#      maximum |t*| over the REMAINING (not yet rejected) hypotheses exceeds
#      |t_j|. Enforce monotonicity so p-values are non-decreasing down the
#      order.
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
# Dependencies: dplyr, fixest, here
# ==============================================================================

set.seed(20260416)

library(dplyr)
library(fixest)
library(here)

source(here("scripts", "R", "firstll", "firstll_helpers.R"))
summary_log <- character()

B_RW <- 999L
PRIMARY_OUTCOMES <- c("points_change", "elo_change", "n_main_draws",
                      "n_matches_250plus")


# ==============================================================================
# LOAD
# ==============================================================================
gs  <- ensure_scaled(readRDS(file.path(FIRSTLL_CLEANED, "firstll_gs_est_v2.rds")))
ngs <- ensure_scaled(readRDS(file.path(FIRSTLL_CLEANED, "firstll_nongs_est_v2.rds")))


# ==============================================================================
# STEP 1: BUILD THE HYPOTHESIS LIST AND OBSERVED t-STATISTICS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 1: OBSERVED TEST STATISTICS")
message(strrep("=", 70))

# Each "spec" is one (design, tour, outcome) regression yielding 5 horizon
# coefficients. We store the fitted frame so the bootstrap can resample it.
specs <- list()
for (design in c("GS", "NonGS")) {
  for (tour_label in c("ATP", "WTA")) {
    base <- if (design == "GS") gs[gs$tour == tour_label, ]
            else                ngs[ngs$tour == tour_label, ]
    if (nrow(base) < 30) next
    fe <- if (design == "GS") "slam_year + horizon" else "tourney_id + horizon"
    cf <- if (design == "GS") NULL else "v_hat"
    st <- stack_horizons_full(base)
    for (ob in PRIMARY_OUTCOMES) {
      if (!(ob %in% names(st))) next
      d <- st
      d$.y <- d[[ob]]
      keep <- !is.na(d$.y) & !is.na(d$pre_rank_pts_s) & !is.na(d$player_age)
      if (!is.null(cf)) keep <- keep & !is.na(d$v_hat)
      d <- d[keep, ]
      if (nrow(d) < 50) next
      rhs <- "got_ll:horizon"
      if (!is.null(cf)) rhs <- paste0(rhs, " + v_hat:horizon")
      rhs <- paste0(rhs, " + ", ZPRE_FIRSTLL)
      specs[[paste(design, tour_label, ob, sep = "|")]] <- list(
        design = design, tour = tour_label, outcome = ob,
        data = d, fml = as.formula(paste0(".y ~ ", rhs, " | ", fe)))
    }
  }
}
message("  Built ", length(specs), " specifications")

#' Fit one spec and return the named vector of horizon t-statistics.
fit_tstats <- function(sp, dat = NULL) {
  d <- if (is.null(dat)) sp$data else dat
  m <- tryCatch(feols(sp$fml, data = d, cluster = ~player_id),
                error = function(e) NULL)
  if (is.null(m)) return(NULL)
  ct <- as.data.frame(coeftable(m))
  out <- setNames(rep(NA_real_, length(HORIZON_LABS)), HORIZON_LABS)
  est <- setNames(rep(NA_real_, length(HORIZON_LABS)), HORIZON_LABS)
  pv  <- setNames(rep(NA_real_, length(HORIZON_LABS)), HORIZON_LABS)
  for (h in HORIZON_LABS) {
    tn <- paste0("got_ll:horizon", h)
    if (tn %in% rownames(ct)) {
      out[h] <- ct[tn, "Estimate"] / ct[tn, "Std. Error"]
      est[h] <- ct[tn, "Estimate"]
      pv[h]  <- ct[tn, "Pr(>|t|)"]
    }
  }
  list(t = out, coef = est, p = pv)
}

obs_rows <- list()
for (nm in names(specs)) {
  sp <- specs[[nm]]
  r <- fit_tstats(sp)
  if (is.null(r)) next
  for (h in HORIZON_LABS) {
    if (is.na(r$t[h])) next
    obs_rows[[paste(nm, h, sep = "|")]] <- data.frame(
      key = paste(nm, h, sep = "|"),
      design = sp$design, tour = sp$tour, outcome = sp$outcome, horizon = h,
      coef = r$coef[h], tstat = r$t[h], p_raw = r$p[h],
      stringsAsFactors = FALSE)
  }
}
obs_df <- do.call(rbind, obs_rows)
rownames(obs_df) <- NULL
M <- nrow(obs_df)
message("  ", M, " hypotheses in the family")


# ==============================================================================
# STEP 2: BOOTSTRAP THE JOINT NULL DISTRIBUTION
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 2: CLUSTER BOOTSTRAP (", B_RW, " replications)")
message(strrep("=", 70))

# Resample players with replacement WITHIN each spec, recentre on the observed
# statistic to impose the null, and record the whole vector jointly.
boot_t <- matrix(NA_real_, nrow = B_RW, ncol = M,
                 dimnames = list(NULL, obs_df$key))

# JOINT RESAMPLING.
#
# The whole point of Romano-Wolf is that the bootstrap preserves the dependence
# among the test statistics, so the max-t null distribution reflects how
# correlated the hypotheses actually are. That requires ONE resample per
# replication, applied to every specification, so that boot_t[b, ] is a
# jointly drawn vector.
#
# An earlier version drew the resample inside the specification loop, giving
# each of the 16 specifications an independent draw. The resulting max-t
# distribution was that of independent statistics, which is far more dispersed
# than the truth, so every adjusted p-value was inflated. The error was
# detectable from the output alone: Romano-Wolf dominates Holm by
# construction, yet the procedure returned p_RW = 0.410 for a hypothesis whose
# Holm bound is at most 80 x 0.002 = 0.16.
#
# Players are resampled from the union across specifications so that a player
# appearing in several samples is drawn or omitted consistently.
all_players <- unique(unlist(lapply(specs, function(s) unique(s$data$player_id))))

# Pre-compute row indices per player per spec once, rather than scanning the
# player column inside the replication loop.
spec_idx <- lapply(specs, function(s) split(seq_len(nrow(s$data)), s$data$player_id))

for (b in seq_len(B_RW)) {
  if (b %% 100 == 0) message("    replication ", b, " / ", B_RW)
  drawn_global <- sample(all_players, length(all_players), replace = TRUE)
  for (nm in names(specs)) {
    sp <- specs[[nm]]
    idx_map <- spec_idx[[nm]]
    # Restrict the global draw to players present in this specification,
    # preserving multiplicity so the same player drawn twice contributes twice.
    drawn <- drawn_global[as.character(drawn_global) %in% names(idx_map)]
    if (length(drawn) < 2) next
    # Rebuild the resampled frame, re-labelling clusters so repeated draws of
    # the same player are treated as distinct clusters.
    idx <- unlist(lapply(seq_along(drawn), function(i) {
      which(sp$data$player_id == drawn[i])
    }))
    if (length(idx) < 50) next
    dstar <- sp$data[idx, ]
    dstar$player_id <- unlist(lapply(seq_along(drawn), function(i) {
      rep(paste0(drawn[i], "_", i), sum(sp$data$player_id == drawn[i]))
    }))
    r <- fit_tstats(sp, dstar)
    if (is.null(r)) next
    for (h in HORIZON_LABS) {
      k <- paste(nm, h, sep = "|")
      if (!(k %in% colnames(boot_t))) next
      if (is.na(r$t[h])) next
      # Recentre: impose the null by subtracting the observed t.
      boot_t[b, k] <- r$t[h] - obs_df$tstat[obs_df$key == k]
    }
  }
}


# ==============================================================================
# STEP 3: STEP-DOWN
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 3: ROMANO-WOLF STEP-DOWN")
message(strrep("=", 70))

ord <- order(abs(obs_df$tstat), decreasing = TRUE)
obs_ord <- obs_df[ord, ]
p_rw <- rep(NA_real_, M)
remaining <- obs_ord$key

for (j in seq_len(M)) {
  k <- obs_ord$key[j]
  cols <- intersect(remaining, colnames(boot_t))
  if (length(cols) == 0) { p_rw[j] <- NA_real_; next }
  sub <- boot_t[, cols, drop = FALSE]
  maxt <- apply(abs(sub), 1, function(z) if (all(is.na(z))) NA_real_ else max(z, na.rm = TRUE))
  maxt <- maxt[is.finite(maxt)]
  if (length(maxt) < 50) { p_rw[j] <- NA_real_; next }
  p_rw[j] <- mean(maxt >= abs(obs_ord$tstat[j]))
  remaining <- setdiff(remaining, k)
}
# Enforce monotonicity down the step-down order.
p_rw <- cummax(ifelse(is.na(p_rw), 0, p_rw))
obs_ord$p_rw <- p_rw

# Re-merge back onto the original ordering and attach BH for comparison.
rw_df <- obs_ord
rw_df$p_bh <- p.adjust(rw_df$p_raw, method = "BH")
rw_df <- rw_df[order(rw_df$design, rw_df$tour, rw_df$outcome, rw_df$horizon), ]

slog("Romano-Wolf vs Benjamini-Hochberg (family of ", M, " tests):")
n_raw <- sum(rw_df$p_raw < 0.05, na.rm = TRUE)
n_bh  <- sum(rw_df$p_bh  < 0.05, na.rm = TRUE)
n_rw  <- sum(rw_df$p_rw  < 0.05, na.rm = TRUE)
slog(sprintf("  Significant at 5%%: raw=%d, BH q<0.05=%d, RW p<0.05=%d",
             n_raw, n_bh, n_rw))

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
