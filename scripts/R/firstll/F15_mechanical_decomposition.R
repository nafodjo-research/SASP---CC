# ==============================================================================
# F15_mechanical_decomposition.R
# Mechanical decomposition of the first-break claim.
#
# The paper reports that first-time LL effects (n=0) are roughly twice the
# pooled effect. A skeptical reader might ask: is the gap a real "first break"
# excess, or just an artifact of averaging over strata with different effects?
#
# Under the law of iterated expectations, if strata are independent and
# treatment effects do not interact with stratum covariates,
#     beta_pool = w_0 * beta_{n=0} + w_1 * beta_{n=1} + w_{2+} * beta_{n>=2}
# where the weights are the treated-share of each stratum in the pool.
#
# This script computes:
#   1. Stratum weights (share of pooled treated observations in each stratum)
#   2. Stratum-specific effects beta_n from F09 (already estimated)
#   3. Weighted average beta_pool_hat = sum(w_n * beta_n)
#   4. Actual pooled beta from a full-sample regression
#   5. Gap: pooled - weighted average
#
# A small gap means the pooled coefficient is a mechanical stratum average.
# A large gap indicates model misspecification or effect heterogeneity.
# Either way, the fact that beta_{n=0} substantially exceeds beta_pool
# and both beta_{n=1} and beta_{n>=2} fall below it is the load-bearing
# empirical claim; the decomposition documents it explicitly.
#
# Inputs:
#   Data/cleaned/skeleton_gs_est_v5.rds
#   Data/cleaned/skeleton_nongs_est_v8.rds
#   Data/cleaned/firstll/marginal_impact_results.rds  (from F09)
#
# Outputs:
#   Tables_FirstLL/table_mechanical_decomposition.tex
#   Data/cleaned/firstll/mechanical_decomposition.rds
#   Output_FirstLL/F15_decomposition_summary.md
#
# Dependencies: dplyr, fixest, here
# ==============================================================================

set.seed(20260416)

library(dplyr)
library(fixest)
library(here)

source(here("scripts", "R", "firstll", "firstll_helpers.R"))
summary_log <- character()


# ==============================================================================
# STEP 1: LOAD DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 1: LOAD DATA")
message(strrep("=", 70))

gs_full  <- readRDS(file.path(CLEANED_DIR, "skeleton_gs_est_v5.rds"))
ngs_full <- readRDS(file.path(CLEANED_DIR, "skeleton_nongs_est_v8.rds"))
gs_full  <- ensure_scaled(gs_full)
ngs_full <- ensure_scaled(ngs_full)

gs_full$n_prior_ll  <- gs_full$n_prior_gs_ll_won + gs_full$n_prior_nongs_ll_won
ngs_full$n_prior_ll <- ngs_full$n_prior_gs_ll_won + ngs_full$n_prior_nongs_ll_won

gs_full$exp_group  <- ifelse(gs_full$n_prior_ll >= 2, "2+",
                             as.character(gs_full$n_prior_ll))
ngs_full$exp_group <- ifelse(ngs_full$n_prior_ll >= 2, "2+",
                             as.character(ngs_full$n_prior_ll))

# Marginal impact estimates (stratum-specific betas from F09)
marg <- readRDS(file.path(FIRSTLL_CLEANED, "marginal_impact_results.rds"))

message("  GS full sample: ", nrow(gs_full), " (ATP: ",
        sum(gs_full$tour == "ATP"), ", WTA: ", sum(gs_full$tour == "WTA"), ")")
message("  NonGS full sample: ", nrow(ngs_full))
message("  F09 marginal results: GS=", nrow(marg$gs), ", NonGS=", nrow(marg$ngs))


# ==============================================================================
# STEP 2: STRATUM WEIGHTS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 2: STRATUM WEIGHTS")
message(strrep("=", 70))

compute_weights <- function(data, tour_label) {
  sub <- data[data$tour == tour_label & data$got_ll == 1, ]
  n_total <- nrow(sub)
  if (n_total == 0) return(NULL)
  w <- table(sub$exp_group) / n_total
  data.frame(
    tour      = tour_label,
    exp_group = names(w),
    w         = as.numeric(w),
    n_treated = as.numeric(table(sub$exp_group)),
    stringsAsFactors = FALSE
  )
}

weights_all <- rbind(
  cbind(design = "GS",    compute_weights(gs_full,  "ATP")),
  cbind(design = "GS",    compute_weights(gs_full,  "WTA")),
  cbind(design = "NonGS", compute_weights(ngs_full, "ATP")),
  cbind(design = "NonGS", compute_weights(ngs_full, "WTA"))
)

slog("Treated-share weights by stratum:")
for (i in seq_len(nrow(weights_all))) {
  r <- weights_all[i, ]
  slog(sprintf("  %s %s n=%s: w=%.3f (%d treated)",
               r$design, r$tour, r$exp_group, r$w, r$n_treated))
}


# ==============================================================================
# STEP 3: POOLED ESTIMATES (no stratum split)
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 3: POOLED ESTIMATES")
message(strrep("=", 70))

run_pool <- function(data, fe_str, cf_term, tour_label) {
  sub <- data[data$tour == tour_label, ]
  st <- stack_horizons_full(sub)
  out <- list()
  for (ob in c("points_change", "elo_change", "n_main_draws", "n_matches_250plus")) {
    sdata <- st[!is.na(st[[ob]]) & !is.na(st$pre_rank_pts_s) &
                !is.na(st$player_age), ]
    if (!is.null(cf_term)) sdata <- sdata[!is.na(sdata$v_hat), ]
    if (nrow(sdata) < 50) next
    rhs <- "got_ll:horizon"
    if (!is.null(cf_term)) rhs <- paste0(rhs, " + v_hat:horizon")
    rhs <- paste0(rhs, " + ", ZPRE_FULL)
    fml <- as.formula(paste0(ob, " ~ ", rhs, " | ", fe_str))
    mod <- tryCatch(feols(fml, data = sdata, cluster = ~player_id),
                    error = function(e) NULL)
    if (is.null(mod)) next
    ct <- as.data.frame(coeftable(mod)); ct$var <- rownames(ct)
    for (h in HORIZON_LABS) {
      tn <- paste0("got_ll:horizon", h)
      if (tn %in% ct$var) {
        rt <- ct[ct$var == tn, ]
        out[[paste0(tour_label, "_", ob, "_", h)]] <- data.frame(
          tour = tour_label, outcome = ob, horizon = h,
          coef_pool = rt$Estimate, se_pool = rt[["Std. Error"]],
          pval_pool = rt[["Pr(>|t|)"]],
          stringsAsFactors = FALSE)
      }
    }
  }
  do.call(rbind, out)
}

pool_gs_atp <- run_pool(gs_full,  "slam_year + horizon", NULL,   "ATP")
pool_gs_wta <- run_pool(gs_full,  "slam_year + horizon", NULL,   "WTA")
pool_ngs_atp <- run_pool(ngs_full, "tourney_id + horizon", "v_hat", "ATP")
pool_ngs_wta <- run_pool(ngs_full, "tourney_id + horizon", "v_hat", "WTA")

pool_all <- rbind(
  cbind(design = "GS",    pool_gs_atp),
  cbind(design = "GS",    pool_gs_wta),
  cbind(design = "NonGS", pool_ngs_atp),
  cbind(design = "NonGS", pool_ngs_wta)
)


# ==============================================================================
# STEP 4: WEIGHTED AVERAGE OF STRATUM EFFECTS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 4: WEIGHTED-AVERAGE DECOMPOSITION")
message(strrep("=", 70))

# Combine marginal estimates
marg_all <- rbind(
  cbind(design = "GS",    marg$gs),
  cbind(design = "NonGS", marg$ngs)
)

# For each (design, tour, outcome, horizon), compute weighted average
decomp_rows <- list()
for (des in c("GS", "NonGS")) {
  for (t in c("ATP", "WTA")) {
    w <- weights_all[weights_all$design == des & weights_all$tour == t, ]
    if (nrow(w) == 0) next
    for (ob in c("points_change", "elo_change", "n_main_draws", "n_matches_250plus")) {
      for (h in HORIZON_LABS) {
        est <- marg_all[marg_all$design == des & marg_all$tour == t &
                        marg_all$outcome == ob & marg_all$horizon == h, ]
        if (nrow(est) == 0) next

        # Merge weights onto stratum estimates
        est <- merge(est, w[, c("exp_group", "w")], by = "exp_group", all.x = TRUE)
        est$w[is.na(est$w)] <- 0

        # Delta-method SE for weighted sum: sqrt(sum(w^2 * se^2))
        # (assumes stratum estimates are independent; conservative but standard)
        wavg <- sum(est$w * est$coef)
        wavg_se <- sqrt(sum(est$w^2 * est$se^2))

        # Get pooled estimate for comparison
        p <- pool_all[pool_all$design == des & pool_all$tour == t &
                      pool_all$outcome == ob & pool_all$horizon == h, ]
        coef_pool <- if (nrow(p)) p$coef_pool[1] else NA_real_
        se_pool   <- if (nrow(p)) p$se_pool[1]   else NA_real_

        # First-break excess: beta_{n=0} - weighted average
        b0_row <- est[est$exp_group == "0", ]
        beta_0 <- if (nrow(b0_row)) b0_row$coef[1] else NA_real_
        se_0   <- if (nrow(b0_row)) b0_row$se[1]   else NA_real_
        excess <- beta_0 - wavg
        # Ratio (only meaningful when denominators are non-trivial)
        ratio  <- if (isTRUE(abs(coef_pool) > 1e-6)) beta_0 / coef_pool else NA_real_

        decomp_rows[[paste(des, t, ob, h, sep = "_")]] <- data.frame(
          design = des, tour = t, outcome = ob, horizon = h,
          beta_pool = coef_pool, se_pool = se_pool,
          wavg = wavg, wavg_se = wavg_se,
          gap = coef_pool - wavg,
          beta_0 = beta_0, se_0 = se_0,
          excess = excess, ratio_0_pool = ratio,
          stringsAsFactors = FALSE
        )
      }
    }
  }
}
decomp <- do.call(rbind, decomp_rows)

slog("\nDecomposition (points_change only, illustrative):")
sub <- decomp[decomp$outcome == "points_change", ]
for (i in seq_len(nrow(sub))) {
  r <- sub[i, ]
  slog(sprintf("  %s-%s %s: pool=%.1f  wavg=%.1f  gap=%.1f  b0=%.1f  ratio=%.2f",
               r$design, r$tour, r$horizon,
               r$beta_pool, r$wavg, r$gap, r$beta_0, r$ratio_0_pool))
}


# ==============================================================================
# STEP 5: GENERATE TABLE
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 5: TABLE")
message(strrep("=", 70))

# Focus the table on ranking points (the headline outcome) across horizons.
# Rows: {beta_pool, weighted_average, gap, beta_n0, ratio}
# Columns: horizons
# Panels: {GS-ATP, GS-WTA, NonGS-ATP, NonGS-WTA}

make_decomp_table <- function(dfr, focal_outcome = "points_change") {
  d <- dfr[dfr$outcome == focal_outcome, ]
  L <- c("\\begin{tabular}{l*{5}{c}}", "\\toprule",
         paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"),
         "\\midrule")

  panels <- expand.grid(design = c("GS", "NonGS"),
                        tour = c("ATP", "WTA"),
                        stringsAsFactors = FALSE)
  first <- TRUE
  for (i in seq_len(nrow(panels))) {
    des <- panels$design[i]; t <- panels$tour[i]
    sub <- d[d$design == des & d$tour == t, ]
    if (nrow(sub) == 0) next
    sub <- sub[match(HORIZON_LABS, sub$horizon), ]

    if (!first) L <- c(L, "\\\\[0.3em]")
    first <- FALSE
    label <- paste0(des, "-", t)
    L <- c(L, paste0("\\multicolumn{6}{l}{\\textit{Panel: ",
                     label, "}} \\\\", " \\midrule"))

    pool_v <- vapply(sub$beta_pool, function(x) if (is.na(x)) "" else fmt(x, 1),
                     character(1))
    wavg_v <- vapply(sub$wavg,      function(x) if (is.na(x)) "" else fmt(x, 1),
                     character(1))
    gap_v  <- vapply(sub$gap,       function(x) if (is.na(x)) "" else fmt(x, 1),
                     character(1))
    b0_v   <- vapply(sub$beta_0,    function(x) if (is.na(x)) "" else fmt(x, 1),
                     character(1))
    rat_v  <- vapply(sub$ratio_0_pool, function(x) if (is.na(x)) "" else fmt(x, 2),
                     character(1))

    L <- c(L,
      paste0("$\\hat{\\beta}^{\\text{pool}}$ & ", paste(pool_v, collapse = " & "), " \\\\"),
      paste0("Weighted avg.\\ $\\sum_n w_n \\hat{\\beta}^{(n)}$ & ",
             paste(wavg_v, collapse = " & "), " \\\\"),
      paste0("Gap (pool $-$ wavg) & ", paste(gap_v, collapse = " & "), " \\\\"),
      paste0("$\\hat{\\beta}^{(n=0)}$ (first LL) & ", paste(b0_v, collapse = " & "), " \\\\"),
      paste0("$\\hat{\\beta}^{(n=0)}/\\hat{\\beta}^{\\text{pool}}$ & ",
             paste(rat_v, collapse = " & "), " \\\\")
    )
  }
  L <- c(L, "\\bottomrule", "\\end{tabular}")
  paste(L, collapse = "\n")
}

writeLines(make_decomp_table(decomp),
           file.path(FIRSTLL_TABLES, "table_mechanical_decomposition.tex"))
message("  Table saved: Tables_FirstLL/table_mechanical_decomposition.tex")

# Save the raw decomposition for cross-referencing in text.
saveRDS(list(weights = weights_all, pool = pool_all, decomp = decomp),
        file.path(FIRSTLL_CLEANED, "mechanical_decomposition.rds"))


# ==============================================================================
# STEP 6: SUMMARY
# ==============================================================================
slog("\n", strrep("=", 70))
slog("MECHANICAL DECOMPOSITION SUMMARY")
slog(strrep("=", 70))
slog("Interpretation:")
slog("  beta_pool  = pooled coefficient on full sample")
slog("  wavg       = w_0*beta_{n=0} + w_1*beta_{n=1} + w_{2+}*beta_{n>=2}")
slog("  gap        = pool - wavg (small: pool is stratum-weighted mean)")
slog("  beta_{n=0} = first-time-only coefficient")
slog("  ratio      = beta_{n=0} / beta_pool (paper claim: ~2x)")

writeLines(summary_log, file.path(FIRSTLL_OUTPUT, "F15_decomposition_summary.md"))
message("\nSummary saved to Output_FirstLL/F15_decomposition_summary.md")
message("DONE.")
