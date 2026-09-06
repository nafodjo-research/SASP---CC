# ==============================================================================
# F12_r2_fixes.R
# Address Round 2 referee blocking issues:
#   FIX A: CF validation table — correct rho extraction (horizon:v_hat order)
#   FIX B: Marginal impact — drop collinear prior-LL controls within strata
#   FIX C: Prize money back-of-envelope calculation
#
# Inputs:
#   Data/cleaned/firstll/firstll_gs_est_v2.rds
#   Data/cleaned/firstll/firstll_nongs_est_v2.rds
#   Data/cleaned/skeleton_gs_est_v5.rds (full for marginal)
#   Data/cleaned/skeleton_nongs_est_v8.rds (full for marginal)
#
# Outputs:
#   Tables_FirstLL/table_cf_validation.tex (replaces buggy version)
#   Tables_FirstLL/table_marginal_impact_gs.tex (replaces buggy version)
#   Tables_FirstLL/table_marginal_impact_nongs.tex (replaces buggy version)
#   Tables_FirstLL/table_prize_money.tex (new)
#   Output_FirstLL/F12_r2_fixes_summary.md
# ==============================================================================

set.seed(20260416)

library(dplyr)
library(fixest)
library(here)

source(here("scripts", "R", "firstll", "firstll_helpers.R"))
summary_log <- character()


# ==============================================================================
# FIX A: CF VALIDATION — correct rho extraction
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX A: CF VALIDATION (corrected rho extraction)")
message(strrep("=", 70))

gs  <- readRDS(file.path(FIRSTLL_CLEANED, "firstll_gs_est_v2.rds"))
gs  <- ensure_scaled(gs)
gs$n_pool <- ave(rep(1, nrow(gs)), gs$tourney_id, FUN = length)
gs$p_uniform <- pmax(pmin(gs$n_ll_slots / gs$n_pool, 0.999), 0.001)
gs$v_hat_gs <- compute_gen_residual(gs$got_ll, gs$p_uniform)

slog("GS uniform P_i: mean=", round(mean(gs$p_uniform), 3),
     " range=[", round(min(gs$p_uniform), 3), ",", round(max(gs$p_uniform), 3), "]")

cf_results <- list()
for (tour_label in c("ATP", "WTA")) {
  sub <- gs[gs$tour == tour_label, ]
  st <- stack_horizons_full(sub)

  for (ob in c("points_change", "elo_change", "n_main_draws")) {
    sdata <- st[!is.na(st[[ob]]) & !is.na(st$pre_rank_pts_s) &
                !is.na(st$player_age), ]
    if (nrow(sdata) < 50) next

    fml1 <- as.formula(paste0(ob, " ~ got_ll:horizon + ", ZPRE_FIRSTLL,
                              " | slam_year + horizon"))
    mod1 <- tryCatch(feols(fml1, data = sdata, cluster = ~player_id),
                     error = function(e) NULL)

    fml2 <- as.formula(paste0(ob, " ~ got_ll:horizon + v_hat_gs:horizon + ",
                              ZPRE_FIRSTLL, " | slam_year + horizon"))
    mod2 <- tryCatch(feols(fml2, data = sdata, cluster = ~player_id),
                     error = function(e) NULL)

    if (!is.null(mod1) && !is.null(mod2)) {
      ct1 <- as.data.frame(coeftable(mod1)); ct1$var <- rownames(ct1)
      ct2 <- as.data.frame(coeftable(mod2)); ct2$var <- rownames(ct2)

      for (h in HORIZON_LABS) {
        tn <- paste0("got_ll:horizon", h)
        # CORRECT rho name pattern: horizon{h}:v_hat_gs
        rn <- paste0("horizon", h, ":v_hat_gs")

        r1 <- ct1[ct1$var == tn, ]
        r2 <- ct2[ct2$var == tn, ]
        rr <- ct2[ct2$var == rn, ]

        if (nrow(r1) > 0 && nrow(r2) > 0) {
          cf_results[[paste0(tour_label, "_", ob, "_", h)]] <- data.frame(
            tour = tour_label, outcome = ob, horizon = h,
            beta_no_cf = r1$Estimate, se_no_cf = r1[["Std. Error"]], p_no_cf = r1[["Pr(>|t|)"]],
            beta_cf = r2$Estimate, se_cf = r2[["Std. Error"]], p_cf = r2[["Pr(>|t|)"]],
            rho = if (nrow(rr) > 0) rr$Estimate else NA_real_,
            rho_se = if (nrow(rr) > 0) rr[["Std. Error"]] else NA_real_,
            rho_p = if (nrow(rr) > 0) rr[["Pr(>|t|)"]] else NA_real_,
            stringsAsFactors = FALSE)
        }
      }
    }
  }
}
cf_df <- do.call(rbind, cf_results)

slog("\nCF validation rho values (should be near zero under randomization):")
for (i in seq_len(nrow(cf_df))) {
  r <- cf_df[i, ]
  if (!is.na(r$rho)) {
    slog(sprintf("  %s %s %s: rho=%.3f (SE=%.3f, p=%.3f) | beta: %.1f -> %.1f",
                 r$tour, r$outcome, r$horizon, r$rho, r$rho_se, r$rho_p,
                 r$beta_no_cf, r$beta_cf))
  }
}

# Generate CF validation table WITH rho row populated
make_cf_table <- function(cf_df) {
  L <- c("\\begin{tabular}{l*{5}{c}}", "\\toprule",
         paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"), "\\midrule")

  for (t in c("ATP", "WTA")) {
    L <- c(L, paste0("\\multicolumn{6}{l}{\\textit{", t, "}} \\\\"))
    for (ob in c("points_change", "elo_change", "n_main_draws")) {
      sub <- cf_df[cf_df$tour == t & cf_df$outcome == ob, ]
      if (nrow(sub) == 0) next
      d <- if (ob == "n_main_draws") 2 else 1

      # No CF row
      cv1 <- sv1 <- character()
      for (h in HORIZON_LABS) {
        r <- sub[sub$horizon == h, ]
        if (nrow(r) == 0) { cv1 <- c(cv1, ""); sv1 <- c(sv1, "") }
        else {
          cv1 <- c(cv1, paste0(fmt(r$beta_no_cf, d), add_stars(r$p_no_cf)))
          sv1 <- c(sv1, paste0("(", fmt(r$se_no_cf, d), ")"))
        }
      }
      L <- c(L, paste0("  ", outcome_labels[ob], " & ", paste(cv1, collapse = " & "), " \\\\"))
      L <- c(L, paste0("  & ", paste(sv1, collapse = " & "), " \\\\"))

      # With CF row
      cv2 <- sv2 <- character()
      for (h in HORIZON_LABS) {
        r <- sub[sub$horizon == h, ]
        if (nrow(r) == 0) { cv2 <- c(cv2, ""); sv2 <- c(sv2, "") }
        else {
          cv2 <- c(cv2, paste0(fmt(r$beta_cf, d), add_stars(r$p_cf)))
          sv2 <- c(sv2, paste0("(", fmt(r$se_cf, d), ")"))
        }
      }
      L <- c(L, paste0("  + CF & ", paste(cv2, collapse = " & "), " \\\\"))
      L <- c(L, paste0("  & ", paste(sv2, collapse = " & "), " \\\\"))

      # Rho row (now populated)
      rv <- rsev <- character()
      for (h in HORIZON_LABS) {
        r <- sub[sub$horizon == h, ]
        if (nrow(r) == 0 || is.na(r$rho)) { rv <- c(rv, ""); rsev <- c(rsev, "") }
        else {
          rv <- c(rv, paste0(fmt(r$rho, 2), add_stars(r$rho_p)))
          rsev <- c(rsev, paste0("(", fmt(r$rho_se, 2), ")"))
        }
      }
      L <- c(L, paste0("  $\\hat{\\rho}_h$ & ", paste(rv, collapse = " & "), " \\\\"))
      L <- c(L, paste0("  & ", paste(rsev, collapse = " & "), " \\\\[0.3em]"))
    }
    L <- c(L, "\\midrule")
  }
  L <- c(L, "\\bottomrule", "\\end{tabular}")
  paste(L, collapse = "\n")
}

writeLines(make_cf_table(cf_df), file.path(FIRSTLL_TABLES, "table_cf_validation.tex"))
message("  CF validation table saved (rho row now populated).")


# ==============================================================================
# FIX B: MARGINAL IMPACT — drop collinear prior-LL controls
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX B: MARGINAL IMPACT (drop prior-LL controls within strata)")
message(strrep("=", 70))

gs_full  <- readRDS(file.path(CLEANED_DIR, "skeleton_gs_est_v5.rds"))
ngs_full <- readRDS(file.path(CLEANED_DIR, "skeleton_nongs_est_v8.rds"))

gs_full  <- ensure_scaled(gs_full)
ngs_full <- ensure_scaled(ngs_full)

gs_full$n_prior_ll  <- gs_full$n_prior_gs_ll_won + gs_full$n_prior_nongs_ll_won
ngs_full$n_prior_ll <- ngs_full$n_prior_gs_ll_won + ngs_full$n_prior_nongs_ll_won

gs_full$exp_group <- ifelse(gs_full$n_prior_ll >= 2, "2+",
                            as.character(gs_full$n_prior_ll))
ngs_full$exp_group <- ifelse(ngs_full$n_prior_ll >= 2, "2+",
                             as.character(ngs_full$n_prior_ll))

# Dose merge
dose_full <- readRDS(file.path(CLEANED_DIR, "performance_dose.rds"))
gs_full  <- merge_dose(gs_full, dose_full, "GS")
ngs_full <- merge_dose(ngs_full, dose_full, "nonGS")

# Use ZPRE_FIRSTLL (no prior-LL count variables) — these are collinear within strata
run_marginal_fixed <- function(data, fe_str, cf_term, tour_label) {
  all_results <- list()

  for (eg in c("0", "1", "2+")) {
    sub <- data[data$exp_group == eg & data$tour == tour_label, ]
    n_treated <- sum(sub$got_ll == 1)
    n_control <- sum(sub$got_ll == 0)

    slog("  ", tour_label, " n=", eg, ": N=", nrow(sub),
         " (T=", n_treated, ", C=", n_control, ")")

    if (n_treated < 10 || n_control < 10) {
      slog("    Skipping: too few observations")
      next
    }

    st <- stack_horizons_full(sub)

    for (ob in c("points_change", "elo_change", "n_main_draws", "n_matches_250plus")) {
      sdata <- st[!is.na(st[[ob]]) &
                  !is.na(st$pre_rank_pts_s) &
                  !is.na(st$player_age), ]
      if (!is.null(cf_term)) sdata <- sdata[!is.na(sdata$v_hat), ]
      if (nrow(sdata) < 50) next

      rhs <- "got_ll:horizon"
      if (!is.null(cf_term)) rhs <- paste0(rhs, " + v_hat:horizon")
      # USE ZPRE_FIRSTLL — drops prior-LL controls that are collinear within stratum
      rhs <- paste0(rhs, " + ", ZPRE_FIRSTLL)
      fml <- as.formula(paste0(ob, " ~ ", rhs, " | ", fe_str))

      mod <- tryCatch(feols(fml, data = sdata, cluster = ~player_id),
                      error = function(e) NULL)
      if (is.null(mod)) next

      ct <- as.data.frame(coeftable(mod)); ct$var <- rownames(ct)
      for (h in HORIZON_LABS) {
        tn <- paste0("got_ll:horizon", h)
        if (tn %in% ct$var) {
          rt <- ct[ct$var == tn, ]
          all_results[[paste0(tour_label, "_", eg, "_", ob, "_", h)]] <- data.frame(
            tour = tour_label, exp_group = eg, outcome = ob, horizon = h,
            coef = rt$Estimate, se = rt[["Std. Error"]],
            pval = rt[["Pr(>|t|)"]], n = nobs(mod),
            stringsAsFactors = FALSE)
        }
      }
    }
  }
  do.call(rbind, all_results)
}

gs_atp_res <- run_marginal_fixed(gs_full, "slam_year + horizon", NULL, "ATP")
gs_wta_res <- run_marginal_fixed(gs_full, "slam_year + horizon", NULL, "WTA")
gs_res <- rbind(gs_atp_res, gs_wta_res)

ngs_atp_res <- run_marginal_fixed(ngs_full, "tourney_id + horizon", "v_hat", "ATP")
ngs_wta_res <- run_marginal_fixed(ngs_full, "tourney_id + horizon", "v_hat", "WTA")
ngs_res <- rbind(ngs_atp_res, ngs_wta_res)

slog("\nMarginal impact summary (should have economically sensible magnitudes):")
for (r in list(GS = gs_res, NonGS = ngs_res)) {
  if (is.null(r) || nrow(r) == 0) next
  for (eg in c("0", "1", "2+")) {
    for (t in c("ATP", "WTA")) {
      sub <- r[r$exp_group == eg & r$tour == t & r$outcome == "points_change", ]
      if (nrow(sub) == 0) next
      vals <- paste0(sprintf("%.1f", sub$coef), " (", sprintf("%.1f", sub$se), ")")
      slog(sprintf("  %s n=%s pts: %s", t, eg, paste(vals, collapse = " | ")))
    }
  }
}

# Save results
saveRDS(list(gs = gs_res, ngs = ngs_res),
        file.path(FIRSTLL_CLEANED, "marginal_impact_results_fixed.rds"))

# Generate tables
make_marginal_table <- function(res) {
  if (is.null(res) || nrow(res) == 0) return("% No marginal impact results")
  tours <- unique(res$tour)
  groups <- c("0", "1", "2+")
  oos <- c("points_change", "elo_change", "n_main_draws", "n_matches_250plus")

  L <- c("\\begin{tabular}{l l*{5}{c}}", "\\toprule",
         paste0("Tour & Experience & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"),
         "\\midrule")

  for (t in tours) {
    for (eg in groups) {
      sub <- res[res$tour == t & res$exp_group == eg, ]
      if (nrow(sub) == 0) next

      eg_label <- switch(eg,
        "0"  = "First LL ($n=0$)",
        "1"  = "Second LL ($n=1$)",
        "2+" = "Third+ LL ($n \\geq 2$)")

      first_outcome <- TRUE
      for (ob in oos) {
        osub <- sub[sub$outcome == ob, ]
        if (nrow(osub) == 0) next

        cv <- sv <- character()
        for (h in HORIZON_LABS) {
          r <- osub[osub$horizon == h, ]
          if (nrow(r) == 0) { cv <- c(cv, ""); sv <- c(sv, "") }
          else {
            cv <- c(cv, paste0(fmt(r$coef, 1), add_stars(r$pval)))
            sv <- c(sv, paste0("(", fmt(r$se, 1), ")"))
          }
        }
        prefix <- if (first_outcome) paste0(t, " & ", eg_label) else "  & "
        first_outcome <- FALSE

        L <- c(L, paste0(prefix, " & ", paste(cv, collapse = " & "), " \\\\"))
        L <- c(L, paste0("  &  & ", paste(sv, collapse = " & "), " \\\\"))
      }
      L <- c(L, "[0.3em]")
    }
    if (t != tours[length(tours)]) L <- c(L, "\\midrule")
  }
  L <- c(L, "\\bottomrule", "\\end{tabular}")
  paste(L, collapse = "\n")
}

writeLines(make_marginal_table(gs_res),
           file.path(FIRSTLL_TABLES, "table_marginal_impact_gs.tex"))
writeLines(make_marginal_table(ngs_res),
           file.path(FIRSTLL_TABLES, "table_marginal_impact_nongs.tex"))
message("  Marginal impact tables saved (without collinear controls).")


# ==============================================================================
# FIX C: PRIZE MONEY BACK-OF-ENVELOPE
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX C: PRIZE MONEY ANALYSIS")
message(strrep("=", 70))

# 2024 prize money (USD) — rounded figures from public tournament data
# Grand Slam prize money (USD, averaged across 4 slams)
prize_gs_atp <- list(
  qualifying_final = 40000,  # stipend for final qualifying round losers
  first_round = 100000,       # main draw first round (won 0 matches)
  second_round = 160000,      # won 1 match
  third_round = 240000,       # won 2 matches
  fourth_round = 400000       # won 3 matches
)
prize_gs_wta <- prize_gs_atp  # equal prize money at Grand Slams since 2007

# Non-GS: Masters 1000 / ATP 500 / ATP 250 averaged
# Approximate first-round prize: M1000 ~25K, ATP 500 ~15K, ATP 250 ~8K
# Weighted by sample composition
prize_ngs_atp_first_round <- 12000
prize_ngs_wta_first_round <- 9000
prize_ngs_qual_stipend <- 3000

# Load samples and compute expected prize money differential
gs  <- readRDS(file.path(FIRSTLL_CLEANED, "firstll_gs_est_v2.rds"))
ngs <- readRDS(file.path(FIRSTLL_CLEANED, "firstll_nongs_est_v2.rds"))

# Immediate event prize: treated earn based on main draw performance; controls earn qualifying stipend
# Use md_matches_won and md_any_win from skeletons
compute_immediate_prize <- function(df, prizes, qual_stipend) {
  df$prize_immediate <- NA_real_
  # Controls get qualifying stipend
  df$prize_immediate[df$got_ll == 0] <- qual_stipend
  # Treated: based on matches won in main draw
  treated <- df$got_ll == 1
  if ("md_matches_won" %in% names(df)) {
    df$prize_immediate[treated] <- with(df[treated, ], ifelse(
      md_matches_won == 0, prizes$first_round,
      ifelse(md_matches_won == 1, prizes$second_round,
      ifelse(md_matches_won == 2, prizes$third_round, prizes$fourth_round))))
  }
  df
}

gs_atp_p <- compute_immediate_prize(gs[gs$tour=="ATP",], prize_gs_atp, prize_gs_atp$qualifying_final)
gs_wta_p <- compute_immediate_prize(gs[gs$tour=="WTA",], prize_gs_wta, prize_gs_wta$qualifying_final)

# Compute differential
compute_prize_effect <- function(df, label) {
  if (!"prize_immediate" %in% names(df) || all(is.na(df$prize_immediate))) return(NULL)
  treated_mean <- mean(df$prize_immediate[df$got_ll == 1], na.rm = TRUE)
  control_mean <- mean(df$prize_immediate[df$got_ll == 0], na.rm = TRUE)
  diff <- treated_mean - control_mean
  # Simple t-test
  tt <- tryCatch(t.test(df$prize_immediate[df$got_ll == 1],
                        df$prize_immediate[df$got_ll == 0]),
                 error = function(e) NULL)
  data.frame(
    sample = label,
    n_treat = sum(df$got_ll == 1), n_ctrl = sum(df$got_ll == 0),
    mean_treated = treated_mean, mean_control = control_mean,
    diff = diff,
    se = if (!is.null(tt)) diff(tt$conf.int)/3.92 else NA,
    pval = if (!is.null(tt)) tt$p.value else NA,
    stringsAsFactors = FALSE)
}

prize_atp <- compute_prize_effect(gs_atp_p, "GS-ATP")
prize_wta <- compute_prize_effect(gs_wta_p, "GS-WTA")

# Non-GS: treated earn first-round prize, controls earn qualifying stipend
ngs_atp_p <- ngs[ngs$tour=="ATP",]
ngs_atp_p$prize_immediate <- ifelse(ngs_atp_p$got_ll == 1,
                                     prize_ngs_atp_first_round, prize_ngs_qual_stipend)
ngs_wta_p <- ngs[ngs$tour=="WTA",]
ngs_wta_p$prize_immediate <- ifelse(ngs_wta_p$got_ll == 1,
                                     prize_ngs_wta_first_round, prize_ngs_qual_stipend)

prize_ngs_atp <- compute_prize_effect(ngs_atp_p, "NonGS-ATP")
prize_ngs_wta <- compute_prize_effect(ngs_wta_p, "NonGS-WTA")

prize_df <- do.call(rbind, list(prize_atp, prize_wta, prize_ngs_atp, prize_ngs_wta))

slog("\nImmediate prize money differential (USD):")
for (i in seq_len(nrow(prize_df))) {
  r <- prize_df[i, ]
  slog(sprintf("  %s: treated=%.0f ctrl=%.0f diff=%.0f p=%.3g (N_T=%d N_C=%d)",
               r$sample, r$mean_treated, r$mean_control, r$diff, r$pval,
               r$n_treat, r$n_ctrl))
}

# Dynamic prize money: use estimated effect on n_main_draws_26w multiplied by average first-round prize
# This is the "continuation" prize money from the access cascade
# Assume average non-GS first-round prize as the forward-looking prize per additional main draw
avg_forward_prize <- 11000  # weighted avg across ATP/WTA, M1000/500/250

# Use the dynamic effects on n_main_draws at 26w
# From F03 results: NonGS-ATP main draws at 26w: +1.71, NonGS-WTA: +0.93
# GS ATP/WTA main draws at 26w are ~+0.66 each
dynamic_prize <- data.frame(
  sample = c("GS-ATP", "GS-WTA", "NonGS-ATP", "NonGS-WTA"),
  draws_effect = c(0.66, 0.66, 1.71, 0.93),
  draws_se = c(0.55, 0.67, 0.25, 0.21),
  forward_prize = avg_forward_prize,
  stringsAsFactors = FALSE
)
dynamic_prize$dollar_effect <- dynamic_prize$draws_effect * dynamic_prize$forward_prize
dynamic_prize$dollar_se <- dynamic_prize$draws_se * dynamic_prize$forward_prize

slog("\n26-week dynamic prize money (USD, via additional main draws):")
for (i in seq_len(nrow(dynamic_prize))) {
  r <- dynamic_prize[i, ]
  slog(sprintf("  %s: %.1f additional draws x $%d = $%.0f (SE $%.0f)",
               r$sample, r$draws_effect, r$forward_prize,
               r$dollar_effect, r$dollar_se))
}

# Total 52-week prize money effect: immediate + forward
total_prize <- data.frame(
  sample = c("GS-ATP", "GS-WTA", "NonGS-ATP", "NonGS-WTA"),
  immediate = c(prize_atp$diff, prize_wta$diff, prize_ngs_atp$diff, prize_ngs_wta$diff),
  dynamic = dynamic_prize$dollar_effect,
  stringsAsFactors = FALSE
)
total_prize$total <- total_prize$immediate + total_prize$dynamic

slog("\nTotal 52-week prize money effect (USD):")
for (i in seq_len(nrow(total_prize))) {
  r <- total_prize[i, ]
  slog(sprintf("  %s: immediate=$%.0f + dynamic=$%.0f = $%.0f",
               r$sample, r$immediate, r$dynamic, r$total))
}

# Generate prize money table
L <- c("\\begin{tabular}{l cccc}", "\\toprule",
       "Sample & Immediate (USD) & Dynamic 26w (USD) & Total (USD) & Treated $N$ \\\\",
       "\\midrule")
for (i in seq_len(nrow(total_prize))) {
  r <- total_prize[i, ]
  imm_p <- prize_df$pval[prize_df$sample == r$sample]
  stars <- if (length(imm_p) > 0 && !is.na(imm_p) && imm_p < 0.01) "***"
           else if (length(imm_p) > 0 && !is.na(imm_p) && imm_p < 0.05) "**"
           else if (length(imm_p) > 0 && !is.na(imm_p) && imm_p < 0.10) "*" else ""
  n_t <- prize_df$n_treat[prize_df$sample == r$sample]
  L <- c(L, sprintf("%s & \\$%s%s & \\$%s & \\$%s & %d \\\\",
                    r$sample,
                    format(round(r$immediate), big.mark = ","),
                    stars,
                    format(round(r$dynamic), big.mark = ","),
                    format(round(r$total), big.mark = ","),
                    n_t))
}
L <- c(L, "\\bottomrule", "\\end{tabular}")
writeLines(paste(L, collapse = "\n"), file.path(FIRSTLL_TABLES, "table_prize_money.tex"))
message("  Prize money table saved.")

saveRDS(list(immediate = prize_df, dynamic = dynamic_prize, total = total_prize),
        file.path(FIRSTLL_CLEANED, "prize_money_results.rds"))


# ==============================================================================
# SUMMARY
# ==============================================================================
writeLines(summary_log, file.path(FIRSTLL_OUTPUT, "F12_r2_fixes_summary.md"))
message("\nAll R2 fixes complete. Summary saved to Output_FirstLL/F12_r2_fixes_summary.md")
message("DONE.")
