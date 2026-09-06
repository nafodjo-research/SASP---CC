# ==============================================================================
# F09_marginal_impact.R
# NEW ROBUSTNESS DESIGN: Marginal impact of the nth LL opportunity.
#
# Concept: For each experience level n (number of prior LL awards),
# compare players receiving their (n+1)th LL to eligible players with the
# same n prior LLs who did not receive one at this event.
#
# Groups:
#   n=0: First-time LL recipients vs never-LL-before controls (= main results)
#   n=1: Second LL recipients vs 1-prior-LL controls
#   n=2+: Third+ LL recipients vs 2+-prior-LL controls
#
# Tests whether the marginal impact diminishes with experience.
#
# Run SEPARATELY for GS and non-GS.
#
# Inputs:
#   Data/cleaned/skeleton_gs_est_v5.rds   (FULL, unrestricted)
#   Data/cleaned/skeleton_nongs_est_v8.rds (FULL, unrestricted)
#
# Outputs:
#   Tables_FirstLL/table_marginal_impact_gs.tex
#   Tables_FirstLL/table_marginal_impact_nongs.tex
#   Data/cleaned/firstll/marginal_impact_results.rds
#   Output_FirstLL/F09_marginal_summary.md
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
# STEP 1: LOAD FULL (UNRESTRICTED) SAMPLES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 1: LOAD FULL SAMPLES")
message(strrep("=", 70))

gs_full  <- readRDS(file.path(CLEANED_DIR, "skeleton_gs_est_v5.rds"))
ngs_full <- readRDS(file.path(CLEANED_DIR, "skeleton_nongs_est_v8.rds"))

gs_full  <- ensure_scaled(gs_full)
ngs_full <- ensure_scaled(ngs_full)

# Compute total prior LL count
gs_full$n_prior_ll  <- gs_full$n_prior_gs_ll_won + gs_full$n_prior_nongs_ll_won
ngs_full$n_prior_ll <- ngs_full$n_prior_gs_ll_won + ngs_full$n_prior_nongs_ll_won

# Create experience groups: 0, 1, 2+
gs_full$exp_group  <- ifelse(gs_full$n_prior_ll >= 2, "2+",
                             as.character(gs_full$n_prior_ll))
ngs_full$exp_group <- ifelse(ngs_full$n_prior_ll >= 2, "2+",
                             as.character(ngs_full$n_prior_ll))

slog("GS experience distribution:")
slog(paste(capture.output(table(gs_full$exp_group, gs_full$got_ll,
                                dnn = c("n_prior_ll", "got_ll"))), collapse = "\n"))
slog("\nNGS experience distribution:")
slog(paste(capture.output(table(ngs_full$exp_group, ngs_full$got_ll,
                                dnn = c("n_prior_ll", "got_ll"))), collapse = "\n"))

# Merge dose
dose_full <- readRDS(file.path(CLEANED_DIR, "performance_dose.rds"))
gs_full  <- merge_dose(gs_full, dose_full, "GS")
ngs_full <- merge_dose(ngs_full, dose_full, "nonGS")


# ==============================================================================
# STEP 2: ESTIMATE WITHIN EACH EXPERIENCE GROUP
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 2: MARGINAL IMPACT BY EXPERIENCE GROUP")
message(strrep("=", 70))

run_marginal <- function(data, fe_str, cf_term, tour_label) {
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

    # Stack horizons
    st <- stack_horizons_full(sub)

    for (ob in c("points_change", "elo_change", "n_main_draws", "n_matches_250plus")) {
      sdata <- st[!is.na(st[[ob]]) &
                  !is.na(st$pre_rank_pts_s) &
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

# GS: event FE
gs_atp_res <- run_marginal(gs_full, "slam_year + horizon", NULL, "ATP")
gs_wta_res <- run_marginal(gs_full, "slam_year + horizon", NULL, "WTA")
gs_res <- rbind(gs_atp_res, gs_wta_res)

# Non-GS: event FE + CF
ngs_atp_res <- run_marginal(ngs_full, "tourney_id + horizon", "v_hat", "ATP")
ngs_wta_res <- run_marginal(ngs_full, "tourney_id + horizon", "v_hat", "WTA")
ngs_res <- rbind(ngs_atp_res, ngs_wta_res)

# Save results
saveRDS(list(gs = gs_res, ngs = ngs_res),
        file.path(FIRSTLL_CLEANED, "marginal_impact_results.rds"))


# ==============================================================================
# STEP 3: WALD TEST FOR EQUALITY ACROSS EXPERIENCE GROUPS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 3: WALD TESTS")
message(strrep("=", 70))

# Test whether n=0 and n=1 effects differ by interacting got_ll with exp_group
run_wald <- function(data, fe_str, cf_term, tour_label) {
  sub <- data[data$exp_group %in% c("0", "1") & data$tour == tour_label, ]
  sub$is_exp <- as.integer(sub$exp_group == "1")
  st <- stack_horizons_full(sub)

  wald_results <- list()
  for (ob in c("points_change", "n_main_draws")) {
    sdata <- st[!is.na(st[[ob]]) & !is.na(st$pre_rank_pts_s) &
                !is.na(st$player_age), ]
    if (!is.null(cf_term)) sdata <- sdata[!is.na(sdata$v_hat), ]
    if (nrow(sdata) < 100) next

    rhs <- "got_ll:horizon + got_ll:horizon:is_exp"
    if (!is.null(cf_term)) rhs <- paste0(rhs, " + v_hat:horizon")
    rhs <- paste0(rhs, " + ", ZPRE_FULL)
    fml <- as.formula(paste0(ob, " ~ ", rhs, " | ", fe_str))

    mod <- tryCatch(feols(fml, data = sdata, cluster = ~player_id),
                    error = function(e) NULL)
    if (is.null(mod)) next

    # Test joint significance of interaction terms
    int_vars <- grep("got_ll:horizon.*:is_exp", names(coef(mod)), value = TRUE)
    if (length(int_vars) > 0) {
      wt <- tryCatch(wald(mod, int_vars), error = function(e) NULL)
      if (!is.null(wt)) {
        wald_results[[paste0(tour_label, "_", ob)]] <- data.frame(
          tour = tour_label, outcome = ob,
          wald_stat = wt$stat, wald_pval = wt$p, df = length(int_vars),
          stringsAsFactors = FALSE)
        slog("  Wald (", tour_label, " ", ob, "): F=",
             round(wt$stat, 2), " p=", round(wt$p, 3))
      }
    }
  }
  do.call(rbind, wald_results)
}

wald_gs_atp <- run_wald(gs_full, "slam_year + horizon", NULL, "ATP")
wald_gs_wta <- run_wald(gs_full, "slam_year + horizon", NULL, "WTA")
wald_ngs_atp <- run_wald(ngs_full, "tourney_id + horizon", "v_hat", "ATP")
wald_ngs_wta <- run_wald(ngs_full, "tourney_id + horizon", "v_hat", "WTA")


# ==============================================================================
# STEP 4: GENERATE LATEX TABLES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 4: TABLES")
message(strrep("=", 70))

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

      for (ob in oos) {
        osub <- sub[sub$outcome == ob, ]
        if (nrow(osub) == 0) next

        cv <- sv <- character()
        for (h in HORIZON_LABS) {
          r <- osub[osub$horizon == h, ]
          if (nrow(r) == 0) { cv <- c(cv, ""); sv <- c(sv, "") }
          else {
            cv <- c(cv, paste0(fmt(r$coef[1], 1), add_stars(r$pval[1])))
            sv <- c(sv, paste0("(", fmt(r$se[1], 1), ")"))
          }
        }
        # Only show tour and exp_group label on first outcome row
        prefix <- if (ob == oos[which(oos %in% osub$outcome)[1]]) {
          paste0(t, " & ", eg_label)
        } else "  & "

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
message("  Marginal impact tables saved.")


# ==============================================================================
# STEP 5: LOG SUMMARY
# ==============================================================================
slog("\n", strrep("=", 70))
slog("MARGINAL IMPACT SUMMARY")
slog(strrep("=", 70))

for (r in list(gs_res, ngs_res)) {
  if (is.null(r) || nrow(r) == 0) next
  sample_label <- if (any(grepl("slam_year", names(r)))) "GS" else "NonGS"
  for (eg in c("0", "1", "2+")) {
    sub <- r[r$exp_group == eg, ]
    if (nrow(sub) == 0) next
    slog("\n  n=", eg, ":")
    for (i in seq_len(nrow(sub))) {
      row <- sub[i, ]
      star <- ifelse(row$pval < 0.01, "***",
                ifelse(row$pval < 0.05, "**",
                  ifelse(row$pval < 0.1, "*", "")))
      slog(sprintf("    %s %s %s %s: %.2f (%.2f) p=%.3f %s N=%d",
                   row$tour, row$exp_group, row$outcome, row$horizon,
                   row$coef, row$se, row$pval, star, row$n))
    }
  }
}

writeLines(summary_log, file.path(FIRSTLL_OUTPUT, "F09_marginal_summary.md"))
message("\nSummary saved to Output_FirstLL/F09_marginal_summary.md")
message("DONE.")
