# ==============================================================================
# 20_final_fixes.R
# Critical fixes for the Lucky Losers pipeline:
#   FIX 1: Correct control specification (horizon-INVARIANT controls)
#          Re-estimate ALL stacked dynamic tables
#   FIX 2: Regenerate hetero tables with HORIZON-SPECIFIC rows
#   FIX 3: Regenerate dose table with HORIZON-SPECIFIC rows
#   FIX 4: Regenerate robustness tables (verified, first-LL) with corrected spec
#   FIX 5: Regenerate event study figures from corrected coefficients
#   FIX 6: Non-GS mirror tables with corrected spec
#   FIX 7: Shared helpers extracted to utils.R (sourced here)
#   FIX 8: saveRDS for all computed objects
#
# Inputs:  Data/cleaned/skeleton_gs_est.rds
#          Data/cleaned/skeleton_nongs_est.rds
# Outputs: Tables/table_dynamic_stacked_atp.tex
#          Tables/table_dynamic_stacked_wta.tex
#          Tables/table_dynamic_stacked_nongs_atp.tex
#          Tables/table_dynamic_stacked_nongs_wta.tex
#          Tables/table_hetero_stacked_atp.tex
#          Tables/table_hetero_stacked_wta.tex
#          Tables/table_dose_stacked.tex
#          Tables/table_verified_stacked.tex
#          Tables/table_firstll_stacked_atp.tex
#          Tables/table_firstll_stacked_wta.tex
#          Tables/table_hetero_stacked_nongs_atp.tex
#          Tables/table_hetero_stacked_nongs_wta.tex
#          Tables/table_dose_stacked_nongs.tex
#          Figures/fig_event_study_atp.pdf
#          Figures/fig_event_study_wta.pdf
#          Data/cleaned/final_fixes_results.rds
#          Output/final_fixes_summary.md
# Dependencies: dplyr, tidyr, fixest, ggplot2, here
# ==============================================================================

set.seed(20260325)

# --- Packages -----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(fixest)
library(ggplot2)
library(here)

# --- Shared helpers (FIX 7) ---------------------------------------------------
source(here("scripts", "R", "utils.R"))
summary_log <- character()

# --- Paths --------------------------------------------------------------------
CLEANED_DIR <- here("Data", "cleaned")
TABLES_DIR  <- here("Tables")
FIGURES_DIR <- here("Figures")
OUTPUT_DIR  <- here("Output")
for (d in c(CLEANED_DIR, TABLES_DIR, FIGURES_DIR, OUTPUT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# --- Outcome definitions ------------------------------------------------------
outcomes_base  <- c("points_change", "n_main_draws", "n_matches_250plus", "elo_change")
outcome_labels <- c(
  "points_change"     = "Ranking points $\\Delta$",
  "n_main_draws"      = "Main draws entered",
  "n_matches_250plus" = "Matches at 250+",
  "elo_change"        = "Elo $\\Delta$"
)
HORIZONS     <- c(4, 8, 12, 26, 52)
HORIZON_LABS <- paste0(HORIZONS, "w")

# ==============================================================================
# LOAD DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("LOADING DATA")
message(strrep("=", 70))

gs_est    <- readRDS(file.path(CLEANED_DIR, "skeleton_gs_est.rds"))
nongs_est <- readRDS(file.path(CLEANED_DIR, "skeleton_nongs_est.rds"))

gs_atp <- gs_est |> filter(tour == "ATP")
gs_wta <- gs_est |> filter(tour == "WTA")

message("  ATP GS: N = ", nrow(gs_atp), " (LL: ", sum(gs_atp$got_ll), ")")
message("  WTA GS: N = ", nrow(gs_wta), " (LL: ", sum(gs_wta$got_ll), ")")
message("  Non-GS: N = ", nrow(nongs_est))

# Ensure columns exist in nongs_est
if (!"pre_elo" %in% names(nongs_est)) {
  nongs_est <- nongs_est |> mutate(pre_elo = 1500, pre_elo_sq = 1500^2)
}
if (!"had_prior_ll" %in% names(nongs_est)) {
  nongs_est <- nongs_est |> mutate(had_prior_ll = 0L)
}
med_elo_nongs <- median(nongs_est$pre_elo, na.rm = TRUE)
if (is.na(med_elo_nongs)) med_elo_nongs <- 1500
nongs_est <- nongs_est |>
  mutate(pre_elo = coalesce(pre_elo, med_elo_nongs),
         pre_elo_sq = pre_elo^2)


# ==============================================================================
# FIX 1: CORRECT CONTROL SPECIFICATION -- STACKED DYNAMIC MODELS
# ==============================================================================
# Paper equation: Y_{ie,h} = alpha_e + alpha_h + beta_h * D_{ie} + Gamma * Z^pre + eps
# Gamma does NOT vary by horizon.
#
# CORRECT:   outcome ~ got_ll:factor(horizon) + pre_rank_pts + pre_rank_pts_sq
#                     + player_age | slam_year + factor(horizon)
#
# WRONG (old): outcome ~ got_ll:factor(horizon) + pre_rank_pts:factor(horizon)
#                       + pre_rank_pts_sq:factor(horizon)
#                       + player_age:factor(horizon) | slam_year + factor(horizon)
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 1: STACKED DYNAMIC MODELS -- CORRECTED CONTROL SPEC")
message(strrep("=", 70))
slog("## FIX 1: Stacked dynamic models with horizon-INVARIANT controls\n")

# --- 1a. Stack the data ------------------------------------------------------
stacked_atp <- stack_horizons(gs_atp, outcomes_base)
stacked_wta <- stack_horizons(gs_wta, outcomes_base)

message("  Stacked ATP GS: ", nrow(stacked_atp), " rows (",
        n_distinct(stacked_atp$player_id), " players)")
message("  Stacked WTA GS: ", nrow(stacked_wta), " rows (",
        n_distinct(stacked_wta$player_id), " players)")

# --- 1b. Estimate corrected stacked GS models --------------------------------
run_stacked_gs_corrected <- function(stacked_data, tour_label) {
  results <- list()
  model_objects <- list()

  for (ob in outcomes_base) {
    if (!ob %in% names(stacked_data)) next

    sdata <- stacked_data |>
      filter(!is.na(.data[[ob]]),
             !is.na(pre_rank_pts), !is.na(player_age))
    if (nrow(sdata) < 20) {
      message("    Skipping ", ob, " for ", tour_label, ": too few obs")
      next
    }

    # CORRECTED SPEC: controls do NOT interact with horizon
    fml <- as.formula(paste0(
      ob, " ~ got_ll:horizon + pre_rank_pts + pre_rank_pts_sq + player_age",
      " | slam_year + horizon"
    ))

    fit <- tryCatch(
      feols(fml, data = sdata, vcov = ~player_id),
      error = function(e) {
        message("    Error for ", ob, " (", tour_label, "): ", e$message)
        NULL
      }
    )
    if (is.null(fit)) next

    model_objects[[ob]] <- fit

    cf <- coef(fit)
    se_vec <- sqrt(diag(vcov(fit)))

    for (h_lab in HORIZON_LABS) {
      coef_name <- paste0("got_ll:horizon", h_lab)
      if (!coef_name %in% names(cf)) next

      beta <- cf[coef_name]
      se_val <- se_vec[coef_name]
      pv <- 2 * pnorm(-abs(beta / se_val))
      n_h <- sum(!is.na(sdata[[ob]]) & sdata$horizon == h_lab)

      results[[paste0(ob, "_", h_lab)]] <- tibble(
        tour = tour_label,
        outcome = ob,
        horizon = h_lab,
        coef = beta,
        se = se_val,
        pvalue = pv,
        n_obs_total = nrow(sdata),
        n_obs_horizon = n_h,
        n_units = n_distinct(paste0(sdata$player_id, "_", sdata$tourney_id))
      )
    }
  }
  list(results = bind_rows(results), models = model_objects)
}

out_atp <- run_stacked_gs_corrected(stacked_atp, "ATP")
out_wta <- run_stacked_gs_corrected(stacked_wta, "WTA")

stacked_res_atp <- out_atp$results
stacked_res_wta <- out_wta$results
stacked_models_atp <- out_atp$models
stacked_models_wta <- out_wta$models

for (i in seq_len(nrow(stacked_res_atp))) {
  r <- stacked_res_atp[i, ]
  slog("- ATP stacked ", r$outcome, " @ ", r$horizon,
       ": coef = ", fmt(r$coef), ", SE = ", fmt(r$se),
       ", p = ", fmt(r$pvalue, 3),
       ", N_obs = ", r$n_obs_total, ", N_units = ", r$n_units)
}
for (i in seq_len(nrow(stacked_res_wta))) {
  r <- stacked_res_wta[i, ]
  slog("- WTA stacked ", r$outcome, " @ ", r$horizon,
       ": coef = ", fmt(r$coef), ", SE = ", fmt(r$se),
       ", p = ", fmt(r$pvalue, 3),
       ", N_obs = ", r$n_obs_total, ", N_units = ", r$n_units)
}
slog("")

# --- 1c. Non-GS stacked IV models (corrected) --------------------------------
message("\n  Non-GS stacked IV models (corrected spec)...")

nongs_iv_atp <- nongs_est |> filter(!is.na(peer_component), tour == "ATP")
nongs_iv_wta <- nongs_est |> filter(!is.na(peer_component), tour == "WTA")

message("  Non-GS ATP with IV: N = ", nrow(nongs_iv_atp))
message("  Non-GS WTA with IV: N = ", nrow(nongs_iv_wta))

stacked_nongs_atp <- stack_horizons(nongs_iv_atp, outcomes_base)
stacked_nongs_wta <- stack_horizons(nongs_iv_wta, outcomes_base)

# For non-GS IV: use fixest with interacted instrument in stacked data
# CORRECTED: controls NOT interacted with horizon
# Formula: outcome ~ pre_rank_pts + pre_rank_pts_sq + player_age
#         | slam_year + horizon
#         | got_ll:factor(horizon) ~ peer_component:factor(horizon)
run_stacked_nongs_iv_corrected <- function(stacked_data, tour_label) {
  results <- list()
  model_objects <- list()

  for (ob in outcomes_base) {
    if (!ob %in% names(stacked_data)) next

    sdata <- stacked_data |>
      filter(!is.na(.data[[ob]]),
             !is.na(pre_rank_pts), !is.na(player_age),
             !is.na(peer_component))
    if (nrow(sdata) < 50) {
      message("    Skipping ", ob, " for ", tour_label, ": too few obs")
      next
    }

    # CORRECTED IV spec with horizon-invariant controls
    fml <- as.formula(paste0(
      ob, " ~ pre_rank_pts + pre_rank_pts_sq + player_age",
      " | slam_year + horizon",
      " | got_ll:horizon ~ peer_component:horizon"
    ))

    fit <- tryCatch(
      feols(fml, data = sdata, vcov = ~player_id),
      error = function(e) {
        message("    IV error for ", ob, " (", tour_label, "): ", e$message)
        NULL
      }
    )
    if (is.null(fit)) next

    model_objects[[ob]] <- fit

    cf <- coef(fit)
    se_vec <- sqrt(diag(vcov(fit)))

    for (h_lab in HORIZON_LABS) {
      coef_name <- paste0("fit_got_ll:horizon", h_lab)
      if (!coef_name %in% names(cf)) next

      beta <- cf[coef_name]
      se_val <- se_vec[coef_name]
      pv <- 2 * pnorm(-abs(beta / se_val))
      n_h <- sum(!is.na(sdata[[ob]]) & sdata$horizon == h_lab)

      results[[paste0(ob, "_", h_lab)]] <- tibble(
        tour = tour_label,
        outcome = ob,
        horizon = h_lab,
        coef = beta,
        se = se_val,
        pvalue = pv,
        n_obs_total = nrow(sdata),
        n_obs_horizon = n_h,
        n_units = n_distinct(paste0(sdata$player_id, "_", sdata$tourney_id))
      )
    }
  }
  list(results = bind_rows(results), models = model_objects)
}

out_nongs_atp <- run_stacked_nongs_iv_corrected(stacked_nongs_atp, "ATP non-GS")
out_nongs_wta <- run_stacked_nongs_iv_corrected(stacked_nongs_wta, "WTA non-GS")

stacked_nongs_res_atp <- out_nongs_atp$results
stacked_nongs_res_wta <- out_nongs_wta$results

slog("## FIX 1 (Non-GS IV corrected):")
for (i in seq_len(nrow(stacked_nongs_res_atp))) {
  r <- stacked_nongs_res_atp[i, ]
  slog("- ATP non-GS IV ", r$outcome, " @ ", r$horizon,
       ": coef = ", fmt(r$coef), ", SE = ", fmt(r$se),
       ", p = ", fmt(r$pvalue, 3),
       ", N_horizon = ", r$n_obs_horizon, ", N_stacked = ", r$n_obs_total)
}
for (i in seq_len(nrow(stacked_nongs_res_wta))) {
  r <- stacked_nongs_res_wta[i, ]
  slog("- WTA non-GS IV ", r$outcome, " @ ", r$horizon,
       ": coef = ", fmt(r$coef), ", SE = ", fmt(r$se),
       ", p = ", fmt(r$pvalue, 3),
       ", N_horizon = ", r$n_obs_horizon, ", N_stacked = ", r$n_obs_total)
}
slog("")

# --- 1d. Build stacked tables ------------------------------------------------
build_stacked_tex <- function(res_df, tour_label, filename) {
  n_hor <- length(HORIZON_LABS)

  tex <- c(
    paste0("\\begin{tabular}{l", paste(rep(" c", n_hor), collapse = ""), "}"),
    "\\toprule",
    paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"),
    "\\midrule"
  )

  avail_outcomes <- intersect(unique(res_df$outcome), names(outcome_labels))
  for (ob in names(outcome_labels)) {
    if (!ob %in% avail_outcomes) next
    olab <- outcome_labels[ob]

    cells <- character()
    se_cells <- character()
    for (h in HORIZON_LABS) {
      r <- res_df |> filter(outcome == ob, horizon == h)
      if (nrow(r) == 0) {
        cells <- c(cells, "")
        se_cells <- c(se_cells, "")
      } else {
        cells <- c(cells, paste0(fmt(r$coef), add_stars(r$pvalue)))
        se_cells <- c(se_cells, paste0("(", fmt(r$se), ")"))
      }
    }
    tex <- c(tex,
      paste0(olab, " & ", paste(cells, collapse = " & "), " \\\\"),
      paste0(" & ", paste(se_cells, collapse = " & "), " \\\\"),
      "\\addlinespace"
    )
  }

  if (nrow(res_df) > 0) {
    n_units <- res_df$n_units[1]
    n_obs <- res_df$n_obs_total[1]

    tex <- c(tex, "\\midrule")

    h_n_cells <- character()
    for (h in HORIZON_LABS) {
      r <- res_df |> filter(horizon == h)
      if (nrow(r) > 0) {
        h_n_cells <- c(h_n_cells, as.character(r$n_obs_horizon[1]))
      } else {
        h_n_cells <- c(h_n_cells, "--")
      }
    }
    tex <- c(tex,
      paste0("$N$ (per horizon) & ", paste(h_n_cells, collapse = " & "), " \\\\"),
      paste0("$N$ (stacked total) & \\multicolumn{", n_hor, "}{c}{", n_obs, "} \\\\"),
      paste0("Player-events & \\multicolumn{", n_hor, "}{c}{", n_units, "} \\\\"),
      paste0("Event FE & \\multicolumn{", n_hor, "}{c}{Yes} \\\\"),
      paste0("Horizon FE & \\multicolumn{", n_hor, "}{c}{Yes} \\\\")
    )
  }

  tex <- c(tex, "\\bottomrule", "\\end{tabular}")
  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

build_stacked_tex(stacked_res_atp, "ATP", "table_dynamic_stacked_atp.tex")
build_stacked_tex(stacked_res_wta, "WTA", "table_dynamic_stacked_wta.tex")
build_stacked_tex(stacked_nongs_res_atp, "ATP non-GS", "table_dynamic_stacked_nongs_atp.tex")
build_stacked_tex(stacked_nongs_res_wta, "WTA non-GS", "table_dynamic_stacked_nongs_wta.tex")


# ==============================================================================
# FIX 2: HETERO TABLES WITH HORIZON-SPECIFIC ROWS
# ==============================================================================
# Equation: Y_{ie,h} = alpha_e + alpha_h + beta_h*D + delta_{k,h}*D*1[G=k]
#           + Gamma*Z^pre + eps
#
# Table structure:
#   Rows = horizons (4w, 8w, 12w, 26w, 52w)
#   Within each row: beta_h (base) and delta_{k,h} (differential)
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 2: HETERO TABLES -- HORIZON-SPECIFIC ROWS")
message(strrep("=", 70))
slog("## FIX 2: Heterogeneity with horizon-specific rows\n")

# Prepare category variables
prepare_hetero_data <- function(data) {
  qts <- quantile(data$pre_rank_pts, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  med_age <- median(data$player_age, na.rm = TRUE)

  data |> mutate(
    rank_q = cut(pre_rank_pts, breaks = c(-Inf, qts[1], qts[2], qts[3], Inf),
                 labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE),
    rank_above_med = as.integer(pre_rank_pts >= median(pre_rank_pts, na.rm = TRUE)),
    age_above_med  = as.integer(player_age >= med_age),
    prior_ll_dum   = as.integer(had_prior_ll == 1)
  )
}

gs_atp_h <- prepare_hetero_data(gs_atp)
gs_wta_h <- prepare_hetero_data(gs_wta)

stacked_atp_h <- stack_horizons(gs_atp_h, outcomes_base)
stacked_wta_h <- stack_horizons(gs_wta_h, outcomes_base)

# Merge category variables back into stacked data
merge_cats <- function(stacked, orig) {
  cats <- orig |> select(player_id, tourney_id,
    rank_above_med, age_above_med, prior_ll_dum)
  stacked |> left_join(cats, by = c("player_id", "tourney_id"))
}

stacked_atp_h <- merge_cats(stacked_atp_h, gs_atp_h)
stacked_wta_h <- merge_cats(stacked_wta_h, gs_wta_h)

# --- Estimate horizon-specific hetero models ----------------------------------
# For each dimension, one stacked regression with got_ll:horizon and
# got_ll:horizon:category_dummy as regressors, horizon-INVARIANT controls
run_hetero_horizon <- function(sdata, tour_label) {
  results <- list()
  model_objects <- list()

  for (ob in outcomes_base) {
    if (!ob %in% names(sdata)) next
    d <- sdata |> filter(!is.na(.data[[ob]]),
                         !is.na(pre_rank_pts), !is.na(player_age))
    if (nrow(d) < 30) next

    # --- Ranking (above-median) ---
    fml_rank <- as.formula(paste0(
      ob, " ~ got_ll:horizon + got_ll:horizon:rank_above_med",
      " + pre_rank_pts + pre_rank_pts_sq + player_age | slam_year + horizon"
    ))
    fit_rank <- tryCatch(feols(fml_rank, data = d, vcov = ~player_id),
                         error = function(e) NULL)
    if (!is.null(fit_rank)) {
      model_objects[[paste0(ob, "_rank")]] <- fit_rank
      cf <- coef(fit_rank); se <- sqrt(diag(vcov(fit_rank)))
      for (h_lab in HORIZON_LABS) {
        base_nm <- paste0("got_ll:horizon", h_lab)
        diff_nm <- paste0("got_ll:horizon", h_lab, ":rank_above_med")
        if (base_nm %in% names(cf)) {
          pv <- 2 * pnorm(-abs(cf[base_nm] / se[base_nm]))
          results[[paste0("rank_base_", ob, "_", h_lab)]] <- tibble(
            tour = tour_label, outcome = ob, horizon = h_lab,
            dimension = "Ranking", term = "beta_h (low rank base)",
            coef = cf[base_nm], se = se[base_nm], pvalue = pv
          )
        }
        if (diff_nm %in% names(cf)) {
          pv <- 2 * pnorm(-abs(cf[diff_nm] / se[diff_nm]))
          results[[paste0("rank_diff_", ob, "_", h_lab)]] <- tibble(
            tour = tour_label, outcome = ob, horizon = h_lab,
            dimension = "Ranking", term = "delta_h (high rank)",
            coef = cf[diff_nm], se = se[diff_nm], pvalue = pv
          )
        }
      }
    }

    # --- Age (above-median) ---
    fml_age <- as.formula(paste0(
      ob, " ~ got_ll:horizon + got_ll:horizon:age_above_med",
      " + pre_rank_pts + pre_rank_pts_sq + player_age | slam_year + horizon"
    ))
    fit_age <- tryCatch(feols(fml_age, data = d, vcov = ~player_id),
                        error = function(e) NULL)
    if (!is.null(fit_age)) {
      model_objects[[paste0(ob, "_age")]] <- fit_age
      cf <- coef(fit_age); se <- sqrt(diag(vcov(fit_age)))
      for (h_lab in HORIZON_LABS) {
        base_nm <- paste0("got_ll:horizon", h_lab)
        diff_nm <- paste0("got_ll:horizon", h_lab, ":age_above_med")
        if (base_nm %in% names(cf)) {
          pv <- 2 * pnorm(-abs(cf[base_nm] / se[base_nm]))
          results[[paste0("age_base_", ob, "_", h_lab)]] <- tibble(
            tour = tour_label, outcome = ob, horizon = h_lab,
            dimension = "Age", term = "beta_h (young base)",
            coef = cf[base_nm], se = se[base_nm], pvalue = pv
          )
        }
        if (diff_nm %in% names(cf)) {
          pv <- 2 * pnorm(-abs(cf[diff_nm] / se[diff_nm]))
          results[[paste0("age_diff_", ob, "_", h_lab)]] <- tibble(
            tour = tour_label, outcome = ob, horizon = h_lab,
            dimension = "Age", term = "delta_h (older)",
            coef = cf[diff_nm], se = se[diff_nm], pvalue = pv
          )
        }
      }
    }

    # --- Prior LL ---
    fml_prior <- as.formula(paste0(
      ob, " ~ got_ll:horizon + got_ll:horizon:prior_ll_dum",
      " + pre_rank_pts + pre_rank_pts_sq + player_age | slam_year + horizon"
    ))
    fit_prior <- tryCatch(feols(fml_prior, data = d, vcov = ~player_id),
                          error = function(e) NULL)
    if (!is.null(fit_prior)) {
      model_objects[[paste0(ob, "_prior")]] <- fit_prior
      cf <- coef(fit_prior); se <- sqrt(diag(vcov(fit_prior)))
      for (h_lab in HORIZON_LABS) {
        base_nm <- paste0("got_ll:horizon", h_lab)
        diff_nm <- paste0("got_ll:horizon", h_lab, ":prior_ll_dum")
        if (base_nm %in% names(cf)) {
          pv <- 2 * pnorm(-abs(cf[base_nm] / se[base_nm]))
          results[[paste0("prior_base_", ob, "_", h_lab)]] <- tibble(
            tour = tour_label, outcome = ob, horizon = h_lab,
            dimension = "Prior LL", term = "beta_h (no prior base)",
            coef = cf[base_nm], se = se[base_nm], pvalue = pv
          )
        }
        if (diff_nm %in% names(cf)) {
          pv <- 2 * pnorm(-abs(cf[diff_nm] / se[diff_nm]))
          results[[paste0("prior_diff_", ob, "_", h_lab)]] <- tibble(
            tour = tour_label, outcome = ob, horizon = h_lab,
            dimension = "Prior LL", term = "delta_h (had prior LL)",
            coef = cf[diff_nm], se = se[diff_nm], pvalue = pv
          )
        }
      }
    }
  }
  list(results = bind_rows(results), models = model_objects)
}

hetero_out_atp <- run_hetero_horizon(stacked_atp_h, "ATP")
hetero_out_wta <- run_hetero_horizon(stacked_wta_h, "WTA")

hetero_res_atp <- hetero_out_atp$results
hetero_res_wta <- hetero_out_wta$results

# Log key results
for (tour_df in list(hetero_res_atp, hetero_res_wta)) {
  if (nrow(tour_df) == 0) next
  pts <- tour_df |> filter(outcome == "points_change")
  for (i in seq_len(nrow(pts))) {
    r <- pts[i, ]
    slog("- ", r$tour, " points_change @ ", r$horizon, " | ", r$dimension,
         " | ", r$term, ": coef = ", fmt(r$coef), " (", fmt(r$se), ")",
         ", p = ", fmt(r$pvalue, 3))
  }
}
slog("")

# --- Build horizon-specific hetero table --------------------------------------
# Structure: Panel per dimension. Rows = horizons. Columns = beta_h, delta_h
# for a given outcome (points_change primary, others in appendix)
build_hetero_horizon_tex <- function(res_df, tour_label, filename) {
  n_hor <- length(HORIZON_LABS)
  dims <- c("Ranking", "Age", "Prior LL")

  # Primary outcome = points_change
  primary_ob <- "points_change"
  primary_data <- res_df |> filter(outcome == primary_ob)

  if (nrow(primary_data) == 0) {
    message("  No points_change results for ", tour_label, " -- skipping")
    return(invisible(NULL))
  }

  # Table: 3 panels (dimensions), each with rows = horizons
  # Columns: beta_h (base), SE, delta_h (diff), SE
  tex <- c(
    "\\begin{tabular}{l c c c c}",
    "\\toprule",
    paste0("Horizon & $\\hat{\\beta}_h$ (base) & SE",
           " & $\\hat{\\delta}_h$ (differential) & SE \\\\"),
    "\\midrule"
  )

  for (dm in dims) {
    dim_data <- primary_data |> filter(dimension == dm)
    if (nrow(dim_data) == 0) next

    # Determine the differential term label
    diff_terms <- dim_data |> filter(grepl("delta", term)) |> pull(term) |> unique()
    diff_label <- if (length(diff_terms) > 0) diff_terms[1] else ""

    tex <- c(tex, paste0("\\multicolumn{5}{l}{\\textit{",
                         dm, " heterogeneity",
                         ifelse(diff_label != "", paste0(" (", gsub("delta_h ", "", diff_label), ")"), ""),
                         "}} \\\\"))

    for (h_lab in HORIZON_LABS) {
      base_row <- dim_data |> filter(horizon == h_lab, grepl("beta", term))
      diff_row <- dim_data |> filter(horizon == h_lab, grepl("delta", term))

      base_cell <- if (nrow(base_row) > 0) {
        paste0(fmt(base_row$coef), add_stars(base_row$pvalue))
      } else ""
      base_se <- if (nrow(base_row) > 0) {
        paste0("(", fmt(base_row$se), ")")
      } else ""

      diff_cell <- if (nrow(diff_row) > 0) {
        paste0(fmt(diff_row$coef), add_stars(diff_row$pvalue))
      } else ""
      diff_se <- if (nrow(diff_row) > 0) {
        paste0("(", fmt(diff_row$se), ")")
      } else ""

      tex <- c(tex, paste0(h_lab, " & ", base_cell, " & ", base_se,
                           " & ", diff_cell, " & ", diff_se, " \\\\"))
    }
    tex <- c(tex, "\\addlinespace")
  }

  # Also show all outcomes in a compact summary panel
  tex <- c(tex,
    "\\midrule",
    "\\multicolumn{5}{l}{\\textit{All outcomes at 26w (ranking heterogeneity)}} \\\\"
  )
  for (ob in names(outcome_labels)) {
    ob_data <- res_df |> filter(outcome == ob, dimension == "Ranking", horizon == "26w")
    base_r <- ob_data |> filter(grepl("beta", term))
    diff_r <- ob_data |> filter(grepl("delta", term))

    base_cell <- if (nrow(base_r) > 0) paste0(fmt(base_r$coef), add_stars(base_r$pvalue)) else ""
    base_se   <- if (nrow(base_r) > 0) paste0("(", fmt(base_r$se), ")") else ""
    diff_cell <- if (nrow(diff_r) > 0) paste0(fmt(diff_r$coef), add_stars(diff_r$pvalue)) else ""
    diff_se   <- if (nrow(diff_r) > 0) paste0("(", fmt(diff_r$se), ")") else ""

    tex <- c(tex, paste0(outcome_labels[ob], " & ", base_cell, " & ", base_se,
                         " & ", diff_cell, " & ", diff_se, " \\\\"))
  }

  # N and FE rows
  n_obs <- if (nrow(primary_data) > 0) {
    stacked_n <- if (tour_label == "ATP") nrow(stacked_atp) else nrow(stacked_wta)
    stacked_n
  } else NA
  n_units <- if (tour_label == "ATP") {
    n_distinct(paste0(stacked_atp$player_id, "_", stacked_atp$tourney_id))
  } else {
    n_distinct(paste0(stacked_wta$player_id, "_", stacked_wta$tourney_id))
  }

  tex <- c(tex,
    "\\midrule",
    paste0("\\multicolumn{5}{l}{$N$ (stacked) = ", n_obs,
           "; Player-events = ", n_units, "} \\\\"),
    paste0("\\multicolumn{5}{l}{Event FE = Yes; Horizon FE = Yes} \\\\"),
    "\\bottomrule", "\\end{tabular}"
  )

  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

if (nrow(hetero_res_atp) > 0) {
  build_hetero_horizon_tex(hetero_res_atp, "ATP", "table_hetero_stacked_atp.tex")
}
if (nrow(hetero_res_wta) > 0) {
  build_hetero_horizon_tex(hetero_res_wta, "WTA", "table_hetero_stacked_wta.tex")
}


# ==============================================================================
# FIX 3: DOSE TABLE WITH HORIZON-SPECIFIC ROWS
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 3: DOSE TABLE -- HORIZON-SPECIFIC ROWS")
message(strrep("=", 70))
slog("## FIX 3: Dose table with horizon-specific rows\n")

# Prepare dose variables
prepare_dose_data <- function(data) {
  data |> mutate(
    dose_1win  = as.integer(!is.na(md_matches_won) & md_matches_won == 1),
    dose_2plus = as.integer(!is.na(md_matches_won) & md_matches_won >= 2)
  )
}

gs_atp_d <- prepare_dose_data(gs_atp)
gs_wta_d <- prepare_dose_data(gs_wta)

stacked_atp_d <- stack_horizons(gs_atp_d, outcomes_base)
stacked_wta_d <- stack_horizons(gs_wta_d, outcomes_base)

# Merge dose variables
merge_dose <- function(stacked, orig) {
  cats <- orig |> select(player_id, tourney_id, dose_1win, dose_2plus)
  stacked |> left_join(cats, by = c("player_id", "tourney_id"))
}

stacked_atp_d <- merge_dose(stacked_atp_d, gs_atp_d)
stacked_wta_d <- merge_dose(stacked_wta_d, gs_wta_d)

run_dose_horizon <- function(sdata, tour_label) {
  results <- list()
  model_objects <- list()

  for (ob in outcomes_base) {
    if (!ob %in% names(sdata)) next
    d <- sdata |> filter(!is.na(.data[[ob]]),
                         !is.na(pre_rank_pts), !is.na(player_age))
    if (nrow(d) < 30) next

    fml <- as.formula(paste0(
      ob, " ~ got_ll:horizon + got_ll:horizon:dose_1win + got_ll:horizon:dose_2plus",
      " + pre_rank_pts + pre_rank_pts_sq + player_age | slam_year + horizon"
    ))
    fit <- tryCatch(feols(fml, data = d, vcov = ~player_id),
                    error = function(e) NULL)
    if (is.null(fit)) next

    model_objects[[ob]] <- fit
    cf <- coef(fit); se <- sqrt(diag(vcov(fit)))

    for (h_lab in HORIZON_LABS) {
      base_nm   <- paste0("got_ll:horizon", h_lab)
      d1win_nm  <- paste0("got_ll:horizon", h_lab, ":dose_1win")
      d2plus_nm <- paste0("got_ll:horizon", h_lab, ":dose_2plus")

      for (nm_info in list(
        list(nm = base_nm, term = "beta_h (0-win base)"),
        list(nm = d1win_nm, term = "delta_h (1 win)"),
        list(nm = d2plus_nm, term = "delta_h (2+ wins)")
      )) {
        if (nm_info$nm %in% names(cf)) {
          pv <- 2 * pnorm(-abs(cf[nm_info$nm] / se[nm_info$nm]))
          results[[paste0(nm_info$term, "_", ob, "_", h_lab)]] <- tibble(
            tour = tour_label, outcome = ob, horizon = h_lab,
            term = nm_info$term,
            coef = cf[nm_info$nm], se = se[nm_info$nm], pvalue = pv
          )
        }
      }
    }
  }
  list(results = bind_rows(results), models = model_objects)
}

dose_out_atp <- run_dose_horizon(stacked_atp_d, "ATP")
dose_out_wta <- run_dose_horizon(stacked_wta_d, "WTA")

dose_res_atp <- dose_out_atp$results
dose_res_wta <- dose_out_wta$results

for (tour_df in list(dose_res_atp, dose_res_wta)) {
  if (nrow(tour_df) == 0) next
  pts <- tour_df |> filter(outcome == "points_change")
  for (i in seq_len(nrow(pts))) {
    r <- pts[i, ]
    slog("- ", r$tour, " dose points_change @ ", r$horizon, " | ", r$term,
         ": coef = ", fmt(r$coef), " (", fmt(r$se), "), p = ", fmt(r$pvalue, 3))
  }
}
slog("")

# Build dose table
build_dose_horizon_tex <- function(atp_res, wta_res, filename) {
  primary_ob <- "points_change"

  tex <- c(
    "\\begin{tabular}{l c c c c c c}",
    "\\toprule",
    paste0("Horizon & $\\hat{\\beta}_h$ & SE",
           " & $\\hat{\\delta}_{1\\text{win},h}$ & SE",
           " & $\\hat{\\delta}_{2+,h}$ & SE \\\\"),
    "\\midrule"
  )

  for (panel in list(list(df = atp_res, label = "Panel A: ATP GS"),
                     list(df = wta_res, label = "Panel B: WTA GS"))) {
    pd <- panel$df |> filter(outcome == primary_ob)
    if (nrow(pd) == 0) next

    tex <- c(tex, paste0("\\multicolumn{7}{l}{\\textit{", panel$label, "}} \\\\"))

    for (h_lab in HORIZON_LABS) {
      base_r   <- pd |> filter(horizon == h_lab, grepl("0-win", term))
      d1win_r  <- pd |> filter(horizon == h_lab, grepl("1 win", term))
      d2plus_r <- pd |> filter(horizon == h_lab, grepl("2\\+", term))

      make_cell <- function(r) {
        if (nrow(r) > 0) paste0(fmt(r$coef), add_stars(r$pvalue)) else ""
      }
      make_se <- function(r) {
        if (nrow(r) > 0) paste0("(", fmt(r$se), ")") else ""
      }

      tex <- c(tex, paste0(
        h_lab, " & ",
        make_cell(base_r), " & ", make_se(base_r), " & ",
        make_cell(d1win_r), " & ", make_se(d1win_r), " & ",
        make_cell(d2plus_r), " & ", make_se(d2plus_r), " \\\\"
      ))
    }
    tex <- c(tex, "\\addlinespace")
  }

  # All outcomes at 26w summary
  tex <- c(tex,
    "\\midrule",
    "\\multicolumn{7}{l}{\\textit{All outcomes at 26w (ATP GS)}} \\\\"
  )
  for (ob in names(outcome_labels)) {
    ob_data <- atp_res |> filter(outcome == ob, horizon == "26w")
    base_r   <- ob_data |> filter(grepl("0-win", term))
    d1win_r  <- ob_data |> filter(grepl("1 win", term))
    d2plus_r <- ob_data |> filter(grepl("2\\+", term))

    make_cell <- function(r) {
      if (nrow(r) > 0) paste0(fmt(r$coef), add_stars(r$pvalue)) else ""
    }
    make_se <- function(r) {
      if (nrow(r) > 0) paste0("(", fmt(r$se), ")") else ""
    }

    tex <- c(tex, paste0(
      outcome_labels[ob], " & ",
      make_cell(base_r), " & ", make_se(base_r), " & ",
      make_cell(d1win_r), " & ", make_se(d1win_r), " & ",
      make_cell(d2plus_r), " & ", make_se(d2plus_r), " \\\\"
    ))
  }

  tex <- c(tex,
    "\\midrule",
    "\\multicolumn{7}{l}{Event FE = Yes; Horizon FE = Yes} \\\\",
    "\\bottomrule", "\\end{tabular}"
  )

  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

build_dose_horizon_tex(dose_res_atp, dose_res_wta, "table_dose_stacked.tex")


# ==============================================================================
# FIX 4: ROBUSTNESS TABLES -- CORRECTED SPEC
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 4: ROBUSTNESS TABLES (VERIFIED, FIRST-LL)")
message(strrep("=", 70))
slog("## FIX 4: Robustness tables with corrected control spec\n")

# --- 4a. Verified lottery subsample -------------------------------------------
verified_events <- gs_est |>
  filter(got_ll == 1) |>
  group_by(tourney_id) |>
  summarise(min_ll_rank = min(rank_among_losers, na.rm = TRUE), .groups = "drop") |>
  filter(min_ll_rank > 1) |>
  pull(tourney_id)

message("  Verified lottery events: ", length(verified_events))

run_robustness_stacked <- function(data_subset, tour_label) {
  stacked_sub <- stack_horizons(data_subset, outcomes_base)

  results <- list()
  for (ob in outcomes_base) {
    if (!ob %in% names(stacked_sub)) next
    d <- stacked_sub |> filter(!is.na(.data[[ob]]),
                                !is.na(pre_rank_pts), !is.na(player_age))
    sy_counts <- d |> count(slam_year) |> filter(n >= 2)
    d <- d |> filter(slam_year %in% sy_counts$slam_year)
    if (nrow(d) < 20 || sum(d$got_ll == 1) < 3) next

    fml <- as.formula(paste0(
      ob, " ~ got_ll:horizon + pre_rank_pts + pre_rank_pts_sq + player_age",
      " | slam_year + horizon"
    ))
    fit <- tryCatch(feols(fml, data = d, vcov = ~player_id), error = function(e) NULL)
    if (is.null(fit)) next

    cf <- coef(fit); se_v <- sqrt(diag(vcov(fit)))
    for (h_lab in HORIZON_LABS) {
      cn <- paste0("got_ll:horizon", h_lab)
      if (cn %in% names(cf)) {
        pv <- 2 * pnorm(-abs(cf[cn] / se_v[cn]))
        results[[paste0(ob, "_", h_lab)]] <- tibble(
          tour = tour_label, outcome = ob, horizon = h_lab,
          coef = cf[cn], se = se_v[cn], pvalue = pv,
          n_obs = nrow(d),
          n_units = n_distinct(paste0(d$player_id, "_", d$tourney_id))
        )
      }
    }
  }
  bind_rows(results)
}

# Verified
ver_atp_data <- gs_atp |> filter(tourney_id %in% verified_events)
ver_wta_data <- gs_wta |> filter(tourney_id %in% verified_events)
ver_res <- bind_rows(
  run_robustness_stacked(ver_atp_data, "ATP"),
  run_robustness_stacked(ver_wta_data, "WTA")
)

slog("- Verified ATP: N = ", nrow(ver_atp_data), " (LL: ", sum(ver_atp_data$got_ll), ")")
slog("- Verified WTA: N = ", nrow(ver_wta_data), " (LL: ", sum(ver_wta_data$got_ll), ")")
for (i in seq_len(nrow(ver_res))) {
  r <- ver_res[i, ]
  if (r$outcome == "points_change") {
    slog("- Verified ", r$tour, " pts @ ", r$horizon,
         ": coef = ", fmt(r$coef), " (", fmt(r$se), "), p = ", fmt(r$pvalue, 3))
  }
}
slog("")

# Build verified table (two panels)
build_robustness_tex <- function(res_df, filename, panel_var = "tour") {
  n_hor <- length(HORIZON_LABS)
  panels <- split(res_df, res_df[[panel_var]])

  tex <- c(
    paste0("\\begin{tabular}{l", paste(rep(" c", n_hor), collapse = ""), "}"),
    "\\toprule",
    paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"),
    "\\midrule"
  )

  panel_idx <- 0
  for (pname in names(panels)) {
    panel_idx <- panel_idx + 1
    pd <- panels[[pname]]
    plabel <- paste0("Panel ", LETTERS[panel_idx], ": ", pname, " GS")

    tex <- c(tex, paste0("\\multicolumn{", n_hor + 1, "}{l}{\\textit{",
                         plabel, "}} \\\\"))

    for (ob in names(outcome_labels)) {
      ob_data <- pd |> filter(outcome == ob)
      if (nrow(ob_data) == 0) next

      cells <- character(); se_cells <- character()
      for (h in HORIZON_LABS) {
        r <- ob_data |> filter(horizon == h)
        if (nrow(r) == 0) {
          cells <- c(cells, ""); se_cells <- c(se_cells, "")
        } else {
          cells <- c(cells, paste0(fmt(r$coef), add_stars(r$pvalue)))
          se_cells <- c(se_cells, paste0("(", fmt(r$se), ")"))
        }
      }
      tex <- c(tex,
        paste0(outcome_labels[ob], " & ", paste(cells, collapse = " & "), " \\\\"),
        paste0(" & ", paste(se_cells, collapse = " & "), " \\\\"),
        "\\addlinespace"
      )
    }

    if (nrow(pd) > 0) {
      tex <- c(tex,
        paste0("$N$ & \\multicolumn{", n_hor, "}{c}{", pd$n_obs[1], "} \\\\"),
        paste0("Player-events & \\multicolumn{", n_hor, "}{c}{", pd$n_units[1], "} \\\\"),
        "\\addlinespace"
      )
    }
  }

  tex <- c(tex,
    "\\midrule",
    paste0("Event FE & \\multicolumn{", n_hor, "}{c}{Yes} \\\\"),
    paste0("Horizon FE & \\multicolumn{", n_hor, "}{c}{Yes} \\\\"),
    "\\bottomrule", "\\end{tabular}"
  )

  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

build_robustness_tex(ver_res, "table_verified_stacked.tex")

# --- 4b. First-LL-only restriction --------------------------------------------
gs_atp_first <- gs_atp |> filter(n_prior_gs_ll_won == 0)
gs_wta_first <- gs_wta |> filter(n_prior_gs_ll_won == 0)

message("  ATP first-LL: N = ", nrow(gs_atp_first),
        " (LL: ", sum(gs_atp_first$got_ll), ")")
message("  WTA first-LL: N = ", nrow(gs_wta_first),
        " (LL: ", sum(gs_wta_first$got_ll), ")")

first_res_atp <- run_robustness_stacked(gs_atp_first, "ATP")
first_res_wta <- run_robustness_stacked(gs_wta_first, "WTA")

slog("- First-LL ATP: N = ", nrow(gs_atp_first), " (LL: ", sum(gs_atp_first$got_ll), ")")
slog("- First-LL WTA: N = ", nrow(gs_wta_first), " (LL: ", sum(gs_wta_first$got_ll), ")")
for (r_df in list(first_res_atp, first_res_wta)) {
  for (i in seq_len(nrow(r_df))) {
    r <- r_df[i, ]
    if (r$outcome == "points_change") {
      slog("- First-LL ", r$tour, " pts @ ", r$horizon,
           ": coef = ", fmt(r$coef), " (", fmt(r$se), "), p = ", fmt(r$pvalue, 3))
    }
  }
}
slog("")

# Build first-LL tables (one per tour)
build_single_panel_tex <- function(res_df, filename) {
  n_hor <- length(HORIZON_LABS)

  tex <- c(
    paste0("\\begin{tabular}{l", paste(rep(" c", n_hor), collapse = ""), "}"),
    "\\toprule",
    paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"),
    "\\midrule"
  )

  for (ob in names(outcome_labels)) {
    ob_data <- res_df |> filter(outcome == ob)
    if (nrow(ob_data) == 0) next

    cells <- character(); se_cells <- character()
    for (h in HORIZON_LABS) {
      r <- ob_data |> filter(horizon == h)
      if (nrow(r) == 0) {
        cells <- c(cells, ""); se_cells <- c(se_cells, "")
      } else {
        cells <- c(cells, paste0(fmt(r$coef), add_stars(r$pvalue)))
        se_cells <- c(se_cells, paste0("(", fmt(r$se), ")"))
      }
    }
    tex <- c(tex,
      paste0(outcome_labels[ob], " & ", paste(cells, collapse = " & "), " \\\\"),
      paste0(" & ", paste(se_cells, collapse = " & "), " \\\\"),
      "\\addlinespace"
    )
  }

  if (nrow(res_df) > 0) {
    tex <- c(tex, "\\midrule",
      paste0("$N$ (stacked) & \\multicolumn{", n_hor, "}{c}{", res_df$n_obs[1], "} \\\\"),
      paste0("Player-events & \\multicolumn{", n_hor, "}{c}{", res_df$n_units[1], "} \\\\"),
      paste0("Event FE & \\multicolumn{", n_hor, "}{c}{Yes} \\\\"),
      paste0("Horizon FE & \\multicolumn{", n_hor, "}{c}{Yes} \\\\")
    )
  }

  tex <- c(tex, "\\bottomrule", "\\end{tabular}")
  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

if (nrow(first_res_atp) > 0) {
  build_single_panel_tex(first_res_atp, "table_firstll_stacked_atp.tex")
}
if (nrow(first_res_wta) > 0) {
  build_single_panel_tex(first_res_wta, "table_firstll_stacked_wta.tex")
}


# ==============================================================================
# FIX 5: EVENT STUDY FIGURES FROM CORRECTED COEFFICIENTS
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 5: EVENT STUDY FIGURES (CORRECTED SPEC)")
message(strrep("=", 70))
slog("## FIX 5: Event study figures from corrected stacked coefficients\n")

build_es_from_coefs <- function(res_df, tour_label) {
  pts_df <- res_df |> filter(outcome == "points_change")
  if (nrow(pts_df) == 0) return(NULL)

  # Add baseline (0w) with zero effect
  baseline <- tibble(
    tour = tour_label, outcome = "points_change", horizon = "0w",
    coef = 0, se = 0, pvalue = NA_real_,
    n_obs_total = pts_df$n_obs_total[1],
    n_obs_horizon = pts_df$n_obs_horizon[1],
    n_units = pts_df$n_units[1]
  )

  bind_rows(baseline, pts_df) |>
    mutate(
      weeks = as.numeric(gsub("w", "", horizon)),
      ci_lo = coef - 1.96 * se,
      ci_hi = coef + 1.96 * se
    )
}

es_atp <- build_es_from_coefs(stacked_res_atp, "ATP")
es_wta <- build_es_from_coefs(stacked_res_wta, "WTA")

make_es_figure <- function(es_data, tour_label) {
  if (is.null(es_data) || nrow(es_data) == 0) return(NULL)

  ggplot(es_data, aes(x = weeks, y = coef)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.2, fill = col_treat) +
    geom_line(color = col_treat, linewidth = 0.9) +
    geom_point(color = col_treat, size = 3) +
    scale_x_continuous(breaks = c(0, 4, 8, 12, 26, 52),
                       labels = c("0", "4", "8", "12", "26", "52")) +
    labs(x = "Weeks after qualifying loss",
         y = "Effect on ranking points (stacked estimate)") +
    theme_paper()
}

p_es_atp <- make_es_figure(es_atp, "ATP")
p_es_wta <- make_es_figure(es_wta, "WTA")

if (!is.null(p_es_atp)) {
  ggsave(file.path(FIGURES_DIR, "fig_event_study_atp.pdf"), p_es_atp,
         width = 7, height = 5, device = cairo_pdf)
  message("  Saved: fig_event_study_atp.pdf")
}
if (!is.null(p_es_wta)) {
  ggsave(file.path(FIGURES_DIR, "fig_event_study_wta.pdf"), p_es_wta,
         width = 7, height = 5, device = cairo_pdf)
  message("  Saved: fig_event_study_wta.pdf")
}

# Log event study coefficients
for (h in c(4, 8, 12, 26, 52)) {
  r <- es_atp |> filter(weeks == h)
  if (nrow(r) > 0) {
    slog("- ATP event study beta_", h, "w = ", fmt(r$coef),
         " [", fmt(r$ci_lo), ", ", fmt(r$ci_hi), "]")
  }
}
for (h in c(4, 8, 12, 26, 52)) {
  r <- es_wta |> filter(weeks == h)
  if (nrow(r) > 0) {
    slog("- WTA event study beta_", h, "w = ", fmt(r$coef),
         " [", fmt(r$ci_lo), ", ", fmt(r$ci_hi), "]")
  }
}
slog("")


# ==============================================================================
# FIX 6: NON-GS MIRROR TABLES
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 6: NON-GS MIRROR TABLES")
message(strrep("=", 70))
slog("## FIX 6: Non-GS mirror tables (hetero, dose) with corrected spec\n")

# --- 6a. Non-GS Heterogeneity (IV, split-sample) -----------------------------
# For IV with interactions, split-sample is the standard approach
prepare_hetero_nongs <- function(data) {
  med_pts <- median(data$pre_rank_pts, na.rm = TRUE)
  med_age <- median(data$player_age, na.rm = TRUE)

  data |> mutate(
    rank_above_med = as.integer(pre_rank_pts >= med_pts),
    age_above_med  = as.integer(player_age >= med_age),
    prior_ll_dum   = as.integer(had_prior_ll == 1)
  )
}

nongs_atp_h <- prepare_hetero_nongs(nongs_iv_atp)
nongs_wta_h <- prepare_hetero_nongs(nongs_iv_wta)

run_hetero_nongs_iv <- function(data, tour_label) {
  results <- list()

  zpre <- "pre_rank_pts + pre_rank_pts_sq + player_age"

  subgroups <- list(
    list(label = "High ranking pts", sub = data |> filter(rank_above_med == 1), dim = "Ranking"),
    list(label = "Low ranking pts",  sub = data |> filter(rank_above_med == 0), dim = "Ranking"),
    list(label = "Older",            sub = data |> filter(age_above_med == 1),  dim = "Age"),
    list(label = "Younger",          sub = data |> filter(age_above_med == 0),  dim = "Age"),
    list(label = "Had prior LL",     sub = data |> filter(prior_ll_dum == 1),   dim = "Prior LL"),
    list(label = "No prior LL",      sub = data |> filter(prior_ll_dum == 0),   dim = "Prior LL")
  )

  for (sg in subgroups) {
    sg_data <- sg$sub
    if (nrow(sg_data) < 50) next

    # Stack this subgroup
    stacked_sg <- stack_horizons(sg_data, outcomes_base)
    stacked_sg <- stacked_sg |> filter(!is.na(peer_component))

    for (ob in outcomes_base) {
      if (!ob %in% names(stacked_sg)) next
      d <- stacked_sg |> filter(!is.na(.data[[ob]]),
                                 !is.na(pre_rank_pts), !is.na(player_age),
                                 !is.na(peer_component))
      if (nrow(d) < 50) next

      # Corrected IV spec: horizon-invariant controls
      fml <- as.formula(paste0(
        ob, " ~ pre_rank_pts + pre_rank_pts_sq + player_age",
        " | slam_year + horizon",
        " | got_ll:horizon ~ peer_component:horizon"
      ))

      fit <- tryCatch(feols(fml, data = d, vcov = ~player_id),
                      error = function(e) NULL)
      if (is.null(fit)) next

      cf <- coef(fit); se_v <- sqrt(diag(vcov(fit)))

      for (h_lab in HORIZON_LABS) {
        cn <- paste0("fit_got_ll:horizon", h_lab)
        if (cn %in% names(cf)) {
          pv <- 2 * pnorm(-abs(cf[cn] / se_v[cn]))
          results[[paste0(sg$label, "_", ob, "_", h_lab)]] <- tibble(
            tour = tour_label, outcome = ob, horizon = h_lab,
            dimension = sg$dim, term = sg$label,
            coef = cf[cn], se = se_v[cn], pvalue = pv,
            n_obs = nrow(d), n_units = n_distinct(sg_data$player_id)
          )
        }
      }
    }
  }
  bind_rows(results)
}

hetero_nongs_atp <- run_hetero_nongs_iv(nongs_atp_h, "ATP non-GS")
hetero_nongs_wta <- run_hetero_nongs_iv(nongs_wta_h, "WTA non-GS")

# Log key results
for (tour_df in list(hetero_nongs_atp, hetero_nongs_wta)) {
  if (nrow(tour_df) == 0) next
  pts26 <- tour_df |> filter(outcome == "points_change", horizon == "26w")
  for (i in seq_len(nrow(pts26))) {
    r <- pts26[i, ]
    slog("- ", r$tour, " pts@26w | ", r$dimension, " | ", r$term,
         ": coef = ", fmt(r$coef), ", SE = ", fmt(r$se), ", p = ", fmt(r$pvalue, 3))
  }
}
slog("")

# Build non-GS hetero tables
build_hetero_nongs_tex <- function(res_df, tour_label, filename) {
  n_hor <- length(HORIZON_LABS)

  pts_df <- res_df |> filter(outcome == "points_change")
  if (nrow(pts_df) == 0) {
    message("  No points_change results for ", tour_label, " -- skipping table")
    return(invisible(NULL))
  }

  tex <- c(
    paste0("\\begin{tabular}{l", paste(rep(" c", n_hor), collapse = ""), "}"),
    "\\toprule",
    paste0("Subgroup & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"),
    "\\midrule"
  )

  dims <- unique(pts_df$dimension)
  for (dm in dims) {
    dim_data <- pts_df |> filter(dimension == dm)
    tex <- c(tex, paste0("\\multicolumn{", n_hor + 1, "}{l}{\\textit{", dm, "}} \\\\"))

    for (tm in unique(dim_data$term)) {
      cells <- character(); se_cells <- character()
      for (h in HORIZON_LABS) {
        r <- dim_data |> filter(term == tm, horizon == h)
        if (nrow(r) == 0) {
          cells <- c(cells, ""); se_cells <- c(se_cells, "")
        } else {
          cells <- c(cells, paste0(fmt(r$coef), add_stars(r$pvalue)))
          se_cells <- c(se_cells, paste0("(", fmt(r$se), ")"))
        }
      }
      tex <- c(tex,
        paste0(tm, " & ", paste(cells, collapse = " & "), " \\\\"),
        paste0(" & ", paste(se_cells, collapse = " & "), " \\\\")
      )
    }
    tex <- c(tex, "\\addlinespace")
  }

  tex <- c(tex,
    "\\midrule",
    paste0("Year FE & \\multicolumn{", n_hor, "}{c}{Yes} \\\\"),
    paste0("Horizon FE & \\multicolumn{", n_hor, "}{c}{Yes} \\\\"),
    paste0("IV (peer component) & \\multicolumn{", n_hor, "}{c}{Yes} \\\\"),
    "\\bottomrule", "\\end{tabular}"
  )

  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

if (nrow(hetero_nongs_atp) > 0) {
  build_hetero_nongs_tex(hetero_nongs_atp, "ATP non-GS", "table_hetero_stacked_nongs_atp.tex")
}
if (nrow(hetero_nongs_wta) > 0) {
  build_hetero_nongs_tex(hetero_nongs_wta, "WTA non-GS", "table_hetero_stacked_nongs_wta.tex")
}

# --- 6b. Non-GS Dose (IV, split-sample) --------------------------------------
run_dose_nongs_iv <- function(data, tour_label) {
  results <- list()
  zpre <- "pre_rank_pts + pre_rank_pts_sq + player_age"

  dose_groups <- list(
    list(label = "0 MD wins",  sub = data |> filter(is.na(md_matches_won) | md_matches_won == 0)),
    list(label = "1 MD win",   sub = data |> filter(!is.na(md_matches_won) & md_matches_won == 1)),
    list(label = "2+ MD wins", sub = data |> filter(!is.na(md_matches_won) & md_matches_won >= 2))
  )

  for (dg in dose_groups) {
    dg_data <- dg$sub
    if (nrow(dg_data) < 100) next  # need substantial N for stacked IV

    # Variation check: need variation in both got_ll and peer_component
    if (length(unique(dg_data$got_ll)) < 2) next
    if (var(dg_data$peer_component, na.rm = TRUE) < 1e-6) next

    # Use per-horizon IV instead of stacked IV (safer for small subsamples)
    for (h in HORIZONS) {
      h_lab <- paste0(h, "w")
      for (ob in outcomes_base) {
        col <- paste0(ob, "_", h, "w")
        if (!col %in% names(dg_data)) next
        ok <- !is.na(dg_data[[col]]) & !is.na(dg_data$peer_component) &
              !is.na(dg_data$pre_rank_pts) & !is.na(dg_data$player_age)
        if (sum(ok) < 50) next

        fit <- tryCatch(
          feols(as.formula(paste0(col, " ~ ", zpre,
                                  " | year | got_ll ~ peer_component")),
                data = dg_data[ok, ], vcov = ~player_id),
          error = function(e) NULL
        )
        if (is.null(fit) || !"fit_got_ll" %in% names(coef(fit))) next

        cf <- coef(fit)["fit_got_ll"]
        se_val <- sqrt(vcov(fit)["fit_got_ll", "fit_got_ll"])
        pv <- 2 * pnorm(-abs(cf / se_val))

        results[[paste0(dg$label, "_", ob, "_", h_lab)]] <- tibble(
          tour = tour_label, outcome = ob, horizon = h_lab,
          term = dg$label,
          coef = cf, se = se_val, pvalue = pv,
          n_obs = sum(ok), n_units = n_distinct(dg_data$player_id[ok])
        )
      }
    }
  }
  bind_rows(results)
}

dose_nongs_atp <- run_dose_nongs_iv(nongs_iv_atp, "ATP non-GS")
dose_nongs_wta <- run_dose_nongs_iv(nongs_iv_wta, "WTA non-GS")

# Build non-GS dose table
build_dose_nongs_tex <- function(atp_res, wta_res, filename) {
  n_hor <- length(HORIZON_LABS)

  tex <- c(
    paste0("\\begin{tabular}{l", paste(rep(" c", n_hor), collapse = ""), "}"),
    "\\toprule",
    paste0("Dose subgroup & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"),
    "\\midrule"
  )

  for (panel in list(list(df = atp_res, label = "Panel A: ATP non-GS IV"),
                     list(df = wta_res, label = "Panel B: WTA non-GS IV"))) {
    pd <- panel$df |> filter(outcome == "points_change")
    if (nrow(pd) == 0) next
    tex <- c(tex, paste0("\\multicolumn{", n_hor + 1, "}{l}{\\textit{",
                         panel$label, "}} \\\\"))

    for (tm in unique(pd$term)) {
      cells <- character(); se_cells <- character()
      for (h in HORIZON_LABS) {
        r <- pd |> filter(term == tm, horizon == h)
        if (nrow(r) == 0) {
          cells <- c(cells, ""); se_cells <- c(se_cells, "")
        } else {
          cells <- c(cells, paste0(fmt(r$coef), add_stars(r$pvalue)))
          se_cells <- c(se_cells, paste0("(", fmt(r$se), ")"))
        }
      }
      tex <- c(tex,
        paste0(tm, " & ", paste(cells, collapse = " & "), " \\\\"),
        paste0(" & ", paste(se_cells, collapse = " & "), " \\\\")
      )
    }
    tex <- c(tex, "\\addlinespace")
  }

  tex <- c(tex,
    "\\midrule",
    paste0("Year FE & \\multicolumn{", n_hor, "}{c}{Yes} \\\\"),
    paste0("Horizon FE & \\multicolumn{", n_hor, "}{c}{Yes} \\\\"),
    paste0("IV (peer component) & \\multicolumn{", n_hor, "}{c}{Yes} \\\\"),
    "\\bottomrule", "\\end{tabular}"
  )

  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

if (nrow(dose_nongs_atp) > 0 || nrow(dose_nongs_wta) > 0) {
  build_dose_nongs_tex(dose_nongs_atp, dose_nongs_wta, "table_dose_stacked_nongs.tex")
}


# ==============================================================================
# FIX 8: SAVE ALL COMPUTED OBJECTS
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 8: SAVING ALL RDS OBJECTS")
message(strrep("=", 70))

all_results <- list(
  # FIX 1: Stacked dynamic (corrected)
  stacked_gs_atp       = stacked_res_atp,
  stacked_gs_wta       = stacked_res_wta,
  stacked_nongs_atp    = stacked_nongs_res_atp,
  stacked_nongs_wta    = stacked_nongs_res_wta,

  # FIX 1: Model objects
  stacked_models_atp   = stacked_models_atp,
  stacked_models_wta   = stacked_models_wta,

  # FIX 2: Heterogeneity (horizon-specific)
  hetero_gs_atp        = hetero_res_atp,
  hetero_gs_wta        = hetero_res_wta,
  hetero_models_atp    = hetero_out_atp$models,
  hetero_models_wta    = hetero_out_wta$models,

  # FIX 3: Dose (horizon-specific)
  dose_gs_atp          = dose_res_atp,
  dose_gs_wta          = dose_res_wta,
  dose_models_atp      = dose_out_atp$models,
  dose_models_wta      = dose_out_wta$models,

  # FIX 4: Robustness
  verified_res         = ver_res,
  firstll_atp          = first_res_atp,
  firstll_wta          = first_res_wta,

  # FIX 5: Event study data
  event_study_atp      = es_atp,
  event_study_wta      = es_wta,

  # FIX 6: Non-GS mirrors
  hetero_nongs_atp     = hetero_nongs_atp,
  hetero_nongs_wta     = hetero_nongs_wta,
  dose_nongs_atp       = dose_nongs_atp,
  dose_nongs_wta       = dose_nongs_wta
)

saveRDS(all_results, file.path(CLEANED_DIR, "final_fixes_results.rds"))
message("  Saved: Data/cleaned/final_fixes_results.rds")

# Also save individual result sets for downstream convenience
saveRDS(list(atp = stacked_res_atp, wta = stacked_res_wta,
             nongs_atp = stacked_nongs_res_atp, nongs_wta = stacked_nongs_res_wta),
        file.path(CLEANED_DIR, "stacked_dynamic_results.rds"))

saveRDS(list(atp = hetero_res_atp, wta = hetero_res_wta),
        file.path(CLEANED_DIR, "hetero_interacted_results.rds"))

saveRDS(list(hetero_nongs_atp = hetero_nongs_atp, hetero_nongs_wta = hetero_nongs_wta,
             dose_nongs_atp = dose_nongs_atp, dose_nongs_wta = dose_nongs_wta),
        file.path(CLEANED_DIR, "nongs_hetero_dose_results.rds"))


# ==============================================================================
# WRITE SUMMARY
# ==============================================================================
message("\n", strrep("=", 70))
message("WRITING SUMMARY")
message(strrep("=", 70))

summary_header <- c(
  "# Final Fixes Summary (20_final_fixes.R)",
  paste0("Generated: ", Sys.time()),
  "",
  "## Key changes from script 18/19:",
  "- FIX 1: Controls are now horizon-INVARIANT (Gamma, not gamma_h)",
  "- FIX 2: Hetero tables have horizon-specific rows (beta_h, delta_h)",
  "- FIX 3: Dose tables have horizon-specific rows",
  "- FIX 4: Robustness tables (verified, first-LL) use corrected spec",
  "- FIX 5: Event study figures plot regression coefficients with 95% CI",
  "- FIX 6: Non-GS mirror tables use corrected stacked IV spec",
  "- FIX 7: Shared helpers extracted to scripts/R/utils.R",
  "- FIX 8: All computed objects saved via saveRDS()",
  ""
)

writeLines(c(summary_header, summary_log),
           file.path(OUTPUT_DIR, "final_fixes_summary.md"))
message("  Saved: Output/final_fixes_summary.md")

message("\n", strrep("=", 70))
message("ALL FINAL FIXES COMPLETE")
message(strrep("=", 70))
message("Tables saved:")
message("  - table_dynamic_stacked_atp.tex")
message("  - table_dynamic_stacked_wta.tex")
message("  - table_dynamic_stacked_nongs_atp.tex")
message("  - table_dynamic_stacked_nongs_wta.tex")
message("  - table_hetero_stacked_atp.tex")
message("  - table_hetero_stacked_wta.tex")
message("  - table_dose_stacked.tex")
message("  - table_verified_stacked.tex")
message("  - table_firstll_stacked_atp.tex")
message("  - table_firstll_stacked_wta.tex")
message("  - table_hetero_stacked_nongs_atp.tex")
message("  - table_hetero_stacked_nongs_wta.tex")
message("  - table_dose_stacked_nongs.tex")
message("Figures saved:")
message("  - fig_event_study_atp.pdf")
message("  - fig_event_study_wta.pdf")
message("Data saved:")
message("  - Data/cleaned/final_fixes_results.rds")
message("  - Data/cleaned/stacked_dynamic_results.rds")
message("  - Data/cleaned/hetero_interacted_results.rds")
message("  - Data/cleaned/nongs_hetero_dose_results.rds")
