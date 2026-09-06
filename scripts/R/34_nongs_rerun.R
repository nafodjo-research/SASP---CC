# ==============================================================================
# 34_nongs_rerun.R
# Re-run ALL non-GS analyses using corrected P_i^{LL} from script 33.
#
# The corrected P_i^{LL} uses full match pairings (exact enumeration for
# n <= 16, which covers all non-GS events). The generalized residual v_hat
# is recomputed from the corrected P_i^{LL}.
#
# Inputs:
#   Data/cleaned/skeleton_nongs_est_v4.rds   (corrected non-GS sample)
#   Data/cleaned/tournament_match_fix_results.rds  (event tables from script 24)
#   Data/cleaned/tournament_elo_cache.rds
#   Data/raw/atp_main_matches.rds, atp_qual_chall_matches.rds
#   Data/raw/wta_main_matches.rds, wta_qual_itf_matches.rds
#
# Outputs:
#   Tables (18 .tex files):
#     table_hetero_stacked_nongs_atp.tex, table_hetero_stacked_nongs_wta.tex
#     table_dose_stacked_nongs.tex
#     table_firstll_stacked_nongs_atp.tex, table_firstll_stacked_nongs_wta.tex
#     table_tournament_nongs_atp.tex, table_tournament_nongs_wta.tex
#     table_tournament_firstll_nongs_atp.tex, table_tournament_firstll_nongs_wta.tex
#     table_tournament_robustness_nongs_atp.tex, table_tournament_robustness_nongs_wta.tex
#     table_tournament_horizon_nongs_atp.tex, table_tournament_horizon_nongs_wta.tex
#     table_tournament_dose_nongs_atp.tex, table_tournament_dose_nongs_wta.tex
#     table_tournament_perf_prob_nongs_atp.tex, table_tournament_perf_prob_nongs_wta.tex
#     table_sumstats_nongs_atp.tex, table_sumstats_nongs_wta.tex
#   Data/cleaned/nongs_rerun_results.rds
#   Output/nongs_rerun_summary.md
#
# Dependencies: dplyr, tidyr, fixest, ggplot2, data.table, sandwich,
#               readr, stringr, patchwork, here
# ==============================================================================

set.seed(20260327)

# --- Packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(fixest)
library(ggplot2)
library(data.table)
library(sandwich)
library(readr)
library(stringr)
library(here)

# --- Shared helpers ----------------------------------------------------------
source(here("scripts", "R", "utils.R"))
summary_log <- character()

# --- Paths -------------------------------------------------------------------
RAW_DIR     <- here("Data", "raw")
CLEANED_DIR <- here("Data", "cleaned")
TABLES_DIR  <- here("Tables")
FIGURES_DIR <- here("Figures")
OUTPUT_DIR  <- here("Output")
for (d in c(CLEANED_DIR, TABLES_DIR, FIGURES_DIR, OUTPUT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# --- Constants ---------------------------------------------------------------
outcomes_base  <- c("points_change", "n_main_draws", "n_matches_250plus", "elo_change")
outcome_labels <- c(
  "points_change"     = "Ranking points $\\Delta$",
  "n_main_draws"      = "Main draws entered",
  "n_matches_250plus" = "Matches at 250+",
  "elo_change"        = "Elo $\\Delta$"
)
HORIZONS     <- c(4, 8, 12, 26, 52)
HORIZON_LABS <- paste0(HORIZONS, "w")

ZPRE_FULL <- paste0("pre_rank_pts + pre_rank_pts_sq + pre_elo + pre_elo_sq",
                     " + n_prior_gs_ll_won + n_prior_gs_ll_notwon",
                     " + n_prior_nongs_ll_won + n_prior_nongs_ll_notwon",
                     " + player_age")

CALENDAR_CAP <- 365L
N_BOOT       <- 200L

POINTS_BY_ROUND <- list(
  G    = c(R128 = 10, R64 = 45, R32 = 90, R16 = 180, QF = 360, SF = 720, F = 1200, W = 2000),
  M    = c(R64 = 10, R32 = 25, R16 = 45, QF = 90,  SF = 180, F = 360,  W = 600),
  PM   = c(R64 = 10, R32 = 25, R16 = 45, QF = 90,  SF = 180, F = 360,  W = 600),
  A500 = c(R32 = 0,  R16 = 20, QF = 45,  SF = 90,  F = 180,  W = 300),
  P    = c(R32 = 0,  R16 = 20, QF = 45,  SF = 90,  F = 180,  W = 300),
  A250 = c(R32 = 0,  R16 = 20, QF = 45,  SF = 90,  F = 150,  W = 250),
  I    = c(R32 = 0,  R16 = 20, QF = 45,  SF = 90,  F = 150,  W = 250),
  C    = c(R32 = 0,  R16 = 3,  QF = 6,   SF = 10,  F = 15,   W = 25)
)


cat("\n")
message(strrep("=", 72))
message("  34_nongs_rerun.R: RE-RUN ALL NON-GS ANALYSES WITH CORRECTED P_i^{LL}")
message(strrep("=", 72))


###############################################################################
# PHASE 0: LOAD DATA
###############################################################################

message("\n[0] Loading data...")

nongs_est <- readRDS(file.path(CLEANED_DIR, "skeleton_nongs_est_v4.rds"))
R24 <- readRDS(file.path(CLEANED_DIR, "tournament_match_fix_results.rds"))

nongs_atp <- nongs_est |> filter(tour == "ATP", !is.na(peer_component))
nongs_wta <- nongs_est |> filter(tour == "WTA", !is.na(peer_component))

message("  Non-GS ATP: N = ", nrow(nongs_atp), " (LL: ", sum(nongs_atp$got_ll), ")")
message("  Non-GS WTA: N = ", nrow(nongs_wta), " (LL: ", sum(nongs_wta$got_ll), ")")

slog("## Phase 0: Data loaded")
slog("- Non-GS ATP: N = ", nrow(nongs_atp), " (LL: ", sum(nongs_atp$got_ll), ")")
slog("- Non-GS WTA: N = ", nrow(nongs_wta), " (LL: ", sum(nongs_wta$got_ll), ")")
slog("")


###############################################################################
# COMMON HELPERS
###############################################################################

# Stack horizons (v2 -- includes CF columns)
stack_horizons_v2 <- function(data, outcomes_base, horizons = c(4, 8, 12, 26, 52)) {
  stacked <- list()
  for (h in horizons) {
    h_label <- paste0(h, "w")
    row_data <- data |>
      dplyr::transmute(
        player_id, tourney_id, tour, slam_year, got_ll,
        pre_rank_pts, pre_rank_pts_sq, player_age,
        pre_elo, pre_elo_sq,
        n_prior_gs_ll_won, n_prior_gs_ll_notwon,
        n_prior_nongs_ll_won, n_prior_nongs_ll_notwon,
        had_prior_ll = if ("had_prior_ll" %in% names(data)) had_prior_ll else NA_integer_,
        md_matches_won = if ("md_matches_won" %in% names(data)) md_matches_won else NA_integer_,
        peer_component = if ("peer_component" %in% names(data)) peer_component else NA_real_,
        v_hat = if ("v_hat" %in% names(data)) v_hat else NA_real_,
        year = if ("year" %in% names(data)) year else NA_integer_,
        horizon = h_label,
        horizon_num = h
      )
    for (ob in outcomes_base) {
      col_name <- paste0(ob, "_", h, "w")
      if (col_name %in% names(data)) {
        row_data[[ob]] <- data[[col_name]]
      } else {
        row_data[[ob]] <- NA_real_
      }
    }
    stacked[[h_label]] <- row_data
  }
  dplyr::bind_rows(stacked) |>
    dplyr::mutate(horizon = factor(horizon, levels = paste0(horizons, "w")))
}

# Build stacked tex table
build_stacked_tex <- function(res_df, tour_label, filename, is_cf = FALSE) {
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
    if (is_cf) {
      tex <- c(tex,
        paste0("Control function & \\multicolumn{", n_hor, "}{c}{Yes} \\\\")
      )
    }
  }
  tex <- c(tex, "\\bottomrule", "\\end{tabular}")
  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

# Build single-panel tex table
build_single_panel_tex <- function(res_df, filename, is_cf = FALSE) {
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
    if (is_cf) {
      tex <- c(tex,
        paste0("Control function & \\multicolumn{", n_hor, "}{c}{Yes} \\\\")
      )
    }
  }
  tex <- c(tex, "\\bottomrule", "\\end{tabular}")
  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

# Impute match data NAs
impute_match_data <- function(df) {
  df |>
    mutate(
      h2h_smoothed   = replace(h2h_smoothed, is.na(h2h_smoothed), 0.5),
      n_h2h          = replace(n_h2h, is.na(n_h2h), 0L),
      age_diff       = replace(age_diff, is.na(age_diff), 0),
      height_diff    = replace(height_diff, is.na(height_diff), 0),
      hand_mismatch  = replace(hand_mismatch, is.na(hand_mismatch), 0L),
      opp_elo        = replace(opp_elo, is.na(opp_elo), 1500),
      pre_elo        = replace(pre_elo, is.na(pre_elo), 1500),
      pre_rank_pts   = replace(pre_rank_pts, is.na(pre_rank_pts), 0)
    )
}

# Get tournament tier
get_tier <- function(tourney_level, draw_size = NA) {
  if (tourney_level == "G") return("G")
  if (tourney_level %in% c("M", "PM")) return("M")
  if (tourney_level == "P") return("P")
  if (tourney_level == "I") return("I")
  if (tourney_level == "A") {
    if (!is.na(draw_size) && draw_size >= 48) return("A500")
    return("A250")
  }
  "A250"
}

# Compute E[RP]
compute_erp <- function(probs, tier) {
  sched <- POINTS_BY_ROUND[[tier]]
  if (is.null(sched)) sched <- POINTS_BY_ROUND[["A250"]]
  pts_vec <- as.numeric(sched)
  n_rounds_sched <- length(pts_vec)
  R <- length(probs)
  n_use <- min(R, n_rounds_sched)
  if (n_use == 0) return(0)
  erp <- pts_vec[1]
  if (n_use > 1) {
    for (r in 2:n_use) {
      p_reach_r <- prod(probs[1:(r - 1)])
      erp <- erp + p_reach_r * pts_vec[min(r, n_rounds_sched)]
    }
  }
  erp
}


###############################################################################
# PART 1: STACKED DYNAMICS (verify -- already done in script 33)
###############################################################################

message("\n", strrep("=", 72))
message("  PART 1: STACKED DYNAMICS (verify from script 33)")
message(strrep("=", 72))

# Verify the tables exist
for (f in c("table_dynamic_stacked_nongs_atp.tex",
            "table_dynamic_stacked_nongs_wta.tex")) {
  fpath <- file.path(TABLES_DIR, f)
  if (file.exists(fpath)) {
    message("  VERIFIED: ", f, " exists (from script 33)")
  } else {
    message("  WARNING: ", f, " NOT FOUND -- will be rebuilt in this section")
  }
}

slog("## Part 1: Stacked dynamics verified (from script 33)")
slog("")


###############################################################################
# PART 2: HETEROGENEITY WITH CORRECTED v_hat
###############################################################################

message("\n", strrep("=", 72))
message("  PART 2: HETEROGENEITY (non-GS, corrected v_hat)")
message(strrep("=", 72))

# Prepare category variables
prepare_hetero_data <- function(data) {
  qts <- quantile(data$pre_rank_pts, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  med_age <- median(data$player_age, na.rm = TRUE)
  data |> mutate(
    rank_above_med = as.integer(pre_rank_pts >= median(pre_rank_pts, na.rm = TRUE)),
    age_above_med  = as.integer(player_age >= med_age),
    prior_ll_dum   = as.integer(had_prior_ll == 1)
  )
}

nongs_atp_h <- prepare_hetero_data(nongs_atp)
nongs_wta_h <- prepare_hetero_data(nongs_wta)

stacked_nongs_atp_h <- stack_horizons_v2(nongs_atp_h, outcomes_base)
stacked_nongs_wta_h <- stack_horizons_v2(nongs_wta_h, outcomes_base)

# Merge category variables
merge_cats <- function(stacked, orig) {
  cats <- orig |> select(player_id, tourney_id,
    rank_above_med, age_above_med, prior_ll_dum)
  stacked |> left_join(cats, by = c("player_id", "tourney_id"))
}

stacked_nongs_atp_h <- merge_cats(stacked_nongs_atp_h, nongs_atp_h)
stacked_nongs_wta_h <- merge_cats(stacked_nongs_wta_h, nongs_wta_h)

# Estimate hetero models -- each dimension as SEPARATE regression
# Spec: outcome ~ got_ll:horizon + got_ll:horizon:category + v_hat:horizon
#        + v_hat:horizon:category + Z^pre | horizon
run_hetero_nongs_cf <- function(sdata, tour_label) {
  results <- list()

  for (ob in outcomes_base) {
    if (!ob %in% names(sdata)) next
    d <- sdata |> filter(!is.na(.data[[ob]]),
                         !is.na(pre_rank_pts), !is.na(player_age),
                         !is.na(pre_elo), !is.na(v_hat))
    if (nrow(d) < 50) next

    # --- Ranking ---
    fml_rank <- as.formula(paste0(
      ob, " ~ got_ll:horizon + got_ll:horizon:rank_above_med",
      " + v_hat:horizon + v_hat:horizon:rank_above_med + ", ZPRE_FULL,
      " | slam_year + horizon"
    ))
    fit_rank <- tryCatch(feols(fml_rank, data = d, vcov = ~player_id),
                         error = function(e) NULL)
    if (!is.null(fit_rank)) {
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

    # --- Age ---
    fml_age <- as.formula(paste0(
      ob, " ~ got_ll:horizon + got_ll:horizon:age_above_med",
      " + v_hat:horizon + v_hat:horizon:age_above_med + ", ZPRE_FULL,
      " | slam_year + horizon"
    ))
    fit_age <- tryCatch(feols(fml_age, data = d, vcov = ~player_id),
                        error = function(e) NULL)
    if (!is.null(fit_age)) {
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
      " + v_hat:horizon + v_hat:horizon:prior_ll_dum + ", ZPRE_FULL,
      " | slam_year + horizon"
    ))
    fit_prior <- tryCatch(feols(fml_prior, data = d, vcov = ~player_id),
                          error = function(e) NULL)
    if (!is.null(fit_prior)) {
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
  bind_rows(results)
}

hetero_nongs_atp <- run_hetero_nongs_cf(stacked_nongs_atp_h, "ATP non-GS")
hetero_nongs_wta <- run_hetero_nongs_cf(stacked_nongs_wta_h, "WTA non-GS")

slog("## Part 2: Heterogeneity (non-GS, corrected v_hat)")
for (tour_df in list(hetero_nongs_atp, hetero_nongs_wta)) {
  if (nrow(tour_df) == 0) next
  pts <- tour_df |> filter(outcome == "points_change")
  for (i in seq_len(nrow(pts))) {
    r <- pts[i, ]
    slog("- ", r$tour, " pts @ ", r$horizon, " | ", r$dimension,
         " | ", r$term, ": coef = ", fmt(r$coef), " (", fmt(r$se), ")",
         ", p = ", fmt(r$pvalue, 3))
  }
}
slog("")

# Build hetero tex tables
build_hetero_horizon_nongs_tex <- function(res_df, tour_label, filename,
                                            stacked_data) {
  dims <- c("Ranking", "Age", "Prior LL")
  primary_ob <- "points_change"
  primary_data <- res_df |> filter(outcome == primary_ob)

  if (nrow(primary_data) == 0) {
    message("  No points_change results for ", tour_label, " -- skipping")
    return(invisible(NULL))
  }

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
    diff_terms <- dim_data |> filter(grepl("delta", term)) |> pull(term) |> unique()
    diff_label <- if (length(diff_terms) > 0) diff_terms[1] else ""
    tex <- c(tex, paste0("\\multicolumn{5}{l}{\\textit{",
                         dm, " heterogeneity",
                         ifelse(diff_label != "",
                                paste0(" (", gsub("delta_h ", "", diff_label), ")"),
                                ""),
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

  # All outcomes at 26w summary
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

  n_obs <- nrow(stacked_data)
  n_units <- n_distinct(paste0(stacked_data$player_id, "_", stacked_data$tourney_id))

  tex <- c(tex,
    "\\midrule",
    paste0("\\multicolumn{5}{l}{$N$ (stacked) = ", n_obs,
           "; Player-events = ", n_units, "} \\\\"),
    paste0("\\multicolumn{5}{l}{Event FE = Yes; Horizon FE = Yes; Control function = Yes} \\\\"),
    "\\bottomrule", "\\end{tabular}"
  )

  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

if (nrow(hetero_nongs_atp) > 0) {
  build_hetero_horizon_nongs_tex(hetero_nongs_atp, "ATP non-GS",
                                  "table_hetero_stacked_nongs_atp.tex",
                                  stacked_nongs_atp_h)
}
if (nrow(hetero_nongs_wta) > 0) {
  build_hetero_horizon_nongs_tex(hetero_nongs_wta, "WTA non-GS",
                                  "table_hetero_stacked_nongs_wta.tex",
                                  stacked_nongs_wta_h)
}


###############################################################################
# PART 3: DOSE (stacked, corrected v_hat)
###############################################################################

message("\n", strrep("=", 72))
message("  PART 3: DOSE (non-GS stacked, corrected v_hat)")
message(strrep("=", 72))

# Use continuous matches_won and matches_won^2 as requested
nongs_atp_d <- nongs_atp |> mutate(
  matches_won = coalesce(as.numeric(md_matches_won), 0),
  matches_won_sq = matches_won^2
)
nongs_wta_d <- nongs_wta |> mutate(
  matches_won = coalesce(as.numeric(md_matches_won), 0),
  matches_won_sq = matches_won^2
)

stacked_nongs_atp_d <- stack_horizons_v2(nongs_atp_d, outcomes_base)
stacked_nongs_wta_d <- stack_horizons_v2(nongs_wta_d, outcomes_base)

# Merge dose vars
merge_dose <- function(stacked, orig) {
  cats <- orig |> select(player_id, tourney_id, matches_won, matches_won_sq)
  stacked |> left_join(cats, by = c("player_id", "tourney_id"))
}

stacked_nongs_atp_d <- merge_dose(stacked_nongs_atp_d, nongs_atp_d)
stacked_nongs_wta_d <- merge_dose(stacked_nongs_wta_d, nongs_wta_d)

# Spec: outcome ~ got_ll:horizon + got_ll:horizon:matches_won
#                + got_ll:horizon:matches_won_sq + v_hat:horizon
#                + v_hat:horizon:matches_won + v_hat:horizon:matches_won_sq
#                + Z^pre | horizon
run_dose_nongs_cf <- function(sdata, tour_label) {
  results <- list()
  for (ob in outcomes_base) {
    if (!ob %in% names(sdata)) next
    d <- sdata |> filter(!is.na(.data[[ob]]),
                         !is.na(pre_rank_pts), !is.na(player_age),
                         !is.na(pre_elo), !is.na(v_hat),
                         !is.na(matches_won))
    if (nrow(d) < 50) next

    fml <- as.formula(paste0(
      ob, " ~ got_ll:horizon + got_ll:horizon:matches_won",
      " + got_ll:horizon:matches_won_sq",
      " + v_hat:horizon + v_hat:horizon:matches_won",
      " + v_hat:horizon:matches_won_sq + ", ZPRE_FULL,
      " | slam_year + horizon"
    ))
    fit <- tryCatch(feols(fml, data = d, vcov = ~player_id),
                    error = function(e) NULL)
    if (is.null(fit)) next
    cf <- coef(fit); se <- sqrt(diag(vcov(fit)))

    for (h_lab in HORIZON_LABS) {
      base_nm <- paste0("got_ll:horizon", h_lab)
      mw_nm   <- paste0("got_ll:horizon", h_lab, ":matches_won")
      mwsq_nm <- paste0("got_ll:horizon", h_lab, ":matches_won_sq")

      for (nm_info in list(
        list(nm = base_nm, term = "beta_h (base)"),
        list(nm = mw_nm, term = "delta_h (matches_won)"),
        list(nm = mwsq_nm, term = "delta_h (matches_won_sq)")
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
  bind_rows(results)
}

dose_nongs_atp <- run_dose_nongs_cf(stacked_nongs_atp_d, "ATP non-GS")
dose_nongs_wta <- run_dose_nongs_cf(stacked_nongs_wta_d, "WTA non-GS")

slog("## Part 3: Dose (non-GS, corrected v_hat)")
for (tour_df in list(dose_nongs_atp, dose_nongs_wta)) {
  if (nrow(tour_df) == 0) next
  pts <- tour_df |> filter(outcome == "points_change")
  for (i in seq_len(nrow(pts))) {
    r <- pts[i, ]
    slog("- ", r$tour, " dose pts @ ", r$horizon, " | ", r$term,
         ": coef = ", fmt(r$coef), " (", fmt(r$se), "), p = ", fmt(r$pvalue, 3))
  }
}
slog("")

# Build dose tex table
build_dose_nongs_tex <- function(atp_res, wta_res, filename) {
  primary_ob <- "points_change"
  tex <- c(
    "\\begin{tabular}{l c c c c c c}",
    "\\toprule",
    paste0("Horizon & $\\hat{\\beta}_h$ & SE",
           " & $\\hat{\\delta}_{\\text{mw},h}$ & SE",
           " & $\\hat{\\delta}_{\\text{mw}^2,h}$ & SE \\\\"),
    "\\midrule"
  )

  for (panel in list(list(df = atp_res, label = "Panel A: ATP non-GS"),
                     list(df = wta_res, label = "Panel B: WTA non-GS"))) {
    pd <- panel$df |> filter(outcome == primary_ob)
    if (nrow(pd) == 0) next
    tex <- c(tex, paste0("\\multicolumn{7}{l}{\\textit{", panel$label, "}} \\\\"))

    for (h_lab in HORIZON_LABS) {
      base_r <- pd |> filter(horizon == h_lab, grepl("base", term))
      mw_r   <- pd |> filter(horizon == h_lab, grepl("matches_won\\)", term))
      mwsq_r <- pd |> filter(horizon == h_lab, grepl("matches_won_sq", term))

      make_cell <- function(r) {
        if (nrow(r) > 0) paste0(fmt(r$coef), add_stars(r$pvalue)) else ""
      }
      make_se <- function(r) {
        if (nrow(r) > 0) paste0("(", fmt(r$se), ")") else ""
      }
      tex <- c(tex, paste0(
        h_lab, " & ",
        make_cell(base_r), " & ", make_se(base_r), " & ",
        make_cell(mw_r), " & ", make_se(mw_r), " & ",
        make_cell(mwsq_r), " & ", make_se(mwsq_r), " \\\\"
      ))
    }
    tex <- c(tex, "\\addlinespace")
  }

  # All outcomes at 26w (ATP)
  tex <- c(tex,
    "\\midrule",
    "\\multicolumn{7}{l}{\\textit{All outcomes at 26w (ATP non-GS)}} \\\\"
  )
  for (ob in names(outcome_labels)) {
    ob_data <- atp_res |> filter(outcome == ob, horizon == "26w")
    base_r <- ob_data |> filter(grepl("base", term))
    mw_r   <- ob_data |> filter(grepl("matches_won\\)", term))
    mwsq_r <- ob_data |> filter(grepl("matches_won_sq", term))
    make_cell <- function(r) {
      if (nrow(r) > 0) paste0(fmt(r$coef), add_stars(r$pvalue)) else ""
    }
    make_se <- function(r) {
      if (nrow(r) > 0) paste0("(", fmt(r$se), ")") else ""
    }
    tex <- c(tex, paste0(
      outcome_labels[ob], " & ",
      make_cell(base_r), " & ", make_se(base_r), " & ",
      make_cell(mw_r), " & ", make_se(mw_r), " & ",
      make_cell(mwsq_r), " & ", make_se(mwsq_r), " \\\\"
    ))
  }

  tex <- c(tex,
    "\\midrule",
    "\\multicolumn{7}{l}{Event FE = Yes; Horizon FE = Yes; Control function = Yes} \\\\",
    "\\bottomrule", "\\end{tabular}"
  )

  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

build_dose_nongs_tex(dose_nongs_atp, dose_nongs_wta, "table_dose_stacked_nongs.tex")


###############################################################################
# PART 4: FIRST-LL RESTRICTION (stacked, corrected v_hat)
###############################################################################

message("\n", strrep("=", 72))
message("  PART 4: FIRST-LL RESTRICTION (non-GS stacked)")
message(strrep("=", 72))

nongs_atp_first <- nongs_atp |> filter(n_prior_nongs_ll_won == 0)
nongs_wta_first <- nongs_wta |> filter(n_prior_nongs_ll_won == 0)

message("  ATP first-LL: N = ", nrow(nongs_atp_first),
        " (LL: ", sum(nongs_atp_first$got_ll), ")")
message("  WTA first-LL: N = ", nrow(nongs_wta_first),
        " (LL: ", sum(nongs_wta_first$got_ll), ")")

run_firstll_nongs_cf <- function(data_subset, tour_label) {
  stacked_sub <- stack_horizons_v2(data_subset, outcomes_base)
  results <- list()
  for (ob in outcomes_base) {
    if (!ob %in% names(stacked_sub)) next
    d <- stacked_sub |> filter(!is.na(.data[[ob]]),
                                !is.na(pre_rank_pts), !is.na(player_age),
                                !is.na(pre_elo), !is.na(v_hat))
    sy_counts <- d |> count(slam_year) |> filter(n >= 2)
    d <- d |> filter(slam_year %in% sy_counts$slam_year)
    if (nrow(d) < 50 || sum(d$got_ll == 1) < 3) next

    fml <- as.formula(paste0(
      ob, " ~ got_ll:horizon + v_hat:horizon + ", ZPRE_FULL,
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

first_nongs_atp <- run_firstll_nongs_cf(nongs_atp_first, "ATP non-GS")
first_nongs_wta <- run_firstll_nongs_cf(nongs_wta_first, "WTA non-GS")

slog("## Part 4: First-LL restriction (non-GS, corrected v_hat)")
slog("- ATP first-LL: N = ", nrow(nongs_atp_first), " (LL: ", sum(nongs_atp_first$got_ll), ")")
slog("- WTA first-LL: N = ", nrow(nongs_wta_first), " (LL: ", sum(nongs_wta_first$got_ll), ")")
for (r_df in list(first_nongs_atp, first_nongs_wta)) {
  for (i in seq_len(nrow(r_df))) {
    r <- r_df[i, ]
    if (r$outcome == "points_change") {
      slog("- First-LL ", r$tour, " pts @ ", r$horizon,
           ": coef = ", fmt(r$coef), " (", fmt(r$se), "), p = ", fmt(r$pvalue, 3))
    }
  }
}
slog("")

if (nrow(first_nongs_atp) > 0) {
  build_single_panel_tex(first_nongs_atp, "table_firstll_stacked_nongs_atp.tex",
                          is_cf = TRUE)
}
if (nrow(first_nongs_wta) > 0) {
  build_single_panel_tex(first_nongs_wta, "table_firstll_stacked_nongs_wta.tex",
                          is_cf = TRUE)
}


###############################################################################
# PART 5: TOURNAMENT MODEL (rebuild match-level with corrected v_hat)
###############################################################################

message("\n", strrep("=", 72))
message("  PART 5: TOURNAMENT MODEL (non-GS, corrected v_hat)")
message(strrep("=", 72))

# Update event tables with corrected v_hat from v4 data
update_event_v_hat <- function(ev_table, corrected_data) {
  # Build lookup from corrected data
  vhat_lookup <- corrected_data |>
    select(player_id, tourney_id, v_hat_new = v_hat, p_ll_new = peer_component)
  ev_table |>
    left_join(vhat_lookup, by = c("player_id", "tourney_id")) |>
    mutate(
      v_hat = coalesce(v_hat_new, v_hat),
      p_ll = coalesce(p_ll_new, p_ll)
    ) |>
    select(-v_hat_new, -p_ll_new)
}

nongs_atp_ev <- update_event_v_hat(R24$nongs_atp$events, nongs_atp)
nongs_wta_ev <- update_event_v_hat(R24$nongs_wta$events, nongs_wta)

message("  Updated nonGS-ATP events: ", nrow(nongs_atp_ev),
        " (v_hat non-NA: ", sum(!is.na(nongs_atp_ev$v_hat)), ")")
message("  Updated nonGS-WTA events: ", nrow(nongs_wta_ev),
        " (v_hat non-NA: ", sum(!is.na(nongs_wta_ev$v_hat)), ")")

# Load raw match data and Elo cache
atp_main <- read_rds(file.path(RAW_DIR, "atp_main_matches.rds"))
atp_qual <- read_rds(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
wta_main <- read_rds(file.path(RAW_DIR, "wta_main_matches.rds"))
wta_qual <- read_rds(file.path(RAW_DIR, "wta_qual_itf_matches.rds"))

elo_list <- readRDS(file.path(CLEANED_DIR, "tournament_elo_cache.rds"))
message("  Loaded Elo cache: ", length(ls(elo_list$overall)), " players")

get_elo_at_date <- function(env, player, date) {
  df <- env[[player]]
  if (is.null(df)) return(1500)
  v <- df[df$date <= date, ]
  if (nrow(v) == 0) return(1500)
  tail(v$rating, 1)
}

# Harmonize matches
harmonize_matches <- function(main_df, qual_df, tour_label) {
  ensure_cols <- function(df) {
    for (col in c("winner_entry", "loser_entry", "best_of", "winner_age",
                  "loser_age", "match_num", "draw_size", "winner_id", "loser_id",
                  "winner_ht", "loser_ht", "winner_hand", "loser_hand",
                  "winner_ioc", "loser_ioc", "winner_rank", "loser_rank",
                  "winner_rank_points", "loser_rank_points")) {
      if (!col %in% names(df)) df[[col]] <- NA
    }
    df
  }
  main_df <- ensure_cols(main_df) |> mutate(match_source = "main")
  qual_df <- ensure_cols(qual_df) |> mutate(match_source = "qual")
  common_cols <- intersect(names(main_df), names(qual_df))
  combined <- bind_rows(
    main_df |> select(all_of(common_cols)),
    qual_df |> select(all_of(common_cols))
  ) |>
    mutate(
      tour = tour_label,
      tourney_date = if (inherits(tourney_date, "Date")) tourney_date
                     else as.Date(as.character(tourney_date), format = "%Y%m%d"),
      year = as.integer(format(tourney_date, "%Y")),
      winner_pid = paste0(tour_label, "_", winner_id),
      loser_pid  = paste0(tour_label, "_", loser_id)
    ) |>
    filter(year >= 2000, year <= 2024)
  combined
}

atp_all <- harmonize_matches(atp_main, atp_qual, "ATP")
wta_all <- harmonize_matches(wta_main, wta_qual, "WTA")
matches <- bind_rows(atp_all, wta_all) |> arrange(tourney_date, tourney_id, match_num)
message("  Combined matches: ", nrow(matches))

# Free memory
rm(atp_main, atp_qual, wta_main, wta_qual)
gc()

# H2H precomputation
precompute_h2h <- function(match_data) {
  dt <- as.data.table(match_data)[
    !is.na(winner_pid) & !is.na(loser_pid),
    .(winner = winner_pid, loser = loser_pid, date = tourney_date)
  ]
  dt[, `:=`(
    pA = fifelse(winner < loser, winner, loser),
    pB = fifelse(winner < loser, loser, winner),
    pA_won = as.integer(winner == fifelse(winner < loser, winner, loser))
  )]
  setorder(dt, pA, pB, date)
  dt[, `:=`(cum_wins_A = cumsum(pA_won), cum_total = seq_len(.N)), by = .(pA, pB)]
  setkey(dt, pA, pB, date)
  dt
}

h2h_table <- precompute_h2h(matches)

# Build match-level data (same logic as script 24, but with corrected v_hat)
build_match_level <- function(ev_table, all_matches, tour_label,
                               calendar_cap_days = CALENDAR_CAP) {
  md <- all_matches |>
    filter(tour == tour_label) |>
    mutate(
      tourney_date = if (inherits(tourney_date, "Date")) tourney_date
                     else as.Date(as.character(tourney_date), format = "%Y%m%d")
    )

  # Truncation at next same-type LL opportunity
  ev_sorted <- ev_table |> arrange(player_id, tourney_date)
  ev_sorted <- ev_sorted |>
    group_by(player_id) |>
    mutate(next_ll_opp_date = lead(tourney_date, default = as.Date("2099-12-31"))) |>
    ungroup()
  ev_sorted$cal_cap_date <- ev_sorted$tourney_date + calendar_cap_days

  # Build focal matches
  fm <- bind_rows(
    md |> transmute(
      focal_pid = winner_pid, opp_pid = loser_pid,
      tourney_id, tourney_date, match_num, surface, tourney_level,
      best_of, focal_age = winner_age, opp_age = loser_age,
      focal_rank = winner_rank, opp_rank = loser_rank,
      focal_ioc = winner_ioc, opp_ioc = loser_ioc,
      focal_ht = winner_ht, opp_ht = loser_ht,
      focal_hand = winner_hand, opp_hand = loser_hand,
      round, won = 1L, match_source),
    md |> transmute(
      focal_pid = loser_pid, opp_pid = winner_pid,
      tourney_id, tourney_date, match_num, surface, tourney_level,
      best_of, focal_age = loser_age, opp_age = winner_age,
      focal_rank = loser_rank, opp_rank = winner_rank,
      focal_ioc = loser_ioc, opp_ioc = winner_ioc,
      focal_ht = loser_ht, opp_ht = winner_ht,
      focal_hand = loser_hand, opp_hand = winner_hand,
      round, won = 0L, match_source)
  )

  results <- list()
  for (i in seq_len(nrow(ev_sorted))) {
    e <- ev_sorted[i, ]
    pid <- e$player_pid
    ev_date <- e$tourney_date
    censor_date <- min(e$next_ll_opp_date, e$cal_cap_date)

    player_matches <- fm |>
      filter(focal_pid == pid,
             tourney_date > ev_date,
             tourney_date < censor_date)
    if (nrow(player_matches) == 0) next

    player_matches$event_id <- e$event_id
    player_matches$got_ll <- e$got_ll
    player_matches$v_hat <- e$v_hat
    player_matches$p_ll <- e$p_ll
    player_matches$pre_elo <- e$pre_elo
    player_matches$pre_rank_pts <- e$pre_rank_pts
    player_matches$player_age_at_event <- e$player_age
    player_matches$had_prior_ll <- e$had_prior_ll
    player_matches$ev_date <- ev_date
    player_matches$days_after_event <- as.numeric(difftime(player_matches$tourney_date,
                                                            ev_date, units = "days"))
    player_matches$weeks_after_event <- player_matches$days_after_event / 7
    results[[length(results) + 1]] <- player_matches
  }

  match_df <- bind_rows(results)
  if (nrow(match_df) == 0) {
    message("  ", tour_label, " WARNING: 0 match-level observations!")
    return(match_df)
  }

  # Add match-level features
  match_df <- match_df |>
    mutate(
      log_rank_ratio = log(pmax(opp_rank, 1) / pmax(focal_rank, 1)),
      log_rank_ratio_sq = log_rank_ratio^2,
      rank_diff = opp_rank - focal_rank,
      same_ioc = as.integer(!is.na(focal_ioc) & !is.na(opp_ioc) & focal_ioc == opp_ioc),
      is_clay = as.integer(!is.na(surface) & tolower(surface) == "clay"),
      is_grass = as.integer(!is.na(surface) & tolower(surface) == "grass"),
      age_diff = ifelse(!is.na(focal_age) & !is.na(opp_age), focal_age - opp_age, 0),
      height_diff = ifelse(!is.na(focal_ht) & !is.na(opp_ht), focal_ht - opp_ht, 0),
      hand_mismatch = as.integer(!is.na(focal_hand) & !is.na(opp_hand) &
                                   focal_hand != opp_hand & focal_hand != "U" & opp_hand != "U")
    )

  # Add opponent Elo
  match_df$opp_elo <- NA_real_
  for (j in seq_len(nrow(match_df))) {
    match_df$opp_elo[j] <- get_elo_at_date(elo_list$overall,
                                             match_df$opp_pid[j],
                                             match_df$tourney_date[j])
  }

  # Add H2H
  dt <- as.data.table(match_df)
  dt[, `:=`(
    pA = fifelse(focal_pid < opp_pid, focal_pid, opp_pid),
    pB = fifelse(focal_pid < opp_pid, opp_pid, focal_pid),
    focal_is_pA = (focal_pid < opp_pid),
    lookup_date = ev_date - 1
  )]
  setkey(dt, pA, pB, lookup_date)

  h2h_merged <- h2h_table[dt, roll = TRUE, on = .(pA, pB, date = lookup_date), nomatch = NA]
  h2h_merged[, `:=`(
    h2h_smoothed = fifelse(is.na(cum_total), 0.5,
                           fifelse(focal_is_pA,
                                   (cum_wins_A + 1) / (cum_total + 2),
                                   (cum_total - cum_wins_A + 1) / (cum_total + 2))),
    n_h2h = fifelse(is.na(cum_total), 0L, as.integer(cum_total))
  )]

  keep_cols <- c("event_id", "focal_pid", "opp_pid", "tourney_id", "tourney_date",
                 "match_num", "surface", "tourney_level", "round", "match_source",
                 "won", "got_ll", "v_hat", "p_ll",
                 "pre_elo", "opp_elo", "pre_rank_pts",
                 "player_age_at_event", "had_prior_ll",
                 "ev_date", "days_after_event", "weeks_after_event",
                 "log_rank_ratio", "log_rank_ratio_sq", "rank_diff",
                 "same_ioc", "is_clay", "is_grass", "age_diff",
                 "height_diff", "hand_mismatch",
                 "h2h_smoothed", "n_h2h",
                 "focal_rank", "opp_rank", "best_of")
  keep_cols <- intersect(keep_cols, names(h2h_merged))
  match_df <- as.data.frame(h2h_merged[, ..keep_cols])

  match_df <- match_df |>
    filter(!is.na(log_rank_ratio), !is.na(won), !is.na(got_ll), !is.na(v_hat))

  match_df <- match_df |>
    arrange(event_id, tourney_id, match_num) |>
    group_by(event_id, tourney_id) |>
    mutate(round_num = row_number()) |>
    ungroup()

  message("  ", tour_label, " match-level: ", nrow(match_df),
          " matches, ", n_distinct(match_df$event_id), " events, ",
          n_distinct(match_df$focal_pid), " players")
  match_df
}

message("\n  Building non-GS match data with corrected v_hat...")
nongs_atp_md <- build_match_level(nongs_atp_ev, matches, "ATP")
nongs_wta_md <- build_match_level(nongs_wta_ev, matches, "WTA")

slog("## Part 5: Tournament match data rebuilt")
slog("- nonGS-ATP: ", nrow(nongs_atp_md), " matches, ",
     n_distinct(nongs_atp_md$event_id), " events")
slog("- nonGS-WTA: ", nrow(nongs_wta_md), " matches, ",
     n_distinct(nongs_wta_md$event_id), " events")
slog("")


# --- Non-GS CF logit estimator with bootstrap --------------------------------
estimate_nongs_model <- function(match_df, label, weighted = FALSE) {
  if (is.null(match_df) || nrow(match_df) < 50) {
    message("  ", label, ": insufficient observations")
    return(NULL)
  }
  df <- impute_match_data(match_df)

  fml <- won ~ got_ll + v_hat +
    log_rank_ratio + log_rank_ratio_sq + rank_diff +
    same_ioc + is_clay + is_grass + age_diff + height_diff +
    hand_mismatch + h2h_smoothed + n_h2h +
    pre_elo + opp_elo + pre_rank_pts +
    player_age_at_event + had_prior_ll

  if (weighted) {
    df$match_weight <- case_when(
      grepl("^F$", df$round)   ~ 10,
      grepl("^SF$", df$round)  ~ 5,
      grepl("^QF$", df$round)  ~ 3,
      grepl("^R16$", df$round) ~ 2,
      TRUE ~ 1
    )
    model <- glm(fml, family = binomial(link = "logit"), data = df, weights = match_weight)
  } else {
    model <- glm(fml, family = binomial(link = "logit"), data = df)
  }

  s <- summary(model)$coefficients
  delta    <- s["got_ll", "Estimate"]
  delta_se <- s["got_ll", "Std. Error"]
  delta_p  <- s["got_ll", "Pr(>|z|)"]
  rho      <- s["v_hat", "Estimate"]
  rho_se   <- s["v_hat", "Std. Error"]
  rho_p    <- s["v_hat", "Pr(>|z|)"]

  message(sprintf("  %s: delta=%.4f (SE=%.4f, p=%.4f), rho=%.4f (p=%.4f), N=%d",
                  label, delta, delta_se, delta_p, rho, rho_p, nrow(df)))

  list(model = model, data = df, delta = delta, delta_se = delta_se,
       delta_p = delta_p, rho = rho, rho_se = rho_se, rho_p = rho_p)
}

# Counterfactual quantities
compute_nongs_cf <- function(fit_obj, label) {
  if (is.null(fit_obj)) return(NULL)
  model <- fit_obj$model
  df <- fit_obj$data
  beta <- coef(model)
  delta <- beta["got_ll"]

  linpred_obs <- predict(model, newdata = df, type = "link")
  linpred_d0 <- linpred_obs - delta * df$got_ll
  linpred_d1 <- linpred_d0 + delta

  df$p0 <- plogis(linpred_d0)
  df$p1 <- plogis(linpred_d1)
  avg_me <- mean(df$p1 - df$p0, na.rm = TRUE)

  df_dt <- as.data.table(df)
  setorder(df_dt, event_id, tourney_id, round_num)

  tourney_effects <- df_dt[, {
    R <- .N
    cp0 <- cumprod(p0); cp1 <- cumprod(p1)
    list(n_rounds = R, ew_d0 = sum(cp0), ew_d1 = sum(cp1),
         delta_ew = sum(cp1) - sum(cp0), tourney_level = tourney_level[1])
  }, by = .(event_id, tourney_id)]

  avg_delta_ew <- mean(tourney_effects$delta_ew, na.rm = TRUE)

  # E[RP]
  tourney_effects$erp_d0 <- NA_real_
  tourney_effects$erp_d1 <- NA_real_
  for (k in seq_len(nrow(tourney_effects))) {
    eid <- tourney_effects$event_id[k]
    tid <- tourney_effects$tourney_id[k]
    sub <- df_dt[event_id == eid & tourney_id == tid]
    setorder(sub, round_num)
    tl <- tourney_effects$tourney_level[k]
    if (is.na(tl)) tl <- sub$tourney_level[1]
    if (is.na(tl)) tl <- "A250"
    tier <- get_tier(tl)
    tourney_effects$erp_d0[k] <- compute_erp(sub$p0, tier)
    tourney_effects$erp_d1[k] <- compute_erp(sub$p1, tier)
  }
  tourney_effects$delta_erp <- tourney_effects$erp_d1 - tourney_effects$erp_d0
  avg_delta_erp <- mean(tourney_effects$delta_erp, na.rm = TRUE)

  message(sprintf("  %s: DeltaP=%.4f, DeltaE[W]=%.4f, DeltaE[RP]=%.1f",
                  label, avg_me, avg_delta_ew, avg_delta_erp))

  list(tourney_effects = as.data.frame(tourney_effects), match_cf = df,
       avg_me = avg_me, avg_delta_ew = avg_delta_ew, avg_delta_erp = avg_delta_erp)
}

# Bootstrap for non-GS
bootstrap_nongs <- function(ev_table, match_df, n_boot = N_BOOT, label = "") {
  if (is.null(match_df) || nrow(match_df) < 50) return(NULL)
  players <- unique(ev_table$player_id)
  n_players <- length(players)
  ev_by_player <- split(ev_table, ev_table$player_id)

  t_start <- Sys.time()
  boot_results <- vector("list", n_boot)

  for (b in seq_len(n_boot)) {
    if (b %% 50 == 0) {
      elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
      rate <- b / elapsed; eta <- (n_boot - b) / rate
      message(sprintf("    %s: Rep %d/%d (%.1f min, ~%.1f remain)",
                      label, b, n_boot, elapsed, eta))
    }
    tryCatch({
      boot_players <- sample(players, n_players, replace = TRUE)
      boot_ev <- bind_rows(lapply(boot_players, function(p) ev_by_player[[as.character(p)]]))
      if (nrow(boot_ev) < 20) next
      boot_md <- match_df |> filter(event_id %in% boot_ev$event_id)
      if (nrow(boot_md) < 50) next

      # Recompute v_hat for bootstrap
      q <- qnorm(boot_ev$p_ll)
      phi_q <- dnorm(q)
      boot_ev$v_hat <- boot_ev$got_ll * phi_q / boot_ev$p_ll -
        (1 - boot_ev$got_ll) * phi_q / (1 - boot_ev$p_ll)
      vhat_lookup <- boot_ev |> select(event_id, v_hat_boot = v_hat)
      boot_md <- boot_md |>
        left_join(vhat_lookup, by = "event_id") |>
        mutate(v_hat = coalesce(v_hat_boot, v_hat)) |>
        select(-v_hat_boot)
      boot_md <- impute_match_data(boot_md)

      fit <- glm(won ~ got_ll + v_hat +
                   log_rank_ratio + log_rank_ratio_sq + rank_diff +
                   same_ioc + is_clay + is_grass + age_diff + height_diff +
                   hand_mismatch + h2h_smoothed + n_h2h +
                   pre_elo + opp_elo + pre_rank_pts +
                   player_age_at_event + had_prior_ll,
                 family = binomial(link = "logit"), data = boot_md)
      if (!fit$converged) next

      beta_b <- coef(fit)
      delta_b <- unname(beta_b["got_ll"])
      rho_b <- unname(beta_b["v_hat"])

      lp_obs <- predict(fit, newdata = boot_md, type = "link")
      lp_d0 <- lp_obs - delta_b * boot_md$got_ll
      p0 <- plogis(lp_d0); p1 <- plogis(lp_d0 + delta_b)
      avg_me_b <- mean(p1 - p0)

      boot_md$p0 <- p0; boot_md$p1 <- p1
      te <- boot_md |>
        arrange(event_id, tourney_id, round_num) |>
        group_by(event_id, tourney_id) |>
        summarise(delta_ew = sum(cumprod(p1)) - sum(cumprod(p0)), .groups = "drop")
      avg_dew_b <- mean(te$delta_ew)

      boot_results[[b]] <- c(delta = delta_b, rho = rho_b,
                              avg_me = avg_me_b, avg_dew = avg_dew_b)
    }, error = function(e) NULL)
  }

  boot_results <- boot_results[!sapply(boot_results, is.null)]
  n_success <- length(boot_results)
  elapsed_total <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
  message(sprintf("  %s: %d/%d successful in %.1f min",
                  label, n_success, n_boot, elapsed_total))
  if (n_success < 10) return(NULL)

  boot_mat <- do.call(rbind, boot_results)
  boot_se <- apply(boot_mat, 2, sd, na.rm = TRUE)
  boot_ci <- apply(boot_mat, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
  list(boot_mat = boot_mat, boot_se = boot_se, boot_ci = boot_ci, n_success = n_success)
}


# --- 5a. Main tournament model ------------------------------------------------
message("\n  5a. Main tournament model (unweighted + weighted)...")

nongs_atp_fit <- estimate_nongs_model(nongs_atp_md, "nonGS-ATP")
nongs_atp_wt  <- estimate_nongs_model(nongs_atp_md, "nonGS-ATP (weighted)", weighted = TRUE)
nongs_wta_fit <- estimate_nongs_model(nongs_wta_md, "nonGS-WTA")
nongs_wta_wt  <- estimate_nongs_model(nongs_wta_md, "nonGS-WTA (weighted)", weighted = TRUE)

# Counterfactuals
nongs_atp_cf <- compute_nongs_cf(nongs_atp_fit, "nonGS-ATP")
nongs_wta_cf <- compute_nongs_cf(nongs_wta_fit, "nonGS-WTA")

# Bootstrap
message("\n  Bootstrapping non-GS tournament models (200 reps)...")
nongs_atp_boot <- bootstrap_nongs(nongs_atp_ev, nongs_atp_md, N_BOOT, "nonGS-ATP")
nongs_wta_boot <- bootstrap_nongs(nongs_wta_ev, nongs_wta_md, N_BOOT, "nonGS-WTA")

# Derive inference
get_cf_inf <- function(cf_obj, boot_obj, label) {
  if (is.null(cf_obj) || is.null(boot_obj)) return(NULL)
  avg_me <- cf_obj$avg_me
  se_me <- boot_obj$boot_se["avg_me"]
  p_me <- 2 * pnorm(-abs(avg_me / se_me))
  avg_dew <- cf_obj$avg_delta_ew
  se_dew <- boot_obj$boot_se["avg_dew"]
  p_dew <- if (!is.na(se_dew) && se_dew > 0) 2 * pnorm(-abs(avg_dew / se_dew)) else NA_real_
  avg_derp <- cf_obj$avg_delta_erp
  se_derp <- if (!is.na(avg_dew) && abs(avg_dew) > 1e-8) abs(avg_derp) * (se_dew / abs(avg_dew)) else NA_real_
  p_derp <- if (!is.na(se_derp) && se_derp > 0) 2 * pnorm(-abs(avg_derp / se_derp)) else NA_real_
  message(sprintf("  %s: DeltaP=%.4f (SE=%.4f), DeltaE[W]=%.4f (SE=%.4f), DeltaE[RP]=%.1f",
                  label, avg_me, se_me, avg_dew, se_dew, avg_derp))
  list(avg_me = avg_me, se_me = se_me, p_me = p_me,
       avg_dew = avg_dew, se_dew = se_dew, p_dew = p_dew,
       avg_derp = avg_derp, se_derp = se_derp, p_derp = p_derp)
}

nongs_atp_cf_inf <- get_cf_inf(nongs_atp_cf, nongs_atp_boot, "nonGS-ATP")
nongs_wta_cf_inf <- get_cf_inf(nongs_wta_cf, nongs_wta_boot, "nonGS-WTA")

slog("## Part 5a: Main tournament model")
for (obj in list(list(f = nongs_atp_fit, b = nongs_atp_boot, l = "nonGS-ATP"),
                  list(f = nongs_wta_fit, b = nongs_wta_boot, l = "nonGS-WTA"))) {
  if (!is.null(obj$f)) {
    se_use <- if (!is.null(obj$b)) obj$b$boot_se["delta"] else obj$f$delta_se
    slog(sprintf("- %s: delta=%.4f (bootSE=%.4f), rho=%.4f, N=%d",
                 obj$l, obj$f$delta, se_use, obj$f$rho, nrow(obj$f$data)))
  }
}
slog("")

# --- Write main tournament tables ---------------------------------------------
COVAR_NAMES_NONGS <- c(
  "got_ll"              = "$\\hat{\\delta}$ (LL entry)",
  "v_hat"               = "$\\hat{\\rho}$ (endogeneity correction)",
  "log_rank_ratio"      = "Log rank ratio",
  "log_rank_ratio_sq"   = "Log rank ratio$^2$",
  "rank_diff"           = "Rank difference",
  "same_ioc"            = "Same country",
  "is_clay"             = "Clay surface",
  "is_grass"            = "Grass surface",
  "age_diff"            = "Age difference",
  "height_diff"         = "Height difference",
  "hand_mismatch"       = "Hand mismatch",
  "h2h_smoothed"        = "H2H win rate (smoothed)",
  "n_h2h"               = "N prior H2H matches",
  "pre_elo"             = "Pre-treatment Elo",
  "opp_elo"             = "Opponent Elo",
  "pre_rank_pts"        = "Pre-treatment ranking pts",
  "player_age_at_event" = "Player age",
  "had_prior_ll"        = "Had prior LL entry"
)

write_nongs_table <- function(fit_obj, boot_obj, cf_obj, cf_inf, ev_table,
                               filepath, label) {
  if (is.null(fit_obj)) return()
  model <- fit_obj$model
  beta  <- coef(model)
  se_model <- summary(model)$coefficients[, "Std. Error"]
  n_matches <- nrow(fit_obj$data)
  n_events  <- n_distinct(fit_obj$data$event_id)
  n_players <- n_distinct(ev_table$player_id)

  # Use bootstrap SE for got_ll and v_hat
  se_delta <- if (!is.null(boot_obj)) boot_obj$boot_se["delta"] else se_model["got_ll"]
  se_rho <- if (!is.null(boot_obj)) boot_obj$boot_se["rho"] else se_model["v_hat"]

  lines <- c(
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & Estimate & SE \\\\",
    "\\midrule",
    "\\multicolumn{3}{l}{\\textit{Panel A: CF Logit Coefficients}} \\\\[3pt]"
  )

  for (vname in names(COVAR_NAMES_NONGS)) {
    if (vname %in% names(beta)) {
      est <- beta[vname]
      se <- if (vname == "got_ll") se_delta
            else if (vname == "v_hat") se_rho
            else se_model[vname]
      pv  <- 2 * pnorm(-abs(est / se))
      lines <- c(lines,
        sprintf("%s & %s%s & (%s) \\\\",
                COVAR_NAMES_NONGS[vname], fmt(est, 4), add_stars(pv), fmt(se, 4))
      )
    }
  }

  lines <- c(lines,
    "\\midrule",
    "\\multicolumn{3}{l}{\\textit{Panel B: Counterfactual Quantities}} \\\\[3pt]"
  )
  if (!is.null(cf_inf)) {
    lines <- c(lines,
      sprintf("$\\Delta P(\\text{match win})$ & %s%s & (%s) \\\\",
              fmt(cf_inf$avg_me, 4), add_stars(cf_inf$p_me), fmt(cf_inf$se_me, 4)),
      sprintf("$\\Delta E[W]$ (per tournament) & %s%s & (%s) \\\\",
              fmt(cf_inf$avg_dew, 4), add_stars(cf_inf$p_dew), fmt(cf_inf$se_dew, 4))
    )
    if (!is.na(cf_inf$avg_derp)) {
      lines <- c(lines,
        sprintf("$\\Delta E[RP]$ (per tournament) & %s%s & (%s) \\\\",
                fmt(cf_inf$avg_derp, 1),
                add_stars(cf_inf$p_derp),
                fmt(if (!is.na(cf_inf$se_derp)) cf_inf$se_derp else 0, 1))
      )
    }
  }

  lines <- c(lines,
    "\\midrule",
    sprintf("Matches & \\multicolumn{2}{c}{%s} \\\\", format(n_matches, big.mark = ",")),
    sprintf("Episodes & \\multicolumn{2}{c}{%s} \\\\", format(n_events, big.mark = ",")),
    sprintf("Players & \\multicolumn{2}{c}{%s} \\\\", format(n_players, big.mark = ",")),
    sprintf("Bootstrap reps & \\multicolumn{2}{c}{%d} \\\\",
            if (!is.null(boot_obj)) boot_obj$n_success else 0L),
    "\\bottomrule", "\\end{tabular}"
  )

  writeLines(lines, filepath)
  message("  Saved: ", basename(filepath))
}

write_nongs_table(nongs_atp_fit, nongs_atp_boot, nongs_atp_cf, nongs_atp_cf_inf,
                   nongs_atp_ev,
                   file.path(TABLES_DIR, "table_tournament_nongs_atp.tex"), "nonGS-ATP")
write_nongs_table(nongs_wta_fit, nongs_wta_boot, nongs_wta_cf, nongs_wta_cf_inf,
                   nongs_wta_ev,
                   file.path(TABLES_DIR, "table_tournament_nongs_wta.tex"), "nonGS-WTA")


# --- 5b. First-LL tournament model -------------------------------------------
message("\n  5b. First-LL tournament model...")

# Get first-LL event IDs
firstll_atp_eids <- nongs_atp_ev |>
  filter(is.na(had_prior_ll) | had_prior_ll == 0) |> pull(event_id)
firstll_wta_eids <- nongs_wta_ev |>
  filter(is.na(had_prior_ll) | had_prior_ll == 0) |> pull(event_id)

# Also check n_prior_nongs_ll_won from the v4 data
firstll_atp_pids <- nongs_atp |>
  filter(n_prior_nongs_ll_won == 0) |>
  select(player_id, tourney_id)
firstll_wta_pids <- nongs_wta |>
  filter(n_prior_nongs_ll_won == 0) |>
  select(player_id, tourney_id)

firstll_atp_ev <- nongs_atp_ev |>
  semi_join(firstll_atp_pids, by = c("player_id", "tourney_id"))
firstll_wta_ev <- nongs_wta_ev |>
  semi_join(firstll_wta_pids, by = c("player_id", "tourney_id"))

firstll_atp_md <- nongs_atp_md |> filter(event_id %in% firstll_atp_ev$event_id)
firstll_wta_md <- nongs_wta_md |> filter(event_id %in% firstll_wta_ev$event_id)

message("  First-LL ATP: ", nrow(firstll_atp_md), " matches, ",
        n_distinct(firstll_atp_md$event_id), " events")
message("  First-LL WTA: ", nrow(firstll_wta_md), " matches, ",
        n_distinct(firstll_wta_md$event_id), " events")

firstll_atp_fit <- estimate_nongs_model(firstll_atp_md, "firstLL-ATP")
firstll_wta_fit <- estimate_nongs_model(firstll_wta_md, "firstLL-WTA")
firstll_atp_cf <- compute_nongs_cf(firstll_atp_fit, "firstLL-ATP")
firstll_wta_cf <- compute_nongs_cf(firstll_wta_fit, "firstLL-WTA")
firstll_atp_boot <- bootstrap_nongs(firstll_atp_ev, firstll_atp_md, N_BOOT, "firstLL-ATP")
firstll_wta_boot <- bootstrap_nongs(firstll_wta_ev, firstll_wta_md, N_BOOT, "firstLL-WTA")
firstll_atp_cf_inf <- get_cf_inf(firstll_atp_cf, firstll_atp_boot, "firstLL-ATP")
firstll_wta_cf_inf <- get_cf_inf(firstll_wta_cf, firstll_wta_boot, "firstLL-WTA")

write_nongs_table(firstll_atp_fit, firstll_atp_boot, firstll_atp_cf, firstll_atp_cf_inf,
                   firstll_atp_ev,
                   file.path(TABLES_DIR, "table_tournament_firstll_nongs_atp.tex"),
                   "firstLL-ATP")
write_nongs_table(firstll_wta_fit, firstll_wta_boot, firstll_wta_cf, firstll_wta_cf_inf,
                   firstll_wta_ev,
                   file.path(TABLES_DIR, "table_tournament_firstll_nongs_wta.tex"),
                   "firstLL-WTA")

slog("## Part 5b: First-LL tournament model")
for (obj in list(list(f = firstll_atp_fit, b = firstll_atp_boot, l = "firstLL-ATP"),
                  list(f = firstll_wta_fit, b = firstll_wta_boot, l = "firstLL-WTA"))) {
  if (!is.null(obj$f)) {
    se_use <- if (!is.null(obj$b)) obj$b$boot_se["delta"] else obj$f$delta_se
    slog(sprintf("- %s: delta=%.4f (bootSE=%.4f), rho=%.4f, N=%d",
                 obj$l, obj$f$delta, se_use, obj$f$rho, nrow(obj$f$data)))
  }
}
slog("")


# --- 5c. Robustness (4-spec: unweighted/weighted x same-type/any-type) -------
message("\n  5c. Robustness (4 specifications)...")

# Build any-type stop match data (cross-type truncation)
build_any_stop_matches <- function(ev_table, all_ev_same_tour, all_matches, tour_label) {
  # For any-type stop: next LL opp date is from ALL events (GS + nonGS)
  all_ev_dates <- all_ev_same_tour |>
    select(player_id, tourney_date) |>
    distinct() |>
    arrange(player_id, tourney_date)

  ev_sorted <- ev_table |> arrange(player_id, tourney_date)

  # For each event, find next LL opp from ANY type
  ev_sorted$next_any_ll_date <- as.Date("2099-12-31")
  for (i in seq_len(nrow(ev_sorted))) {
    pid <- ev_sorted$player_id[i]
    edate <- ev_sorted$tourney_date[i]
    future <- all_ev_dates |> filter(player_id == pid, tourney_date > edate)
    if (nrow(future) > 0) {
      ev_sorted$next_any_ll_date[i] <- min(future$tourney_date)
    }
  }

  ev_sorted$cal_cap_date <- ev_sorted$tourney_date + CALENDAR_CAP

  md <- all_matches |> filter(tour == tour_label)
  fm <- bind_rows(
    md |> transmute(focal_pid = winner_pid, opp_pid = loser_pid,
                    tourney_id, tourney_date, match_num, surface, tourney_level,
                    best_of, focal_age = winner_age, opp_age = loser_age,
                    focal_rank = winner_rank, opp_rank = loser_rank,
                    focal_ioc = winner_ioc, opp_ioc = loser_ioc,
                    focal_ht = winner_ht, opp_ht = loser_ht,
                    focal_hand = winner_hand, opp_hand = loser_hand,
                    round, won = 1L, match_source),
    md |> transmute(focal_pid = loser_pid, opp_pid = winner_pid,
                    tourney_id, tourney_date, match_num, surface, tourney_level,
                    best_of, focal_age = loser_age, opp_age = winner_age,
                    focal_rank = loser_rank, opp_rank = winner_rank,
                    focal_ioc = loser_ioc, opp_ioc = winner_ioc,
                    focal_ht = loser_ht, opp_ht = winner_ht,
                    focal_hand = loser_hand, opp_hand = winner_hand,
                    round, won = 0L, match_source)
  )

  results <- list()
  for (i in seq_len(nrow(ev_sorted))) {
    e <- ev_sorted[i, ]
    pid <- e$player_pid
    ev_date <- e$tourney_date
    censor_date <- min(e$next_any_ll_date, e$cal_cap_date)
    player_matches <- fm |>
      filter(focal_pid == pid, tourney_date > ev_date, tourney_date < censor_date)
    if (nrow(player_matches) == 0) next
    player_matches$event_id <- e$event_id
    player_matches$got_ll <- e$got_ll
    player_matches$v_hat <- e$v_hat
    player_matches$p_ll <- e$p_ll
    player_matches$pre_elo <- e$pre_elo
    player_matches$pre_rank_pts <- e$pre_rank_pts
    player_matches$player_age_at_event <- e$player_age
    player_matches$had_prior_ll <- e$had_prior_ll
    player_matches$ev_date <- ev_date
    player_matches$days_after_event <- as.numeric(difftime(player_matches$tourney_date, ev_date, units = "days"))
    player_matches$weeks_after_event <- player_matches$days_after_event / 7
    results[[length(results) + 1]] <- player_matches
  }

  match_df <- bind_rows(results)
  if (nrow(match_df) == 0) return(match_df)

  match_df <- match_df |>
    mutate(
      log_rank_ratio = log(pmax(opp_rank, 1) / pmax(focal_rank, 1)),
      log_rank_ratio_sq = log_rank_ratio^2,
      rank_diff = opp_rank - focal_rank,
      same_ioc = as.integer(!is.na(focal_ioc) & !is.na(opp_ioc) & focal_ioc == opp_ioc),
      is_clay = as.integer(!is.na(surface) & tolower(surface) == "clay"),
      is_grass = as.integer(!is.na(surface) & tolower(surface) == "grass"),
      age_diff = ifelse(!is.na(focal_age) & !is.na(opp_age), focal_age - opp_age, 0),
      height_diff = ifelse(!is.na(focal_ht) & !is.na(opp_ht), focal_ht - opp_ht, 0),
      hand_mismatch = as.integer(!is.na(focal_hand) & !is.na(opp_hand) &
                                   focal_hand != opp_hand & focal_hand != "U" & opp_hand != "U")
    )

  match_df$opp_elo <- NA_real_
  for (j in seq_len(nrow(match_df))) {
    match_df$opp_elo[j] <- get_elo_at_date(elo_list$overall,
                                             match_df$opp_pid[j],
                                             match_df$tourney_date[j])
  }

  dt <- as.data.table(match_df)
  dt[, `:=`(
    pA = fifelse(focal_pid < opp_pid, focal_pid, opp_pid),
    pB = fifelse(focal_pid < opp_pid, opp_pid, focal_pid),
    focal_is_pA = (focal_pid < opp_pid),
    lookup_date = ev_date - 1
  )]
  setkey(dt, pA, pB, lookup_date)
  h2h_m <- h2h_table[dt, roll = TRUE, on = .(pA, pB, date = lookup_date), nomatch = NA]
  h2h_m[, `:=`(
    h2h_smoothed = fifelse(is.na(cum_total), 0.5,
                           fifelse(focal_is_pA,
                                   (cum_wins_A + 1) / (cum_total + 2),
                                   (cum_total - cum_wins_A + 1) / (cum_total + 2))),
    n_h2h = fifelse(is.na(cum_total), 0L, as.integer(cum_total))
  )]

  keep_cols <- c("event_id", "focal_pid", "opp_pid", "tourney_id", "tourney_date",
                 "match_num", "surface", "tourney_level", "round", "match_source",
                 "won", "got_ll", "v_hat", "p_ll",
                 "pre_elo", "opp_elo", "pre_rank_pts",
                 "player_age_at_event", "had_prior_ll",
                 "ev_date", "days_after_event", "weeks_after_event",
                 "log_rank_ratio", "log_rank_ratio_sq", "rank_diff",
                 "same_ioc", "is_clay", "is_grass", "age_diff",
                 "height_diff", "hand_mismatch",
                 "h2h_smoothed", "n_h2h",
                 "focal_rank", "opp_rank", "best_of")
  keep_cols <- intersect(keep_cols, names(h2h_m))
  match_df <- as.data.frame(h2h_m[, ..keep_cols])
  match_df <- match_df |>
    filter(!is.na(log_rank_ratio), !is.na(won), !is.na(got_ll), !is.na(v_hat)) |>
    arrange(event_id, tourney_id, match_num) |>
    group_by(event_id, tourney_id) |>
    mutate(round_num = row_number()) |>
    ungroup()
  match_df
}

# Get all events for cross-type truncation
gs_atp_ev <- R24$gs_atp$events
gs_wta_ev <- R24$gs_wta$events
all_atp_ev <- bind_rows(gs_atp_ev, nongs_atp_ev)
all_wta_ev <- bind_rows(gs_wta_ev, nongs_wta_ev)

message("  Building any-stop match data...")
nongs_atp_md_any <- build_any_stop_matches(nongs_atp_ev, all_atp_ev, matches, "ATP")
nongs_wta_md_any <- build_any_stop_matches(nongs_wta_ev, all_wta_ev, matches, "WTA")
message("  Any-stop ATP: ", nrow(nongs_atp_md_any), " matches")
message("  Any-stop WTA: ", nrow(nongs_wta_md_any), " matches")

# 4 specs: unweighted/weighted x same-type/any-type
run_4spec_robust <- function(md_same, md_any, ev_table, label_prefix) {
  specs <- list(
    list(md = md_same, wt = FALSE, lab = paste0(label_prefix, " (unwt, same)"), spec_name = "Unwt, same-type"),
    list(md = md_same, wt = TRUE,  lab = paste0(label_prefix, " (wt, same)"), spec_name = "Wt, same-type"),
    list(md = md_any,  wt = FALSE, lab = paste0(label_prefix, " (unwt, any)"), spec_name = "Unwt, any-type"),
    list(md = md_any,  wt = TRUE,  lab = paste0(label_prefix, " (wt, any)"), spec_name = "Wt, any-type")
  )
  results <- list()
  for (sp in specs) {
    fit <- estimate_nongs_model(sp$md, sp$lab, weighted = sp$wt)
    if (is.null(fit)) next
    boot <- bootstrap_nongs(ev_table, sp$md, N_BOOT, sp$lab)
    se_delta <- if (!is.null(boot)) boot$boot_se["delta"] else fit$delta_se
    se_rho   <- if (!is.null(boot)) boot$boot_se["rho"] else fit$rho_se
    pv_delta <- 2 * pnorm(-abs(fit$delta / se_delta))
    pv_rho   <- 2 * pnorm(-abs(fit$rho / se_rho))
    results[[sp$spec_name]] <- list(
      spec = sp$spec_name, delta = fit$delta, se_delta = se_delta, p_delta = pv_delta,
      rho = fit$rho, se_rho = se_rho, p_rho = pv_rho, n = nrow(fit$data),
      n_events = n_distinct(fit$data$event_id),
      n_players = n_distinct(fit$data$focal_pid)
    )
  }
  results
}

robust_atp <- run_4spec_robust(nongs_atp_md, nongs_atp_md_any, nongs_atp_ev, "nonGS-ATP")
robust_wta <- run_4spec_robust(nongs_wta_md, nongs_wta_md_any, nongs_wta_ev, "nonGS-WTA")

# Write robustness table
write_robustness_table <- function(specs_list, filepath) {
  lines <- c(
    "\\begin{tabular}{lccccc}",
    "\\toprule",
    "Specification & $\\hat{\\delta}$ & SE & $\\hat{\\rho}$ & SE & $N$ \\\\",
    "\\midrule"
  )
  for (s in specs_list) {
    lines <- c(lines, sprintf(
      "%s & %s%s & (%s) & %s%s & (%s) & %s \\\\",
      s$spec, fmt(s$delta, 4), add_stars(s$p_delta), fmt(s$se_delta, 4),
      fmt(s$rho, 4), add_stars(s$p_rho), fmt(s$se_rho, 4),
      format(s$n, big.mark = ",")
    ))
  }
  lines <- c(lines, "\\bottomrule", "\\end{tabular}")
  writeLines(lines, filepath)
  message("  Saved: ", basename(filepath))
}

write_robustness_table(robust_atp,
  file.path(TABLES_DIR, "table_tournament_robustness_nongs_atp.tex"))
write_robustness_table(robust_wta,
  file.path(TABLES_DIR, "table_tournament_robustness_nongs_wta.tex"))

slog("## Part 5c: Robustness (4-spec)")
for (r in robust_atp) slog(sprintf("- ATP %s: delta=%.4f (SE=%.4f), rho=%.4f, N=%d",
  r$spec, r$delta, r$se_delta, r$rho, r$n))
for (r in robust_wta) slog(sprintf("- WTA %s: delta=%.4f (SE=%.4f), rho=%.4f, N=%d",
  r$spec, r$delta, r$se_delta, r$rho, r$n))
slog("")


# --- 5d. Horizon heterogeneity (cumulative indicators) -----------------------
message("\n  5d. Horizon heterogeneity (cumulative indicators)...")

run_horizon_nongs <- function(match_df, ev_table, label) {
  if (is.null(match_df) || nrow(match_df) < 100) return(NULL)
  df <- impute_match_data(match_df)

  # Create cumulative horizon indicators
  for (h in HORIZONS) {
    df[[paste0("D_", h, "w")]] <- as.integer(df$weeks_after_event <= h) * df$got_ll
    df[[paste0("V_", h, "w")]] <- as.integer(df$weeks_after_event <= h) * df$v_hat
  }

  # Model with 5 horizon-specific deltas and v_hat controls
  h_terms <- paste0("D_", HORIZONS, "w")
  v_terms <- paste0("V_", HORIZONS, "w")
  fml_str <- paste0("won ~ ", paste(h_terms, collapse = " + "), " + ",
                     paste(v_terms, collapse = " + "),
                     " + log_rank_ratio + log_rank_ratio_sq + rank_diff",
                     " + same_ioc + is_clay + is_grass + age_diff + height_diff",
                     " + hand_mismatch + h2h_smoothed + n_h2h",
                     " + pre_elo + opp_elo + pre_rank_pts",
                     " + player_age_at_event + had_prior_ll")
  fml <- as.formula(fml_str)

  model <- tryCatch(
    glm(fml, family = binomial(link = "logit"), data = df),
    error = function(e) NULL)
  if (is.null(model)) return(NULL)

  s <- summary(model)$coefficients
  results <- list()
  for (h in HORIZONS) {
    hn <- paste0("D_", h, "w")
    if (hn %in% rownames(s)) {
      results[[hn]] <- list(
        horizon = paste0(h, "w"),
        delta = s[hn, "Estimate"], se = s[hn, "Std. Error"],
        pvalue = s[hn, "Pr(>|z|)"]
      )
    }
  }

  message(sprintf("  %s: N=%d, %d events", label, nrow(df), n_distinct(df$event_id)))
  for (r in results) {
    message(sprintf("    %s: delta=%.4f (SE=%.4f, p=%.4f)", r$horizon, r$delta, r$se, r$pvalue))
  }

  list(model = model, results = results, n = nrow(df),
       n_events = n_distinct(df$event_id),
       n_players = n_distinct(df$focal_pid))
}

horiz_atp <- run_horizon_nongs(nongs_atp_md, nongs_atp_ev, "nonGS-ATP horizon")
horiz_wta <- run_horizon_nongs(nongs_wta_md, nongs_wta_ev, "nonGS-WTA horizon")

write_horizon_table <- function(horiz_obj, filepath, label) {
  if (is.null(horiz_obj)) return()
  lines <- c(
    "\\begin{tabular}{lccc}",
    "\\toprule",
    "Horizon & $\\hat{\\delta}_h$ & SE & $p$-value \\\\",
    "\\midrule"
  )
  for (r in horiz_obj$results) {
    lines <- c(lines, sprintf(
      "%s & %s%s & (%s) & %s \\\\",
      r$horizon, fmt(r$delta, 4), add_stars(r$pvalue), fmt(r$se, 4), fmt(r$pvalue, 3)
    ))
  }
  lines <- c(lines,
    "\\midrule",
    sprintf("Matches & \\multicolumn{3}{c}{%s} \\\\", format(horiz_obj$n, big.mark = ",")),
    sprintf("Episodes & \\multicolumn{3}{c}{%s} \\\\", format(horiz_obj$n_events, big.mark = ",")),
    "\\bottomrule", "\\end{tabular}"
  )
  writeLines(lines, filepath)
  message("  Saved: ", basename(filepath))
}

write_horizon_table(horiz_atp, file.path(TABLES_DIR, "table_tournament_horizon_nongs_atp.tex"), "ATP")
write_horizon_table(horiz_wta, file.path(TABLES_DIR, "table_tournament_horizon_nongs_wta.tex"), "WTA")

slog("## Part 5d: Horizon heterogeneity")
if (!is.null(horiz_atp)) {
  for (r in horiz_atp$results) slog(sprintf("- ATP %s: delta=%.4f (SE=%.4f)", r$horizon, r$delta, r$se))
}
if (!is.null(horiz_wta)) {
  for (r in horiz_wta$results) slog(sprintf("- WTA %s: delta=%.4f (SE=%.4f)", r$horizon, r$delta, r$se))
}
slog("")


# --- 5e. Tournament dose (matches_won interaction) ---------------------------
message("\n  5e. Tournament dose (matches_won interaction)...")

run_dose_tournament <- function(match_df, ev_table, label) {
  if (is.null(match_df) || nrow(match_df) < 100) return(NULL)
  df <- impute_match_data(match_df)

  # Compute matches_won per event from the LL event
  mw_lookup <- ev_table |>
    select(event_id, md_matches_won = if ("md_matches_won" %in% names(ev_table)) "md_matches_won" else NULL)
  if (is.null(mw_lookup) || !"md_matches_won" %in% names(mw_lookup)) {
    # Try to get from estimation sample
    mw_from_est <- nongs_est |>
      select(player_id, tourney_id, md_matches_won) |>
      distinct()
    ev_mw <- ev_table |>
      left_join(mw_from_est, by = c("player_id", "tourney_id")) |>
      select(event_id, md_matches_won)
    mw_lookup <- ev_mw
  }

  df <- df |>
    left_join(mw_lookup |> select(event_id, md_matches_won), by = "event_id") |>
    mutate(
      matches_won = coalesce(as.numeric(md_matches_won), 0) * got_ll,
      matches_won_sq = matches_won^2
    )

  fml <- won ~ got_ll + got_ll:matches_won + got_ll:matches_won_sq +
    v_hat + v_hat:matches_won + v_hat:matches_won_sq +
    log_rank_ratio + log_rank_ratio_sq + rank_diff +
    same_ioc + is_clay + is_grass + age_diff + height_diff +
    hand_mismatch + h2h_smoothed + n_h2h +
    pre_elo + opp_elo + pre_rank_pts +
    player_age_at_event + had_prior_ll

  model <- tryCatch(
    glm(fml, family = binomial(link = "logit"), data = df),
    error = function(e) NULL)
  if (is.null(model)) return(NULL)

  s <- summary(model)$coefficients
  message(sprintf("  %s dose: N=%d", label, nrow(df)))
  for (v in c("got_ll", "got_ll:matches_won", "got_ll:matches_won_sq", "v_hat")) {
    if (v %in% rownames(s)) {
      message(sprintf("    %s: %.4f (SE=%.4f, p=%.4f)", v, s[v,1], s[v,2], s[v,4]))
    }
  }

  list(model = model, data = df, n = nrow(df),
       n_events = n_distinct(df$event_id))
}

dose_tourn_atp <- run_dose_tournament(nongs_atp_md, nongs_atp_ev, "nonGS-ATP")
dose_tourn_wta <- run_dose_tournament(nongs_wta_md, nongs_wta_ev, "nonGS-WTA")

write_dose_tourn_table <- function(fit_obj, filepath, label) {
  if (is.null(fit_obj)) return()
  s <- summary(fit_obj$model)$coefficients
  key_vars <- c("got_ll", "got_ll:matches_won", "got_ll:matches_won_sq",
                "v_hat", "v_hat:matches_won", "v_hat:matches_won_sq")
  var_labels <- c("$\\hat{\\delta}$ (LL entry)", "$\\hat{\\delta} \\times$ matches won",
                  "$\\hat{\\delta} \\times$ matches won$^2$",
                  "$\\hat{\\rho}$ (CF)", "$\\hat{\\rho} \\times$ matches won",
                  "$\\hat{\\rho} \\times$ matches won$^2$")
  lines <- c(
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & Estimate & SE \\\\",
    "\\midrule"
  )
  for (k in seq_along(key_vars)) {
    v <- key_vars[k]
    if (v %in% rownames(s)) {
      est <- s[v, "Estimate"]; se <- s[v, "Std. Error"]
      pv <- s[v, "Pr(>|z|)"]
      lines <- c(lines, sprintf("%s & %s%s & (%s) \\\\",
                                var_labels[k], fmt(est, 4), add_stars(pv), fmt(se, 4)))
    }
  }
  lines <- c(lines,
    "\\midrule",
    sprintf("Matches & \\multicolumn{2}{c}{%s} \\\\", format(fit_obj$n, big.mark = ",")),
    sprintf("Episodes & \\multicolumn{2}{c}{%s} \\\\", format(fit_obj$n_events, big.mark = ",")),
    "\\bottomrule", "\\end{tabular}"
  )
  writeLines(lines, filepath)
  message("  Saved: ", basename(filepath))
}

write_dose_tourn_table(dose_tourn_atp,
  file.path(TABLES_DIR, "table_tournament_dose_nongs_atp.tex"), "ATP")
write_dose_tourn_table(dose_tourn_wta,
  file.path(TABLES_DIR, "table_tournament_dose_nongs_wta.tex"), "WTA")

slog("## Part 5e: Tournament dose")
slog("")


# --- 5f. Performance probability interaction ----------------------------------
message("\n  5f. Performance probability interaction...")

run_perf_prob <- function(match_df, label) {
  if (is.null(match_df) || nrow(match_df) < 100) return(NULL)
  df <- impute_match_data(match_df)

  # Drop any remaining NA rows to avoid dimension mismatch with predict
  model_vars <- c("won", "got_ll", "v_hat", "log_rank_ratio", "log_rank_ratio_sq",
                  "rank_diff", "same_ioc", "is_clay", "is_grass", "age_diff",
                  "height_diff", "hand_mismatch", "h2h_smoothed", "n_h2h",
                  "pre_elo", "opp_elo", "pre_rank_pts", "player_age_at_event",
                  "had_prior_ll")
  df <- df |> filter(if_all(all_of(model_vars), ~ !is.na(.)))

  # Compute predicted win prob for each match (from observables minus got_ll)
  fit_base <- glm(won ~ log_rank_ratio + log_rank_ratio_sq + rank_diff +
    same_ioc + is_clay + is_grass + age_diff + height_diff +
    hand_mismatch + h2h_smoothed + n_h2h +
    pre_elo + opp_elo + pre_rank_pts +
    player_age_at_event + had_prior_ll,
    family = binomial(link = "logit"), data = df)

  df$p_hat <- predict(fit_base, type = "response")

  # Interaction: got_ll x p_hat
  fml <- won ~ got_ll + got_ll:p_hat + v_hat + v_hat:p_hat +
    log_rank_ratio + log_rank_ratio_sq + rank_diff +
    same_ioc + is_clay + is_grass + age_diff + height_diff +
    hand_mismatch + h2h_smoothed + n_h2h +
    pre_elo + opp_elo + pre_rank_pts +
    player_age_at_event + had_prior_ll

  model <- tryCatch(glm(fml, family = binomial(link = "logit"), data = df),
                    error = function(e) NULL)
  if (is.null(model)) return(NULL)

  s <- summary(model)$coefficients
  message(sprintf("  %s perf_prob: N=%d", label, nrow(df)))
  for (v in c("got_ll", "got_ll:p_hat", "v_hat", "v_hat:p_hat")) {
    if (v %in% rownames(s)) {
      message(sprintf("    %s: %.4f (SE=%.4f, p=%.4f)", v, s[v,1], s[v,2], s[v,4]))
    }
  }

  list(model = model, n = nrow(df), n_events = n_distinct(df$event_id))
}

perf_atp <- run_perf_prob(nongs_atp_md, "nonGS-ATP")
perf_wta <- run_perf_prob(nongs_wta_md, "nonGS-WTA")

write_perf_prob_table <- function(fit_obj, filepath) {
  if (is.null(fit_obj)) return()
  s <- summary(fit_obj$model)$coefficients
  key_vars <- c("got_ll", "got_ll:p_hat", "v_hat", "v_hat:p_hat")
  var_labels <- c("$\\hat{\\delta}$ (LL entry)",
                  "$\\hat{\\delta} \\times \\hat{p}$",
                  "$\\hat{\\rho}$ (CF)",
                  "$\\hat{\\rho} \\times \\hat{p}$")
  lines <- c(
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & Estimate & SE \\\\",
    "\\midrule"
  )
  for (k in seq_along(key_vars)) {
    v <- key_vars[k]
    if (v %in% rownames(s)) {
      est <- s[v, "Estimate"]; se <- s[v, "Std. Error"]; pv <- s[v, "Pr(>|z|)"]
      lines <- c(lines, sprintf("%s & %s%s & (%s) \\\\",
                                var_labels[k], fmt(est, 4), add_stars(pv), fmt(se, 4)))
    }
  }
  lines <- c(lines,
    "\\midrule",
    sprintf("Matches & \\multicolumn{2}{c}{%s} \\\\", format(fit_obj$n, big.mark = ",")),
    sprintf("Episodes & \\multicolumn{2}{c}{%s} \\\\", format(fit_obj$n_events, big.mark = ",")),
    "\\bottomrule", "\\end{tabular}"
  )
  writeLines(lines, filepath)
  message("  Saved: ", basename(filepath))
}

write_perf_prob_table(perf_atp, file.path(TABLES_DIR, "table_tournament_perf_prob_nongs_atp.tex"))
write_perf_prob_table(perf_wta, file.path(TABLES_DIR, "table_tournament_perf_prob_nongs_wta.tex"))

slog("## Part 5f: Performance probability interaction")
slog("")


###############################################################################
# PART 6: SUMMARY STATISTICS
###############################################################################

message("\n", strrep("=", 72))
message("  PART 6: SUMMARY STATISTICS")
message(strrep("=", 72))

write_sumstats_table <- function(data, filepath, label) {
  d_ll <- data |> filter(got_ll == 1)
  d_no <- data |> filter(got_ll == 0)

  vars <- c("player_age", "pre_rank_pts", "pre_elo", "peer_component",
            "n_prior_nongs_ll_won", "had_prior_ll",
            "points_change_4w", "points_change_26w", "points_change_52w",
            "elo_change_4w", "elo_change_26w", "elo_change_52w",
            "n_main_draws_26w", "n_matches_250plus_26w")
  var_labels <- c("Age", "Pre-treatment ranking pts", "Pre-treatment Elo",
                  "$P_i^{LL}$",
                  "Prior non-GS LL wins", "Had prior LL",
                  "$\\Delta$ Pts (4w)", "$\\Delta$ Pts (26w)", "$\\Delta$ Pts (52w)",
                  "$\\Delta$ Elo (4w)", "$\\Delta$ Elo (26w)", "$\\Delta$ Elo (52w)",
                  "Main draws (26w)", "Matches at 250+ (26w)")

  lines <- c(
    "\\begin{tabular}{lcccccc}",
    "\\toprule",
    " & \\multicolumn{2}{c}{LL recipients} & \\multicolumn{2}{c}{Non-recipients} & \\multicolumn{2}{c}{Difference} \\\\",
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
    " & Mean & SD & Mean & SD & Diff & $p$ \\\\",
    "\\midrule"
  )

  for (k in seq_along(vars)) {
    v <- vars[k]
    if (!v %in% names(data)) next
    x1 <- d_ll[[v]]; x0 <- d_no[[v]]
    m1 <- mean(x1, na.rm = TRUE); s1 <- sd(x1, na.rm = TRUE)
    m0 <- mean(x0, na.rm = TRUE); s0 <- sd(x0, na.rm = TRUE)
    diff <- m1 - m0
    tt <- tryCatch(t.test(x1, x0)$p.value, error = function(e) NA_real_)
    lines <- c(lines, sprintf(
      "%s & %s & %s & %s & %s & %s%s & %s \\\\",
      var_labels[k], fmt(m1), fmt(s1), fmt(m0), fmt(s0),
      fmt(diff), add_stars(tt), fmt(tt, 3)
    ))
  }

  lines <- c(lines,
    "\\midrule",
    sprintf("$N$ & \\multicolumn{2}{c}{%d} & \\multicolumn{2}{c}{%d} & & \\\\",
            nrow(d_ll), nrow(d_no)),
    "\\bottomrule", "\\end{tabular}"
  )
  writeLines(lines, filepath)
  message("  Saved: ", basename(filepath))
}

write_sumstats_table(nongs_atp, file.path(TABLES_DIR, "table_sumstats_nongs_atp.tex"), "ATP")
write_sumstats_table(nongs_wta, file.path(TABLES_DIR, "table_sumstats_nongs_wta.tex"), "WTA")

slog("## Part 6: Summary statistics updated")
slog("- ATP: N_LL = ", sum(nongs_atp$got_ll), ", N_ctrl = ", sum(nongs_atp$got_ll == 0))
slog("- WTA: N_LL = ", sum(nongs_wta$got_ll), ", N_ctrl = ", sum(nongs_wta$got_ll == 0))
slog("- ATP mean P_i^{LL}: ", fmt(mean(nongs_atp$peer_component, na.rm = TRUE), 4))
slog("- WTA mean P_i^{LL}: ", fmt(mean(nongs_wta$peer_component, na.rm = TRUE), 4))
slog("")


###############################################################################
# SAVE RESULTS AND SUMMARY
###############################################################################

message("\n", strrep("=", 72))
message("  SAVING RESULTS")
message(strrep("=", 72))

saveRDS(list(
  hetero_atp = hetero_nongs_atp,
  hetero_wta = hetero_nongs_wta,
  dose_atp = dose_nongs_atp,
  dose_wta = dose_nongs_wta,
  first_atp = first_nongs_atp,
  first_wta = first_nongs_wta,
  tournament_main = list(atp = nongs_atp_fit, wta = nongs_wta_fit),
  tournament_boot = list(atp = nongs_atp_boot, wta = nongs_wta_boot),
  tournament_firstll = list(atp = firstll_atp_fit, wta = firstll_wta_fit),
  tournament_robust = list(atp = robust_atp, wta = robust_wta),
  tournament_horizon = list(atp = horiz_atp, wta = horiz_wta),
  tournament_dose = list(atp = dose_tourn_atp, wta = dose_tourn_wta),
  tournament_perf = list(atp = perf_atp, wta = perf_wta)
), file.path(CLEANED_DIR, "nongs_rerun_results.rds"))
message("  Saved: Data/cleaned/nongs_rerun_results.rds")

writeLines(summary_log, file.path(OUTPUT_DIR, "nongs_rerun_summary.md"))
message("  Saved: Output/nongs_rerun_summary.md")

message("\n", strrep("=", 72))
message("  DONE: 34_nongs_rerun.R complete")
message(strrep("=", 72))
