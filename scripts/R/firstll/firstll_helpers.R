# ==============================================================================
# firstll_helpers.R
# Shared constants and helpers for the First-LL parallel analysis.
# Sourced by all F00-F10 scripts.
#
# Differences from main pipeline:
#   - ZPRE drops prior-LL variables (all zero by construction)
#   - Output paths point to *_FirstLL/ directories
#   - Inherits stack_horizons(), theme_paper(), etc. from utils.R
#
# Dependencies: here, ggplot2, dplyr (via utils.R)
# ==============================================================================

library(here)

source(here("scripts", "R", "utils.R"))

# --- Output directories -------------------------------------------------------
FIRSTLL_CLEANED <- here("Data", "cleaned", "firstll")
FIRSTLL_TABLES  <- here("Tables_FirstLL")
FIRSTLL_FIGURES <- here("Figures_FirstLL")
FIRSTLL_OUTPUT  <- here("Output_FirstLL")

for (d in c(FIRSTLL_CLEANED, FIRSTLL_TABLES, FIRSTLL_FIGURES, FIRSTLL_OUTPUT))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

# Shared input directories (read from main pipeline)
CLEANED_DIR <- here("Data", "cleaned")
RAW_DIR     <- here("Data", "raw")

# --- Horizons and outcomes ----------------------------------------------------
HORIZONS     <- c(4, 8, 12, 26, 52)
HORIZON_LABS <- paste0(HORIZONS, "w")

outcomes_base <- c("points_change", "elo_change", "n_main_draws",
                   "n_matches_250plus", "win_pct")
outcome_labels <- c(
  points_change     = "Ranking Pts $\\Delta$",
  elo_change        = "Elo $\\Delta$",
  n_main_draws      = "Main Draws",
  n_matches_250plus = "Matches 250+",
  win_pct           = "Win \\%"
)

# --- ZPRE specification (no prior-LL variables) -------------------------------
# Main pipeline ZPRE (from 42_final_fixes.R:118-125):
#   pre_rank_pts_s + pre_rank_pts_sq_s +
#   pre_elo_s + pre_elo_sq_s +
#   pre_surf_elo_s + pre_surf_elo_sq_s +
#   n_prior_gs_ll_won + n_prior_gs_ll_notwon +
#   n_prior_nongs_ll_won + n_prior_nongs_ll_notwon +
#   player_age
#
# First-LL version: drop the 4 prior-LL vars (all zero by construction)
ZPRE_FIRSTLL <- paste0(
  "pre_rank_pts_s + pre_rank_pts_sq_s + ",
  "pre_elo_s + pre_elo_sq_s + ",
  "pre_surf_elo_s + pre_surf_elo_sq_s + ",
  "player_age"
)

# Full ZPRE (for F09_marginal_impact which uses the unrestricted sample)
ZPRE_FULL <- paste0(
  "pre_rank_pts_s + pre_rank_pts_sq_s + ",
  "pre_elo_s + pre_elo_sq_s + ",
  "pre_surf_elo_s + pre_surf_elo_sq_s + ",
  "n_prior_gs_ll_won + n_prior_gs_ll_notwon + ",
  "n_prior_nongs_ll_won + n_prior_nongs_ll_notwon + ",
  "player_age"
)

# --- Ensure scaled variables --------------------------------------------------
# Copied from 42_final_fixes.R:87-112
ensure_scaled <- function(df) {
  df$pre_rank_pts_s <- df$pre_rank_pts / 1000
  df$pre_rank_pts_sq_s <- df$pre_rank_pts_s^2
  if (is.null(df$pre_elo_s) || all(is.na(df$pre_elo_s))) {
    if (median(df$pre_elo, na.rm = TRUE) > 100) {
      df$pre_elo_s <- df$pre_elo / 100
    } else {
      df$pre_elo_s <- df$pre_elo
    }
  }
  df$pre_elo_sq_s <- df$pre_elo_s^2
  if (!"pre_surf_elo_s" %in% names(df) || all(is.na(df$pre_surf_elo_s))) {
    if ("pre_surf_elo" %in% names(df)) {
      if (median(df$pre_surf_elo, na.rm = TRUE) > 100) {
        df$pre_surf_elo_s <- df$pre_surf_elo / 100
      } else {
        df$pre_surf_elo_s <- df$pre_surf_elo
      }
    } else {
      df$pre_surf_elo_s <- df$pre_elo_s
    }
  }
  df$pre_surf_elo_sq_s <- df$pre_surf_elo_s^2
  df
}

# --- Stack horizons (from 42_final_fixes.R:140-157) ---------------------------
# More flexible version that preserves all columns
stack_horizons_full <- function(data, outcomes = outcomes_base) {
  stacked <- list()
  for (h in HORIZONS) {
    hl <- paste0(h, "w")
    rd <- data
    rd$horizon <- hl
    rd$horizon_num <- h
    for (ob in outcomes) {
      cn <- paste0(ob, "_", h, "w")
      if (cn %in% names(data)) rd[[ob]] <- data[[cn]]
      else rd[[ob]] <- NA_real_
    }
    stacked[[hl]] <- rd
  }
  result <- do.call(rbind, stacked)
  result$horizon <- factor(result$horizon, levels = HORIZON_LABS)
  result
}

# --- Merge dose into skeleton -------------------------------------------------
# Copied from 42_final_fixes.R:161-168
merge_dose <- function(df, dose_data, event_type_val) {
  dd <- dose_data[dose_data$event_type == event_type_val,
                  c("player_id", "tourney_id", "dose", "matches_won")]
  df <- merge(df, dd, by = c("player_id", "tourney_id"), all.x = TRUE)
  df$dose[is.na(df$dose)] <- 0
  df$matches_won_dose <- ifelse(is.na(df$matches_won), 0L, df$matches_won)
  df
}

# --- Generalized residual for control function --------------------------------
# From 35d_win_model_final.R:412-416
compute_gen_residual <- function(D, P) {
  P <- pmax(pmin(P, 0.9999), 0.0001)
  pp <- qnorm(P); phi <- dnorm(pp)
  D * phi / P - (1 - D) * phi / (1 - P)
}

# --- Colour palette -----------------------------------------------------------
col_treat   <- "#E69F00"
col_control <- "#56B4E9"
