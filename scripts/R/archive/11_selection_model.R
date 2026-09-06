# ==============================================================================
# 11_selection_model.R
# Selection probability model for Lucky Loser entry using Bernoulli convolution
# Implements a control function / IV approach exploiting the fact that LL status
# depends on OTHER players' qualifying match outcomes (exclusion restriction)
#
# Tasks:
#   1. Expand sample to ALL final-round qualifiers (winners + losers)
#   2. Compute win probabilities P_t(i,j) for every final-round qualifying match
#   3. Compute selection probabilities via Bernoulli convolution
#   4. Control function / IV estimation of causal LL effect
#   5. Tables and figures
#   6. Mechanism tests with selection model
#   7. WTA selection model
#
# Inputs:  Data/raw/*.rds, Data/cleaned/estimation_sample_final.rds,
#          Data/cleaned/win_model_v2.rds, Data/cleaned/competitive_index_v2.rds,
#          Data/cleaned/main_fuzzy_rdd_results.rds
# Outputs: Data/cleaned/qualifying_match_probabilities.rds
#          Data/cleaned/selection_probabilities.rds
#          Data/cleaned/selection_model_results.rds
#          Data/cleaned/selection_model_mechanisms.rds
#          Data/cleaned/wta_selection_model_results.rds
#          Tables/table_first_stage_selection.tex
#          Tables/table_selection_model_results.tex
#          Figures/fig_selection_probability.pdf
#          Figures/fig_three_designs_comparison.pdf
#          Output/selection_model_summary.md
# Dependencies: dplyr, tidyr, readr, stringr, ggplot2, fixest, here
# Project: Lucky Losers and Career Trajectories
# ==============================================================================

set.seed(20260321)

# --- Packages -----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(fixest)
library(here)

# --- Paths --------------------------------------------------------------------
RAW_DIR     <- here("Data", "raw")
CLEANED_DIR <- here("Data", "cleaned")
TABLES_DIR  <- here("Tables")
FIGURES_DIR <- here("Figures")
OUTPUT_DIR  <- here("Output")
for (d in c(TABLES_DIR, FIGURES_DIR, OUTPUT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

CUTOFF <- 0.5

# --- Custom theme (matches project conventions) -------------------------------
theme_paper <- function(base_size = 14) {
  theme_minimal(base_size = base_size, base_family = "serif") %+replace%
    theme(
      plot.title       = element_blank(),
      plot.subtitle    = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.3, color = "grey85"),
      axis.line        = element_line(linewidth = 0.4, color = "grey30"),
      axis.ticks       = element_line(linewidth = 0.3, color = "grey30"),
      axis.ticks.length = unit(2, "pt"),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      legend.key.width = unit(18, "pt"),
      plot.margin      = margin(8, 12, 8, 8, unit = "pt"),
      strip.text       = element_text(face = "bold", size = base_size - 1)
    )
}

# ==============================================================================
# LOAD COMMON DATA
# ==============================================================================
message("=== Loading common data ===")

# Raw match data
atp_main <- read_rds(file.path(RAW_DIR, "atp_main_matches.rds"))
atp_qual <- read_rds(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
wta_main <- read_rds(file.path(RAW_DIR, "wta_main_matches.rds"))
wta_qual <- read_rds(file.path(RAW_DIR, "wta_qual_itf_matches.rds"))
atp_rankings_raw <- read_rds(file.path(RAW_DIR, "atp_rankings.rds"))
wta_rankings_raw <- read_rds(file.path(RAW_DIR, "wta_rankings.rds"))
atp_players <- read_rds(file.path(RAW_DIR, "atp_players.rds"))
wta_players <- read_rds(file.path(RAW_DIR, "wta_players.rds"))

# Cleaned data
est_final <- read_rds(file.path(CLEANED_DIR, "estimation_sample_final.rds"))

# Win probability model
win_model_v2 <- tryCatch(
  read_rds(file.path(CLEANED_DIR, "win_model_v2.rds")),
  error = function(e) {
    message("  WARNING: win_model_v2.rds not found; will build simple logit")
    NULL
  }
)

# Competitive index
comp_index_v2 <- tryCatch(
  read_rds(file.path(CLEANED_DIR, "competitive_index_v2.rds")),
  error = function(e) { message("  competitive_index_v2 not found"); NULL }
)

# Old RDD results for comparison
rdd_results <- tryCatch(
  readRDS(file.path(CLEANED_DIR, "main_fuzzy_rdd_results.rds")),
  error = function(e) { message("  main_fuzzy_rdd_results.rds not found"); NULL }
)

# Elo history
elo_history <- tryCatch(
  read_rds(file.path(CLEANED_DIR, "elo_history.rds")),
  error = function(e) { message("  elo_history not found"); NULL }
)

message("  ATP main matches: ", nrow(atp_main))
message("  ATP qual matches: ", nrow(atp_qual))
message("  Estimation sample final: ", nrow(est_final))

# Accumulator for summary
sel_summary <- list()

# ==============================================================================
# TASK 1: EXPAND ESTIMATION SAMPLE TO ALL FINAL-ROUND QUALIFIERS
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 1: EXPAND ESTIMATION SAMPLE")
message(strrep("=", 70))

# --- 1a. Identify all final-round qualifying matches (ATP) -------------------
message("\n--- 1a. ATP final-round qualifying matches ---")

atp_qual_tour <- atp_qual |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(
    year >= 2000,
    tourney_level %in% c("G", "M", "A"),
    str_detect(round, "^Q")
  )

# Determine final qualifying round per tournament
final_qual_round <- atp_qual_tour |>
  group_by(tourney_id) |>
  summarise(max_qual_round = max(round), .groups = "drop")

# Get final-round matches only
final_qual_matches <- atp_qual_tour |>
  inner_join(final_qual_round, by = "tourney_id") |>
  filter(round == max_qual_round)

message("  Final qualifying round matches: ", nrow(final_qual_matches))

# --- 1b. Build ALL qualifiers dataset (winners + losers) ----------------------
message("\n--- 1b. Building all-qualifiers dataset ---")

# WINNERS of final qualifying round (they qualified for main draw)
qual_winners <- final_qual_matches |>
  transmute(
    tourney_id, tourney_name, tourney_date, tourney_level, surface, draw_size,
    year = as.integer(str_sub(tourney_date, 1, 4)),
    player_id = winner_id,
    player_name = winner_name,
    player_rank = winner_rank,
    player_rank_points = winner_rank_points,
    player_age = winner_age,
    player_hand = winner_hand,
    player_ht = winner_ht,
    player_ioc = winner_ioc,
    player_seed = winner_seed,
    opponent_id = loser_id,
    opponent_name = loser_name,
    opponent_rank = loser_rank,
    opponent_age = loser_age,
    opponent_ioc = loser_ioc,
    won_qualifying_match = 1L,
    score = score,
    minutes = minutes
  )

# LOSERS of final qualifying round (potential LL candidates)
qual_losers <- final_qual_matches |>
  transmute(
    tourney_id, tourney_name, tourney_date, tourney_level, surface, draw_size,
    year = as.integer(str_sub(tourney_date, 1, 4)),
    player_id = loser_id,
    player_name = loser_name,
    player_rank = loser_rank,
    player_rank_points = loser_rank_points,
    player_age = loser_age,
    player_hand = loser_hand,
    player_ht = loser_ht,
    player_ioc = loser_ioc,
    player_seed = loser_seed,
    opponent_id = winner_id,
    opponent_name = winner_name,
    opponent_rank = winner_rank,
    opponent_age = winner_age,
    opponent_ioc = winner_ioc,
    won_qualifying_match = 0L,
    score = score,
    minutes = minutes
  )

all_qualifiers <- bind_rows(qual_winners, qual_losers)
message("  All final-round qualifiers: ", nrow(all_qualifiers))
message("  Winners (qualified): ", sum(all_qualifiers$won_qualifying_match))
message("  Losers (potential LL): ", sum(!all_qualifiers$won_qualifying_match))

# --- 1c. Rank ALL qualifiers by ATP ranking within each tournament ------------
message("\n--- 1c. Ranking all qualifiers ---")

all_qualifiers <- all_qualifiers |>
  group_by(tourney_id) |>
  mutate(
    n_qualifiers = n(),
    rank_among_all_qualifiers = rank(
      ifelse(is.na(player_rank), 9999, player_rank),
      ties.method = "min"
    )
  ) |>
  ungroup()

# --- 1d. Identify LL entries -------------------------------------------------
message("\n--- 1d. Identifying LL entries ---")

ll_winners <- atp_main |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(year >= 2000, winner_entry == "LL") |>
  transmute(tourney_id, ll_player_id = winner_id)

ll_losers <- atp_main |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(year >= 2000, loser_entry == "LL") |>
  transmute(tourney_id, ll_player_id = loser_id)

ll_entries <- bind_rows(ll_winners, ll_losers) |>
  distinct(tourney_id, ll_player_id)

# Flag LL recipients
all_qualifiers <- all_qualifiers |>
  mutate(
    got_ll = as.integer(paste0(tourney_id, "_", player_id) %in%
                          paste0(ll_entries$tourney_id, "_", ll_entries$ll_player_id))
  )

# LL slots per tournament (from losers only -- only losers can get LL)
ll_slots <- all_qualifiers |>
  filter(won_qualifying_match == 0L) |>
  group_by(tourney_id) |>
  summarise(n_ll_slots = sum(got_ll), .groups = "drop")

all_qualifiers <- all_qualifiers |>
  left_join(ll_slots, by = "tourney_id") |>
  mutate(n_ll_slots = replace_na(n_ll_slots, 0L))

message("  LL recipients: ", sum(all_qualifiers$got_ll))
message("  Tournaments with LL slots: ", sum(ll_slots$n_ll_slots > 0))

# --- 1e. Merge outcome variables from estimation_sample_final -----------------
message("\n--- 1e. Merging outcome variables ---")

# The estimation_sample_final has ranking/elo trajectories for LOSERS only
# We need to build them for WINNERS too
# First, merge what we have from est_final for losers
outcome_vars <- c("rank_t0", "rank_t4", "rank_t8", "rank_t12", "rank_t26", "rank_t52",
                   "points_t0", "points_t4", "points_t8", "points_t12", "points_t26", "points_t52",
                   "rank_change_4w", "rank_change_8w", "rank_change_12w",
                   "rank_change_26w", "rank_change_52w",
                   "elo_t0", "elo_t4", "elo_t8", "elo_t12", "elo_t26", "elo_t52",
                   "elo_change_4w", "elo_change_8w", "elo_change_12w",
                   "elo_change_26w", "elo_change_52w")

# Check which outcome vars exist in est_final
avail_outcomes <- outcome_vars[outcome_vars %in% names(est_final)]
message("  Available outcomes from est_final: ", length(avail_outcomes))

# Build outcomes for ALL qualifiers by merging from rankings and elo directly
message("  Building ranking trajectories for all qualifiers...")

# Parse dates
atp_rankings <- atp_rankings_raw |>
  mutate(rank_date = as.Date(as.character(ranking_date), format = "%Y%m%d")) |>
  filter(!is.na(rank_date))

rankings_slim <- atp_rankings |>
  select(player = player, rank_date, rank, points)

all_qualifiers <- all_qualifiers |>
  mutate(event_date = as.Date(as.character(tourney_date), format = "%Y%m%d"))

horizons <- c(0, 4, 8, 12, 26, 52)

for (h in horizons) {
  message("    Horizon: t+", h, " weeks")

  target_df <- all_qualifiers |>
    filter(!is.na(event_date)) |>
    transmute(tourney_id, player_id, target_date = event_date + h * 7)

  matched <- target_df |>
    inner_join(
      rankings_slim |> rename(player_id = player),
      by = "player_id",
      relationship = "many-to-many"
    ) |>
    filter(abs(as.numeric(rank_date - target_date)) <= 10) |>
    mutate(date_diff = abs(as.numeric(rank_date - target_date))) |>
    group_by(tourney_id, player_id) |>
    slice_min(date_diff, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(tourney_id, player_id,
           !!paste0("rank_t", h) := rank,
           !!paste0("points_t", h) := points)

  all_qualifiers <- all_qualifiers |>
    left_join(matched, by = c("tourney_id", "player_id"))
}

# Compute ranking changes
all_qualifiers <- all_qualifiers |>
  mutate(
    rank_change_4w  = rank_t4  - rank_t0,
    rank_change_8w  = rank_t8  - rank_t0,
    rank_change_12w = rank_t12 - rank_t0,
    rank_change_26w = rank_t26 - rank_t0,
    rank_change_52w = rank_t52 - rank_t0
  )

# Merge Elo trajectories if available
if (!is.null(elo_history)) {
  message("  Merging Elo trajectories...")
  for (h in horizons) {
    target <- all_qualifiers |>
      filter(!is.na(event_date)) |>
      transmute(tourney_id, player_id, target_date = event_date + h * 7)

    elo_matched <- target |>
      inner_join(elo_history, by = "player_id", relationship = "many-to-many") |>
      filter(abs(as.numeric(match_date - target_date)) <= 21) |>
      mutate(date_diff = abs(as.numeric(match_date - target_date))) |>
      group_by(tourney_id, player_id) |>
      slice_min(date_diff, n = 1, with_ties = FALSE) |>
      ungroup() |>
      select(tourney_id, player_id, !!paste0("elo_t", h) := elo)

    all_qualifiers <- all_qualifiers |>
      left_join(elo_matched, by = c("tourney_id", "player_id"))
  }

  all_qualifiers <- all_qualifiers |>
    mutate(
      elo_change_4w  = elo_t4  - elo_t0,
      elo_change_8w  = elo_t8  - elo_t0,
      elo_change_12w = elo_t12 - elo_t0,
      elo_change_26w = elo_t26 - elo_t0,
      elo_change_52w = elo_t52 - elo_t0
    )
}

message("  All qualifiers with outcomes: ", nrow(all_qualifiers))
message("  rank_t0 non-NA: ", sum(!is.na(all_qualifiers$rank_t0)))

sel_summary$n_all_qualifiers <- nrow(all_qualifiers)
sel_summary$n_winners <- sum(all_qualifiers$won_qualifying_match)
sel_summary$n_losers <- sum(!all_qualifiers$won_qualifying_match)

# ==============================================================================
# TASK 2: COMPUTE WIN PROBABILITIES P_t(i, j)
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 2: COMPUTE WIN PROBABILITIES")
message(strrep("=", 70))

# --- 2a. Build or use existing win probability model --------------------------
if (is.null(win_model_v2)) {
  message("\n--- 2a. Building simple logit win model ---")

  # Combine ATP match data for model estimation
  atp_all <- bind_rows(
    atp_main |> mutate(match_source = "main"),
    atp_qual |> mutate(match_source = "qual")
  ) |>
    mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
    filter(year >= 2000, !is.na(winner_id), !is.na(loser_id))

  # Stack from winner/loser perspective
  win_side <- atp_all |>
    filter(!is.na(winner_rank), !is.na(loser_rank)) |>
    transmute(
      player_id = winner_id, opponent_id = loser_id,
      player_rank = winner_rank, opponent_rank = loser_rank,
      player_age = winner_age, opponent_age = loser_age,
      player_ioc = winner_ioc, opponent_ioc = loser_ioc,
      surface, tourney_level, match_source,
      won = 1L,
      player_entry = winner_entry
    )

  loss_side <- atp_all |>
    filter(!is.na(winner_rank), !is.na(loser_rank)) |>
    transmute(
      player_id = loser_id, opponent_id = winner_id,
      player_rank = loser_rank, opponent_rank = winner_rank,
      player_age = loser_age, opponent_age = winner_age,
      player_ioc = loser_ioc, opponent_ioc = winner_ioc,
      surface, tourney_level, match_source,
      won = 0L,
      player_entry = loser_entry
    )

  model_panel <- bind_rows(win_side, loss_side) |>
    filter(is.na(player_entry) | player_entry != "LL") |>
    mutate(
      log_rank_ratio = log(pmax(opponent_rank, 1) / pmax(player_rank, 1)),
      rank_diff = opponent_rank - player_rank,
      same_ioc = as.integer(player_ioc == opponent_ioc),
      age_diff = player_age - opponent_age,
      surface_clay = as.integer(surface == "Clay"),
      surface_grass = as.integer(surface == "Grass")
    ) |>
    filter(!is.na(log_rank_ratio), !is.na(age_diff))

  win_model_v2 <- glm(
    won ~ log_rank_ratio + I(log_rank_ratio^2) + rank_diff +
      same_ioc + surface_clay + surface_grass + age_diff,
    data = model_panel, family = binomial(link = "logit")
  )

  message("  Built fallback logit model")
  message("  McFadden R2: ", round(1 - win_model_v2$deviance / win_model_v2$null.deviance, 4))
}

# --- 2b. Predict P_t(i, j) for every final-round qualifying match ------------
message("\n--- 2b. Predicting win probabilities for qualifying matches ---")

# Build prediction data from the final_qual_matches
# Each match has a winner and loser; we predict from each player's perspective
pred_data_matches <- final_qual_matches |>
  mutate(
    year = as.integer(str_sub(tourney_date, 1, 4)),
    # Winner perspective
    w_log_rank_ratio = log(pmax(loser_rank, 1) / pmax(winner_rank, 1)),
    w_rank_diff = loser_rank - winner_rank,
    w_age_diff = winner_age - loser_age,
    l_log_rank_ratio = log(pmax(winner_rank, 1) / pmax(loser_rank, 1)),
    l_rank_diff = winner_rank - loser_rank,
    l_age_diff = loser_age - winner_age,
    same_ioc = as.integer(winner_ioc == loser_ioc),
    surface_clay = as.integer(surface == "Clay"),
    surface_grass = as.integer(surface == "Grass")
  )

# Predict from winner's perspective (P_t(winner, loser))
pred_winner <- pred_data_matches |>
  transmute(
    tourney_id, match_id = row_number(),
    player_id = winner_id, opponent_id = loser_id,
    log_rank_ratio = w_log_rank_ratio,
    rank_diff = w_rank_diff,
    age_diff = w_age_diff,
    same_ioc, surface_clay, surface_grass
  )

# Predict from loser's perspective (P_t(loser, winner))
pred_loser <- pred_data_matches |>
  transmute(
    tourney_id, match_id = row_number(),
    player_id = loser_id, opponent_id = winner_id,
    log_rank_ratio = l_log_rank_ratio,
    rank_diff = l_rank_diff,
    age_diff = l_age_diff,
    same_ioc, surface_clay, surface_grass
  )

# Get model terms to check which predictors are needed
model_terms <- names(coef(win_model_v2))
message("  Model terms: ", paste(model_terms, collapse = ", "))

# For enhanced model, add rolling stats if model uses them
# Check if the model uses player_win_rate etc.
needs_extra <- any(str_detect(model_terms, "player_win_rate|opponent_win_rate|h2h|is_gs|ace_rate"))

if (needs_extra) {
  message("  Enhanced model detected -- adding extra predictors with defaults")
  add_defaults <- function(df) {
    # Add columns the enhanced model expects, with sensible defaults
    if (!"player_win_rate" %in% names(df)) df$player_win_rate <- 0.5
    if (!"opponent_win_rate" %in% names(df)) df$opponent_win_rate <- 0.5
    if (!"h2h_win_rate" %in% names(df)) df$h2h_win_rate <- 0.5
    if (!"has_h2h" %in% names(df)) df$has_h2h <- 0L
    if (!"is_gs" %in% names(df)) df$is_gs <- 0L
    if (!"is_masters" %in% names(df)) df$is_masters <- 0L
    if (!"is_qual" %in% names(df)) df$is_qual <- 1L
    # Serve/return stats -- use model training medians
    for (v in c("player_ace_rate_cum", "player_df_rate_cum",
                "player_1st_pct_cum", "player_1st_win_cum",
                "player_2nd_win_cum", "player_bp_save_cum",
                "player_ret_win_cum")) {
      if (v %in% model_terms && !v %in% names(df)) {
        df[[v]] <- NA_real_
      }
    }
    if (!"ht_diff" %in% names(df) && "ht_diff" %in% model_terms) df$ht_diff <- NA_real_
    if (!"hand_mismatch" %in% names(df) && "hand_mismatch" %in% model_terms) df$hand_mismatch <- 0L
    df
  }
  pred_winner <- add_defaults(pred_winner)
  pred_loser <- add_defaults(pred_loser)

  # Fill NA numeric columns with training data medians
  train_data <- win_model_v2$model
  for (v in names(pred_winner)) {
    if (is.numeric(pred_winner[[v]]) && any(is.na(pred_winner[[v]])) && v %in% names(train_data)) {
      med_val <- median(train_data[[v]], na.rm = TRUE)
      pred_winner[[v]] <- ifelse(is.na(pred_winner[[v]]), med_val, pred_winner[[v]])
    }
  }
  for (v in names(pred_loser)) {
    if (is.numeric(pred_loser[[v]]) && any(is.na(pred_loser[[v]])) && v %in% names(train_data)) {
      med_val <- median(train_data[[v]], na.rm = TRUE)
      pred_loser[[v]] <- ifelse(is.na(pred_loser[[v]]), med_val, pred_loser[[v]])
    }
  }
}

# Filter to obs with valid rank ratio (need ranks for both players)
pred_winner_valid <- pred_winner |> filter(!is.na(log_rank_ratio), !is.na(rank_diff))
pred_loser_valid  <- pred_loser  |> filter(!is.na(log_rank_ratio), !is.na(rank_diff))

# Fill remaining NAs in age_diff
pred_winner_valid$age_diff[is.na(pred_winner_valid$age_diff)] <- 0
pred_loser_valid$age_diff[is.na(pred_loser_valid$age_diff)] <- 0
pred_winner_valid$same_ioc[is.na(pred_winner_valid$same_ioc)] <- 0L
pred_loser_valid$same_ioc[is.na(pred_loser_valid$same_ioc)] <- 0L

# Predict
pred_winner_valid$p_win <- predict(win_model_v2, newdata = pred_winner_valid, type = "response")
pred_loser_valid$p_win  <- predict(win_model_v2, newdata = pred_loser_valid, type = "response")

message("  Winner predictions: ", nrow(pred_winner_valid), " (mean P = ", round(mean(pred_winner_valid$p_win), 4), ")")
message("  Loser predictions: ", nrow(pred_loser_valid), " (mean P = ", round(mean(pred_loser_valid$p_win), 4), ")")

# Build match-level probability dataset
qual_match_probs <- pred_winner_valid |>
  select(tourney_id, match_id, winner_id = player_id, p_win_winner = p_win) |>
  left_join(
    pred_loser_valid |>
      select(tourney_id, match_id, loser_id = player_id, p_win_loser = p_win),
    by = c("tourney_id", "match_id")
  )

message("  Qualifying match probabilities: ", nrow(qual_match_probs), " matches")

saveRDS(qual_match_probs, file.path(CLEANED_DIR, "qualifying_match_probabilities.rds"))
message("  Saved: qualifying_match_probabilities.rds")

# Merge probabilities back to all_qualifiers
# For each player-tournament, attach their win probability
winner_probs <- pred_winner_valid |>
  select(tourney_id, player_id, p_win_own_match = p_win) |>
  distinct(tourney_id, player_id, .keep_all = TRUE)

loser_probs <- pred_loser_valid |>
  select(tourney_id, player_id, p_win_own_match = p_win) |>
  distinct(tourney_id, player_id, .keep_all = TRUE)

all_probs <- bind_rows(winner_probs, loser_probs)

all_qualifiers <- all_qualifiers |>
  left_join(all_probs, by = c("tourney_id", "player_id"))

message("  Players with P_t(i,j): ", sum(!is.na(all_qualifiers$p_win_own_match)),
        " / ", nrow(all_qualifiers))

sel_summary$n_with_probs <- sum(!is.na(all_qualifiers$p_win_own_match))

# ==============================================================================
# TASK 3: COMPUTE SELECTION PROBABILITIES VIA BERNOULLI CONVOLUTION
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 3: COMPUTE SELECTION PROBABILITIES")
message(strrep("=", 70))

# --- 3a. Bernoulli convolution function ---------------------------------------
message("\n--- 3a. Bernoulli convolution function ---")

#' Compute Pr(sum of independent Bernoulli >= threshold)
#' @param probs vector of Bernoulli probabilities (p_1, ..., p_n)
#' @param threshold integer threshold
#' @return probability that sum >= threshold
bernoulli_conv_ge <- function(probs, threshold) {
  if (length(probs) == 0) {
    return(if (threshold <= 0) 1.0 else 0.0)
  }
  if (threshold <= 0) return(1.0)
  if (threshold > length(probs)) return(0.0)

  # Dynamic programming: prob[j+1] = Pr(sum == j) after processing k variables
  n <- length(probs)
  prob <- numeric(n + 1)
  prob[1] <- 1.0  # Pr(sum == 0) = 1 initially

  for (k in seq_along(probs)) {
    pk <- probs[k]
    new_prob <- numeric(n + 1)
    for (j in seq(min(k, n), 0, -1)) {
      # Pr(sum == j) = Pr(sum_prev == j) * (1 - p_k) + Pr(sum_prev == j-1) * p_k
      new_prob[j + 1] <- prob[j + 1] * (1 - pk)
      if (j >= 1) {
        new_prob[j + 1] <- new_prob[j + 1] + prob[j] * pk
      }
    }
    prob <- new_prob
  }

  # Pr(sum >= threshold) = sum of prob[threshold+1 : n+1]
  sum(prob[(threshold + 1):(n + 1)])
}

# Verify: single Bernoulli(0.5), threshold 1 should give 0.5
stopifnot(abs(bernoulli_conv_ge(0.5, 1) - 0.5) < 1e-10)
# Two Bernoulli(0.5), threshold 1 should give 0.75
stopifnot(abs(bernoulli_conv_ge(c(0.5, 0.5), 1) - 0.75) < 1e-10)
message("  Bernoulli convolution verified.")

# --- 3b. Compute selection probability for each loser-tournament obs ----------
message("\n--- 3b. Computing selection probabilities ---")

# For LOSERS only (only losers can get LL)
# Focus on tournaments with at least one LL slot for tractability
losers_for_sel <- all_qualifiers |>
  filter(won_qualifying_match == 0L, n_ll_slots > 0, !is.na(p_win_own_match))

message("  Losers with valid data for selection prob: ", nrow(losers_for_sel))

# For each loser i at tournament t:
# 1. Identify all qualifiers ranked ABOVE i (lower rank number)
# 2. For each such player k, their loss probability = 1 - P_t(k, j_k)
# 3. Player i gets LL if they lose AND rank_among_losers <= c_t
# 4. Pr(LL_i | i loses) = Pr(enough higher-ranked also lose)

# Build tournament-level data for the convolution
# We need the loss probabilities of all qualifiers, grouped by tournament
tourney_ids <- unique(losers_for_sel$tourney_id)
message("  Tournaments to process: ", length(tourney_ids))

# Pre-compute: for each tournament, get all qualifiers sorted by rank
# and their loss probabilities
sel_prob_results <- vector("list", nrow(losers_for_sel))
counter <- 0
report_interval <- max(1, length(tourney_ids) %/% 20)

for (tid in tourney_ids) {
  counter <- counter + 1
  if (counter %% report_interval == 0) {
    message("    Tournament ", counter, " / ", length(tourney_ids))
  }

  # All qualifiers at this tournament with valid win probs
  t_all <- all_qualifiers |>
    filter(tourney_id == tid, !is.na(p_win_own_match))

  # Sort by rank (ascending = best rank first)
  t_all <- t_all |>
    mutate(rank_sort = ifelse(is.na(player_rank), 9999, player_rank)) |>
    arrange(rank_sort)

  # Number of LL slots
  c_t <- t_all$n_ll_slots[1]
  if (is.na(c_t) || c_t == 0) next

  # Loss probability for each qualifier
  t_all$loss_prob <- 1 - t_all$p_win_own_match

  # Assign rank position among ALL qualifiers (1 = best)
  t_all$rank_pos <- seq_len(nrow(t_all))

  # For each LOSER, compute selection probability
  losers_t <- which(t_all$won_qualifying_match == 0L)

  for (idx in losers_t) {
    player_id_i <- t_all$player_id[idx]
    rank_pos_i <- t_all$rank_pos[idx]
    loss_prob_i <- t_all$loss_prob[idx]

    # Higher-ranked qualifiers (those with rank_pos < rank_pos_i)
    higher_ranked <- t_all |> filter(rank_pos < rank_pos_i)

    # Player i gets LL if:
    # (a) i loses their match (prob = loss_prob_i)
    # (b) Among the higher-ranked qualifiers, at least (rank_pos_i - c_t) also lose
    #     so that i's rank among losers <= c_t

    if (rank_pos_i <= c_t) {
      # Player i is among the top c_t ranked qualifiers overall
      # If i loses, they are automatically among the top c_t losers
      # (regardless of how many others lose)
      pr_ll_given_loss <- 1.0
    } else {
      # Need enough higher-ranked players to also lose
      # Among the (rank_pos_i - 1) higher-ranked qualifiers,
      # need at least (rank_pos_i - c_t) to also lose
      threshold_needed <- rank_pos_i - c_t

      if (nrow(higher_ranked) > 0) {
        higher_loss_probs <- higher_ranked$loss_prob
        pr_ll_given_loss <- bernoulli_conv_ge(higher_loss_probs, threshold_needed)
      } else {
        pr_ll_given_loss <- if (threshold_needed <= 0) 1.0 else 0.0
      }
    }

    # Full selection probability
    pr_ll <- loss_prob_i * pr_ll_given_loss

    # Find the row index in losers_for_sel
    row_match <- which(losers_for_sel$tourney_id == tid &
                         losers_for_sel$player_id == player_id_i)
    if (length(row_match) == 1) {
      sel_prob_results[[row_match]] <- tibble(
        tourney_id = tid,
        player_id = player_id_i,
        selection_prob = pr_ll,
        pr_loss = loss_prob_i,
        pr_ll_given_loss = pr_ll_given_loss,
        rank_pos = rank_pos_i,
        n_higher_ranked = nrow(higher_ranked),
        c_t = c_t
      )
    }
  }
}

sel_prob_df <- bind_rows(sel_prob_results)
message("  Selection probabilities computed: ", nrow(sel_prob_df), " observations")
message("  Mean selection prob: ", round(mean(sel_prob_df$selection_prob, na.rm = TRUE), 4))
message("  SD selection prob: ", round(sd(sel_prob_df$selection_prob, na.rm = TRUE), 4))
message("  Range: [", round(min(sel_prob_df$selection_prob, na.rm = TRUE), 4),
        ", ", round(max(sel_prob_df$selection_prob, na.rm = TRUE), 4), "]")

# Merge selection probabilities back
losers_for_sel <- losers_for_sel |>
  left_join(
    sel_prob_df |> select(tourney_id, player_id, selection_prob, pr_loss,
                           pr_ll_given_loss, rank_pos, n_higher_ranked, c_t),
    by = c("tourney_id", "player_id")
  )

saveRDS(sel_prob_df, file.path(CLEANED_DIR, "selection_probabilities.rds"))
message("  Saved: selection_probabilities.rds")

sel_summary$mean_sel_prob <- mean(sel_prob_df$selection_prob, na.rm = TRUE)
sel_summary$sd_sel_prob <- sd(sel_prob_df$selection_prob, na.rm = TRUE)
sel_summary$n_sel_prob <- nrow(sel_prob_df)

# ==============================================================================
# TASK 4: CONTROL FUNCTION / IV ESTIMATION
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 4: CONTROL FUNCTION / IV ESTIMATION")
message(strrep("=", 70))

# --- 4a. Prepare estimation data ----------------------------------------------
message("\n--- 4a. Preparing estimation data ---")

est_sel <- losers_for_sel |>
  filter(!is.na(selection_prob), !is.na(got_ll))

message("  Estimation sample: ", nrow(est_sel), " obs")
message("  Treated (got LL): ", sum(est_sel$got_ll))
message("  Control (no LL): ", sum(!est_sel$got_ll))

# Define outcome variables
rank_outcomes <- c("rank_change_4w", "rank_change_8w", "rank_change_12w",
                   "rank_change_26w", "rank_change_52w")
elo_outcomes <- if ("elo_change_4w" %in% names(est_sel)) {
  c("elo_change_4w", "elo_change_8w", "elo_change_12w",
    "elo_change_26w", "elo_change_52w")
} else {
  character(0)
}
all_outcomes <- c(rank_outcomes, elo_outcomes)

# --- 4b. First stage ---------------------------------------------------------
message("\n--- 4b. First stage: got_ll ~ selection_prob ---")

# Simple first stage
fs_simple <- feols(got_ll ~ selection_prob, data = est_sel, vcov = ~player_id)
message("  Simple first stage:")
message("    Coef on selection_prob: ", round(coef(fs_simple)["selection_prob"], 4))
fs_se <- sqrt(vcov(fs_simple)["selection_prob", "selection_prob"])
fs_tstat <- coef(fs_simple)["selection_prob"] / fs_se
message("    SE: ", round(fs_se, 4))
message("    t-stat: ", round(fs_tstat, 2), " (F = ", round(fs_tstat^2, 2), ")")

# First stage with covariates
fs_full <- tryCatch({
  feols(got_ll ~ selection_prob + player_rank + player_age +
          i(tourney_level) + i(surface) | year,
        data = est_sel, vcov = ~player_id)
}, error = function(e) {
  message("    Full first stage failed, trying simpler: ", e$message)
  feols(got_ll ~ selection_prob + player_rank + player_age,
        data = est_sel, vcov = ~player_id)
})

message("  Full first stage:")
message("    Coef on selection_prob: ", round(coef(fs_full)["selection_prob"], 4))

# Store first stage results
fs_results <- tibble(
  specification = c("Simple", "With covariates"),
  coef_sel_prob = c(coef(fs_simple)["selection_prob"],
                     coef(fs_full)["selection_prob"]),
  se_sel_prob = c(
    sqrt(vcov(fs_simple)["selection_prob", "selection_prob"]),
    sqrt(vcov(fs_full)["selection_prob", "selection_prob"])
  ),
  r_squared = c(tryCatch(fitstat(fs_simple, "r2")$r2, error = function(e) summary(fs_simple)$r.squared),
                 tryCatch(fitstat(fs_full, "r2")$r2, error = function(e) summary(fs_full)$r.squared)),
  n_obs = c(nobs(fs_simple), nobs(fs_full))
)

# Compute F-stat properly
fs_results$f_stat <- c(
  (fs_results$coef_sel_prob[1] / fs_results$se_sel_prob[1])^2,
  (fs_results$coef_sel_prob[2] / fs_results$se_sel_prob[2])^2
)

message("\n  First stage results:")
print(fs_results)

sel_summary$fs_coef_simple <- fs_results$coef_sel_prob[1]
sel_summary$fs_fstat_simple <- fs_results$f_stat[1]

# --- 4c. 2SLS estimation -----------------------------------------------------
message("\n--- 4c. 2SLS estimation ---")

tsls_results <- list()

for (outcome in all_outcomes) {
  y <- est_sel[[outcome]]
  ok <- !is.na(y)
  n_ok <- sum(ok)

  if (n_ok < 100) {
    message("  ", outcome, ": insufficient obs (", n_ok, ")")
    next
  }

  # 2SLS: outcome ~ covariates | got_ll ~ selection_prob
  tryCatch({
    iv_fit <- feols(
      as.formula(paste0(outcome, " ~ player_rank + player_age | year | got_ll ~ selection_prob")),
      data = est_sel[ok, ],
      vcov = ~player_id
    )

    iv_summ <- summary(iv_fit)
    iv_coef <- coef(iv_fit)["fit_got_ll"]
    iv_se <- sqrt(vcov(iv_fit)["fit_got_ll", "fit_got_ll"])
    iv_pv <- 2 * pnorm(-abs(iv_coef / iv_se))

    tsls_results[[outcome]] <- tibble(
      outcome = outcome,
      method = "2SLS",
      coef = iv_coef,
      se = iv_se,
      pv = iv_pv,
      ci_lower = iv_coef - 1.96 * iv_se,
      ci_upper = iv_coef + 1.96 * iv_se,
      n_obs = n_ok,
      n_clusters = length(unique(est_sel$player_id[ok]))
    )

    message("  ", outcome, " (2SLS): coef = ", round(iv_coef, 2),
            " (SE = ", round(iv_se, 2), "), p = ", round(iv_pv, 3),
            ", N = ", n_ok)
  }, error = function(e) {
    message("  ", outcome, " (2SLS): FAILED -- ", e$message)
  })
}

tsls_df <- bind_rows(tsls_results)

# --- 4d. Control function approach --------------------------------------------
message("\n--- 4d. Control function estimation ---")

# First stage residuals (generalized residual for probit/logit)
# For LPM first stage, generalized residual = Y - Xb_hat
est_sel$fs_fitted <- predict(fs_full, newdata = est_sel)
est_sel$fs_resid <- est_sel$got_ll - est_sel$fs_fitted

cf_results <- list()

for (outcome in all_outcomes) {
  y <- est_sel[[outcome]]
  ok <- !is.na(y)
  n_ok <- sum(ok)
  if (n_ok < 100) next

  tryCatch({
    cf_fit <- feols(
      as.formula(paste0(outcome, " ~ got_ll + fs_resid + player_rank + player_age | year")),
      data = est_sel[ok, ],
      vcov = ~player_id
    )

    cf_coef <- coef(cf_fit)["got_ll"]
    cf_se <- sqrt(vcov(cf_fit)["got_ll", "got_ll"])
    cf_pv <- 2 * pnorm(-abs(cf_coef / cf_se))

    # Hausman test: significance of fs_resid
    resid_coef <- coef(cf_fit)["fs_resid"]
    resid_se <- sqrt(vcov(cf_fit)["fs_resid", "fs_resid"])
    hausman_pv <- 2 * pnorm(-abs(resid_coef / resid_se))

    cf_results[[outcome]] <- tibble(
      outcome = outcome,
      method = "Control Function",
      coef = cf_coef,
      se = cf_se,
      pv = cf_pv,
      ci_lower = cf_coef - 1.96 * cf_se,
      ci_upper = cf_coef + 1.96 * cf_se,
      n_obs = n_ok,
      n_clusters = length(unique(est_sel$player_id[ok])),
      hausman_resid_coef = resid_coef,
      hausman_resid_pv = hausman_pv
    )

    message("  ", outcome, " (CF): coef = ", round(cf_coef, 2),
            " (SE = ", round(cf_se, 2), "), p = ", round(cf_pv, 3),
            ", Hausman p = ", round(hausman_pv, 3))
  }, error = function(e) {
    message("  ", outcome, " (CF): FAILED -- ", e$message)
  })
}

cf_df <- bind_rows(cf_results)

# --- 4e. BH correction -------------------------------------------------------
message("\n--- 4e. BH multiple testing correction ---")

# Combine results
all_iv_results <- bind_rows(tsls_df, cf_df)

# Apply BH within ranking change family for 2SLS
rank_tsls <- tsls_df |> filter(str_detect(outcome, "^rank_change"))
if (nrow(rank_tsls) > 0) {
  rank_tsls$pv_bh <- p.adjust(rank_tsls$pv, method = "BH")
  message("  Ranking change family (2SLS, BH-adjusted):")
  for (i in seq_len(nrow(rank_tsls))) {
    message("    ", rank_tsls$outcome[i], ": raw p = ", round(rank_tsls$pv[i], 3),
            ", BH p = ", round(rank_tsls$pv_bh[i], 3))
  }
  # Merge back
  tsls_df <- tsls_df |>
    left_join(rank_tsls |> select(outcome, pv_bh), by = "outcome")
}

# BH for Elo family
elo_tsls <- tsls_df |> filter(str_detect(outcome, "^elo_change"))
if (nrow(elo_tsls) > 0) {
  elo_tsls$pv_bh_elo <- p.adjust(elo_tsls$pv, method = "BH")
  tsls_df <- tsls_df |>
    left_join(elo_tsls |> select(outcome, pv_bh_elo), by = "outcome") |>
    mutate(pv_bh = coalesce(pv_bh, pv_bh_elo)) |>
    select(-pv_bh_elo)
}

# Save combined results
all_sel_results <- list(
  first_stage = fs_results,
  tsls = tsls_df,
  control_function = cf_df,
  all_combined = all_iv_results
)
saveRDS(all_sel_results, file.path(CLEANED_DIR, "selection_model_results.rds"))
message("  Saved: selection_model_results.rds")

sel_summary$n_tsls_outcomes <- nrow(tsls_df)
sel_summary$n_cf_outcomes <- nrow(cf_df)

# ==============================================================================
# TASK 5: TABLES AND FIGURES
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 5: TABLES AND FIGURES")
message(strrep("=", 70))

# --- 5a. First-stage table ----------------------------------------------------
message("\n--- 5a. First-stage table ---")

fs_tex <- c(
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & (1) & (2) \\\\",
  " & Simple & With Covariates \\\\",
  "\\midrule",
  paste0("Selection Probability & ",
         sprintf("%.3f", fs_results$coef_sel_prob[1]),
         ifelse(abs(fs_results$coef_sel_prob[1] / fs_results$se_sel_prob[1]) > 2.576, "$^{***}$",
                ifelse(abs(fs_results$coef_sel_prob[1] / fs_results$se_sel_prob[1]) > 1.96, "$^{**}$",
                       ifelse(abs(fs_results$coef_sel_prob[1] / fs_results$se_sel_prob[1]) > 1.645, "$^{*}$", ""))),
         " & ",
         sprintf("%.3f", fs_results$coef_sel_prob[2]),
         ifelse(abs(fs_results$coef_sel_prob[2] / fs_results$se_sel_prob[2]) > 2.576, "$^{***}$",
                ifelse(abs(fs_results$coef_sel_prob[2] / fs_results$se_sel_prob[2]) > 1.96, "$^{**}$",
                       ifelse(abs(fs_results$coef_sel_prob[2] / fs_results$se_sel_prob[2]) > 1.645, "$^{*}$", ""))),
         " \\\\"),
  paste0(" & (", sprintf("%.3f", fs_results$se_sel_prob[1]),
         ") & (", sprintf("%.3f", fs_results$se_sel_prob[2]), ") \\\\"),
  "\\midrule",
  paste0("Player controls & No & Yes \\\\"),
  paste0("Tournament controls & No & Yes \\\\"),
  paste0("Year FE & No & Yes \\\\"),
  "\\midrule",
  paste0("$F$-statistic & ", sprintf("%.1f", fs_results$f_stat[1]),
         " & ", sprintf("%.1f", fs_results$f_stat[2]), " \\\\"),
  paste0("$R^2$ & ", sprintf("%.3f", fs_results$r_squared[1]),
         " & ", sprintf("%.3f", fs_results$r_squared[2]), " \\\\"),
  paste0("$N$ & ", format(fs_results$n_obs[1], big.mark = ","),
         " & ", format(fs_results$n_obs[2], big.mark = ","), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(fs_tex, file.path(TABLES_DIR, "table_first_stage_selection.tex"))
message("  Saved: table_first_stage_selection.tex")

# --- 5b. Main results table --------------------------------------------------
message("\n--- 5b. Main results table ---")

if (nrow(tsls_df) > 0) {
  # Add stars
  add_stars <- function(pv) {
    ifelse(pv < 0.01, "$^{***}$",
           ifelse(pv < 0.05, "$^{**}$",
                  ifelse(pv < 0.1, "$^{*}$", "")))
  }

  results_tex <- c(
    "\\begin{tabular}{lccccc}",
    "\\toprule",
    "Outcome & Coef. & SE & $p$-value & BH $p$ & $N$ \\\\",
    "\\midrule",
    "\\multicolumn{6}{l}{\\textit{Panel A: 2SLS}} \\\\"
  )

  for (i in seq_len(nrow(tsls_df))) {
    r <- tsls_df[i, ]
    outcome_label <- r$outcome |>
      str_replace("rank_change_", "Rank $\\\\Delta$ ") |>
      str_replace("elo_change_", "Elo $\\\\Delta$ ") |>
      str_replace("w$", "w")
    bh_pv <- if ("pv_bh" %in% names(r) && !is.na(r$pv_bh)) sprintf("%.3f", r$pv_bh) else "--"
    results_tex <- c(results_tex,
      paste0(outcome_label, " & ",
             sprintf("%.2f", r$coef), add_stars(r$pv), " & ",
             sprintf("%.2f", r$se), " & ",
             sprintf("%.3f", r$pv), " & ",
             bh_pv, " & ",
             format(r$n_obs, big.mark = ","), " \\\\")
    )
  }

  if (nrow(cf_df) > 0) {
    results_tex <- c(results_tex,
      "\\midrule",
      "\\multicolumn{6}{l}{\\textit{Panel B: Control Function}} \\\\"
    )
    for (i in seq_len(nrow(cf_df))) {
      r <- cf_df[i, ]
      outcome_label <- r$outcome |>
        str_replace("rank_change_", "Rank $\\\\Delta$ ") |>
        str_replace("elo_change_", "Elo $\\\\Delta$ ") |>
        str_replace("w$", "w")
      results_tex <- c(results_tex,
        paste0(outcome_label, " & ",
               sprintf("%.2f", r$coef), add_stars(r$pv), " & ",
               sprintf("%.2f", r$se), " & ",
               sprintf("%.3f", r$pv), " & -- & ",
               format(r$n_obs, big.mark = ","), " \\\\")
      )
    }
  }

  results_tex <- c(results_tex,
    "\\bottomrule",
    "\\end{tabular}"
  )

  writeLines(results_tex, file.path(TABLES_DIR, "table_selection_model_results.tex"))
  message("  Saved: table_selection_model_results.tex")
}

# --- 5c. Figure: Selection probability vs actual LL status (binned scatter) ---
message("\n--- 5c. Selection probability scatter ---")

binned_data <- losers_for_sel |>
  filter(!is.na(selection_prob)) |>
  mutate(prob_bin = cut(selection_prob,
                         breaks = seq(0, 1, by = 0.05),
                         include.lowest = TRUE)) |>
  group_by(prob_bin) |>
  summarise(
    mean_prob = mean(selection_prob, na.rm = TRUE),
    mean_ll = mean(got_ll, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  filter(!is.na(prob_bin), n >= 5)

p_scatter <- ggplot(binned_data, aes(x = mean_prob, y = mean_ll)) +
  geom_point(aes(size = n), color = "#2c3e50", alpha = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
  geom_smooth(method = "lm", se = FALSE, color = "#e74c3c", linewidth = 0.8) +
  scale_x_continuous("Predicted Selection Probability", limits = c(0, 1)) +
  scale_y_continuous("Observed LL Rate", limits = c(0, 1)) +
  scale_size_continuous("Bin size", range = c(1, 6)) +
  theme_paper()

ggsave(file.path(FIGURES_DIR, "fig_selection_probability.pdf"),
       p_scatter, width = 7, height = 5)
message("  Saved: fig_selection_probability.pdf")

# --- 5d. Figure: Three-designs comparison forest plot -------------------------
message("\n--- 5d. Three-designs comparison forest plot ---")

# Collect estimates from three designs
forest_data <- list()

# 1. Selection model (2SLS)
if (nrow(tsls_df) > 0) {
  forest_data[["Selection Model"]] <- tsls_df |>
    filter(str_detect(outcome, "^rank_change")) |>
    transmute(outcome, design = "Selection Model", coef, se, ci_lower, ci_upper)
}

# 2. Old RDD results
if (!is.null(rdd_results)) {
  rdd_cov <- rdd_results |>
    filter(covariates == "Yes" | is.na(covariates)) |>
    filter(str_detect(outcome, "^rank_change"))
  if (nrow(rdd_cov) > 0) {
    forest_data[["Fuzzy RDD"]] <- rdd_cov |>
      transmute(outcome, design = "Fuzzy RDD", coef,
                se = se_robust,
                ci_lower = coef - 1.96 * se_robust,
                ci_upper = coef + 1.96 * se_robust)
  }
}

if (length(forest_data) > 0) {
  forest_df <- bind_rows(forest_data) |>
    mutate(
      horizon = str_extract(outcome, "\\d+") |> as.integer(),
      outcome_label = paste0(horizon, "w")
    )

  p_forest <- ggplot(forest_df, aes(x = coef, y = outcome_label, color = design, shape = design)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(position = position_dodge(width = 0.4), size = 3) +
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                   position = position_dodge(width = 0.4), height = 0.2) +
    scale_color_manual(values = c("Selection Model" = "#e74c3c",
                                   "Fuzzy RDD" = "#2c3e50",
                                   "GS Lottery" = "#27ae60")) +
    scale_shape_manual(values = c("Selection Model" = 16,
                                   "Fuzzy RDD" = 17,
                                   "GS Lottery" = 15)) +
    labs(x = "Treatment Effect (Ranking Change)", y = "Horizon") +
    theme_paper() +
    theme(legend.position = "bottom")

  ggsave(file.path(FIGURES_DIR, "fig_three_designs_comparison.pdf"),
         p_forest, width = 8, height = 5)
  message("  Saved: fig_three_designs_comparison.pdf")
} else {
  message("  WARNING: No data for forest plot")
}

# ==============================================================================
# TASK 6: MECHANISM TESTS WITH SELECTION MODEL
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 6: MECHANISM TESTS")
message(strrep("=", 70))

mechanism_results <- list()

# --- 6a. Competitiveness index change -----------------------------------------
message("\n--- 6a. Competitiveness index change ---")

if (!is.null(comp_index_v2)) {
  est_sel_comp <- est_sel |>
    mutate(player_id = as.character(player_id)) |>
    left_join(comp_index_v2 |> mutate(player_id = as.character(player_id)),
              by = c("player_id", "tourney_id"))

  comp_var <- intersect(names(est_sel_comp),
                         c("competitiveness_v2", "comp_v2_change_12w",
                           "comp_v2_change_26w", "comp_v2_change_52w"))

  for (cv in comp_var) {
    y <- est_sel_comp[[cv]]
    ok <- !is.na(y) & !is.na(est_sel_comp$selection_prob)
    if (sum(ok) < 100) next

    tryCatch({
      iv_comp <- feols(
        as.formula(paste0(cv, " ~ player_rank + player_age | year | got_ll ~ selection_prob")),
        data = est_sel_comp[ok, ],
        vcov = ~player_id
      )
      iv_c <- coef(iv_comp)["fit_got_ll"]
      iv_s <- sqrt(vcov(iv_comp)["fit_got_ll", "fit_got_ll"])
      mechanism_results[[cv]] <- tibble(
        mechanism = cv, coef = iv_c, se = iv_s,
        pv = 2 * pnorm(-abs(iv_c / iv_s)), n = sum(ok)
      )
      message("  ", cv, ": coef = ", round(iv_c, 4), ", p = ", round(2 * pnorm(-abs(iv_c / iv_s)), 3))
    }, error = function(e) message("  ", cv, ": FAILED -- ", e$message))
  }
} else {
  message("  Skipping competitiveness tests (no comp_index_v2)")
}

# --- 6b. Elo change ----------------------------------------------------------
message("\n--- 6b. Elo change ---")

for (ev in elo_outcomes) {
  if (!ev %in% names(est_sel)) next
  y <- est_sel[[ev]]
  ok <- !is.na(y)
  if (sum(ok) < 100) next

  tryCatch({
    iv_elo <- feols(
      as.formula(paste0(ev, " ~ player_rank + player_age | year | got_ll ~ selection_prob")),
      data = est_sel[ok, ],
      vcov = ~player_id
    )
    iv_c <- coef(iv_elo)["fit_got_ll"]
    iv_s <- sqrt(vcov(iv_elo)["fit_got_ll", "fit_got_ll"])
    mechanism_results[[ev]] <- tibble(
      mechanism = ev, coef = iv_c, se = iv_s,
      pv = 2 * pnorm(-abs(iv_c / iv_s)), n = sum(ok)
    )
    message("  ", ev, ": coef = ", round(iv_c, 2), ", p = ", round(2 * pnorm(-abs(iv_c / iv_s)), 3))
  }, error = function(e) message("  ", ev, ": FAILED -- ", e$message))
}

# --- 6c. Treatment dose (interaction with match wins in LL main draw) ---------
message("\n--- 6c. Treatment dose (interaction with LL match wins) ---")

# Merge LL match details from the estimation sample
ll_detail <- est_final |>
  filter(got_ll == 1) |>
  select(tourney_id, player_id, ll_matches_won) |>
  mutate(player_id = as.character(player_id))

est_sel_dose <- est_sel |>
  mutate(player_id = as.character(player_id)) |>
  left_join(ll_detail, by = c("tourney_id", "player_id")) |>
  mutate(
    ll_matches_won = replace_na(ll_matches_won, 0L),
    got_ll_won = got_ll * ll_matches_won
  )

# Check if we have enough variation
if (sum(est_sel_dose$got_ll_won > 0, na.rm = TRUE) >= 20) {
  for (outcome in c("rank_change_26w", "rank_change_52w")) {
    y <- est_sel_dose[[outcome]]
    ok <- !is.na(y)
    if (sum(ok) < 100) next

    tryCatch({
      dose_fit <- feols(
        as.formula(paste0(outcome, " ~ got_ll + got_ll_won + player_rank + player_age | year")),
        data = est_sel_dose[ok, ],
        vcov = ~player_id
      )
      mechanism_results[[paste0("dose_", outcome)]] <- tibble(
        mechanism = paste0("dose_", outcome),
        coef = coef(dose_fit)["got_ll"],
        se = sqrt(vcov(dose_fit)["got_ll", "got_ll"]),
        pv = 2 * pnorm(-abs(coef(dose_fit)["got_ll"] /
                               sqrt(vcov(dose_fit)["got_ll", "got_ll"]))),
        n = sum(ok),
        dose_coef = coef(dose_fit)["got_ll_won"],
        dose_se = sqrt(vcov(dose_fit)["got_ll_won", "got_ll_won"])
      )
      message("  ", outcome, ": got_ll = ", round(coef(dose_fit)["got_ll"], 2),
              ", dose = ", round(coef(dose_fit)["got_ll_won"], 2))
    }, error = function(e) message("  dose ", outcome, ": FAILED -- ", e$message))
  }
} else {
  message("  Insufficient variation in LL match wins for dose analysis")
}

mech_df <- bind_rows(mechanism_results)
saveRDS(mech_df, file.path(CLEANED_DIR, "selection_model_mechanisms.rds"))
message("  Saved: selection_model_mechanisms.rds")

sel_summary$n_mechanisms <- nrow(mech_df)

# ==============================================================================
# TASK 7: WTA SELECTION MODEL
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 7: WTA SELECTION MODEL")
message(strrep("=", 70))

# --- 7a. WTA final-round qualifying matches -----------------------------------
message("\n--- 7a. WTA final-round qualifying matches ---")

wta_qual_tour <- wta_qual |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(
    year >= 2000,
    tourney_level %in% c("G", "P", "PM", "I", "IS"),  # WTA levels
    str_detect(round, "^Q")
  )

# If very few WTA qualifying matches, try broader filter
if (nrow(wta_qual_tour) < 100) {
  message("  Few WTA qualifying matches with strict filter, broadening...")
  wta_qual_tour <- wta_qual |>
    mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
    filter(year >= 2000, str_detect(round, "^Q"))
}

message("  WTA qualifying matches: ", nrow(wta_qual_tour))

if (nrow(wta_qual_tour) > 0) {
  # Final qualifying round per tournament
  wta_final_qr <- wta_qual_tour |>
    group_by(tourney_id) |>
    summarise(max_qual_round = max(round), .groups = "drop")

  wta_final_qm <- wta_qual_tour |>
    inner_join(wta_final_qr, by = "tourney_id") |>
    filter(round == max_qual_round)

  message("  WTA final qual round matches: ", nrow(wta_final_qm))

  # --- 7b. Build WTA all-qualifiers dataset -----------------------------------
  wta_qual_winners <- wta_final_qm |>
    transmute(
      tourney_id, tourney_name, tourney_date, tourney_level, surface, draw_size,
      year = as.integer(str_sub(tourney_date, 1, 4)),
      player_id = winner_id, player_name = winner_name,
      player_rank = winner_rank, player_age = winner_age,
      player_ioc = winner_ioc,
      opponent_id = loser_id, opponent_rank = loser_rank,
      opponent_age = loser_age, opponent_ioc = loser_ioc,
      won_qualifying_match = 1L
    )

  wta_qual_losers <- wta_final_qm |>
    transmute(
      tourney_id, tourney_name, tourney_date, tourney_level, surface, draw_size,
      year = as.integer(str_sub(tourney_date, 1, 4)),
      player_id = loser_id, player_name = loser_name,
      player_rank = loser_rank, player_age = loser_age,
      player_ioc = loser_ioc,
      opponent_id = winner_id, opponent_rank = winner_rank,
      opponent_age = winner_age, opponent_ioc = winner_ioc,
      won_qualifying_match = 0L
    )

  wta_all_qual <- bind_rows(wta_qual_winners, wta_qual_losers)

  # Rank qualifiers
  wta_all_qual <- wta_all_qual |>
    group_by(tourney_id) |>
    mutate(
      n_qualifiers = n(),
      rank_among_all = rank(ifelse(is.na(player_rank), 9999, player_rank),
                             ties.method = "min")
    ) |>
    ungroup()

  # --- 7c. WTA LL entries ---
  wta_ll_w <- wta_main |>
    mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
    filter(year >= 2000, winner_entry == "LL") |>
    transmute(tourney_id, ll_player_id = winner_id)

  wta_ll_l <- wta_main |>
    mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
    filter(year >= 2000, loser_entry == "LL") |>
    transmute(tourney_id, ll_player_id = loser_id)

  wta_ll <- bind_rows(wta_ll_w, wta_ll_l) |>
    distinct(tourney_id, ll_player_id)

  wta_all_qual <- wta_all_qual |>
    mutate(got_ll = as.integer(
      paste0(tourney_id, "_", player_id) %in%
        paste0(wta_ll$tourney_id, "_", wta_ll$ll_player_id)
    ))

  wta_ll_slots <- wta_all_qual |>
    filter(won_qualifying_match == 0L) |>
    group_by(tourney_id) |>
    summarise(n_ll_slots = sum(got_ll), .groups = "drop")

  wta_all_qual <- wta_all_qual |>
    left_join(wta_ll_slots, by = "tourney_id") |>
    mutate(n_ll_slots = replace_na(n_ll_slots, 0L))

  message("  WTA LL recipients: ", sum(wta_all_qual$got_ll))

  # --- 7d. WTA win probabilities ---
  message("\n--- 7d. WTA win probabilities ---")

  # Build WTA logit model (or reuse ATP model as approximation)
  wta_all_matches <- bind_rows(
    wta_main |> mutate(match_source = "main"),
    wta_qual |> mutate(match_source = "qual")
  ) |>
    mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
    filter(year >= 2000, !is.na(winner_id), !is.na(loser_id))

  wta_model_data <- bind_rows(
    wta_all_matches |>
      filter(!is.na(winner_rank), !is.na(loser_rank)) |>
      transmute(
        won = 1L,
        log_rank_ratio = log(pmax(loser_rank, 1) / pmax(winner_rank, 1)),
        rank_diff = loser_rank - winner_rank,
        same_ioc = as.integer(winner_ioc == loser_ioc),
        age_diff = winner_age - loser_age,
        surface_clay = as.integer(surface == "Clay"),
        surface_grass = as.integer(surface == "Grass")
      ),
    wta_all_matches |>
      filter(!is.na(winner_rank), !is.na(loser_rank)) |>
      transmute(
        won = 0L,
        log_rank_ratio = log(pmax(winner_rank, 1) / pmax(loser_rank, 1)),
        rank_diff = winner_rank - loser_rank,
        same_ioc = as.integer(winner_ioc == loser_ioc),
        age_diff = loser_age - winner_age,
        surface_clay = as.integer(surface == "Clay"),
        surface_grass = as.integer(surface == "Grass")
      )
  ) |>
    filter(!is.na(log_rank_ratio), !is.na(age_diff))

  wta_win_model <- glm(
    won ~ log_rank_ratio + I(log_rank_ratio^2) + rank_diff +
      same_ioc + surface_clay + surface_grass + age_diff,
    data = wta_model_data, family = binomial(link = "logit")
  )

  message("  WTA logit McFadden R2: ",
          round(1 - wta_win_model$deviance / wta_win_model$null.deviance, 4))

  # Predict for WTA qualifying matches
  wta_pred <- wta_all_qual |>
    filter(!is.na(player_rank), !is.na(opponent_rank)) |>
    mutate(
      log_rank_ratio = log(pmax(opponent_rank, 1) / pmax(player_rank, 1)),
      rank_diff = opponent_rank - player_rank,
      same_ioc = as.integer(player_ioc == opponent_ioc),
      age_diff = player_age - opponent_age,
      surface_clay = as.integer(surface == "Clay"),
      surface_grass = as.integer(surface == "Grass")
    )

  wta_pred$age_diff[is.na(wta_pred$age_diff)] <- 0
  wta_pred$same_ioc[is.na(wta_pred$same_ioc)] <- 0L

  wta_pred$p_win_own_match <- predict(wta_win_model, newdata = wta_pred, type = "response")

  # Merge back
  wta_all_qual <- wta_all_qual |>
    left_join(
      wta_pred |> select(tourney_id, player_id, p_win_own_match) |>
        distinct(tourney_id, player_id, .keep_all = TRUE),
      by = c("tourney_id", "player_id")
    )

  # --- 7e. WTA selection probabilities ---
  message("\n--- 7e. WTA selection probabilities ---")

  wta_losers <- wta_all_qual |>
    filter(won_qualifying_match == 0L, n_ll_slots > 0, !is.na(p_win_own_match))

  if (nrow(wta_losers) > 0) {
    wta_tourney_ids <- unique(wta_losers$tourney_id)
    wta_sel_results <- vector("list", nrow(wta_losers))

    for (tid in wta_tourney_ids) {
      t_all <- wta_all_qual |>
        filter(tourney_id == tid, !is.na(p_win_own_match)) |>
        mutate(rank_sort = ifelse(is.na(player_rank), 9999, player_rank)) |>
        arrange(rank_sort)

      c_t <- t_all$n_ll_slots[1]
      if (is.na(c_t) || c_t == 0) next

      t_all$loss_prob <- 1 - t_all$p_win_own_match
      t_all$rank_pos <- seq_len(nrow(t_all))

      losers_idx <- which(t_all$won_qualifying_match == 0L)

      for (idx in losers_idx) {
        pid <- t_all$player_id[idx]
        rp <- t_all$rank_pos[idx]
        lp <- t_all$loss_prob[idx]

        higher <- t_all |> filter(rank_pos < rp)

        if (rp <= c_t) {
          pr_ll_given_loss <- 1.0
        } else {
          thresh <- rp - c_t
          if (nrow(higher) > 0) {
            pr_ll_given_loss <- bernoulli_conv_ge(higher$loss_prob, thresh)
          } else {
            pr_ll_given_loss <- if (thresh <= 0) 1.0 else 0.0
          }
        }

        row_match <- which(wta_losers$tourney_id == tid & wta_losers$player_id == pid)
        if (length(row_match) == 1) {
          wta_sel_results[[row_match]] <- tibble(
            tourney_id = tid, player_id = pid,
            selection_prob = lp * pr_ll_given_loss
          )
        }
      }
    }

    wta_sel_df <- bind_rows(wta_sel_results)
    message("  WTA selection probabilities: ", nrow(wta_sel_df))

    wta_losers <- wta_losers |>
      left_join(wta_sel_df, by = c("tourney_id", "player_id"))

    # --- 7f. WTA outcome trajectories ---
    message("\n--- 7f. WTA ranking trajectories ---")

    wta_rankings <- wta_rankings_raw |>
      mutate(rank_date = as.Date(as.character(ranking_date), format = "%Y%m%d")) |>
      filter(!is.na(rank_date))

    wta_rankings_slim <- wta_rankings |>
      select(player, rank_date, rank, points)

    wta_losers <- wta_losers |>
      mutate(event_date = as.Date(as.character(tourney_date), format = "%Y%m%d"))

    for (h in c(0, 4, 8, 12, 26, 52)) {
      target_df <- wta_losers |>
        filter(!is.na(event_date)) |>
        transmute(tourney_id, player_id, target_date = event_date + h * 7)

      matched <- target_df |>
        inner_join(
          wta_rankings_slim |> rename(player_id = player),
          by = "player_id", relationship = "many-to-many"
        ) |>
        filter(abs(as.numeric(rank_date - target_date)) <= 10) |>
        mutate(date_diff = abs(as.numeric(rank_date - target_date))) |>
        group_by(tourney_id, player_id) |>
        slice_min(date_diff, n = 1, with_ties = FALSE) |>
        ungroup() |>
        select(tourney_id, player_id,
               !!paste0("wta_rank_t", h) := rank)

      wta_losers <- wta_losers |>
        left_join(matched, by = c("tourney_id", "player_id"))
    }

    wta_losers <- wta_losers |>
      mutate(
        rank_change_12w = wta_rank_t12 - wta_rank_t0,
        rank_change_26w = wta_rank_t26 - wta_rank_t0,
        rank_change_52w = wta_rank_t52 - wta_rank_t0
      )

    # --- 7g. WTA 2SLS estimation ---
    message("\n--- 7g. WTA 2SLS estimation ---")

    wta_est <- wta_losers |>
      filter(!is.na(selection_prob), !is.na(got_ll))

    message("  WTA estimation sample: ", nrow(wta_est),
            " (LL: ", sum(wta_est$got_ll), ")")

    wta_iv_results <- list()

    for (outcome in c("rank_change_12w", "rank_change_26w", "rank_change_52w")) {
      y <- wta_est[[outcome]]
      ok <- !is.na(y)
      if (sum(ok) < 50) {
        message("  WTA ", outcome, ": insufficient obs (", sum(ok), ")")
        next
      }

      tryCatch({
        wta_iv <- feols(
          as.formula(paste0(outcome, " ~ player_rank + player_age | year | got_ll ~ selection_prob")),
          data = wta_est[ok, ],
          vcov = ~player_id
        )
        iv_c <- coef(wta_iv)["fit_got_ll"]
        iv_s <- sqrt(vcov(wta_iv)["fit_got_ll", "fit_got_ll"])
        wta_iv_results[[outcome]] <- tibble(
          outcome = outcome, coef = iv_c, se = iv_s,
          pv = 2 * pnorm(-abs(iv_c / iv_s)), n = sum(ok)
        )
        message("  WTA ", outcome, ": coef = ", round(iv_c, 2),
                ", p = ", round(2 * pnorm(-abs(iv_c / iv_s)), 3))
      }, error = function(e) message("  WTA ", outcome, ": FAILED -- ", e$message))
    }

    wta_iv_df <- bind_rows(wta_iv_results)
    saveRDS(wta_iv_df, file.path(CLEANED_DIR, "wta_selection_model_results.rds"))
    message("  Saved: wta_selection_model_results.rds")

    sel_summary$wta_n <- nrow(wta_est)
    sel_summary$wta_n_ll <- sum(wta_est$got_ll)
    sel_summary$wta_results <- nrow(wta_iv_df)
  } else {
    message("  No WTA losers with valid data for selection model")
    sel_summary$wta_n <- 0
  }
} else {
  message("  No WTA qualifying match data available")
  sel_summary$wta_n <- 0
}

# ==============================================================================
# SAVE COMPREHENSIVE SUMMARY
# ==============================================================================
message("\n", strrep("=", 70))
message("SAVING SUMMARY")
message(strrep("=", 70))

summary_lines <- c(
  "# Selection Model Summary",
  "",
  paste0("Generated: ", Sys.time()),
  "",
  "## Sample Construction",
  "",
  paste0("- All final-round qualifiers (winners + losers): ", sel_summary$n_all_qualifiers),
  paste0("- Winners (qualified for main draw): ", sel_summary$n_winners),
  paste0("- Losers (potential LL candidates): ", sel_summary$n_losers),
  paste0("- Players with valid win probabilities: ", sel_summary$n_with_probs),
  paste0("- Selection probabilities computed: ", sel_summary$n_sel_prob),
  "",
  "## Selection Probability Distribution",
  "",
  paste0("- Mean: ", round(sel_summary$mean_sel_prob, 4)),
  paste0("- SD: ", round(sel_summary$sd_sel_prob, 4)),
  "",
  "## First Stage",
  "",
  paste0("- Coefficient on selection probability (simple): ",
         round(sel_summary$fs_coef_simple, 4)),
  paste0("- F-statistic (simple): ", round(sel_summary$fs_fstat_simple, 1)),
  "",
  "## Main Results (2SLS)",
  ""
)

if (nrow(tsls_df) > 0) {
  for (i in seq_len(nrow(tsls_df))) {
    r <- tsls_df[i, ]
    summary_lines <- c(summary_lines,
      paste0("- ", r$outcome, ": coef = ", round(r$coef, 2),
             " (SE = ", round(r$se, 2), "), p = ", round(r$pv, 3),
             ", N = ", r$n_obs)
    )
  }
}

summary_lines <- c(summary_lines,
  "",
  "## Control Function Results",
  ""
)

if (nrow(cf_df) > 0) {
  for (i in seq_len(nrow(cf_df))) {
    r <- cf_df[i, ]
    summary_lines <- c(summary_lines,
      paste0("- ", r$outcome, ": coef = ", round(r$coef, 2),
             " (SE = ", round(r$se, 2), "), p = ", round(r$pv, 3),
             if (!is.na(r$hausman_resid_pv)) paste0(", Hausman p = ", round(r$hausman_resid_pv, 3)) else "")
    )
  }
}

summary_lines <- c(summary_lines,
  "",
  "## Mechanism Tests",
  paste0("- Mechanisms estimated: ", sel_summary$n_mechanisms),
  ""
)

if (nrow(mech_df) > 0) {
  for (i in seq_len(nrow(mech_df))) {
    r <- mech_df[i, ]
    summary_lines <- c(summary_lines,
      paste0("- ", r$mechanism, ": coef = ", round(r$coef, 4),
             " (SE = ", round(r$se, 4), "), p = ", round(r$pv, 3))
    )
  }
}

summary_lines <- c(summary_lines,
  "",
  "## WTA Results",
  paste0("- WTA estimation sample: ", sel_summary$wta_n),
  paste0("- WTA LL recipients: ", ifelse(is.null(sel_summary$wta_n_ll), 0, sel_summary$wta_n_ll)),
  ""
)

if (exists("wta_iv_df") && nrow(wta_iv_df) > 0) {
  for (i in seq_len(nrow(wta_iv_df))) {
    r <- wta_iv_df[i, ]
    summary_lines <- c(summary_lines,
      paste0("- WTA ", r$outcome, ": coef = ", round(r$coef, 2),
             " (SE = ", round(r$se, 2), "), p = ", round(r$pv, 3))
    )
  }
}

summary_lines <- c(summary_lines,
  "",
  "## Output Files",
  "",
  "### Data",
  "- `Data/cleaned/qualifying_match_probabilities.rds`",
  "- `Data/cleaned/selection_probabilities.rds`",
  "- `Data/cleaned/selection_model_results.rds`",
  "- `Data/cleaned/selection_model_mechanisms.rds`",
  "- `Data/cleaned/wta_selection_model_results.rds`",
  "",
  "### Tables",
  "- `Tables/table_first_stage_selection.tex`",
  "- `Tables/table_selection_model_results.tex`",
  "",
  "### Figures",
  "- `Figures/fig_selection_probability.pdf`",
  "- `Figures/fig_three_designs_comparison.pdf`"
)

writeLines(summary_lines, file.path(OUTPUT_DIR, "selection_model_summary.md"))
message("  Saved: Output/selection_model_summary.md")

message("\n", strrep("=", 70))
message("SELECTION MODEL COMPLETE")
message(strrep("=", 70))
message("  Total elapsed: script finished at ", Sys.time())
