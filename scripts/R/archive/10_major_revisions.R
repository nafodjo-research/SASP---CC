# ==============================================================================
# 10_major_revisions.R
# Major revision tasks:
#   Task 1: Rebuild competitive index with full match statistics
#   Task 2: LL distribution tables (GS lottery + RDD)
#   Task 3: Comprehensive summary statistics table
#   Task 4: Full WTA lottery analysis (parallel to ATP)
#   Task 5: Complete the pooled ATP-WTA table
#   Task 6: Gender heterogeneity analysis
# Inputs:  Data/raw/*.rds, Data/cleaned/*.rds
# Outputs: Data/cleaned/competitive_index_v2.rds, win_model_v2.rds,
#          Data/cleaned/wta_lottery_full_results.rds
#          Tables/table_ll_distribution_gs.tex, table_ll_distribution_rdd.tex,
#          Tables/table_summary_stats.tex, Tables/table_pooled_complete.tex
#          Figures/fig_wta_lottery_event_study.pdf
#          Output/gender_heterogeneity.md, Output/major_revision_summary.md
# Dependencies: dplyr, tidyr, readr, stringr, ggplot2, fixest, rdrobust, here
# Project: Lucky Losers and Career Trajectories
# ==============================================================================

set.seed(20260321)

# --- Packages -----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(rdrobust)
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
est <- read_rds(file.path(CLEANED_DIR, "estimation_sample_final.rds"))
rdd <- est |> filter(n_ll_slots > 0)
message("  ATP RDD sample: ", nrow(rdd), " obs (LL: ", sum(rdd$got_ll), ")")

# Load raw match data
atp_main <- read_rds(file.path(RAW_DIR, "atp_main_matches.rds"))
atp_qual <- read_rds(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
wta_main <- read_rds(file.path(RAW_DIR, "wta_main_matches.rds"))
wta_qual <- read_rds(file.path(RAW_DIR, "wta_qual_itf_matches.rds"))
atp_rankings_raw <- read_rds(file.path(RAW_DIR, "atp_rankings.rds"))
wta_rankings_raw <- read_rds(file.path(RAW_DIR, "wta_rankings.rds"))
atp_players <- read_rds(file.path(RAW_DIR, "atp_players.rds"))
wta_players <- read_rds(file.path(RAW_DIR, "wta_players.rds"))

# Load pooled GS lottery sample from 09_revisions.R
pooled_gs <- tryCatch(
  read_rds(file.path(CLEANED_DIR, "pooled_gs_lottery_sample.rds")),
  error = function(e) {
    message("  WARNING: pooled_gs_lottery_sample.rds not found; will rebuild")
    NULL
  }
)

# Accumulator for summary
revision_summary <- list()

# ==============================================================================
# TASK 1: REBUILD COMPETITIVE INDEX WITH FULL MATCH STATISTICS
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 1: REBUILD COMPETITIVE INDEX WITH FULL MATCH STATISTICS")
message(strrep("=", 70))

# --- 1a. Combine all match data (ATP + WTA) ----------------------------------
message("\n--- 1a. Combining all match data ---")

prepare_matches <- function(main_df, qual_df, tour_label) {
  all <- bind_rows(
    main_df |> mutate(match_source = "main"),
    qual_df |> mutate(match_source = "qual")
  ) |>
    mutate(
      year = as.integer(str_sub(tourney_date, 1, 4)),
      tour = tour_label
    ) |>
    filter(year >= 2000, !is.na(winner_id), !is.na(loser_id))
  all
}

atp_all <- prepare_matches(atp_main, atp_qual, "ATP")
wta_all <- prepare_matches(wta_main, wta_qual, "WTA")
all_matches <- bind_rows(atp_all, wta_all)

message("  ATP matches: ", nrow(atp_all))
message("  WTA matches: ", nrow(wta_all))
message("  Combined: ", nrow(all_matches))

# --- 1b. Build player-level rolling statistics (52-week window) ---------------
message("\n--- 1b. Building player-level rolling statistics ---")

# First, stack matches from the perspective of each player
# Winner perspective
winner_stats <- all_matches |>
  filter(!is.na(w_svpt), w_svpt > 0) |>
  transmute(
    tour, match_source,
    match_date = as.Date(as.character(tourney_date), format = "%Y%m%d"),
    tourney_id, surface, tourney_level,
    player_id = winner_id,
    opponent_id = loser_id,
    player_rank = winner_rank,
    opponent_rank = loser_rank,
    player_age = winner_age,
    opponent_age = loser_age,
    player_ht = winner_ht,
    opponent_ht = loser_ht,
    player_hand = winner_hand,
    opponent_hand = loser_hand,
    player_ioc = winner_ioc,
    opponent_ioc = loser_ioc,
    won = 1L,
    # Serve stats (from winner's serve)
    aces = w_ace,
    dfs = w_df,
    svpt = w_svpt,
    first_in = w_1stIn,
    first_won = w_1stWon,
    second_won = w_2ndWon,
    bp_saved = w_bpSaved,
    bp_faced = w_bpFaced,
    # Return stats (opponent's serve stats = this player's return performance)
    opp_svpt = l_svpt,
    opp_first_in = l_1stIn,
    opp_first_won = l_1stWon,
    opp_second_won = l_2ndWon,
    match_minutes = minutes,
    player_entry = winner_entry
  )

# Loser perspective
loser_stats <- all_matches |>
  filter(!is.na(l_svpt), l_svpt > 0) |>
  transmute(
    tour, match_source,
    match_date = as.Date(as.character(tourney_date), format = "%Y%m%d"),
    tourney_id, surface, tourney_level,
    player_id = loser_id,
    opponent_id = winner_id,
    player_rank = loser_rank,
    opponent_rank = winner_rank,
    player_age = loser_age,
    opponent_age = winner_age,
    player_ht = loser_ht,
    opponent_ht = winner_ht,
    player_hand = loser_hand,
    opponent_hand = winner_hand,
    player_ioc = loser_ioc,
    opponent_ioc = winner_ioc,
    won = 0L,
    aces = l_ace,
    dfs = l_df,
    svpt = l_svpt,
    first_in = l_1stIn,
    first_won = l_1stWon,
    second_won = l_2ndWon,
    bp_saved = l_bpSaved,
    bp_faced = l_bpFaced,
    opp_svpt = w_svpt,
    opp_first_in = w_1stIn,
    opp_first_won = w_1stWon,
    opp_second_won = w_2ndWon,
    match_minutes = minutes,
    player_entry = loser_entry
  )

match_panel_full <- bind_rows(winner_stats, loser_stats) |>
  filter(!is.na(match_date)) |>
  arrange(player_id, match_date)

message("  Match panel with stats: ", nrow(match_panel_full), " player-match obs")

# Compute per-match derived rates
match_panel_full <- match_panel_full |>
  mutate(
    ace_rate = aces / pmax(svpt, 1),
    df_rate = dfs / pmax(svpt, 1),
    first_serve_pct = first_in / pmax(svpt, 1),
    first_serve_win_pct = first_won / pmax(first_in, 1),
    second_serve_win_pct = second_won / pmax(svpt - first_in, 1),
    bp_save_rate = bp_saved / pmax(bp_faced, 1),
    # Return stats: points won on opponent's serve
    return_pts_won = (opp_svpt - opp_first_won - opp_second_won),
    return_win_rate = return_pts_won / pmax(opp_svpt, 1),
    is_clay = as.integer(surface == "Clay"),
    is_grass = as.integer(surface == "Grass"),
    is_hard = as.integer(surface == "Hard")
  )

# --- 1b2. Compute rolling 52-week averages -----------------------------------
message("  Computing rolling 52-week player statistics...")

# For efficiency, compute rolling stats at a snapshot level rather than match-by-match
# Group by player and compute cumulative stats within a 52-week window
# We use a tournament-date level aggregation

# Player rolling stats: for each player-match, average over the prior 52 weeks
# This is computationally expensive, so we compute it at quarterly snapshots
# and then join to the nearest snapshot

# Create quarterly snapshots for each player
snapshot_dates <- seq(as.Date("2001-01-01"), as.Date("2024-12-31"), by = "91 days")

compute_player_stats_at_date <- function(player_matches, ref_date) {
  # Filter to matches in the 52 weeks BEFORE ref_date
  window <- player_matches |>
    filter(match_date >= (ref_date - 365) & match_date < ref_date)

  if (nrow(window) < 3) return(NULL)  # Need at least 3 matches

  tibble(
    n_matches_52w = nrow(window),
    win_rate_52w = mean(window$won),
    ace_rate_52w = mean(window$ace_rate, na.rm = TRUE),
    df_rate_52w = mean(window$df_rate, na.rm = TRUE),
    first_serve_pct_52w = mean(window$first_serve_pct, na.rm = TRUE),
    first_serve_win_52w = mean(window$first_serve_win_pct, na.rm = TRUE),
    second_serve_win_52w = mean(window$second_serve_win_pct, na.rm = TRUE),
    bp_save_rate_52w = mean(window$bp_save_rate, na.rm = TRUE),
    return_win_rate_52w = mean(window$return_win_rate, na.rm = TRUE),
    avg_minutes_52w = mean(window$match_minutes, na.rm = TRUE),
    # Surface-specific win rates
    win_rate_clay = mean(window$won[window$is_clay == 1], na.rm = TRUE),
    win_rate_grass = mean(window$won[window$is_grass == 1], na.rm = TRUE),
    win_rate_hard = mean(window$won[window$is_hard == 1], na.rm = TRUE),
    # Recent form (last 10 matches)
    recent_10_winrate = mean(tail(window$won, 10))
  )
}

# For speed, compute only for players in the RDD sample + main draw opponents
# Get unique player IDs we need stats for
rdd_player_ids <- unique(rdd$player_id)

# Also get all main draw players at RDD tournaments (for the reference pool)
main_draw_rdd_tourneys <- all_matches |>
  filter(tourney_id %in% unique(rdd$tourney_id)) |>
  select(winner_id, loser_id) |>
  unlist() |>
  unique()

needed_players <- unique(c(rdd_player_ids, main_draw_rdd_tourneys))
message("  Players needing rolling stats: ", length(needed_players))

# Instead of per-player per-snapshot (too slow), use a batch approach:
# For each match in the panel, compute rolling stats over the prior 52 weeks
# But only for needed players

match_panel_needed <- match_panel_full |>
  filter(player_id %in% needed_players)

message("  Matches for needed players: ", nrow(match_panel_needed))

# Group by player, then for each match compute rolling stats from prior matches
# We use a lag-based approach for efficiency
message("  Computing rolling stats (this may take a few minutes)...")

player_rolling <- match_panel_needed |>
  arrange(player_id, match_date) |>
  group_by(player_id) |>
  mutate(
    # Cumulative match number
    match_num_player = row_number(),
    # Rolling win rate over prior matches (up to 52 weeks)
    # We cannot efficiently do a date-window rolling mean in dplyr alone
    # So we use a simpler approach: rolling over last N matches as proxy
    # where N = typical matches in 52 weeks (approx 40-60 for top players)
    # Use last 40 matches as proxy for 52-week window
    roll_n = pmin(match_num_player - 1, 40)
  ) |>
  ungroup()

# For the actual model, we compute stats at the tournament level for each
# player in the RDD sample. This is more tractable.

# Compute player stats as of each tournament date in the RDD sample
message("  Computing player stats for RDD sample tournaments...")

rdd_events <- rdd |>
  select(player_id, tourney_id, tourney_date) |>
  mutate(event_date = as.Date(as.character(tourney_date), format = "%Y%m%d")) |>
  distinct()

player_stats_at_event <- list()
unique_events <- unique(rdd_events[, c("player_id", "event_date")])

# Batch computation: for each player, get all their matches and compute stats
# at each event date
# Ensure player_id is character for consistent joins
unique_events$player_id <- as.character(unique_events$player_id)
match_panel_needed$player_id <- as.character(match_panel_needed$player_id)

players_list <- split(unique_events, unique_events$player_id)

# Pre-split match panel by player for O(1) lookup instead of repeated filter
match_by_player <- split(
  match_panel_needed |> arrange(match_date),
  match_panel_needed$player_id
)

message("  Processing ", length(players_list), " players...")
counter <- 0
for (pid in names(players_list)) {
  counter <- counter + 1
  if (counter %% 500 == 0) message("    ", counter, " / ", length(players_list))

  p_matches <- match_by_player[[pid]]
  if (is.null(p_matches) || nrow(p_matches) < 3) next

  for (i in seq_len(nrow(players_list[[pid]]))) {
    ref_date <- players_list[[pid]]$event_date[i]
    stats <- compute_player_stats_at_date(p_matches, ref_date)
    if (!is.null(stats)) {
      stats$player_id <- pid
      stats$event_date <- ref_date
      player_stats_at_event[[length(player_stats_at_event) + 1]] <- stats
    }
  }
}

player_stats_df <- bind_rows(player_stats_at_event)
message("  Player stats computed: ", nrow(player_stats_df), " player-event obs")

# Merge with RDD sample
rdd_with_stats <- rdd |>
  mutate(
    player_id = as.character(player_id),
    event_date = as.Date(as.character(tourney_date), format = "%Y%m%d")
  ) |>
  left_join(
    player_stats_df |> mutate(player_id = as.character(player_id)),
    by = c("player_id", "event_date")
  )

message("  RDD sample with rolling stats: ",
        sum(!is.na(rdd_with_stats$win_rate_52w)), " / ", nrow(rdd_with_stats),
        " have win_rate_52w")

# --- 1c. Build enhanced win probability model --------------------------------
message("\n--- 1c. Building enhanced win probability model ---")

# Create match panel for the logit, with all available covariates
# Exclude LL main draw matches to avoid circularity
model_panel <- match_panel_full |>
  filter(is.na(player_entry) | player_entry != "LL") |>
  filter(!is.na(player_rank), !is.na(opponent_rank)) |>
  mutate(
    player_id = as.character(player_id),
    opponent_id = as.character(opponent_id),
    log_rank_ratio = log(pmax(opponent_rank, 1) / pmax(player_rank, 1)),
    rank_diff = opponent_rank - player_rank,
    same_ioc = as.integer(player_ioc == opponent_ioc),
    age_diff = player_age - opponent_age,
    surface_clay = as.integer(surface == "Clay"),
    surface_grass = as.integer(surface == "Grass")
  )

message("  Model panel (excluding LL): ", nrow(model_panel), " obs")

# Check if height is available
has_height <- sum(!is.na(model_panel$player_ht)) > nrow(model_panel) * 0.3
message("  Height available: ", has_height, " (", sum(!is.na(model_panel$player_ht)), " non-NA)")

# Check handedness
has_hand <- sum(!is.na(model_panel$player_hand)) > nrow(model_panel) * 0.3

# Build head-to-head record
message("  Computing head-to-head records...")
h2h_records <- match_panel_full |>
  mutate(player_id = as.character(player_id),
         opponent_id = as.character(opponent_id)) |>
  filter(!is.na(player_id), !is.na(opponent_id)) |>
  arrange(match_date) |>
  group_by(player_id, opponent_id) |>
  mutate(
    prior_wins_vs = cumsum(lag(won, default = 0)),
    prior_matches_vs = row_number() - 1
  ) |>
  ungroup() |>
  select(player_id, opponent_id, match_date, tourney_id,
         prior_wins_vs, prior_matches_vs)

# Add H2H to model panel (this is a self-join, so match on player-opponent-date)
model_panel <- model_panel |>
  left_join(
    h2h_records |> select(player_id, opponent_id, match_date, tourney_id,
                           prior_wins_vs, prior_matches_vs),
    by = c("player_id", "opponent_id", "match_date", "tourney_id")
  ) |>
  mutate(
    h2h_win_rate = ifelse(prior_matches_vs > 0, prior_wins_vs / prior_matches_vs, 0.5),
    has_h2h = as.integer(prior_matches_vs > 0)
  )

# Compute player rolling win rate (simple cumulative for speed)
message("  Computing rolling win rates for model panel...")
model_panel <- model_panel |>
  arrange(player_id, match_date) |>
  group_by(player_id) |>
  mutate(
    cum_wins = cumsum(lag(won, default = 0)),
    cum_matches = row_number() - 1,
    player_win_rate = ifelse(cum_matches > 0, cum_wins / cum_matches, 0.5)
  ) |>
  ungroup()

# Similarly for opponent
model_panel <- model_panel |>
  arrange(opponent_id, match_date) |>
  group_by(opponent_id) |>
  mutate(
    opp_cum_wins = cumsum(lag(1 - won, default = 0)),
    opp_cum_matches = row_number() - 1,
    opponent_win_rate = ifelse(opp_cum_matches > 0, opp_cum_wins / opp_cum_matches, 0.5)
  ) |>
  ungroup()

# Add serve/return stats as rolling averages for each player
# (These are already in the match row for the current match --
#  for a predictive model we should use PRIOR stats, not current match stats)
# Use the cumulative stats up to the prior match
model_panel <- model_panel |>
  arrange(player_id, match_date) |>
  group_by(player_id) |>
  mutate(
    player_ace_rate_cum = cummean(lag(ace_rate, default = NA_real_)),
    player_df_rate_cum = cummean(lag(df_rate, default = NA_real_)),
    player_1st_pct_cum = cummean(lag(first_serve_pct, default = NA_real_)),
    player_1st_win_cum = cummean(lag(first_serve_win_pct, default = NA_real_)),
    player_2nd_win_cum = cummean(lag(second_serve_win_pct, default = NA_real_)),
    player_bp_save_cum = cummean(lag(bp_save_rate, default = NA_real_)),
    player_ret_win_cum = cummean(lag(return_win_rate, default = NA_real_))
  ) |>
  ungroup()

# Build height and handedness differences
if (has_height) {
  # Need opponent height -- we need to look it up from player info
  # Get player heights from the players table
  # Build height lookup -- check which column name is used in the players table
  ht_col_atp <- if ("height" %in% names(atp_players)) "height" else if ("ht" %in% names(atp_players)) "ht" else NULL
  ht_col_wta <- if ("height" %in% names(wta_players)) "height" else if ("ht" %in% names(wta_players)) "ht" else NULL

  player_ht_lookup <- bind_rows(
    if (!is.null(ht_col_atp)) atp_players |> transmute(player_id = as.character(player_id), ht = .data[[ht_col_atp]]) else tibble(),
    if (!is.null(ht_col_wta)) wta_players |> transmute(player_id = as.character(player_id), ht = .data[[ht_col_wta]]) else tibble()
  ) |>
    filter(!is.na(ht)) |>
    distinct(player_id, .keep_all = TRUE)

  model_panel <- model_panel |>
    left_join(player_ht_lookup |> rename(player_ht_lookup = ht),
              by = "player_id") |>
    left_join(player_ht_lookup |> rename(opp_ht_lookup = ht, opponent_id = player_id),
              by = "opponent_id") |>
    mutate(
      ht_diff = coalesce(as.numeric(player_ht), as.numeric(player_ht_lookup)) -
                coalesce(as.numeric(opponent_ht), as.numeric(opp_ht_lookup), NA_real_)
    )
}

# Build handedness matchup
if (has_hand) {
  model_panel <- model_panel |>
    mutate(
      player_lefty = as.integer(player_hand == "L"),
      # Need opponent hand from players table
      hand_mismatch = NA_integer_  # placeholder, filled below
    )

  hand_col_atp <- if ("hand" %in% names(atp_players)) "hand" else NULL
  hand_col_wta <- if ("hand" %in% names(wta_players)) "hand" else NULL

  player_hand_lookup <- bind_rows(
    if (!is.null(hand_col_atp)) atp_players |> transmute(player_id = as.character(player_id), hand = .data[[hand_col_atp]]) else tibble(),
    if (!is.null(hand_col_wta)) wta_players |> transmute(player_id = as.character(player_id), hand = .data[[hand_col_wta]]) else tibble()
  ) |>
    filter(!is.na(hand)) |>
    distinct(player_id, .keep_all = TRUE)

  model_panel <- model_panel |>
    left_join(player_hand_lookup |> rename(opp_hand = hand, opponent_id = player_id),
              by = "opponent_id") |>
    mutate(
      hand_mismatch = as.integer(
        (player_hand == "L" & opp_hand == "R") |
        (player_hand == "R" & opp_hand == "L")
      )
    )
}

# Add tournament level indicators
model_panel <- model_panel |>
  mutate(
    is_gs = as.integer(tourney_level == "G"),
    is_masters = as.integer(tourney_level == "M"),
    is_qual = as.integer(match_source == "qual")
  )

# --- 1c2. Estimate enhanced logit model --------------------------------------
message("  Estimating enhanced logit model...")

# Build formula dynamically based on available variables
base_vars <- c("log_rank_ratio", "I(log_rank_ratio^2)", "rank_diff",
               "same_ioc", "surface_clay", "surface_grass", "age_diff",
               "player_win_rate", "opponent_win_rate",
               "h2h_win_rate", "has_h2h",
               "is_gs", "is_masters", "is_qual")

# Add serve/return stats if enough data
stat_vars <- c("player_ace_rate_cum", "player_df_rate_cum",
               "player_1st_pct_cum", "player_1st_win_cum",
               "player_2nd_win_cum", "player_bp_save_cum",
               "player_ret_win_cum")

# Check which stat vars have enough non-NA values
good_stat_vars <- character()
for (sv in stat_vars) {
  if (sv %in% names(model_panel) && sum(!is.na(model_panel[[sv]])) > nrow(model_panel) * 0.3) {
    good_stat_vars <- c(good_stat_vars, sv)
  }
}
message("  Serve/return stats available: ", paste(good_stat_vars, collapse = ", "))

extra_vars <- character()
if (has_height && "ht_diff" %in% names(model_panel) &&
    sum(!is.na(model_panel$ht_diff)) > nrow(model_panel) * 0.2) {
  extra_vars <- c(extra_vars, "ht_diff")
}
if (has_hand && "hand_mismatch" %in% names(model_panel) &&
    sum(!is.na(model_panel$hand_mismatch)) > nrow(model_panel) * 0.2) {
  extra_vars <- c(extra_vars, "hand_mismatch")
}

all_model_vars <- c(base_vars, good_stat_vars, extra_vars)
model_formula <- as.formula(paste("won ~", paste(all_model_vars, collapse = " + ")))

message("  Model formula: ", deparse(model_formula))

# Filter to complete cases for model variables
# (remove I() wrapper for checking)
check_vars <- gsub("I\\((.+)\\^2\\)", "\\1", all_model_vars)
check_vars <- unique(check_vars)
check_vars <- check_vars[check_vars %in% names(model_panel)]

model_data <- model_panel |>
  filter(if_all(all_of(check_vars), ~!is.na(.)))

message("  Model data rows (complete cases): ", nrow(model_data))

win_model_v2 <- glm(model_formula, data = model_data, family = binomial(link = "logit"))

mcfadden_r2_v2 <- 1 - win_model_v2$deviance / win_model_v2$null.deviance
message("  Enhanced logit McFadden R2: ", round(mcfadden_r2_v2, 4))

# Compare to old model
old_formula <- won ~ log_rank_ratio + I(log_rank_ratio^2) + rank_diff +
  same_ioc + surface_clay + surface_grass + age_diff
win_model_old <- glm(old_formula, data = model_data, family = binomial(link = "logit"))
mcfadden_r2_old <- 1 - win_model_old$deviance / win_model_old$null.deviance
message("  Old logit McFadden R2: ", round(mcfadden_r2_old, 4))
message("  Improvement: ", round(mcfadden_r2_v2 - mcfadden_r2_old, 4),
        " (", round((mcfadden_r2_v2 / mcfadden_r2_old - 1) * 100, 1), "% increase)")

revision_summary$comp_r2_old <- mcfadden_r2_old
revision_summary$comp_r2_new <- mcfadden_r2_v2

# --- 1c3. LL time interactions ------------------------------------------------
message("\n--- 1c3. Estimating LL time interaction model ---")

# Identify LL matches and compute weeks since LL entry
# First, find all LL entries from the main draw
ll_entry_dates <- bind_rows(
  atp_main |> filter(winner_entry == "LL") |>
    transmute(player_id = as.character(winner_id), tourney_id,
              ll_date = as.Date(as.character(tourney_date), format = "%Y%m%d")),
  atp_main |> filter(loser_entry == "LL") |>
    transmute(player_id = as.character(loser_id), tourney_id,
              ll_date = as.Date(as.character(tourney_date), format = "%Y%m%d")),
  wta_main |> filter(winner_entry == "LL") |>
    transmute(player_id = as.character(winner_id), tourney_id,
              ll_date = as.Date(as.character(tourney_date), format = "%Y%m%d")),
  wta_main |> filter(loser_entry == "LL") |>
    transmute(player_id = as.character(loser_id), tourney_id,
              ll_date = as.Date(as.character(tourney_date), format = "%Y%m%d"))
) |>
  distinct(player_id, tourney_id, .keep_all = TRUE)

# For each match in model_data, find the most recent prior LL entry
model_data_ll <- model_data |>
  left_join(
    ll_entry_dates |> select(player_id, ll_date) |>
      group_by(player_id) |>
      arrange(ll_date) |>
      # Keep all LL dates for range join
      mutate(ll_seq = row_number()) |>
      ungroup(),
    by = "player_id",
    relationship = "many-to-many"
  ) |>
  filter(is.na(ll_date) | ll_date <= match_date) |>
  group_by(player_id, match_date, tourney_id, opponent_id) |>
  slice_max(ll_date, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    weeks_since_ll = as.numeric(difftime(match_date, ll_date, units = "weeks")),
    had_ll = as.integer(!is.na(ll_date)),
    ll_0_4w = as.integer(had_ll == 1 & weeks_since_ll >= 0 & weeks_since_ll < 4),
    ll_4_12w = as.integer(had_ll == 1 & weeks_since_ll >= 4 & weeks_since_ll < 12),
    ll_12_26w = as.integer(had_ll == 1 & weeks_since_ll >= 12 & weeks_since_ll < 26),
    ll_26_52w = as.integer(had_ll == 1 & weeks_since_ll >= 26 & weeks_since_ll < 52),
    ll_52plus = as.integer(had_ll == 1 & weeks_since_ll >= 52)
  )

# Estimate model with LL time dummies
ll_time_formula <- update(model_formula, . ~ . + ll_0_4w + ll_4_12w + ll_12_26w +
                            ll_26_52w + ll_52plus)

# Filter to complete cases
ll_time_vars <- c("ll_0_4w", "ll_4_12w", "ll_12_26w", "ll_26_52w", "ll_52plus")
model_data_ll_complete <- model_data_ll |>
  mutate(across(all_of(ll_time_vars), ~replace_na(., 0L)))

win_model_ll <- tryCatch({
  glm(ll_time_formula, data = model_data_ll_complete, family = binomial(link = "logit"))
}, error = function(e) {
  message("  LL time model failed: ", e$message)
  NULL
})

if (!is.null(win_model_ll)) {
  message("  LL time interaction coefficients:")
  ll_coefs <- coef(win_model_ll)[ll_time_vars]
  ll_ses <- sqrt(diag(vcov(win_model_ll)))[ll_time_vars]
  for (i in seq_along(ll_time_vars)) {
    message("    ", ll_time_vars[i], ": ",
            round(ll_coefs[i], 4), " (SE = ", round(ll_ses[i], 4), ")")
  }
  revision_summary$ll_time_coefs <- ll_coefs
}

# --- 1d. Compute new competitiveness index for RDD sample ---------------------
message("\n--- 1d. Computing new competitiveness index ---")

# Get main draw players at each RDD tournament (reference pool)
main_draw_w <- atp_main |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(year >= 2000, !is.na(winner_rank)) |>
  transmute(tourney_id, opponent_id = as.character(winner_id),
            opponent_rank = winner_rank,
            opponent_age = winner_age, opponent_ioc = winner_ioc)

main_draw_l <- atp_main |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(year >= 2000, !is.na(loser_rank)) |>
  transmute(tourney_id, opponent_id = as.character(loser_id),
            opponent_rank = loser_rank,
            opponent_age = loser_age, opponent_ioc = loser_ioc)

main_draw_players <- bind_rows(main_draw_w, main_draw_l) |>
  distinct(tourney_id, opponent_id, .keep_all = TRUE)

# For the NEW model, we need all the covariates. Some (like serve stats)
# are player-level rolling stats, so we need to merge them in.
# For the competitiveness index, we need to build prediction data for each
# player-opponent pair at each tournament.

# Simplify: use the enhanced model but only with variables we can construct
# for the player-opponent pairs in the prediction sample
# The key enhancement is: player_win_rate, opponent_win_rate, h2h

# Build prediction data for competitiveness index
message("  Building prediction dataset for C_it...")
comp_pred <- rdd_with_stats |>
  mutate(player_id = as.character(player_id)) |>
  select(player_id, tourney_id, player_rank, player_age, player_ioc,
         surface, got_ll, R_tilde,
         win_rate_52w, ace_rate_52w, df_rate_52w,
         first_serve_pct_52w, first_serve_win_52w,
         second_serve_win_52w, bp_save_rate_52w, return_win_rate_52w) |>
  inner_join(main_draw_players, by = "tourney_id", relationship = "many-to-many") |>
  filter(opponent_id != player_id) |>
  mutate(
    log_rank_ratio = log(pmax(opponent_rank, 1) / pmax(player_rank, 1)),
    rank_diff = opponent_rank - player_rank,
    same_ioc = as.integer(player_ioc == opponent_ioc),
    surface_clay = as.integer(surface == "Clay"),
    surface_grass = as.integer(surface == "Grass"),
    age_diff = player_age - opponent_age,
    # Use rolling stats as model inputs
    player_win_rate = coalesce(win_rate_52w, 0.5),
    opponent_win_rate = 0.5,  # We don't have opponent rolling stats in this context
    h2h_win_rate = 0.5,  # No H2H available at prediction time
    has_h2h = 0L,
    is_gs = as.integer(FALSE),  # These are qualifying events
    is_masters = as.integer(FALSE),
    is_qual = 0L
  )

# Add serve stats to prediction
if (length(good_stat_vars) > 0) {
  stat_mapping <- c(
    "player_ace_rate_cum" = "ace_rate_52w",
    "player_df_rate_cum" = "df_rate_52w",
    "player_1st_pct_cum" = "first_serve_pct_52w",
    "player_1st_win_cum" = "first_serve_win_52w",
    "player_2nd_win_cum" = "second_serve_win_52w",
    "player_bp_save_cum" = "bp_save_rate_52w",
    "player_ret_win_cum" = "return_win_rate_52w"
  )
  for (mv in good_stat_vars) {
    src <- stat_mapping[mv]
    if (!is.na(src) && src %in% names(comp_pred)) {
      comp_pred[[mv]] <- comp_pred[[src]]
    } else {
      comp_pred[[mv]] <- NA_real_
    }
  }
}

# Add height and handedness dummies if in model
if ("ht_diff" %in% all_model_vars) {
  comp_pred$ht_diff <- NA_real_  # Not available for prediction; will be imputed
}
if ("hand_mismatch" %in% all_model_vars) {
  comp_pred$hand_mismatch <- 0L  # Default
}

# Fill NAs with column medians for prediction
pred_vars <- check_vars[check_vars %in% names(comp_pred)]
for (pv in pred_vars) {
  if (any(is.na(comp_pred[[pv]]))) {
    med_val <- median(model_data[[pv]], na.rm = TRUE)
    comp_pred[[pv]] <- coalesce(comp_pred[[pv]], med_val)
  }
}

# Predict
message("  Predicting win probabilities...")
comp_pred$p_win_v2 <- predict(win_model_v2, newdata = comp_pred, type = "response")

# Also predict with old model for comparison
comp_pred$p_win_old <- predict(win_model_old, newdata = comp_pred, type = "response")

# Aggregate to C_it
C_it_v2 <- comp_pred |>
  group_by(player_id, tourney_id) |>
  summarise(
    competitiveness_v2 = mean(p_win_v2, na.rm = TRUE),
    competitiveness_old = mean(p_win_old, na.rm = TRUE),
    n_opponents = n(),
    .groups = "drop"
  )

message("  New C_it computed for ", nrow(C_it_v2), " player-tournament obs")
message("  New C_it mean: ", round(mean(C_it_v2$competitiveness_v2, na.rm = TRUE), 4),
        ", SD: ", round(sd(C_it_v2$competitiveness_v2, na.rm = TRUE), 4))
message("  Old C_it mean: ", round(mean(C_it_v2$competitiveness_old, na.rm = TRUE), 4))

# --- 1e. Compute C_it at future horizons and re-run RDD ----------------------
message("\n--- 1e. Computing future C_it and RDD mechanism test ---")

# Merge C_it_v2 with RDD data
rdd_comp_v2 <- rdd |>
  mutate(player_id = as.character(player_id)) |>
  left_join(C_it_v2, by = c("player_id", "tourney_id"))

# For future horizons, recompute using future rankings
compute_future_comp_v2 <- function(rank_var, horizon_label) {
  future_data <- rdd |>
    mutate(player_id = as.character(player_id)) |>
    select(player_id, tourney_id, player_age, player_ioc, surface) |>
    mutate(player_rank_future = rdd[[rank_var]]) |>
    filter(!is.na(player_rank_future)) |>
    inner_join(main_draw_players, by = "tourney_id", relationship = "many-to-many") |>
    filter(opponent_id != player_id) |>
    mutate(
      log_rank_ratio = log(pmax(opponent_rank, 1) / pmax(player_rank_future, 1)),
      rank_diff = opponent_rank - player_rank_future,
      same_ioc = as.integer(player_ioc == opponent_ioc),
      surface_clay = as.integer(surface == "Clay"),
      surface_grass = as.integer(surface == "Grass"),
      age_diff = player_age - opponent_age
    ) |>
    filter(!is.na(log_rank_ratio))

  # For the old model prediction (which uses fewer variables)
  future_data$p_win <- predict(win_model_old, newdata = future_data, type = "response")

  future_data |>
    group_by(player_id, tourney_id) |>
    summarise(
      !!paste0("comp_v2_", horizon_label) := mean(p_win, na.rm = TRUE),
      .groups = "drop"
    )
}

C_12w_v2 <- compute_future_comp_v2("rank_t12", "t12")
C_26w_v2 <- compute_future_comp_v2("rank_t26", "t26")
C_52w_v2 <- compute_future_comp_v2("rank_t52", "t52")

rdd_comp_v2 <- rdd_comp_v2 |>
  left_join(C_12w_v2, by = c("player_id", "tourney_id")) |>
  left_join(C_26w_v2, by = c("player_id", "tourney_id")) |>
  left_join(C_52w_v2, by = c("player_id", "tourney_id")) |>
  mutate(
    comp_v2_change_12w = comp_v2_t12 - competitiveness_v2,
    comp_v2_change_26w = comp_v2_t26 - competitiveness_v2,
    comp_v2_change_52w = comp_v2_t52 - competitiveness_v2
  )

# Run RDD on new competitiveness changes
message("\n--- RDD on New Competitiveness Index ---")
comp_v2_outcomes <- c("comp_v2_change_12w", "comp_v2_change_26w", "comp_v2_change_52w")
comp_v2_results <- list()

covs_comp <- as.matrix(rdd_comp_v2[, c("player_rank", "player_age")])
covs_comp[is.na(covs_comp)] <- apply(covs_comp, 2, median, na.rm = TRUE)[
  col(covs_comp)[is.na(covs_comp)]]

# Add elo_t0 if available
if ("elo_t0" %in% names(rdd_comp_v2)) {
  elo_col <- replace(rdd_comp_v2$elo_t0, is.na(rdd_comp_v2$elo_t0),
                     median(rdd_comp_v2$elo_t0, na.rm = TRUE))
  covs_comp <- cbind(covs_comp, elo_t0 = elo_col)
}

for (outcome in comp_v2_outcomes) {
  y <- rdd_comp_v2[[outcome]]
  ok <- !is.na(y)
  if (sum(ok) < 200) {
    message("  ", outcome, ": insufficient obs (", sum(ok), ")")
    next
  }

  tryCatch({
    rd <- rdrobust(y = y[ok], x = rdd_comp_v2$R_tilde[ok], c = CUTOFF,
                   fuzzy = rdd_comp_v2$got_ll[ok],
                   covs = covs_comp[ok, ],
                   cluster = rdd_comp_v2$player_id[ok],
                   kernel = "triangular", bwselect = "mserd")
    comp_v2_results[[outcome]] <- tibble(
      outcome = outcome,
      coef = rd$coef["Conventional", 1],
      se = rd$se["Robust", 1],
      pval = rd$pv["Robust", 1],
      ci_lo = rd$ci["Robust", 1],
      ci_hi = rd$ci["Robust", 2],
      bw = rd$bws[1, 1],
      eff_n = rd$N_h[1] + rd$N_h[2]
    )
    message("  ", outcome, ": LATE = ", round(rd$coef["Conventional", 1], 4),
            " (p = ", round(rd$pv["Robust", 1], 3), ")")
  }, error = function(e) {
    message("  ", outcome, " FAILED: ", e$message)
  })
}

comp_v2_results_df <- bind_rows(comp_v2_results)

# --- 1f. Try AMEN package (network model) ------------------------------------
message("\n--- 1f. Attempting AMEN network model ---")

amen_success <- FALSE
tryCatch({
  if (!requireNamespace("amen", quietly = TRUE)) {
    message("  Installing amen package...")
    install.packages("amen", repos = "https://cloud.r-project.org", quiet = TRUE)
  }
  library(amen)
  message("  amen package loaded successfully")

  # The AMEN model requires a sociomatrix (adjacency matrix of outcomes)
  # This is computationally very expensive for a large player set
  # We would need to restrict to a subset of players

  # For feasibility, restrict to top ~200 players in a recent year
  amen_year <- 2022
  amen_matches <- model_data |>
    filter(year == amen_year) |>
    filter(player_rank <= 200, opponent_rank <= 200)

  message("  AMEN sample (", amen_year, ", top 200): ", nrow(amen_matches), " matches")

  if (nrow(amen_matches) > 500) {
    # Build binary sociomatrix
    players_amen <- sort(unique(c(amen_matches$player_id, amen_matches$opponent_id)))
    n_amen <- length(players_amen)
    message("  Players in AMEN network: ", n_amen)

    if (n_amen <= 300) {
      # Create outcome matrix Y (i beat j = 1, j beat i = 0, no match = NA)
      Y_amen <- matrix(NA, nrow = n_amen, ncol = n_amen,
                       dimnames = list(players_amen, players_amen))

      for (k in seq_len(nrow(amen_matches))) {
        pid <- amen_matches$player_id[k]
        oid <- amen_matches$opponent_id[k]
        if (pid %in% players_amen && oid %in% players_amen) {
          i_idx <- match(pid, players_amen)
          j_idx <- match(oid, players_amen)
          if (amen_matches$won[k] == 1) {
            Y_amen[i_idx, j_idx] <- 1
          } else {
            Y_amen[i_idx, j_idx] <- 0
          }
        }
      }

      message("  Fitting AMEN probit model (this may take several minutes)...")

      # Build dyadic covariate array: rank_ratio as a simple Xd
      Xd_rank <- matrix(0, nrow = n_amen, ncol = n_amen)
      for (k in seq_len(nrow(amen_matches))) {
        pid <- amen_matches$player_id[k]
        oid <- amen_matches$opponent_id[k]
        if (pid %in% players_amen && oid %in% players_amen) {
          i_idx <- match(pid, players_amen)
          j_idx <- match(oid, players_amen)
          Xd_rank[i_idx, j_idx] <- amen_matches$log_rank_ratio[k]
        }
      }

      Xd_array <- array(Xd_rank, dim = c(n_amen, n_amen, 1),
                         dimnames = list(players_amen, players_amen, "log_rank_ratio"))

      amen_fit <- tryCatch({
        ame(Y = Y_amen, Xd = Xd_array, family = "bin",
            R = 2, burn = 500, nscan = 1000, odens = 1, print = FALSE)
      }, error = function(e) {
        message("  AMEN fit failed: ", e$message)
        NULL
      })

      if (!is.null(amen_fit)) {
        amen_success <- TRUE
        message("  AMEN model fitted successfully")
        message("  AMEN sender effects (player ability) computed for ", n_amen, " players")
        saveRDS(amen_fit, file.path(CLEANED_DIR, "amen_network_model.rds"))
      }
    } else {
      message("  Too many players for AMEN (", n_amen, "); skipping")
    }
  } else {
    message("  Insufficient matches for AMEN; skipping")
  }
}, error = function(e) {
  message("  AMEN failed: ", e$message)
  message("  Continuing with enhanced logit (which is the primary model)")
})

revision_summary$amen_success <- amen_success

# --- 1g. Save Task 1 outputs -------------------------------------------------
message("\n--- 1g. Saving Task 1 outputs ---")
saveRDS(C_it_v2, file.path(CLEANED_DIR, "competitive_index_v2.rds"))
saveRDS(win_model_v2, file.path(CLEANED_DIR, "win_model_v2.rds"))
saveRDS(comp_v2_results_df, file.path(CLEANED_DIR, "mechanism_competitiveness_v2.rds"))
saveRDS(rdd_comp_v2, file.path(CLEANED_DIR, "rdd_with_competitiveness_v2.rds"))
if (!is.null(win_model_ll)) {
  saveRDS(win_model_ll, file.path(CLEANED_DIR, "win_model_ll_time.rds"))
}
message("  Task 1 complete")


# ==============================================================================
# TASK 2: LL DISTRIBUTION TABLES
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 2: LL DISTRIBUTION TABLES")
message(strrep("=", 70))

# --- 2a. Grand Slam lottery distribution table --------------------------------
message("\n--- 2a. Grand Slam LL distribution ---")

# Need to rebuild the GS lottery sample with slam names
# ATP GS qualifying losers
atp_gs_losers <- est |>
  filter(tourney_level == "G") |>
  mutate(
    tour = "ATP",
    slam = case_when(
      str_detect(tourney_name, "Australian") ~ "Australian Open",
      str_detect(tourney_name, "Roland|French") ~ "Roland Garros",
      str_detect(tourney_name, "Wimbledon") ~ "Wimbledon",
      str_detect(tourney_name, "Us Open|US Open") ~ "US Open",
      TRUE ~ "Other"
    ),
    status = ifelse(got_ll == 1, "LL Selected", "Not Selected")
  )

# WTA GS qualifying losers
wta_qual_gs <- wta_qual |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(year >= 2000, tourney_level == "G", str_detect(round, "^Q"))

wta_final_qr <- wta_qual_gs |>
  group_by(tourney_id) |>
  summarise(max_qual_round = max(round), .groups = "drop")

wta_final_qm <- wta_qual_gs |>
  inner_join(wta_final_qr, by = "tourney_id") |>
  filter(round == max_qual_round)

wta_gs_losers_raw <- wta_final_qm |>
  transmute(
    tourney_id, tourney_name, tourney_date, tourney_level, year,
    player_id = loser_id,
    player_name = loser_name,
    player_rank = loser_rank,
    player_age = loser_age,
    player_ht = loser_ht,
    player_ioc = loser_ioc,
    player_hand = loser_hand,
    tour = "WTA"
  )

# Get WTA LL entries
wta_ll <- bind_rows(
  wta_main |> filter(winner_entry == "LL") |>
    transmute(tourney_id, player_id = winner_id),
  wta_main |> filter(loser_entry == "LL") |>
    transmute(tourney_id, player_id = loser_id)
) |>
  distinct() |>
  mutate(got_ll = 1L)

wta_gs_losers <- wta_gs_losers_raw |>
  left_join(wta_ll, by = c("tourney_id", "player_id")) |>
  mutate(
    got_ll = replace_na(got_ll, 0L),
    slam = case_when(
      str_detect(tourney_name, "Australian") ~ "Australian Open",
      str_detect(tourney_name, regex("Roland|French", ignore_case = TRUE)) ~ "Roland Garros",
      str_detect(tourney_name, "Wimbledon") ~ "Wimbledon",
      str_detect(tourney_name, regex("Us Open|US Open", ignore_case = TRUE)) ~ "US Open",
      TRUE ~ "Other"
    ),
    status = ifelse(got_ll == 1, "LL Selected", "Not Selected")
  )

# Combine
gs_all <- bind_rows(
  atp_gs_losers |> select(tour, slam, status, got_ll),
  wta_gs_losers |> select(tour, slam, status, got_ll)
) |>
  filter(slam != "Other")

# Build cross-tabulation
gs_tab <- gs_all |>
  group_by(slam, tour, status) |>
  summarise(n = n(), .groups = "drop") |>
  pivot_wider(names_from = status, values_from = n, values_fill = 0) |>
  mutate(Total = `LL Selected` + `Not Selected`) |>
  arrange(slam, tour)

message("  GS distribution table:")
print(gs_tab)

# Write LaTeX table
gs_tex <- c(
  "\\begin{tabular}{llrrr}",
  "\\toprule",
  "Grand Slam & Tour & LL Selected & Not Selected & Total \\\\",
  "\\midrule"
)

slams <- c("Australian Open", "Roland Garros", "Wimbledon", "US Open")
for (s in slams) {
  sub <- gs_tab |> filter(slam == s)
  for (i in seq_len(nrow(sub))) {
    r <- sub[i, ]
    slam_label <- if (i == 1) s else ""
    gs_tex <- c(gs_tex, sprintf(
      "%s & %s & %d & %d & %d \\\\",
      slam_label, r$tour, r$`LL Selected`, r$`Not Selected`, r$Total
    ))
  }
  if (s != "US Open") gs_tex <- c(gs_tex, "\\addlinespace")
}

# Totals
gs_totals <- gs_all |>
  group_by(tour) |>
  summarise(ll = sum(got_ll), not_ll = sum(got_ll == 0), .groups = "drop") |>
  mutate(total = ll + not_ll)

gs_tex <- c(gs_tex,
  "\\midrule",
  sprintf("\\textbf{Total} & ATP & %d & %d & %d \\\\",
          gs_totals$ll[gs_totals$tour == "ATP"],
          gs_totals$not_ll[gs_totals$tour == "ATP"],
          gs_totals$total[gs_totals$tour == "ATP"]),
  sprintf(" & WTA & %d & %d & %d \\\\",
          gs_totals$ll[gs_totals$tour == "WTA"],
          gs_totals$not_ll[gs_totals$tour == "WTA"],
          gs_totals$total[gs_totals$tour == "WTA"]),
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(gs_tex, file.path(TABLES_DIR, "table_ll_distribution_gs.tex"))
message("  Saved: Tables/table_ll_distribution_gs.tex")

# --- 2b. RDD sample distribution by tournament level -------------------------
message("\n--- 2b. RDD LL distribution by tournament level ---")

rdd_dist <- rdd |>
  mutate(
    tourney_tier = case_when(
      tourney_level == "G" ~ "Grand Slam",
      tourney_level == "M" ~ "Masters 1000",
      tourney_level == "A" & draw_size >= 48 ~ "ATP 500",
      tourney_level == "A" ~ "ATP 250",
      tourney_level == "C" ~ "Challenger",
      TRUE ~ "Other"
    ),
    status = ifelse(got_ll == 1, "LL", "Control")
  )

rdd_tab <- rdd_dist |>
  group_by(tourney_tier, status) |>
  summarise(n = n(), .groups = "drop") |>
  pivot_wider(names_from = status, values_from = n, values_fill = 0) |>
  mutate(Total = LL + Control)

# Order
tier_order <- c("Grand Slam", "Masters 1000", "ATP 500", "ATP 250", "Challenger", "Other")
rdd_tab <- rdd_tab |>
  mutate(tourney_tier = factor(tourney_tier, levels = tier_order)) |>
  arrange(tourney_tier)

message("  RDD distribution:")
print(rdd_tab)

rdd_tex <- c(
  "\\begin{tabular}{lrrr}",
  "\\toprule",
  "Tournament Level & LL & Control & Total \\\\",
  "\\midrule"
)

for (i in seq_len(nrow(rdd_tab))) {
  r <- rdd_tab[i, ]
  rdd_tex <- c(rdd_tex, sprintf(
    "%s & %d & %d & %d \\\\",
    as.character(r$tourney_tier), r$LL, r$Control, r$Total
  ))
}

rdd_totals <- rdd_dist |>
  summarise(ll = sum(got_ll), ctrl = sum(got_ll == 0))

rdd_tex <- c(rdd_tex,
  "\\midrule",
  sprintf("\\textbf{Total} & %d & %d & %d \\\\",
          rdd_totals$ll, rdd_totals$ctrl, rdd_totals$ll + rdd_totals$ctrl),
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(rdd_tex, file.path(TABLES_DIR, "table_ll_distribution_rdd.tex"))
message("  Saved: Tables/table_ll_distribution_rdd.tex")


# ==============================================================================
# TASK 3: COMPREHENSIVE SUMMARY STATISTICS TABLE
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 3: COMPREHENSIVE SUMMARY STATISTICS TABLE")
message(strrep("=", 70))

# Build two panels: Grand Slam Lottery and ATP RDD

# Function to compute summary stats and balance test
compute_panel_stats <- function(df, vars, labels, group_var = "got_ll") {
  results <- list()
  for (i in seq_along(vars)) {
    v <- vars[i]
    if (!v %in% names(df)) next
    y <- df[[v]]
    g <- df[[group_var]]

    treated <- y[g == 1]
    control <- y[g == 0]

    t_mean <- mean(treated, na.rm = TRUE)
    t_sd   <- sd(treated, na.rm = TRUE)
    c_mean <- mean(control, na.rm = TRUE)
    c_sd   <- sd(control, na.rm = TRUE)
    diff_val <- t_mean - c_mean

    # t-test p-value
    pval <- tryCatch({
      tt <- t.test(treated, control)
      tt$p.value
    }, error = function(e) NA_real_)

    results[[v]] <- tibble(
      variable = v,
      label = labels[i],
      ll_mean = t_mean, ll_sd = t_sd,
      ctrl_mean = c_mean, ctrl_sd = c_sd,
      diff = diff_val, pval = pval,
      n_ll = sum(!is.na(treated)), n_ctrl = sum(!is.na(control))
    )
  }
  bind_rows(results)
}

# Variables for both panels
sumstat_vars <- c("player_age", "player_rank", "player_ht")
sumstat_labels <- c("Age", "Ranking", "Height (cm)")

# Add Elo if available
if ("elo_t0" %in% names(rdd)) {
  sumstat_vars <- c(sumstat_vars, "elo_t0")
  sumstat_labels <- c(sumstat_labels, "Elo Rating")
}

# Add rolling stats if available
if ("win_rate_52w" %in% names(rdd_with_stats)) {
  rdd_for_stats <- rdd_with_stats
  extra_stat_vars <- c("win_rate_52w", "ace_rate_52w", "first_serve_pct_52w",
                        "return_win_rate_52w")
  extra_stat_labels <- c("Win Rate (52w)", "Ace Rate (52w)",
                          "1st Serve \\% (52w)", "Return Win \\% (52w)")
  # Only include if enough data
  for (k in seq_along(extra_stat_vars)) {
    ev <- extra_stat_vars[k]
    if (ev %in% names(rdd_for_stats) &&
        sum(!is.na(rdd_for_stats[[ev]])) > 100) {
      sumstat_vars <- c(sumstat_vars, ev)
      sumstat_labels <- c(sumstat_labels, extra_stat_labels[k])
    }
  }
} else {
  rdd_for_stats <- rdd
}

# Panel A: Grand Slam Lottery
message("\n  Panel A: Grand Slam Lottery...")
if (!is.null(pooled_gs)) {
  gs_stats <- compute_panel_stats(pooled_gs, sumstat_vars, sumstat_labels)
  message("  Lottery panel: ", nrow(gs_stats), " variables")
} else {
  gs_stats <- tibble()
  message("  WARNING: No pooled GS data available")
}

# Panel B: ATP RDD
message("  Panel B: ATP RDD...")
rdd_stats <- compute_panel_stats(rdd_for_stats, sumstat_vars, sumstat_labels)
message("  RDD panel: ", nrow(rdd_stats), " variables")

# Write LaTeX table
sumstat_tex <- c(
  "\\begin{tabular}{l rr rr rr rr rr}",
  "\\toprule",
  " & \\multicolumn{5}{c}{\\textit{Grand Slam Lottery}} & \\multicolumn{5}{c}{\\textit{ATP RDD Sample}} \\\\",
  "\\cmidrule(lr){2-6} \\cmidrule(lr){7-11}",
  " & \\multicolumn{2}{c}{LL} & \\multicolumn{2}{c}{Control} & & \\multicolumn{2}{c}{LL} & \\multicolumn{2}{c}{Control} & \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){7-8} \\cmidrule(lr){9-10}",
  "Variable & Mean & SD & Mean & SD & $p$ & Mean & SD & Mean & SD & $p$ \\\\",
  "\\midrule"
)

# For each variable, write a row combining both panels
all_vars_used <- unique(c(
  if (nrow(gs_stats) > 0) gs_stats$variable else character(),
  if (nrow(rdd_stats) > 0) rdd_stats$variable else character()
))

for (v in all_vars_used) {
  gs_row <- gs_stats |> filter(variable == v)
  rdd_row <- rdd_stats |> filter(variable == v)

  label <- if (nrow(gs_row) > 0) gs_row$label[1] else rdd_row$label[1]

  # GS columns
  if (nrow(gs_row) > 0 && !is.na(gs_row$ll_mean)) {
    gs_part <- sprintf("%.1f & %.1f & %.1f & %.1f & %.3f",
                       gs_row$ll_mean, gs_row$ll_sd,
                       gs_row$ctrl_mean, gs_row$ctrl_sd,
                       gs_row$pval)
  } else {
    gs_part <- "--- & --- & --- & --- & ---"
  }

  # RDD columns
  if (nrow(rdd_row) > 0 && !is.na(rdd_row$ll_mean)) {
    rdd_part <- sprintf("%.1f & %.1f & %.1f & %.1f & %.3f",
                        rdd_row$ll_mean, rdd_row$ll_sd,
                        rdd_row$ctrl_mean, rdd_row$ctrl_sd,
                        rdd_row$pval)
  } else {
    rdd_part <- "--- & --- & --- & --- & ---"
  }

  sumstat_tex <- c(sumstat_tex,
    paste0(label, " & ", gs_part, " & ", rdd_part, " \\\\")
  )
}

# Observation counts
gs_n <- if (!is.null(pooled_gs)) {
  sprintf("$N$ & \\multicolumn{2}{c}{%d} & \\multicolumn{2}{c}{%d} & ",
          sum(pooled_gs$got_ll == 1), sum(pooled_gs$got_ll == 0))
} else "--- & --- & --- & --- & "

rdd_n <- sprintf("& \\multicolumn{2}{c}{%d} & \\multicolumn{2}{c}{%d} & \\\\",
                 sum(rdd$got_ll == 1), sum(rdd$got_ll == 0))

sumstat_tex <- c(sumstat_tex,
  "\\midrule",
  paste0(gs_n, rdd_n),
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(sumstat_tex, file.path(TABLES_DIR, "table_summary_stats.tex"))
message("  Saved: Tables/table_summary_stats.tex")


# ==============================================================================
# TASK 4: FULL WTA LOTTERY ANALYSIS
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 4: FULL WTA LOTTERY ANALYSIS")
message(strrep("=", 70))

# --- 4a. Build WTA lottery sample with full horizons -------------------------
message("\n--- 4a. Building WTA lottery sample with all horizons ---")

# Parse WTA rankings
wta_rankings_parsed <- wta_rankings_raw |>
  mutate(rank_date = as.Date(as.character(ranking_date), format = "%Y%m%d")) |>
  filter(!is.na(rank_date)) |>
  select(player_id = player, rank_date, rank, points)

# Rebuild WTA GS pool with rank_among_losers restriction
wta_gs_losers_full <- wta_gs_losers |>
  group_by(tourney_id) |>
  mutate(
    n_losers = n(),
    player_rank_num = as.numeric(player_rank),
    rank_among_losers = rank(
      ifelse(is.na(player_rank_num), 9999, player_rank_num),
      ties.method = "min"
    )
  ) |>
  ungroup()

# Check if we have player_rank available for WTA losers
# If not, merge from rankings
wta_gs_pool_full <- wta_gs_losers_full |>
  filter(rank_among_losers <= 4) |>
  mutate(event_date = as.Date(as.character(tourney_date), format = "%Y%m%d"))

message("  WTA GS top-4 pool: ", nrow(wta_gs_pool_full),
        " (LL: ", sum(wta_gs_pool_full$got_ll), ")")

# Merge rankings at ALL horizons
wta_all_horizons <- c(-12, -8, -4, 0, 4, 8, 12, 26, 52)

for (h in wta_all_horizons) {
  h_label <- ifelse(h < 0, paste0("minus", abs(h)), as.character(h))
  col_name <- paste0("rank_t", h_label)
  pts_name <- paste0("points_t", h_label)

  if (col_name %in% names(wta_gs_pool_full)) next

  target_df <- wta_gs_pool_full |>
    filter(!is.na(event_date)) |>
    transmute(tourney_id, player_id, target_date = event_date + h * 7)

  matched <- target_df |>
    inner_join(wta_rankings_parsed, by = "player_id", relationship = "many-to-many") |>
    filter(abs(as.numeric(rank_date - target_date)) <= 14) |>
    mutate(date_diff = abs(as.numeric(rank_date - target_date))) |>
    group_by(tourney_id, player_id) |>
    slice_min(date_diff, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(tourney_id, player_id, !!col_name := rank, !!pts_name := points)

  wta_gs_pool_full <- wta_gs_pool_full |>
    left_join(matched, by = c("tourney_id", "player_id"))
}

# Compute all ranking changes
wta_gs_pool_full <- wta_gs_pool_full |>
  mutate(
    rank_change_4w = rank_t4 - rank_t0,
    rank_change_8w = rank_t8 - rank_t0,
    rank_change_12w = rank_t12 - rank_t0,
    rank_change_26w = rank_t26 - rank_t0,
    rank_change_52w = rank_t52 - rank_t0
  )

message("  WTA pool with all horizons: ", nrow(wta_gs_pool_full))
for (h in c(4, 8, 12, 26, 52)) {
  rc_var <- paste0("rank_change_", h, "w")
  if (rc_var %in% names(wta_gs_pool_full)) {
    message("    Non-missing ", rc_var, ": ", sum(!is.na(wta_gs_pool_full[[rc_var]])))
  }
}

# --- 4b. WTA difference-in-means for all horizons ----------------------------
message("\n--- 4b. WTA difference-in-means ---")

wta_horizons_analysis <- c("rank_change_4w", "rank_change_8w", "rank_change_12w",
                            "rank_change_26w", "rank_change_52w")
wta_results <- list()

for (outcome in wta_horizons_analysis) {
  if (!outcome %in% names(wta_gs_pool_full)) next
  y <- wta_gs_pool_full[[outcome]]
  ok <- !is.na(y)
  tr <- wta_gs_pool_full$got_ll[ok] == 1

  if (sum(tr) < 3 || sum(!tr) < 3) {
    message("  ", outcome, ": insufficient treatment variation")
    next
  }

  diff_means <- mean(y[ok][tr]) - mean(y[ok][!tr])
  tt <- tryCatch(t.test(y[ok] ~ wta_gs_pool_full$got_ll[ok]), error = function(e) NULL)

  # OLS with controls
  wta_sub <- wta_gs_pool_full[ok, ] |>
    mutate(
      age_imp = replace_na(player_age, median(player_age, na.rm = TRUE))
    )

  # Check if we have numeric year
  if (!"year" %in% names(wta_sub) || all(is.na(wta_sub$year))) {
    wta_sub$year <- as.integer(str_sub(wta_sub$tourney_date, 1, 4))
  }

  ols <- tryCatch({
    feols(as.formula(paste0(outcome, " ~ got_ll + age_imp | year")),
          data = wta_sub, vcov = ~player_id)
  }, error = function(e) NULL)

  wta_results[[outcome]] <- tibble(
    outcome = outcome,
    sample = "WTA",
    diff_means = diff_means,
    dm_pvalue = if (!is.null(tt)) tt$p.value else NA_real_,
    ols_coef = if (!is.null(ols)) coef(ols)["got_ll"] else NA_real_,
    ols_se = if (!is.null(ols)) sqrt(vcov(ols)["got_ll", "got_ll"]) else NA_real_,
    ols_pvalue = if (!is.null(ols)) pvalue(ols)["got_ll"] else NA_real_,
    n_treated = sum(tr),
    n_control = sum(!tr),
    n_total = sum(ok)
  )

  message("  WTA ", outcome, ": diff = ", round(diff_means, 1),
          " (p = ", round(wta_results[[outcome]]$dm_pvalue, 3),
          ", N = ", sum(ok), ")")
}

wta_results_df <- bind_rows(wta_results)

# BH correction
if (nrow(wta_results_df) > 1) {
  wta_results_df <- wta_results_df |>
    mutate(
      dm_pvalue_bh = p.adjust(dm_pvalue, method = "BH"),
      ols_pvalue_bh = p.adjust(ols_pvalue, method = "BH")
    )
  message("\n  WTA BH-adjusted p-values:")
  for (i in seq_len(nrow(wta_results_df))) {
    r <- wta_results_df[i, ]
    message("    ", r$outcome, ": raw p = ", round(r$dm_pvalue, 3),
            ", BH p = ", round(r$dm_pvalue_bh, 3))
  }
}

# --- 4c. WTA Elo changes (compute WTA Elo first) -----------------------------
message("\n--- 4c. WTA Elo computation (if not already done) ---")

wta_elo_path <- file.path(CLEANED_DIR, "wta_elo_history.rds")
if (file.exists(wta_elo_path)) {
  wta_elo_history <- read_rds(wta_elo_path)
  message("  Loaded existing WTA Elo history: ", nrow(wta_elo_history), " rows")
} else {
  message("  Computing WTA Elo ratings (this may take several minutes)...")

  wta_matches_sorted <- wta_all |>
    mutate(
      tourney_date_num = as.numeric(tourney_date),
      round_order = case_when(
        round == "Q1" ~ 1, round == "Q2" ~ 2, round == "Q3" ~ 3,
        round == "R128" ~ 4, round == "R64" ~ 5, round == "R32" ~ 6,
        round == "R16" ~ 7, round == "QF" ~ 8, round == "SF" ~ 9,
        round == "F" ~ 10, round == "RR" ~ 11, TRUE ~ 0
      )
    ) |>
    arrange(tourney_date_num, tourney_id, round_order, match_num)

  INITIAL_ELO <- 1500; K_NEW <- 32; K_ESTAB <- 24; MATCH_THRESHOLD <- 20
  wta_elo_env <- new.env(hash = TRUE)

  get_elo_w <- function(pid) {
    val <- wta_elo_env[[as.character(pid)]]
    if (is.null(val)) return(list(elo = INITIAL_ELO, n = 0L))
    val
  }
  set_elo_w <- function(pid, elo_val, n_val) {
    wta_elo_env[[as.character(pid)]] <- list(elo = elo_val, n = n_val)
  }
  expected_fn <- function(a, b) 1 / (1 + 10^((b - a) / 400))

  n_w <- nrow(wta_matches_sorted)
  w_elo_pre <- w_elo_post <- l_elo_pre <- l_elo_post <- numeric(n_w)

  for (i in seq_len(n_w)) {
    if (i %% 100000 == 0) message("    WTA Elo: ", i, " / ", n_w)
    wid <- wta_matches_sorted$winner_id[i]
    lid <- wta_matches_sorted$loser_id[i]
    if (is.na(wid) || is.na(lid)) next

    ws <- get_elo_w(wid); ls <- get_elo_w(lid)
    w_elo_pre[i] <- ws$elo; l_elo_pre[i] <- ls$elo

    kw <- if (ws$n < MATCH_THRESHOLD) K_NEW else K_ESTAB
    kl <- if (ls$n < MATCH_THRESHOLD) K_NEW else K_ESTAB

    ew <- expected_fn(ws$elo, ls$elo)
    new_w <- ws$elo + kw * (1 - ew)
    new_l <- ls$elo + kl * (0 - (1 - ew))

    w_elo_post[i] <- new_w; l_elo_post[i] <- new_l
    set_elo_w(wid, new_w, ws$n + 1L)
    set_elo_w(lid, new_l, ls$n + 1L)
  }

  wta_elo_history <- bind_rows(
    wta_matches_sorted |>
      transmute(player_id = winner_id,
                match_date = as.Date(as.character(tourney_date_num), format = "%Y%m%d"),
                elo = w_elo_post),
    wta_matches_sorted |>
      transmute(player_id = loser_id,
                match_date = as.Date(as.character(tourney_date_num), format = "%Y%m%d"),
                elo = l_elo_post)
  ) |>
    filter(!is.na(player_id), !is.na(match_date)) |>
    group_by(player_id, match_date) |>
    summarise(elo = last(elo), .groups = "drop") |>
    arrange(player_id, match_date)

  saveRDS(wta_elo_history, wta_elo_path)
  message("  WTA Elo computed and saved: ", nrow(wta_elo_history), " rows")
}

# Merge WTA Elo at horizons 0, 12, 26
for (h in c(0, 12, 26)) {
  elo_col <- paste0("wta_elo_t", h)
  if (elo_col %in% names(wta_gs_pool_full)) next

  target_df <- wta_gs_pool_full |>
    filter(!is.na(event_date)) |>
    transmute(tourney_id, player_id, target_date = event_date + h * 7)

  matched <- target_df |>
    inner_join(wta_elo_history, by = "player_id", relationship = "many-to-many") |>
    filter(abs(as.numeric(match_date - target_date)) <= 21) |>
    mutate(date_diff = abs(as.numeric(match_date - target_date))) |>
    group_by(tourney_id, player_id) |>
    slice_min(date_diff, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(tourney_id, player_id, !!elo_col := elo)

  wta_gs_pool_full <- wta_gs_pool_full |>
    left_join(matched, by = c("tourney_id", "player_id"))
}

wta_gs_pool_full <- wta_gs_pool_full |>
  mutate(
    wta_elo_change_12w = wta_elo_t12 - wta_elo_t0,
    wta_elo_change_26w = wta_elo_t26 - wta_elo_t0
  )

# Add Elo results to WTA results
for (elo_out in c("wta_elo_change_12w", "wta_elo_change_26w")) {
  if (!elo_out %in% names(wta_gs_pool_full)) next
  y <- wta_gs_pool_full[[elo_out]]
  ok <- !is.na(y)
  tr <- wta_gs_pool_full$got_ll[ok] == 1

  if (sum(tr) >= 3 && sum(!tr) >= 3) {
    diff_means <- mean(y[ok][tr]) - mean(y[ok][!tr])
    tt <- tryCatch(t.test(y[ok] ~ wta_gs_pool_full$got_ll[ok]), error = function(e) NULL)

    wta_results_df <- bind_rows(wta_results_df, tibble(
      outcome = elo_out,
      sample = "WTA",
      diff_means = diff_means,
      dm_pvalue = if (!is.null(tt)) tt$p.value else NA_real_,
      ols_coef = NA_real_, ols_se = NA_real_, ols_pvalue = NA_real_,
      n_treated = sum(tr), n_control = sum(!tr), n_total = sum(ok)
    ))
    message("  WTA ", elo_out, ": diff = ", round(diff_means, 1),
            " (p = ", round(if (!is.null(tt)) tt$p.value else NA, 3), ")")
  }
}

# --- 4d. WTA lottery event study figure ---------------------------------------
message("\n--- 4d. WTA lottery event study figure ---")

wta_rank_cols <- c("rank_tminus12", "rank_tminus8", "rank_tminus4",
                   "rank_t0", "rank_t4", "rank_t8", "rank_t12", "rank_t26", "rank_t52")
wta_week_vals <- c(-12, -8, -4, 0, 4, 8, 12, 26, 52)

wta_avail <- wta_rank_cols[wta_rank_cols %in% names(wta_gs_pool_full)]
wta_avail_weeks <- wta_week_vals[wta_rank_cols %in% names(wta_gs_pool_full)]

if (length(wta_avail) >= 3) {
  wta_es_long <- wta_gs_pool_full |>
    select(tourney_id, player_id, got_ll, all_of(wta_avail)) |>
    pivot_longer(cols = all_of(wta_avail), names_to = "horizon_col", values_to = "ranking") |>
    mutate(
      weeks = case_when(
        horizon_col == "rank_tminus12" ~ -12L,
        horizon_col == "rank_tminus8"  ~ -8L,
        horizon_col == "rank_tminus4"  ~ -4L,
        horizon_col == "rank_t0"       ~ 0L,
        horizon_col == "rank_t4"       ~ 4L,
        horizon_col == "rank_t8"       ~ 8L,
        horizon_col == "rank_t12"      ~ 12L,
        horizon_col == "rank_t26"      ~ 26L,
        horizon_col == "rank_t52"      ~ 52L
      ),
      group = ifelse(got_ll == 1, "LL Selected", "Not Selected")
    ) |>
    filter(!is.na(ranking))

  wta_es_summary <- wta_es_long |>
    group_by(weeks, group) |>
    summarise(
      mean_rank = mean(ranking, na.rm = TRUE),
      se_rank = sd(ranking, na.rm = TRUE) / sqrt(n()),
      n = n(),
      .groups = "drop"
    ) |>
    mutate(
      ci_lower = mean_rank - 1.96 * se_rank,
      ci_upper = mean_rank + 1.96 * se_rank
    )

  p_wta_es <- ggplot(wta_es_summary, aes(x = weeks, y = mean_rank,
                                           color = group, fill = group)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.15, color = NA) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    scale_y_reverse() +
    scale_x_continuous(breaks = wta_avail_weeks) +
    scale_color_manual(values = c("LL Selected" = "#2166AC", "Not Selected" = "#B2182B")) +
    scale_fill_manual(values = c("LL Selected" = "#2166AC", "Not Selected" = "#B2182B")) +
    labs(x = "Weeks Relative to Tournament",
         y = "Average WTA Ranking (Lower = Better)",
         title = NULL) +
    theme_paper() +
    theme(legend.position = "bottom")

  ggsave(file.path(FIGURES_DIR, "fig_wta_lottery_event_study.pdf"),
         p_wta_es, width = 7, height = 5)
  message("  Saved: Figures/fig_wta_lottery_event_study.pdf")
} else {
  message("  WARNING: Insufficient horizon data for WTA event study")
}

# --- 4e. Save WTA results ----------------------------------------------------
saveRDS(wta_results_df, file.path(CLEANED_DIR, "wta_lottery_full_results.rds"))
saveRDS(wta_gs_pool_full, file.path(CLEANED_DIR, "wta_gs_pool_full.rds"))
message("  Saved WTA results: ", nrow(wta_results_df), " rows")


# ==============================================================================
# TASK 5: COMPLETE THE POOLED ATP-WTA TABLE
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 5: COMPLETE POOLED ATP-WTA TABLE")
message(strrep("=", 70))

# Load existing ATP GS results (from 09_revisions or 06_main_analysis)
atp_gs_results <- tryCatch(
  read_rds(file.path(CLEANED_DIR, "main_gs_validation_results.rds")),
  error = function(e) {
    message("  WARNING: ATP GS validation results not found")
    tibble()
  }
)

# Rebuild pooled sample
if (!is.null(pooled_gs)) {
  pooled_data <- pooled_gs |>
    mutate(
      age_imp = replace_na(player_age, median(player_age, na.rm = TRUE)),
      is_wta = as.integer(tour == "WTA")
    )
} else {
  message("  Rebuilding pooled sample...")
  # Minimal rebuild from ATP GS + WTA GS pools
  safe_col <- function(df, col) {
    if (col %in% names(df)) df[[col]] else NA_real_
  }

  atp_gs_sub <- rdd |>
    filter(tourney_level == "G", rank_among_losers <= 4) |>
    mutate(tour = "ATP",
           event_date = as.Date(as.character(tourney_date), format = "%Y%m%d"))

  common <- c("tour", "tourney_id", "tourney_name", "tourney_date", "year",
              "player_id", "player_name", "player_rank", "player_age",
              "got_ll", "event_date")

  pooled_data <- bind_rows(
    atp_gs_sub |> select(any_of(common)),
    wta_gs_pool_full |> mutate(tour = "WTA") |> select(any_of(common))
  ) |>
    mutate(
      age_imp = replace_na(player_age, median(player_age, na.rm = TRUE)),
      is_wta = as.integer(tour == "WTA")
    )
}

# Ensure rank changes are present in pooled data
for (h in c(4, 8, 12, 26, 52)) {
  rc_var <- paste0("rank_change_", h, "w")
  rank_var <- paste0("rank_t", h)
  if (!rc_var %in% names(pooled_data) && rank_var %in% names(pooled_data) &&
      "rank_t0" %in% names(pooled_data)) {
    pooled_data[[rc_var]] <- pooled_data[[rank_var]] - pooled_data[["rank_t0"]]
  }
}

# Run pooled analysis for ALL horizons
message("\n--- Pooled diff-in-means and OLS for all horizons ---")
pooled_all_horizons <- c("rank_change_4w", "rank_change_8w", "rank_change_12w",
                          "rank_change_26w", "rank_change_52w")

pooled_complete_results <- list()

for (outcome in pooled_all_horizons) {
  if (!outcome %in% names(pooled_data)) {
    message("  ", outcome, ": not available in pooled data")
    next
  }

  y <- pooled_data[[outcome]]
  ok <- !is.na(y)
  if (sum(ok) < 20) {
    message("  ", outcome, ": too few obs (", sum(ok), ")")
    next
  }

  tr <- pooled_data$got_ll[ok] == 1
  if (sum(tr) < 3 || sum(!tr) < 3) {
    message("  ", outcome, ": insufficient treatment variation")
    next
  }

  diff_means <- mean(y[ok][tr]) - mean(y[ok][!tr])
  tt <- tryCatch(t.test(y[ok] ~ pooled_data$got_ll[ok]), error = function(e) NULL)

  # OLS with controls + tour indicator
  sub <- pooled_data[ok, ]
  ols <- tryCatch({
    feols(as.formula(paste0(outcome, " ~ got_ll + age_imp + is_wta | year")),
          data = sub, vcov = ~player_id)
  }, error = function(e) NULL)

  pooled_complete_results[[outcome]] <- tibble(
    outcome = outcome,
    sample = "Pooled ATP+WTA",
    diff_means = diff_means,
    dm_pvalue = if (!is.null(tt)) tt$p.value else NA_real_,
    ols_coef = if (!is.null(ols)) coef(ols)["got_ll"] else NA_real_,
    ols_se = if (!is.null(ols)) sqrt(vcov(ols)["got_ll", "got_ll"]) else NA_real_,
    ols_pvalue = if (!is.null(ols)) pvalue(ols)["got_ll"] else NA_real_,
    n_treated = sum(tr),
    n_control = sum(!tr),
    n_total = sum(ok)
  )

  message("  Pooled ", outcome, ": diff = ", round(diff_means, 1),
          " (p = ", round(pooled_complete_results[[outcome]]$dm_pvalue, 3), ")")
}

pooled_complete_df <- bind_rows(pooled_complete_results)

# BH correction
if (nrow(pooled_complete_df) > 1) {
  pooled_complete_df <- pooled_complete_df |>
    mutate(
      dm_pvalue_bh = p.adjust(dm_pvalue, method = "BH"),
      ols_pvalue_bh = p.adjust(ols_pvalue, method = "BH")
    )
}

# Write complete pooled table
message("\n--- Writing complete pooled table ---")

# Also load ATP-only results for comparison
atp_lottery_results <- tryCatch(
  read_rds(file.path(CLEANED_DIR, "main_gs_validation_results.rds")),
  error = function(e) tibble()
)

pool_tex <- c(
  "\\begin{tabular}{lrrrrrr}",
  "\\toprule",
  " & \\multicolumn{3}{c}{Difference in Means} & \\multicolumn{3}{c}{OLS with Controls} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
  "Outcome & Diff. & $p$-value & $N$ & Coef. & SE & $p$-value \\\\",
  "\\midrule",
  "\\multicolumn{7}{l}{\\textit{Panel A: ATP Grand Slam Lottery}} \\\\"
)

# ATP-specific results
atp_specific <- tryCatch(
  read_rds(file.path(CLEANED_DIR, "pooled_gs_lottery_results.rds")) |>
    filter(sample == "Pooled ATP+WTA"),
  error = function(e) tibble()
)

# Use the main GS validation if available, otherwise leave as dashes
if (nrow(atp_gs_results) > 0) {
  for (i in seq_len(nrow(atp_gs_results))) {
    r <- atp_gs_results[i, ]
    pool_tex <- c(pool_tex, sprintf(
      "%s & %.1f & %.3f & %d & --- & --- & --- \\\\",
      gsub("_", "\\_", r$outcome, fixed = TRUE),
      r$coef, r$pv_robust, r$n_eff
    ))
  }
} else {
  pool_tex <- c(pool_tex, "\\multicolumn{7}{c}{(ATP-only results not available)} \\\\")
}

pool_tex <- c(pool_tex,
  "\\midrule",
  "\\multicolumn{7}{l}{\\textit{Panel B: WTA Grand Slam Lottery}} \\\\"
)

if (nrow(wta_results_df) > 0) {
  rank_wta <- wta_results_df |> filter(str_detect(outcome, "rank_change"))
  for (i in seq_len(nrow(rank_wta))) {
    r <- rank_wta[i, ]
    ols_coef_str <- if (!is.na(r$ols_coef)) sprintf("%.1f", r$ols_coef) else "---"
    ols_se_str <- if (!is.na(r$ols_se)) sprintf("(%.1f)", r$ols_se) else "---"
    ols_p_str <- if (!is.na(r$ols_pvalue)) sprintf("%.3f", r$ols_pvalue) else "---"

    pool_tex <- c(pool_tex, sprintf(
      "$\\Delta R_{%s}$ & %.1f & %.3f & %d & %s & %s & %s \\\\",
      gsub("rank_change_", "", r$outcome),
      r$diff_means, r$dm_pvalue, r$n_total,
      ols_coef_str, ols_se_str, ols_p_str
    ))
  }
}

pool_tex <- c(pool_tex,
  "\\midrule",
  "\\multicolumn{7}{l}{\\textit{Panel C: Pooled ATP + WTA}} \\\\"
)

if (nrow(pooled_complete_df) > 0) {
  for (i in seq_len(nrow(pooled_complete_df))) {
    r <- pooled_complete_df[i, ]
    ols_coef_str <- if (!is.na(r$ols_coef)) sprintf("%.1f", r$ols_coef) else "---"
    ols_se_str <- if (!is.na(r$ols_se)) sprintf("(%.1f)", r$ols_se) else "---"
    ols_p_str <- if (!is.na(r$ols_pvalue)) sprintf("%.3f", r$ols_pvalue) else "---"

    pool_tex <- c(pool_tex, sprintf(
      "$\\Delta R_{%s}$ & %.1f & %.3f & %d & %s & %s & %s \\\\",
      gsub("rank_change_", "", r$outcome),
      r$diff_means, r$dm_pvalue, r$n_total,
      ols_coef_str, ols_se_str, ols_p_str
    ))
  }
}

pool_tex <- c(pool_tex,
  "\\midrule",
  "\\multicolumn{7}{l}{\\footnotesize Grand Slam LL assignment by lottery among final-round qualifying losers (top-4 ranked).} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize OLS controls: age, tour indicator (pooled only), year FE. SEs clustered at player level.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize Elo changes available for ATP only (WTA Elo computed but not pooled).} \\\\",
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(pool_tex, file.path(TABLES_DIR, "table_pooled_complete.tex"))
message("  Saved: Tables/table_pooled_complete.tex")

saveRDS(pooled_complete_df, file.path(CLEANED_DIR, "pooled_complete_results.rds"))


# ==============================================================================
# TASK 6: GENDER HETEROGENEITY ANALYSIS
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 6: GENDER HETEROGENEITY ANALYSIS")
message(strrep("=", 70))

gender_lines <- c(
  "# Gender Heterogeneity Analysis",
  paste0("Generated: ", Sys.time()),
  ""
)

# --- 6a. Compare tournament structures ---------------------------------------
message("\n--- 6a. Comparing tournament structures ---")

# ATP tournament structure
atp_tour_struct <- atp_main |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(year >= 2000) |>
  group_by(tourney_level) |>
  summarise(
    n_tournaments = n_distinct(tourney_id),
    avg_draw = mean(draw_size, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(tour = "ATP")

# WTA tournament structure
wta_tour_struct <- wta_main |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(year >= 2000) |>
  group_by(tourney_level) |>
  summarise(
    n_tournaments = n_distinct(tourney_id),
    avg_draw = mean(draw_size, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(tour = "WTA")

tour_comparison <- bind_rows(atp_tour_struct, wta_tour_struct)

gender_lines <- c(gender_lines,
  "## Tournament Structure Comparison",
  "",
  "| Tour | Level | N Tournaments | Avg Draw Size |",
  "|------|-------|---------------|---------------|"
)

for (i in seq_len(nrow(tour_comparison))) {
  r <- tour_comparison[i, ]
  gender_lines <- c(gender_lines, sprintf(
    "| %s | %s | %d | %.0f |",
    r$tour, r$tourney_level, r$n_tournaments, r$avg_draw
  ))
}

# --- 6b. LL slot comparison --------------------------------------------------
message("\n--- 6b. LL slot comparison ---")

# Count LL entries by tour at Grand Slams
atp_gs_ll_count <- atp_main |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(year >= 2000, tourney_level == "G") |>
  filter(winner_entry == "LL" | loser_entry == "LL") |>
  summarise(n_ll_matches = n(), n_tournaments = n_distinct(tourney_id))

wta_gs_ll_count <- wta_main |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(year >= 2000, tourney_level == "G") |>
  filter(winner_entry == "LL" | loser_entry == "LL") |>
  summarise(n_ll_matches = n(), n_tournaments = n_distinct(tourney_id))

gender_lines <- c(gender_lines, "",
  "## LL Entries at Grand Slams",
  sprintf("- ATP: %d LL matches across %d tournaments",
          atp_gs_ll_count$n_ll_matches, atp_gs_ll_count$n_tournaments),
  sprintf("- WTA: %d LL matches across %d tournaments",
          wta_gs_ll_count$n_ll_matches, wta_gs_ll_count$n_tournaments),
  ""
)

# --- 6c. Ranking point systems -----------------------------------------------
message("\n--- 6c. Ranking point comparison ---")

# How much do 10 ranking points move a typical player?
# Compare ranking density at different points levels

atp_ranks <- atp_rankings_raw |>
  mutate(year = as.integer(str_sub(ranking_date, 1, 4))) |>
  filter(year == 2022) |>
  group_by(player) |>
  slice_max(ranking_date, n = 1) |>
  ungroup()

wta_ranks <- wta_rankings_raw |>
  mutate(year = as.integer(str_sub(ranking_date, 1, 4))) |>
  filter(year == 2022) |>
  group_by(player) |>
  slice_max(ranking_date, n = 1) |>
  ungroup()

# Look at players ranked 50-200 (typical qualifying loser range)
atp_density <- atp_ranks |> filter(rank >= 50, rank <= 200)
wta_density <- wta_ranks |> filter(rank >= 50, rank <= 200)

# Points per ranking position (gradient)
if (nrow(atp_density) > 10 && nrow(wta_density) > 10) {
  atp_pts_per_rank <- lm(points ~ rank, data = atp_density)
  wta_pts_per_rank <- lm(points ~ rank, data = wta_density)

  atp_gradient <- abs(coef(atp_pts_per_rank)["rank"])
  wta_gradient <- abs(coef(wta_pts_per_rank)["rank"])

  # How many ranking positions does 10 extra points buy?
  atp_positions_per_10pts <- 10 / atp_gradient
  wta_positions_per_10pts <- 10 / wta_gradient

  gender_lines <- c(gender_lines,
    "## Ranking Point Sensitivity (Ranks 50-200, Year 2022)",
    sprintf("- ATP: %.1f points per ranking position (gradient)", atp_gradient),
    sprintf("- WTA: %.1f points per ranking position (gradient)", wta_gradient),
    sprintf("- ATP: 10 extra points = %.1f ranking positions improvement", atp_positions_per_10pts),
    sprintf("- WTA: 10 extra points = %.1f ranking positions improvement", wta_positions_per_10pts),
    ""
  )

  # Implication for LL effect
  # A typical ATP 250 R1 loss = 0 points; R1 win = 20 points
  gender_lines <- c(gender_lines,
    "## Implication for LL Effect",
    sprintf("- ATP: 20 ranking points (R1 win at ATP 250) = %.0f ranking positions",
            20 / atp_gradient),
    sprintf("- WTA: 20 ranking points (R1 win at WTA equivalent) = %.0f ranking positions",
            20 / wta_gradient),
    "- Hypothesis: If WTA ranking is less dense, same points = larger rank change for ATP",
    "  (which would explain stronger ATP results)",
    ""
  )

  revision_summary$atp_gradient <- atp_gradient
  revision_summary$wta_gradient <- wta_gradient
}

# --- 6d. Interaction test: pooled regression --------------------------------
message("\n--- 6d. Pooled interaction test ---")

if (!is.null(pooled_data) && nrow(pooled_data) > 50) {
  for (outcome in c("rank_change_12w", "rank_change_26w")) {
    if (!outcome %in% names(pooled_data)) next
    y <- pooled_data[[outcome]]
    ok <- !is.na(y)
    if (sum(ok) < 30) next

    interaction_formula <- as.formula(paste0(outcome,
      " ~ got_ll * is_wta + age_imp | year"))

    int_ols <- tryCatch({
      feols(interaction_formula, data = pooled_data[ok, ], vcov = ~player_id)
    }, error = function(e) NULL)

    if (!is.null(int_ols)) {
      ct <- summary(int_ols)$coeftable
      message("  Interaction (", outcome, "):")
      # Print all coefficients
      for (rn in rownames(ct)) {
        message("    ", rn, ": ", round(ct[rn, 1], 2),
                " (SE = ", round(ct[rn, 2], 2),
                ", p = ", round(ct[rn, 4], 3), ")")
      }

      # Extract key coefficients
      ll_main <- if ("got_ll" %in% rownames(ct)) ct["got_ll", ] else rep(NA, 4)
      ll_wta_int <- grep("got_ll.*is_wta|is_wta.*got_ll", rownames(ct), value = TRUE)
      if (length(ll_wta_int) > 0) {
        int_coef <- ct[ll_wta_int[1], ]
        gender_lines <- c(gender_lines,
          sprintf("## Interaction Test: %s", outcome),
          sprintf("- LL main effect (ATP): %.1f (SE=%.1f, p=%.3f)",
                  ll_main[1], ll_main[2], ll_main[4]),
          sprintf("- LL x WTA interaction: %.1f (SE=%.1f, p=%.3f)",
                  int_coef[1], int_coef[2], int_coef[4]),
          sprintf("- Interpretation: WTA LL effect is %.1f + (%.1f) = %.1f relative to ATP",
                  ll_main[1], int_coef[1], ll_main[1] + int_coef[1]),
          ""
        )
      }
    }
  }
}

# Save gender heterogeneity analysis
writeLines(gender_lines, file.path(OUTPUT_DIR, "gender_heterogeneity.md"))
message("  Saved: Output/gender_heterogeneity.md")


# ==============================================================================
# FINAL: COMPREHENSIVE SUMMARY
# ==============================================================================
message("\n", strrep("=", 70))
message("FINAL: WRITING COMPREHENSIVE SUMMARY")
message(strrep("=", 70))

summary_lines <- c(
  "# Major Revision Summary",
  paste0("Generated: ", Sys.time()),
  ""
)

# Task 1 summary
summary_lines <- c(summary_lines,
  "## Task 1: Enhanced Competitive Index",
  sprintf("- Old model McFadden R2: %.4f", revision_summary$comp_r2_old),
  sprintf("- New model McFadden R2: %.4f", revision_summary$comp_r2_new),
  sprintf("- Improvement: %.4f (%.1f%% increase)",
          revision_summary$comp_r2_new - revision_summary$comp_r2_old,
          (revision_summary$comp_r2_new / revision_summary$comp_r2_old - 1) * 100),
  sprintf("- AMEN network model: %s",
          if (revision_summary$amen_success) "SUCCESS" else "FAILED (used enhanced logit)"),
  ""
)

# LL time interaction results
if (!is.null(revision_summary$ll_time_coefs)) {
  summary_lines <- c(summary_lines,
    "### LL Time Interaction (from enhanced win probability model):"
  )
  for (tv in names(revision_summary$ll_time_coefs)) {
    summary_lines <- c(summary_lines,
      sprintf("  - %s: %.4f", tv, revision_summary$ll_time_coefs[tv])
    )
  }
  summary_lines <- c(summary_lines, "")
}

# RDD mechanism results with new index
if (nrow(comp_v2_results_df) > 0) {
  summary_lines <- c(summary_lines,
    "### RDD on New Competitiveness Index:"
  )
  for (i in seq_len(nrow(comp_v2_results_df))) {
    r <- comp_v2_results_df[i, ]
    summary_lines <- c(summary_lines, sprintf(
      "  - %s: LATE = %.4f (p = %.3f, N = %d)",
      r$outcome, r$coef, r$pval, r$eff_n
    ))
  }
  summary_lines <- c(summary_lines, "")
}

# Task 2 summary
summary_lines <- c(summary_lines,
  "## Task 2: LL Distribution Tables",
  sprintf("- GS table: %d total observations across 4 slams x 2 tours",
          nrow(gs_all)),
  sprintf("- RDD table: %d total observations across tournament levels",
          nrow(rdd)),
  "- Saved: Tables/table_ll_distribution_gs.tex, Tables/table_ll_distribution_rdd.tex",
  ""
)

# Task 3 summary
summary_lines <- c(summary_lines,
  "## Task 3: Summary Statistics Table",
  sprintf("- Variables included: %d", length(sumstat_vars)),
  sprintf("- GS Lottery panel: %s available", if (nrow(gs_stats) > 0) "YES" else "NO"),
  sprintf("- RDD panel: %s available", if (nrow(rdd_stats) > 0) "YES" else "NO"),
  "- Saved: Tables/table_summary_stats.tex",
  ""
)

# Task 4 summary
summary_lines <- c(summary_lines,
  "## Task 4: WTA Lottery Analysis",
  sprintf("- WTA GS top-4 pool: %d observations", nrow(wta_gs_pool_full)),
  sprintf("- WTA LL count: %d", sum(wta_gs_pool_full$got_ll))
)
if (nrow(wta_results_df) > 0) {
  for (i in seq_len(nrow(wta_results_df))) {
    r <- wta_results_df[i, ]
    summary_lines <- c(summary_lines, sprintf(
      "  - %s: diff = %.1f (p = %.3f)", r$outcome, r$diff_means, r$dm_pvalue
    ))
  }
}
summary_lines <- c(summary_lines,
  "- Saved: Figures/fig_wta_lottery_event_study.pdf",
  ""
)

# Task 5 summary
summary_lines <- c(summary_lines,
  "## Task 5: Pooled ATP-WTA Table (Complete)"
)
if (nrow(pooled_complete_df) > 0) {
  for (i in seq_len(nrow(pooled_complete_df))) {
    r <- pooled_complete_df[i, ]
    summary_lines <- c(summary_lines, sprintf(
      "  - %s: diff = %.1f (p = %.3f, OLS = %s)",
      r$outcome, r$diff_means, r$dm_pvalue,
      if (!is.na(r$ols_coef)) sprintf("%.1f", r$ols_coef) else "---"
    ))
  }
}
summary_lines <- c(summary_lines,
  "- Saved: Tables/table_pooled_complete.tex",
  ""
)

# Task 6 summary
summary_lines <- c(summary_lines,
  "## Task 6: Gender Heterogeneity",
  "- See Output/gender_heterogeneity.md for full analysis"
)
if (!is.null(revision_summary$atp_gradient)) {
  summary_lines <- c(summary_lines,
    sprintf("- ATP ranking gradient: %.1f points/position", revision_summary$atp_gradient),
    sprintf("- WTA ranking gradient: %.1f points/position", revision_summary$wta_gradient)
  )
}
summary_lines <- c(summary_lines, "")

# Output file list
summary_lines <- c(summary_lines,
  "## Output Files",
  "### Data",
  "- Data/cleaned/competitive_index_v2.rds",
  "- Data/cleaned/win_model_v2.rds",
  "- Data/cleaned/mechanism_competitiveness_v2.rds",
  "- Data/cleaned/wta_lottery_full_results.rds",
  "- Data/cleaned/wta_gs_pool_full.rds",
  "- Data/cleaned/pooled_complete_results.rds",
  if (file.exists(file.path(CLEANED_DIR, "wta_elo_history.rds")))
    "- Data/cleaned/wta_elo_history.rds" else NULL,
  "",
  "### Tables",
  "- Tables/table_ll_distribution_gs.tex",
  "- Tables/table_ll_distribution_rdd.tex",
  "- Tables/table_summary_stats.tex",
  "- Tables/table_pooled_complete.tex",
  "",
  "### Figures",
  "- Figures/fig_wta_lottery_event_study.pdf",
  "",
  "### Reports",
  "- Output/gender_heterogeneity.md",
  "- Output/major_revision_summary.md"
)

writeLines(summary_lines, file.path(OUTPUT_DIR, "major_revision_summary.md"))
message("  Saved: Output/major_revision_summary.md")

message("\n", strrep("=", 70))
message("ALL TASKS COMPLETE")
message(strrep("=", 70))
