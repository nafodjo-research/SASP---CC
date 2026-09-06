# ==============================================================================
# 22_tournament_rebuild.R
# COMPLETE REBUILD: Sequential Bernoulli Tournament Performance Model
#
# Purpose:  CF-IV estimation of the causal effect of Lucky Loser entry on
#           match-level winning probability, with proper sample construction,
#           analytically-imposed Bernoulli convolution instrument, and
#           counterfactual tournament quantities.
#
# Addresses 18 audit action items:
#   1: Final-round qualifying losers only
#   2: LL-granting events only
#   3-4: Separate GS/nonGS x ATP/WTA (4 models)
#   5: Truncation at next LL opportunity (not any qualifying loss)
#   6: Drop crude instrument; use only Bernoulli convolution P_i^{LL}
#   7: Fix counterfactual formula (include rho*v_hat)
#   8: 200-rep player-level block bootstrap
#   9a: Rename "win tourney" -> "tournament performance probability"
#   9b: Expected ranking points E[RP|d]
#   10: Points-weighted logit robustness
#   11: H2H encounter count in win model
#   12: Four-panel calendar-horizon figure
#   13: Drop fig_dynamic_effects.pdf
#   14: Enhanced win model with Z_ie^pre and impose P_i^LL directly
#   15: Document estimation sample = LL candidates only
#   16: Align sample periods (GS 2006-2024, nonGS-ATP 2007+, nonGS-WTA 2009+)
#   17: No first-stage probit needed (P_i^LL imposed analytically)
#   18: Generate 4 tables
#
# Inputs:
#   Data/raw/atp_main_matches.rds, atp_qual_chall_matches.rds
#   Data/raw/wta_main_matches.rds, wta_qual_itf_matches.rds
#   Data/raw/atp_players.rds, wta_players.rds
#   Data/cleaned/tournament_elo_cache.rds (95MB, skip Elo recomputation)
#
# Outputs:
#   Tables/table_tournament_gs_atp.tex
#   Tables/table_tournament_gs_wta.tex
#   Tables/table_tournament_nongs_atp.tex
#   Tables/table_tournament_nongs_wta.tex
#   Figures/fig_tournament_4panel.pdf
#   Data/cleaned/tournament_rebuild_results.rds
#   Output/tournament_rebuild_summary.md
#
# Dependencies: dplyr, tidyr, readr, stringr, ggplot2, data.table, patchwork, here
# ==============================================================================

set.seed(20260326)

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(data.table)
library(patchwork)
library(here)

source(here("scripts", "R", "utils.R"))

# -- Project paths -----------------------------------------------------------
RAW_DIR     <- here("Data", "raw")
CLEANED_DIR <- here("Data", "cleaned")
TABLE_DIR   <- here("Tables")
FIG_DIR     <- here("Figures")
OUTPUT_DIR  <- here("Output")

for (d in c(CLEANED_DIR, TABLE_DIR, FIG_DIR, OUTPUT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# -- Analysis parameters -----------------------------------------------------
CALENDAR_CAP     <- 365L       # Max days after qualifying loss
N_BOOT           <- 200L       # Bootstrap replications (Item 8)
GS_MIN_YEAR      <- 2006L      # Item 16
NONGS_ATP_MIN_YR <- 2007L      # Item 16
NONGS_WTA_MIN_YR <- 2009L      # Item 16

# -- Ranking points schedules (Item 9b) --------------------------------------
# Approximate ATP/WTA points by round for each tournament tier
POINTS_SCHEDULE <- list(
  G       = c(R128 = 10, R64 = 45, R32 = 90, R16 = 180, QF = 360, SF = 720, F = 1200, W = 2000),
  M       = c(R64 = 10, R32 = 25, R16 = 45, QF = 90, SF = 180, F = 360, W = 600),
  PM      = c(R64 = 10, R32 = 25, R16 = 45, QF = 90, SF = 180, F = 360, W = 600),
  A500    = c(R32 = 0, R16 = 20, QF = 45, SF = 90, F = 180, W = 300),
  P       = c(R32 = 0, R16 = 20, QF = 45, SF = 90, F = 180, W = 300),
  A250    = c(R32 = 0, R16 = 20, QF = 45, SF = 90, F = 150, W = 250),
  I       = c(R32 = 0, R16 = 20, QF = 45, SF = 90, F = 150, W = 250)
)

# -- Initialize summary log --------------------------------------------------
summary_log <- character()

cat("\n")
message(strrep("=", 72))
message("  TOURNAMENT PERFORMANCE MODEL REBUILD (22_tournament_rebuild.R)")
message(strrep("=", 72))

###############################################################################
# PHASE 1: DATA LOADING
###############################################################################

message("\n[PHASE 1] Loading raw match data...")

atp_main <- read_rds(file.path(RAW_DIR, "atp_main_matches.rds"))
atp_qual <- read_rds(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
wta_main <- read_rds(file.path(RAW_DIR, "wta_main_matches.rds"))
wta_qual <- read_rds(file.path(RAW_DIR, "wta_qual_itf_matches.rds"))
atp_players <- read_rds(file.path(RAW_DIR, "atp_players.rds"))
wta_players <- read_rds(file.path(RAW_DIR, "wta_players.rds"))

slog("## Phase 1: Data Loading")
slog("- ATP main matches: ", nrow(atp_main))
slog("- ATP qual matches: ", nrow(atp_qual))
slog("- WTA main matches: ", nrow(wta_main))
slog("- WTA qual matches: ", nrow(wta_qual))
slog("")

###############################################################################
# PHASE 1A: HARMONIZE AND COMBINE MATCH DATA
###############################################################################

message("[PHASE 1A] Harmonizing match data...")

harmonize_matches <- function(main_df, qual_df, tour_label) {
  ensure_cols <- function(df) {
    if (!"winner_entry" %in% names(df)) df$winner_entry <- NA_character_
    if (!"loser_entry" %in% names(df)) df$loser_entry <- NA_character_
    if (!"best_of" %in% names(df)) df$best_of <- NA_integer_
    if (!"winner_age" %in% names(df)) df$winner_age <- NA_real_
    if (!"loser_age" %in% names(df)) df$loser_age <- NA_real_
    if (!"match_num" %in% names(df)) df$match_num <- seq_len(nrow(df))
    if (!"draw_size" %in% names(df)) df$draw_size <- NA_integer_
    if (!"winner_id" %in% names(df)) df$winner_id <- NA_integer_
    if (!"loser_id" %in% names(df)) df$loser_id <- NA_integer_
    if (!"winner_ht" %in% names(df)) df$winner_ht <- NA_real_
    if (!"loser_ht" %in% names(df)) df$loser_ht <- NA_real_
    if (!"winner_hand" %in% names(df)) df$winner_hand <- NA_character_
    if (!"loser_hand" %in% names(df)) df$loser_hand <- NA_character_
    if (!"winner_ioc" %in% names(df)) df$winner_ioc <- NA_character_
    if (!"loser_ioc" %in% names(df)) df$loser_ioc <- NA_character_
    if (!"winner_rank" %in% names(df)) df$winner_rank <- NA_integer_
    if (!"loser_rank" %in% names(df)) df$loser_rank <- NA_integer_
    if (!"winner_rank_points" %in% names(df)) df$winner_rank_points <- NA_real_
    if (!"loser_rank_points" %in% names(df)) df$loser_rank_points <- NA_real_
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

###############################################################################
# PHASE 1B: ELO RATINGS (LOAD CACHE)
###############################################################################

message("\n[PHASE 1B] Loading Elo cache...")

elo_cache_path <- here("Data", "cleaned", "tournament_elo_cache.rds")
if (!file.exists(elo_cache_path)) {
  stop("Elo cache not found at ", elo_cache_path,
       ". Run 12_tournament_performance_optimized.R first to build it.")
}
elo_list <- readRDS(elo_cache_path)
message("  Loaded Elo cache: ", length(ls(elo_list$overall)), " players")

# Helper: get Elo at date from environment-based storage
get_elo_at_date <- function(env, player, date) {
  df <- env[[player]]
  if (is.null(df)) return(1500)
  v <- df[df$date <= date, ]
  if (nrow(v) == 0) return(1500)
  tail(v$rating, 1)
}

###############################################################################
# PHASE 1C: H2H PRECOMPUTATION
###############################################################################

message("\n[PHASE 1C] Precomputing H2H lookup (data.table)...")

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
message("  H2H records: ", nrow(h2h_table))

###############################################################################
# PHASE 1D: SAMPLE CONSTRUCTION (Items 1, 2, 3, 4, 5, 6, 16)
###############################################################################

message("\n[PHASE 1D] Building LL candidate event table...")

# Item 15 documentation: The estimation sample consists ONLY of LL candidates,
# i.e., players who lost in the final round of qualifying at an LL-granting event.
# Treated = those who received LL entry. Control = those who did not.
# This is NOT all tour matches -- it is the subset of matches played by players
# from the LL candidate pool in tournaments following their qualifying loss.

build_event_table <- function(qual_df, main_df, tour_label) {
  # Tour-specific valid levels and year cutoffs (Item 16)
  if (tour_label == "ATP") {
    valid_levels <- c("G", "M", "A")
    min_nongs_year <- NONGS_ATP_MIN_YR
  } else {
    valid_levels <- c("G", "PM", "P", "I")
    min_nongs_year <- NONGS_WTA_MIN_YR
  }

  # Parse qualifying matches
  qual_tour <- qual_df |>
    mutate(
      tourney_date = if (inherits(tourney_date, "Date")) tourney_date
                     else as.Date(as.character(tourney_date), format = "%Y%m%d"),
      year = as.integer(format(tourney_date, "%Y"))
    ) |>
    filter(year >= 2000, year <= 2024,
           tourney_level %in% valid_levels,
           str_detect(round, "^Q"))

  # Apply year restrictions (Item 16)
  qual_tour <- qual_tour |>
    filter(tourney_level == "G" | year >= min_nongs_year) |>
    filter(!(tourney_level == "G" & year < GS_MIN_YEAR))

  # Item 1: Final qualifying round ONLY per tournament
  final_round <- qual_tour |>
    group_by(tourney_id) |>
    summarise(max_qual_round = max(round), .groups = "drop")

  final_matches <- qual_tour |>
    inner_join(final_round, by = "tourney_id") |>
    filter(round == max_qual_round)

  message("  ", tour_label, " final qualifying round matches: ", nrow(final_matches))

  # Losers of final qualifying round = LL candidate pool
  ql <- final_matches |>
    transmute(
      tourney_id, tourney_name, tourney_date, tourney_level, surface, draw_size,
      year,
      player_id = loser_id, player_name = loser_name,
      player_rank = loser_rank, player_rank_points = loser_rank_points,
      player_age = loser_age, player_hand = loser_hand,
      player_ht = loser_ht, player_ioc = loser_ioc,
      qual_opponent_id = winner_id, qual_opponent_rank = winner_rank,
      tour = tour_label,
      player_pid = paste0(tour_label, "_", loser_id)
    )

  # Identify LL entries from main draw
  ll_w <- main_df |>
    mutate(year = as.integer(format(
      if (inherits(tourney_date, "Date")) tourney_date
      else as.Date(as.character(tourney_date), format = "%Y%m%d"), "%Y"))) |>
    filter(year >= 2000, !is.na(winner_entry), winner_entry == "LL") |>
    transmute(tourney_id, ll_player_id = winner_id)
  ll_l <- main_df |>
    mutate(year = as.integer(format(
      if (inherits(tourney_date, "Date")) tourney_date
      else as.Date(as.character(tourney_date), format = "%Y%m%d"), "%Y"))) |>
    filter(year >= 2000, !is.na(loser_entry), loser_entry == "LL") |>
    transmute(tourney_id, ll_player_id = loser_id)
  ll_entries <- bind_rows(ll_w, ll_l) |> distinct(tourney_id, ll_player_id)

  ql <- ql |>
    mutate(got_ll = as.integer(paste0(tourney_id, "_", player_id) %in%
                                 paste0(ll_entries$tourney_id, "_", ll_entries$ll_player_id)))

  # Item 2: LL slots per tournament; filter to events with >= 1 LL awarded

  ll_slots <- ql |>
    group_by(tourney_id) |>
    summarise(n_ll_slots = sum(got_ll), .groups = "drop")
  ql <- ql |>
    left_join(ll_slots, by = "tourney_id") |>
    mutate(n_ll_slots = replace_na(n_ll_slots, 0L))

  n_before_ll_filter <- nrow(ql)
  ql <- ql |> filter(n_ll_slots > 0)
  message("  ", tour_label, " after LL>=1 filter: ", nrow(ql),
          " (dropped ", n_before_ll_filter - nrow(ql), " from 0-LL events)")

  # Rank among losers (for computing Bernoulli convolution)
  ql <- ql |>
    group_by(tourney_id) |>
    mutate(
      n_losers_at_event = n(),
      rank_among_losers = rank(ifelse(is.na(player_rank), 9999, player_rank),
                                ties.method = "min")
    ) |>
    ungroup()

  # GS top-4 pool restriction
  if (any(ql$tourney_level == "G")) {
    n_gs_pre <- sum(ql$tourney_level == "G")
    ql <- ql |> filter(tourney_level != "G" | rank_among_losers <= 4)
    n_gs_post <- sum(ql$tourney_level == "G")
    message("  ", tour_label, " GS top-4 pool: ", n_gs_pre, " -> ", n_gs_post)
  }

  # LL history variables
  ql <- ql |>
    arrange(player_id, tourney_date) |>
    group_by(player_id) |>
    mutate(
      cum_ll = cumsum(got_ll) - got_ll,
      had_prior_ll = as.integer(cum_ll > 0)
    ) |>
    ungroup() |>
    select(-cum_ll)

  # Pre-treatment Elo
  ql$pre_elo <- NA_real_
  for (i in seq_len(nrow(ql))) {
    ql$pre_elo[i] <- get_elo_at_date(elo_list$overall, ql$player_pid[i], ql$tourney_date[i])
  }

  # Pre-treatment rank points
  ql <- ql |> mutate(pre_rank_pts = coalesce(player_rank_points, 0))

  # Unique event ID
  ql$event_id <- seq_len(nrow(ql))

  message("  ", tour_label, " total events: ", nrow(ql),
          " (LL: ", sum(ql$got_ll), ", Control: ", sum(ql$got_ll == 0), ")")

  ql
}

atp_events <- build_event_table(atp_qual, atp_main, "ATP")
wta_events <- build_event_table(wta_qual, wta_main, "WTA")

# Item 3-4: Split into 4 samples
gs_atp_ev   <- atp_events |> filter(tourney_level == "G")
gs_wta_ev   <- wta_events |> filter(tourney_level == "G")
nongs_atp_ev <- atp_events |> filter(tourney_level != "G")
nongs_wta_ev <- wta_events |> filter(tourney_level != "G")

# Re-assign event_ids within each sample for cleanliness
gs_atp_ev$event_id   <- seq_len(nrow(gs_atp_ev))
gs_wta_ev$event_id   <- seq_len(nrow(gs_wta_ev))
nongs_atp_ev$event_id <- seq_len(nrow(nongs_atp_ev))
nongs_wta_ev$event_id <- seq_len(nrow(nongs_wta_ev))

slog("## Phase 1D: Sample Construction")
slog("- GS-ATP events: ", nrow(gs_atp_ev), " (LL: ", sum(gs_atp_ev$got_ll), ")")
slog("- GS-WTA events: ", nrow(gs_wta_ev), " (LL: ", sum(gs_wta_ev$got_ll), ")")
slog("- nonGS-ATP events: ", nrow(nongs_atp_ev), " (LL: ", sum(nongs_atp_ev$got_ll), ")")
slog("- nonGS-WTA events: ", nrow(nongs_wta_ev), " (LL: ", sum(nongs_wta_ev$got_ll), ")")
slog("")


###############################################################################
# PHASE 2: WIN PROBABILITY MODEL (Items 11, 14)
###############################################################################

message("\n[PHASE 2] Estimating tour-specific win probability models...")

# Build pairwise match dataset for win model estimation
# Exclude LL entries to avoid circularity (Item 14)
build_win_model_data <- function(main_df, tour_label) {
  main_df <- main_df |>
    mutate(
      tourney_date = if (inherits(tourney_date, "Date")) tourney_date
                     else as.Date(as.character(tourney_date), format = "%Y%m%d"),
      year = as.integer(format(tourney_date, "%Y"))
    ) |>
    filter(year >= 2000, year <= 2024,
           match_source == "main")

  # Exclude LL entries
  main_df <- main_df |>
    filter(is.na(winner_entry) | winner_entry != "LL") |>
    filter(is.na(loser_entry) | loser_entry != "LL")

  # Create pairwise rows (winner perspective: won=1)
  # We only need winner rows since we model P(winner wins) = 1
  # and the loser row is just 1-P. We'll model with both perspectives.
  win_rows <- main_df |>
    transmute(
      tourney_id, tourney_date, tourney_level, surface, year,
      focal_pid = paste0(tour_label, "_", winner_id),
      opp_pid   = paste0(tour_label, "_", loser_id),
      focal_rank = winner_rank, opp_rank = loser_rank,
      focal_age = winner_age, opp_age = loser_age,
      focal_ht = winner_ht, opp_ht = loser_ht,
      focal_hand = winner_hand, opp_hand = loser_hand,
      focal_ioc = winner_ioc, opp_ioc = loser_ioc,
      won = 1L
    )
  lose_rows <- main_df |>
    transmute(
      tourney_id, tourney_date, tourney_level, surface, year,
      focal_pid = paste0(tour_label, "_", loser_id),
      opp_pid   = paste0(tour_label, "_", winner_id),
      focal_rank = loser_rank, opp_rank = winner_rank,
      focal_age = loser_age, opp_age = winner_age,
      focal_ht = loser_ht, opp_ht = winner_ht,
      focal_hand = loser_hand, opp_hand = winner_hand,
      focal_ioc = loser_ioc, opp_ioc = winner_ioc,
      won = 0L
    )

  pairwise <- bind_rows(win_rows, lose_rows) |>
    mutate(
      # X_ijm: match-level pairwise variables
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

  # Filter to complete cases on critical variables
  pairwise <- pairwise |>
    filter(!is.na(focal_rank), !is.na(opp_rank),
           focal_rank > 0, opp_rank > 0)

  message("  ", tour_label, " win model training data: ", nrow(pairwise), " rows")
  pairwise
}

atp_win_data <- build_win_model_data(atp_all, "ATP")
wta_win_data <- build_win_model_data(wta_all, "WTA")

# Add H2H features (Item 11)
add_h2h_features <- function(win_data, h2h_dt) {
  dt <- as.data.table(win_data)
  dt[, `:=`(
    pA = fifelse(focal_pid < opp_pid, focal_pid, opp_pid),
    pB = fifelse(focal_pid < opp_pid, opp_pid, focal_pid),
    focal_is_pA = (focal_pid < opp_pid)
  )]
  dt[, lookup_date := tourney_date - 1]
  setkey(dt, pA, pB, lookup_date)

  merged <- h2h_dt[dt, roll = TRUE, on = .(pA, pB, date = lookup_date), nomatch = NA]

  # Compute Laplace-smoothed H2H and n_h2h
  merged[, `:=`(
    h2h_smoothed = fifelse(is.na(cum_total), 0.5,
                           fifelse(focal_is_pA,
                                   (cum_wins_A + 1) / (cum_total + 2),
                                   (cum_total - cum_wins_A + 1) / (cum_total + 2))),
    n_h2h = fifelse(is.na(cum_total), 0L, as.integer(cum_total))
  )]

  # Clean up and return
  keep <- c("tourney_id", "tourney_date", "tourney_level", "surface", "year",
            "focal_pid", "opp_pid", "focal_rank", "opp_rank",
            "focal_age", "opp_age", "focal_ht", "opp_ht",
            "focal_hand", "opp_hand", "focal_ioc", "opp_ioc", "won",
            "log_rank_ratio", "log_rank_ratio_sq", "rank_diff",
            "same_ioc", "is_clay", "is_grass", "age_diff", "height_diff",
            "hand_mismatch", "h2h_smoothed", "n_h2h")
  keep <- intersect(keep, names(merged))
  as.data.frame(merged[, ..keep])
}

message("  Adding H2H features...")
atp_win_data <- add_h2h_features(atp_win_data, h2h_table)
wta_win_data <- add_h2h_features(wta_win_data, h2h_table)

# Fit tour-specific logit win models
fit_win_model <- function(win_data, tour_label) {
  model <- glm(
    won ~ log_rank_ratio + log_rank_ratio_sq + rank_diff +
      same_ioc + is_clay + is_grass + age_diff + height_diff +
      hand_mismatch + h2h_smoothed + n_h2h,
    family = binomial(link = "logit"),
    data = win_data
  )
  s <- summary(model)
  message("  ", tour_label, " win model: N=", nrow(win_data),
          ", AIC=", round(s$aic, 1),
          ", rank_ratio coef=", round(coef(model)["log_rank_ratio"], 4))
  model
}

atp_win_model <- fit_win_model(atp_win_data, "ATP")
wta_win_model <- fit_win_model(wta_win_data, "WTA")

slog("## Phase 2: Win Probability Models")
slog("- ATP win model N: ", nrow(atp_win_data), ", AIC: ", round(summary(atp_win_model)$aic, 1))
slog("- WTA win model N: ", nrow(wta_win_data), ", AIC: ", round(summary(wta_win_model)$aic, 1))
slog("")


###############################################################################
# PHASE 3: BERNOULLI CONVOLUTION P_i^{LL} (Items 6, 14, 17)
###############################################################################

message("\n[PHASE 3] Computing Bernoulli convolution P_i^{LL}...")

# Item 17: No first-stage probit regression is needed. P_i^{LL} is computed
# analytically from the institutional selection mechanism (ranking-based for
# non-GS, lottery for GS) and the win model probabilities.

# Bernoulli convolution: P(sum of independent Bernoullis >= threshold)
bernoulli_conv_ge <- function(probs, threshold) {
  if (length(probs) == 0) return(if (threshold <= 0) 1.0 else 0.0)
  if (threshold <= 0) return(1.0)
  if (threshold > length(probs)) return(0.0)
  n <- length(probs)
  prob <- numeric(n + 1)
  prob[1] <- 1.0
  for (k in seq_along(probs)) {
    pk <- probs[k]
    new_prob <- numeric(n + 1)
    for (j in seq(min(k, n), 0, -1)) {
      new_prob[j + 1] <- prob[j + 1] * (1 - pk)
      if (j >= 1) new_prob[j + 1] <- new_prob[j + 1] + prob[j] * pk
    }
    prob <- new_prob
  }
  sum(prob[(threshold + 1):(n + 1)])
}

compute_p_ll <- function(ev_table, qual_df, win_model, tour_label) {
  # For each event, we need:
  #   - The qualifying match opponents and predicted win probabilities
  #   - The number of LL slots (n_ll_slots)
  #   - The rank ordering of qualifying losers
  #
  # For GS: LL is by lottery among top-4 ranked losers, so P_i^{LL} = n_ll_slots / 4
  #          (conditional on being in the eligible pool, which we've already filtered)
  #
  # For non-GS: LL slots go to highest-ranked losing qualifier.
  #   P_i^{LL} = P(rank_i among losers <= n_ll_slots)
  #   This requires computing P(each higher-ranked qualifier loses their final Q match),
  #   then using Bernoulli convolution.

  # Parse qualifying final-round match data for this tour
  qual_parsed <- qual_df |>
    mutate(
      tourney_date = if (inherits(tourney_date, "Date")) tourney_date
                     else as.Date(as.character(tourney_date), format = "%Y%m%d"),
      year = as.integer(format(tourney_date, "%Y"))
    ) |>
    filter(year >= 2000, year <= 2024)

  # Final qualifying round per tournament
  qual_rounds <- qual_parsed |>
    filter(str_detect(round, "^Q")) |>
    group_by(tourney_id) |>
    summarise(max_qual_round = max(round), .groups = "drop")

  final_q <- qual_parsed |>
    inner_join(qual_rounds, by = "tourney_id") |>
    filter(round == max_qual_round) |>
    mutate(
      winner_pid = paste0(tour_label, "_", winner_id),
      loser_pid  = paste0(tour_label, "_", loser_id)
    )

  ev_table$p_ll <- NA_real_

  for (i in seq_len(nrow(ev_table))) {
    tid <- ev_table$tourney_id[i]
    pid <- ev_table$player_id[i]
    player_rank <- ev_table$player_rank[i]
    n_slots <- ev_table$n_ll_slots[i]
    tlevel <- ev_table$tourney_level[i]

    if (tlevel == "G") {
      # GS: lottery among top-4 ranked losers
      # P_i^{LL} = n_slots / (number in eligible pool at this event)
      n_eligible <- sum(ev_table$tourney_id == tid)
      ev_table$p_ll[i] <- min(n_slots / max(n_eligible, 1), 1.0)
    } else {
      # Non-GS: ranking-based selection
      # Find all final-round qualifying matches at this tournament
      q_matches <- final_q |> filter(tourney_id == tid)
      if (nrow(q_matches) == 0) {
        ev_table$p_ll[i] <- NA_real_
        next
      }

      # Get the ranks of all qualifying losers
      losers_info <- q_matches |>
        transmute(
          loser_id, loser_rank, loser_pid,
          winner_id, winner_rank, winner_pid
        )

      # For focal player i, identify qualifiers with BETTER (lower) rank
      # These are the ones who, if they also lose, would rank ahead of i
      # Actually, the LL goes to the best-ranked LOSER. So we need to know
      # which qualifiers in the same round have better rank than player i.
      # If they WIN their match, they qualify (not losers). If they LOSE, they
      # are in the LL pool and ranked above player i.

      # All qualifiers in the final round (including the focal player's match)
      # A qualifier with rank better than pid who LOSES will be ahead of pid in LL pool
      # We need P(qualifier j loses) for all j with rank < player_rank

      if (is.na(player_rank)) {
        ev_table$p_ll[i] <- NA_real_
        next
      }

      # Higher-ranked qualifiers (those with lower rank number)
      higher_ranked <- losers_info |>
        filter(!is.na(loser_rank) | !is.na(winner_rank)) |>
        # Get the qualifier's rank (they could be winner or loser of final Q match)
        # Actually: the "loser" in final_q is already the one who lost.
        # We need the ENTRANTS to the final qualifying round.
        # The losers are the LL candidates. But we need to think about this
        # from the perspective of: before the final Q round is played,
        # which qualifiers have rank better than i?

        # In final_q, winner = qualifier who qualified, loser = LL candidate
        # We need all pairs: for each final Q match, the TWO players competing.
        # If the higher-ranked player WINS, they qualify. If they LOSE, they
        # enter the LL pool ranked above i.

        # So: for each final Q match at this tournament EXCEPT player i's match,
        # we need P(higher-ranked player loses their match).
        # Actually, we need P(player with rank < player_rank loses their match).
        filter(TRUE)  # placeholder

      # Recompute properly: get all final Q match entrants
      q_entrants <- bind_rows(
        q_matches |> transmute(qualifier_id = winner_id,
                                qualifier_pid = winner_pid,
                                qualifier_rank = winner_rank,
                                opponent_id = loser_id,
                                opponent_pid = loser_pid,
                                opponent_rank = loser_rank,
                                qualifier_won = 1L),
        q_matches |> transmute(qualifier_id = loser_id,
                                qualifier_pid = loser_pid,
                                qualifier_rank = loser_rank,
                                opponent_id = winner_id,
                                opponent_pid = winner_pid,
                                opponent_rank = winner_rank,
                                qualifier_won = 0L)
      )

      # The focal player: lost their Q match (qualifier_won = 0)
      focal_row <- q_entrants |>
        filter(qualifier_id == pid, qualifier_won == 0L)

      if (nrow(focal_row) == 0) {
        ev_table$p_ll[i] <- NA_real_
        next
      }

      # Other qualifiers with BETTER rank who competed in this round
      # (not the focal player's match opponent)
      other_entrants <- q_entrants |>
        filter(qualifier_id != pid,
               qualifier_id != focal_row$opponent_id[1]) |>
        # Only those with better (lower) rank
        filter(!is.na(qualifier_rank), qualifier_rank < player_rank)

      # For each such qualifier, compute P(they lose their final Q match)
      # using the win model
      loss_probs <- numeric(0)
      for (j in seq_len(nrow(other_entrants))) {
        # Build prediction data for this qualifier vs their opponent
        ent <- other_entrants[j, ]
        pred_data <- data.frame(
          log_rank_ratio = log(pmax(ent$opponent_rank, 1) / pmax(ent$qualifier_rank, 1)),
          log_rank_ratio_sq = log(pmax(ent$opponent_rank, 1) / pmax(ent$qualifier_rank, 1))^2,
          rank_diff = ent$opponent_rank - ent$qualifier_rank,
          same_ioc = 0L,
          is_clay = 0L, is_grass = 0L,
          age_diff = 0, height_diff = 0, hand_mismatch = 0L,
          h2h_smoothed = 0.5, n_h2h = 0L,
          stringsAsFactors = FALSE
        )

        p_win <- tryCatch(
          predict(win_model, newdata = pred_data, type = "response"),
          error = function(e) 0.5
        )
        loss_probs <- c(loss_probs, 1 - p_win)  # P(this qualifier loses)
      }

      # P_i^{LL} = P(number of higher-ranked losers < n_slots - (n_slots already used))
      # More precisely: among qualifiers with rank < player_rank,
      # count how many LOSE. Player i gets LL if the number of higher-ranked
      # losers (who rank above i) is < n_slots.
      # So P_i^{LL} = P(#{higher-ranked losers} < n_slots)
      # But player i IS already a loser. The question is: among the other
      # losers, how many rank above me?
      # P_i^{LL} = P(#{others with rank < my_rank who ALSO lose} < n_slots)

      if (length(loss_probs) == 0) {
        # No one ranks above me who is also in the Q final
        ev_table$p_ll[i] <- min(1.0, n_slots / 1)
      } else {
        # Bernoulli convolution: P(sum of losses < n_slots) = P(sum < n_slots)
        # = 1 - P(sum >= n_slots)
        ev_table$p_ll[i] <- 1 - bernoulli_conv_ge(loss_probs, n_slots)
      }
    }
  }

  # Item 6: Drop observations without valid P_i^{LL}
  n_before <- nrow(ev_table)
  ev_table <- ev_table |> filter(!is.na(p_ll), p_ll > 0, p_ll < 1)
  n_after <- nrow(ev_table)
  message("  ", tour_label, " P_i^{LL} computed: ", n_after,
          " valid (dropped ", n_before - n_after, " without valid P_i^{LL})")

  ev_table
}

# Compute for each sample
message("  Computing P_i^{LL} for GS-ATP...")
gs_atp_ev <- compute_p_ll(gs_atp_ev, atp_qual, atp_win_model, "ATP")
message("  Computing P_i^{LL} for GS-WTA...")
gs_wta_ev <- compute_p_ll(gs_wta_ev, wta_qual, wta_win_model, "WTA")
message("  Computing P_i^{LL} for nonGS-ATP...")
nongs_atp_ev <- compute_p_ll(nongs_atp_ev, atp_qual, atp_win_model, "ATP")
message("  Computing P_i^{LL} for nonGS-WTA...")
nongs_wta_ev <- compute_p_ll(nongs_wta_ev, wta_qual, wta_win_model, "WTA")

# Re-assign event_ids after filtering
gs_atp_ev$event_id   <- seq_len(nrow(gs_atp_ev))
gs_wta_ev$event_id   <- seq_len(nrow(gs_wta_ev))
nongs_atp_ev$event_id <- seq_len(nrow(nongs_atp_ev))
nongs_wta_ev$event_id <- seq_len(nrow(nongs_wta_ev))

# Item 14 / user request 3: Compute generalized residual ANALYTICALLY
# v_hat_i = D_i * phi(Phi^{-1}(P_i^{LL})) / P_i^{LL}
#          - (1-D_i) * phi(Phi^{-1}(P_i^{LL})) / (1 - P_i^{LL})
compute_gen_residual <- function(ev) {
  q <- qnorm(ev$p_ll)
  phi_q <- dnorm(q)
  ev$v_hat <- ev$got_ll * phi_q / ev$p_ll -
    (1 - ev$got_ll) * phi_q / (1 - ev$p_ll)
  ev
}

gs_atp_ev   <- compute_gen_residual(gs_atp_ev)
gs_wta_ev   <- compute_gen_residual(gs_wta_ev)
nongs_atp_ev <- compute_gen_residual(nongs_atp_ev)
nongs_wta_ev <- compute_gen_residual(nongs_wta_ev)

slog("## Phase 3: Bernoulli Convolution P_i^{LL}")
slog("- GS-ATP: N=", nrow(gs_atp_ev), ", mean P_ll=", round(mean(gs_atp_ev$p_ll), 3))
slog("- GS-WTA: N=", nrow(gs_wta_ev), ", mean P_ll=", round(mean(gs_wta_ev$p_ll), 3))
slog("- nonGS-ATP: N=", nrow(nongs_atp_ev), ", mean P_ll=", round(mean(nongs_atp_ev$p_ll), 3))
slog("- nonGS-WTA: N=", nrow(nongs_wta_ev), ", mean P_ll=", round(mean(nongs_wta_ev$p_ll), 3))
slog("")


###############################################################################
# PHASE 4: MATCH-LEVEL DATASET CONSTRUCTION (Item 5)
###############################################################################

message("\n[PHASE 4] Building match-level analysis datasets...")

build_match_level <- function(ev_table, main_matches, tour_label,
                               calendar_cap_days = CALENDAR_CAP) {
  # main_matches should be the harmonized matches for this tour
  md <- main_matches |>
    filter(match_source == "main", tour == tour_label) |>
    mutate(
      tourney_date = if (inherits(tourney_date, "Date")) tourney_date
                     else as.Date(as.character(tourney_date), format = "%Y%m%d")
    )

  # Item 5: Truncation at next LL opportunity
  # Censor at player's next final-round qualifying loss at an LL-granting event
  ev_sorted <- ev_table |> arrange(player_id, tourney_date)
  ev_sorted <- ev_sorted |>
    group_by(player_id) |>
    mutate(next_ll_opp_date = lead(tourney_date, default = as.Date("2099-12-31"))) |>
    ungroup()

  ev_sorted$cal_cap_date <- ev_sorted$tourney_date + calendar_cap_days

  # Build focal matches: player-centric view of main draw matches
  fm <- bind_rows(
    md |> transmute(
      focal_pid = winner_pid, opp_pid = loser_pid,
      tourney_id, tourney_date, match_num, surface, tourney_level,
      best_of, focal_age = winner_age, opp_age = loser_age,
      focal_rank = winner_rank, opp_rank = loser_rank,
      focal_ioc = winner_ioc, opp_ioc = loser_ioc,
      focal_ht = winner_ht, opp_ht = loser_ht,
      focal_hand = winner_hand, opp_hand = loser_hand,
      round, won = 1L),
    md |> transmute(
      focal_pid = loser_pid, opp_pid = winner_pid,
      tourney_id, tourney_date, match_num, surface, tourney_level,
      best_of, focal_age = loser_age, opp_age = winner_age,
      focal_rank = loser_rank, opp_rank = winner_rank,
      focal_ioc = loser_ioc, opp_ioc = winner_ioc,
      focal_ht = loser_ht, opp_ht = winner_ht,
      focal_hand = loser_hand, opp_hand = winner_hand,
      round, won = 0L)
  )

  # Join events to future matches
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

  # Add match-level features for the second stage
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

  # Add opponent Elo at match time
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
    lookup_date = ev_date - 1  # pre-treatment H2H
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
                 "match_num", "surface", "tourney_level", "round",
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

  # Filter complete cases
  match_df <- match_df |>
    filter(!is.na(log_rank_ratio), !is.na(won), !is.na(got_ll), !is.na(v_hat))

  # Round number within each event-tournament
  match_df <- match_df |>
    arrange(event_id, tourney_id, match_num) |>
    group_by(event_id, tourney_id) |>
    mutate(round_num = row_number()) |>
    ungroup()

  message("  ", tour_label, " match-level observations: ", nrow(match_df),
          " across ", n_distinct(match_df$event_id), " events")
  match_df
}

message("  Building GS-ATP match data...")
gs_atp_md <- build_match_level(gs_atp_ev, matches, "ATP")
message("  Building GS-WTA match data...")
gs_wta_md <- build_match_level(gs_wta_ev, matches, "WTA")
message("  Building nonGS-ATP match data...")
nongs_atp_md <- build_match_level(nongs_atp_ev, matches, "ATP")
message("  Building nonGS-WTA match data...")
nongs_wta_md <- build_match_level(nongs_wta_ev, matches, "WTA")

slog("## Phase 4: Match-Level Datasets")
slog("- GS-ATP: ", nrow(gs_atp_md), " matches, ", n_distinct(gs_atp_md$event_id), " events")
slog("- GS-WTA: ", nrow(gs_wta_md), " matches, ", n_distinct(gs_wta_md$event_id), " events")
slog("- nonGS-ATP: ", nrow(nongs_atp_md), " matches, ", n_distinct(nongs_atp_md$event_id), " events")
slog("- nonGS-WTA: ", nrow(nongs_wta_md), " matches, ", n_distinct(nongs_wta_md$event_id), " events")
slog("")


###############################################################################
# PHASE 5: CF-IV ESTIMATION (Items 7, 8, 10, 14)
###############################################################################

message("\n[PHASE 5] Estimating CF-IV models...")

# Second-stage logit: won ~ LL_entry + v_hat + X_ijm + Z_ie^pre
estimate_model <- function(match_df, label, weighted = FALSE) {
  if (nrow(match_df) < 50) {
    message("  ", label, ": insufficient observations (", nrow(match_df), ")")
    return(NULL)
  }

  # Impute non-critical NAs
  df <- match_df |>
    mutate(
      h2h_smoothed = replace_na(h2h_smoothed, 0.5),
      n_h2h = replace_na(n_h2h, 0L),
      age_diff = replace_na(age_diff, 0),
      height_diff = replace_na(height_diff, 0),
      hand_mismatch = replace_na(hand_mismatch, 0L),
      opp_elo = replace_na(opp_elo, 1500),
      pre_elo = replace_na(pre_elo, 1500),
      pre_rank_pts = replace_na(pre_rank_pts, 0)
    )

  # Item 10: points-weighted logit
  if (weighted) {
    # Assign round points based on tournament level and round
    df$match_weight <- 1  # default
    # We use a simple heuristic: later rounds get more weight
    df <- df |>
      mutate(match_weight = case_when(
        grepl("^F$", round) ~ 10,
        grepl("^SF$", round) ~ 5,
        grepl("^QF$", round) ~ 3,
        grepl("^R16$", round) ~ 2,
        TRUE ~ 1
      ))
  }

  formula_str <- won ~ got_ll + v_hat +
    log_rank_ratio + log_rank_ratio_sq + rank_diff +
    same_ioc + is_clay + is_grass + age_diff + height_diff +
    hand_mismatch + h2h_smoothed + n_h2h +
    pre_elo + opp_elo + pre_rank_pts +
    player_age_at_event + had_prior_ll

  if (weighted) {
    model <- glm(formula_str, family = binomial(link = "logit"),
                 data = df, weights = match_weight)
  } else {
    model <- glm(formula_str, family = binomial(link = "logit"), data = df)
  }

  s <- summary(model)$coefficients
  delta <- s["got_ll", "Estimate"]
  delta_se <- s["got_ll", "Std. Error"]
  delta_p <- s["got_ll", "Pr(>|z|)"]
  rho <- s["v_hat", "Estimate"]
  rho_se <- s["v_hat", "Std. Error"]
  rho_p <- s["v_hat", "Pr(>|z|)"]

  message(sprintf("  %s: delta=%.4f (SE=%.4f, p=%.4f), rho=%.4f (p=%.4f), N=%d",
                  label, delta, delta_se, delta_p, rho, rho_p, nrow(df)))

  list(model = model, data = df, delta = delta, delta_se = delta_se,
       delta_p = delta_p, rho = rho, rho_se = rho_se, rho_p = rho_p)
}

# Main specifications
gs_atp_fit    <- estimate_model(gs_atp_md, "GS-ATP")
gs_wta_fit    <- estimate_model(gs_wta_md, "GS-WTA")
nongs_atp_fit <- estimate_model(nongs_atp_md, "nonGS-ATP")
nongs_wta_fit <- estimate_model(nongs_wta_md, "nonGS-WTA")

# Item 10: Points-weighted robustness
gs_atp_wt    <- estimate_model(gs_atp_md, "GS-ATP (weighted)", weighted = TRUE)
gs_wta_wt    <- estimate_model(gs_wta_md, "GS-WTA (weighted)", weighted = TRUE)
nongs_atp_wt <- estimate_model(nongs_atp_md, "nonGS-ATP (weighted)", weighted = TRUE)
nongs_wta_wt <- estimate_model(nongs_wta_md, "nonGS-WTA (weighted)", weighted = TRUE)

slog("## Phase 5: CF-IV Estimation")
for (fit_obj in list(gs_atp_fit, gs_wta_fit, nongs_atp_fit, nongs_wta_fit)) {
  if (!is.null(fit_obj)) {
    slog(sprintf("- delta=%.4f (SE=%.4f, p=%.4f), rho=%.4f (SE=%.4f, p=%.4f), N=%.0f",
                 fit_obj$delta, fit_obj$delta_se, fit_obj$delta_p,
                 fit_obj$rho, fit_obj$rho_se, fit_obj$rho_p,
                 nrow(fit_obj$data)))
  }
}
slog("")


###############################################################################
# PHASE 6: COUNTERFACTUAL QUANTITIES (Items 7, 9a, 9b)
###############################################################################

message("\n[PHASE 6] Computing counterfactual tournament quantities...")

compute_counterfactuals <- function(fit_obj, label, tour_tier = "G") {
  if (is.null(fit_obj)) return(NULL)

  model <- fit_obj$model
  df <- fit_obj$data
  beta <- coef(model)
  delta <- beta["got_ll"]
  rho <- beta["v_hat"]

  # Item 7: Correct counterfactual formula includes rho*v_hat
  linpred_obs <- predict(model, newdata = df, type = "link")
  linpred_d0 <- linpred_obs - delta * df$got_ll  # remove LL effect
  linpred_d1 <- linpred_d0 + delta                # add LL effect

  df$p0 <- plogis(linpred_d0)
  df$p1 <- plogis(linpred_d1)
  df$marginal_effect <- df$p1 - df$p0

  # 1. DeltaP(match win): average marginal effect
  avg_me <- mean(df$marginal_effect)

  # Tournament-level quantities
  df_dt <- as.data.table(df)
  setorder(df_dt, event_id, tourney_id, round_num)

  tourney_effects <- df_dt[, {
    R <- .N
    cp0 <- cumprod(p0)
    cp1 <- cumprod(p1)
    list(
      n_rounds = R,
      avg_match_me = mean(p1 - p0),
      ew_d0 = sum(cp0),          # E[W | d=0]
      ew_d1 = sum(cp1),          # E[W | d=1]
      delta_ew = sum(cp1) - sum(cp0),  # Item 9a: delta E[W]
      # Item 9a: "tournament performance probability" (probability of observed trajectory)
      perf_prob_d0 = cp0[R],
      perf_prob_d1 = cp1[R]
    )
  }, by = .(event_id, tourney_id)]

  # 2. DeltaE[W]: expected additional wins per tournament
  avg_delta_ew <- mean(tourney_effects$delta_ew)

  # Item 9b: Expected ranking points E[RP|d]
  # For each tournament, compute round-reaching probabilities and multiply by points
  compute_erp <- function(dt_sub, d_col, tier) {
    # Get points schedule
    sched <- POINTS_SCHEDULE[[tier]]
    if (is.null(sched)) sched <- POINTS_SCHEDULE[["A250"]]  # fallback
    pts_vec <- as.numeric(sched)
    n_rounds_sched <- length(pts_vec)

    R <- nrow(dt_sub)
    probs <- dt_sub[[d_col]]

    # Round-reaching probabilities
    # P(reach round r) = prod_{s=1}^{r-1} p_s
    # E[RP] = sum_r P(reach round r) * points(round r)
    # For r=1, P(reach R1) = 1 (they're in the draw)
    # For r=2, P(reach R2) = p1
    # etc.

    # But: the player may play fewer rounds than the schedule.
    # Use min of actual rounds and schedule rounds.
    n_use <- min(R, n_rounds_sched)
    if (n_use == 0) return(0)

    # Round 1 participation points (always earned)
    erp <- pts_vec[1]

    # Rounds 2+: conditional on winning all previous
    if (n_use > 1) {
      for (r in 2:n_use) {
        p_reach_r <- prod(probs[1:(r-1)])
        erp <- erp + p_reach_r * pts_vec[min(r, n_rounds_sched)]
      }
    }
    erp
  }

  # Determine tier for points schedule
  get_tier <- function(tourney_level, draw_size = NA) {
    if (tourney_level == "G") return("G")
    if (tourney_level %in% c("M", "PM")) return("M")
    if (tourney_level == "P") return("P")
    if (tourney_level == "I") return("I")
    if (tourney_level == "A") {
      # Distinguish 250 vs 500 by draw size (rough heuristic)
      if (!is.na(draw_size) && draw_size >= 48) return("A500")
      return("A250")
    }
    "A250"
  }

  # Compute E[RP|d] for each event-tournament
  tourney_effects$erp_d0 <- NA_real_
  tourney_effects$erp_d1 <- NA_real_

  for (k in seq_len(nrow(tourney_effects))) {
    eid <- tourney_effects$event_id[k]
    tid <- tourney_effects$tourney_id[k]
    sub <- df_dt[event_id == eid & tourney_id == tid]
    setorder(sub, round_num)

    tier <- get_tier(sub$tourney_level[1])
    tourney_effects$erp_d0[k] <- compute_erp(sub, "p0", tier)
    tourney_effects$erp_d1[k] <- compute_erp(sub, "p1", tier)
  }
  tourney_effects$delta_erp <- tourney_effects$erp_d1 - tourney_effects$erp_d0

  # 3. DeltaE[RP]: expected additional ranking points per tournament
  avg_delta_erp <- mean(tourney_effects$delta_erp)

  message(sprintf("  %s: DeltaP(match)=%.4f, DeltaE[W]=%.4f, DeltaE[RP]=%.1f",
                  label, avg_me, avg_delta_ew, avg_delta_erp))

  list(
    tourney_effects = as.data.frame(tourney_effects),
    match_cf = df,
    avg_me = avg_me,
    avg_delta_ew = avg_delta_ew,
    avg_delta_erp = avg_delta_erp
  )
}

gs_atp_cf    <- compute_counterfactuals(gs_atp_fit, "GS-ATP", "G")
gs_wta_cf    <- compute_counterfactuals(gs_wta_fit, "GS-WTA", "G")
nongs_atp_cf <- compute_counterfactuals(nongs_atp_fit, "nonGS-ATP", "M")
nongs_wta_cf <- compute_counterfactuals(nongs_wta_fit, "nonGS-WTA", "P")

slog("## Phase 6: Counterfactual Quantities")
for (cf in list(gs_atp_cf, gs_wta_cf, nongs_atp_cf, nongs_wta_cf)) {
  if (!is.null(cf)) {
    slog(sprintf("- DeltaP(match)=%.4f, DeltaE[W]=%.4f, DeltaE[RP]=%.1f",
                 cf$avg_me, cf$avg_delta_ew, cf$avg_delta_erp))
  }
}
slog("")


###############################################################################
# PHASE 7: BOOTSTRAP INFERENCE (Item 8)
###############################################################################

message("\n[PHASE 7] Bootstrap inference (", N_BOOT, " replications)...")

bootstrap_cf_iv <- function(ev_table, match_df, n_boot = N_BOOT, label = "") {
  if (is.null(match_df) || nrow(match_df) < 50) {
    message("  ", label, ": skipping bootstrap (insufficient data)")
    return(NULL)
  }

  # Identify unique players for block bootstrap
  players <- unique(ev_table$player_id)
  n_players <- length(players)

  # Pre-split data by player for fast subsetting
  ev_by_player <- split(ev_table, ev_table$player_id)
  md_by_player <- split(match_df, match_df$event_id)

  # Map event_id to player_id
  ev_player_map <- ev_table |> select(event_id, player_id) |> distinct()

  t_start <- Sys.time()
  boot_results <- vector("list", n_boot)

  for (b in seq_len(n_boot)) {
    if (b %% 50 == 0) {
      elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
      rate <- b / elapsed
      eta <- (n_boot - b) / rate
      message(sprintf("    %s: Rep %d/%d (%.1f min elapsed, ~%.1f min remain)",
                      label, b, n_boot, elapsed, eta))
    }

    tryCatch({
      boot_players <- sample(players, n_players, replace = TRUE)

      # Reconstruct boot event table
      boot_ev_list <- lapply(boot_players, function(p) ev_by_player[[as.character(p)]])
      boot_ev <- bind_rows(boot_ev_list)
      if (nrow(boot_ev) < 20) next

      # Reconstruct boot match data
      boot_md <- match_df |> filter(event_id %in% boot_ev$event_id)
      if (nrow(boot_md) < 50) next

      # Recompute generalized residuals for bootstrap sample
      q <- qnorm(boot_ev$p_ll)
      phi_q <- dnorm(q)
      boot_ev$v_hat <- boot_ev$got_ll * phi_q / boot_ev$p_ll -
        (1 - boot_ev$got_ll) * phi_q / (1 - boot_ev$p_ll)

      # Update v_hat in match data
      vhat_lookup <- boot_ev |> select(event_id, v_hat_boot = v_hat)
      boot_md <- boot_md |>
        left_join(vhat_lookup, by = "event_id") |>
        mutate(v_hat = coalesce(v_hat_boot, v_hat)) |>
        select(-v_hat_boot)

      # Impute NAs
      boot_md <- boot_md |>
        mutate(
          h2h_smoothed = replace_na(h2h_smoothed, 0.5),
          n_h2h = replace_na(n_h2h, 0L),
          age_diff = replace_na(age_diff, 0),
          height_diff = replace_na(height_diff, 0),
          hand_mismatch = replace_na(hand_mismatch, 0L),
          opp_elo = replace_na(opp_elo, 1500),
          pre_elo = replace_na(pre_elo, 1500),
          pre_rank_pts = replace_na(pre_rank_pts, 0)
        )

      # Fit second stage
      fit <- glm(won ~ got_ll + v_hat +
                   log_rank_ratio + log_rank_ratio_sq + rank_diff +
                   same_ioc + is_clay + is_grass + age_diff + height_diff +
                   hand_mismatch + h2h_smoothed + n_h2h +
                   pre_elo + opp_elo + pre_rank_pts +
                   player_age_at_event + had_prior_ll,
                 family = binomial(link = "logit"), data = boot_md)

      if (!fit$converged) next

      beta <- coef(fit)
      delta_b <- unname(beta["got_ll"])
      rho_b <- unname(beta["v_hat"])

      # Counterfactual quantities
      lp_obs <- predict(fit, newdata = boot_md, type = "link")
      lp_d0 <- lp_obs - delta_b * boot_md$got_ll
      lp_d1 <- lp_d0 + delta_b
      p0 <- plogis(lp_d0)
      p1 <- plogis(lp_d1)

      avg_me_b <- mean(p1 - p0)

      # Tournament-level E[W]
      boot_md$p0 <- p0
      boot_md$p1 <- p1
      te <- boot_md |>
        arrange(event_id, tourney_id, round_num) |>
        group_by(event_id, tourney_id) |>
        summarise(
          delta_ew = sum(cumprod(p1)) - sum(cumprod(p0)),
          .groups = "drop"
        )
      avg_dew_b <- mean(te$delta_ew)

      boot_results[[b]] <- c(delta = delta_b, rho = rho_b,
                              avg_me = avg_me_b, avg_dew = avg_dew_b)

    }, error = function(e) NULL)
  }

  # Collect results
  boot_results <- boot_results[!sapply(boot_results, is.null)]
  n_success <- length(boot_results)

  elapsed_total <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
  message(sprintf("  %s: %d/%d successful in %.1f min",
                  label, n_success, n_boot, elapsed_total))

  if (n_success < 10) {
    message("  ", label, ": too few bootstrap replications")
    return(NULL)
  }

  boot_mat <- do.call(rbind, boot_results)
  boot_se <- apply(boot_mat, 2, sd, na.rm = TRUE)
  boot_ci <- apply(boot_mat, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)

  message(sprintf("  %s: delta_SE_boot=%.4f, ME_CI=[%.4f, %.4f]",
                  label, boot_se["delta"],
                  boot_ci["2.5%", "avg_me"], boot_ci["97.5%", "avg_me"]))

  list(boot_mat = boot_mat, boot_se = boot_se, boot_ci = boot_ci,
       n_success = n_success)
}

gs_atp_boot   <- bootstrap_cf_iv(gs_atp_ev, gs_atp_md, N_BOOT, "GS-ATP")
gs_wta_boot   <- bootstrap_cf_iv(gs_wta_ev, gs_wta_md, N_BOOT, "GS-WTA")
nongs_atp_boot <- bootstrap_cf_iv(nongs_atp_ev, nongs_atp_md, N_BOOT, "nonGS-ATP")
nongs_wta_boot <- bootstrap_cf_iv(nongs_wta_ev, nongs_wta_md, N_BOOT, "nonGS-WTA")

slog("## Phase 7: Bootstrap Inference")
for (blist in list(
  list(b = gs_atp_boot, l = "GS-ATP"),
  list(b = gs_wta_boot, l = "GS-WTA"),
  list(b = nongs_atp_boot, l = "nonGS-ATP"),
  list(b = nongs_wta_boot, l = "nonGS-WTA")
)) {
  if (!is.null(blist$b)) {
    slog(sprintf("- %s: delta_SE=%.4f, success=%d/%d",
                 blist$l, blist$b$boot_se["delta"], blist$b$n_success, N_BOOT))
  }
}
slog("")


###############################################################################
# PHASE 8: TABLES (Item 18)
###############################################################################

message("\n[PHASE 8] Generating tables...")

write_results_table <- function(fit_obj, fit_wt, cf_obj, boot_obj,
                                 ev_table, filepath, label) {
  if (is.null(fit_obj)) {
    message("  ", label, ": no fit object, skipping table")
    return()
  }

  delta <- fit_obj$delta
  delta_se <- if (!is.null(boot_obj)) boot_obj$boot_se["delta"] else fit_obj$delta_se
  delta_p <- if (!is.null(boot_obj)) {
    z <- delta / delta_se
    2 * pnorm(-abs(z))
  } else fit_obj$delta_p

  rho <- fit_obj$rho
  rho_se <- if (!is.null(boot_obj)) boot_obj$boot_se["rho"] else fit_obj$rho_se
  rho_p <- if (!is.null(boot_obj)) {
    z <- rho / rho_se
    2 * pnorm(-abs(z))
  } else fit_obj$rho_p

  # Counterfactual quantities
  avg_me <- if (!is.null(cf_obj)) cf_obj$avg_me else NA
  avg_dew <- if (!is.null(cf_obj)) cf_obj$avg_delta_ew else NA
  avg_derp <- if (!is.null(cf_obj)) cf_obj$avg_delta_erp else NA

  # Bootstrap CIs for counterfactual quantities
  me_ci <- if (!is.null(boot_obj)) {
    sprintf("[%.4f, %.4f]",
            boot_obj$boot_ci["2.5%", "avg_me"],
            boot_obj$boot_ci["97.5%", "avg_me"])
  } else "---"

  dew_ci <- if (!is.null(boot_obj)) {
    sprintf("[%.4f, %.4f]",
            boot_obj$boot_ci["2.5%", "avg_dew"],
            boot_obj$boot_ci["97.5%", "avg_dew"])
  } else "---"

  # Weighted delta
  delta_wt <- if (!is.null(fit_wt)) fit_wt$delta else NA
  delta_wt_se <- if (!is.null(fit_wt)) fit_wt$delta_se else NA

  n_matches <- nrow(fit_obj$data)
  n_events <- n_distinct(fit_obj$data$event_id)
  n_players <- n_distinct(ev_table$player_id)

  lines <- c(
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & Estimate & SE \\\\",
    "\\midrule",
    "\\multicolumn{3}{l}{\\textit{Panel A: Second-Stage Logit}} \\\\[3pt]",
    sprintf("$\\hat{\\delta}$ (LL entry) & %.4f%s & (%.4f) \\\\",
            delta, add_stars(delta_p), delta_se),
    sprintf("$\\hat{\\rho}$ (endogeneity) & %.4f%s & (%.4f) \\\\",
            rho, add_stars(rho_p), rho_se),
    "\\midrule",
    "\\multicolumn{3}{l}{\\textit{Panel B: Counterfactual Quantities}} \\\\[3pt]"
  )

  if (!is.na(avg_me)) {
    lines <- c(lines,
      sprintf("$\\Delta P(\\text{match win})$ & %.4f & %s \\\\", avg_me, me_ci))
  }
  if (!is.na(avg_dew)) {
    lines <- c(lines,
      sprintf("$\\Delta E[W]$ (wins/tournament) & %.4f & %s \\\\", avg_dew, dew_ci))
  }
  if (!is.na(avg_derp)) {
    lines <- c(lines,
      sprintf("$\\Delta E[RP]$ (ranking points) & %.1f & \\\\", avg_derp))
  }

  # Item 10: points-weighted robustness
  if (!is.na(delta_wt)) {
    lines <- c(lines,
      "\\midrule",
      "\\multicolumn{3}{l}{\\textit{Panel C: Robustness}} \\\\[3pt]",
      sprintf("$\\hat{\\delta}$ (points-weighted) & %.4f%s & (%.4f) \\\\",
              delta_wt, add_stars(if (!is.null(fit_wt)) fit_wt$delta_p else NA), delta_wt_se))
  }

  lines <- c(lines,
    "\\midrule",
    sprintf("$N$ matches & \\multicolumn{2}{c}{%s} \\\\",
            format(n_matches, big.mark = ",")),
    sprintf("$N$ player-episodes & \\multicolumn{2}{c}{%s} \\\\",
            format(n_events, big.mark = ",")),
    sprintf("$N$ unique players & \\multicolumn{2}{c}{%s} \\\\",
            format(n_players, big.mark = ",")),
    "\\bottomrule",
    "\\end{tabular}"
  )

  writeLines(lines, filepath)
  message("  Saved: ", filepath)
}

write_results_table(gs_atp_fit, gs_atp_wt, gs_atp_cf, gs_atp_boot,
                     gs_atp_ev, file.path(TABLE_DIR, "table_tournament_gs_atp.tex"), "GS-ATP")
write_results_table(gs_wta_fit, gs_wta_wt, gs_wta_cf, gs_wta_boot,
                     gs_wta_ev, file.path(TABLE_DIR, "table_tournament_gs_wta.tex"), "GS-WTA")
write_results_table(nongs_atp_fit, nongs_atp_wt, nongs_atp_cf, nongs_atp_boot,
                     nongs_atp_ev, file.path(TABLE_DIR, "table_tournament_nongs_atp.tex"), "nonGS-ATP")
write_results_table(nongs_wta_fit, nongs_wta_wt, nongs_wta_cf, nongs_wta_boot,
                     nongs_wta_ev, file.path(TABLE_DIR, "table_tournament_nongs_wta.tex"), "nonGS-WTA")


###############################################################################
# PHASE 9: FIGURES (Items 12, 13)
###############################################################################

message("\n[PHASE 9] Generating four-panel calendar-horizon figure...")

# Item 13: Do NOT regenerate fig_dynamic_effects.pdf
# Item 12: Four-panel figure using CALENDAR horizons (4w, 8w, 12w, 26w, 52w)

make_calendar_horizon_figure <- function(fit_obj, cf_obj, match_df, boot_obj, label) {
  if (is.null(fit_obj) || is.null(cf_obj)) return(NULL)

  horizons_weeks <- c(4, 8, 12, 26, 52)
  df <- cf_obj$match_cf

  # Assign calendar horizon bins to each match
  df$cal_horizon <- NA_character_
  df$cal_weeks <- df$weeks_after_event
  for (h in rev(horizons_weeks)) {
    df$cal_horizon[df$cal_weeks <= h] <- paste0(h, "w")
  }
  df <- df |> filter(!is.na(cal_horizon))
  df$cal_horizon <- factor(df$cal_horizon, levels = paste0(horizons_weeks, "w"))

  # Panel (a): DeltaP(match win) by horizon
  panel_a_data <- df |>
    group_by(cal_horizon) |>
    summarise(
      avg_me = mean(p1 - p0, na.rm = TRUE),
      se_me = sd(p1 - p0, na.rm = TRUE) / sqrt(n()),
      n = n(),
      .groups = "drop"
    ) |>
    mutate(lower = avg_me - 1.96 * se_me, upper = avg_me + 1.96 * se_me)

  p_a <- ggplot(panel_a_data, aes(x = cal_horizon, y = avg_me)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = col_treat) +
    geom_point(color = col_treat, size = 3) +
    labs(x = "Calendar Horizon", y = expression(Delta*"P(match win)")) +
    theme_paper()

  # For panels b-d, compute tournament-level quantities by calendar horizon
  te <- cf_obj$tourney_effects
  # Merge calendar info: use median days_after_event per event-tourney
  match_tourney_horizon <- df |>
    group_by(event_id, tourney_id) |>
    summarise(median_weeks = median(cal_weeks, na.rm = TRUE), .groups = "drop")
  te <- te |> left_join(match_tourney_horizon, by = c("event_id", "tourney_id"))
  te$cal_horizon <- NA_character_
  for (h in rev(horizons_weeks)) {
    te$cal_horizon[te$median_weeks <= h] <- paste0(h, "w")
  }
  te <- te |> filter(!is.na(cal_horizon))
  te$cal_horizon <- factor(te$cal_horizon, levels = paste0(horizons_weeks, "w"))

  # Panel (b): DeltaE[RP] by horizon
  panel_b_data <- te |>
    group_by(cal_horizon) |>
    summarise(avg_derp = mean(delta_erp, na.rm = TRUE),
              se_derp = sd(delta_erp, na.rm = TRUE) / sqrt(n()),
              n = n(), .groups = "drop") |>
    mutate(lower = avg_derp - 1.96 * se_derp, upper = avg_derp + 1.96 * se_derp)

  p_b <- ggplot(panel_b_data, aes(x = cal_horizon, y = avg_derp)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = col_treat) +
    geom_point(color = col_treat, size = 3) +
    labs(x = "Calendar Horizon", y = expression(Delta*"E[RP]")) +
    theme_paper()

  # Panel (c): Cumulative DeltaE[RP] through each horizon
  panel_c_data <- data.frame(cal_horizon = paste0(horizons_weeks, "w"),
                              cum_derp = NA_real_)
  for (idx in seq_along(horizons_weeks)) {
    h <- horizons_weeks[idx]
    sub <- te |> filter(as.numeric(gsub("w", "", as.character(cal_horizon))) <= h)
    panel_c_data$cum_derp[idx] <- sum(sub$delta_erp, na.rm = TRUE) / n_distinct(sub$event_id)
  }
  panel_c_data$cal_horizon <- factor(panel_c_data$cal_horizon,
                                      levels = paste0(horizons_weeks, "w"))

  p_c <- ggplot(panel_c_data, aes(x = cal_horizon, y = cum_derp, group = 1)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_line(color = col_treat, linewidth = 0.8) +
    geom_point(color = col_treat, size = 3) +
    labs(x = "Calendar Horizon", y = expression("Cumulative "*Delta*"E[RP]")) +
    theme_paper()

  # Panel (d): Cumulative DeltaE[W] through each horizon
  panel_d_data <- data.frame(cal_horizon = paste0(horizons_weeks, "w"),
                              cum_dew = NA_real_)
  for (idx in seq_along(horizons_weeks)) {
    h <- horizons_weeks[idx]
    sub <- te |> filter(as.numeric(gsub("w", "", as.character(cal_horizon))) <= h)
    panel_d_data$cum_dew[idx] <- sum(sub$delta_ew, na.rm = TRUE) / n_distinct(sub$event_id)
  }
  panel_d_data$cal_horizon <- factor(panel_d_data$cal_horizon,
                                      levels = paste0(horizons_weeks, "w"))

  p_d <- ggplot(panel_d_data, aes(x = cal_horizon, y = cum_dew, group = 1)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_line(color = col_treat, linewidth = 0.8) +
    geom_point(color = col_treat, size = 3) +
    labs(x = "Calendar Horizon", y = expression("Cumulative "*Delta*"E[W]")) +
    theme_paper()

  # Combine
  combined <- (p_a + p_b) / (p_c + p_d) +
    plot_annotation(tag_levels = list(c("(a)", "(b)", "(c)", "(d)"))) &
    theme(plot.tag = element_text(family = "serif", size = 12))

  combined
}

# Use the largest sample (GS-ATP or nonGS-ATP) for the main figure
# Try GS-ATP first, fall back to others
fig_obj <- NULL
for (triple in list(
  list(f = gs_atp_fit, c = gs_atp_cf, m = gs_atp_md, b = gs_atp_boot, l = "GS-ATP"),
  list(f = nongs_atp_fit, c = nongs_atp_cf, m = nongs_atp_md, b = nongs_atp_boot, l = "nonGS-ATP"),
  list(f = gs_wta_fit, c = gs_wta_cf, m = gs_wta_md, b = gs_wta_boot, l = "GS-WTA")
)) {
  fig_obj <- make_calendar_horizon_figure(triple$f, triple$c, triple$m, triple$b, triple$l)
  if (!is.null(fig_obj)) {
    message("  Using ", triple$l, " for four-panel figure")
    break
  }
}

if (!is.null(fig_obj)) {
  ggsave(file.path(FIG_DIR, "fig_tournament_4panel.pdf"), fig_obj,
         width = 10, height = 8, device = cairo_pdf)
  message("  Saved: fig_tournament_4panel.pdf")
} else {
  message("  WARNING: Could not generate four-panel figure (insufficient data)")
}


###############################################################################
# PHASE 10: SAVE RESULTS AND SUMMARY
###############################################################################

message("\n[PHASE 10] Saving results...")

all_results <- list(
  gs_atp = list(events = gs_atp_ev, matches = gs_atp_md, fit = gs_atp_fit,
                fit_wt = gs_atp_wt, cf = gs_atp_cf, boot = gs_atp_boot),
  gs_wta = list(events = gs_wta_ev, matches = gs_wta_md, fit = gs_wta_fit,
                fit_wt = gs_wta_wt, cf = gs_wta_cf, boot = gs_wta_boot),
  nongs_atp = list(events = nongs_atp_ev, matches = nongs_atp_md, fit = nongs_atp_fit,
                    fit_wt = nongs_atp_wt, cf = nongs_atp_cf, boot = nongs_atp_boot),
  nongs_wta = list(events = nongs_wta_ev, matches = nongs_wta_md, fit = nongs_wta_fit,
                    fit_wt = nongs_wta_wt, cf = nongs_wta_cf, boot = nongs_wta_boot),
  win_models = list(atp = atp_win_model, wta = wta_win_model)
)

saveRDS(all_results, file.path(CLEANED_DIR, "tournament_rebuild_results.rds"))
message("  Saved: Data/cleaned/tournament_rebuild_results.rds")

# Write summary
summary_text <- c(
  "# Tournament Performance Model Rebuild Summary",
  paste0("Generated: ", Sys.time()),
  "",
  "## Audit Items Addressed",
  "- Item 1: Final-round qualifying losers only (max Q round per tournament)",
  "- Item 2: LL-granting events only (n_ll_slots > 0)",
  "- Item 3-4: Four separate models (GS-ATP, GS-WTA, nonGS-ATP, nonGS-WTA)",
  "- Item 5: Truncation at next LL opportunity (not any qualifying loss)",
  "- Item 6: Bernoulli convolution P_i^{LL} only (no crude fallback)",
  "- Item 7: Counterfactual formula includes rho*v_hat term",
  "- Item 8: 200-rep player-level block bootstrap",
  "- Item 9a: Renamed to tournament performance probability",
  "- Item 9b: Expected ranking points E[RP|d] computed",
  "- Item 10: Points-weighted logit robustness",
  "- Item 11: H2H encounter count (n_h2h) in win model",
  "- Item 12: Four-panel calendar-horizon figure (fig_tournament_4panel.pdf)",
  "- Item 13: fig_dynamic_effects.pdf NOT regenerated",
  "- Item 14: Enhanced win model with Z_ie^pre; P_i^{LL} imposed directly",
  "- Item 15: Estimation sample = LL candidates only (documented)",
  "- Item 16: GS 2006-2024, nonGS-ATP 2007+, nonGS-WTA 2009+",
  "- Item 17: No first-stage probit (generalized residual computed analytically)",
  "- Item 18: Four tables generated",
  "",
  "## Sample Sizes",
  "",
  summary_log,
  "",
  "## Key Results",
  ""
)

# Add key results
for (triple in list(
  list(f = gs_atp_fit, c = gs_atp_cf, b = gs_atp_boot, l = "GS-ATP"),
  list(f = gs_wta_fit, c = gs_wta_cf, b = gs_wta_boot, l = "GS-WTA"),
  list(f = nongs_atp_fit, c = nongs_atp_cf, b = nongs_atp_boot, l = "nonGS-ATP"),
  list(f = nongs_wta_fit, c = nongs_wta_cf, b = nongs_wta_boot, l = "nonGS-WTA")
)) {
  if (!is.null(triple$f)) {
    delta_se_final <- if (!is.null(triple$b)) triple$b$boot_se["delta"] else triple$f$delta_se
    summary_text <- c(summary_text,
      sprintf("### %s", triple$l),
      sprintf("- delta (LL entry): %.4f (SE=%.4f)", triple$f$delta, delta_se_final),
      sprintf("- rho (endogeneity): %.4f (SE=%.4f)", triple$f$rho, triple$f$rho_se),
      sprintf("- N matches: %d, N events: %d", nrow(triple$f$data), n_distinct(triple$f$data$event_id))
    )
    if (!is.null(triple$c)) {
      summary_text <- c(summary_text,
        sprintf("- DeltaP(match win): %.4f", triple$c$avg_me),
        sprintf("- DeltaE[W]: %.4f", triple$c$avg_delta_ew),
        sprintf("- DeltaE[RP]: %.1f", triple$c$avg_delta_erp)
      )
    }
    summary_text <- c(summary_text, "")
  }
}

writeLines(summary_text, file.path(OUTPUT_DIR, "tournament_rebuild_summary.md"))
message("  Saved: Output/tournament_rebuild_summary.md")

message("\n", strrep("=", 72))
message("  TOURNAMENT REBUILD COMPLETE")
message(strrep("=", 72))
