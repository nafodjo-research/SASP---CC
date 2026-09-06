# ==============================================================================
# 25_tournament_robustness.R
# Robustness comparison table: 4 specifications x 4 samples
#
# Purpose:  For each of the 4 samples (GS-ATP, GS-WTA, nonGS-ATP, nonGS-WTA),
#           estimate 4 specifications varying:
#             (a) Window stop rule: "any LL" vs "same-type LL"
#             (b) Weighting: unweighted vs points-weighted
#           Produce a compact robustness table per sample.
#
# Inputs:
#   Data/cleaned/tournament_match_fix_results.rds (from script 24)
#   Data/cleaned/tournament_rebuild_results.rds   (from script 22, for full event table)
#   Data/raw/atp_main_matches.rds, atp_qual_chall_matches.rds
#   Data/raw/wta_main_matches.rds, wta_qual_itf_matches.rds
#   Data/cleaned/tournament_elo_cache.rds
#
# Outputs:
#   Tables/table_tournament_robustness_gs_atp.tex
#   Tables/table_tournament_robustness_gs_wta.tex
#   Tables/table_tournament_robustness_nongs_atp.tex
#   Tables/table_tournament_robustness_nongs_wta.tex
#   Output/tournament_robustness_summary.md
#
# Dependencies: dplyr, tidyr, readr, stringr, data.table, sandwich, here
# ==============================================================================

set.seed(20260326)

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(data.table)
library(sandwich)
library(here)

source(here("scripts", "R", "utils.R"))

# -- Project paths -------------------------------------------------------------
RAW_DIR     <- here("Data", "raw")
CLEANED_DIR <- here("Data", "cleaned")
TABLE_DIR   <- here("Tables")
OUTPUT_DIR  <- here("Output")

for (d in c(CLEANED_DIR, TABLE_DIR, OUTPUT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# -- Analysis parameters -------------------------------------------------------
CALENDAR_CAP <- 365L
N_BOOT       <- 200L

# -- Points schedules (ranking points by round, per tournament tier) -----------
# These are the approximate ATP/WTA points for the WINNER of each round.
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

# -- Initialize summary log ---------------------------------------------------
summary_log <- character()

cat("\n")
message(strrep("=", 72))
message("  TOURNAMENT ROBUSTNESS (25_tournament_robustness.R)")
message(strrep("=", 72))


###############################################################################
# PHASE 1: LOAD DATA
###############################################################################

message("\n[1] Loading data...")

R24 <- readRDS(file.path(CLEANED_DIR, "tournament_match_fix_results.rds"))
R22 <- readRDS(file.path(CLEANED_DIR, "tournament_rebuild_results.rds"))

# Extract event tables (from script 24's corrected data)
gs_atp_ev    <- R24$gs_atp$events
gs_wta_ev    <- R24$gs_wta$events
nongs_atp_ev <- R24$nongs_atp$events
nongs_wta_ev <- R24$nongs_wta$events

# "Same-type stop" match data is already in R24 (the default from script 24)
gs_atp_md_same    <- R24$gs_atp$matches
gs_wta_md_same    <- R24$gs_wta$matches
nongs_atp_md_same <- R24$nongs_atp$matches
nongs_wta_md_same <- R24$nongs_wta$matches

slog("## Phase 1: Data Loaded")
slog("- GS-ATP events: ", nrow(gs_atp_ev), " | same-type matches: ", nrow(gs_atp_md_same))
slog("- GS-WTA events: ", nrow(gs_wta_ev), " | same-type matches: ", nrow(gs_wta_md_same))
slog("- nonGS-ATP events: ", nrow(nongs_atp_ev), " | same-type matches: ", nrow(nongs_atp_md_same))
slog("- nonGS-WTA events: ", nrow(nongs_wta_ev), " | same-type matches: ", nrow(nongs_wta_md_same))
slog("")


###############################################################################
# PHASE 2: BUILD "ANY LL STOP" MATCH DATA
###############################################################################

message("\n[2] Building match data with 'any LL stop' truncation...")

# For "any LL stop", we need to find each player's next LL opportunity across
# ALL event types (GS + nonGS) for their tour, not just the same type.
# The current script 24 data uses same-type stop (next_ll_opp computed within
# each split sample). We need to rebuild with cross-type truncation.

# Load raw match data (same as script 24)
atp_main <- read_rds(file.path(RAW_DIR, "atp_main_matches.rds"))
atp_qual <- read_rds(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
wta_main <- read_rds(file.path(RAW_DIR, "wta_main_matches.rds"))
wta_qual <- read_rds(file.path(RAW_DIR, "wta_qual_itf_matches.rds"))

# Load Elo cache
elo_list <- readRDS(here("Data", "cleaned", "tournament_elo_cache.rds"))
message("  Loaded Elo cache: ", length(ls(elo_list$overall)), " players")

get_elo_at_date <- function(env, player, date) {
  df <- env[[player]]
  if (is.null(df)) return(1500)
  v <- df[df$date <= date, ]
  if (nrow(v) == 0) return(1500)
  tail(v$rating, 1)
}

# Harmonize matches (same logic as script 24)
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
message("  Combined matches (all sources): ", nrow(matches))

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

# Build ALL events (GS + nonGS) for each tour, to compute cross-type truncation
# We already have the 4 split event tables. Combine them per tour.
atp_all_events <- bind_rows(
  gs_atp_ev |> mutate(event_type = "GS"),
  nongs_atp_ev |> mutate(event_type = "nonGS")
) |> arrange(player_id, tourney_date)

wta_all_events <- bind_rows(
  gs_wta_ev |> mutate(event_type = "GS"),
  nongs_wta_ev |> mutate(event_type = "nonGS")
) |> arrange(player_id, tourney_date)

# Compute "any LL" next_ll_opp_date: next qualifying loss at ANY LL-granting event
compute_any_ll_next <- function(all_events) {
  all_events |>
    arrange(player_id, tourney_date) |>
    group_by(player_id) |>
    mutate(next_ll_opp_date_any = lead(tourney_date, default = as.Date("2099-12-31"))) |>
    ungroup()
}

atp_all_events <- compute_any_ll_next(atp_all_events)
wta_all_events <- compute_any_ll_next(wta_all_events)

# Map back to the split event tables: add cross-type next_ll_opp_date
# We need a unique key to join: player_id + tourney_date + tourney_id
add_any_ll_date <- function(ev_split, all_ev) {
  lookup <- all_ev |>
    select(player_id, tourney_id, tourney_date, next_ll_opp_date_any)
  ev_split |>
    left_join(lookup, by = c("player_id", "tourney_id", "tourney_date"))
}

gs_atp_ev_any    <- add_any_ll_date(gs_atp_ev, atp_all_events)
gs_wta_ev_any    <- add_any_ll_date(gs_wta_ev, wta_all_events)
nongs_atp_ev_any <- add_any_ll_date(nongs_atp_ev, atp_all_events)
nongs_wta_ev_any <- add_any_ll_date(nongs_wta_ev, wta_all_events)

# Verify
message("  Any-LL dates added: ",
        "GS-ATP=", sum(!is.na(gs_atp_ev_any$next_ll_opp_date_any)), "/", nrow(gs_atp_ev_any),
        ", nonGS-ATP=", sum(!is.na(nongs_atp_ev_any$next_ll_opp_date_any)), "/", nrow(nongs_atp_ev_any))

# Now rebuild match-level data using "any LL stop" truncation
# This is the build_match_level_fixed function from script 24 but with
# the next_ll_opp_date parameter taken from the event table column.
build_match_level_custom <- function(ev_table, all_matches, tour_label,
                                     next_ll_col = "next_ll_opp_date_any",
                                     calendar_cap_days = CALENDAR_CAP) {
  md <- all_matches |>
    filter(tour == tour_label) |>
    mutate(
      tourney_date = if (inherits(tourney_date, "Date")) tourney_date
                     else as.Date(as.character(tourney_date), format = "%Y%m%d")
    )

  # Use provided next_ll column, or compute within-sample if not present
  ev_sorted <- ev_table |> arrange(player_id, tourney_date)
  if (next_ll_col %in% names(ev_sorted)) {
    # Use the pre-computed column
    ev_sorted$next_ll_opp_date <- ev_sorted[[next_ll_col]]
  } else {
    # Compute within-sample (same-type stop)
    ev_sorted <- ev_sorted |>
      group_by(player_id) |>
      mutate(next_ll_opp_date = lead(tourney_date, default = as.Date("2099-12-31"))) |>
      ungroup()
  }
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
      round, won = 1L, match_source, draw_size),
    md |> transmute(
      focal_pid = loser_pid, opp_pid = winner_pid,
      tourney_id, tourney_date, match_num, surface, tourney_level,
      best_of, focal_age = loser_age, opp_age = winner_age,
      focal_rank = loser_rank, opp_rank = winner_rank,
      focal_ioc = loser_ioc, opp_ioc = winner_ioc,
      focal_ht = loser_ht, opp_ht = winner_ht,
      focal_hand = loser_hand, opp_hand = winner_hand,
      round, won = 0L, match_source, draw_size)
  )

  results <- list()
  for (i in seq_len(nrow(ev_sorted))) {
    e <- ev_sorted[i, ]
    pid <- e$player_pid
    ev_date <- e$tourney_date
    censor_date <- min(e$next_ll_opp_date, e$cal_cap_date, na.rm = TRUE)

    player_matches <- fm |>
      filter(focal_pid == pid,
             tourney_date > ev_date,
             tourney_date < censor_date)

    if (nrow(player_matches) == 0) next

    player_matches$event_id            <- e$event_id
    player_matches$got_ll              <- e$got_ll
    player_matches$v_hat               <- e$v_hat
    player_matches$p_ll                <- e$p_ll
    player_matches$pre_elo             <- e$pre_elo
    player_matches$pre_rank_pts        <- e$pre_rank_pts
    player_matches$player_age_at_event <- e$player_age
    player_matches$had_prior_ll        <- e$had_prior_ll
    player_matches$ev_date             <- ev_date
    player_matches$days_after_event    <- as.numeric(difftime(player_matches$tourney_date,
                                                              ev_date, units = "days"))
    player_matches$weeks_after_event   <- player_matches$days_after_event / 7

    results[[length(results) + 1]] <- player_matches
  }

  match_df <- bind_rows(results)
  if (nrow(match_df) == 0) {
    message("    ", tour_label, " WARNING: 0 match-level observations!")
    return(match_df)
  }

  # Add match-level features
  match_df <- match_df |>
    mutate(
      log_rank_ratio    = log(pmax(opp_rank, 1) / pmax(focal_rank, 1)),
      log_rank_ratio_sq = log_rank_ratio^2,
      rank_diff         = opp_rank - focal_rank,
      same_ioc      = as.integer(!is.na(focal_ioc) & !is.na(opp_ioc) & focal_ioc == opp_ioc),
      is_clay        = as.integer(!is.na(surface) & tolower(surface) == "clay"),
      is_grass       = as.integer(!is.na(surface) & tolower(surface) == "grass"),
      age_diff       = ifelse(!is.na(focal_age) & !is.na(opp_age), focal_age - opp_age, 0),
      height_diff    = ifelse(!is.na(focal_ht) & !is.na(opp_ht), focal_ht - opp_ht, 0),
      hand_mismatch  = as.integer(!is.na(focal_hand) & !is.na(opp_hand) &
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
                 "focal_rank", "opp_rank", "best_of", "draw_size")
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

  match_df
}

# Build "any LL stop" match data for all 4 samples
message("  Building GS-ATP (any LL stop)...")
gs_atp_md_any <- build_match_level_custom(gs_atp_ev_any, matches, "ATP",
                                           next_ll_col = "next_ll_opp_date_any")
message("  Building GS-WTA (any LL stop)...")
gs_wta_md_any <- build_match_level_custom(gs_wta_ev_any, matches, "WTA",
                                           next_ll_col = "next_ll_opp_date_any")
message("  Building nonGS-ATP (any LL stop)...")
nongs_atp_md_any <- build_match_level_custom(nongs_atp_ev_any, matches, "ATP",
                                              next_ll_col = "next_ll_opp_date_any")
message("  Building nonGS-WTA (any LL stop)...")
nongs_wta_md_any <- build_match_level_custom(nongs_wta_ev_any, matches, "WTA",
                                              next_ll_col = "next_ll_opp_date_any")

slog("## Phase 2: Any-LL Stop Match Data")
slog("- GS-ATP: any=", nrow(gs_atp_md_any), " vs same=", nrow(gs_atp_md_same))
slog("- GS-WTA: any=", nrow(gs_wta_md_any), " vs same=", nrow(gs_wta_md_same))
slog("- nonGS-ATP: any=", nrow(nongs_atp_md_any), " vs same=", nrow(nongs_atp_md_same))
slog("- nonGS-WTA: any=", nrow(nongs_wta_md_any), " vs same=", nrow(nongs_wta_md_same))
slog("")


###############################################################################
# PHASE 3: POINTS WEIGHTING
###############################################################################

message("\n[3] Computing ranking-points weights...")

# Determine tournament tier from tourney_level and draw_size
get_tier <- function(tourney_level, draw_size = NA) {
  if (is.na(tourney_level)) return("A250")
  if (tourney_level == "G") return("G")
  if (tourney_level %in% c("M", "PM")) return("M")
  if (tourney_level == "P") return("P")
  if (tourney_level == "I") return("I")
  if (tourney_level == "A") {
    if (!is.na(draw_size) && draw_size >= 48) return("A500")
    return("A250")
  }
  if (tourney_level == "C") return("C")
  "A250"
}

# Map round string to schedule key
normalize_round <- function(round_str) {
  r <- toupper(trimws(round_str))
  # Map common round labels to schedule keys
  case_when(
    r %in% c("F", "F ")         ~ "F",
    r %in% c("SF", "SF ")       ~ "SF",
    r %in% c("QF", "QF ")       ~ "QF",
    r %in% c("R16", "R16 ")     ~ "R16",
    r %in% c("R32", "R32 ")     ~ "R32",
    r %in% c("R64", "R64 ")     ~ "R64",
    r %in% c("R128", "R128 ")   ~ "R128",
    r == "W"                     ~ "W",
    r == "RR"                    ~ "R32",
    TRUE                         ~ NA_character_
  )
}

# Add points weight to a match dataframe
add_points_weight <- function(df) {
  df$points_weight <- 1  # default weight

  for (i in seq_len(nrow(df))) {
    tl <- df$tourney_level[i]
    ds <- if ("draw_size" %in% names(df)) df$draw_size[i] else NA_integer_
    tier <- get_tier(tl, ds)
    sched <- POINTS_BY_ROUND[[tier]]
    if (is.null(sched)) sched <- POINTS_BY_ROUND[["A250"]]

    rnd <- normalize_round(df$round[i])
    if (!is.na(rnd) && rnd %in% names(sched)) {
      pts <- sched[rnd]
      # Use ranking points as weight; floor at 1 so all matches count
      df$points_weight[i] <- max(pts, 1)
    } else {
      # If round not in schedule (e.g., challenger early rounds), use 1
      df$points_weight[i] <- 1
    }
  }
  df
}

# Add points weights to all 8 match datasets
gs_atp_md_any     <- add_points_weight(gs_atp_md_any)
gs_atp_md_same    <- add_points_weight(gs_atp_md_same)
gs_wta_md_any     <- add_points_weight(gs_wta_md_any)
gs_wta_md_same    <- add_points_weight(gs_wta_md_same)
nongs_atp_md_any  <- add_points_weight(nongs_atp_md_any)
nongs_atp_md_same <- add_points_weight(nongs_atp_md_same)
nongs_wta_md_any  <- add_points_weight(nongs_wta_md_any)
nongs_wta_md_same <- add_points_weight(nongs_wta_md_same)

message("  Points weights added to all 8 datasets")


###############################################################################
# PHASE 4: ESTIMATION FUNCTIONS
###############################################################################

message("\n[4] Defining estimation functions...")

# -- Impute NAs ----------------------------------------------------------------
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

# -- GS estimation (simple logit, clustered SEs) ------------------------------
estimate_gs <- function(match_df, weighted = FALSE, label = "") {
  if (is.null(match_df) || nrow(match_df) < 30) {
    message("    ", label, ": insufficient observations (", nrow(match_df), ")")
    return(NULL)
  }

  df <- impute_match_data(match_df)

  fml <- won ~ got_ll +
    log_rank_ratio + log_rank_ratio_sq + rank_diff +
    same_ioc + is_clay + is_grass + age_diff + height_diff +
    hand_mismatch + h2h_smoothed + n_h2h +
    pre_elo + opp_elo + pre_rank_pts +
    player_age_at_event + had_prior_ll

  if (weighted) {
    model <- glm(fml, family = binomial(link = "logit"), data = df,
                 weights = df$points_weight)
  } else {
    model <- glm(fml, family = binomial(link = "logit"), data = df)
  }

  V <- vcovCL(model, cluster = df$focal_pid, type = "HC1")
  se_cl <- sqrt(diag(V))

  delta    <- coef(model)["got_ll"]
  delta_se <- se_cl["got_ll"]
  delta_z  <- delta / delta_se
  delta_p  <- 2 * pnorm(-abs(delta_z))

  n_matches <- nrow(df)
  n_events  <- n_distinct(df$event_id)

  message(sprintf("    %s: delta=%.4f (SE=%.4f, p=%.4f), N=%d, events=%d",
                  label, delta, delta_se, delta_p, n_matches, n_events))

  list(delta = unname(delta), delta_se = unname(delta_se), delta_p = unname(delta_p),
       n_matches = n_matches, n_events = n_events,
       rho = NA_real_, rho_p = NA_real_)
}

# -- NonGS estimation (CF logit with v_hat, bootstrap SEs) --------------------
estimate_nongs <- function(match_df, ev_table, weighted = FALSE,
                           n_boot = N_BOOT, label = "") {
  if (is.null(match_df) || nrow(match_df) < 50) {
    message("    ", label, ": insufficient observations (", nrow(match_df), ")")
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
    model <- glm(fml, family = binomial(link = "logit"), data = df,
                 weights = df$points_weight)
  } else {
    model <- glm(fml, family = binomial(link = "logit"), data = df)
  }

  s <- summary(model)$coefficients
  delta_pt <- s["got_ll", "Estimate"]
  rho_pt   <- s["v_hat", "Estimate"]

  n_matches <- nrow(df)
  n_events  <- n_distinct(df$event_id)

  # Player-level block bootstrap for SEs
  players <- unique(ev_table$player_id)
  n_players <- length(players)
  ev_by_player <- split(ev_table, ev_table$player_id)

  boot_delta <- numeric(0)
  boot_rho   <- numeric(0)

  t_start <- Sys.time()

  for (b in seq_len(n_boot)) {
    if (b %% 100 == 0) {
      elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
      message(sprintf("      %s: boot rep %d/%d (%.1f min elapsed)",
                      label, b, n_boot, elapsed))
    }

    tryCatch({
      boot_players <- sample(players, n_players, replace = TRUE)
      boot_ev <- bind_rows(lapply(boot_players, function(p) ev_by_player[[as.character(p)]]))
      if (nrow(boot_ev) < 20) next

      boot_md <- df |> filter(event_id %in% boot_ev$event_id)
      if (nrow(boot_md) < 50) next

      # Recompute generalized residuals for bootstrap sample
      q <- qnorm(boot_ev$p_ll)
      phi_q <- dnorm(q)
      boot_ev$v_hat <- boot_ev$got_ll * phi_q / boot_ev$p_ll -
        (1 - boot_ev$got_ll) * phi_q / (1 - boot_ev$p_ll)

      vhat_lookup <- boot_ev |> select(event_id, v_hat_boot = v_hat)
      boot_md <- boot_md |>
        left_join(vhat_lookup, by = "event_id") |>
        mutate(v_hat = coalesce(v_hat_boot, v_hat)) |>
        select(-v_hat_boot)

      if (weighted) {
        fit_b <- glm(fml, family = binomial(link = "logit"), data = boot_md,
                     weights = boot_md$points_weight)
      } else {
        fit_b <- glm(fml, family = binomial(link = "logit"), data = boot_md)
      }

      if (!fit_b$converged) next

      boot_delta <- c(boot_delta, unname(coef(fit_b)["got_ll"]))
      boot_rho   <- c(boot_rho,   unname(coef(fit_b)["v_hat"]))
    }, error = function(e) NULL)
  }

  elapsed_total <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))

  if (length(boot_delta) < 10) {
    message("    ", label, ": too few bootstrap reps (", length(boot_delta), ")")
    delta_se <- s["got_ll", "Std. Error"]
    rho_se   <- s["v_hat", "Std. Error"]
  } else {
    delta_se <- sd(boot_delta, na.rm = TRUE)
    rho_se   <- sd(boot_rho, na.rm = TRUE)
  }

  delta_p <- 2 * pnorm(-abs(delta_pt / delta_se))
  rho_p   <- 2 * pnorm(-abs(rho_pt / rho_se))

  message(sprintf("    %s: delta=%.4f (bootSE=%.4f, p=%.4f), rho=%.4f (p=%.4f), N=%d, events=%d [%.1f min]",
                  label, delta_pt, delta_se, delta_p, rho_pt, rho_p,
                  n_matches, n_events, elapsed_total))

  list(delta = unname(delta_pt), delta_se = delta_se, delta_p = delta_p,
       n_matches = n_matches, n_events = n_events,
       rho = unname(rho_pt), rho_p = rho_p)
}


###############################################################################
# PHASE 5: ESTIMATE ALL SPECIFICATIONS
###############################################################################

message("\n[5] Estimating all 4 specifications x 4 samples...")
message("    This will take a while for non-GS samples (bootstrap)...\n")

# Storage for results: list of lists
all_results <- list()

# --- GS-ATP ---
message("  === GS-ATP ===")
all_results$gs_atp <- list(
  unw_any  = estimate_gs(gs_atp_md_any,  weighted = FALSE, label = "GS-ATP unw/any"),
  unw_same = estimate_gs(gs_atp_md_same, weighted = FALSE, label = "GS-ATP unw/same"),
  wt_any   = estimate_gs(gs_atp_md_any,  weighted = TRUE,  label = "GS-ATP wt/any"),
  wt_same  = estimate_gs(gs_atp_md_same, weighted = TRUE,  label = "GS-ATP wt/same")
)

# --- GS-WTA ---
message("\n  === GS-WTA ===")
all_results$gs_wta <- list(
  unw_any  = estimate_gs(gs_wta_md_any,  weighted = FALSE, label = "GS-WTA unw/any"),
  unw_same = estimate_gs(gs_wta_md_same, weighted = FALSE, label = "GS-WTA unw/same"),
  wt_any   = estimate_gs(gs_wta_md_any,  weighted = TRUE,  label = "GS-WTA wt/any"),
  wt_same  = estimate_gs(gs_wta_md_same, weighted = TRUE,  label = "GS-WTA wt/same")
)

# --- nonGS-ATP ---
message("\n  === nonGS-ATP ===")
all_results$nongs_atp <- list(
  unw_any  = estimate_nongs(nongs_atp_md_any,  nongs_atp_ev, weighted = FALSE,
                            label = "nonGS-ATP unw/any"),
  unw_same = estimate_nongs(nongs_atp_md_same, nongs_atp_ev, weighted = FALSE,
                            label = "nonGS-ATP unw/same"),
  wt_any   = estimate_nongs(nongs_atp_md_any,  nongs_atp_ev, weighted = TRUE,
                            label = "nonGS-ATP wt/any"),
  wt_same  = estimate_nongs(nongs_atp_md_same, nongs_atp_ev, weighted = TRUE,
                            label = "nonGS-ATP wt/same")
)

# --- nonGS-WTA ---
message("\n  === nonGS-WTA ===")
all_results$nongs_wta <- list(
  unw_any  = estimate_nongs(nongs_wta_md_any,  nongs_wta_ev, weighted = FALSE,
                            label = "nonGS-WTA unw/any"),
  unw_same = estimate_nongs(nongs_wta_md_same, nongs_wta_ev, weighted = FALSE,
                            label = "nonGS-WTA unw/same"),
  wt_any   = estimate_nongs(nongs_wta_md_any,  nongs_wta_ev, weighted = TRUE,
                            label = "nonGS-WTA wt/any"),
  wt_same  = estimate_nongs(nongs_wta_md_same, nongs_wta_ev, weighted = TRUE,
                            label = "nonGS-WTA wt/same")
)


###############################################################################
# PHASE 6: GENERATE TABLES
###############################################################################

message("\n[6] Generating robustness comparison tables...")

# Spec labels (rows of the table)
SPEC_LABELS <- c(
  unw_any  = "Unweighted, any LL stop",
  unw_same = "Unweighted, same-type stop",
  wt_any   = "Points-weighted, any LL stop",
  wt_same  = "Points-weighted, same-type stop"
)

write_robustness_table <- function(results_list, filepath, is_gs = TRUE, label = "") {
  # results_list: named list with keys unw_any, unw_same, wt_any, wt_same
  # Each element has: delta, delta_se, delta_p, n_matches, n_events, rho, rho_p

  lines <- character()
  if (is_gs) {
    lines <- c(lines,
      "\\begin{tabular}{lrrrrrr}",
      "\\toprule",
      "Specification & $N_{\\text{matches}}$ & $N_{\\text{events}}$ & $\\hat{\\delta}$ & SE & OR \\\\",
      "\\midrule"
    )
  } else {
    lines <- c(lines,
      "\\begin{tabular}{lrrrrrrr}",
      "\\toprule",
      "Specification & $N_{\\text{matches}}$ & $N_{\\text{events}}$ & $\\hat{\\delta}$ & SE & OR & $\\hat{\\rho}$ & Endog.\\ $p$ \\\\",
      "\\midrule"
    )
  }

  for (spec_key in names(SPEC_LABELS)) {
    r <- results_list[[spec_key]]
    if (is.null(r)) {
      if (is_gs) {
        lines <- c(lines, sprintf("%s & --- & --- & --- & --- & --- \\\\", SPEC_LABELS[spec_key]))
      } else {
        lines <- c(lines, sprintf("%s & --- & --- & --- & --- & --- & --- & --- \\\\", SPEC_LABELS[spec_key]))
      }
      next
    }

    n_match_str <- format(r$n_matches, big.mark = ",")
    n_event_str <- format(r$n_events, big.mark = ",")
    delta_str   <- paste0(fmt(r$delta, 4), add_stars(r$delta_p))
    se_str      <- paste0("(", fmt(r$delta_se, 4), ")")
    or_str      <- fmt(exp(r$delta), 3)

    if (is_gs) {
      lines <- c(lines,
        sprintf("%s & %s & %s & %s & %s & %s \\\\",
                SPEC_LABELS[spec_key], n_match_str, n_event_str,
                delta_str, se_str, or_str)
      )
    } else {
      rho_str <- if (!is.na(r$rho)) fmt(r$rho, 4) else "---"
      endp_str <- if (!is.na(r$rho_p)) fmt(r$rho_p, 3) else "---"
      lines <- c(lines,
        sprintf("%s & %s & %s & %s & %s & %s & %s & %s \\\\",
                SPEC_LABELS[spec_key], n_match_str, n_event_str,
                delta_str, se_str, or_str, rho_str, endp_str)
      )
    }
  }

  lines <- c(lines,
    "\\bottomrule",
    "\\end{tabular}"
  )

  writeLines(lines, filepath)
  message("  Saved: ", filepath)
}

# Write all 4 tables
write_robustness_table(all_results$gs_atp,
                       file.path(TABLE_DIR, "table_tournament_robustness_gs_atp.tex"),
                       is_gs = TRUE, label = "GS-ATP")
write_robustness_table(all_results$gs_wta,
                       file.path(TABLE_DIR, "table_tournament_robustness_gs_wta.tex"),
                       is_gs = TRUE, label = "GS-WTA")
write_robustness_table(all_results$nongs_atp,
                       file.path(TABLE_DIR, "table_tournament_robustness_nongs_atp.tex"),
                       is_gs = FALSE, label = "nonGS-ATP")
write_robustness_table(all_results$nongs_wta,
                       file.path(TABLE_DIR, "table_tournament_robustness_nongs_wta.tex"),
                       is_gs = FALSE, label = "nonGS-WTA")


###############################################################################
# PHASE 7: SUMMARY
###############################################################################

message("\n[7] Writing summary...")

summary_text <- c(
  "# Tournament Robustness Summary (Script 25)",
  paste0("Generated: ", Sys.time()),
  "",
  "## Design",
  "",
  "4 specifications per sample, varying:",
  "- Window stop rule: any LL opportunity (GS or non-GS) vs same-type LL only",
  "- Weighting: unweighted vs ranking-points-weighted",
  "",
  "GS models: simple logit with clustered SEs (lottery assignment).",
  "NonGS models: CF logit with generalized residual v_hat, 200-rep player-level block bootstrap.",
  ""
)

for (sample_key in c("gs_atp", "gs_wta", "nongs_atp", "nongs_wta")) {
  sample_label <- toupper(gsub("_", "-", sample_key))
  summary_text <- c(summary_text,
    paste0("## ", sample_label),
    "",
    "| Specification | N_matches | N_events | delta | SE | OR | rho | endog p |",
    "|---|---|---|---|---|---|---|---|"
  )

  for (spec_key in names(SPEC_LABELS)) {
    r <- all_results[[sample_key]][[spec_key]]
    if (is.null(r)) {
      summary_text <- c(summary_text,
        sprintf("| %s | --- | --- | --- | --- | --- | --- | --- |", SPEC_LABELS[spec_key]))
    } else {
      rho_str <- if (!is.na(r$rho)) fmt(r$rho, 4) else "---"
      endp_str <- if (!is.na(r$rho_p)) fmt(r$rho_p, 3) else "---"
      summary_text <- c(summary_text,
        sprintf("| %s | %s | %d | %.4f | %.4f | %.3f | %s | %s |",
                SPEC_LABELS[spec_key], format(r$n_matches, big.mark = ","),
                r$n_events, r$delta, r$delta_se, exp(r$delta),
                rho_str, endp_str))
    }
  }
  summary_text <- c(summary_text, "")
}

# Append summary_log
summary_text <- c(summary_text, "", "## Execution Log", "", summary_log)

writeLines(summary_text, file.path(OUTPUT_DIR, "tournament_robustness_summary.md"))
message("  Saved: Output/tournament_robustness_summary.md")

# Save results object
saveRDS(all_results, file.path(CLEANED_DIR, "tournament_robustness_results.rds"))
message("  Saved: Data/cleaned/tournament_robustness_results.rds")

message("\n", strrep("=", 72))
message("  DONE: 25_tournament_robustness.R")
message(strrep("=", 72), "\n")
