# ==============================================================================
# 28_tournament_first_ll.R
# First-LL restriction and treatment dose analysis for tournament performance
#
# Purpose:
#   PART 1: Replicate tournament model for players with at most 1 LL award
#           Two variants: type-specific restriction & any-type restriction
#   PART 2: Add treatment dose interactions (matches_won at LL event)
#
# Inputs:
#   Data/cleaned/tournament_match_fix_results.rds  (from script 24)
#   Data/cleaned/tournament_rebuild_results.rds    (from script 22)
#   Data/cleaned/tournament_elo_cache.rds
#   Data/raw/atp_main_matches.rds, atp_qual_chall_matches.rds
#   Data/raw/wta_main_matches.rds, wta_qual_itf_matches.rds
#
# Outputs:
#   Tables: 24 .tex files (12 firstll, 12 anytype, plus dose tables)
#   Data/cleaned/tournament_firstll_results.rds
#   Data/cleaned/tournament_dose_results.rds
#   Output/tournament_firstll_summary.md
#
# Dependencies: dplyr, tidyr, readr, stringr, data.table, sandwich, here
# ==============================================================================

set.seed(20260326)

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(data.table)
library(sandwich)
library(here)

source(here("scripts", "R", "utils.R"))

# -- Project paths -------------------------------------------------------------
RAW_DIR     <- here("Data", "raw")
CLEANED_DIR <- here("Data", "cleaned")
TABLE_DIR   <- here("Tables")
FIG_DIR     <- here("Figures")
OUTPUT_DIR  <- here("Output")

for (d in c(CLEANED_DIR, TABLE_DIR, FIG_DIR, OUTPUT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# -- Analysis parameters -------------------------------------------------------
CALENDAR_CAP <- 365L
N_BOOT       <- 200L
HORIZONS     <- c(4, 8, 12, 26, 52)

# -- Points schedules ----------------------------------------------------------
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
message("  FIRST-LL RESTRICTION + DOSE ANALYSIS (28_tournament_first_ll.R)")
message(strrep("=", 72))


###############################################################################
# PHASE 1: LOAD DATA
###############################################################################

message("\n[1] Loading data...")

R24 <- readRDS(file.path(CLEANED_DIR, "tournament_match_fix_results.rds"))
R22 <- readRDS(file.path(CLEANED_DIR, "tournament_rebuild_results.rds"))

# Extract event tables
gs_atp_ev    <- R24$gs_atp$events
gs_wta_ev    <- R24$gs_wta$events
nongs_atp_ev <- R24$nongs_atp$events
nongs_wta_ev <- R24$nongs_wta$events

# Same-type stop match data from script 24
gs_atp_md_same    <- R24$gs_atp$matches
gs_wta_md_same    <- R24$gs_wta$matches
nongs_atp_md_same <- R24$nongs_atp$matches
nongs_wta_md_same <- R24$nongs_wta$matches

slog("## Phase 1: Data Loaded")
slog("- GS-ATP events: ", nrow(gs_atp_ev), " | matches: ", nrow(gs_atp_md_same))
slog("- GS-WTA events: ", nrow(gs_wta_ev), " | matches: ", nrow(gs_wta_md_same))
slog("- nonGS-ATP events: ", nrow(nongs_atp_ev), " | matches: ", nrow(nongs_atp_md_same))
slog("- nonGS-WTA events: ", nrow(nongs_wta_ev), " | matches: ", nrow(nongs_wta_md_same))
slog("")


###############################################################################
# PHASE 2: BUILD "ANY LL STOP" MATCH DATA (reuse from script 25/27 approach)
###############################################################################

message("\n[2] Building 'any LL stop' match data and loading raw matches...")

# Load raw match data
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

# Harmonize matches
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

# Build cross-type next_ll_opp_dates for "any LL stop"
atp_all_events <- bind_rows(
  gs_atp_ev |> mutate(event_type = "GS"),
  nongs_atp_ev |> mutate(event_type = "nonGS")
) |> arrange(player_id, tourney_date)

wta_all_events <- bind_rows(
  gs_wta_ev |> mutate(event_type = "GS"),
  nongs_wta_ev |> mutate(event_type = "nonGS")
) |> arrange(player_id, tourney_date)

compute_any_ll_next <- function(all_events) {
  all_events |>
    arrange(player_id, tourney_date) |>
    group_by(player_id) |>
    mutate(next_ll_opp_date_any = lead(tourney_date, default = as.Date("2099-12-31"))) |>
    ungroup()
}

atp_all_events <- compute_any_ll_next(atp_all_events)
wta_all_events <- compute_any_ll_next(wta_all_events)

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

# Build match-level data with custom truncation
build_match_level_custom <- function(ev_table, all_matches, tour_label,
                                     next_ll_col = "next_ll_opp_date_any",
                                     calendar_cap_days = CALENDAR_CAP) {
  md <- all_matches |>
    filter(tour == tour_label) |>
    mutate(
      tourney_date = if (inherits(tourney_date, "Date")) tourney_date
                     else as.Date(as.character(tourney_date), format = "%Y%m%d")
    )

  ev_sorted <- ev_table |> arrange(player_id, tourney_date)
  if (next_ll_col %in% names(ev_sorted)) {
    ev_sorted$next_ll_opp_date <- ev_sorted[[next_ll_col]]
  } else {
    ev_sorted <- ev_sorted |>
      group_by(player_id) |>
      mutate(next_ll_opp_date = lead(tourney_date, default = as.Date("2099-12-31"))) |>
      ungroup()
  }
  ev_sorted$cal_cap_date <- ev_sorted$tourney_date + calendar_cap_days

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

  match_df <- match_df |>
    filter(!is.na(log_rank_ratio), !is.na(won), !is.na(got_ll), !is.na(v_hat))

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

slog("## Phase 2: Match Data Built")
slog("- GS-ATP: same=", nrow(gs_atp_md_same), ", any=", nrow(gs_atp_md_any))
slog("- GS-WTA: same=", nrow(gs_wta_md_same), ", any=", nrow(gs_wta_md_any))
slog("- nonGS-ATP: same=", nrow(nongs_atp_md_same), ", any=", nrow(nongs_atp_md_any))
slog("- nonGS-WTA: same=", nrow(nongs_wta_md_same), ", any=", nrow(nongs_wta_md_any))
slog("")


###############################################################################
# PHASE 3: COMPUTE TYPE-SPECIFIC PRIOR LL COUNTS + FIRST-LL RESTRICTION
###############################################################################

message("\n[3] Computing first-LL restrictions...")

# Type-specific restriction: for GS samples, no prior GS LL; for nonGS, no prior nonGS LL
# The event tables only have had_prior_ll (any type). We need to compute type-specific.
# Since events are ordered by player_id and tourney_date, we can compute cumulative
# type-specific LL wins within each event table.

add_type_specific_prior <- function(ev_table) {
  ev_table |>
    arrange(player_id, tourney_date) |>
    group_by(player_id) |>
    mutate(
      cum_type_ll = cumsum(got_ll) - got_ll,
      n_prior_type_ll_won = as.integer(cum_type_ll)
    ) |>
    ungroup() |>
    select(-cum_type_ll)
}

gs_atp_ev    <- add_type_specific_prior(gs_atp_ev)
gs_wta_ev    <- add_type_specific_prior(gs_wta_ev)
nongs_atp_ev <- add_type_specific_prior(nongs_atp_ev)
nongs_wta_ev <- add_type_specific_prior(nongs_wta_ev)

# Also add to the _any variants
gs_atp_ev_any    <- add_type_specific_prior(gs_atp_ev_any)
gs_wta_ev_any    <- add_type_specific_prior(gs_wta_ev_any)
nongs_atp_ev_any <- add_type_specific_prior(nongs_atp_ev_any)
nongs_wta_ev_any <- add_type_specific_prior(nongs_wta_ev_any)

# FIRSTLL restriction: type-specific (no prior LL of SAME type)
apply_firstll_restriction <- function(ev_table, md_table) {
  first_events <- ev_table |> filter(n_prior_type_ll_won == 0)
  first_eids <- first_events$event_id
  md_restricted <- md_table |> filter(event_id %in% first_eids)
  list(events = first_events, matches = md_restricted)
}

# ANYTYPE restriction: no prior LL of ANY type (had_prior_ll == 0)
apply_anytype_restriction <- function(ev_table, md_table) {
  first_events <- ev_table |> filter(had_prior_ll == 0)
  first_eids <- first_events$event_id
  md_restricted <- md_table |> filter(event_id %in% first_eids)
  list(events = first_events, matches = md_restricted)
}

# Apply firstll (type-specific) restriction to all 8 datasets
firstll <- list()
firstll$gs_atp_same    <- apply_firstll_restriction(gs_atp_ev, gs_atp_md_same)
firstll$gs_atp_any     <- apply_firstll_restriction(gs_atp_ev_any, gs_atp_md_any)
firstll$gs_wta_same    <- apply_firstll_restriction(gs_wta_ev, gs_wta_md_same)
firstll$gs_wta_any     <- apply_firstll_restriction(gs_wta_ev_any, gs_wta_md_any)
firstll$nongs_atp_same <- apply_firstll_restriction(nongs_atp_ev, nongs_atp_md_same)
firstll$nongs_atp_any  <- apply_firstll_restriction(nongs_atp_ev_any, nongs_atp_md_any)
firstll$nongs_wta_same <- apply_firstll_restriction(nongs_wta_ev, nongs_wta_md_same)
firstll$nongs_wta_any  <- apply_firstll_restriction(nongs_wta_ev_any, nongs_wta_md_any)

# Apply anytype restriction to all 8 datasets
anytype <- list()
anytype$gs_atp_same    <- apply_anytype_restriction(gs_atp_ev, gs_atp_md_same)
anytype$gs_atp_any     <- apply_anytype_restriction(gs_atp_ev_any, gs_atp_md_any)
anytype$gs_wta_same    <- apply_anytype_restriction(gs_wta_ev, gs_wta_md_same)
anytype$gs_wta_any     <- apply_anytype_restriction(gs_wta_ev_any, gs_wta_md_any)
anytype$nongs_atp_same <- apply_anytype_restriction(nongs_atp_ev, nongs_atp_md_same)
anytype$nongs_atp_any  <- apply_anytype_restriction(nongs_atp_ev_any, nongs_atp_md_any)
anytype$nongs_wta_same <- apply_anytype_restriction(nongs_wta_ev, nongs_wta_md_same)
anytype$nongs_wta_any  <- apply_anytype_restriction(nongs_wta_ev_any, nongs_wta_md_any)

# Log counts
slog("## Phase 3: First-LL Sample Counts")
for (nm in sort(names(firstll))) {
  slog(sprintf("  firstll/%s: events=%d (LL=%d), matches=%d",
               nm, nrow(firstll[[nm]]$events),
               sum(firstll[[nm]]$events$got_ll),
               nrow(firstll[[nm]]$matches)))
}
slog("")
for (nm in sort(names(anytype))) {
  slog(sprintf("  anytype/%s: events=%d (LL=%d), matches=%d",
               nm, nrow(anytype[[nm]]$events),
               sum(anytype[[nm]]$events$got_ll),
               nrow(anytype[[nm]]$matches)))
}
slog("")


###############################################################################
# PHASE 4: HELPER FUNCTIONS (shared across all analyses)
###############################################################################

message("\n[4] Defining estimation helper functions...")

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

normalize_round <- function(round_str) {
  r <- toupper(trimws(round_str))
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

add_points_weight <- function(df) {
  df$points_weight <- 1
  for (i in seq_len(nrow(df))) {
    tl <- df$tourney_level[i]
    ds <- if ("draw_size" %in% names(df)) df$draw_size[i] else NA_integer_
    tier <- get_tier(tl, ds)
    sched <- POINTS_BY_ROUND[[tier]]
    if (is.null(sched)) sched <- POINTS_BY_ROUND[["A250"]]
    rnd <- normalize_round(df$round[i])
    if (!is.na(rnd) && rnd %in% names(sched)) {
      pts <- sched[rnd]
      df$points_weight[i] <- max(pts, 1)
    } else {
      df$points_weight[i] <- 1
    }
  }
  df
}

add_horizon_dummies <- function(df) {
  df <- df |>
    mutate(
      D_4w  = as.numeric(got_ll) * as.numeric(weeks_after_event <= 4),
      D_8w  = as.numeric(got_ll) * as.numeric(weeks_after_event <= 8),
      D_12w = as.numeric(got_ll) * as.numeric(weeks_after_event <= 12),
      D_26w = as.numeric(got_ll) * as.numeric(weeks_after_event <= 26),
      D_52w = as.numeric(got_ll) * as.numeric(weeks_after_event <= 52)
    )
  if ("v_hat" %in% names(df)) {
    df <- df |>
      mutate(
        v_4w  = v_hat * as.numeric(weeks_after_event <= 4),
        v_8w  = v_hat * as.numeric(weeks_after_event <= 8),
        v_12w = v_hat * as.numeric(weeks_after_event <= 12),
        v_26w = v_hat * as.numeric(weeks_after_event <= 26),
        v_52w = v_hat * as.numeric(weeks_after_event <= 52)
      )
  }
  df
}

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

compute_cumulative_from_vcov <- function(deltas, V_delta) {
  n <- length(deltas)
  cum_totals <- numeric(n)
  cum_ses    <- numeric(n)
  for (i in seq_len(n)) {
    idx <- i:n
    cum_totals[i] <- sum(deltas[idx])
    cum_ses[i] <- sqrt(sum(V_delta[idx, idx]))
  }
  cum_ps <- 2 * pnorm(-abs(cum_totals / cum_ses))
  list(totals = cum_totals, ses = cum_ses, ps = cum_ps, ors = exp(cum_totals))
}


###############################################################################
# PHASE 5: MAIN ESTIMATION FUNCTIONS
###############################################################################

message("\n[5] Defining main estimation functions...")

# -- GS estimation (simple logit, clustered SEs) --- NO had_prior_ll ----------
# In firstll samples, had_prior_ll is constant (0) or near-constant, so we drop it.
estimate_gs_firstll <- function(match_df, label = "", weighted = FALSE) {
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
    player_age_at_event

  if (weighted) {
    if (!"points_weight" %in% names(df)) df <- add_points_weight(df)
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
  n_players <- n_distinct(df$focal_pid)

  message(sprintf("    %s: delta=%.4f (SE=%.4f, p=%.4f), N=%d, events=%d, players=%d",
                  label, delta, delta_se, delta_p, n_matches, n_events, n_players))

  list(model = model, vcov_cl = V, data = df,
       delta = unname(delta), delta_se = unname(delta_se), delta_p = unname(delta_p),
       n_matches = n_matches, n_events = n_events, n_players = n_players,
       rho = NA_real_, rho_se = NA_real_, rho_p = NA_real_)
}

# -- NonGS estimation (CF logit with v_hat, bootstrap SEs) --- NO had_prior_ll
estimate_nongs_firstll <- function(match_df, ev_table, n_boot = N_BOOT,
                                    weighted = FALSE, label = "") {
  if (is.null(match_df) || nrow(match_df) < 50) {
    message("    ", label, ": insufficient observations (",
            if (is.null(match_df)) 0 else nrow(match_df), ")")
    return(NULL)
  }

  df <- impute_match_data(match_df)
  if (weighted && !"points_weight" %in% names(df)) df <- add_points_weight(df)

  fml <- won ~ got_ll + v_hat +
    log_rank_ratio + log_rank_ratio_sq + rank_diff +
    same_ioc + is_clay + is_grass + age_diff + height_diff +
    hand_mismatch + h2h_smoothed + n_h2h +
    pre_elo + opp_elo + pre_rank_pts +
    player_age_at_event

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
  n_players <- n_distinct(df$focal_pid)

  # Player-level block bootstrap
  players <- unique(ev_table$player_id)
  n_pl <- length(players)
  ev_by_player <- split(ev_table, ev_table$player_id)

  boot_delta <- numeric(0)
  boot_rho   <- numeric(0)
  t_start <- Sys.time()

  for (b in seq_len(n_boot)) {
    if (b %% 50 == 0) {
      elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
      message(sprintf("      %s: boot rep %d/%d (%.1f min)", label, b, n_boot, elapsed))
    }
    tryCatch({
      boot_players <- sample(players, n_pl, replace = TRUE)
      boot_ev <- bind_rows(lapply(boot_players, function(p) ev_by_player[[as.character(p)]]))
      if (nrow(boot_ev) < 20) next
      boot_md <- df |> filter(event_id %in% boot_ev$event_id)
      if (nrow(boot_md) < 50) next

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
    delta_se <- s["got_ll", "Std. Error"]
    rho_se   <- s["v_hat", "Std. Error"]
    message("    ", label, ": too few boot reps (", length(boot_delta), "), using model SE")
  } else {
    delta_se <- sd(boot_delta, na.rm = TRUE)
    rho_se   <- sd(boot_rho, na.rm = TRUE)
  }

  delta_p <- 2 * pnorm(-abs(delta_pt / delta_se))
  rho_p   <- 2 * pnorm(-abs(rho_pt / rho_se))

  message(sprintf("    %s: delta=%.4f (bootSE=%.4f, p=%.4f), rho=%.4f (p=%.4f), N=%d [%.1f min]",
                  label, delta_pt, delta_se, delta_p, rho_pt, rho_p, n_matches, elapsed_total))

  list(model = model, data = df,
       delta = unname(delta_pt), delta_se = delta_se, delta_p = delta_p,
       n_matches = n_matches, n_events = n_events, n_players = n_players,
       rho = unname(rho_pt), rho_se = rho_se, rho_p = rho_p)
}


###############################################################################
# PHASE 6: COUNTERFACTUAL FUNCTIONS
###############################################################################

message("\n[6] Defining counterfactual functions...")

compute_gs_counterfactuals_firstll <- function(fit_obj, ev_table, label) {
  if (is.null(fit_obj)) return(NULL)

  model <- fit_obj$model
  df    <- fit_obj$data
  V     <- fit_obj$vcov_cl
  beta  <- coef(model)
  delta <- beta["got_ll"]

  linpred_obs <- predict(model, newdata = df, type = "link")
  linpred_d0  <- linpred_obs - delta * df$got_ll
  linpred_d1  <- linpred_d0 + delta

  df$p0 <- plogis(linpred_d0)
  df$p1 <- plogis(linpred_d1)
  df$marginal_effect <- df$p1 - df$p0

  avg_me <- mean(df$marginal_effect)
  d_ame_d_delta <- mean(df$p1 * (1 - df$p1))
  var_delta <- V["got_ll", "got_ll"]
  se_me <- abs(d_ame_d_delta) * sqrt(var_delta)
  p_me  <- 2 * pnorm(-abs(avg_me / se_me))

  df_sorted <- df |> arrange(event_id, tourney_id, round_num)
  tourney_effects <- df_sorted |>
    group_by(event_id, tourney_id) |>
    summarise(
      n_rounds = n(),
      ew_d0 = sum(cumprod(p0)),
      ew_d1 = sum(cumprod(p1)),
      delta_ew = sum(cumprod(p1)) - sum(cumprod(p0)),
      tourney_level = first(tourney_level),
      .groups = "drop"
    )
  avg_delta_ew <- mean(tourney_effects$delta_ew)

  eps <- 1e-5
  df$p1_plus  <- plogis(linpred_d0 + delta + eps)
  df$p1_minus <- plogis(linpred_d0 + delta - eps)
  df_s2 <- df |> arrange(event_id, tourney_id, round_num)

  te_plus <- df_s2 |>
    group_by(event_id, tourney_id) |>
    summarise(delta_ew = sum(cumprod(p1_plus)) - sum(cumprod(p0)), .groups = "drop")
  te_minus <- df_s2 |>
    group_by(event_id, tourney_id) |>
    summarise(delta_ew = sum(cumprod(p1_minus)) - sum(cumprod(p0)), .groups = "drop")

  d_dew_d_delta <- (mean(te_plus$delta_ew) - mean(te_minus$delta_ew)) / (2 * eps)
  se_dew <- abs(d_dew_d_delta) * sqrt(var_delta)
  p_dew  <- 2 * pnorm(-abs(avg_delta_ew / se_dew))

  tourney_effects$erp_d0 <- NA_real_
  tourney_effects$erp_d1 <- NA_real_
  for (k in seq_len(nrow(tourney_effects))) {
    eid <- tourney_effects$event_id[k]
    tid <- tourney_effects$tourney_id[k]
    sub <- df_s2 |> filter(event_id == eid, tourney_id == tid) |> arrange(round_num)
    tier <- get_tier(sub$tourney_level[1])
    tourney_effects$erp_d0[k] <- compute_erp(sub$p0, tier)
    tourney_effects$erp_d1[k] <- compute_erp(sub$p1, tier)
  }
  tourney_effects$delta_erp <- tourney_effects$erp_d1 - tourney_effects$erp_d0
  avg_delta_erp <- mean(tourney_effects$delta_erp)

  te_erp_plus <- te_erp_minus <- tourney_effects
  for (k in seq_len(nrow(tourney_effects))) {
    eid <- tourney_effects$event_id[k]
    tid <- tourney_effects$tourney_id[k]
    sub <- df_s2 |> filter(event_id == eid, tourney_id == tid) |> arrange(round_num)
    tier <- get_tier(sub$tourney_level[1])
    te_erp_plus$delta_erp[k]  <- compute_erp(sub$p1_plus, tier) - tourney_effects$erp_d0[k]
    te_erp_minus$delta_erp[k] <- compute_erp(sub$p1_minus, tier) - tourney_effects$erp_d0[k]
  }
  d_derp_d_delta <- (mean(te_erp_plus$delta_erp) - mean(te_erp_minus$delta_erp)) / (2 * eps)
  se_derp <- abs(d_derp_d_delta) * sqrt(var_delta)
  p_derp  <- 2 * pnorm(-abs(avg_delta_erp / se_derp))

  message(sprintf("  %s CF: DeltaP=%.4f (SE=%.4f), DeltaE[W]=%.4f (SE=%.4f), DeltaE[RP]=%.1f (SE=%.1f)",
                  label, avg_me, se_me, avg_delta_ew, se_dew, avg_delta_erp, se_derp))

  list(avg_me = avg_me, se_me = se_me, p_me = p_me,
       avg_delta_ew = avg_delta_ew, se_dew = se_dew, p_dew = p_dew,
       avg_delta_erp = avg_delta_erp, se_derp = se_derp, p_derp = p_derp)
}


###############################################################################
# PHASE 7: RUN ALL FIRST-LL ESTIMATIONS
###############################################################################

message("\n[7] Running first-LL estimations (all 8 samples x 2 restriction types)...")
message("    This will take a while due to bootstrap for nonGS samples...\n")

# Prepare all match datasets with points weights
prepare_md <- function(dataset_list) {
  for (nm in names(dataset_list)) {
    dataset_list[[nm]]$matches <- add_points_weight(dataset_list[[nm]]$matches)
    dataset_list[[nm]]$matches <- add_horizon_dummies(dataset_list[[nm]]$matches)
  }
  dataset_list
}

firstll <- prepare_md(firstll)
anytype <- prepare_md(anytype)

# ---- Estimation wrapper per restriction set ----------------------------------
run_all_estimates <- function(dataset_list, restriction_label) {
  results <- list()

  # ----- GS-ATP -----
  message(sprintf("\n  === %s: GS-ATP ===", restriction_label))
  gs_atp_same <- dataset_list$gs_atp_same
  gs_atp_any  <- dataset_list$gs_atp_any

  results$gs_atp <- list(
    main_fit  = estimate_gs_firstll(gs_atp_same$matches,
                                     label = paste0(restriction_label, " GS-ATP main")),
    main_wt   = estimate_gs_firstll(gs_atp_same$matches,
                                     label = paste0(restriction_label, " GS-ATP main_wt"), weighted = TRUE),
    cf = NULL,
    robustness = list(
      unw_any  = list(delta = NA, delta_se = NA, delta_p = NA, n_matches = 0, n_events = 0, rho = NA, rho_p = NA),
      unw_same = list(delta = NA, delta_se = NA, delta_p = NA, n_matches = 0, n_events = 0, rho = NA, rho_p = NA),
      wt_any   = list(delta = NA, delta_se = NA, delta_p = NA, n_matches = 0, n_events = 0, rho = NA, rho_p = NA),
      wt_same  = list(delta = NA, delta_se = NA, delta_p = NA, n_matches = 0, n_events = 0, rho = NA, rho_p = NA)
    ),
    events = gs_atp_same$events
  )

  # Counterfactuals
  if (!is.null(results$gs_atp$main_fit)) {
    results$gs_atp$cf <- compute_gs_counterfactuals_firstll(
      results$gs_atp$main_fit, gs_atp_same$events,
      paste0(restriction_label, " GS-ATP"))
  }

  # Robustness: 4 specs
  r_unw_any  <- estimate_gs_firstll(gs_atp_any$matches,
                                     label = paste0(restriction_label, " GS-ATP unw/any"))
  r_unw_same <- results$gs_atp$main_fit  # already estimated above (unweighted, same-type)
  r_wt_any   <- estimate_gs_firstll(gs_atp_any$matches,
                                     label = paste0(restriction_label, " GS-ATP wt/any"), weighted = TRUE)
  r_wt_same  <- results$gs_atp$main_wt   # already estimated above

  extract_robust <- function(fit) {
    if (is.null(fit)) return(list(delta = NA, delta_se = NA, delta_p = NA, n_matches = 0, n_events = 0, rho = NA_real_, rho_p = NA_real_))
    list(delta = fit$delta, delta_se = fit$delta_se, delta_p = fit$delta_p,
         n_matches = fit$n_matches, n_events = fit$n_events,
         rho = fit$rho, rho_p = fit$rho_p)
  }

  results$gs_atp$robustness <- list(
    unw_any  = extract_robust(r_unw_any),
    unw_same = extract_robust(r_unw_same),
    wt_any   = extract_robust(r_wt_any),
    wt_same  = extract_robust(r_wt_same)
  )

  # ----- GS-WTA -----
  message(sprintf("\n  === %s: GS-WTA ===", restriction_label))
  gs_wta_same <- dataset_list$gs_wta_same
  gs_wta_any  <- dataset_list$gs_wta_any

  results$gs_wta <- list(
    main_fit  = estimate_gs_firstll(gs_wta_same$matches,
                                     label = paste0(restriction_label, " GS-WTA main")),
    main_wt   = estimate_gs_firstll(gs_wta_same$matches,
                                     label = paste0(restriction_label, " GS-WTA main_wt"), weighted = TRUE),
    cf = NULL, robustness = list(), events = gs_wta_same$events
  )
  if (!is.null(results$gs_wta$main_fit)) {
    results$gs_wta$cf <- compute_gs_counterfactuals_firstll(
      results$gs_wta$main_fit, gs_wta_same$events,
      paste0(restriction_label, " GS-WTA"))
  }
  r_unw_any_w  <- estimate_gs_firstll(gs_wta_any$matches, label = paste0(restriction_label, " GS-WTA unw/any"))
  r_wt_any_w   <- estimate_gs_firstll(gs_wta_any$matches, label = paste0(restriction_label, " GS-WTA wt/any"), weighted = TRUE)
  results$gs_wta$robustness <- list(
    unw_any  = extract_robust(r_unw_any_w),
    unw_same = extract_robust(results$gs_wta$main_fit),
    wt_any   = extract_robust(r_wt_any_w),
    wt_same  = extract_robust(results$gs_wta$main_wt)
  )

  # ----- nonGS-ATP -----
  message(sprintf("\n  === %s: nonGS-ATP ===", restriction_label))
  nongs_atp_same <- dataset_list$nongs_atp_same
  nongs_atp_any  <- dataset_list$nongs_atp_any

  results$nongs_atp <- list(
    main_fit = estimate_nongs_firstll(nongs_atp_same$matches, nongs_atp_same$events,
                                       label = paste0(restriction_label, " nonGS-ATP main")),
    main_wt  = estimate_nongs_firstll(nongs_atp_same$matches, nongs_atp_same$events,
                                       weighted = TRUE,
                                       label = paste0(restriction_label, " nonGS-ATP main_wt")),
    cf = NULL, robustness = list(), events = nongs_atp_same$events
  )

  # Robustness
  r_ua <- estimate_nongs_firstll(nongs_atp_any$matches, nongs_atp_any$events,
                                  label = paste0(restriction_label, " nonGS-ATP unw/any"))
  r_wa <- estimate_nongs_firstll(nongs_atp_any$matches, nongs_atp_any$events,
                                  weighted = TRUE,
                                  label = paste0(restriction_label, " nonGS-ATP wt/any"))
  results$nongs_atp$robustness <- list(
    unw_any  = extract_robust(r_ua),
    unw_same = extract_robust(results$nongs_atp$main_fit),
    wt_any   = extract_robust(r_wa),
    wt_same  = extract_robust(results$nongs_atp$main_wt)
  )

  # ----- nonGS-WTA -----
  message(sprintf("\n  === %s: nonGS-WTA ===", restriction_label))
  nongs_wta_same <- dataset_list$nongs_wta_same
  nongs_wta_any  <- dataset_list$nongs_wta_any

  results$nongs_wta <- list(
    main_fit = estimate_nongs_firstll(nongs_wta_same$matches, nongs_wta_same$events,
                                       label = paste0(restriction_label, " nonGS-WTA main")),
    main_wt  = estimate_nongs_firstll(nongs_wta_same$matches, nongs_wta_same$events,
                                       weighted = TRUE,
                                       label = paste0(restriction_label, " nonGS-WTA main_wt")),
    cf = NULL, robustness = list(), events = nongs_wta_same$events
  )

  r_ua_w <- estimate_nongs_firstll(nongs_wta_any$matches, nongs_wta_any$events,
                                    label = paste0(restriction_label, " nonGS-WTA unw/any"))
  r_wa_w <- estimate_nongs_firstll(nongs_wta_any$matches, nongs_wta_any$events,
                                    weighted = TRUE,
                                    label = paste0(restriction_label, " nonGS-WTA wt/any"))
  results$nongs_wta$robustness <- list(
    unw_any  = extract_robust(r_ua_w),
    unw_same = extract_robust(results$nongs_wta$main_fit),
    wt_any   = extract_robust(r_wa_w),
    wt_same  = extract_robust(results$nongs_wta$main_wt)
  )

  results
}

message("  --- FIRSTLL (type-specific) restriction ---")
firstll_results <- run_all_estimates(firstll, "firstll")

message("\n  --- ANYTYPE restriction ---")
anytype_results <- run_all_estimates(anytype, "anytype")


###############################################################################
# PHASE 8: HORIZON HETEROGENEITY FOR FIRST-LL
###############################################################################

message("\n[8] Estimating horizon heterogeneity for first-LL samples...")

estimate_gs_horizon_firstll <- function(match_df, label = "") {
  if (is.null(match_df) || nrow(match_df) < 30) {
    message("    ", label, ": insufficient observations")
    return(NULL)
  }

  df <- impute_match_data(match_df)

  fml <- won ~ D_4w + D_8w + D_12w + D_26w + D_52w +
    log_rank_ratio + log_rank_ratio_sq + rank_diff +
    same_ioc + is_clay + is_grass + age_diff + height_diff +
    hand_mismatch + h2h_smoothed + n_h2h +
    pre_elo + opp_elo + pre_rank_pts +
    player_age_at_event

  model <- tryCatch(
    glm(fml, family = binomial(link = "logit"), data = df,
        weights = df$points_weight),
    error = function(e) { message("    ", label, " GLM error: ", e$message); NULL }
  )
  if (is.null(model) || !model$converged) {
    message("    ", label, ": model did not converge")
    return(NULL)
  }

  V_full <- vcovCL(model, cluster = df$focal_pid, type = "HC1")
  se_cl <- sqrt(diag(V_full))

  horizon_names <- c("D_4w", "D_8w", "D_12w", "D_26w", "D_52w")
  deltas    <- coef(model)[horizon_names]
  delta_ses <- se_cl[horizon_names]
  delta_ps  <- 2 * pnorm(-abs(deltas / delta_ses))

  V_delta <- V_full[horizon_names, horizon_names]
  cum <- compute_cumulative_from_vcov(unname(deltas), V_delta)

  n_matches <- nrow(df)
  n_events  <- n_distinct(df$event_id)

  message(sprintf("    %s: N=%d, events=%d", label, n_matches, n_events))

  list(deltas = unname(deltas), delta_ses = unname(delta_ses), delta_ps = unname(delta_ps),
       cum_totals = cum$totals, cum_ses = cum$ses, cum_ps = cum$ps, cum_ors = cum$ors,
       horizon_names = horizon_names, n_matches = n_matches, n_events = n_events,
       rhos = rep(NA_real_, 5), rho_ps = rep(NA_real_, 5), rho_ses = rep(NA_real_, 5))
}

estimate_nongs_horizon_firstll <- function(match_df, ev_table, n_boot = N_BOOT, label = "") {
  if (is.null(match_df) || nrow(match_df) < 50) {
    message("    ", label, ": insufficient observations")
    return(NULL)
  }

  df <- impute_match_data(match_df)

  fml <- won ~ D_4w + D_8w + D_12w + D_26w + D_52w +
    v_4w + v_8w + v_12w + v_26w + v_52w +
    log_rank_ratio + log_rank_ratio_sq + rank_diff +
    same_ioc + is_clay + is_grass + age_diff + height_diff +
    hand_mismatch + h2h_smoothed + n_h2h +
    pre_elo + opp_elo + pre_rank_pts +
    player_age_at_event

  model <- tryCatch(
    glm(fml, family = binomial(link = "logit"), data = df,
        weights = df$points_weight),
    error = function(e) { message("    ", label, " GLM error: ", e$message); NULL }
  )
  if (is.null(model) || !model$converged) {
    message("    ", label, ": model did not converge")
    return(NULL)
  }

  s <- summary(model)$coefficients
  horizon_names <- c("D_4w", "D_8w", "D_12w", "D_26w", "D_52w")
  v_names       <- c("v_4w", "v_8w", "v_12w", "v_26w", "v_52w")

  delta_pts <- s[horizon_names, "Estimate"]
  rho_pts   <- s[v_names, "Estimate"]

  n_matches <- nrow(df)
  n_events  <- n_distinct(df$event_id)

  players <- unique(ev_table$player_id)
  n_pl <- length(players)
  ev_by_player <- split(ev_table, ev_table$player_id)

  boot_deltas <- matrix(NA_real_, nrow = 0, ncol = 5)
  boot_rhos   <- matrix(NA_real_, nrow = 0, ncol = 5)
  t_start <- Sys.time()

  for (b in seq_len(n_boot)) {
    if (b %% 50 == 0) {
      elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
      message(sprintf("      %s: boot %d/%d (%.1f min)", label, b, n_boot, elapsed))
    }
    tryCatch({
      boot_players <- sample(players, n_pl, replace = TRUE)
      boot_ev <- bind_rows(lapply(boot_players, function(p) ev_by_player[[as.character(p)]]))
      if (nrow(boot_ev) < 20) next
      boot_md <- df |> filter(event_id %in% boot_ev$event_id)
      if (nrow(boot_md) < 50) next

      q <- qnorm(boot_ev$p_ll)
      phi_q <- dnorm(q)
      boot_ev$v_hat <- boot_ev$got_ll * phi_q / boot_ev$p_ll -
        (1 - boot_ev$got_ll) * phi_q / (1 - boot_ev$p_ll)
      vhat_lookup <- boot_ev |> select(event_id, v_hat_boot = v_hat)
      boot_md <- boot_md |>
        left_join(vhat_lookup, by = "event_id") |>
        mutate(v_hat = coalesce(v_hat_boot, v_hat)) |>
        select(-v_hat_boot)
      boot_md <- boot_md |>
        mutate(
          v_4w  = v_hat * as.numeric(weeks_after_event <= 4),
          v_8w  = v_hat * as.numeric(weeks_after_event <= 8),
          v_12w = v_hat * as.numeric(weeks_after_event <= 12),
          v_26w = v_hat * as.numeric(weeks_after_event <= 26),
          v_52w = v_hat * as.numeric(weeks_after_event <= 52)
        )

      fit_b <- glm(fml, family = binomial(link = "logit"), data = boot_md,
                    weights = boot_md$points_weight)
      if (!fit_b$converged) next
      b_coefs <- coef(fit_b)
      if (all(horizon_names %in% names(b_coefs)) && all(v_names %in% names(b_coefs))) {
        boot_deltas <- rbind(boot_deltas, unname(b_coefs[horizon_names]))
        boot_rhos   <- rbind(boot_rhos,   unname(b_coefs[v_names]))
      }
    }, error = function(e) NULL)
  }

  elapsed_total <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))

  if (nrow(boot_deltas) < 10) {
    delta_ses <- s[horizon_names, "Std. Error"]
    rho_ses   <- s[v_names, "Std. Error"]
  } else {
    delta_ses <- apply(boot_deltas, 2, sd, na.rm = TRUE)
    rho_ses   <- apply(boot_rhos, 2, sd, na.rm = TRUE)
  }

  delta_ps <- 2 * pnorm(-abs(delta_pts / delta_ses))
  rho_ps   <- 2 * pnorm(-abs(rho_pts / rho_ses))

  # Compute cumulative from bootstrap
  if (nrow(boot_deltas) >= 10) {
    cum_boot <- apply(boot_deltas, 1, function(row) {
      n <- length(row)
      sapply(1:n, function(i) sum(row[i:n]))
    }) |> t()
    cum_totals <- colMeans(cum_boot, na.rm = TRUE)
    cum_ses    <- apply(cum_boot, 2, sd, na.rm = TRUE)
    cum_ps     <- 2 * pnorm(-abs(cum_totals / cum_ses))
  } else {
    cum_totals <- sapply(1:5, function(i) sum(delta_pts[i:5]))
    cum_ses    <- rep(NA_real_, 5)
    cum_ps     <- rep(NA_real_, 5)
  }

  message(sprintf("    %s: N=%d, events=%d [%.1f min]", label, n_matches, n_events, elapsed_total))

  list(deltas = unname(delta_pts), delta_ses = unname(delta_ses), delta_ps = unname(delta_ps),
       cum_totals = cum_totals, cum_ses = cum_ses, cum_ps = cum_ps,
       cum_ors = exp(cum_totals),
       horizon_names = horizon_names, n_matches = n_matches, n_events = n_events,
       rhos = unname(rho_pts), rho_ps = unname(rho_ps), rho_ses = unname(rho_ses))
}

# Run horizon estimation for both restriction types
run_horizon_estimates <- function(dataset_list, restriction_label) {
  horizon_results <- list()

  # GS-ATP
  message(sprintf("\n  === %s: GS-ATP horizon ===", restriction_label))
  horizon_results$gs_atp <- list(
    wt_any  = estimate_gs_horizon_firstll(dataset_list$gs_atp_any$matches,
                                           label = paste0(restriction_label, " GS-ATP wt/any")),
    wt_same = estimate_gs_horizon_firstll(dataset_list$gs_atp_same$matches,
                                           label = paste0(restriction_label, " GS-ATP wt/same"))
  )

  # GS-WTA
  message(sprintf("\n  === %s: GS-WTA horizon ===", restriction_label))
  horizon_results$gs_wta <- list(
    wt_any  = estimate_gs_horizon_firstll(dataset_list$gs_wta_any$matches,
                                           label = paste0(restriction_label, " GS-WTA wt/any")),
    wt_same = estimate_gs_horizon_firstll(dataset_list$gs_wta_same$matches,
                                           label = paste0(restriction_label, " GS-WTA wt/same"))
  )

  # nonGS-ATP
  message(sprintf("\n  === %s: nonGS-ATP horizon ===", restriction_label))
  horizon_results$nongs_atp <- list(
    wt_any  = estimate_nongs_horizon_firstll(dataset_list$nongs_atp_any$matches,
                                              dataset_list$nongs_atp_any$events,
                                              label = paste0(restriction_label, " nonGS-ATP wt/any")),
    wt_same = estimate_nongs_horizon_firstll(dataset_list$nongs_atp_same$matches,
                                              dataset_list$nongs_atp_same$events,
                                              label = paste0(restriction_label, " nonGS-ATP wt/same"))
  )

  # nonGS-WTA
  message(sprintf("\n  === %s: nonGS-WTA horizon ===", restriction_label))
  horizon_results$nongs_wta <- list(
    wt_any  = estimate_nongs_horizon_firstll(dataset_list$nongs_wta_any$matches,
                                              dataset_list$nongs_wta_any$events,
                                              label = paste0(restriction_label, " nonGS-WTA wt/any")),
    wt_same = estimate_nongs_horizon_firstll(dataset_list$nongs_wta_same$matches,
                                              dataset_list$nongs_wta_same$events,
                                              label = paste0(restriction_label, " nonGS-WTA wt/same"))
  )

  horizon_results
}

firstll_horizon <- run_horizon_estimates(firstll, "firstll")
anytype_horizon <- run_horizon_estimates(anytype, "anytype")


###############################################################################
# PHASE 9: TREATMENT DOSE ANALYSIS
###############################################################################

message("\n[9] Treatment dose analysis...")

# Compute matches_won at the LL event for each event_id
# For treated: count main draw wins at the LL-granting tournament
# For control: matches_won = 0
compute_matches_won <- function(ev_table, all_matches, tour_label) {
  # Find main draw wins by LL players at their LL tournament
  md <- all_matches |> filter(tour == tour_label, match_source == "main")

  ev_ll <- ev_table |> filter(got_ll == 1)
  if (nrow(ev_ll) == 0) {
    ev_table$matches_won <- 0L
    ev_table$matches_won_sq <- 0
    return(ev_table)
  }

  # For each LL event, count wins at that tournament
  ll_wins <- ev_ll |>
    rowwise() |>
    mutate(
      matches_won = {
        wins <- md |>
          filter(winner_pid == player_pid,
                 tourney_id == .data$tourney_id) |>
          nrow()
        as.integer(wins)
      }
    ) |>
    ungroup() |>
    select(event_id, matches_won)

  ev_table <- ev_table |>
    left_join(ll_wins, by = "event_id") |>
    mutate(
      matches_won = replace_na(matches_won, 0L),
      matches_won_sq = matches_won^2
    )

  ev_table
}

# Add matches_won to event tables and propagate to match data
add_dose_to_matches <- function(ev_table, md_table, all_matches, tour_label) {
  ev_table <- compute_matches_won(ev_table, all_matches, tour_label)
  dose_lookup <- ev_table |> select(event_id, matches_won, matches_won_sq)
  md_table <- md_table |>
    left_join(dose_lookup, by = "event_id") |>
    mutate(
      matches_won = replace_na(matches_won, 0L),
      matches_won_sq = replace_na(matches_won_sq, 0),
      ll_x_mw   = got_ll * matches_won,
      ll_x_mw_sq = got_ll * matches_won_sq
    )
  list(events = ev_table, matches = md_table)
}

# Dose estimation: GS
estimate_gs_dose <- function(match_df, label = "") {
  if (is.null(match_df) || nrow(match_df) < 30) {
    message("    ", label, ": insufficient observations")
    return(NULL)
  }

  df <- impute_match_data(match_df)

  fml <- won ~ got_ll + ll_x_mw + ll_x_mw_sq +
    log_rank_ratio + log_rank_ratio_sq + rank_diff +
    same_ioc + is_clay + is_grass + age_diff + height_diff +
    hand_mismatch + h2h_smoothed + n_h2h +
    pre_elo + opp_elo + pre_rank_pts +
    player_age_at_event

  model <- tryCatch(
    glm(fml, family = binomial(link = "logit"), data = df),
    error = function(e) { message("    ", label, " GLM error: ", e$message); NULL }
  )
  if (is.null(model) || !model$converged) {
    message("    ", label, ": model did not converge")
    return(NULL)
  }

  V <- vcovCL(model, cluster = df$focal_pid, type = "HC1")
  se_cl <- sqrt(diag(V))
  beta <- coef(model)

  dose_names <- c("got_ll", "ll_x_mw", "ll_x_mw_sq")
  coefs <- beta[dose_names]
  ses   <- se_cl[dose_names]
  ps    <- 2 * pnorm(-abs(coefs / ses))

  # Implied total effects at 0, 1, 2, 3 wins
  implied <- data.frame(wins = 0:3)
  for (w in 0:3) {
    total <- coefs["got_ll"] + w * coefs["ll_x_mw"] + w^2 * coefs["ll_x_mw_sq"]
    # SE via delta method: gradient = (1, w, w^2)
    grad <- c(1, w, w^2)
    V_dose <- V[dose_names, dose_names]
    se_total <- sqrt(t(grad) %*% V_dose %*% grad)
    p_total  <- 2 * pnorm(-abs(total / se_total))
    implied$total[implied$wins == w]  <- total
    implied$se[implied$wins == w]     <- se_total
    implied$p[implied$wins == w]      <- p_total
  }

  n_matches <- nrow(df)
  n_events  <- n_distinct(df$event_id)

  message(sprintf("    %s: base_delta=%.4f, dose=%.4f, dose_sq=%.4f, N=%d",
                  label, coefs[1], coefs[2], coefs[3], n_matches))

  list(coefs = unname(coefs), ses = unname(ses), ps = unname(ps),
       implied = implied, n_matches = n_matches, n_events = n_events,
       rhos = rep(NA_real_, 3), rho_ses = rep(NA_real_, 3), rho_ps = rep(NA_real_, 3))
}

# Dose estimation: nonGS (with v_hat dose interactions, bootstrap)
estimate_nongs_dose <- function(match_df, ev_table, n_boot = N_BOOT, label = "") {
  if (is.null(match_df) || nrow(match_df) < 50) {
    message("    ", label, ": insufficient observations")
    return(NULL)
  }

  df <- impute_match_data(match_df)
  # Create v_hat dose interactions
  df <- df |>
    mutate(
      v_x_mw   = v_hat * matches_won,
      v_x_mw_sq = v_hat * matches_won_sq
    )

  fml <- won ~ got_ll + ll_x_mw + ll_x_mw_sq +
    v_hat + v_x_mw + v_x_mw_sq +
    log_rank_ratio + log_rank_ratio_sq + rank_diff +
    same_ioc + is_clay + is_grass + age_diff + height_diff +
    hand_mismatch + h2h_smoothed + n_h2h +
    pre_elo + opp_elo + pre_rank_pts +
    player_age_at_event

  model <- tryCatch(
    glm(fml, family = binomial(link = "logit"), data = df),
    error = function(e) { message("    ", label, " GLM error: ", e$message); NULL }
  )
  if (is.null(model) || !model$converged) {
    message("    ", label, ": model did not converge")
    return(NULL)
  }

  s <- summary(model)$coefficients
  dose_names <- c("got_ll", "ll_x_mw", "ll_x_mw_sq")
  rho_names  <- c("v_hat", "v_x_mw", "v_x_mw_sq")

  delta_pts <- s[dose_names, "Estimate"]
  rho_pts   <- s[rho_names, "Estimate"]

  n_matches <- nrow(df)
  n_events  <- n_distinct(df$event_id)

  # Bootstrap
  players <- unique(ev_table$player_id)
  n_pl <- length(players)
  ev_by_player <- split(ev_table, ev_table$player_id)

  boot_deltas <- matrix(NA_real_, nrow = 0, ncol = 3)
  boot_rhos   <- matrix(NA_real_, nrow = 0, ncol = 3)
  t_start <- Sys.time()

  for (b in seq_len(n_boot)) {
    if (b %% 50 == 0) {
      elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
      message(sprintf("      %s: boot %d/%d (%.1f min)", label, b, n_boot, elapsed))
    }
    tryCatch({
      boot_players <- sample(players, n_pl, replace = TRUE)
      boot_ev <- bind_rows(lapply(boot_players, function(p) ev_by_player[[as.character(p)]]))
      if (nrow(boot_ev) < 20) next
      boot_md <- df |> filter(event_id %in% boot_ev$event_id)
      if (nrow(boot_md) < 50) next

      q <- qnorm(boot_ev$p_ll)
      phi_q <- dnorm(q)
      boot_ev$v_hat <- boot_ev$got_ll * phi_q / boot_ev$p_ll -
        (1 - boot_ev$got_ll) * phi_q / (1 - boot_ev$p_ll)
      vhat_lookup <- boot_ev |> select(event_id, v_hat_boot = v_hat)
      boot_md <- boot_md |>
        left_join(vhat_lookup, by = "event_id") |>
        mutate(v_hat = coalesce(v_hat_boot, v_hat)) |>
        select(-v_hat_boot) |>
        mutate(v_x_mw = v_hat * matches_won, v_x_mw_sq = v_hat * matches_won_sq)

      fit_b <- glm(fml, family = binomial(link = "logit"), data = boot_md)
      if (!fit_b$converged) next
      b_coefs <- coef(fit_b)
      if (all(dose_names %in% names(b_coefs)) && all(rho_names %in% names(b_coefs))) {
        boot_deltas <- rbind(boot_deltas, unname(b_coefs[dose_names]))
        boot_rhos   <- rbind(boot_rhos,   unname(b_coefs[rho_names]))
      }
    }, error = function(e) NULL)
  }

  elapsed_total <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))

  if (nrow(boot_deltas) < 10) {
    delta_ses <- s[dose_names, "Std. Error"]
    rho_ses   <- s[rho_names, "Std. Error"]
  } else {
    delta_ses <- apply(boot_deltas, 2, sd, na.rm = TRUE)
    rho_ses   <- apply(boot_rhos, 2, sd, na.rm = TRUE)
  }

  delta_ps <- 2 * pnorm(-abs(delta_pts / delta_ses))
  rho_ps   <- 2 * pnorm(-abs(rho_pts / rho_ses))

  # Implied total effects at 0, 1, 2, 3 wins
  implied <- data.frame(wins = 0:3)
  for (w in 0:3) {
    total <- delta_pts[1] + w * delta_pts[2] + w^2 * delta_pts[3]
    if (nrow(boot_deltas) >= 10) {
      boot_totals <- boot_deltas[, 1] + w * boot_deltas[, 2] + w^2 * boot_deltas[, 3]
      se_total <- sd(boot_totals, na.rm = TRUE)
    } else {
      se_total <- NA_real_
    }
    p_total <- if (!is.na(se_total) && se_total > 0) 2 * pnorm(-abs(total / se_total)) else NA_real_
    implied$total[implied$wins == w]  <- total
    implied$se[implied$wins == w]     <- se_total
    implied$p[implied$wins == w]      <- p_total
  }

  message(sprintf("    %s: base=%.4f, dose=%.4f, dose_sq=%.4f, N=%d [%.1f min]",
                  label, delta_pts[1], delta_pts[2], delta_pts[3], n_matches, elapsed_total))

  list(coefs = unname(delta_pts), ses = unname(delta_ses), ps = unname(delta_ps),
       implied = implied, n_matches = n_matches, n_events = n_events,
       rhos = unname(rho_pts), rho_ses = unname(rho_ses), rho_ps = unname(rho_ps))
}

# Run dose estimation for all 8 firstll samples
message("  Computing matches_won for dose analysis...")

dose_results <- list()

for (sample_key in c("gs_atp_same", "gs_wta_same", "nongs_atp_same", "nongs_wta_same")) {
  tour <- ifelse(grepl("atp", sample_key), "ATP", "WTA")
  d <- add_dose_to_matches(firstll[[sample_key]]$events,
                           firstll[[sample_key]]$matches,
                           matches, tour)
  firstll[[sample_key]]$events  <- d$events
  firstll[[sample_key]]$matches <- d$matches
}

# GS dose
message("\n  === Dose: GS-ATP ===")
dose_results$gs_atp <- estimate_gs_dose(firstll$gs_atp_same$matches,
                                         label = "dose GS-ATP")
message("\n  === Dose: GS-WTA ===")
dose_results$gs_wta <- estimate_gs_dose(firstll$gs_wta_same$matches,
                                         label = "dose GS-WTA")
message("\n  === Dose: nonGS-ATP ===")
dose_results$nongs_atp <- estimate_nongs_dose(firstll$nongs_atp_same$matches,
                                               firstll$nongs_atp_same$events,
                                               label = "dose nonGS-ATP")
message("\n  === Dose: nonGS-WTA ===")
dose_results$nongs_wta <- estimate_nongs_dose(firstll$nongs_wta_same$matches,
                                               firstll$nongs_wta_same$events,
                                               label = "dose nonGS-WTA")


###############################################################################
# PHASE 10: TABLE GENERATION
###############################################################################

message("\n[10] Generating tables...")

# -- Covariate names (no had_prior_ll) ----------------------------------------
COVAR_NAMES_GS <- c(
  "got_ll"              = "$\\hat{\\delta}$ (LL entry)",
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
  "player_age_at_event" = "Player age"
)

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
  "player_age_at_event" = "Player age"
)

# -- Main table writer (GS) ---------------------------------------------------
write_gs_main_table <- function(result, filepath, label) {
  fit <- result$main_fit
  fit_wt <- result$main_wt
  cf  <- result$cf
  ev  <- result$events

  if (is.null(fit)) { message("  ", label, ": no fit, skipping"); return() }

  model <- fit$model
  V     <- fit$vcov_cl
  beta  <- coef(model)
  se_cl <- sqrt(diag(V))

  lines <- c(
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & Estimate & SE \\\\",
    "\\midrule",
    "\\multicolumn{3}{l}{\\textit{Panel A: Logit Coefficients (First-LL Sample)}} \\\\[3pt]"
  )

  for (vname in names(COVAR_NAMES_GS)) {
    if (vname %in% names(beta)) {
      est <- beta[vname]
      se  <- se_cl[vname]
      pv  <- 2 * pnorm(-abs(est / se))
      lines <- c(lines, sprintf("%s & %s%s & (%s) \\\\",
                                COVAR_NAMES_GS[vname], fmt(est, 4), add_stars(pv), fmt(se, 4)))
    }
  }

  # Counterfactuals
  if (!is.null(cf)) {
    lines <- c(lines, "\\midrule",
      "\\multicolumn{3}{l}{\\textit{Panel B: Counterfactual Quantities}} \\\\[3pt]",
      sprintf("$\\Delta P(\\text{match win})$ & %s%s & (%s) \\\\",
              fmt(cf$avg_me, 4), add_stars(cf$p_me), fmt(cf$se_me, 4)),
      sprintf("$\\Delta E[W]$ (wins/tournament) & %s%s & (%s) \\\\",
              fmt(cf$avg_delta_ew, 4), add_stars(cf$p_dew), fmt(cf$se_dew, 4)),
      sprintf("$\\Delta E[RP]$ (ranking points) & %s%s & (%s) \\\\",
              fmt(cf$avg_delta_erp, 1), add_stars(cf$p_derp), fmt(cf$se_derp, 1))
    )
  }

  # Points-weighted
  if (!is.null(fit_wt)) {
    lines <- c(lines, "\\midrule",
      "\\multicolumn{3}{l}{\\textit{Panel C: Points-Weighted Logit}} \\\\[3pt]",
      sprintf("$\\hat{\\delta}$ (points-weighted) & %s%s & (%s) \\\\",
              fmt(fit_wt$delta, 4), add_stars(fit_wt$delta_p), fmt(fit_wt$delta_se, 4))
    )
  }

  lines <- c(lines, "\\midrule",
    sprintf("$N$ matches & \\multicolumn{2}{c}{%s} \\\\", format(fit$n_matches, big.mark = ",")),
    sprintf("$N$ player-episodes & \\multicolumn{2}{c}{%s} \\\\", format(fit$n_events, big.mark = ",")),
    sprintf("$N$ unique players & \\multicolumn{2}{c}{%s} \\\\", format(fit$n_players, big.mark = ",")),
    "\\bottomrule", "\\end{tabular}")

  writeLines(lines, filepath)
  message("  Saved: ", filepath)
}

# -- Main table writer (nonGS) ------------------------------------------------
write_nongs_main_table <- function(result, filepath, label) {
  fit <- result$main_fit
  fit_wt <- result$main_wt
  ev  <- result$events

  if (is.null(fit)) { message("  ", label, ": no fit, skipping"); return() }

  model <- fit$model
  beta  <- coef(model)
  s <- summary(model)$coefficients

  lines <- c(
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & Estimate & SE \\\\",
    "\\midrule",
    "\\multicolumn{3}{l}{\\textit{Panel A: CF Logit (First-LL Sample)}} \\\\[3pt]"
  )

  for (vname in names(COVAR_NAMES_NONGS)) {
    if (vname %in% names(beta)) {
      est <- beta[vname]
      if (vname == "got_ll") {
        se <- fit$delta_se
      } else if (vname == "v_hat") {
        se <- fit$rho_se
      } else {
        se <- s[vname, "Std. Error"]
      }
      pv <- 2 * pnorm(-abs(est / se))
      lines <- c(lines, sprintf("%s & %s%s & (%s) \\\\",
                                COVAR_NAMES_NONGS[vname], fmt(est, 4), add_stars(pv), fmt(se, 4)))
    }
  }

  # Points-weighted
  if (!is.null(fit_wt)) {
    lines <- c(lines, "\\midrule",
      "\\multicolumn{3}{l}{\\textit{Panel B: Points-Weighted CF Logit}} \\\\[3pt]",
      sprintf("$\\hat{\\delta}$ (points-weighted) & %s%s & (%s) \\\\",
              fmt(fit_wt$delta, 4), add_stars(fit_wt$delta_p), fmt(fit_wt$delta_se, 4)),
      sprintf("$\\hat{\\rho}$ (points-weighted) & %s & (%s) \\\\",
              fmt(fit_wt$rho, 4), fmt(fit_wt$rho_se, 4))
    )
  }

  lines <- c(lines, "\\midrule",
    sprintf("Endogeneity $p$-value & \\multicolumn{2}{c}{%s} \\\\", fmt(fit$rho_p, 3)),
    sprintf("$N$ matches & \\multicolumn{2}{c}{%s} \\\\", format(fit$n_matches, big.mark = ",")),
    sprintf("$N$ player-episodes & \\multicolumn{2}{c}{%s} \\\\", format(fit$n_events, big.mark = ",")),
    sprintf("$N$ unique players & \\multicolumn{2}{c}{%s} \\\\", format(fit$n_players, big.mark = ",")),
    "\\bottomrule", "\\end{tabular}")

  writeLines(lines, filepath)
  message("  Saved: ", filepath)
}

# -- Robustness table writer ---------------------------------------------------
SPEC_LABELS <- c(
  unw_any  = "Unweighted, any LL stop",
  unw_same = "Unweighted, same-type stop",
  wt_any   = "Points-weighted, any LL stop",
  wt_same  = "Points-weighted, same-type stop"
)

write_robustness_table <- function(robustness_list, filepath, is_gs = TRUE, label = "") {
  lines <- character()
  if (is_gs) {
    lines <- c(lines,
      "\\begin{tabular}{lrrrrrr}",
      "\\toprule",
      "Specification & $N_{\\text{matches}}$ & $N_{\\text{events}}$ & $\\hat{\\delta}$ & SE & OR \\\\",
      "\\midrule")
  } else {
    lines <- c(lines,
      "\\begin{tabular}{lrrrrrrr}",
      "\\toprule",
      "Specification & $N_{\\text{matches}}$ & $N_{\\text{events}}$ & $\\hat{\\delta}$ & SE & OR & $\\hat{\\rho}$ & Endog.\\ $p$ \\\\",
      "\\midrule")
  }

  for (spec_key in names(SPEC_LABELS)) {
    r <- robustness_list[[spec_key]]
    if (is.null(r) || is.na(r$delta)) {
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
      lines <- c(lines, sprintf("%s & %s & %s & %s & %s & %s \\\\",
                                SPEC_LABELS[spec_key], n_match_str, n_event_str,
                                delta_str, se_str, or_str))
    } else {
      rho_str  <- if (!is.na(r$rho)) fmt(r$rho, 4) else "---"
      endp_str <- if (!is.na(r$rho_p)) fmt(r$rho_p, 3) else "---"
      lines <- c(lines, sprintf("%s & %s & %s & %s & %s & %s & %s & %s \\\\",
                                SPEC_LABELS[spec_key], n_match_str, n_event_str,
                                delta_str, se_str, or_str, rho_str, endp_str))
    }
  }

  lines <- c(lines, "\\bottomrule", "\\end{tabular}")
  writeLines(lines, filepath)
  message("  Saved: ", filepath)
}

# -- Horizon table writer (with cumulative effects) ----------------------------
HORIZON_LABELS_INCR <- c(
  "$t \\leq 4$w", "$4\\text{w} < t \\leq 8$w", "$8\\text{w} < t \\leq 12$w",
  "$12\\text{w} < t \\leq 26$w", "$26\\text{w} < t \\leq 52$w"
)

write_horizon_table <- function(results_list, filepath, is_gs = TRUE, label = "") {
  fmt_coef_cell <- function(val, pval) {
    if (is.na(val)) return("---")
    paste0(fmt(val, 3), add_stars(pval))
  }
  fmt_se_cell <- function(val) {
    if (is.na(val)) return("---")
    paste0("(", fmt(val, 3), ")")
  }
  fmt_or <- function(val) {
    if (is.na(val)) return("---")
    fmt(val, 3)
  }

  SPEC_LABS <- c(wt_any = "Any LL stop", wt_same = "Same-type stop")
  lines <- c()

  for (spec_key in c("wt_any", "wt_same")) {
    r <- results_list[[spec_key]]

    if (spec_key == "wt_any") {
      lines <- c(lines, "\\begin{tabular}{lccccc}", "\\toprule")
    }

    lines <- c(lines,
      sprintf("\\multicolumn{6}{l}{\\textbf{Panel %s: Points-weighted, %s}} \\\\",
              ifelse(spec_key == "wt_any", "A", "B"), tolower(SPEC_LABS[spec_key])),
      "\\midrule",
      " & $\\hat{\\delta}$ (incr.) & SE & Total effect & SE & OR (total) \\\\",
      "\\midrule")

    if (is.null(r)) {
      lines <- c(lines, "\\multicolumn{6}{c}{Estimation failed or insufficient data} \\\\")
    } else {
      lines <- c(lines, "\\multicolumn{6}{l}{\\textit{LL treatment effect by horizon}} \\\\")
      for (i in 1:5) {
        incr_str  <- fmt_coef_cell(r$deltas[i], r$delta_ps[i])
        incr_se   <- fmt_se_cell(r$delta_ses[i])
        total_str <- if (!is.null(r$cum_totals)) fmt_coef_cell(r$cum_totals[i], r$cum_ps[i]) else "---"
        total_se  <- if (!is.null(r$cum_ses))    fmt_se_cell(r$cum_ses[i]) else "---"
        or_str    <- if (!is.null(r$cum_ors))    fmt_or(r$cum_ors[i]) else "---"
        lines <- c(lines, sprintf("%s & %s & %s & %s & %s & %s \\\\",
                                  HORIZON_LABELS_INCR[i], incr_str, incr_se,
                                  total_str, total_se, or_str))
      }

      if (!is_gs && !all(is.na(r$rhos))) {
        V_LABELS <- c("$\\hat{\\rho}_{4w}$", "$\\hat{\\rho}_{8w}$",
                       "$\\hat{\\rho}_{12w}$", "$\\hat{\\rho}_{26w}$",
                       "$\\hat{\\rho}_{52w}$")
        lines <- c(lines, "[4pt]",
          "\\multicolumn{6}{l}{\\textit{Selection control by horizon}} \\\\")
        for (i in 1:5) {
          rho_str <- fmt_coef_cell(r$rhos[i], r$rho_ps[i])
          rse_str <- if (!is.null(r$rho_ses)) fmt_se_cell(r$rho_ses[i]) else "---"
          lines <- c(lines, sprintf("%s & %s & %s & & & \\\\", V_LABELS[i], rho_str, rse_str))
        }
      }

      lines <- c(lines, "[4pt]",
        sprintf("$N_{\\text{matches}}$ & \\multicolumn{5}{l}{%s} \\\\", format(r$n_matches, big.mark = ",")),
        sprintf("$N_{\\text{events}}$ & \\multicolumn{5}{l}{%s} \\\\", format(r$n_events, big.mark = ",")))
    }

    if (spec_key == "wt_any") {
      lines <- c(lines, "\\midrule")
    } else {
      lines <- c(lines, "\\bottomrule", "\\end{tabular}")
    }
  }

  writeLines(lines, filepath)
  message("  Saved: ", filepath)
}

# -- Dose table writer ---------------------------------------------------------
write_dose_table <- function(dose_result, filepath, is_gs = TRUE, label = "") {
  if (is.null(dose_result)) {
    message("  ", label, ": no dose result, skipping")
    return()
  }

  lines <- c(
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & Estimate & SE \\\\",
    "\\midrule",
    "\\multicolumn{3}{l}{\\textit{Panel A: Dose-Response Coefficients}} \\\\[3pt]"
  )

  dose_labels <- c("$\\hat{\\delta}$ (base LL effect)",
                    "$\\hat{\\delta}_{\\text{dose}}$ (LL $\\times$ wins)",
                    "$\\hat{\\delta}_{\\text{dose}^2}$ (LL $\\times$ wins$^2$)")

  for (i in 1:3) {
    lines <- c(lines, sprintf("%s & %s%s & (%s) \\\\",
                              dose_labels[i],
                              fmt(dose_result$coefs[i], 4),
                              add_stars(dose_result$ps[i]),
                              fmt(dose_result$ses[i], 4)))
  }

  if (!is_gs && !all(is.na(dose_result$rhos))) {
    rho_labels <- c("$\\hat{\\rho}$ (base)",
                     "$\\hat{\\rho}_{\\text{dose}}$",
                     "$\\hat{\\rho}_{\\text{dose}^2}$")
    lines <- c(lines, "[4pt]")
    for (i in 1:3) {
      lines <- c(lines, sprintf("%s & %s%s & (%s) \\\\",
                                rho_labels[i],
                                fmt(dose_result$rhos[i], 4),
                                add_stars(dose_result$rho_ps[i]),
                                fmt(dose_result$rho_ses[i], 4)))
    }
  }

  # Implied total effects
  lines <- c(lines, "\\midrule",
    "\\multicolumn{3}{l}{\\textit{Panel B: Implied Total Effect by Wins at LL Event}} \\\\[3pt]")

  implied <- dose_result$implied
  for (j in seq_len(nrow(implied))) {
    w <- implied$wins[j]
    se_str <- if (!is.na(implied$se[j])) paste0("(", fmt(implied$se[j], 4), ")") else "---"
    lines <- c(lines, sprintf("At %d win%s & %s%s & %s \\\\",
                              w, ifelse(w == 1, "", "s"),
                              fmt(implied$total[j], 4),
                              add_stars(implied$p[j]),
                              se_str))
  }

  lines <- c(lines, "\\midrule",
    sprintf("$N$ matches & \\multicolumn{2}{c}{%s} \\\\", format(dose_result$n_matches, big.mark = ",")),
    sprintf("$N$ events & \\multicolumn{2}{c}{%s} \\\\", format(dose_result$n_events, big.mark = ",")),
    "\\bottomrule", "\\end{tabular}")

  writeLines(lines, filepath)
  message("  Saved: ", filepath)
}


# ---- Write all tables --------------------------------------------------------

# FIRSTLL (type-specific) tables
write_tables_for_restriction <- function(results, horizon_results, table_prefix) {
  # Main tables
  write_gs_main_table(results$gs_atp,
                      file.path(TABLE_DIR, paste0(table_prefix, "_gs_atp.tex")), "GS-ATP")
  write_gs_main_table(results$gs_wta,
                      file.path(TABLE_DIR, paste0(table_prefix, "_gs_wta.tex")), "GS-WTA")
  write_nongs_main_table(results$nongs_atp,
                         file.path(TABLE_DIR, paste0(table_prefix, "_nongs_atp.tex")), "nonGS-ATP")
  write_nongs_main_table(results$nongs_wta,
                         file.path(TABLE_DIR, paste0(table_prefix, "_nongs_wta.tex")), "nonGS-WTA")

  # Robustness tables
  write_robustness_table(results$gs_atp$robustness,
                         file.path(TABLE_DIR, paste0(table_prefix, "_robust_gs_atp.tex")),
                         is_gs = TRUE, label = "GS-ATP")
  write_robustness_table(results$gs_wta$robustness,
                         file.path(TABLE_DIR, paste0(table_prefix, "_robust_gs_wta.tex")),
                         is_gs = TRUE, label = "GS-WTA")
  write_robustness_table(results$nongs_atp$robustness,
                         file.path(TABLE_DIR, paste0(table_prefix, "_robust_nongs_atp.tex")),
                         is_gs = FALSE, label = "nonGS-ATP")
  write_robustness_table(results$nongs_wta$robustness,
                         file.path(TABLE_DIR, paste0(table_prefix, "_robust_nongs_wta.tex")),
                         is_gs = FALSE, label = "nonGS-WTA")

  # Horizon tables
  write_horizon_table(horizon_results$gs_atp,
                      file.path(TABLE_DIR, paste0(table_prefix, "_horizon_gs_atp.tex")),
                      is_gs = TRUE, label = "GS-ATP")
  write_horizon_table(horizon_results$gs_wta,
                      file.path(TABLE_DIR, paste0(table_prefix, "_horizon_gs_wta.tex")),
                      is_gs = TRUE, label = "GS-WTA")
  write_horizon_table(horizon_results$nongs_atp,
                      file.path(TABLE_DIR, paste0(table_prefix, "_horizon_nongs_atp.tex")),
                      is_gs = FALSE, label = "nonGS-ATP")
  write_horizon_table(horizon_results$nongs_wta,
                      file.path(TABLE_DIR, paste0(table_prefix, "_horizon_nongs_wta.tex")),
                      is_gs = FALSE, label = "nonGS-WTA")
}

write_tables_for_restriction(firstll_results, firstll_horizon, "table_tournament_firstll")
write_tables_for_restriction(anytype_results, anytype_horizon, "table_tournament_anytype")

# Dose tables
write_dose_table(dose_results$gs_atp,
                 file.path(TABLE_DIR, "table_tournament_dose_gs_atp.tex"),
                 is_gs = TRUE, label = "dose GS-ATP")
write_dose_table(dose_results$gs_wta,
                 file.path(TABLE_DIR, "table_tournament_dose_gs_wta.tex"),
                 is_gs = TRUE, label = "dose GS-WTA")
write_dose_table(dose_results$nongs_atp,
                 file.path(TABLE_DIR, "table_tournament_dose_nongs_atp.tex"),
                 is_gs = FALSE, label = "dose nonGS-ATP")
write_dose_table(dose_results$nongs_wta,
                 file.path(TABLE_DIR, "table_tournament_dose_nongs_wta.tex"),
                 is_gs = FALSE, label = "dose nonGS-WTA")


###############################################################################
# PHASE 11: DOSE x HORIZON (FALLBACK TO NON-HORIZON IF CONVERGENCE ISSUES)
###############################################################################

message("\n[11] Attempting dose x horizon estimation (GS only, as proof of concept)...")

# Only attempt for GS (simpler model, no bootstrap needed)
dose_horizon_results <- list()

for (sample_key in c("gs_atp_same", "gs_wta_same")) {
  tour_label <- ifelse(grepl("atp", sample_key), "ATP", "WTA")
  sample_label <- paste0("GS-", toupper(tour_label))
  md <- firstll[[sample_key]]$matches

  if (is.null(md) || nrow(md) < 30) {
    message("  ", sample_label, ": skipping dose x horizon (insufficient data)")
    dose_horizon_results[[sample_key]] <- NULL
    next
  }

  df <- impute_match_data(md)

  # Create dose x horizon interactions
  df <- df |>
    mutate(
      D_4w_mw  = D_4w * matches_won,  D_4w_mw_sq  = D_4w * matches_won_sq,
      D_8w_mw  = D_8w * matches_won,  D_8w_mw_sq  = D_8w * matches_won_sq,
      D_12w_mw = D_12w * matches_won, D_12w_mw_sq = D_12w * matches_won_sq,
      D_26w_mw = D_26w * matches_won, D_26w_mw_sq = D_26w * matches_won_sq,
      D_52w_mw = D_52w * matches_won, D_52w_mw_sq = D_52w * matches_won_sq
    )

  fml_dose_horizon <- won ~
    D_4w + D_8w + D_12w + D_26w + D_52w +
    D_4w_mw + D_8w_mw + D_12w_mw + D_26w_mw + D_52w_mw +
    D_4w_mw_sq + D_8w_mw_sq + D_12w_mw_sq + D_26w_mw_sq + D_52w_mw_sq +
    log_rank_ratio + log_rank_ratio_sq + rank_diff +
    same_ioc + is_clay + is_grass + age_diff + height_diff +
    hand_mismatch + h2h_smoothed + n_h2h +
    pre_elo + opp_elo + pre_rank_pts +
    player_age_at_event

  model <- tryCatch(
    glm(fml_dose_horizon, family = binomial(link = "logit"), data = df,
        weights = df$points_weight),
    error = function(e) { message("  ", sample_label, " dose x horizon error: ", e$message); NULL }
  )

  if (is.null(model) || !model$converged) {
    message("  ", sample_label, ": dose x horizon did not converge -- falling back to simple dose")
    dose_horizon_results[[sample_key]] <- NULL
  } else {
    V <- vcovCL(model, cluster = df$focal_pid, type = "HC1")
    se_cl <- sqrt(diag(V))
    message("  ", sample_label, ": dose x horizon converged, N=", nrow(df))
    dose_horizon_results[[sample_key]] <- list(
      model = model, vcov = V,
      coefs = coef(model), ses = se_cl, n = nrow(df)
    )
  }
}

# NonGS dose x horizon: skip (too complex with v_hat interactions + bootstrap)
message("  nonGS dose x horizon: skipped (convergence risk with triple interactions)")


###############################################################################
# PHASE 12: SAVE DATA AND SUMMARY
###############################################################################

message("\n[12] Saving results...")

# Save firstll results
firstll_save <- list(
  firstll = firstll_results,
  anytype = anytype_results,
  firstll_horizon = firstll_horizon,
  anytype_horizon = anytype_horizon,
  sample_counts = list(
    firstll = sapply(firstll, function(x) c(events = nrow(x$events), matches = nrow(x$matches))),
    anytype = sapply(anytype, function(x) c(events = nrow(x$events), matches = nrow(x$matches)))
  )
)
saveRDS(firstll_save, file.path(CLEANED_DIR, "tournament_firstll_results.rds"))
message("  Saved: Data/cleaned/tournament_firstll_results.rds")

# Save dose results
dose_save <- list(
  dose = dose_results,
  dose_horizon = dose_horizon_results
)
saveRDS(dose_save, file.path(CLEANED_DIR, "tournament_dose_results.rds"))
message("  Saved: Data/cleaned/tournament_dose_results.rds")

# Write summary
summary_text <- c(
  "# Tournament First-LL + Dose Analysis Summary (Script 28)",
  paste0("Generated: ", Sys.time()),
  "",
  "## Design",
  "",
  "### Part 1: First-LL Restriction",
  "Two restriction variants:",
  "- **firstll (type-specific)**: No prior LL of SAME type (GS for GS samples, nonGS for nonGS)",
  "- **anytype**: No prior LL of ANY type (had_prior_ll == 0)",
  "",
  "For each of 8 samples (4 subsamples x 2 truncation rules):",
  "- Main specification (pooled delta)",
  "- Robustness table (4 specs: unw/wt x any/same stop)",
  "- Horizon heterogeneity (5 horizons, points-weighted, cumulative effects)",
  "",
  "### Part 2: Treatment Dose",
  "Interactions: ll_entry x matches_won + ll_entry x matches_won_sq",
  "Implied total effects at 0, 1, 2, 3 wins",
  ""
)

# Add results tables to summary
for (restriction in c("firstll", "anytype")) {
  results <- if (restriction == "firstll") firstll_results else anytype_results

  summary_text <- c(summary_text,
    paste0("## ", toupper(restriction), " Results"),
    "")

  for (sample_key in c("gs_atp", "gs_wta", "nongs_atp", "nongs_wta")) {
    fit <- results[[sample_key]]$main_fit
    if (is.null(fit)) {
      summary_text <- c(summary_text,
        sprintf("### %s: No fit (insufficient data)", toupper(gsub("_", "-", sample_key))),
        "")
      next
    }
    summary_text <- c(summary_text,
      sprintf("### %s", toupper(gsub("_", "-", sample_key))),
      sprintf("- delta = %.4f (SE = %.4f, p = %.4f)", fit$delta, fit$delta_se, fit$delta_p),
      sprintf("- OR = %.3f", exp(fit$delta)),
      sprintf("- N_matches = %s, N_events = %d, N_players = %d",
              format(fit$n_matches, big.mark = ","), fit$n_events, fit$n_players),
      "")
    if (!is.na(fit$rho)) {
      summary_text <- c(summary_text,
        sprintf("- rho = %.4f (SE = %.4f, p = %.4f)", fit$rho, fit$rho_se, fit$rho_p),
        "")
    }
  }
}

# Dose results
summary_text <- c(summary_text,
  "## DOSE RESULTS (firstll type-specific sample)",
  "")

for (sample_key in c("gs_atp", "gs_wta", "nongs_atp", "nongs_wta")) {
  dr <- dose_results[[sample_key]]
  if (is.null(dr)) {
    summary_text <- c(summary_text,
      sprintf("### %s: No dose result", toupper(gsub("_", "-", sample_key))),
      "")
    next
  }
  summary_text <- c(summary_text,
    sprintf("### %s", toupper(gsub("_", "-", sample_key))),
    sprintf("- base delta = %.4f (SE = %.4f)", dr$coefs[1], dr$ses[1]),
    sprintf("- dose coef = %.4f (SE = %.4f)", dr$coefs[2], dr$ses[2]),
    sprintf("- dose_sq coef = %.4f (SE = %.4f)", dr$coefs[3], dr$ses[3]),
    "- Implied total effects:")
  for (j in seq_len(nrow(dr$implied))) {
    w <- dr$implied$wins[j]
    se_str <- if (!is.na(dr$implied$se[j])) sprintf("%.4f", dr$implied$se[j]) else "---"
    summary_text <- c(summary_text,
      sprintf("  - At %d wins: %.4f (SE = %s)", w, dr$implied$total[j], se_str))
  }
  summary_text <- c(summary_text, "")
}

# Append execution log
summary_text <- c(summary_text, "", "## Execution Log", "", summary_log)

writeLines(summary_text, file.path(OUTPUT_DIR, "tournament_firstll_summary.md"))
message("  Saved: Output/tournament_firstll_summary.md")

# -- Dose-response figure ------------------------------------------------------
message("\n[13] Generating dose-response figure...")

dose_fig_data <- data.frame()
for (sample_key in c("gs_atp", "gs_wta", "nongs_atp", "nongs_wta")) {
  dr <- dose_results[[sample_key]]
  if (is.null(dr)) next
  impl <- dr$implied
  impl$sample <- toupper(gsub("_", "-", sample_key))
  dose_fig_data <- bind_rows(dose_fig_data, impl)
}

if (nrow(dose_fig_data) > 0) {
  dose_fig_data <- dose_fig_data |>
    mutate(
      lower = total - 1.96 * se,
      upper = total + 1.96 * se
    )

  p_dose <- ggplot(dose_fig_data, aes(x = wins, y = total, color = sample)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_line(linewidth = 0.8) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15) +
    scale_x_continuous(breaks = 0:3) +
    labs(x = "Main Draw Wins at LL Event", y = "Implied Total LL Effect (logit)") +
    theme_paper() +
    theme(legend.position = "bottom")

  ggsave(file.path(FIG_DIR, "fig_dose_response.pdf"), p_dose,
         width = 8, height = 6, device = cairo_pdf)
  message("  Saved: Figures/fig_dose_response.pdf")
}

message("\n", strrep("=", 72))
message("  SCRIPT 28 COMPLETE")
message(strrep("=", 72))
message("  Tables written: 24 firstll/anytype + 4 dose = 28 total")
message("  Data saved: tournament_firstll_results.rds, tournament_dose_results.rds")
message("  Summary: Output/tournament_firstll_summary.md")
