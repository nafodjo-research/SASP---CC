# ==============================================================================
# 26_tournament_horizon_hetero.R
# Horizon-heterogeneous tournament performance specifications
#
# Purpose:  For the 2 WEIGHTED specifications (points-weighted, any LL stop;
#           points-weighted, same-type stop), replace the single delta with
#           5 horizon-specific delta_h using cumulative horizon indicators:
#             D_4w  = D_i * 1(t <= 4w)
#             D_8w  = D_i * 1(t <= 8w)
#             D_12w = D_i * 1(t <= 12w)
#             D_26w = D_i * 1(t <= 26w)
#             D_52w = D_i * 1(t <= 52w)  [= D_i for all obs]
#           Since these are nested, the coefficients capture the incremental
#           effect at each horizon band (beyond the longer-horizon base).
#
#           For GS samples: simple weighted logit with clustered SEs.
#           For nonGS samples: CF logit with 5 horizon-specific v_hat controls
#             (v_ih = v_i * 1(t <= h)), 200-rep player-level block bootstrap.
#
# Inputs:
#   Data/cleaned/tournament_match_fix_results.rds  (from script 24)
#   Data/cleaned/tournament_robustness_results.rds (from script 25, for match data)
#   Data/raw/atp_main_matches.rds, atp_qual_chall_matches.rds
#   Data/raw/wta_main_matches.rds, wta_qual_itf_matches.rds
#   Data/cleaned/tournament_elo_cache.rds
#   Data/cleaned/tournament_rebuild_results.rds (from script 22)
#
# Outputs:
#   Tables/table_tournament_horizon_gs_atp.tex
#   Tables/table_tournament_horizon_gs_wta.tex
#   Tables/table_tournament_horizon_nongs_atp.tex
#   Tables/table_tournament_horizon_nongs_wta.tex
#   Data/cleaned/tournament_horizon_results.rds
#   Output/tournament_horizon_summary.md
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
message("  TOURNAMENT HORIZON HETEROGENEITY (26_tournament_horizon_hetero.R)")
message(strrep("=", 72))


###############################################################################
# PHASE 1: LOAD DATA (reuse script 25 approach)
###############################################################################

message("\n[1] Loading data...")

R24 <- readRDS(file.path(CLEANED_DIR, "tournament_match_fix_results.rds"))
R22 <- readRDS(file.path(CLEANED_DIR, "tournament_rebuild_results.rds"))

# Extract event tables
gs_atp_ev    <- R24$gs_atp$events
gs_wta_ev    <- R24$gs_wta$events
nongs_atp_ev <- R24$nongs_atp$events
nongs_wta_ev <- R24$nongs_wta$events

# "Same-type stop" match data from script 24
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
# PHASE 2: BUILD "ANY LL STOP" MATCH DATA (same as script 25)
###############################################################################

message("\n[2] Building match data with 'any LL stop' truncation...")

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

# Harmonize matches (same logic as script 25)
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

# Build ALL events per tour for cross-type truncation
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

# Build match-level data with custom truncation (from script 25)
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

# Build "any LL stop" match data
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
# PHASE 4: ADD HORIZON INDICATORS
###############################################################################

message("\n[4] Adding cumulative horizon indicators...")

add_horizon_dummies <- function(df) {
  df <- df |>
    mutate(
      D_4w  = as.numeric(got_ll) * as.numeric(weeks_after_event <= 4),
      D_8w  = as.numeric(got_ll) * as.numeric(weeks_after_event <= 8),
      D_12w = as.numeric(got_ll) * as.numeric(weeks_after_event <= 12),
      D_26w = as.numeric(got_ll) * as.numeric(weeks_after_event <= 26),
      D_52w = as.numeric(got_ll) * as.numeric(weeks_after_event <= 52)
    )
  # For nonGS CF: also create horizon-specific v_hat controls
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

gs_atp_md_any     <- add_horizon_dummies(gs_atp_md_any)
gs_atp_md_same    <- add_horizon_dummies(gs_atp_md_same)
gs_wta_md_any     <- add_horizon_dummies(gs_wta_md_any)
gs_wta_md_same    <- add_horizon_dummies(gs_wta_md_same)
nongs_atp_md_any  <- add_horizon_dummies(nongs_atp_md_any)
nongs_atp_md_same <- add_horizon_dummies(nongs_atp_md_same)
nongs_wta_md_any  <- add_horizon_dummies(nongs_wta_md_any)
nongs_wta_md_same <- add_horizon_dummies(nongs_wta_md_same)

# Diagnostic: check variation in horizon dummies
for (nm in c("gs_atp_md_any", "gs_atp_md_same", "nongs_atp_md_any")) {
  df_tmp <- get(nm)
  msg <- sprintf("  %s: D_4w=%d, D_8w=%d, D_12w=%d, D_26w=%d, D_52w=%d (of %d rows)",
                 nm,
                 sum(df_tmp$D_4w > 0), sum(df_tmp$D_8w > 0), sum(df_tmp$D_12w > 0),
                 sum(df_tmp$D_26w > 0), sum(df_tmp$D_52w > 0), nrow(df_tmp))
  message(msg)
}

slog("## Phase 4: Horizon Indicators Added")
slog("")


###############################################################################
# PHASE 5: ESTIMATION FUNCTIONS
###############################################################################

message("\n[5] Defining horizon-heterogeneous estimation functions...")

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

# -- GS horizon estimation (weighted logit, clustered SEs) --------------------
estimate_gs_horizon <- function(match_df, label = "") {
  if (is.null(match_df) || nrow(match_df) < 30) {
    message("    ", label, ": insufficient observations (", nrow(match_df), ")")
    return(NULL)
  }

  df <- impute_match_data(match_df)

  fml <- won ~ D_4w + D_8w + D_12w + D_26w + D_52w +
    log_rank_ratio + log_rank_ratio_sq + rank_diff +
    same_ioc + is_clay + is_grass + age_diff + height_diff +
    hand_mismatch + h2h_smoothed + n_h2h +
    pre_elo + opp_elo + pre_rank_pts +
    player_age_at_event + had_prior_ll

  model <- tryCatch(
    glm(fml, family = binomial(link = "logit"), data = df,
        weights = df$points_weight),
    error = function(e) { message("    ", label, " GLM error: ", e$message); NULL }
  )
  if (is.null(model) || !model$converged) {
    message("    ", label, ": model did not converge")
    return(NULL)
  }

  V <- vcovCL(model, cluster = df$focal_pid, type = "HC1")
  se_cl <- sqrt(diag(V))

  horizon_names <- c("D_4w", "D_8w", "D_12w", "D_26w", "D_52w")
  deltas    <- coef(model)[horizon_names]
  delta_ses <- se_cl[horizon_names]
  delta_zs  <- deltas / delta_ses
  delta_ps  <- 2 * pnorm(-abs(delta_zs))

  n_matches <- nrow(df)
  n_events  <- n_distinct(df$event_id)

  message(sprintf("    %s: N=%d, events=%d", label, n_matches, n_events))
  for (h in horizon_names) {
    message(sprintf("      %s: delta=%.4f (SE=%.4f, p=%.4f)",
                    h, deltas[h], delta_ses[h], delta_ps[h]))
  }

  list(
    deltas    = unname(deltas),
    delta_ses = unname(delta_ses),
    delta_ps  = unname(delta_ps),
    horizon_names = horizon_names,
    n_matches = n_matches,
    n_events  = n_events,
    rhos      = rep(NA_real_, 5),
    rho_ps    = rep(NA_real_, 5)
  )
}

# -- NonGS horizon estimation (CF logit with horizon v_hat, bootstrap SEs) ----
estimate_nongs_horizon <- function(match_df, ev_table, n_boot = N_BOOT, label = "") {
  if (is.null(match_df) || nrow(match_df) < 50) {
    message("    ", label, ": insufficient observations (", nrow(match_df), ")")
    return(NULL)
  }

  df <- impute_match_data(match_df)

  fml <- won ~ D_4w + D_8w + D_12w + D_26w + D_52w +
    v_4w + v_8w + v_12w + v_26w + v_52w +
    log_rank_ratio + log_rank_ratio_sq + rank_diff +
    same_ioc + is_clay + is_grass + age_diff + height_diff +
    hand_mismatch + h2h_smoothed + n_h2h +
    pre_elo + opp_elo + pre_rank_pts +
    player_age_at_event + had_prior_ll

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

  # Player-level block bootstrap
  players <- unique(ev_table$player_id)
  n_players <- length(players)
  ev_by_player <- split(ev_table, ev_table$player_id)

  boot_deltas <- matrix(NA_real_, nrow = 0, ncol = 5)
  boot_rhos   <- matrix(NA_real_, nrow = 0, ncol = 5)

  t_start <- Sys.time()

  for (b in seq_len(n_boot)) {
    if (b %% 50 == 0) {
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

      # Recompute horizon-specific v_hat controls
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
    message("    ", label, ": too few bootstrap reps (", nrow(boot_deltas), ")")
    delta_ses <- s[horizon_names, "Std. Error"]
    rho_ses   <- s[v_names, "Std. Error"]
  } else {
    delta_ses <- apply(boot_deltas, 2, sd, na.rm = TRUE)
    rho_ses   <- apply(boot_rhos, 2, sd, na.rm = TRUE)
  }

  delta_ps <- 2 * pnorm(-abs(delta_pts / delta_ses))
  rho_ps   <- 2 * pnorm(-abs(rho_pts / rho_ses))

  message(sprintf("    %s: N=%d, events=%d [%.1f min, %d successful boot reps]",
                  label, n_matches, n_events, elapsed_total, nrow(boot_deltas)))
  for (i in seq_along(horizon_names)) {
    message(sprintf("      %s: delta=%.4f (bootSE=%.4f, p=%.4f), rho=%.4f (p=%.4f)",
                    horizon_names[i], delta_pts[i], delta_ses[i], delta_ps[i],
                    rho_pts[i], rho_ps[i]))
  }

  list(
    deltas    = unname(delta_pts),
    delta_ses = unname(delta_ses),
    delta_ps  = unname(delta_ps),
    horizon_names = horizon_names,
    n_matches = n_matches,
    n_events  = n_events,
    rhos      = unname(rho_pts),
    rho_ps    = unname(rho_ps),
    n_boot_success = nrow(boot_deltas)
  )
}


###############################################################################
# PHASE 6: ESTIMATE ALL SPECIFICATIONS
###############################################################################

message("\n[6] Estimating horizon-heterogeneous specifications (weighted only)...")
message("    This will take a while for non-GS samples (bootstrap)...\n")

all_results <- list()

# --- GS-ATP ---
message("  === GS-ATP ===")
all_results$gs_atp <- list(
  wt_any  = estimate_gs_horizon(gs_atp_md_any,  label = "GS-ATP wt/any"),
  wt_same = estimate_gs_horizon(gs_atp_md_same, label = "GS-ATP wt/same")
)

# --- GS-WTA ---
message("\n  === GS-WTA ===")
all_results$gs_wta <- list(
  wt_any  = estimate_gs_horizon(gs_wta_md_any,  label = "GS-WTA wt/any"),
  wt_same = estimate_gs_horizon(gs_wta_md_same, label = "GS-WTA wt/same")
)

# --- nonGS-ATP ---
message("\n  === nonGS-ATP ===")
all_results$nongs_atp <- list(
  wt_any  = estimate_nongs_horizon(nongs_atp_md_any,  nongs_atp_ev,
                                    label = "nonGS-ATP wt/any"),
  wt_same = estimate_nongs_horizon(nongs_atp_md_same, nongs_atp_ev,
                                    label = "nonGS-ATP wt/same")
)

# --- nonGS-WTA ---
message("\n  === nonGS-WTA ===")
all_results$nongs_wta <- list(
  wt_any  = estimate_nongs_horizon(nongs_wta_md_any,  nongs_wta_ev,
                                    label = "nonGS-WTA wt/any"),
  wt_same = estimate_nongs_horizon(nongs_wta_md_same, nongs_wta_ev,
                                    label = "nonGS-WTA wt/same")
)


###############################################################################
# PHASE 7: GENERATE TABLES
###############################################################################

message("\n[7] Generating horizon heterogeneity tables...")

HORIZON_LABELS <- c(
  "D_4w"  = "$\\hat{\\delta}_{4w}$",
  "D_8w"  = "$\\hat{\\delta}_{8w}$",
  "D_12w" = "$\\hat{\\delta}_{12w}$",
  "D_26w" = "$\\hat{\\delta}_{26w}$",
  "D_52w" = "$\\hat{\\delta}_{52w}$"
)

V_LABELS <- c(
  "v_4w"  = "$\\hat{\\rho}_{4w}$",
  "v_8w"  = "$\\hat{\\rho}_{8w}$",
  "v_12w" = "$\\hat{\\rho}_{12w}$",
  "v_26w" = "$\\hat{\\rho}_{26w}$",
  "v_52w" = "$\\hat{\\rho}_{52w}$"
)

SPEC_LABELS <- c(
  wt_any  = "Any LL stop",
  wt_same = "Same-type stop"
)

write_horizon_table <- function(results_list, filepath, is_gs = TRUE, label = "") {
  # Two panels (any LL stop, same-type stop), each with 5 horizon rows
  n_specs <- length(results_list)

  if (is_gs) {
    ncols <- 2  # coefficient + SE per spec
    # Columns: horizon label | (delta, SE) x 2 specs
    lines <- c(
      paste0("\\begin{tabular}{l", paste(rep("cc", n_specs), collapse = ""), "}"),
      "\\toprule",
      paste0(" & \\multicolumn{2}{c}{", SPEC_LABELS["wt_any"], "} & \\multicolumn{2}{c}{",
             SPEC_LABELS["wt_same"], "} \\\\"),
      paste0("\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}"),
      " & $\\hat{\\delta}$ & SE & $\\hat{\\delta}$ & SE \\\\",
      "\\midrule"
    )
  } else {
    # nonGS: delta rows + rho rows per spec
    lines <- c(
      paste0("\\begin{tabular}{l", paste(rep("cc", n_specs), collapse = ""), "}"),
      "\\toprule",
      paste0(" & \\multicolumn{2}{c}{", SPEC_LABELS["wt_any"], "} & \\multicolumn{2}{c}{",
             SPEC_LABELS["wt_same"], "} \\\\"),
      paste0("\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}"),
      " & Coef. & SE & Coef. & SE \\\\",
      "\\midrule"
    )
  }

  # Helper to format one coefficient cell
  fmt_coef <- function(val, pval) {
    if (is.na(val)) return("---")
    paste0(fmt(val, 4), add_stars(pval))
  }

  fmt_se <- function(val) {
    if (is.na(val)) return("---")
    paste0("(", fmt(val, 4), ")")
  }

  # Delta rows
  lines <- c(lines, "\\multicolumn{5}{l}{\\textit{LL treatment effect by horizon}} \\\\")
  for (i in 1:5) {
    h_label <- names(HORIZON_LABELS)[i]
    row_label <- HORIZON_LABELS[i]

    r_any  <- results_list$wt_any
    r_same <- results_list$wt_same

    d_any  <- if (!is.null(r_any))  fmt_coef(r_any$deltas[i], r_any$delta_ps[i])   else "---"
    se_any <- if (!is.null(r_any))  fmt_se(r_any$delta_ses[i])                       else "---"
    d_same <- if (!is.null(r_same)) fmt_coef(r_same$deltas[i], r_same$delta_ps[i]) else "---"
    se_same <- if (!is.null(r_same)) fmt_se(r_same$delta_ses[i])                     else "---"

    lines <- c(lines, sprintf("%s & %s & %s & %s & %s \\\\", row_label, d_any, se_any, d_same, se_same))
  }

  # Rho rows (nonGS only)
  if (!is_gs) {
    lines <- c(lines, "\\midrule")
    lines <- c(lines, "\\multicolumn{5}{l}{\\textit{Selection control ($\\hat{v}$) by horizon}} \\\\")
    for (i in 1:5) {
      v_label <- V_LABELS[i]

      r_any  <- results_list$wt_any
      r_same <- results_list$wt_same

      rho_any  <- if (!is.null(r_any))  fmt_coef(r_any$rhos[i], r_any$rho_ps[i])   else "---"
      rse_any  <- if (!is.null(r_any))  fmt_se(r_any$delta_ses[i])                   else "---"
      # Oops -- need rho SEs. We stored rho_ps but not rho_ses explicitly.
      # Actually we computed delta_ses for the delta and rho_ses... but we didn't store rho_ses.
      # We stored rho_ps = 2*pnorm(-|rho/rho_se|). So rho_se = |rho/qnorm(rho_p/2)|.
      # Better: store rho_ses directly. Let me fix the estimation function return.
      # Actually looking above, I do have rho_ps but not rho_ses in the return.
      # Let me add rho_ses to the return value. For now, back-compute:
      rho_se_any  <- if (!is.null(r_any) && !is.na(r_any$rhos[i]) && !is.na(r_any$rho_ps[i]) && r_any$rho_ps[i] < 1) {
        abs(r_any$rhos[i] / qnorm(r_any$rho_ps[i] / 2))
      } else { NA_real_ }

      rho_se_same <- if (!is.null(r_same) && !is.na(r_same$rhos[i]) && !is.na(r_same$rho_ps[i]) && r_same$rho_ps[i] < 1) {
        abs(r_same$rhos[i] / qnorm(r_same$rho_ps[i] / 2))
      } else { NA_real_ }

      rho_any_str  <- if (!is.null(r_any))  fmt_coef(r_any$rhos[i], r_any$rho_ps[i])   else "---"
      rse_any_str  <- fmt_se(rho_se_any)
      rho_same_str <- if (!is.null(r_same)) fmt_coef(r_same$rhos[i], r_same$rho_ps[i]) else "---"
      rse_same_str <- fmt_se(rho_se_same)

      lines <- c(lines, sprintf("%s & %s & %s & %s & %s \\\\",
                                v_label, rho_any_str, rse_any_str, rho_same_str, rse_same_str))
    }
  }

  # Footer with N
  lines <- c(lines, "\\midrule")

  r_any  <- results_list$wt_any
  r_same <- results_list$wt_same
  n_any  <- if (!is.null(r_any))  format(r_any$n_matches, big.mark = ",") else "---"
  n_same <- if (!is.null(r_same)) format(r_same$n_matches, big.mark = ",") else "---"
  ev_any  <- if (!is.null(r_any))  format(r_any$n_events, big.mark = ",") else "---"
  ev_same <- if (!is.null(r_same)) format(r_same$n_events, big.mark = ",") else "---"

  lines <- c(lines,
    sprintf("$N_{\\text{matches}}$ & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} \\\\", n_any, n_same),
    sprintf("$N_{\\text{events}}$ & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} \\\\", ev_any, ev_same)
  )

  lines <- c(lines,
    "\\bottomrule",
    "\\end{tabular}"
  )

  writeLines(lines, filepath)
  message("  Saved: ", filepath)
}

# Actually, the back-computation of rho_ses from rho_ps is fragile.
# Let me instead modify the estimation function return to include rho_ses directly.
# I'll add a post-hoc fix: store rho_ses in the results by back-computing once.

add_rho_ses <- function(res) {
  if (is.null(res)) return(NULL)
  # If rho is NA, skip
  if (all(is.na(res$rhos))) {
    res$rho_ses <- rep(NA_real_, length(res$rhos))
    return(res)
  }
  # Back-compute: rho_p = 2*pnorm(-|rho/rho_se|)
  # => qnorm(rho_p/2) = -|rho/rho_se|
  # => rho_se = |rho / qnorm(rho_p/2)|  (only when rho_p < 1)
  rho_ses <- ifelse(
    !is.na(res$rhos) & !is.na(res$rho_ps) & res$rho_ps < 1 & res$rhos != 0,
    abs(res$rhos / qnorm(res$rho_ps / 2)),
    NA_real_
  )
  res$rho_ses <- rho_ses
  res
}

# Apply to all nonGS results
for (sample_key in c("nongs_atp", "nongs_wta")) {
  for (spec_key in names(all_results[[sample_key]])) {
    all_results[[sample_key]][[spec_key]] <- add_rho_ses(all_results[[sample_key]][[spec_key]])
  }
}

# Rewrite the table function to use rho_ses directly
write_horizon_table_v2 <- function(results_list, filepath, is_gs = TRUE, label = "") {
  n_specs <- 2

  lines <- c(
    paste0("\\begin{tabular}{l", paste(rep("cc", n_specs), collapse = ""), "}"),
    "\\toprule",
    paste0(" & \\multicolumn{2}{c}{", SPEC_LABELS["wt_any"], "} & \\multicolumn{2}{c}{",
           SPEC_LABELS["wt_same"], "} \\\\"),
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
    " & Coef. & SE & Coef. & SE \\\\",
    "\\midrule"
  )

  fmt_coef_cell <- function(val, pval) {
    if (is.na(val)) return("---")
    paste0(fmt(val, 4), add_stars(pval))
  }
  fmt_se_cell <- function(val) {
    if (is.na(val)) return("---")
    paste0("(", fmt(val, 4), ")")
  }

  # Delta rows
  lines <- c(lines, "\\multicolumn{5}{l}{\\textit{LL treatment effect by horizon}} \\\\")
  for (i in 1:5) {
    row_label <- HORIZON_LABELS[i]
    r_any  <- results_list$wt_any
    r_same <- results_list$wt_same

    d_any_str  <- if (!is.null(r_any))  fmt_coef_cell(r_any$deltas[i], r_any$delta_ps[i])   else "---"
    se_any_str <- if (!is.null(r_any))  fmt_se_cell(r_any$delta_ses[i])                       else "---"
    d_same_str <- if (!is.null(r_same)) fmt_coef_cell(r_same$deltas[i], r_same$delta_ps[i]) else "---"
    se_same_str <- if (!is.null(r_same)) fmt_se_cell(r_same$delta_ses[i])                     else "---"

    lines <- c(lines, sprintf("%s & %s & %s & %s & %s \\\\",
                              row_label, d_any_str, se_any_str, d_same_str, se_same_str))
  }

  # Rho rows (nonGS only)
  if (!is_gs) {
    lines <- c(lines, "\\midrule")
    lines <- c(lines, "\\multicolumn{5}{l}{\\textit{Selection control ($\\hat{v}$) by horizon}} \\\\")
    for (i in 1:5) {
      v_label <- V_LABELS[i]
      r_any  <- results_list$wt_any
      r_same <- results_list$wt_same

      rho_any_str  <- if (!is.null(r_any))  fmt_coef_cell(r_any$rhos[i], r_any$rho_ps[i])   else "---"
      rse_any_str  <- if (!is.null(r_any))  fmt_se_cell(r_any$rho_ses[i])                     else "---"
      rho_same_str <- if (!is.null(r_same)) fmt_coef_cell(r_same$rhos[i], r_same$rho_ps[i]) else "---"
      rse_same_str <- if (!is.null(r_same)) fmt_se_cell(r_same$rho_ses[i])                     else "---"

      lines <- c(lines, sprintf("%s & %s & %s & %s & %s \\\\",
                                v_label, rho_any_str, rse_any_str, rho_same_str, rse_same_str))
    }
  }

  # Footer
  lines <- c(lines, "\\midrule")
  r_any  <- results_list$wt_any
  r_same <- results_list$wt_same
  n_any  <- if (!is.null(r_any))  format(r_any$n_matches, big.mark = ",") else "---"
  n_same <- if (!is.null(r_same)) format(r_same$n_matches, big.mark = ",") else "---"
  ev_any  <- if (!is.null(r_any))  format(r_any$n_events, big.mark = ",") else "---"
  ev_same <- if (!is.null(r_same)) format(r_same$n_events, big.mark = ",") else "---"

  lines <- c(lines,
    sprintf("$N_{\\text{matches}}$ & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} \\\\", n_any, n_same),
    sprintf("$N_{\\text{events}}$ & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} \\\\", ev_any, ev_same)
  )

  lines <- c(lines, "\\bottomrule", "\\end{tabular}")
  writeLines(lines, filepath)
  message("  Saved: ", filepath)
}

# Write all 4 tables
write_horizon_table_v2(all_results$gs_atp,
                       file.path(TABLE_DIR, "table_tournament_horizon_gs_atp.tex"),
                       is_gs = TRUE, label = "GS-ATP")
write_horizon_table_v2(all_results$gs_wta,
                       file.path(TABLE_DIR, "table_tournament_horizon_gs_wta.tex"),
                       is_gs = TRUE, label = "GS-WTA")
write_horizon_table_v2(all_results$nongs_atp,
                       file.path(TABLE_DIR, "table_tournament_horizon_nongs_atp.tex"),
                       is_gs = FALSE, label = "nonGS-ATP")
write_horizon_table_v2(all_results$nongs_wta,
                       file.path(TABLE_DIR, "table_tournament_horizon_nongs_wta.tex"),
                       is_gs = FALSE, label = "nonGS-WTA")


###############################################################################
# PHASE 8: SUMMARY
###############################################################################

message("\n[8] Writing summary...")

summary_text <- c(
  "# Tournament Horizon Heterogeneity Summary (Script 26)",
  paste0("Generated: ", Sys.time()),
  "",
  "## Design",
  "",
  "Horizon-heterogeneous tournament performance specifications.",
  "For 2 weighted specifications (points-weighted, any LL stop; points-weighted, same-type stop),",
  "the single delta is replaced by 5 cumulative horizon indicators:",
  "  D_4w = D_i * 1(t <= 4 weeks)",
  "  D_8w = D_i * 1(t <= 8 weeks)",
  "  D_12w = D_i * 1(t <= 12 weeks)",
  "  D_26w = D_i * 1(t <= 26 weeks)",
  "  D_52w = D_i * 1(t <= 52 weeks) [= D_i for all obs]",
  "",
  "Since these are nested (D_4w=1 implies D_8w=1, etc.), the coefficient on D_hw",
  "captures the INCREMENTAL effect of LL entry for matches within h weeks,",
  "beyond what is already captured by longer horizons.",
  "",
  "Total LL effect at horizon h = sum of delta_h' for all h' >= h.",
  "E.g., total effect at 4 weeks = delta_4w + delta_8w + delta_12w + delta_26w + delta_52w.",
  "",
  "GS models: weighted logit with player-clustered SEs (lottery assignment).",
  "NonGS models: CF logit with 5 horizon-specific v_hat controls, 200-rep player block bootstrap.",
  ""
)

for (sample_key in c("gs_atp", "gs_wta", "nongs_atp", "nongs_wta")) {
  sample_label <- toupper(gsub("_", "-", sample_key))
  is_gs <- grepl("^gs_", sample_key)

  summary_text <- c(summary_text,
    paste0("## ", sample_label),
    ""
  )

  for (spec_key in c("wt_any", "wt_same")) {
    spec_label <- SPEC_LABELS[spec_key]
    r <- all_results[[sample_key]][[spec_key]]

    summary_text <- c(summary_text,
      paste0("### ", spec_label),
      ""
    )

    if (is.null(r)) {
      summary_text <- c(summary_text, "Estimation failed or insufficient data.", "")
      next
    }

    summary_text <- c(summary_text,
      sprintf("N_matches = %s, N_events = %d",
              format(r$n_matches, big.mark = ","), r$n_events),
      "",
      "| Horizon | delta | SE | p-value | OR |",
      "|---|---|---|---|---|"
    )

    for (i in 1:5) {
      h_name <- c("4w", "8w", "12w", "26w", "52w")[i]
      summary_text <- c(summary_text,
        sprintf("| %s | %.4f | %.4f | %.4f | %.3f |",
                h_name, r$deltas[i], r$delta_ses[i], r$delta_ps[i], exp(r$deltas[i])))
    }

    # Cumulative effects
    summary_text <- c(summary_text,
      "",
      "Cumulative LL effects (sum from horizon h to 52w):",
      ""
    )
    for (i in 1:5) {
      h_name <- c("4w", "8w", "12w", "26w", "52w")[i]
      cum_delta <- sum(r$deltas[i:5])
      summary_text <- c(summary_text,
        sprintf("  Total at %s: %.4f (OR = %.3f)", h_name, cum_delta, exp(cum_delta)))
    }

    if (!is_gs && !all(is.na(r$rhos))) {
      summary_text <- c(summary_text,
        "",
        "| Horizon | rho | p-value |",
        "|---|---|---|"
      )
      for (i in 1:5) {
        h_name <- c("4w", "8w", "12w", "26w", "52w")[i]
        summary_text <- c(summary_text,
          sprintf("| %s | %.4f | %.4f |", h_name, r$rhos[i], r$rho_ps[i]))
      }
    }

    summary_text <- c(summary_text, "")
  }
}

# Append execution log
summary_text <- c(summary_text, "", "## Execution Log", "", summary_log)

writeLines(summary_text, file.path(OUTPUT_DIR, "tournament_horizon_summary.md"))
message("  Saved: Output/tournament_horizon_summary.md")

# Save results object
saveRDS(all_results, file.path(CLEANED_DIR, "tournament_horizon_results.rds"))
message("  Saved: Data/cleaned/tournament_horizon_results.rds")

message("\n", strrep("=", 72))
message("  DONE: 26_tournament_horizon_hetero.R")
message(strrep("=", 72), "\n")
