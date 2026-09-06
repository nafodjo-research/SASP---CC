# ==============================================================================
# 27_horizon_cumulative.R
# Add cumulative total effects to tournament horizon heterogeneity tables
#
# Purpose:  Re-estimate horizon-heterogeneous specifications from script 26,
#           extract full vcov matrices (GS) and bootstrap replicate matrices
#           (nonGS), compute cumulative total effects with proper SEs, and
#           regenerate the 4 horizon tables with incremental + cumulative panels.
#
# Inputs:
#   Data/cleaned/tournament_match_fix_results.rds  (from script 24)
#   Data/cleaned/tournament_rebuild_results.rds    (from script 22)
#   Data/cleaned/tournament_elo_cache.rds
#   Data/raw/atp_main_matches.rds, atp_qual_chall_matches.rds
#   Data/raw/wta_main_matches.rds, wta_qual_itf_matches.rds
#
# Outputs:
#   Tables/table_tournament_horizon_gs_atp.tex   (overwritten)
#   Tables/table_tournament_horizon_gs_wta.tex   (overwritten)
#   Tables/table_tournament_horizon_nongs_atp.tex (overwritten)
#   Tables/table_tournament_horizon_nongs_wta.tex (overwritten)
#   Data/cleaned/tournament_horizon_results.rds  (overwritten, now with vcov/boot)
#   Output/tournament_horizon_cumulative_summary.md
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
message("  HORIZON CUMULATIVE EFFECTS (27_horizon_cumulative.R)")
message(strrep("=", 72))


###############################################################################
# PHASE 1: LOAD DATA (reuse script 26 approach)
###############################################################################

message("\n[1] Loading data...")

R24 <- readRDS(file.path(CLEANED_DIR, "tournament_match_fix_results.rds"))
R22 <- readRDS(file.path(CLEANED_DIR, "tournament_rebuild_results.rds"))

gs_atp_ev    <- R24$gs_atp$events
gs_wta_ev    <- R24$gs_wta$events
nongs_atp_ev <- R24$nongs_atp$events
nongs_wta_ev <- R24$nongs_wta$events

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
# PHASE 2: BUILD "ANY LL STOP" MATCH DATA (same as script 26)
###############################################################################

message("\n[2] Building match data with 'any LL stop' truncation...")

atp_main <- read_rds(file.path(RAW_DIR, "atp_main_matches.rds"))
atp_qual <- read_rds(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
wta_main <- read_rds(file.path(RAW_DIR, "wta_main_matches.rds"))
wta_qual <- read_rds(file.path(RAW_DIR, "wta_qual_itf_matches.rds"))

elo_list <- readRDS(here("Data", "cleaned", "tournament_elo_cache.rds"))
message("  Loaded Elo cache: ", length(ls(elo_list$overall)), " players")

get_elo_at_date <- function(env, player, date) {
  df <- env[[player]]
  if (is.null(df)) return(1500)
  v <- df[df$date <= date, ]
  if (nrow(v) == 0) return(1500)
  tail(v$rating, 1)
}

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
# PHASE 3: POINTS WEIGHTING + HORIZON INDICATORS
###############################################################################

message("\n[3] Computing points weights and horizon indicators...")

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

# Apply to all 8 datasets
all_md <- list(
  gs_atp_any     = gs_atp_md_any,
  gs_atp_same    = gs_atp_md_same,
  gs_wta_any     = gs_wta_md_any,
  gs_wta_same    = gs_wta_md_same,
  nongs_atp_any  = nongs_atp_md_any,
  nongs_atp_same = nongs_atp_md_same,
  nongs_wta_any  = nongs_wta_md_any,
  nongs_wta_same = nongs_wta_md_same
)

for (nm in names(all_md)) {
  all_md[[nm]] <- add_points_weight(all_md[[nm]])
  all_md[[nm]] <- add_horizon_dummies(all_md[[nm]])
}

message("  Points weights and horizon indicators added to all 8 datasets")


###############################################################################
# PHASE 4: ESTIMATION FUNCTIONS (with vcov and bootstrap matrices)
###############################################################################

message("\n[4] Defining estimation functions with cumulative effect computation...")

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

# Compute cumulative totals from incremental deltas and vcov
# Since D_4w subset of D_8w etc., a match at t<=4 has ALL five dummies active.
# Total at horizon h = sum of delta_j for j in {h, h+1, ..., 52w}
#   where the ordering is 4w < 8w < 12w < 26w < 52w
# In index terms (1-indexed):
#   Total at index i = sum(deltas[i:5])
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

# -- GS horizon estimation (weighted logit, clustered SEs + full vcov) --------
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

  V_full <- vcovCL(model, cluster = df$focal_pid, type = "HC1")
  se_cl <- sqrt(diag(V_full))

  horizon_names <- c("D_4w", "D_8w", "D_12w", "D_26w", "D_52w")
  deltas    <- coef(model)[horizon_names]
  delta_ses <- se_cl[horizon_names]
  delta_zs  <- deltas / delta_ses
  delta_ps  <- 2 * pnorm(-abs(delta_zs))

  # Extract the 5x5 vcov block for the delta coefficients
  V_delta <- V_full[horizon_names, horizon_names]

  # Compute cumulative effects
  cum <- compute_cumulative_from_vcov(unname(deltas), V_delta)

  n_matches <- nrow(df)
  n_events  <- n_distinct(df$event_id)

  message(sprintf("    %s: N=%d, events=%d", label, n_matches, n_events))
  for (i in seq_along(horizon_names)) {
    message(sprintf("      %s: incr=%.4f (SE=%.4f), total=%.4f (SE=%.4f, OR=%.3f)",
                    horizon_names[i], deltas[i], delta_ses[i],
                    cum$totals[i], cum$ses[i], cum$ors[i]))
  }

  list(
    deltas     = unname(deltas),
    delta_ses  = unname(delta_ses),
    delta_ps   = unname(delta_ps),
    cum_totals = cum$totals,
    cum_ses    = cum$ses,
    cum_ps     = cum$ps,
    cum_ors    = cum$ors,
    horizon_names = horizon_names,
    n_matches  = n_matches,
    n_events   = n_events,
    rhos       = rep(NA_real_, 5),
    rho_ps     = rep(NA_real_, 5),
    rho_ses    = rep(NA_real_, 5)
  )
}

# -- NonGS horizon estimation (CF logit, bootstrap SEs for both incr and cum) -
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
  boot_cum    <- matrix(NA_real_, nrow = 0, ncol = 5)  # cumulative totals per rep

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
        b_deltas <- unname(b_coefs[horizon_names])
        boot_deltas <- rbind(boot_deltas, b_deltas)
        boot_rhos   <- rbind(boot_rhos,   unname(b_coefs[v_names]))

        # Compute cumulative totals for this bootstrap rep
        b_cum <- numeric(5)
        for (i in 1:5) b_cum[i] <- sum(b_deltas[i:5])
        boot_cum <- rbind(boot_cum, b_cum)
      }
    }, error = function(e) NULL)
  }

  elapsed_total <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))

  if (nrow(boot_deltas) < 10) {
    message("    ", label, ": too few bootstrap reps (", nrow(boot_deltas), ")")
    delta_ses <- s[horizon_names, "Std. Error"]
    rho_ses   <- s[v_names, "Std. Error"]
    # Fallback cumulative SEs: use model-based vcov
    V_model <- vcov(model)
    V_delta <- V_model[horizon_names, horizon_names]
    cum <- compute_cumulative_from_vcov(unname(delta_pts), V_delta)
  } else {
    delta_ses <- apply(boot_deltas, 2, sd, na.rm = TRUE)
    rho_ses   <- apply(boot_rhos, 2, sd, na.rm = TRUE)
    # Cumulative SEs from bootstrap
    cum_totals <- numeric(5)
    for (i in 1:5) cum_totals[i] <- sum(delta_pts[i:5])
    cum_ses <- apply(boot_cum, 2, sd, na.rm = TRUE)
    cum_ps  <- 2 * pnorm(-abs(cum_totals / cum_ses))
    cum <- list(totals = cum_totals, ses = cum_ses, ps = cum_ps, ors = exp(cum_totals))
  }

  delta_ps <- 2 * pnorm(-abs(delta_pts / delta_ses))
  rho_ps   <- 2 * pnorm(-abs(rho_pts / rho_ses))

  message(sprintf("    %s: N=%d, events=%d [%.1f min, %d successful boot reps]",
                  label, n_matches, n_events, elapsed_total, nrow(boot_deltas)))
  for (i in seq_along(horizon_names)) {
    message(sprintf("      %s: incr=%.4f (bootSE=%.4f), total=%.4f (bootSE=%.4f, OR=%.3f)",
                    horizon_names[i], delta_pts[i], delta_ses[i],
                    cum$totals[i], cum$ses[i], cum$ors[i]))
  }

  list(
    deltas     = unname(delta_pts),
    delta_ses  = unname(delta_ses),
    delta_ps   = unname(delta_ps),
    cum_totals = cum$totals,
    cum_ses    = cum$ses,
    cum_ps     = cum$ps,
    cum_ors    = cum$ors,
    horizon_names = horizon_names,
    n_matches  = n_matches,
    n_events   = n_events,
    rhos       = unname(rho_pts),
    rho_ps     = unname(rho_ps),
    rho_ses    = unname(rho_ses),
    n_boot_success = nrow(boot_deltas)
  )
}


###############################################################################
# PHASE 5: ESTIMATE ALL SPECIFICATIONS
###############################################################################

message("\n[5] Estimating all 8 horizon-heterogeneous specifications...")
message("    Bootstrap for nonGS will take some time...\n")

all_results <- list()

# --- GS-ATP ---
message("  === GS-ATP ===")
all_results$gs_atp <- list(
  wt_any  = estimate_gs_horizon(all_md$gs_atp_any,  label = "GS-ATP wt/any"),
  wt_same = estimate_gs_horizon(all_md$gs_atp_same, label = "GS-ATP wt/same")
)

# --- GS-WTA ---
message("\n  === GS-WTA ===")
all_results$gs_wta <- list(
  wt_any  = estimate_gs_horizon(all_md$gs_wta_any,  label = "GS-WTA wt/any"),
  wt_same = estimate_gs_horizon(all_md$gs_wta_same, label = "GS-WTA wt/same")
)

# --- nonGS-ATP ---
message("\n  === nonGS-ATP ===")
all_results$nongs_atp <- list(
  wt_any  = estimate_nongs_horizon(all_md$nongs_atp_any,  nongs_atp_ev,
                                    label = "nonGS-ATP wt/any"),
  wt_same = estimate_nongs_horizon(all_md$nongs_atp_same, nongs_atp_ev,
                                    label = "nonGS-ATP wt/same")
)

# --- nonGS-WTA ---
message("\n  === nonGS-WTA ===")
all_results$nongs_wta <- list(
  wt_any  = estimate_nongs_horizon(all_md$nongs_wta_any,  nongs_wta_ev,
                                    label = "nonGS-WTA wt/any"),
  wt_same = estimate_nongs_horizon(all_md$nongs_wta_same, nongs_wta_ev,
                                    label = "nonGS-WTA wt/same")
)


###############################################################################
# PHASE 6: GENERATE TABLES WITH CUMULATIVE EFFECTS
###############################################################################

message("\n[6] Generating horizon tables with cumulative total effects...")

HORIZON_LABELS_INCR <- c(
  "$t \\leq 4$w",
  "$4\\text{w} < t \\leq 8$w",
  "$8\\text{w} < t \\leq 12$w",
  "$12\\text{w} < t \\leq 26$w",
  "$26\\text{w} < t \\leq 52$w"
)

HORIZON_LABELS_CUM <- c(
  "$\\leq 4$w",
  "$\\leq 8$w",
  "$\\leq 12$w",
  "$\\leq 26$w",
  "$\\leq 52$w"
)

V_LABELS <- c(
  "$\\hat{\\rho}_{4w}$",
  "$\\hat{\\rho}_{8w}$",
  "$\\hat{\\rho}_{12w}$",
  "$\\hat{\\rho}_{26w}$",
  "$\\hat{\\rho}_{52w}$"
)

SPEC_LABELS <- c(
  wt_any  = "Any LL stop",
  wt_same = "Same-type stop"
)

write_horizon_table_cumulative <- function(results_list, filepath, is_gs = TRUE, label = "") {
  # Table has 2 spec columns, each with 3 sub-columns: delta_incr, total, OR(total)
  # For nonGS: also rho rows at bottom

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

  # Layout: 6 columns per spec (coef, SE, total, SE, OR) -- actually let's do
  # a cleaner layout with multicolumn headers
  # Horizon | Incr. delta (any) | SE | Total (any) | SE | OR | Incr (same) | SE | Total(same) | SE | OR
  # That's 11 columns which is very wide. Let me use two panels instead.

  lines <- c()

  for (spec_key in c("wt_any", "wt_same")) {
    r <- results_list[[spec_key]]
    panel_label <- SPEC_LABELS[spec_key]

    if (spec_key == "wt_any") {
      # Start the tabular
      lines <- c(lines,
        "\\begin{tabular}{lccccc}",
        "\\toprule"
      )
    }

    lines <- c(lines,
      sprintf("\\multicolumn{6}{l}{\\textbf{Panel %s: Points-weighted, %s}} \\\\",
              ifelse(spec_key == "wt_any", "A", "B"),
              tolower(panel_label)),
      "\\midrule",
      " & $\\hat{\\delta}$ (incr.) & SE & Total effect & SE & OR (total) \\\\",
      "\\midrule"
    )

    if (is.null(r)) {
      lines <- c(lines, "\\multicolumn{6}{c}{Estimation failed or insufficient data} \\\\")
    } else {
      # Delta rows with incremental + cumulative + OR
      lines <- c(lines, "\\multicolumn{6}{l}{\\textit{LL treatment effect by horizon}} \\\\")
      for (i in 1:5) {
        incr_str  <- fmt_coef_cell(r$deltas[i], r$delta_ps[i])
        incr_se   <- fmt_se_cell(r$delta_ses[i])
        total_str <- fmt_coef_cell(r$cum_totals[i], r$cum_ps[i])
        total_se  <- fmt_se_cell(r$cum_ses[i])
        or_str    <- fmt_or(r$cum_ors[i])

        lines <- c(lines, sprintf("%s & %s & %s & %s & %s & %s \\\\",
                                  HORIZON_LABELS_INCR[i], incr_str, incr_se,
                                  total_str, total_se, or_str))
      }

      # Rho rows (nonGS only)
      if (!is_gs && !all(is.na(r$rhos))) {
        lines <- c(lines, "[4pt]",
          "\\multicolumn{6}{l}{\\textit{Selection control ($\\hat{v}$) by horizon}} \\\\")
        for (i in 1:5) {
          rho_str <- fmt_coef_cell(r$rhos[i], r$rho_ps[i])
          rse_str <- fmt_se_cell(r$rho_ses[i])
          lines <- c(lines, sprintf("%s & %s & %s & & & \\\\",
                                    V_LABELS[i], rho_str, rse_str))
        }
      }

      # N row
      lines <- c(lines, "[4pt]",
        sprintf("$N_{\\text{matches}}$ & \\multicolumn{5}{l}{%s} \\\\",
                format(r$n_matches, big.mark = ",")),
        sprintf("$N_{\\text{events}}$ & \\multicolumn{5}{l}{%s} \\\\",
                format(r$n_events, big.mark = ","))
      )
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

# Write all 4 tables
write_horizon_table_cumulative(all_results$gs_atp,
                                file.path(TABLE_DIR, "table_tournament_horizon_gs_atp.tex"),
                                is_gs = TRUE, label = "GS-ATP")
write_horizon_table_cumulative(all_results$gs_wta,
                                file.path(TABLE_DIR, "table_tournament_horizon_gs_wta.tex"),
                                is_gs = TRUE, label = "GS-WTA")
write_horizon_table_cumulative(all_results$nongs_atp,
                                file.path(TABLE_DIR, "table_tournament_horizon_nongs_atp.tex"),
                                is_gs = FALSE, label = "nonGS-ATP")
write_horizon_table_cumulative(all_results$nongs_wta,
                                file.path(TABLE_DIR, "table_tournament_horizon_nongs_wta.tex"),
                                is_gs = FALSE, label = "nonGS-WTA")


###############################################################################
# PHASE 7: SUMMARY
###############################################################################

message("\n[7] Writing summary...")

summary_text <- c(
  "# Tournament Horizon Cumulative Effects Summary (Script 27)",
  paste0("Generated: ", Sys.time()),
  "",
  "## Design",
  "",
  "Re-estimation of horizon-heterogeneous specs from script 26, now with",
  "cumulative total effects and proper SEs.",
  "",
  "Model: won ~ D_4w + D_8w + D_12w + D_26w + D_52w + covariates",
  "where D_hw = D_i * 1(t <= h).",
  "",
  "Since D_4w=1 implies D_8w=1 etc., a match at t<=4w has ALL five dummies active.",
  "The incremental coefficient delta_h captures the additional effect at horizon h.",
  "",
  "Cumulative total effect at horizon h = sum(delta_j for j = h, h+1, ..., 52w)",
  "  Total at 4w  = delta_4w + delta_8w + delta_12w + delta_26w + delta_52w",
  "  Total at 8w  = delta_8w + delta_12w + delta_26w + delta_52w",
  "  Total at 12w = delta_12w + delta_26w + delta_52w",
  "  Total at 26w = delta_26w + delta_52w",
  "  Total at 52w = delta_52w",
  "",
  "GS SEs: from clustered vcov matrix (sqrt of sum of relevant vcov block).",
  "NonGS SEs: from bootstrap SD of cumulative totals across 200 reps.",
  ""
)

for (sample_key in c("gs_atp", "gs_wta", "nongs_atp", "nongs_wta")) {
  sample_label <- toupper(gsub("_", "-", sample_key))

  summary_text <- c(summary_text, paste0("## ", sample_label), "")

  for (spec_key in c("wt_any", "wt_same")) {
    spec_label <- SPEC_LABELS[spec_key]
    r <- all_results[[sample_key]][[spec_key]]

    summary_text <- c(summary_text, paste0("### ", spec_label), "")

    if (is.null(r)) {
      summary_text <- c(summary_text, "Estimation failed or insufficient data.", "")
      next
    }

    summary_text <- c(summary_text,
      sprintf("N_matches = %s, N_events = %d",
              format(r$n_matches, big.mark = ","), r$n_events),
      "",
      "| Horizon | Incr. delta | SE | p | Total | SE | p | OR (total) |",
      "|---|---|---|---|---|---|---|---|"
    )

    for (i in 1:5) {
      h_name <- c("4w", "8w", "12w", "26w", "52w")[i]
      summary_text <- c(summary_text,
        sprintf("| %s | %.4f | %.4f | %.4f | %.4f | %.4f | %.4f | %.3f |",
                h_name, r$deltas[i], r$delta_ses[i], r$delta_ps[i],
                r$cum_totals[i], r$cum_ses[i], r$cum_ps[i], r$cum_ors[i]))
    }

    summary_text <- c(summary_text, "")
  }
}

summary_text <- c(summary_text, "", "## Execution Log", "", summary_log)

writeLines(summary_text, file.path(OUTPUT_DIR, "tournament_horizon_cumulative_summary.md"))
message("  Saved: Output/tournament_horizon_cumulative_summary.md")

# Save results object (overwrites previous)
saveRDS(all_results, file.path(CLEANED_DIR, "tournament_horizon_results.rds"))
message("  Saved: Data/cleaned/tournament_horizon_results.rds")

message("\n", strrep("=", 72))
message("  DONE: 27_horizon_cumulative.R")
message(strrep("=", 72), "\n")
