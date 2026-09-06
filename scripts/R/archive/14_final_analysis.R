# ==============================================================================
# 14_final_analysis.R
# Consolidated final analysis for the Lucky Losers paper.
#
# Tasks:
#   1. First-LL-only restriction (GS lottery + LOO-IV)
#   2. Intensive margin / treatment dose analysis
#   3. Predicted win probability P(i->j) outcome (rename from "competitiveness")
#   4. Comprehensive descriptive statistics
#   5. Main results tables (final)
#   6. Key figures (final)
#
# Inputs:  Data/raw/*.rds, Data/cleaned/*.rds
# Outputs: Data/cleaned/first_ll_only_results.rds
#          Data/cleaned/dose_analysis_results.rds
#          Data/cleaned/win_prob_analysis.rds
#          Tables/table_summary_stats_final.tex
#          Tables/table_ll_distribution_gs_final.tex
#          Tables/table_ll_distribution_nongs_final.tex
#          Tables/table_ll_careers_final.tex
#          Tables/table_ll_success_rates_final.tex
#          Tables/table_main_lottery_results_final.tex
#          Tables/table_main_iv_results_final.tex
#          Tables/table_dose_final.tex
#          Tables/table_dose_response.tex
#          Tables/table_tournament_model_final.tex
#          Figures/fig_event_study_final.pdf
#          Figures/fig_dose_response.pdf
#          Figures/fig_ranking_distribution.pdf
#          Figures/fig_first_stage_iv.pdf
#          Output/sample_counts.md
#          Output/final_analysis_summary.md
# Dependencies: dplyr, tidyr, readr, stringr, ggplot2, fixest, here, patchwork
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
library(patchwork)

# --- Paths --------------------------------------------------------------------
RAW_DIR     <- here("Data", "raw")
CLEANED_DIR <- here("Data", "cleaned")
TABLES_DIR  <- here("Tables")
FIGURES_DIR <- here("Figures")
OUTPUT_DIR  <- here("Output")
for (d in c(CLEANED_DIR, TABLES_DIR, FIGURES_DIR, OUTPUT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

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

col_treat   <- "#E69F00"
col_control <- "#56B4E9"

# --- Helper: significance stars -----------------------------------------------
add_stars <- function(pv) {
  ifelse(pv < 0.01, "$^{***}$",
         ifelse(pv < 0.05, "$^{**}$",
                ifelse(pv < 0.1, "$^{*}$", "")))
}

# --- Bernoulli convolution (from 11/13) ---------------------------------------
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

# ==============================================================================
# LOAD ALL DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("LOADING ALL DATA")
message(strrep("=", 70))

atp_main <- read_rds(file.path(RAW_DIR, "atp_main_matches.rds"))
atp_qual <- read_rds(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
wta_main <- read_rds(file.path(RAW_DIR, "wta_main_matches.rds"))
wta_qual <- read_rds(file.path(RAW_DIR, "wta_qual_itf_matches.rds"))
atp_rankings_raw <- read_rds(file.path(RAW_DIR, "atp_rankings.rds"))
wta_rankings_raw <- read_rds(file.path(RAW_DIR, "wta_rankings.rds"))
atp_players <- read_rds(file.path(RAW_DIR, "atp_players.rds"))
wta_players <- read_rds(file.path(RAW_DIR, "wta_players.rds"))

est_final <- read_rds(file.path(CLEANED_DIR, "estimation_sample_final.rds"))
elo_history <- tryCatch(read_rds(file.path(CLEANED_DIR, "elo_history.rds")),
                         error = function(e) { message("  elo_history.rds not found"); NULL })
wta_elo_history <- tryCatch(read_rds(file.path(CLEANED_DIR, "wta_elo_history.rds")),
                              error = function(e) { message("  wta_elo_history.rds not found"); NULL })
win_model_v2 <- tryCatch(read_rds(file.path(CLEANED_DIR, "win_model_v2.rds")),
                           error = function(e) { message("  win_model_v2.rds not found"); NULL })
pooled_gs <- tryCatch(read_rds(file.path(CLEANED_DIR, "pooled_gs_lottery_sample.rds")),
                        error = function(e) { message("  pooled_gs_lottery_sample.rds not found"); NULL })
tournament_results <- tryCatch(read_rds(file.path(CLEANED_DIR, "tournament_model_results.rds")),
                                 error = function(e) { message("  tournament_model_results.rds not found"); NULL })

message("  ATP main: ", nrow(atp_main), " matches")
message("  WTA main: ", nrow(wta_main), " matches")
message("  Estimation sample final: ", nrow(est_final))

# Summary accumulator
final_summary <- list()

# ==============================================================================
# REBUILD ALL-QUALIFIERS DATASET (ATP + WTA)
# ==============================================================================
message("\n", strrep("=", 70))
message("REBUILDING ALL-QUALIFIERS DATASET (ATP + WTA)")
message(strrep("=", 70))

# --- Helper to build qualifiers from one tour ---
build_qualifiers <- function(main_df, qual_df, rankings_raw, tour_label) {
  message("  Building ", tour_label, " qualifiers...")

  qual_tour <- qual_df |>
    mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
    filter(year >= 2000,
           tourney_level %in% c("G", "M", "A"),
           str_detect(round, "^Q"))

  final_round <- qual_tour |>
    group_by(tourney_id) |>
    summarise(max_qual_round = max(round), .groups = "drop")

  final_matches <- qual_tour |>
    inner_join(final_round, by = "tourney_id") |>
    filter(round == max_qual_round)

  message("    Final qualifying round matches: ", nrow(final_matches))

  # Winners
  qw <- final_matches |>
    transmute(
      tourney_id, tourney_name, tourney_date, tourney_level, surface, draw_size,
      year = as.integer(str_sub(tourney_date, 1, 4)),
      player_id = winner_id, player_name = winner_name,
      player_rank = winner_rank, player_rank_points = winner_rank_points,
      player_age = winner_age, player_hand = winner_hand,
      player_ht = winner_ht, player_ioc = winner_ioc,
      opponent_id = loser_id, opponent_rank = loser_rank,
      opponent_age = loser_age, opponent_ioc = loser_ioc,
      won_qualifying_match = 1L, score = score, minutes = minutes,
      tour = tour_label
    )

  # Losers
  ql <- final_matches |>
    transmute(
      tourney_id, tourney_name, tourney_date, tourney_level, surface, draw_size,
      year = as.integer(str_sub(tourney_date, 1, 4)),
      player_id = loser_id, player_name = loser_name,
      player_rank = loser_rank, player_rank_points = loser_rank_points,
      player_age = loser_age, player_hand = loser_hand,
      player_ht = loser_ht, player_ioc = loser_ioc,
      opponent_id = winner_id, opponent_rank = winner_rank,
      opponent_age = winner_age, opponent_ioc = winner_ioc,
      won_qualifying_match = 0L, score = score, minutes = minutes,
      tour = tour_label
    )

  all_q <- bind_rows(qw, ql)

  # Identify LL entries from main draw
  ll_w <- main_df |>
    mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
    filter(year >= 2000, winner_entry == "LL") |>
    transmute(tourney_id, ll_player_id = winner_id)
  ll_l <- main_df |>
    mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
    filter(year >= 2000, loser_entry == "LL") |>
    transmute(tourney_id, ll_player_id = loser_id)
  ll_entries <- bind_rows(ll_w, ll_l) |> distinct(tourney_id, ll_player_id)

  all_q <- all_q |>
    mutate(got_ll = as.integer(paste0(tourney_id, "_", player_id) %in%
                                 paste0(ll_entries$tourney_id, "_", ll_entries$ll_player_id)))

  # LL slots per tournament (among losers only)
  ll_slots <- all_q |>
    filter(won_qualifying_match == 0L) |>
    group_by(tourney_id) |>
    summarise(n_ll_slots = sum(got_ll), .groups = "drop")

  all_q <- all_q |>
    left_join(ll_slots, by = "tourney_id") |>
    mutate(n_ll_slots = replace_na(n_ll_slots, 0L))

  # Rank among losers
  loser_ranks <- all_q |>
    filter(won_qualifying_match == 0L) |>
    group_by(tourney_id) |>
    mutate(rank_among_losers = rank(ifelse(is.na(player_rank), 9999, player_rank),
                                     ties.method = "min")) |>
    ungroup() |>
    select(tourney_id, player_id, rank_among_losers)

  all_q <- all_q |>
    left_join(loser_ranks, by = c("tourney_id", "player_id"))

  # LL match details (for dose analysis)
  ll_match_details <- bind_rows(
    main_df |>
      mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
      filter(year >= 2000, winner_entry == "LL") |>
      transmute(tourney_id, ll_player_id = winner_id, ll_won_match = 1L,
                ll_round = round, ll_opp_rank = loser_rank),
    main_df |>
      mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
      filter(year >= 2000, loser_entry == "LL") |>
      transmute(tourney_id, ll_player_id = loser_id, ll_won_match = 0L,
                ll_round = round, ll_opp_rank = winner_rank)
  )

  ll_summary <- ll_match_details |>
    group_by(tourney_id, ll_player_id) |>
    summarise(
      ll_matches_played = n(),
      ll_matches_won = sum(ll_won_match),
      .groups = "drop"
    )

  all_q <- all_q |>
    left_join(ll_summary |> rename(player_id = ll_player_id),
              by = c("tourney_id", "player_id")) |>
    mutate(
      ll_matches_played = replace_na(ll_matches_played, 0L),
      ll_matches_won = replace_na(ll_matches_won, 0L)
    )

  message("    All qualifiers: ", nrow(all_q),
          " (LL: ", sum(all_q$got_ll), ")")
  all_q
}

atp_qualifiers <- build_qualifiers(atp_main, atp_qual, atp_rankings_raw, "ATP")
wta_qualifiers <- build_qualifiers(wta_main, wta_qual, wta_rankings_raw, "WTA")

all_qualifiers <- bind_rows(atp_qualifiers, wta_qualifiers)
message("  Combined qualifiers: ", nrow(all_qualifiers),
        " (ATP: ", sum(all_qualifiers$tour == "ATP"),
        ", WTA: ", sum(all_qualifiers$tour == "WTA"), ")")

# ==============================================================================
# BUILD RANKING TRAJECTORIES
# ==============================================================================
message("\n", strrep("=", 70))
message("BUILDING RANKING TRAJECTORIES")
message(strrep("=", 70))

# Parse rankings
atp_rankings <- atp_rankings_raw |>
  mutate(rank_date = as.Date(as.character(ranking_date), format = "%Y%m%d")) |>
  filter(!is.na(rank_date)) |>
  select(player = player, rank_date, rank, points)

wta_rankings <- wta_rankings_raw |>
  mutate(rank_date = as.Date(as.character(ranking_date), format = "%Y%m%d")) |>
  filter(!is.na(rank_date)) |>
  select(player = player, rank_date, rank, points)

all_qualifiers <- all_qualifiers |>
  mutate(event_date = as.Date(as.character(tourney_date), format = "%Y%m%d"))

horizons <- c(0, 4, 8, 12, 26, 52)

for (h in horizons) {
  col_rank <- paste0("rank_t", h)
  col_pts  <- paste0("points_t", h)
  if (col_rank %in% names(all_qualifiers)) {
    message("  Horizon t+", h, "w: already present, skipping")
    next
  }

  message("  Horizon t+", h, "w: merging rankings...")

  # ATP
  atp_targets <- all_qualifiers |>
    filter(tour == "ATP", !is.na(event_date)) |>
    transmute(tourney_id, player_id, target_date = event_date + h * 7)

  atp_matched <- atp_targets |>
    inner_join(atp_rankings |> rename(player_id = player),
               by = "player_id", relationship = "many-to-many") |>
    filter(abs(as.numeric(rank_date - target_date)) <= 10) |>
    mutate(date_diff = abs(as.numeric(rank_date - target_date))) |>
    group_by(tourney_id, player_id) |>
    slice_min(date_diff, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(tourney_id, player_id,
           !!col_rank := rank,
           !!col_pts := points)

  # WTA
  wta_targets <- all_qualifiers |>
    filter(tour == "WTA", !is.na(event_date)) |>
    transmute(tourney_id, player_id, target_date = event_date + h * 7)

  wta_matched <- wta_targets |>
    inner_join(wta_rankings |> rename(player_id = player),
               by = "player_id", relationship = "many-to-many") |>
    filter(abs(as.numeric(rank_date - target_date)) <= 10) |>
    mutate(date_diff = abs(as.numeric(rank_date - target_date))) |>
    group_by(tourney_id, player_id) |>
    slice_min(date_diff, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(tourney_id, player_id,
           !!col_rank := rank,
           !!col_pts := points)

  combined_matched <- bind_rows(atp_matched, wta_matched)

  all_qualifiers <- all_qualifiers |>
    left_join(combined_matched, by = c("tourney_id", "player_id"))
}

# Compute changes
all_qualifiers <- all_qualifiers |>
  mutate(
    rank_change_4w  = rank_t4  - rank_t0,
    rank_change_8w  = rank_t8  - rank_t0,
    rank_change_12w = rank_t12 - rank_t0,
    rank_change_26w = rank_t26 - rank_t0,
    rank_change_52w = rank_t52 - rank_t0,
    points_change_12w = points_t12 - points_t0,
    points_change_26w = points_t26 - points_t0,
    points_change_52w = points_t52 - points_t0
  )

# Merge Elo
if (!is.null(elo_history)) {
  message("  Merging ATP Elo trajectories...")
  for (h in horizons) {
    col_elo <- paste0("elo_t", h)
    if (col_elo %in% names(all_qualifiers)) next
    target <- all_qualifiers |>
      filter(tour == "ATP", !is.na(event_date)) |>
      transmute(tourney_id, player_id, target_date = event_date + h * 7)
    elo_m <- target |>
      inner_join(elo_history, by = "player_id", relationship = "many-to-many") |>
      filter(abs(as.numeric(match_date - target_date)) <= 21) |>
      mutate(date_diff = abs(as.numeric(match_date - target_date))) |>
      group_by(tourney_id, player_id) |>
      slice_min(date_diff, n = 1, with_ties = FALSE) |>
      ungroup() |>
      select(tourney_id, player_id, !!col_elo := elo)
    all_qualifiers <- all_qualifiers |>
      left_join(elo_m, by = c("tourney_id", "player_id"))
  }
  all_qualifiers <- all_qualifiers |>
    mutate(
      elo_change_12w = elo_t12 - elo_t0,
      elo_change_26w = elo_t26 - elo_t0
    )
}

message("  All qualifiers with trajectories: ", nrow(all_qualifiers))
message("  rank_t0 non-NA: ", sum(!is.na(all_qualifiers$rank_t0)))

# ==============================================================================
# BUILD ADDITIONAL OUTCOMES (direct entry, main draws, matches 250+)
# ==============================================================================
message("\n", strrep("=", 70))
message("BUILDING ADDITIONAL OUTCOMES")
message(strrep("=", 70))

# --- Direct entry next year ---
build_direct_entry <- function(main_df, tour_label) {
  entries <- main_df |>
    mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
    filter(year >= 2000) |>
    transmute(
      tourney_name_clean = str_to_lower(str_replace_all(tourney_name, "[^a-zA-Z0-9]", "")),
      year,
      player_id_w = winner_id, entry_w = winner_entry,
      player_id_l = loser_id, entry_l = loser_entry
    )

  appearances <- bind_rows(
    entries |> transmute(tourney_name_clean, year, player_id = player_id_w, entry = entry_w),
    entries |> transmute(tourney_name_clean, year, player_id = player_id_l, entry = entry_l)
  ) |> distinct(tourney_name_clean, year, player_id, entry)

  direct <- appearances |>
    filter(is.na(entry) | entry == "" |
             !entry %in% c("LL", "Q", "WC", "PR", "SE", "ALT", "Alt")) |>
    distinct(tourney_name_clean, year, player_id) |>
    mutate(direct_entry_next_year = 1L)
  direct
}

atp_direct <- build_direct_entry(atp_main, "ATP")
wta_direct <- build_direct_entry(wta_main, "WTA")
all_direct <- bind_rows(atp_direct, wta_direct)

all_qualifiers <- all_qualifiers |>
  mutate(
    tourney_name_clean = str_to_lower(str_replace_all(tourney_name, "[^a-zA-Z0-9]", "")),
    next_year = year + 1L
  ) |>
  left_join(all_direct |> rename(next_year = year),
            by = c("tourney_name_clean", "next_year", "player_id")) |>
  mutate(direct_entry_next_year = replace_na(direct_entry_next_year, 0L))

message("  Direct entry next year rate: ",
        round(mean(all_qualifiers$direct_entry_next_year[all_qualifiers$won_qualifying_match == 0L]) * 100, 1), "%")

# --- Main draws entered and matches at 250+ ---
message("  Computing main draw / match counts...")

build_future_counts <- function(main_df, tour_label) {
  all_app <- bind_rows(
    main_df |> mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
      filter(year >= 2000) |>
      transmute(player_id = winner_id,
                match_date = as.Date(as.character(tourney_date), format = "%Y%m%d"),
                tourney_id, tourney_level),
    main_df |> mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
      filter(year >= 2000) |>
      transmute(player_id = loser_id,
                match_date = as.Date(as.character(tourney_date), format = "%Y%m%d"),
                tourney_id, tourney_level)
  )

  tourney_app <- all_app |> distinct(player_id, tourney_id, match_date, tourney_level)
  match_app <- all_app |> filter(tourney_level %in% c("G", "M", "A"))

  list(tourneys = tourney_app, matches = match_app)
}

atp_future <- build_future_counts(atp_main, "ATP")
wta_future <- build_future_counts(wta_main, "WTA")

# Combine by tour
all_tourney_app <- bind_rows(
  atp_future$tourneys |> mutate(tour = "ATP"),
  wta_future$tourneys |> mutate(tour = "WTA")
)
all_match_app <- bind_rows(
  atp_future$matches |> mutate(tour = "ATP"),
  wta_future$matches |> mutate(tour = "WTA")
)

# Pre-split by player for efficiency
tourney_by_player <- split(all_tourney_app, all_tourney_app$player_id)
match_by_player <- split(all_match_app, all_match_app$player_id)

# Only compute for losers (who are the estimation sample)
losers_idx <- which(all_qualifiers$won_qualifying_match == 0L & !is.na(all_qualifiers$event_date))
message("  Computing counts for ", length(losers_idx), " loser observations...")

all_qualifiers$n_main_draws_26w <- NA_integer_
all_qualifiers$n_main_draws_52w <- NA_integer_
all_qualifiers$n_matches_250plus_26w <- NA_integer_
all_qualifiers$n_matches_250plus_52w <- NA_integer_

for (ii in seq_along(losers_idx)) {
  i <- losers_idx[ii]
  if (ii %% 2000 == 0) message("    Row ", ii, " / ", length(losers_idx))
  pid <- all_qualifiers$player_id[i]
  edate <- all_qualifiers$event_date[i]

  pt <- tourney_by_player[[as.character(pid)]]
  if (!is.null(pt)) {
    pt_f <- pt |> filter(!is.na(match_date))
    all_qualifiers$n_main_draws_26w[i] <- n_distinct(pt_f$tourney_id[pt_f$match_date > edate & pt_f$match_date <= edate + 26 * 7])
    all_qualifiers$n_main_draws_52w[i] <- n_distinct(pt_f$tourney_id[pt_f$match_date > edate & pt_f$match_date <= edate + 52 * 7])
  } else {
    all_qualifiers$n_main_draws_26w[i] <- 0L
    all_qualifiers$n_main_draws_52w[i] <- 0L
  }

  pm <- match_by_player[[as.character(pid)]]
  if (!is.null(pm)) {
    pm_f <- pm |> filter(!is.na(match_date))
    all_qualifiers$n_matches_250plus_26w[i] <- sum(pm_f$match_date > edate & pm_f$match_date <= edate + 26 * 7)
    all_qualifiers$n_matches_250plus_52w[i] <- sum(pm_f$match_date > edate & pm_f$match_date <= edate + 52 * 7)
  } else {
    all_qualifiers$n_matches_250plus_26w[i] <- 0L
    all_qualifiers$n_matches_250plus_52w[i] <- 0L
  }
}

message("  Main draws 26w mean: ",
        round(mean(all_qualifiers$n_main_draws_26w[losers_idx], na.rm = TRUE), 1))
message("  Matches 250+ 26w mean: ",
        round(mean(all_qualifiers$n_matches_250plus_26w[losers_idx], na.rm = TRUE), 1))

# ==============================================================================
# TASK 1: FIRST-LL-ONLY RESTRICTION
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 1: FIRST-LL-ONLY RESTRICTION")
message(strrep("=", 70))

# --- 1a. Identify first qualifying loss per player ---------------------------
message("\n--- 1a. Identifying first qualifying loss per player ---")

losers_only <- all_qualifiers |>
  filter(won_qualifying_match == 0L, !is.na(event_date))

first_ll_only <- losers_only |>
  arrange(player_id, event_date) |>
  group_by(player_id) |>
  slice_head(n = 1) |>
  ungroup()

message("  Full losers sample: ", nrow(losers_only))
message("  First-LL-only sample: ", nrow(first_ll_only))
message("  Drop: ", nrow(losers_only) - nrow(first_ll_only), " subsequent qualifying losses")
message("  First-LL-only LL recipients: ", sum(first_ll_only$got_ll))

# --- 1b. GS lottery analysis on first-LL-only --------------------------------
message("\n--- 1b. GS lottery analysis (first-LL-only) ---")

# GS lottery: top-4 ranked losers at Grand Slams
gs_first_ll <- first_ll_only |>
  filter(tourney_level == "G", n_ll_slots > 0,
         !is.na(rank_among_losers), rank_among_losers <= 4)

message("  GS first-LL-only: ", nrow(gs_first_ll),
        " (LL: ", sum(gs_first_ll$got_ll), ", control: ", sum(!gs_first_ll$got_ll), ")")

all_outcomes_names <- c(
  "rank_change_4w", "rank_change_8w", "rank_change_12w", "rank_change_26w", "rank_change_52w",
  "points_change_12w", "points_change_26w", "points_change_52w",
  "n_main_draws_26w", "n_main_draws_52w",
  "n_matches_250plus_26w", "n_matches_250plus_52w",
  "direct_entry_next_year"
)
if ("elo_change_12w" %in% names(gs_first_ll)) {
  all_outcomes_names <- c(all_outcomes_names, "elo_change_12w", "elo_change_26w")
}

run_lottery_diff <- function(data, outcomes, label) {
  results <- list()
  for (out in outcomes) {
    if (!out %in% names(data)) next
    y <- data[[out]]
    d <- data$got_ll
    ok <- !is.na(y)
    n_tr <- sum(d[ok] == 1)
    n_ct <- sum(d[ok] == 0)
    if (n_tr < 3 || n_ct < 3) next

    diff <- mean(y[ok][d[ok] == 1]) - mean(y[ok][d[ok] == 0])
    tt <- tryCatch(t.test(y[ok] ~ d[ok]), error = function(e) NULL)

    results[[out]] <- tibble(
      sample = label, outcome = out,
      diff = diff,
      mean_treated = mean(y[ok][d[ok] == 1]),
      mean_control = mean(y[ok][d[ok] == 0]),
      pv = if (!is.null(tt)) tt$p.value else NA_real_,
      n_treated = n_tr, n_control = n_ct
    )
  }
  bind_rows(results)
}

# Run for each subsample
gs_atp_first <- gs_first_ll |> filter(tour == "ATP")
gs_wta_first <- gs_first_ll |> filter(tour == "WTA")

lottery_results_first <- bind_rows(
  run_lottery_diff(gs_atp_first, all_outcomes_names, "ATP first-LL-only"),
  run_lottery_diff(gs_wta_first, all_outcomes_names, "WTA first-LL-only"),
  run_lottery_diff(gs_first_ll, all_outcomes_names, "Pooled first-LL-only")
)

# Also run on FULL GS lottery sample for comparison
gs_full <- losers_only |>
  filter(tourney_level == "G", n_ll_slots > 0,
         !is.na(rank_among_losers), rank_among_losers <= 4)

gs_atp_full <- gs_full |> filter(tour == "ATP")
gs_wta_full <- gs_full |> filter(tour == "WTA")

lottery_results_full <- bind_rows(
  run_lottery_diff(gs_atp_full, all_outcomes_names, "ATP full"),
  run_lottery_diff(gs_wta_full, all_outcomes_names, "WTA full"),
  run_lottery_diff(gs_full, all_outcomes_names, "Pooled full")
)

all_lottery_results <- bind_rows(lottery_results_full, lottery_results_first)

# BH correction within each sample
all_lottery_results <- all_lottery_results |>
  group_by(sample) |>
  mutate(pv_bh = p.adjust(pv, method = "BH")) |>
  ungroup()

message("\n  GS Lottery results (first-LL-only, pooled):")
all_lottery_results |>
  filter(sample == "Pooled first-LL-only") |>
  select(outcome, diff, pv, pv_bh) |>
  print(n = 20)

# --- 1c. LOO-IV on first-LL-only ---------------------------------------------
message("\n--- 1c. LOO-IV on first-LL-only ---")

# Build win probability model if needed
if (is.null(win_model_v2)) {
  message("  Building fallback logit win model...")
  atp_all_tmp <- bind_rows(
    atp_main |> mutate(match_source = "main"),
    atp_qual |> mutate(match_source = "qual")
  ) |>
    mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
    filter(year >= 2000, !is.na(winner_id), !is.na(loser_id))

  mp <- bind_rows(
    atp_all_tmp |> filter(!is.na(winner_rank), !is.na(loser_rank)) |>
      transmute(player_rank = winner_rank, opponent_rank = loser_rank,
                player_age = winner_age, opponent_age = loser_age,
                player_ioc = winner_ioc, opponent_ioc = loser_ioc,
                surface, won = 1L, player_entry = winner_entry),
    atp_all_tmp |> filter(!is.na(winner_rank), !is.na(loser_rank)) |>
      transmute(player_rank = loser_rank, opponent_rank = winner_rank,
                player_age = loser_age, opponent_age = winner_age,
                player_ioc = loser_ioc, opponent_ioc = winner_ioc,
                surface, won = 0L, player_entry = loser_entry)
  ) |>
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
    data = mp, family = binomial(link = "logit")
  )
  message("  Built fallback logit, McFadden R2: ",
          round(1 - win_model_v2$deviance / win_model_v2$null.deviance, 4))
  rm(atp_all_tmp, mp)
}

# Predict win probabilities for first-LL-only (non-GS sample for IV)
first_ll_nongs <- first_ll_only |>
  filter(n_ll_slots > 0, !is.na(opponent_rank), !is.na(player_rank))

first_ll_nongs <- first_ll_nongs |>
  mutate(
    log_rank_ratio = log(pmax(opponent_rank, 1) / pmax(player_rank, 1)),
    rank_diff = opponent_rank - player_rank,
    same_ioc = as.integer(player_ioc == opponent_ioc),
    age_diff = coalesce(player_age - opponent_age, 0),
    surface_clay = as.integer(surface == "Clay"),
    surface_grass = as.integer(surface == "Grass")
  )

# Determine model terms needed
model_terms <- names(coef(win_model_v2))
needs_extra <- any(str_detect(model_terms, "player_win_rate|opponent_win_rate|h2h|is_gs|ace_rate"))

if (needs_extra) {
  message("  Enhanced model: adding defaults for missing predictors")
  default_cols <- c("player_win_rate", "opponent_win_rate", "h2h_win_rate",
                    "has_h2h", "is_gs", "is_masters", "is_qual",
                    "player_ace_rate_cum", "player_df_rate_cum",
                    "player_1st_pct_cum", "player_1st_win_cum",
                    "player_2nd_win_cum", "player_bp_save_cum",
                    "player_ret_win_cum", "ht_diff", "hand_mismatch")
  for (v in default_cols) {
    if (v %in% model_terms && !v %in% names(first_ll_nongs)) {
      first_ll_nongs[[v]] <- if (v %in% c("player_win_rate", "opponent_win_rate",
                                            "h2h_win_rate")) 0.5
      else if (v == "is_qual") 1L
      else 0
    }
  }
  # Fill NAs with training medians
  if (!is.null(win_model_v2$model)) {
    train_data <- win_model_v2$model
    for (v in intersect(names(first_ll_nongs), names(train_data))) {
      if (is.numeric(first_ll_nongs[[v]]) && any(is.na(first_ll_nongs[[v]]))) {
        med_val <- median(train_data[[v]], na.rm = TRUE)
        first_ll_nongs[[v]][is.na(first_ll_nongs[[v]])] <- med_val
      }
    }
  }
}

# Predict
pred_valid <- first_ll_nongs |> filter(!is.na(log_rank_ratio), !is.na(rank_diff))
pred_valid$p_win_own_match <- predict(win_model_v2, newdata = pred_valid, type = "response")

first_ll_nongs <- first_ll_nongs |>
  left_join(
    pred_valid |> select(tourney_id, player_id, p_win_own_match) |>
      distinct(tourney_id, player_id, .keep_all = TRUE),
    by = c("tourney_id", "player_id")
  )

# Compute LOO peer_component
message("  Computing LOO instrument for first-LL-only...")
tryCatch({
first_ll_iv <- first_ll_nongs |> filter(!is.na(p_win_own_match))

# Need to get all qualifiers at these tournaments for the convolution
tourney_ids_fl <- unique(first_ll_iv$tourney_id)

# Use full all_qualifiers for the convolution (we need all players, not just first-LL)
loo_fl_results <- vector("list", nrow(first_ll_iv))

for (tid in tourney_ids_fl) {
  # All qualifiers at this tournament with valid win probs
  t_all_q <- all_qualifiers |>
    filter(tourney_id == tid, !is.na(p_win_own_match) | !is.na(opponent_rank))

  # Predict win probs for all qualifiers at this tournament
  t_all_q <- t_all_q |>
    mutate(
      log_rank_ratio = log(pmax(opponent_rank, 1) / pmax(player_rank, 1)),
      rank_diff = opponent_rank - player_rank,
      same_ioc = as.integer(player_ioc == opponent_ioc),
      age_diff = coalesce(player_age - opponent_age, 0),
      surface_clay = as.integer(surface == "Clay"),
      surface_grass = as.integer(surface == "Grass")
    )

  # Add extra columns if needed
  if (needs_extra) {
    for (v in default_cols) {
      if (v %in% model_terms && !v %in% names(t_all_q)) {
        t_all_q[[v]] <- if (v %in% c("player_win_rate", "opponent_win_rate",
                                       "h2h_win_rate")) 0.5
        else if (v == "is_qual") 1L
        else 0
      }
    }
  }

  valid_t <- t_all_q |> filter(!is.na(log_rank_ratio), !is.na(rank_diff))
  if (nrow(valid_t) == 0) next
  valid_t$p_win_t <- predict(win_model_v2, newdata = valid_t, type = "response")

  valid_t <- valid_t |>
    mutate(
      rank_sort = ifelse(is.na(player_rank), 9999, player_rank),
      loss_prob = 1 - p_win_t
    ) |>
    arrange(rank_sort) |>
    mutate(rank_pos = row_number())

  c_t <- valid_t$n_ll_slots[1]
  if (is.na(c_t) || c_t == 0) next

  # For each loser in first_ll_iv at this tournament
  fl_at_t <- which(first_ll_iv$tourney_id == tid)
  for (fl_idx in fl_at_t) {
    pid <- first_ll_iv$player_id[fl_idx]
    # Find this player in valid_t
    p_row <- which(valid_t$player_id == pid & valid_t$won_qualifying_match == 0L)
    if (length(p_row) == 0) next
    p_row <- p_row[1]

    rank_pos_i <- valid_t$rank_pos[p_row]
    own_loss <- valid_t$loss_prob[p_row]

    higher <- valid_t |> filter(rank_pos < rank_pos_i)

    if (rank_pos_i <= c_t) {
      peer_comp <- 1.0
    } else {
      threshold_needed <- rank_pos_i - c_t
      if (nrow(higher) > 0) {
        peer_comp <- bernoulli_conv_ge(higher$loss_prob, threshold_needed)
      } else {
        peer_comp <- if (threshold_needed <= 0) 1.0 else 0.0
      }
    }

    loo_fl_results[[fl_idx]] <- tibble(
      tourney_id = tid, player_id = pid,
      own_loss_prob = own_loss,
      peer_component = peer_comp
    )
  }
}

loo_fl_df <- bind_rows(loo_fl_results)
message("  LOO instrument computed for first-LL-only: ", nrow(loo_fl_df))

first_ll_iv <- first_ll_iv |>
  left_join(loo_fl_df, by = c("tourney_id", "player_id"))

est_fl <- first_ll_iv |> filter(!is.na(peer_component), !is.na(got_ll))
message("  First-LL-only IV estimation sample: ", nrow(est_fl),
        " (LL: ", sum(est_fl$got_ll), ")")

# Run LOO-IV
iv_outcomes <- c(
  "rank_change_4w", "rank_change_8w", "rank_change_12w", "rank_change_26w", "rank_change_52w",
  "points_change_12w", "points_change_26w", "points_change_52w",
  "n_main_draws_26w", "n_main_draws_52w",
  "n_matches_250plus_26w", "n_matches_250plus_52w",
  "direct_entry_next_year"
)
if ("elo_change_12w" %in% names(est_fl)) {
  iv_outcomes <- c(iv_outcomes, "elo_change_12w", "elo_change_26w")
}

first_ll_iv_results <- list()

for (outcome in iv_outcomes) {
  if (!outcome %in% names(est_fl)) next
  y <- est_fl[[outcome]]
  ok <- !is.na(y)
  if (sum(ok) < 50) {
    message("  ", outcome, ": insufficient obs (", sum(ok), ")")
    next
  }

  tryCatch({
    iv_fit <- feols(
      as.formula(paste0(outcome,
                        " ~ player_rank + player_age | year | got_ll ~ peer_component")),
      data = est_fl[ok, ],
      vcov = ~player_id
    )
    iv_c <- coef(iv_fit)["fit_got_ll"]
    iv_s <- sqrt(vcov(iv_fit)["fit_got_ll", "fit_got_ll"])
    iv_p <- 2 * pnorm(-abs(iv_c / iv_s))

    first_ll_iv_results[[outcome]] <- tibble(
      outcome = outcome, spec = "LOO-IV first-LL-only",
      coef = iv_c, se = iv_s, pv = iv_p, n_obs = sum(ok)
    )
    message("  ", outcome, " (first-LL IV): coef = ", round(iv_c, 2),
            " (SE = ", round(iv_s, 2), "), p = ", round(iv_p, 3))
  }, error = function(e) message("  ", outcome, " FAILED: ", e$message))
}

first_ll_iv_df <- bind_rows(first_ll_iv_results)

# BH correction on first-LL-only IV
first_ll_iv_df <- first_ll_iv_df |>
  mutate(pv_bh = p.adjust(pv, method = "BH"))
}, error = function(e) {
  message("  LOO-IV on first-LL-only failed: ", e$message)
  message("  Continuing with GS lottery results only.")
  first_ll_iv_df <<- tibble(outcome = character(), coef = numeric(), se = numeric(), pv = numeric())
})

# Save Task 1
saveRDS(list(
  first_ll_only_sample = first_ll_only,
  gs_lottery = all_lottery_results,
  loo_iv = first_ll_iv_df,
  n_full = nrow(losers_only),
  n_first_ll = nrow(first_ll_only)
), file.path(CLEANED_DIR, "first_ll_only_results.rds"))
message("  Saved: first_ll_only_results.rds")


# ==============================================================================
# TASK 2: INTENSIVE MARGIN / TREATMENT DOSE
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 2: INTENSIVE MARGIN / TREATMENT DOSE")
message(strrep("=", 70))

# --- 2a. Compute treatment dose for LL recipients ---
message("\n--- 2a. Treatment dose: bonus matches won at LL event ---")

# ll_matches_won already computed during qualifiers build
# For LL recipients, this is the number of main draw matches won
# For non-recipients, this is 0 by definition

dose_data <- losers_only |>
  filter(n_ll_slots > 0) |>
  mutate(
    dose_group = case_when(
      got_ll == 0 ~ "Control",
      ll_matches_won == 0 ~ "LL: 0 wins",
      ll_matches_won == 1 ~ "LL: 1 win",
      ll_matches_won >= 2 ~ "LL: 2+ wins"
    ),
    dose_group = factor(dose_group, levels = c("Control", "LL: 0 wins", "LL: 1 win", "LL: 2+ wins"))
  )

message("  Dose distribution:")
dose_data |> count(dose_group) |> print()

# Estimate ranking points from GS round progression
# GS R128: ~10 pts, R64: ~25 pts, R32: ~50 pts, R16: ~90 pts
message("  Approximate bonus points by matches won:")
message("    0 wins (R1 loss): ~10 ranking points")
message("    1 win (R64 loss): ~25 ranking points")
message("    2 wins (R32 loss): ~50 ranking points")
message("    3+ wins: ~90+ ranking points")

# --- 2b. GS lottery: mean outcomes by dose group ---
message("\n--- 2b. Mean outcomes by dose group (GS lottery) ---")

gs_dose <- dose_data |>
  filter(tourney_level == "G", !is.na(rank_among_losers), rank_among_losers <= 4)

dose_outcomes <- c("rank_change_26w", "rank_change_52w",
                   "points_change_26w", "n_main_draws_26w", "n_matches_250plus_26w")

dose_means <- list()
for (out in dose_outcomes) {
  if (!out %in% names(gs_dose)) next
  dm <- gs_dose |>
    filter(!is.na(.data[[out]])) |>
    group_by(dose_group) |>
    summarise(
      mean_val = mean(.data[[out]], na.rm = TRUE),
      sd_val = sd(.data[[out]], na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) |>
    mutate(outcome = out)
  dose_means[[out]] <- dm
}
dose_means_df <- bind_rows(dose_means)

message("  Dose means (GS lottery sample):")
dose_means_df |> filter(outcome == "rank_change_26w") |> print()

# --- 2c. OLS interaction: outcome ~ got_ll + got_ll:ll_matches_won ---
message("\n--- 2c. OLS dose interaction ---")

dose_ols_results <- list()
for (out in dose_outcomes) {
  if (!out %in% names(gs_dose)) next
  y <- gs_dose[[out]]
  ok <- !is.na(y) & !is.na(gs_dose$player_rank)
  if (sum(ok) < 30) next

  tryCatch({
    fit <- feols(
      as.formula(paste0(out, " ~ got_ll + got_ll:ll_matches_won + player_rank + player_age")),
      data = gs_dose[ok, ],
      vcov = "hetero"
    )
    dose_ols_results[[out]] <- tibble(
      outcome = out,
      coef_got_ll = coef(fit)["got_ll"],
      coef_interaction = coef(fit)["got_ll:ll_matches_won"],
      se_got_ll = sqrt(vcov(fit)["got_ll", "got_ll"]),
      se_interaction = sqrt(vcov(fit)["got_ll:ll_matches_won", "got_ll:ll_matches_won"]),
      n = sum(ok)
    )
    message("  ", out, ": got_ll = ", round(coef(fit)["got_ll"], 2),
            ", interaction = ", round(coef(fit)["got_ll:ll_matches_won"], 2))
  }, error = function(e) message("  ", out, " OLS FAILED: ", e$message))
}
dose_ols_df <- bind_rows(dose_ols_results)

# --- 2d. Dose-response figure ---
message("\n--- 2d. Dose-response figure ---")

fig_dose_data <- gs_dose |>
  filter(!is.na(rank_change_26w)) |>
  mutate(matches_won_at_event = if_else(got_ll == 0, 0L,
                                          pmin(ll_matches_won, 3L))) |>
  group_by(matches_won_at_event, got_ll) |>
  summarise(
    mean_rank_change = mean(rank_change_26w, na.rm = TRUE),
    se_rank_change = sd(rank_change_26w, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  ) |>
  mutate(group = if_else(got_ll == 1, "LL recipient", "Control"))

p_dose <- ggplot(fig_dose_data, aes(x = matches_won_at_event, y = mean_rank_change,
                                     color = group, shape = group)) +
  geom_pointrange(aes(ymin = mean_rank_change - 1.96 * se_rank_change,
                       ymax = mean_rank_change + 1.96 * se_rank_change),
                  size = 0.8, position = position_dodge(0.2)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("Control" = col_control, "LL recipient" = col_treat)) +
  scale_x_continuous(breaks = 0:3, labels = c("0", "1", "2", "3+")) +
  labs(x = "Main draw matches won at LL event",
       y = "Mean ranking change (26 weeks)") +
  theme_paper()

ggsave(file.path(FIGURES_DIR, "fig_dose_response.pdf"), p_dose,
       width = 7, height = 5, device = cairo_pdf)
message("  Saved: fig_dose_response.pdf")

# --- 2e. Dose table ---
message("\n--- 2e. Dose table ---")

dose_tex <- c(
  "\\begin{tabular}{l ccc ccc}",
  "\\toprule",
  " & \\multicolumn{3}{c}{Mean Outcome} & \\multicolumn{3}{c}{OLS Interaction} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
  "Outcome & 0 wins & 1 win & 2+ wins & LL coef. & Interaction & $N$ \\\\",
  "\\midrule"
)

nice_dose_labels <- c(
  "rank_change_26w" = "Rank $\\Delta$ 26w",
  "rank_change_52w" = "Rank $\\Delta$ 52w",
  "points_change_26w" = "Points $\\Delta$ 26w",
  "n_main_draws_26w" = "Main draws 26w",
  "n_matches_250plus_26w" = "Matches (250+) 26w"
)

for (out in dose_outcomes) {
  label <- if (out %in% names(nice_dose_labels)) nice_dose_labels[out] else out
  dm <- dose_means_df |> filter(outcome == out)
  ols_r <- dose_ols_df |> filter(outcome == out)

  m0 <- dm |> filter(dose_group == "LL: 0 wins")
  m1 <- dm |> filter(dose_group == "LL: 1 win")
  m2 <- dm |> filter(dose_group == "LL: 2+ wins")

  v0 <- if (nrow(m0) > 0) sprintf("%.1f", m0$mean_val) else "--"
  v1 <- if (nrow(m1) > 0) sprintf("%.1f", m1$mean_val) else "--"
  v2 <- if (nrow(m2) > 0) sprintf("%.1f", m2$mean_val) else "--"

  ols_c <- if (nrow(ols_r) > 0) paste0(sprintf("%.2f", ols_r$coef_got_ll)) else "--"
  ols_i <- if (nrow(ols_r) > 0) paste0(sprintf("%.2f", ols_r$coef_interaction)) else "--"
  ols_n <- if (nrow(ols_r) > 0) format(ols_r$n, big.mark = ",") else "--"

  dose_tex <- c(dose_tex,
    paste0(label, " & ", v0, " & ", v1, " & ", v2,
           " & ", ols_c, " & ", ols_i, " & ", ols_n, " \\\\"))
}

dose_tex <- c(dose_tex, "\\bottomrule", "\\end{tabular}")
writeLines(dose_tex, file.path(TABLES_DIR, "table_dose_response.tex"))
writeLines(dose_tex, file.path(TABLES_DIR, "table_dose_final.tex"))
message("  Saved: table_dose_response.tex, table_dose_final.tex")

# Save Task 2
saveRDS(list(
  dose_means = dose_means_df,
  dose_ols = dose_ols_df,
  dose_data_summary = dose_data |> count(dose_group)
), file.path(CLEANED_DIR, "dose_analysis_results.rds"))
message("  Saved: dose_analysis_results.rds")


# ==============================================================================
# TASK 3: PREDICTED WIN PROBABILITY P(i->j) AS OUTCOME
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 3: PREDICTED WIN PROBABILITY P(i->j)")
message(strrep("=", 70))

# For each player-event, compute average P(i->j) across representative opponents
# using PRE-TREATMENT model parameters only

# Get main draw players at each tournament as the reference pool
message("  Building reference opponent pool...")

main_draw_opp <- function(main_df) {
  bind_rows(
    main_df |>
      mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
      filter(year >= 2000, !is.na(winner_rank)) |>
      transmute(tourney_id, opp_id = winner_id, opp_rank = winner_rank,
                opp_age = winner_age, opp_ioc = winner_ioc),
    main_df |>
      mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
      filter(year >= 2000, !is.na(loser_rank)) |>
      transmute(tourney_id, opp_id = loser_id, opp_rank = loser_rank,
                opp_age = loser_age, opp_ioc = loser_ioc)
  ) |> distinct(tourney_id, opp_id, .keep_all = TRUE)
}

atp_opp_pool <- main_draw_opp(atp_main)
wta_opp_pool <- main_draw_opp(wta_main)

# Compute P(i->j) for each player-event at horizons 0, 12, 26, 52
# Use the pre-treatment model (frozen at t=0)
compute_avg_pij <- function(player_data, opp_pool, model, horizon_rank_col = "player_rank") {
  pred_pairs <- player_data |>
    select(tourney_id, player_id, player_rank_h = !!sym(horizon_rank_col),
           player_age, player_ioc, surface) |>
    filter(!is.na(player_rank_h)) |>
    inner_join(opp_pool, by = "tourney_id", relationship = "many-to-many") |>
    filter(opp_id != player_id) |>
    mutate(
      log_rank_ratio = log(pmax(opp_rank, 1) / pmax(player_rank_h, 1)),
      rank_diff = opp_rank - player_rank_h,
      same_ioc = as.integer(player_ioc == opp_ioc),
      age_diff = coalesce(player_age - opp_age, 0),
      surface_clay = as.integer(surface == "Clay"),
      surface_grass = as.integer(surface == "Grass")
    ) |>
    filter(!is.na(log_rank_ratio))

  # Add extra columns if needed
  mt <- names(coef(model))
  ne <- any(str_detect(mt, "player_win_rate|is_gs"))
  if (ne) {
    if (!"player_win_rate" %in% names(pred_pairs)) pred_pairs$player_win_rate <- 0.5
    if (!"opponent_win_rate" %in% names(pred_pairs)) pred_pairs$opponent_win_rate <- 0.5
    if (!"h2h_win_rate" %in% names(pred_pairs)) pred_pairs$h2h_win_rate <- 0.5
    if (!"has_h2h" %in% names(pred_pairs)) pred_pairs$has_h2h <- 0L
    if (!"is_gs" %in% names(pred_pairs)) pred_pairs$is_gs <- 0L
    if (!"is_masters" %in% names(pred_pairs)) pred_pairs$is_masters <- 0L
    if (!"is_qual" %in% names(pred_pairs)) pred_pairs$is_qual <- 0L
    for (v in c("player_ace_rate_cum", "player_df_rate_cum", "player_1st_pct_cum",
                "player_1st_win_cum", "player_2nd_win_cum", "player_bp_save_cum",
                "player_ret_win_cum", "ht_diff", "hand_mismatch")) {
      if (v %in% mt && !v %in% names(pred_pairs)) {
        pred_pairs[[v]] <- if (!is.null(model$model) && v %in% names(model$model))
          median(model$model[[v]], na.rm = TRUE) else 0
      }
    }
  }

  pred_pairs$p_win <- predict(model, newdata = pred_pairs, type = "response")

  pred_pairs |>
    group_by(tourney_id, player_id) |>
    summarise(avg_pij = mean(p_win, na.rm = TRUE), n_opp = n(), .groups = "drop")
}

# Compute at baseline (using rank_t0) and at horizons 12, 26, 52
message("  Computing average P(i->j) at t=0...")
# For losers only (estimation sample)
losers_for_pij <- losers_only |> filter(n_ll_slots > 0)

# Split by tour
atp_losers_pij <- losers_for_pij |> filter(tour == "ATP")
wta_losers_pij <- losers_for_pij |> filter(tour == "WTA")

pij_t0_atp <- compute_avg_pij(atp_losers_pij |> mutate(player_rank = rank_t0),
                                atp_opp_pool, win_model_v2, "player_rank")
pij_t0_wta <- tryCatch(
  compute_avg_pij(wta_losers_pij |> mutate(player_rank = rank_t0),
                  wta_opp_pool, win_model_v2, "player_rank"),
  error = function(e) { message("  WTA P(i->j) failed: ", e$message); tibble() }
)

pij_t0 <- bind_rows(
  pij_t0_atp |> mutate(horizon = "t0"),
  pij_t0_wta |> mutate(horizon = "t0")
) |> rename(pij_t0 = avg_pij)

# At horizons using future ranking but same model (frozen)
for (h in c(12, 26, 52)) {
  message("  Computing average P(i->j) at t+", h, "w...")
  rank_col <- paste0("rank_t", h)

  h_atp <- tryCatch(
    compute_avg_pij(atp_losers_pij |> rename(player_rank_orig = player_rank) |>
                      mutate(player_rank = .data[[rank_col]]),
                    atp_opp_pool, win_model_v2, "player_rank"),
    error = function(e) tibble()
  )
  h_wta <- tryCatch(
    compute_avg_pij(wta_losers_pij |> rename(player_rank_orig = player_rank) |>
                      mutate(player_rank = .data[[rank_col]]),
                    wta_opp_pool, win_model_v2, "player_rank"),
    error = function(e) tibble()
  )

  combined_h <- bind_rows(h_atp, h_wta)
  col_name <- paste0("pij_t", h)
  pij_t0 <- pij_t0 |>
    left_join(combined_h |> select(tourney_id, player_id, !!col_name := avg_pij),
              by = c("tourney_id", "player_id"))
}

# Merge back and compute changes
losers_for_pij <- losers_for_pij |>
  left_join(pij_t0 |> select(tourney_id, player_id, pij_t0, pij_t12, pij_t26, pij_t52),
            by = c("tourney_id", "player_id")) |>
  mutate(
    pij_change_12w = pij_t12 - pij_t0,
    pij_change_26w = pij_t26 - pij_t0,
    pij_change_52w = pij_t52 - pij_t0
  )

message("  P(i->j) at t0 mean: ", round(mean(losers_for_pij$pij_t0, na.rm = TRUE), 4))
message("  P(i->j) change 26w mean: ", round(mean(losers_for_pij$pij_change_26w, na.rm = TRUE), 4))

# Run GS lottery on P(i->j)
gs_pij <- losers_for_pij |>
  filter(tourney_level == "G", !is.na(rank_among_losers), rank_among_losers <= 4)

pij_lottery <- run_lottery_diff(gs_pij,
  c("pij_change_12w", "pij_change_26w", "pij_change_52w"),
  "Pooled GS lottery (P(i->j))")

message("  P(i->j) lottery results:")
print(pij_lottery)

# Save Task 3
saveRDS(list(
  pij_data = losers_for_pij |> select(tourney_id, player_id, tour,
    pij_t0, pij_t12, pij_t26, pij_t52,
    pij_change_12w, pij_change_26w, pij_change_52w, got_ll),
  lottery_results = pij_lottery
), file.path(CLEANED_DIR, "win_prob_analysis.rds"))
message("  Saved: win_prob_analysis.rds")


# ==============================================================================
# TASK 4: COMPREHENSIVE DESCRIPTIVE STATISTICS
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 4: DESCRIPTIVE STATISTICS")
message(strrep("=", 70))

# --- 4a. Summary statistics table (4 panels) ---------------------------------
message("\n--- 4a. Summary statistics table ---")

make_balance_panel <- function(data, label) {
  treated <- data |> filter(got_ll == 1)
  control <- data |> filter(got_ll == 0)

  vars <- c("player_age", "player_rank", "player_rank_points")
  if ("elo_t0" %in% names(data)) vars <- c(vars, "elo_t0")
  if ("player_ht" %in% names(data) && sum(!is.na(data$player_ht)) > 10)
    vars <- c(vars, "player_ht")

  rows <- list()
  for (v in vars) {
    t_vals <- treated[[v]]
    c_vals <- control[[v]]
    t_ok <- !is.na(t_vals)
    c_ok <- !is.na(c_vals)

    if (sum(t_ok) < 3 || sum(c_ok) < 3) next

    tt <- tryCatch(t.test(t_vals[t_ok], c_vals[c_ok]), error = function(e) NULL)

    rows[[v]] <- tibble(
      panel = label, variable = v,
      mean_treated = mean(t_vals, na.rm = TRUE),
      sd_treated = sd(t_vals, na.rm = TRUE),
      mean_control = mean(c_vals, na.rm = TRUE),
      sd_control = sd(c_vals, na.rm = TRUE),
      diff = mean(t_vals, na.rm = TRUE) - mean(c_vals, na.rm = TRUE),
      pval = if (!is.null(tt)) tt$p.value else NA_real_,
      n_treated = sum(t_ok), n_control = sum(c_ok)
    )
  }
  bind_rows(rows)
}

gs_atp_balance <- make_balance_panel(gs_atp_full, "GS Lottery (ATP)")
gs_wta_balance <- make_balance_panel(gs_wta_full, "GS Lottery (WTA)")

nongs_atp <- losers_only |> filter(tour == "ATP", tourney_level != "G", n_ll_slots > 0)
nongs_balance <- make_balance_panel(nongs_atp, "Non-GS Selection Model (ATP)")

fl_balance <- make_balance_panel(first_ll_only |> filter(n_ll_slots > 0), "First-LL-Only")

summary_stats <- bind_rows(gs_atp_balance, gs_wta_balance, nongs_balance, fl_balance)

# Write LaTeX table
nice_var_labels <- c(
  "player_age" = "Age",
  "player_rank" = "ATP/WTA Ranking",
  "player_rank_points" = "Ranking Points",
  "elo_t0" = "Elo Rating",
  "player_ht" = "Height (cm)"
)

panels <- unique(summary_stats$panel)
ss_tex <- c(
  "\\begin{tabular}{l cccc cc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Treated} & \\multicolumn{2}{c}{Control} & & \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  "Variable & Mean & SD & Mean & SD & Diff. & $p$ \\\\",
  "\\midrule"
)

for (panel in panels) {
  ss_tex <- c(ss_tex, paste0("\\multicolumn{7}{l}{\\textit{", panel, "}} \\\\"))
  p_data <- summary_stats |> filter(panel == !!panel)
  for (i in seq_len(nrow(p_data))) {
    r <- p_data[i, ]
    vl <- if (r$variable %in% names(nice_var_labels)) nice_var_labels[r$variable] else r$variable
    ss_tex <- c(ss_tex,
      paste0("\\quad ", vl, " & ",
             sprintf("%.1f", r$mean_treated), " & ",
             sprintf("%.1f", r$sd_treated), " & ",
             sprintf("%.1f", r$mean_control), " & ",
             sprintf("%.1f", r$sd_control), " & ",
             sprintf("%.1f", r$diff), " & ",
             sprintf("%.3f", r$pval), " \\\\"))
  }
  # N row
  n_t <- p_data$n_treated[1]
  n_c <- p_data$n_control[1]
  ss_tex <- c(ss_tex,
    paste0("\\quad $N$ & \\multicolumn{2}{c}{", n_t, "} & \\multicolumn{2}{c}{", n_c,
           "} & & \\\\"),
    "\\addlinespace")
}

ss_tex <- c(ss_tex, "\\bottomrule", "\\end{tabular}")
writeLines(ss_tex, file.path(TABLES_DIR, "table_summary_stats_final.tex"))
message("  Saved: table_summary_stats_final.tex")

# --- 4b. LL distribution by GS ---
message("\n--- 4b. LL distribution by GS ---")

gs_dist <- losers_only |>
  filter(tourney_level == "G") |>
  mutate(
    slam = str_extract(tourney_name, "Australian|Roland|Wimbledon|US"),
    slam = replace_na(slam, "Other")
  ) |>
  group_by(tour, slam) |>
  summarise(n_candidates = n(), n_ll = sum(got_ll), .groups = "drop") |>
  mutate(ll_rate = n_ll / n_candidates)

gs_dist_tex <- c(
  "\\begin{tabular}{llccc}",
  "\\toprule",
  "Tour & Grand Slam & Candidates & LL Entries & LL Rate \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(gs_dist))) {
  r <- gs_dist[i, ]
  gs_dist_tex <- c(gs_dist_tex,
    paste0(r$tour, " & ", r$slam, " & ", r$n_candidates, " & ",
           r$n_ll, " & ", sprintf("%.3f", r$ll_rate), " \\\\"))
}
gs_dist_tex <- c(gs_dist_tex, "\\bottomrule", "\\end{tabular}")
writeLines(gs_dist_tex, file.path(TABLES_DIR, "table_ll_distribution_gs_final.tex"))
message("  Saved: table_ll_distribution_gs_final.tex")

# --- 4c. LL distribution by non-GS tournament level ---
message("\n--- 4c. LL distribution by tournament level (non-GS) ---")

nongs_dist <- losers_only |>
  filter(tourney_level != "G") |>
  group_by(tour, tourney_level) |>
  summarise(n_candidates = n(), n_ll = sum(got_ll), ll_rate = mean(got_ll), .groups = "drop")

nongs_tex <- c(
  "\\begin{tabular}{llccc}",
  "\\toprule",
  "Tour & Level & Candidates & LL Entries & LL Rate \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(nongs_dist))) {
  r <- nongs_dist[i, ]
  nongs_tex <- c(nongs_tex,
    paste0(r$tour, " & ", r$tourney_level, " & ", format(r$n_candidates, big.mark = ","),
           " & ", r$n_ll, " & ", sprintf("%.3f", r$ll_rate), " \\\\"))
}
nongs_tex <- c(nongs_tex, "\\bottomrule", "\\end{tabular}")
writeLines(nongs_tex, file.path(TABLES_DIR, "table_ll_distribution_nongs_final.tex"))
message("  Saved: table_ll_distribution_nongs_final.tex")

# --- 4d. LL careers two-way table ---
message("\n--- 4d. LL careers two-way table ---")

player_ll_careers <- losers_only |>
  filter(n_ll_slots > 0) |>
  mutate(is_gs = tourney_level == "G") |>
  group_by(player_id, is_gs) |>
  summarise(n_opps = n(), n_won = sum(got_ll), .groups = "drop") |>
  mutate(
    opp_bin = case_when(n_opps == 1 ~ "1", n_opps == 2 ~ "2", n_opps == 3 ~ "3",
                         n_opps == 4 ~ "4", n_opps >= 5 ~ "5+"),
    won_bin = case_when(n_won == 0 ~ "0", n_won == 1 ~ "1", n_won == 2 ~ "2", n_won >= 3 ~ "3+"),
    type = if_else(is_gs, "GS", "Non-GS")
  )

tw_table <- player_ll_careers |>
  count(type, opp_bin, won_bin) |>
  pivot_wider(names_from = c(type, won_bin), values_from = n, values_fill = 0L)

opp_levels <- c("1", "2", "3", "4", "5+")
won_levels <- c("0", "1", "2", "3+")

tw_tex <- c(
  "\\begin{tabular}{l cccc cccc}",
  "\\toprule",
  " & \\multicolumn{4}{c}{Grand Slam} & \\multicolumn{4}{c}{Non-Grand Slam} \\\\",
  "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9}",
  paste0("Opportunities & ", paste(paste0("Won=", won_levels), collapse = " & "),
         " & ", paste(paste0("Won=", won_levels), collapse = " & "), " \\\\"),
  "\\midrule"
)
for (opp in opp_levels) {
  row_vals <- c()
  for (type in c("GS", "Non-GS")) {
    for (won in won_levels) {
      col_name <- paste0(type, "_", won)
      val <- tw_table |> filter(opp_bin == opp)
      if (nrow(val) > 0 && col_name %in% names(val)) {
        row_vals <- c(row_vals, as.character(val[[col_name]]))
      } else {
        row_vals <- c(row_vals, "0")
      }
    }
  }
  tw_tex <- c(tw_tex, paste0(opp, " & ", paste(row_vals, collapse = " & "), " \\\\"))
}
tw_tex <- c(tw_tex, "\\bottomrule", "\\end{tabular}")
writeLines(tw_tex, file.path(TABLES_DIR, "table_ll_careers_final.tex"))
message("  Saved: table_ll_careers_final.tex")

# --- 4e. Top/bottom 5 by LL success rate ---
message("\n--- 4e. LL success rates ---")

player_success <- losers_only |>
  filter(n_ll_slots > 0) |>
  group_by(player_id) |>
  summarise(
    player_name = first(na.omit(player_name)),
    tour = first(tour),
    n_opps = n(), n_ll = sum(got_ll), rate = mean(got_ll),
    first_year = min(year), last_year = max(year),
    .groups = "drop"
  ) |>
  filter(n_opps >= 5)

top5 <- player_success |> arrange(desc(rate), desc(n_opps)) |> head(5)
bot5 <- player_success |> arrange(rate, desc(n_opps)) |> head(5)

sr_tex <- c(
  "\\begin{tabular}{llccccc}",
  "\\toprule",
  "Player & Tour & Opportunities & LL Entries & Success Rate & Years \\\\",
  "\\midrule",
  "\\multicolumn{6}{l}{\\textit{Highest success rate ($\\geq$ 5 opportunities)}} \\\\"
)
for (i in seq_len(nrow(top5))) {
  r <- top5[i, ]
  sr_tex <- c(sr_tex,
    paste0(r$player_name, " & ", r$tour, " & ", r$n_opps, " & ", r$n_ll,
           " & ", sprintf("%.0f\\%%", r$rate * 100),
           " & ", r$first_year, "--", r$last_year, " \\\\"))
}
sr_tex <- c(sr_tex,
  "\\addlinespace",
  "\\multicolumn{6}{l}{\\textit{Lowest success rate ($\\geq$ 5 opportunities)}} \\\\"
)
for (i in seq_len(nrow(bot5))) {
  r <- bot5[i, ]
  sr_tex <- c(sr_tex,
    paste0(r$player_name, " & ", r$tour, " & ", r$n_opps, " & ", r$n_ll,
           " & ", sprintf("%.0f\\%%", r$rate * 100),
           " & ", r$first_year, "--", r$last_year, " \\\\"))
}
sr_tex <- c(sr_tex, "\\bottomrule", "\\end{tabular}")
writeLines(sr_tex, file.path(TABLES_DIR, "table_ll_success_rates_final.tex"))
message("  Saved: table_ll_success_rates_final.tex")

# --- 4f-4g. Sample period documentation and counts ---------------------------
message("\n--- 4f-4g. Sample counts ---")

sample_counts <- list()
sample_counts$full_gs_lottery <- nrow(gs_full)
sample_counts$gs_lottery_atp <- nrow(gs_atp_full)
sample_counts$gs_lottery_wta <- nrow(gs_wta_full)
sample_counts$gs_lottery_pooled <- nrow(gs_full)
sample_counts$first_ll_gs <- nrow(gs_first_ll)
sample_counts$first_ll_gs_atp <- nrow(gs_atp_first)
sample_counts$first_ll_gs_wta <- nrow(gs_wta_first)
sample_counts$nongs_selection_model <- nrow(nongs_atp)
sample_counts$first_ll_nongs_iv <- nrow(est_fl)
sample_counts$all_losers <- nrow(losers_only)
sample_counts$first_ll_all <- nrow(first_ll_only)
sample_counts$year_range_atp <- paste0(min(losers_only$year[losers_only$tour == "ATP"]),
                                        "-", max(losers_only$year[losers_only$tour == "ATP"]))
sample_counts$year_range_wta <- paste0(min(losers_only$year[losers_only$tour == "WTA"]),
                                        "-", max(losers_only$year[losers_only$tour == "WTA"]))

counts_md <- c(
  "# Sample Counts",
  paste0("\nGenerated: ", Sys.time()),
  "",
  "## Grand Slam Lottery",
  paste0("- Full GS lottery (pooled): ", sample_counts$full_gs_lottery),
  paste0("  - ATP: ", sample_counts$gs_lottery_atp),
  paste0("  - WTA: ", sample_counts$gs_lottery_wta),
  paste0("- First-LL-only GS: ", sample_counts$first_ll_gs),
  paste0("  - ATP: ", sample_counts$first_ll_gs_atp),
  paste0("  - WTA: ", sample_counts$first_ll_gs_wta),
  "",
  "## Non-GS Selection Model",
  paste0("- ATP selection model: ", sample_counts$nongs_selection_model),
  paste0("- First-LL-only IV: ", sample_counts$first_ll_nongs_iv),
  "",
  "## Full Samples",
  paste0("- All qualifying losers: ", sample_counts$all_losers),
  paste0("- First-LL-only (all): ", sample_counts$first_ll_all),
  "",
  "## Year Ranges",
  paste0("- ATP: ", sample_counts$year_range_atp),
  paste0("- WTA: ", sample_counts$year_range_wta)
)
writeLines(counts_md, file.path(OUTPUT_DIR, "sample_counts.md"))
message("  Saved: sample_counts.md")


# ==============================================================================
# TASK 5: MAIN RESULTS TABLES (FINAL)
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 5: MAIN RESULTS TABLES")
message(strrep("=", 70))

# --- 5a. GS Lottery Main Results ---
message("\n--- 5a. GS Lottery main results table ---")

# Combine full and first-LL-only
lottery_for_table <- all_lottery_results |>
  filter(sample %in% c("ATP full", "WTA full", "ATP first-LL-only", "WTA first-LL-only"))

nice_out_labels <- c(
  "rank_change_4w" = "Rank $\\Delta$ 4w",
  "rank_change_8w" = "Rank $\\Delta$ 8w",
  "rank_change_12w" = "Rank $\\Delta$ 12w",
  "rank_change_26w" = "Rank $\\Delta$ 26w",
  "rank_change_52w" = "Rank $\\Delta$ 52w",
  "points_change_12w" = "Points $\\Delta$ 12w",
  "points_change_26w" = "Points $\\Delta$ 26w",
  "points_change_52w" = "Points $\\Delta$ 52w",
  "elo_change_12w" = "Elo $\\Delta$ 12w",
  "elo_change_26w" = "Elo $\\Delta$ 26w",
  "n_main_draws_26w" = "Main draws 26w",
  "n_main_draws_52w" = "Main draws 52w",
  "n_matches_250plus_26w" = "Matches (250+) 26w",
  "n_matches_250plus_52w" = "Matches (250+) 52w",
  "direct_entry_next_year" = "Direct entry next year"
)

main_lottery_tex <- c(
  "\\begin{tabular}{l cccc cccc}",
  "\\toprule",
  " & \\multicolumn{4}{c}{Full Sample} & \\multicolumn{4}{c}{First-LL-Only} \\\\",
  "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9}",
  " & \\multicolumn{2}{c}{ATP} & \\multicolumn{2}{c}{WTA} & \\multicolumn{2}{c}{ATP} & \\multicolumn{2}{c}{WTA} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7} \\cmidrule(lr){8-9}",
  "Outcome & Diff. & $p_{\\text{BH}}$ & Diff. & $p_{\\text{BH}}$ & Diff. & $p_{\\text{BH}}$ & Diff. & $p_{\\text{BH}}$ \\\\",
  "\\midrule"
)

outcome_order <- c("rank_change_4w", "rank_change_8w", "rank_change_12w", "rank_change_26w",
                    "rank_change_52w", "points_change_12w", "points_change_26w", "points_change_52w",
                    "elo_change_12w", "elo_change_26w",
                    "n_main_draws_26w", "n_main_draws_52w",
                    "n_matches_250plus_26w", "n_matches_250plus_52w", "direct_entry_next_year")

for (out in outcome_order) {
  label <- if (out %in% names(nice_out_labels)) nice_out_labels[out] else out

  vals <- c()
  for (samp in c("ATP full", "WTA full", "ATP first-LL-only", "WTA first-LL-only")) {
    r <- lottery_for_table |> filter(sample == samp, outcome == out)
    if (nrow(r) > 0) {
      vals <- c(vals, paste0(sprintf("%.1f", r$diff), add_stars(r$pv)),
                sprintf("%.3f", r$pv_bh))
    } else {
      vals <- c(vals, "--", "--")
    }
  }
  main_lottery_tex <- c(main_lottery_tex,
    paste0(label, " & ", paste(vals, collapse = " & "), " \\\\"))
}

# Add N row
n_atp_f <- gs_atp_full |> summarise(nt = sum(got_ll), nc = sum(!got_ll))
n_wta_f <- gs_wta_full |> summarise(nt = sum(got_ll), nc = sum(!got_ll))
n_atp_fl <- gs_atp_first |> summarise(nt = sum(got_ll), nc = sum(!got_ll))
n_wta_fl <- gs_wta_first |> summarise(nt = sum(got_ll), nc = sum(!got_ll))

main_lottery_tex <- c(main_lottery_tex,
  "\\midrule",
  paste0("$N$ (treated/control) & \\multicolumn{2}{c}{",
         n_atp_f$nt, "/", n_atp_f$nc, "} & \\multicolumn{2}{c}{",
         n_wta_f$nt, "/", n_wta_f$nc, "} & \\multicolumn{2}{c}{",
         n_atp_fl$nt, "/", n_atp_fl$nc, "} & \\multicolumn{2}{c}{",
         n_wta_fl$nt, "/", n_wta_fl$nc, "} \\\\"),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(main_lottery_tex, file.path(TABLES_DIR, "table_main_lottery_results_final.tex"))
message("  Saved: table_main_lottery_results_final.tex")

# --- 5b. LOO-IV Main Results ---
message("\n--- 5b. LOO-IV main results table ---")

# Load the full-sample LOO-IV from script 13
loo_full <- tryCatch(read_rds(file.path(CLEANED_DIR, "leave_one_out_iv_results.rds")),
                      error = function(e) { message("  leave_one_out_iv_results.rds not found"); NULL })

iv_full_df <- if (!is.null(loo_full) && "loo_iv" %in% names(loo_full)) loo_full$loo_iv else tibble()
iv_fl_df <- first_ll_iv_df

iv_table_tex <- c(
  "\\begin{tabular}{l cccc cccc}",
  "\\toprule",
  " & \\multicolumn{4}{c}{Full Sample} & \\multicolumn{4}{c}{First-LL-Only} \\\\",
  "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9}",
  "Outcome & Coef. & SE & $p$ & $p_{\\text{BH}}$ & Coef. & SE & $p$ & $p_{\\text{BH}}$ \\\\",
  "\\midrule"
)

for (out in outcome_order) {
  label <- if (out %in% names(nice_out_labels)) nice_out_labels[out] else out

  r_f <- iv_full_df |> filter(outcome == out)
  r_fl <- iv_fl_df |> filter(outcome == out)

  if (nrow(r_f) > 0) {
    f_vals <- paste0(sprintf("%.2f", r_f$coef), add_stars(r_f$pv), " & ",
                     sprintf("%.2f", r_f$se), " & ",
                     sprintf("%.3f", r_f$pv), " & ",
                     if ("pv_bh" %in% names(r_f)) sprintf("%.3f", r_f$pv_bh) else "--")
  } else {
    f_vals <- "-- & -- & -- & --"
  }

  if (nrow(r_fl) > 0) {
    fl_vals <- paste0(sprintf("%.2f", r_fl$coef), add_stars(r_fl$pv), " & ",
                      sprintf("%.2f", r_fl$se), " & ",
                      sprintf("%.3f", r_fl$pv), " & ",
                      sprintf("%.3f", r_fl$pv_bh))
  } else {
    fl_vals <- "-- & -- & -- & --"
  }

  iv_table_tex <- c(iv_table_tex,
    paste0(label, " & ", f_vals, " & ", fl_vals, " \\\\"))
}

iv_table_tex <- c(iv_table_tex,
  "\\midrule",
  "Player controls & \\multicolumn{4}{c}{Yes} & \\multicolumn{4}{c}{Yes} \\\\",
  "Year FE & \\multicolumn{4}{c}{Yes} & \\multicolumn{4}{c}{Yes} \\\\",
  "Clustered (player) & \\multicolumn{4}{c}{Yes} & \\multicolumn{4}{c}{Yes} \\\\",
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(iv_table_tex, file.path(TABLES_DIR, "table_main_iv_results_final.tex"))
message("  Saved: table_main_iv_results_final.tex")

# --- 5d. Tournament Performance Model ---
message("\n--- 5d. Tournament model table ---")

if (!is.null(tournament_results)) {
  tm_tex <- c(
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    " & $\\delta$ (log-odds) & SE & Odds Ratio & $p$-value \\\\",
    "\\midrule"
  )

  # Extract key results
  delta <- tournament_results$delta
  se <- tournament_results$se_analytic
  or <- tournament_results$odds_ratio
  pv <- tournament_results$pv_analytic

  if (!is.null(delta)) {
    tm_tex <- c(tm_tex,
      paste0("Pooled CF-IV & ", sprintf("%.4f", delta), add_stars(pv), " & ",
             sprintf("%.4f", se), " & ", sprintf("%.4f", or), " & ",
             sprintf("%.4f", pv), " \\\\"))
  }

  # Robustness
  if (!is.null(tournament_results$delta_first_ll)) {
    tm_tex <- c(tm_tex,
      paste0("First-LL only & ", sprintf("%.4f", tournament_results$delta_first_ll),
             " & & & \\\\"))
  }

  tm_tex <- c(tm_tex, "\\bottomrule", "\\end{tabular}")
  writeLines(tm_tex, file.path(TABLES_DIR, "table_tournament_model_final.tex"))
  message("  Saved: table_tournament_model_final.tex")
} else {
  message("  Tournament model results not available; skipping table")
}


# ==============================================================================
# TASK 6: KEY FIGURES (FINAL)
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 6: KEY FIGURES")
message(strrep("=", 70))

# --- 6a. Lottery event study (ATP + WTA panels) ---
message("\n--- 6a. Event study figure ---")

# Build trajectory data for GS lottery sample
horizon_weeks <- c(-12, -8, -4, 0, 4, 8, 12, 26, 52)

build_trajectory <- function(data, tour_label) {
  # For each horizon, compute mean ranking LEVEL by treatment status
  traj <- list()
  for (hw in horizon_weeks) {
    if (hw <= 0) {
      # Pre-treatment: rank at t0 + hw weeks relative to event
      # Approximate: use rank_t0 as baseline (we don't have pre-treatment rankings)
      # For hw = 0, use rank_t0
      if (hw == 0) {
        col <- "rank_t0"
      } else {
        # Pre-treatment approximation: if we have it, use it; else skip
        col <- paste0("rank_t", abs(hw))
        # These are POST-treatment horizons named differently
        # We need to look backwards -- use rank_t0 for all pre-treatment
        col <- "rank_t0"  # Simplification: pre-treatment all at rank_t0
      }
    } else {
      col <- paste0("rank_t", hw)
    }

    if (!col %in% names(data)) next

    vals <- data |>
      filter(!is.na(.data[[col]])) |>
      group_by(got_ll) |>
      summarise(
        mean_rank = mean(.data[[col]], na.rm = TRUE),
        se_rank = sd(.data[[col]], na.rm = TRUE) / sqrt(n()),
        n = n(),
        .groups = "drop"
      ) |>
      mutate(week = hw, tour = tour_label)

    traj[[length(traj) + 1]] <- vals
  }
  bind_rows(traj)
}

# Use actual post-treatment horizons
horizon_actual <- c(0, 4, 8, 12, 26, 52)

build_trajectory_actual <- function(data, tour_label) {
  traj <- list()
  for (hw in horizon_actual) {
    col <- paste0("rank_t", hw)
    if (!col %in% names(data)) next
    vals <- data |>
      filter(!is.na(.data[[col]])) |>
      group_by(got_ll) |>
      summarise(
        mean_rank = mean(.data[[col]], na.rm = TRUE),
        se_rank = sd(.data[[col]], na.rm = TRUE) / sqrt(n()),
        n = n(),
        .groups = "drop"
      ) |>
      mutate(week = hw, tour = tour_label)
    traj[[length(traj) + 1]] <- vals
  }
  bind_rows(traj)
}

traj_atp <- build_trajectory_actual(gs_atp_full, "ATP")
traj_wta <- build_trajectory_actual(gs_wta_full, "WTA")
traj_all <- bind_rows(traj_atp, traj_wta) |>
  mutate(group = if_else(got_ll == 1, "LL recipient", "Control"))

p_event_atp <- ggplot(traj_all |> filter(tour == "ATP"),
                       aes(x = week, y = mean_rank, color = group, shape = group)) +
  geom_pointrange(aes(ymin = mean_rank - 1.96 * se_rank,
                       ymax = mean_rank + 1.96 * se_rank),
                  size = 0.6, position = position_dodge(1.5)) +
  geom_line(aes(group = group), linewidth = 0.5) +
  scale_y_reverse() +
  scale_color_manual(values = c("Control" = col_control, "LL recipient" = col_treat)) +
  scale_x_continuous(breaks = horizon_actual) +
  labs(x = "Weeks after qualifying loss", y = "Mean ranking (lower = better)") +
  theme_paper() +
  theme(legend.position = "none")

p_event_wta <- ggplot(traj_all |> filter(tour == "WTA"),
                       aes(x = week, y = mean_rank, color = group, shape = group)) +
  geom_pointrange(aes(ymin = mean_rank - 1.96 * se_rank,
                       ymax = mean_rank + 1.96 * se_rank),
                  size = 0.6, position = position_dodge(1.5)) +
  geom_line(aes(group = group), linewidth = 0.5) +
  scale_y_reverse() +
  scale_color_manual(values = c("Control" = col_control, "LL recipient" = col_treat)) +
  scale_x_continuous(breaks = horizon_actual) +
  labs(x = "Weeks after qualifying loss", y = "Mean ranking (lower = better)") +
  theme_paper()

p_event_combined <- p_event_atp + p_event_wta +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = "A")

ggsave(file.path(FIGURES_DIR, "fig_event_study_final.pdf"), p_event_combined,
       width = 12, height = 5, device = cairo_pdf)
message("  Saved: fig_event_study_final.pdf")

# --- 6c. Ranking distribution ---
message("\n--- 6c. Ranking distribution figure ---")

dist_data <- losers_only |>
  filter(n_ll_slots > 0, !is.na(rank_t0)) |>
  mutate(
    sample = case_when(
      tourney_level == "G" ~ "GS Lottery",
      TRUE ~ "Non-GS Selection Model"
    )
  )

p_dist <- ggplot(dist_data, aes(x = rank_t0, fill = sample)) +
  geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
  scale_fill_manual(values = c("GS Lottery" = col_treat, "Non-GS Selection Model" = col_control)) +
  labs(x = "Pre-treatment ATP/WTA ranking", y = "Count") +
  theme_paper()

ggsave(file.path(FIGURES_DIR, "fig_ranking_distribution.pdf"), p_dist,
       width = 7, height = 5, device = cairo_pdf)
message("  Saved: fig_ranking_distribution.pdf")

# --- 6d. First-stage figure for LOO-IV ---
message("\n--- 6d. First-stage IV figure ---")

if (nrow(est_fl) > 0 && "peer_component" %in% names(est_fl)) {
  # Binned scatter of peer_component vs LL entry rate
  n_bins <- 20
  fs_plot_data <- est_fl |>
    filter(!is.na(peer_component)) |>
    mutate(bin = ntile(peer_component, n_bins)) |>
    group_by(bin) |>
    summarise(
      mean_peer = mean(peer_component, na.rm = TRUE),
      ll_rate = mean(got_ll, na.rm = TRUE),
      n = n(),
      se = sqrt(ll_rate * (1 - ll_rate) / n),
      .groups = "drop"
    )

  p_fs <- ggplot(fs_plot_data, aes(x = mean_peer, y = ll_rate)) +
    geom_pointrange(aes(ymin = pmax(ll_rate - 1.96 * se, 0),
                         ymax = pmin(ll_rate + 1.96 * se, 1)),
                    color = col_treat, size = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "grey30", linewidth = 0.8) +
    labs(x = "Peer component (leave-one-out instrument)",
         y = "Lucky Loser entry rate") +
    theme_paper()

  ggsave(file.path(FIGURES_DIR, "fig_first_stage_iv.pdf"), p_fs,
         width = 7, height = 5, device = cairo_pdf)
  message("  Saved: fig_first_stage_iv.pdf")
} else {
  message("  First-stage figure: insufficient data, skipping")
}


# ==============================================================================
# FINAL SUMMARY
# ==============================================================================
message("\n", strrep("=", 70))
message("WRITING FINAL SUMMARY")
message(strrep("=", 70))

# Collect key results
summary_md <- c(
  "# Final Analysis Summary",
  paste0("\nGenerated: ", Sys.time()),
  "",
  "## Sample Sizes",
  paste0("- All qualifying losers (ATP + WTA): ", nrow(losers_only)),
  paste0("- First-LL-only: ", nrow(first_ll_only)),
  paste0("- GS lottery (full, pooled): ", nrow(gs_full),
         " (LL: ", sum(gs_full$got_ll), ")"),
  paste0("- GS lottery (first-LL-only): ", nrow(gs_first_ll),
         " (LL: ", sum(gs_first_ll$got_ll), ")"),
  paste0("- LOO-IV estimation (first-LL-only): ", nrow(est_fl)),
  "",
  "## Task 1: First-LL-Only GS Lottery (Pooled)",
  ""
)

fl_pooled <- all_lottery_results |> filter(sample == "Pooled first-LL-only")
for (i in seq_len(nrow(fl_pooled))) {
  r <- fl_pooled[i, ]
  summary_md <- c(summary_md,
    paste0("- ", r$outcome, ": diff = ", sprintf("%.2f", r$diff),
           ", p = ", sprintf("%.3f", r$pv),
           ", p_BH = ", sprintf("%.3f", r$pv_bh)))
}

summary_md <- c(summary_md,
  "",
  "## Task 1: First-LL-Only LOO-IV",
  ""
)

for (i in seq_len(nrow(first_ll_iv_df))) {
  r <- first_ll_iv_df[i, ]
  summary_md <- c(summary_md,
    paste0("- ", r$outcome, ": coef = ", sprintf("%.2f", r$coef),
           " (SE = ", sprintf("%.2f", r$se), "), p = ", sprintf("%.3f", r$pv),
           ", p_BH = ", sprintf("%.3f", r$pv_bh)))
}

summary_md <- c(summary_md,
  "",
  "## Task 2: Dose-Response",
  ""
)

dose_summary <- dose_means_df |> filter(outcome == "rank_change_26w")
for (i in seq_len(nrow(dose_summary))) {
  r <- dose_summary[i, ]
  summary_md <- c(summary_md,
    paste0("- ", r$dose_group, ": mean rank change 26w = ", sprintf("%.1f", r$mean_val),
           " (n = ", r$n, ")"))
}

summary_md <- c(summary_md,
  "",
  "## Task 3: Predicted Win Probability P(i->j)",
  paste0("- P(i->j) at t0 mean: ",
         round(mean(losers_for_pij$pij_t0, na.rm = TRUE), 4)),
  paste0("- P(i->j) change 26w: ",
         round(mean(losers_for_pij$pij_change_26w, na.rm = TRUE), 4)),
  ""
)

if (nrow(pij_lottery) > 0) {
  summary_md <- c(summary_md, "### P(i->j) Lottery Results")
  for (i in seq_len(nrow(pij_lottery))) {
    r <- pij_lottery[i, ]
    summary_md <- c(summary_md,
      paste0("- ", r$outcome, ": diff = ", sprintf("%.4f", r$diff),
             ", p = ", if (!is.na(r$pv)) sprintf("%.3f", r$pv) else "NA"))
  }
}

summary_md <- c(summary_md,
  "",
  "## Output Files",
  "",
  "### Data",
  "- Data/cleaned/first_ll_only_results.rds",
  "- Data/cleaned/dose_analysis_results.rds",
  "- Data/cleaned/win_prob_analysis.rds",
  "",
  "### Tables",
  "- Tables/table_summary_stats_final.tex",
  "- Tables/table_ll_distribution_gs_final.tex",
  "- Tables/table_ll_distribution_nongs_final.tex",
  "- Tables/table_ll_careers_final.tex",
  "- Tables/table_ll_success_rates_final.tex",
  "- Tables/table_main_lottery_results_final.tex",
  "- Tables/table_main_iv_results_final.tex",
  "- Tables/table_dose_response.tex",
  "- Tables/table_dose_final.tex",
  "- Tables/table_tournament_model_final.tex",
  "",
  "### Figures",
  "- Figures/fig_event_study_final.pdf",
  "- Figures/fig_dose_response.pdf",
  "- Figures/fig_ranking_distribution.pdf",
  "- Figures/fig_first_stage_iv.pdf",
  "",
  "### Documentation",
  "- Output/sample_counts.md"
)

writeLines(summary_md, file.path(OUTPUT_DIR, "final_analysis_summary.md"))
message("  Saved: final_analysis_summary.md")

message("\n", strrep("=", 70))
message("14_final_analysis.R COMPLETE")
message(strrep("=", 70))
