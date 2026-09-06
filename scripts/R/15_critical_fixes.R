# ==============================================================================
# 15_critical_fixes.R
# Critical fixes for the Lucky Losers analysis pipeline.
#
# Fixes:
#   1. GS sample starts 2006 (lottery attribution post-Wimbledon 2005)
#   2. Compute selection probs for ALL non-GS tournaments (not just 620)
#   3. Control group restricted to events with >= 1 LL spot actually awarded
#   4. Address access outcome confound (pre-treatment ranking controls)
#   5. Instrument = peer_component only (no own_loss_prob)
#   6. Fix "Other" in GS table (map all GS tourney names to 4 slams)
#   7. Rebuild all descriptive tables
#   8. Report exact sample counts
#
# Inputs:  Data/raw/*.rds, Data/cleaned/*.rds
# Outputs: Data/cleaned/selection_probabilities_full.rds
#          Data/cleaned/estimation_sample_v2.rds
#          Tables/table_summary_stats_v2.tex
#          Tables/table_ll_distribution_gs_v2.tex
#          Tables/table_ll_distribution_nongs_v2.tex
#          Tables/table_ll_careers_v2.tex
#          Tables/table_ll_success_rates_v2.tex
#          Tables/table_main_lottery_v2.tex
#          Tables/table_main_iv_v2.tex
#          Output/sample_counts_v2.md
# Dependencies: dplyr, tidyr, readr, stringr, ggplot2, fixest, here
# Project: Lucky Losers and Career Trajectories
# ==============================================================================

set.seed(20260323)

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
for (d in c(CLEANED_DIR, TABLES_DIR, FIGURES_DIR, OUTPUT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# --- Helper: significance stars -----------------------------------------------
add_stars <- function(pv) {
  ifelse(pv < 0.01, "$^{***}$",
         ifelse(pv < 0.05, "$^{**}$",
                ifelse(pv < 0.1, "$^{*}$", "")))
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

# --- Bernoulli convolution (from scripts 11/14) ------------------------------
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

# Verify
stopifnot(abs(bernoulli_conv_ge(0.5, 1) - 0.5) < 1e-10)
stopifnot(abs(bernoulli_conv_ge(c(0.5, 0.5), 1) - 0.75) < 1e-10)

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

elo_history <- tryCatch(read_rds(file.path(CLEANED_DIR, "elo_history.rds")),
                         error = function(e) { message("  elo_history not found"); NULL })
wta_elo_history <- tryCatch(read_rds(file.path(CLEANED_DIR, "wta_elo_history.rds")),
                              error = function(e) { message("  wta_elo_history not found"); NULL })
win_model_v2 <- tryCatch(read_rds(file.path(CLEANED_DIR, "win_model_v2.rds")),
                           error = function(e) { message("  win_model_v2 not found"); NULL })

message("  ATP main: ", nrow(atp_main), " matches")
message("  ATP qual: ", nrow(atp_qual), " matches")
message("  WTA main: ", nrow(wta_main), " matches")
message("  WTA qual: ", nrow(wta_qual), " matches")

# Summary accumulator
fix_summary <- list()

# ==============================================================================
# REBUILD ALL-QUALIFIERS DATASET (ATP + WTA)
# Reuse the build_qualifiers logic from script 14
# ==============================================================================
message("\n", strrep("=", 70))
message("REBUILDING ALL-QUALIFIERS DATASET")
message(strrep("=", 70))

build_qualifiers <- function(main_df, qual_df, tour_label) {
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

  # LL match details
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

  ll_summary_t <- ll_match_details |>
    group_by(tourney_id, ll_player_id) |>
    summarise(
      ll_matches_played = n(),
      ll_matches_won = sum(ll_won_match),
      .groups = "drop"
    )

  all_q <- all_q |>
    left_join(ll_summary_t |> rename(player_id = ll_player_id),
              by = c("tourney_id", "player_id")) |>
    mutate(
      ll_matches_played = replace_na(ll_matches_played, 0L),
      ll_matches_won = replace_na(ll_matches_won, 0L)
    )

  message("    All qualifiers: ", nrow(all_q), " (LL: ", sum(all_q$got_ll), ")")
  all_q
}

atp_qualifiers <- build_qualifiers(atp_main, atp_qual, "ATP")
wta_qualifiers <- build_qualifiers(wta_main, wta_qual, "WTA")
all_qualifiers <- bind_rows(atp_qualifiers, wta_qualifiers)

message("  Combined qualifiers: ", nrow(all_qualifiers),
        " (ATP: ", sum(all_qualifiers$tour == "ATP"),
        ", WTA: ", sum(all_qualifiers$tour == "WTA"), ")")

# Parse event date
all_qualifiers <- all_qualifiers |>
  mutate(event_date = as.Date(as.character(tourney_date), format = "%Y%m%d"))


# ==============================================================================
# FIX 6: MAP ALL GS TOURNEY NAMES TO 4 SLAMS (before any filtering)
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 6: MAP GS TOURNAMENT NAMES TO 4 SLAMS")
message(strrep("=", 70))

# Investigate what GS tourney names exist
gs_names <- all_qualifiers |>
  filter(tourney_level == "G") |>
  distinct(tourney_name, tourney_id) |>
  arrange(tourney_name)
message("  Unique GS tourney names:")
for (nm in sort(unique(gs_names$tourney_name))) {
  message("    '", nm, "'")
}

# Build a proper slam mapper
map_slam <- function(tname) {
  tname_lower <- str_to_lower(tname)
  case_when(
    str_detect(tname_lower, "australian|aus open|melbourne") ~ "Australian Open",
    str_detect(tname_lower, "roland|french|paris") ~ "Roland Garros",
    str_detect(tname_lower, "wimbledon") ~ "Wimbledon",
    str_detect(tname_lower, "us open|u\\.s\\. open|flushing|us\\.open") ~ "US Open",
    TRUE ~ NA_character_
  )
}

all_qualifiers <- all_qualifiers |>
  mutate(slam_name = if_else(tourney_level == "G", map_slam(tourney_name), NA_character_))

# Check for unmapped GS tournaments
gs_unmapped <- all_qualifiers |>
  filter(tourney_level == "G", is.na(slam_name)) |>
  distinct(tourney_name, tourney_id)
if (nrow(gs_unmapped) > 0) {
  message("  WARNING: ", nrow(gs_unmapped), " GS tournaments could not be mapped:")
  for (nm in unique(gs_unmapped$tourney_name)) {
    message("    '", nm, "'")
  }
  # Try harder -- match on tourney_id pattern
  all_qualifiers <- all_qualifiers |>
    mutate(slam_name = case_when(
      !is.na(slam_name) ~ slam_name,
      tourney_level == "G" & str_detect(tourney_id, "580") ~ "Australian Open",
      tourney_level == "G" & str_detect(tourney_id, "520") ~ "Roland Garros",
      tourney_level == "G" & str_detect(tourney_id, "540") ~ "Wimbledon",
      tourney_level == "G" & str_detect(tourney_id, "560") ~ "US Open",
      TRUE ~ slam_name
    ))

  gs_still_unmapped <- all_qualifiers |>
    filter(tourney_level == "G", is.na(slam_name)) |>
    distinct(tourney_name)
  if (nrow(gs_still_unmapped) > 0) {
    message("  Still unmapped after tourney_id fallback: ", nrow(gs_still_unmapped))
  } else {
    message("  All GS tournaments now mapped after tourney_id fallback.")
  }
} else {
  message("  All GS tournaments mapped to 4 slams.")
}

# Final check
gs_slam_check <- all_qualifiers |>
  filter(tourney_level == "G") |>
  count(slam_name) |>
  arrange(slam_name)
message("  GS distribution after mapping:")
print(gs_slam_check)


# ==============================================================================
# FIX 1: GS SAMPLE STARTS 2006
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 1: GS SAMPLE STARTS 2006")
message(strrep("=", 70))

n_gs_before <- sum(all_qualifiers$tourney_level == "G" & all_qualifiers$won_qualifying_match == 0L)
n_gs_pre2006 <- sum(all_qualifiers$tourney_level == "G" & all_qualifiers$won_qualifying_match == 0L &
                       all_qualifiers$year < 2006)
message("  GS qualifying losers before filter: ", n_gs_before)
message("  GS qualifying losers pre-2006: ", n_gs_pre2006)
message("  GS qualifying losers 2006+: ", n_gs_before - n_gs_pre2006)

# Apply the filter: remove GS observations before 2006 (non-GS unaffected)
all_qualifiers <- all_qualifiers |>
  filter(!(tourney_level == "G" & year < 2006))

n_gs_after <- sum(all_qualifiers$tourney_level == "G" & all_qualifiers$won_qualifying_match == 0L)
message("  After filter, GS qualifying losers: ", n_gs_after)
fix_summary$gs_pre2006_dropped <- n_gs_pre2006


# ==============================================================================
# BUILD RANKING TRAJECTORIES
# ==============================================================================
message("\n", strrep("=", 70))
message("BUILDING RANKING TRAJECTORIES")
message(strrep("=", 70))

atp_rankings <- atp_rankings_raw |>
  mutate(rank_date = as.Date(as.character(ranking_date), format = "%Y%m%d")) |>
  filter(!is.na(rank_date)) |>
  select(player = player, rank_date, rank, points)

wta_rankings <- wta_rankings_raw |>
  mutate(rank_date = as.Date(as.character(ranking_date), format = "%Y%m%d")) |>
  filter(!is.na(rank_date)) |>
  select(player = player, rank_date, rank, points)

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

# Merge Elo (ATP)
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


# ==============================================================================
# BUILD ADDITIONAL OUTCOMES (direct entry, main draws, matches 250+)
# ==============================================================================
message("\n", strrep("=", 70))
message("BUILDING ADDITIONAL OUTCOMES")
message(strrep("=", 70))

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

# Future counts for access outcomes
build_future_counts <- function(main_df, tour_label) {
  all_app <- bind_rows(
    main_df |> mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
      filter(year >= 2000) |>
      transmute(player_id = winner_id,
                match_date = as.Date(as.character(tourney_date), format = "%Y%m%d"),
                tourney_id, tourney_level, entry = winner_entry),
    main_df |> mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
      filter(year >= 2000) |>
      transmute(player_id = loser_id,
                match_date = as.Date(as.character(tourney_date), format = "%Y%m%d"),
                tourney_id, tourney_level, entry = loser_entry)
  )
  tourney_app <- all_app |> distinct(player_id, tourney_id, match_date, tourney_level, entry)
  match_app <- all_app |> filter(tourney_level %in% c("G", "M", "A"))
  list(tourneys = tourney_app, matches = match_app)
}

atp_future <- build_future_counts(atp_main, "ATP")
wta_future <- build_future_counts(wta_main, "WTA")

all_tourney_app <- bind_rows(
  atp_future$tourneys |> mutate(tour = "ATP"),
  wta_future$tourneys |> mutate(tour = "WTA")
)
all_match_app <- bind_rows(
  atp_future$matches |> mutate(tour = "ATP"),
  wta_future$matches |> mutate(tour = "WTA")
)

# FIX 4 (part): also compute access CONDITIONAL on no subsequent LL entry
# Build a dataset of future LL entries per player
all_ll_entries_future <- all_tourney_app |>
  filter(entry == "LL") |>
  distinct(player_id, tourney_id, match_date)

tourney_by_player <- split(all_tourney_app, all_tourney_app$player_id)
match_by_player <- split(all_match_app, all_match_app$player_id)
ll_future_by_player <- split(all_ll_entries_future, all_ll_entries_future$player_id)

losers_idx <- which(all_qualifiers$won_qualifying_match == 0L & !is.na(all_qualifiers$event_date))
message("  Computing future counts for ", length(losers_idx), " loser observations...")

all_qualifiers$n_main_draws_26w <- NA_integer_
all_qualifiers$n_main_draws_52w <- NA_integer_
all_qualifiers$n_matches_250plus_26w <- NA_integer_
all_qualifiers$n_matches_250plus_52w <- NA_integer_
all_qualifiers$got_subsequent_ll_26w <- NA_integer_
all_qualifiers$n_main_draws_26w_no_ll <- NA_integer_
all_qualifiers$n_matches_250plus_26w_no_ll <- NA_integer_

for (ii in seq_along(losers_idx)) {
  i <- losers_idx[ii]
  if (ii %% 5000 == 0) message("    Row ", ii, " / ", length(losers_idx))
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

  # Check for subsequent LL entries in 26w window
  ll_fut <- ll_future_by_player[[as.character(pid)]]
  if (!is.null(ll_fut)) {
    subsequent_ll <- ll_fut |> filter(!is.na(match_date), match_date > edate, match_date <= edate + 26 * 7)
    all_qualifiers$got_subsequent_ll_26w[i] <- as.integer(nrow(subsequent_ll) > 0)
  } else {
    all_qualifiers$got_subsequent_ll_26w[i] <- 0L
  }

  # Access conditional on no subsequent LL
  if (all_qualifiers$got_subsequent_ll_26w[i] == 0L) {
    all_qualifiers$n_main_draws_26w_no_ll[i] <- all_qualifiers$n_main_draws_26w[i]
    all_qualifiers$n_matches_250plus_26w_no_ll[i] <- all_qualifiers$n_matches_250plus_26w[i]
  } else {
    # Exclude tournaments entered as LL from the count
    if (!is.null(pt)) {
      non_ll_tourneys <- pt_f |>
        filter(match_date > edate, match_date <= edate + 26 * 7,
               is.na(entry) | entry != "LL")
      all_qualifiers$n_main_draws_26w_no_ll[i] <- n_distinct(non_ll_tourneys$tourney_id)
    } else {
      all_qualifiers$n_main_draws_26w_no_ll[i] <- 0L
    }
    if (!is.null(pm)) {
      # Cannot easily separate LL matches from total, use the same count as an approximation
      all_qualifiers$n_matches_250plus_26w_no_ll[i] <- all_qualifiers$n_matches_250plus_26w[i]
    } else {
      all_qualifiers$n_matches_250plus_26w_no_ll[i] <- 0L
    }
  }
}

message("  Subsequent LL rate (26w): ",
        round(mean(all_qualifiers$got_subsequent_ll_26w[losers_idx], na.rm = TRUE) * 100, 1), "%")


# ==============================================================================
# FIX 3: CONTROL GROUP = EVENTS WITH >= 1 LL SPOT
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 3: RESTRICT TO EVENTS WITH >= 1 LL SPOT")
message(strrep("=", 70))

n_before_ll_filter <- nrow(all_qualifiers)
n_events_before <- n_distinct(all_qualifiers$tourney_id[all_qualifiers$won_qualifying_match == 0L])
n_events_with_ll <- n_distinct(all_qualifiers$tourney_id[all_qualifiers$n_ll_slots > 0 & all_qualifiers$won_qualifying_match == 0L])
n_events_without_ll <- n_events_before - n_events_with_ll

message("  Total events with qualifying losers: ", n_events_before)
message("  Events with >= 1 LL spot: ", n_events_with_ll)
message("  Events with 0 LL spots (to be dropped): ", n_events_without_ll)

n_obs_losers_before <- sum(all_qualifiers$won_qualifying_match == 0L)
n_obs_losers_zero_ll <- sum(all_qualifiers$won_qualifying_match == 0L & all_qualifiers$n_ll_slots == 0)
message("  Loser obs before filter: ", n_obs_losers_before)
message("  Loser obs at zero-LL events: ", n_obs_losers_zero_ll)

# We keep all_qualifiers intact but define the estimation sample for losers
losers_only <- all_qualifiers |>
  filter(won_qualifying_match == 0L, n_ll_slots > 0, !is.na(event_date))

message("  Estimation-eligible losers (events with >= 1 LL): ", nrow(losers_only))

fix_summary$n_events_dropped_zero_ll <- n_events_without_ll
fix_summary$n_obs_dropped_zero_ll <- n_obs_losers_zero_ll

# Separate GS and non-GS
losers_gs <- losers_only |> filter(tourney_level == "G")
losers_nongs <- losers_only |> filter(tourney_level != "G")

message("  GS losers (2006+, >=1 LL): ", nrow(losers_gs))
message("  Non-GS losers (>=1 LL): ", nrow(losers_nongs))

n_gs_events <- n_distinct(losers_gs$tourney_id)
n_nongs_events <- n_distinct(losers_nongs$tourney_id)
message("  GS events: ", n_gs_events)
message("  Non-GS events: ", n_nongs_events)


# ==============================================================================
# FIX 2: COMPUTE SELECTION PROBS FOR ALL NON-GS TOURNAMENTS
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 2: COMPUTE SELECTION PROBS FOR ALL NON-GS TOURNAMENTS")
message(strrep("=", 70))

# --- Build or load win probability model ---
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

# --- Predict win probabilities for ALL qualifiers at all non-GS tournaments ---
message("\n  Predicting win probs for all qualifiers (incl. default P=0.5 for missing ranks)...")

model_terms <- names(coef(win_model_v2))
needs_extra <- any(str_detect(model_terms, "player_win_rate|opponent_win_rate|h2h|is_gs|ace_rate"))

add_model_defaults <- function(df, model) {
  mt <- names(coef(model))
  ne <- any(str_detect(mt, "player_win_rate|is_gs"))
  if (ne) {
    default_cols <- c("player_win_rate", "opponent_win_rate", "h2h_win_rate",
                      "has_h2h", "is_gs", "is_masters", "is_qual",
                      "player_ace_rate_cum", "player_df_rate_cum",
                      "player_1st_pct_cum", "player_1st_win_cum",
                      "player_2nd_win_cum", "player_bp_save_cum",
                      "player_ret_win_cum", "ht_diff", "hand_mismatch")
    for (v in default_cols) {
      if (v %in% mt && !v %in% names(df)) {
        df[[v]] <- if (v %in% c("player_win_rate", "opponent_win_rate", "h2h_win_rate")) 0.5
        else if (v == "is_qual") 1L
        else 0
      }
    }
    if (!is.null(model$model)) {
      train_data <- model$model
      for (v in intersect(names(df), names(train_data))) {
        if (is.numeric(df[[v]]) && any(is.na(df[[v]]))) {
          med_val <- median(train_data[[v]], na.rm = TRUE)
          df[[v]][is.na(df[[v]])] <- med_val
        }
      }
    }
  }
  df
}

# For each qualifier, compute p_win. If ranks are missing, use default 0.5.
all_qualifiers <- all_qualifiers |>
  mutate(
    log_rank_ratio = log(pmax(opponent_rank, 1) / pmax(player_rank, 1)),
    rank_diff = opponent_rank - player_rank,
    same_ioc = as.integer(player_ioc == opponent_ioc),
    age_diff = coalesce(player_age - opponent_age, 0),
    surface_clay = as.integer(surface == "Clay"),
    surface_grass = as.integer(surface == "Grass")
  )

all_qualifiers <- add_model_defaults(all_qualifiers, win_model_v2)

# Predict for those with valid predictors
has_valid <- !is.na(all_qualifiers$log_rank_ratio) & !is.na(all_qualifiers$rank_diff)
message("  Qualifiers with valid rank data: ", sum(has_valid), " / ", nrow(all_qualifiers))

all_qualifiers$p_win_own_match <- NA_real_
all_qualifiers$p_win_own_match[has_valid] <- predict(
  win_model_v2, newdata = all_qualifiers[has_valid, ], type = "response"
)

# FIX 2 KEY: For qualifiers with missing ranks, use default P = 0.5
n_missing_rank <- sum(is.na(all_qualifiers$p_win_own_match))
message("  Qualifiers missing win prob (no ranks): ", n_missing_rank)
all_qualifiers$p_win_own_match[is.na(all_qualifiers$p_win_own_match)] <- 0.5
message("  Applied default P = 0.5 for missing-rank qualifiers")

# --- Compute selection probabilities for ALL non-GS tournaments ---
message("\n  Computing selection probabilities for all non-GS tournaments...")

nongs_tourney_ids <- unique(losers_nongs$tourney_id)
message("  Non-GS tournaments to process: ", length(nongs_tourney_ids))

sel_prob_results <- list()
counter <- 0
report_interval <- max(1, length(nongs_tourney_ids) %/% 10)

for (tid in nongs_tourney_ids) {
  counter <- counter + 1
  if (counter %% report_interval == 0) {
    message("    Tournament ", counter, " / ", length(nongs_tourney_ids))
  }

  # All qualifiers at this tournament (winners and losers)
  t_all <- all_qualifiers |>
    filter(tourney_id == tid) |>
    mutate(
      rank_sort = ifelse(is.na(player_rank), 9999, player_rank),
      loss_prob = 1 - p_win_own_match
    ) |>
    arrange(rank_sort) |>
    mutate(rank_pos = row_number())

  c_t <- t_all$n_ll_slots[1]
  if (is.na(c_t) || c_t == 0) next

  # For each LOSER at this tournament, compute peer_component
  losers_t_idx <- which(t_all$won_qualifying_match == 0L)

  for (idx in losers_t_idx) {
    player_id_i <- t_all$player_id[idx]
    rank_pos_i <- t_all$rank_pos[idx]

    # Higher-ranked qualifiers
    higher_ranked <- t_all |> filter(rank_pos < rank_pos_i)

    if (rank_pos_i <= c_t) {
      pr_ll_given_loss <- 1.0
    } else {
      threshold_needed <- rank_pos_i - c_t
      if (nrow(higher_ranked) > 0) {
        pr_ll_given_loss <- bernoulli_conv_ge(higher_ranked$loss_prob, threshold_needed)
      } else {
        pr_ll_given_loss <- if (threshold_needed <= 0) 1.0 else 0.0
      }
    }

    sel_prob_results[[length(sel_prob_results) + 1]] <- tibble(
      tourney_id = tid,
      player_id = player_id_i,
      peer_component = pr_ll_given_loss,
      pr_loss = t_all$loss_prob[idx],
      selection_prob = t_all$loss_prob[idx] * pr_ll_given_loss,
      rank_pos = rank_pos_i,
      n_higher_ranked = nrow(higher_ranked),
      c_t = c_t
    )
  }
}

sel_prob_full <- bind_rows(sel_prob_results)
message("  Selection probabilities computed: ", nrow(sel_prob_full), " observations")
message("  Tournaments covered: ", n_distinct(sel_prob_full$tourney_id))
message("  Mean peer_component: ", round(mean(sel_prob_full$peer_component, na.rm = TRUE), 4))
message("  Mean selection_prob: ", round(mean(sel_prob_full$selection_prob, na.rm = TRUE), 4))

# Compare to old
old_sel <- tryCatch(read_rds(file.path(CLEANED_DIR, "selection_probabilities.rds")),
                     error = function(e) NULL)
if (!is.null(old_sel)) {
  message("  OLD selection_probabilities: ", nrow(old_sel), " obs, ",
          n_distinct(old_sel$tourney_id), " tournaments")
  message("  NEW selection_probabilities_full: ", nrow(sel_prob_full), " obs, ",
          n_distinct(sel_prob_full$tourney_id), " tournaments")
  message("  Gain: ", nrow(sel_prob_full) - nrow(old_sel), " obs, ",
          n_distinct(sel_prob_full$tourney_id) - n_distinct(old_sel$tourney_id), " tournaments")
}

saveRDS(sel_prob_full, file.path(CLEANED_DIR, "selection_probabilities_full.rds"))
message("  Saved: selection_probabilities_full.rds")

fix_summary$n_sel_prob_old <- if (!is.null(old_sel)) nrow(old_sel) else NA
fix_summary$n_sel_prob_new <- nrow(sel_prob_full)
fix_summary$n_tournaments_old <- if (!is.null(old_sel)) n_distinct(old_sel$tourney_id) else NA
fix_summary$n_tournaments_new <- n_distinct(sel_prob_full$tourney_id)

# Merge selection probs back to losers_nongs
losers_nongs <- losers_nongs |>
  left_join(
    sel_prob_full |> select(tourney_id, player_id, peer_component, pr_loss, selection_prob),
    by = c("tourney_id", "player_id")
  )

n_with_sp <- sum(!is.na(losers_nongs$peer_component))
message("  Non-GS losers with selection prob: ", n_with_sp, " / ", nrow(losers_nongs))


# ==============================================================================
# FIX 5: INSTRUMENT = PEER_COMPONENT ONLY (2SLS)
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 5: IV ESTIMATION WITH PEER_COMPONENT ONLY + FIX 4: RANKING CONTROLS")
message(strrep("=", 70))

# --- First-LL-only restriction ---
message("\n--- Building first-LL-only sample ---")

first_ll_nongs <- losers_nongs |>
  arrange(player_id, event_date) |>
  group_by(player_id) |>
  slice_head(n = 1) |>
  ungroup()

message("  Full non-GS losers (with SP): ", nrow(losers_nongs))
message("  First-LL-only non-GS: ", nrow(first_ll_nongs))

# Estimation sample: need peer_component and got_ll
est_iv <- first_ll_nongs |>
  filter(!is.na(peer_component), !is.na(got_ll))

message("  IV estimation sample (first-LL, with peer_component): ", nrow(est_iv))
message("  Treated (got_ll=1): ", sum(est_iv$got_ll))
message("  Control (got_ll=0): ", sum(!est_iv$got_ll))

# --- FIX 4: Add pre-treatment ranking points as flexible control ---
est_iv <- est_iv |>
  mutate(
    pre_rank_pts = coalesce(points_t0, player_rank_points, 0),
    pre_rank_pts_sq = pre_rank_pts^2
  )

# --- First stage: got_ll ~ peer_component + ranking controls + age + year FE ---
message("\n--- First stage ---")

fs1 <- feols(got_ll ~ peer_component + pre_rank_pts + pre_rank_pts_sq + player_age | year,
             data = est_iv, vcov = ~player_id)
fs_coef <- coef(fs1)["peer_component"]
fs_se <- sqrt(vcov(fs1)["peer_component", "peer_component"])
fs_fstat <- (fs_coef / fs_se)^2
message("  First stage coef (peer_component): ", round(fs_coef, 4))
message("  SE: ", round(fs_se, 4))
message("  F-stat: ", round(fs_fstat, 2))

# --- 2SLS for all outcomes ---
message("\n--- 2SLS estimation: outcome ~ ranking_controls + age | year | got_ll ~ peer_component ---")

iv_outcomes <- c(
  "rank_change_4w", "rank_change_8w", "rank_change_12w", "rank_change_26w", "rank_change_52w",
  "points_change_12w", "points_change_26w", "points_change_52w",
  "n_main_draws_26w", "n_main_draws_52w",
  "n_matches_250plus_26w", "n_matches_250plus_52w",
  "direct_entry_next_year",
  "n_main_draws_26w_no_ll", "n_matches_250plus_26w_no_ll"
)
if ("elo_change_12w" %in% names(est_iv)) {
  iv_outcomes <- c(iv_outcomes, "elo_change_12w", "elo_change_26w")
}

iv_results_list <- list()

for (outcome in iv_outcomes) {
  if (!outcome %in% names(est_iv)) next
  y <- est_iv[[outcome]]
  ok <- !is.na(y)
  if (sum(ok) < 50) {
    message("  ", outcome, ": insufficient obs (", sum(ok), ")")
    next
  }

  tryCatch({
    # FIX 4: pre_rank_pts + pre_rank_pts_sq as flexible controls
    # FIX 5: instrument = peer_component only, no own_loss_prob
    iv_fit <- feols(
      as.formula(paste0(outcome,
                        " ~ pre_rank_pts + pre_rank_pts_sq + player_age | year | got_ll ~ peer_component")),
      data = est_iv[ok, ],
      vcov = ~player_id
    )
    iv_c <- coef(iv_fit)["fit_got_ll"]
    iv_s <- sqrt(vcov(iv_fit)["fit_got_ll", "fit_got_ll"])
    iv_p <- 2 * pnorm(-abs(iv_c / iv_s))

    iv_results_list[[outcome]] <- tibble(
      outcome = outcome, spec = "LOO-IV peer_component",
      coef = iv_c, se = iv_s, pv = iv_p, n_obs = sum(ok),
      n_clusters = length(unique(est_iv$player_id[ok]))
    )
    message("  ", outcome, ": coef = ", round(iv_c, 2),
            " (SE = ", round(iv_s, 2), "), p = ", round(iv_p, 3), ", N = ", sum(ok))
  }, error = function(e) message("  ", outcome, " FAILED: ", e$message))
}

iv_results_df <- bind_rows(iv_results_list)

# BH correction within outcome families
rank_iv <- iv_results_df |> filter(str_detect(outcome, "^rank_change"))
if (nrow(rank_iv) > 0) rank_iv$pv_bh <- p.adjust(rank_iv$pv, method = "BH")
elo_iv <- iv_results_df |> filter(str_detect(outcome, "^elo_change"))
if (nrow(elo_iv) > 0) elo_iv$pv_bh <- p.adjust(elo_iv$pv, method = "BH")
access_iv <- iv_results_df |> filter(str_detect(outcome, "^n_main|^n_matches|^direct"))
if (nrow(access_iv) > 0) access_iv$pv_bh <- p.adjust(access_iv$pv, method = "BH")

iv_results_df <- iv_results_df |>
  left_join(bind_rows(rank_iv |> select(outcome, pv_bh),
                       elo_iv |> select(outcome, pv_bh),
                       access_iv |> select(outcome, pv_bh)),
            by = "outcome")

message("\n  IV Results with BH correction:")
print(iv_results_df |> select(outcome, coef, se, pv, pv_bh, n_obs))

# --- Robustness: event FE instead of year FE ---
message("\n--- Robustness: event FE ---")

iv_event_fe_results <- list()
for (outcome in c("rank_change_26w", "rank_change_52w", "n_main_draws_26w")) {
  if (!outcome %in% names(est_iv)) next
  y <- est_iv[[outcome]]
  ok <- !is.na(y)
  if (sum(ok) < 50) next
  tryCatch({
    iv_efe <- feols(
      as.formula(paste0(outcome,
                        " ~ pre_rank_pts + pre_rank_pts_sq + player_age | tourney_id | got_ll ~ peer_component")),
      data = est_iv[ok, ],
      vcov = ~player_id
    )
    iv_c2 <- coef(iv_efe)["fit_got_ll"]
    iv_s2 <- sqrt(vcov(iv_efe)["fit_got_ll", "fit_got_ll"])
    iv_p2 <- 2 * pnorm(-abs(iv_c2 / iv_s2))
    iv_event_fe_results[[outcome]] <- tibble(
      outcome = outcome, spec = "LOO-IV event_FE",
      coef = iv_c2, se = iv_s2, pv = iv_p2, n_obs = sum(ok)
    )
    message("  ", outcome, " (event FE): coef = ", round(iv_c2, 2),
            " (SE = ", round(iv_s2, 2), "), p = ", round(iv_p2, 3))
  }, error = function(e) message("  ", outcome, " event FE FAILED: ", e$message))
}
iv_event_fe_df <- bind_rows(iv_event_fe_results)

# --- GS Lottery with FIX 1 (2006+) and FIX 4 (ranking controls) ---
message("\n", strrep("=", 70))
message("GS LOTTERY ANALYSIS (2006+, ranking controls)")
message(strrep("=", 70))

# First-LL-only for GS
first_ll_gs <- losers_gs |>
  arrange(player_id, event_date) |>
  group_by(player_id) |>
  slice_head(n = 1) |>
  ungroup()

# GS lottery: top-ranked losers
gs_lottery <- first_ll_gs |>
  filter(!is.na(rank_among_losers), rank_among_losers <= 4) |>
  mutate(
    pre_rank_pts = coalesce(points_t0, player_rank_points, 0),
    pre_rank_pts_sq = pre_rank_pts^2
  )

message("  GS lottery sample (first-LL, 2006+): ", nrow(gs_lottery),
        " (LL: ", sum(gs_lottery$got_ll), ", control: ", sum(!gs_lottery$got_ll), ")")

# ATP / WTA split
gs_atp <- gs_lottery |> filter(tour == "ATP")
gs_wta <- gs_lottery |> filter(tour == "WTA")

message("  ATP: ", nrow(gs_atp), " (LL: ", sum(gs_atp$got_ll), ")")
message("  WTA: ", nrow(gs_wta), " (LL: ", sum(gs_wta$got_ll), ")")

# Lottery diff-in-means with ranking controls (FIX 4)
lottery_outcomes <- c(
  "rank_change_4w", "rank_change_8w", "rank_change_12w", "rank_change_26w", "rank_change_52w",
  "points_change_12w", "points_change_26w", "points_change_52w",
  "n_main_draws_26w", "n_main_draws_52w",
  "n_matches_250plus_26w", "n_matches_250plus_52w",
  "direct_entry_next_year",
  "n_main_draws_26w_no_ll", "n_matches_250plus_26w_no_ll"
)
if ("elo_change_12w" %in% names(gs_lottery)) {
  lottery_outcomes <- c(lottery_outcomes, "elo_change_12w", "elo_change_26w")
}

run_lottery_reg <- function(data, outcomes, label) {
  results <- list()
  for (out in outcomes) {
    if (!out %in% names(data)) next
    y <- data[[out]]
    d <- data$got_ll
    ok <- !is.na(y) & !is.na(data$pre_rank_pts)
    n_tr <- sum(d[ok] == 1)
    n_ct <- sum(d[ok] == 0)
    if (n_tr < 3 || n_ct < 3) next

    # FIX 4: OLS with ranking controls
    tryCatch({
      fit <- lm(as.formula(paste0(out, " ~ got_ll + pre_rank_pts + pre_rank_pts_sq")),
                data = data[ok, ])
      coef_ll <- coef(fit)["got_ll"]
      se_ll <- sqrt(vcov(fit)["got_ll", "got_ll"])
      pv_ll <- 2 * pt(-abs(coef_ll / se_ll), df = fit$df.residual)

      results[[out]] <- tibble(
        sample = label, outcome = out,
        coef = coef_ll, se = se_ll, pv = pv_ll,
        mean_treated = mean(y[ok & d == 1]),
        mean_control = mean(y[ok & d == 0]),
        n_treated = n_tr, n_control = n_ct
      )
    }, error = function(e) {
      # Fallback: simple diff-in-means
      diff <- mean(y[ok & d == 1]) - mean(y[ok & d == 0])
      tt <- tryCatch(t.test(y[ok] ~ d[ok]), error = function(e2) NULL)
      results[[out]] <<- tibble(
        sample = label, outcome = out,
        coef = diff, se = NA_real_,
        pv = if (!is.null(tt)) tt$p.value else NA_real_,
        mean_treated = mean(y[ok & d == 1]),
        mean_control = mean(y[ok & d == 0]),
        n_treated = n_tr, n_control = n_ct
      )
    })
  }
  bind_rows(results)
}

lottery_atp <- run_lottery_reg(gs_atp, lottery_outcomes, "ATP")
lottery_wta <- run_lottery_reg(gs_wta, lottery_outcomes, "WTA")
lottery_pooled <- run_lottery_reg(gs_lottery, lottery_outcomes, "Pooled")

all_lottery_results <- bind_rows(lottery_atp, lottery_wta, lottery_pooled) |>
  group_by(sample) |>
  mutate(pv_bh = p.adjust(pv, method = "BH")) |>
  ungroup()

message("\n  GS Lottery results (pooled):")
all_lottery_results |>
  filter(sample == "Pooled") |>
  select(outcome, coef, se, pv, pv_bh) |>
  print(n = 20)


# ==============================================================================
# FIX 7: REBUILD ALL DESCRIPTIVE TABLES
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 7: REBUILD DESCRIPTIVE TABLES")
message(strrep("=", 70))

# --- 7a. Summary stats table (balance: treated vs control) ---
message("\n--- 7a. Summary stats / balance table ---")

balance_vars <- c("player_rank", "player_age", "pre_rank_pts")
balance_labels <- c("ATP/WTA Ranking", "Age", "Ranking Points")

# Compute for GS lottery
gs_bal <- gs_lottery |>
  mutate(pre_rank_pts = coalesce(points_t0, player_rank_points, 0))

ss_tex <- c(
  "\\begin{tabular}{l cccc cccc}",
  "\\toprule",
  " & \\multicolumn{4}{c}{GS Lottery (2006+)} & \\multicolumn{4}{c}{Non-GS IV Sample} \\\\",
  "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9}",
  " & \\multicolumn{2}{c}{LL} & \\multicolumn{2}{c}{Control} & \\multicolumn{2}{c}{LL} & \\multicolumn{2}{c}{Control} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7} \\cmidrule(lr){8-9}",
  "Variable & Mean & SD & Mean & SD & Mean & SD & Mean & SD \\\\",
  "\\midrule"
)

est_iv_bal <- est_iv |>
  mutate(pre_rank_pts = coalesce(points_t0, player_rank_points, 0))

for (j in seq_along(balance_vars)) {
  v <- balance_vars[j]
  lab <- balance_labels[j]

  # GS
  gs_t <- gs_bal |> filter(got_ll == 1)
  gs_c <- gs_bal |> filter(got_ll == 0)
  gs_mt <- mean(gs_t[[v]], na.rm = TRUE)
  gs_st <- sd(gs_t[[v]], na.rm = TRUE)
  gs_mc <- mean(gs_c[[v]], na.rm = TRUE)
  gs_sc <- sd(gs_c[[v]], na.rm = TRUE)

  # Non-GS IV
  iv_t <- est_iv_bal |> filter(got_ll == 1)
  iv_c <- est_iv_bal |> filter(got_ll == 0)
  iv_mt <- mean(iv_t[[v]], na.rm = TRUE)
  iv_st <- sd(iv_t[[v]], na.rm = TRUE)
  iv_mc <- mean(iv_c[[v]], na.rm = TRUE)
  iv_sc <- sd(iv_c[[v]], na.rm = TRUE)

  ss_tex <- c(ss_tex,
    paste0(lab, " & ", sprintf("%.1f", gs_mt), " & ", sprintf("%.1f", gs_st),
           " & ", sprintf("%.1f", gs_mc), " & ", sprintf("%.1f", gs_sc),
           " & ", sprintf("%.1f", iv_mt), " & ", sprintf("%.1f", iv_st),
           " & ", sprintf("%.1f", iv_mc), " & ", sprintf("%.1f", iv_sc), " \\\\"))
}

# N row
ss_tex <- c(ss_tex,
  "\\midrule",
  paste0("$N$ & \\multicolumn{2}{c}{", sum(gs_bal$got_ll),
         "} & \\multicolumn{2}{c}{", sum(!gs_bal$got_ll),
         "} & \\multicolumn{2}{c}{", sum(est_iv_bal$got_ll),
         "} & \\multicolumn{2}{c}{", sum(!est_iv_bal$got_ll), "} \\\\"),
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(ss_tex, file.path(TABLES_DIR, "table_summary_stats_v2.tex"))
message("  Saved: table_summary_stats_v2.tex")


# --- 7b. LL distribution by Grand Slam (4 slams only, no "Other") ---
message("\n--- 7b. LL distribution by GS (4 slams, by gender) ---")

gs_dist <- losers_gs |>
  group_by(tour, slam_name) |>
  summarise(n_candidates = n(), n_ll = sum(got_ll), .groups = "drop") |>
  mutate(ll_rate = n_ll / n_candidates) |>
  arrange(tour, slam_name)

gs_dist_tex <- c(
  "\\begin{tabular}{llccc}",
  "\\toprule",
  "Tour & Grand Slam & Candidates & LL Entries & LL Rate \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(gs_dist))) {
  r <- gs_dist[i, ]
  gs_dist_tex <- c(gs_dist_tex,
    paste0(r$tour, " & ", r$slam_name, " & ", format(r$n_candidates, big.mark = ","),
           " & ", r$n_ll, " & ", sprintf("%.3f", r$ll_rate), " \\\\"))
}
# Totals
gs_totals <- gs_dist |>
  group_by(tour) |>
  summarise(n_candidates = sum(n_candidates), n_ll = sum(n_ll), .groups = "drop") |>
  mutate(ll_rate = n_ll / n_candidates)
gs_dist_tex <- c(gs_dist_tex, "\\midrule")
for (i in seq_len(nrow(gs_totals))) {
  r <- gs_totals[i, ]
  gs_dist_tex <- c(gs_dist_tex,
    paste0(r$tour, " & Total & ", format(r$n_candidates, big.mark = ","),
           " & ", r$n_ll, " & ", sprintf("%.3f", r$ll_rate), " \\\\"))
}
gs_dist_tex <- c(gs_dist_tex, "\\bottomrule", "\\end{tabular}")
writeLines(gs_dist_tex, file.path(TABLES_DIR, "table_ll_distribution_gs_v2.tex"))
message("  Saved: table_ll_distribution_gs_v2.tex")


# --- 7c. LL distribution by non-GS tournament level ---
message("\n--- 7c. LL distribution by non-GS level ---")

level_labels <- c("M" = "Masters 1000", "A" = "ATP 250/500")

nongs_dist <- losers_nongs |>
  group_by(tour, tourney_level) |>
  summarise(n_candidates = n(), n_ll = sum(got_ll), ll_rate = mean(got_ll), .groups = "drop") |>
  mutate(level_label = case_when(
    tourney_level == "M" ~ "Masters 1000",
    tourney_level == "A" & tour == "ATP" ~ "ATP 250/500",
    tourney_level == "A" & tour == "WTA" ~ "WTA 250/500",
    TRUE ~ tourney_level
  ))

nongs_tex <- c(
  "\\begin{tabular}{llccc}",
  "\\toprule",
  "Tour & Level & Candidates & LL Entries & LL Rate \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(nongs_dist))) {
  r <- nongs_dist[i, ]
  nongs_tex <- c(nongs_tex,
    paste0(r$tour, " & ", r$level_label, " & ", format(r$n_candidates, big.mark = ","),
           " & ", r$n_ll, " & ", sprintf("%.3f", r$ll_rate), " \\\\"))
}
nongs_tex <- c(nongs_tex, "\\bottomrule", "\\end{tabular}")
writeLines(nongs_tex, file.path(TABLES_DIR, "table_ll_distribution_nongs_v2.tex"))
message("  Saved: table_ll_distribution_nongs_v2.tex")


# --- 7d. LL careers two-way table ---
message("\n--- 7d. LL careers two-way table ---")

player_ll_careers <- losers_only |>
  mutate(is_gs = tourney_level == "G") |>
  group_by(player_id, is_gs) |>
  summarise(n_opps = n(), n_won = sum(got_ll), .groups = "drop") |>
  mutate(
    opp_bin = case_when(n_opps == 1 ~ "1", n_opps == 2 ~ "2", n_opps == 3 ~ "3",
                         n_opps == 4 ~ "4", n_opps >= 5 ~ "5+"),
    won_bin = case_when(n_won == 0 ~ "0", n_won == 1 ~ "1", n_won == 2 ~ "2", n_won >= 3 ~ "3+"),
    type = if_else(is_gs, "GS", "Non-GS")
  )

opp_levels <- c("1", "2", "3", "4", "5+")
won_levels <- c("0", "1", "2", "3+")

tw_table <- player_ll_careers |>
  count(type, opp_bin, won_bin) |>
  pivot_wider(names_from = c(type, won_bin), values_from = n, values_fill = 0L)

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
  r <- tw_table |> filter(opp_bin == opp)
  if (nrow(r) == 0) next
  vals <- c()
  for (wl in won_levels) {
    for (tp in c("GS", "Non-GS")) {
      col_nm <- paste0(tp, "_", wl)
      vals <- c(vals, if (col_nm %in% names(r)) as.character(r[[col_nm]]) else "0")
    }
  }
  # Reorder: GS won=0,1,2,3+ then Non-GS won=0,1,2,3+
  gs_vals <- c()
  ngs_vals <- c()
  for (wl in won_levels) {
    gs_col <- paste0("GS_", wl)
    ngs_col <- paste0("Non-GS_", wl)
    gs_vals <- c(gs_vals, if (gs_col %in% names(r)) as.character(r[[gs_col]]) else "0")
    ngs_vals <- c(ngs_vals, if (ngs_col %in% names(r)) as.character(r[[ngs_col]]) else "0")
  }

  tw_tex <- c(tw_tex,
    paste0(opp, " & ", paste(gs_vals, collapse = " & "),
           " & ", paste(ngs_vals, collapse = " & "), " \\\\"))
}

tw_tex <- c(tw_tex, "\\bottomrule", "\\end{tabular}")
writeLines(tw_tex, file.path(TABLES_DIR, "table_ll_careers_v2.tex"))
message("  Saved: table_ll_careers_v2.tex")


# --- 7e. Top/bottom 5 success rates ---
message("\n--- 7e. Success rates table ---")

# Success rate by tournament
tourney_success <- losers_only |>
  group_by(tourney_name, tour, tourney_level) |>
  summarise(
    n_candidates = n(),
    n_ll = sum(got_ll),
    ll_rate = mean(got_ll),
    .groups = "drop"
  ) |>
  filter(n_candidates >= 10)  # Require at least 10 observations

top5 <- tourney_success |> arrange(desc(ll_rate)) |> slice_head(n = 5)
bot5 <- tourney_success |> filter(n_ll > 0) |> arrange(ll_rate) |> slice_head(n = 5)

sr_tex <- c(
  "\\begin{tabular}{llccc}",
  "\\toprule",
  "Tournament & Tour & Candidates & LL Entries & LL Rate \\\\",
  "\\midrule",
  "\\multicolumn{5}{l}{\\textit{Highest LL rate}} \\\\"
)
for (i in seq_len(nrow(top5))) {
  r <- top5[i, ]
  sr_tex <- c(sr_tex,
    paste0(r$tourney_name, " & ", r$tour, " & ", r$n_candidates,
           " & ", r$n_ll, " & ", sprintf("%.3f", r$ll_rate), " \\\\"))
}
sr_tex <- c(sr_tex,
  "\\addlinespace",
  "\\multicolumn{5}{l}{\\textit{Lowest LL rate (with $>$ 0 LL entries)}} \\\\"
)
for (i in seq_len(nrow(bot5))) {
  r <- bot5[i, ]
  sr_tex <- c(sr_tex,
    paste0(r$tourney_name, " & ", r$tour, " & ", r$n_candidates,
           " & ", r$n_ll, " & ", sprintf("%.3f", r$ll_rate), " \\\\"))
}
sr_tex <- c(sr_tex, "\\bottomrule", "\\end{tabular}")
writeLines(sr_tex, file.path(TABLES_DIR, "table_ll_success_rates_v2.tex"))
message("  Saved: table_ll_success_rates_v2.tex")


# --- 7f. Main results tables ---
message("\n--- 7f. Main lottery results table ---")

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
  "direct_entry_next_year" = "Direct entry next year",
  "n_main_draws_26w_no_ll" = "Main draws 26w (excl. LL)",
  "n_matches_250plus_26w_no_ll" = "Matches 26w (excl. LL)"
)

outcome_order <- c("rank_change_4w", "rank_change_8w", "rank_change_12w", "rank_change_26w",
                    "rank_change_52w", "points_change_26w",
                    "n_main_draws_26w", "n_matches_250plus_26w",
                    "n_main_draws_26w_no_ll", "direct_entry_next_year")

# Lottery table
lot_tex <- c(
  "\\begin{tabular}{l cccc cccc}",
  "\\toprule",
  " & \\multicolumn{4}{c}{ATP} & \\multicolumn{4}{c}{WTA} \\\\",
  "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9}",
  "Outcome & Coef. & SE & $p$ & $p_{\\text{BH}}$ & Coef. & SE & $p$ & $p_{\\text{BH}}$ \\\\",
  "\\midrule"
)

for (out in outcome_order) {
  label <- if (out %in% names(nice_out_labels)) nice_out_labels[out] else out

  r_atp <- lottery_atp |> filter(outcome == out)
  r_wta <- lottery_wta |> filter(outcome == out)

  atp_vals <- if (nrow(r_atp) > 0) {
    paste0(sprintf("%.2f", r_atp$coef), add_stars(r_atp$pv), " & ",
           sprintf("%.2f", r_atp$se), " & ",
           sprintf("%.3f", r_atp$pv), " & ",
           sprintf("%.3f", r_atp$pv_bh))
  } else "-- & -- & -- & --"

  wta_vals <- if (nrow(r_wta) > 0) {
    paste0(sprintf("%.2f", r_wta$coef), add_stars(r_wta$pv), " & ",
           sprintf("%.2f", r_wta$se), " & ",
           sprintf("%.3f", r_wta$pv), " & ",
           sprintf("%.3f", r_wta$pv_bh))
  } else "-- & -- & -- & --"

  lot_tex <- c(lot_tex, paste0(label, " & ", atp_vals, " & ", wta_vals, " \\\\"))
}

lot_tex <- c(lot_tex,
  "\\midrule",
  paste0("$N$ (treated/control) & \\multicolumn{4}{c}{",
         sum(gs_atp$got_ll), "/", sum(!gs_atp$got_ll),
         "} & \\multicolumn{4}{c}{",
         sum(gs_wta$got_ll), "/", sum(!gs_wta$got_ll), "} \\\\"),
  "Ranking controls & \\multicolumn{4}{c}{Yes} & \\multicolumn{4}{c}{Yes} \\\\",
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(lot_tex, file.path(TABLES_DIR, "table_main_lottery_v2.tex"))
message("  Saved: table_main_lottery_v2.tex")


# --- 7g. Main IV results table ---
message("\n--- 7g. Main IV results table ---")

iv_tex <- c(
  "\\begin{tabular}{l cccc}",
  "\\toprule",
  "Outcome & Coef. & SE & $p$ & $p_{\\text{BH}}$ \\\\",
  "\\midrule"
)

for (out in outcome_order) {
  label <- if (out %in% names(nice_out_labels)) nice_out_labels[out] else out
  r <- iv_results_df |> filter(outcome == out)
  if (nrow(r) > 0) {
    iv_tex <- c(iv_tex,
      paste0(label, " & ",
             sprintf("%.2f", r$coef), add_stars(r$pv), " & ",
             sprintf("%.2f", r$se), " & ",
             sprintf("%.3f", r$pv), " & ",
             if (!is.na(r$pv_bh)) sprintf("%.3f", r$pv_bh) else "--", " \\\\"))
  }
}

iv_tex <- c(iv_tex,
  "\\midrule",
  paste0("$N$ & \\multicolumn{4}{c}{", nrow(est_iv), "} \\\\"),
  paste0("First-stage $F$ & \\multicolumn{4}{c}{", sprintf("%.1f", fs_fstat), "} \\\\"),
  "Instrument & \\multicolumn{4}{c}{Peer component (LOO)} \\\\",
  "Ranking controls & \\multicolumn{4}{c}{Linear + quadratic} \\\\",
  "Year FE & \\multicolumn{4}{c}{Yes} \\\\",
  "Clustered (player) & \\multicolumn{4}{c}{Yes} \\\\",
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(iv_tex, file.path(TABLES_DIR, "table_main_iv_v2.tex"))
message("  Saved: table_main_iv_v2.tex")


# ==============================================================================
# FIX 8: REPORT EXACT SAMPLE COUNTS
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 8: SAMPLE COUNTS")
message(strrep("=", 70))

# Full GS lottery (before first-LL restriction, but with 2006+ filter)
gs_full_sample <- losers_gs |>
  filter(!is.na(rank_among_losers), rank_among_losers <= 4, n_ll_slots > 0)
gs_full_atp <- gs_full_sample |> filter(tour == "ATP")
gs_full_wta <- gs_full_sample |> filter(tour == "WTA")

# First-LL GS lottery
first_ll_gs_lottery <- gs_lottery  # already first-LL, 2006+, >=1 LL
first_ll_gs_atp <- gs_atp
first_ll_gs_wta <- gs_wta

counts_md <- c(
  "# Sample Counts (v2 -- after critical fixes)",
  paste0("Generated: ", Sys.time()),
  "",
  "## Grand Slam Lottery Sample",
  "",
  "### Full GS lottery (2006+, events with >= 1 LL slot, top-4 ranked losers)",
  paste0("- ATP: N = ", nrow(gs_full_atp), " (treated = ", sum(gs_full_atp$got_ll),
         ", control = ", sum(!gs_full_atp$got_ll), ")"),
  paste0("- WTA: N = ", nrow(gs_full_wta), " (treated = ", sum(gs_full_wta$got_ll),
         ", control = ", sum(!gs_full_wta$got_ll), ")"),
  paste0("- Pooled: N = ", nrow(gs_full_sample), " (treated = ", sum(gs_full_sample$got_ll),
         ", control = ", sum(!gs_full_sample$got_ll), ")"),
  "",
  "### First-LL-only GS lottery (2006+)",
  paste0("- ATP: N = ", nrow(first_ll_gs_atp), " (treated = ", sum(first_ll_gs_atp$got_ll),
         ", control = ", sum(!first_ll_gs_atp$got_ll), ")"),
  paste0("- WTA: N = ", nrow(first_ll_gs_wta), " (treated = ", sum(first_ll_gs_wta$got_ll),
         ", control = ", sum(!first_ll_gs_wta$got_ll), ")"),
  paste0("- Pooled: N = ", nrow(gs_lottery), " (treated = ", sum(gs_lottery$got_ll),
         ", control = ", sum(!gs_lottery$got_ll), ")"),
  "",
  "## Non-GS Selection Model (IV) Sample",
  "",
  paste0("- Full non-GS losers (events with >= 1 LL): N = ", nrow(losers_nongs)),
  paste0("- Non-GS losers with selection prob: N = ", n_with_sp),
  paste0("- First-LL-only non-GS: N = ", nrow(first_ll_nongs)),
  paste0("- First-LL-only IV estimation sample: N = ", nrow(est_iv),
         " (treated = ", sum(est_iv$got_ll),
         ", control = ", sum(!est_iv$got_ll), ")"),
  "",
  "## Event Filtering",
  "",
  paste0("- Total events with qualifying losers (all levels, all years): ", n_events_before),
  paste0("- Events with >= 1 LL spot awarded: ", n_events_with_ll),
  paste0("- Events with 0 LL spots (DROPPED): ", n_events_without_ll),
  paste0("- Observations dropped (zero-LL events): ", n_obs_losers_zero_ll),
  "",
  "### GS 2006 filter",
  paste0("- GS qualifying losers pre-2006 (DROPPED): ", fix_summary$gs_pre2006_dropped),
  "",
  "## Selection Probability Coverage",
  "",
  paste0("- OLD (script 11): ", fix_summary$n_sel_prob_old, " obs across ",
         fix_summary$n_tournaments_old, " tournaments"),
  paste0("- NEW (script 15): ", fix_summary$n_sel_prob_new, " obs across ",
         fix_summary$n_tournaments_new, " tournaments"),
  paste0("- Gain: ", fix_summary$n_sel_prob_new - fix_summary$n_sel_prob_old, " obs, ",
         fix_summary$n_tournaments_new - fix_summary$n_tournaments_old, " tournaments"),
  "",
  "## Instrument Details",
  "",
  "- Instrument: peer_component = Pr(rank among losers <= c_t | i loses)",
  "- Does NOT include own loss probability",
  paste0("- First-stage F-stat: ", sprintf("%.1f", fs_fstat)),
  paste0("- First-stage coef: ", sprintf("%.4f", fs_coef)),
  "",
  "## Controls",
  "",
  "- Pre-treatment ranking points (linear + quadratic)",
  "- Player age",
  "- Year fixed effects",
  "- Clustered standard errors at player level"
)

writeLines(counts_md, file.path(OUTPUT_DIR, "sample_counts_v2.md"))
message("  Saved: sample_counts_v2.md")


# ==============================================================================
# SAVE CLEANED ESTIMATION SAMPLE
# ==============================================================================
message("\n", strrep("=", 70))
message("SAVING OUTPUTS")
message(strrep("=", 70))

saveRDS(list(
  gs_lottery = gs_lottery,
  est_iv = est_iv,
  losers_gs = losers_gs,
  losers_nongs = losers_nongs,
  all_qualifiers = all_qualifiers
), file.path(CLEANED_DIR, "estimation_sample_v2.rds"))
message("  Saved: estimation_sample_v2.rds")

saveRDS(list(
  lottery_results = all_lottery_results,
  iv_results = iv_results_df,
  iv_event_fe = iv_event_fe_df,
  first_stage = tibble(coef = fs_coef, se = fs_se, fstat = fs_fstat)
), file.path(CLEANED_DIR, "critical_fixes_results.rds"))
message("  Saved: critical_fixes_results.rds")


# ==============================================================================
# SUMMARY
# ==============================================================================
message("\n", strrep("=", 70))
message("CRITICAL FIXES COMPLETE")
message(strrep("=", 70))

message("\nFix 1 (GS 2006+): Dropped ", fix_summary$gs_pre2006_dropped, " pre-2006 GS observations")
message("Fix 2 (Full SP): ", fix_summary$n_sel_prob_new, " obs across ",
        fix_summary$n_tournaments_new, " tournaments (was ",
        fix_summary$n_sel_prob_old, " / ", fix_summary$n_tournaments_old, ")")
message("Fix 3 (>=1 LL): Dropped ", fix_summary$n_events_dropped_zero_ll, " events (",
        fix_summary$n_obs_dropped_zero_ll, " obs) with zero LL slots")
message("Fix 4 (Ranking controls): Pre-treatment ranking points (linear + quadratic) in all regressions")
message("Fix 5 (Instrument): peer_component only, no own_loss_prob")
message("Fix 6 (GS names): All GS tournaments mapped to 4 slams")
message("Fix 7: All descriptive tables rebuilt with _v2 suffix")
message("Fix 8: Sample counts in Output/sample_counts_v2.md")
message("\nFirst-stage F-stat: ", sprintf("%.1f", fs_fstat))
message("IV estimation sample N: ", nrow(est_iv))
message("GS lottery sample N: ", nrow(gs_lottery))

message("\n", strrep("=", 70))
message("DONE")
message(strrep("=", 70))
