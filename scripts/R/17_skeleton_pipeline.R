# ==============================================================================
# 17_skeleton_pipeline.R
# Complete analysis pipeline aligned to Skeleton.tex specification.
#
# KEY FIX: The eligible pool is ALL final-round qualifying losers,
# NOT restricted to the "top-4 ranked losers" as in previous scripts.
#
# Purpose:  Build estimation samples, construct outcomes, produce all tables
#           and figures specified in the skeleton.
# Inputs:   Data/raw/*.rds, Data/cleaned/elo_history.rds,
#           Data/cleaned/wta_elo_history.rds
# Outputs:  Tables/*.tex, Figures/*.pdf, Output/final_sample_counts.md,
#           Output/skeleton_pipeline_summary.md,
#           Data/cleaned/skeleton_*.rds
# Dependencies: dplyr, tidyr, readr, stringr, fixest, ggplot2, here
# ==============================================================================

set.seed(20260325)

# --- Packages -----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(fixest)
library(ggplot2)
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

# --- Helpers ------------------------------------------------------------------
add_stars <- function(pv) {
  ifelse(is.na(pv), "",
    ifelse(pv < 0.01, "$^{***}$",
      ifelse(pv < 0.05, "$^{**}$",
        ifelse(pv < 0.1, "$^{*}$", ""))))
}

fmt <- function(x, d = 2) sprintf(paste0("%.", d, "f"), x)

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

summary_log <- character()
slog <- function(...) {
  msg <- paste0(...)
  summary_log <<- c(summary_log, msg)
  message(msg)
}

# ==============================================================================
# STEP 0: LOAD ALL DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 0: LOAD ALL DATA")
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

message("  ATP main: ", nrow(atp_main), " matches")
message("  ATP qual: ", nrow(atp_qual), " matches")
message("  WTA main: ", nrow(wta_main), " matches")
message("  WTA qual: ", nrow(wta_qual), " matches")
message("  ATP rankings: ", nrow(atp_rankings_raw))
message("  WTA rankings: ", nrow(wta_rankings_raw))

# ==============================================================================
# STEP 1: BUILD ESTIMATION SAMPLES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 1: BUILD ESTIMATION SAMPLES")
message(strrep("=", 70))

# --- 1a. Extract final-round qualifying losers --------------------------------
build_qualifiers <- function(main_df, qual_df, tour_label) {
  message("  Building ", tour_label, " qualifiers...")

  # Tour-specific tournament level codes
  # ATP: G (Grand Slam), M (Masters 1000), A (250/500)

  # WTA: G (Grand Slam), PM (Premier Mandatory / WTA 1000), P (Premier / WTA 500), I (International / WTA 250)
  if (tour_label == "WTA") {
    valid_levels <- c("G", "PM", "P", "I")
    min_nongs_year <- 2009L  # WTA tier system changed in 2009
  } else {
    valid_levels <- c("G", "M", "A")
    min_nongs_year <- 2007L
  }

  qual_tour <- qual_df |>
    mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
    filter(year >= 2000,
           tourney_level %in% valid_levels,
           str_detect(round, "^Q")) |>
    # For non-GS, enforce tour-specific minimum year
    filter(tourney_level == "G" | year >= min_nongs_year)

  final_round <- qual_tour |>
    group_by(tourney_id) |>
    summarise(max_qual_round = max(round), .groups = "drop")

  final_matches <- qual_tour |>
    inner_join(final_round, by = "tourney_id") |>
    filter(round == max_qual_round)

  message("    Final qualifying round matches: ", nrow(final_matches))

  # Losers of final qualifying round = LL candidate pool
  ql <- final_matches |>
    transmute(
      tourney_id, tourney_name, tourney_date, tourney_level, surface, draw_size,
      year = as.integer(str_sub(tourney_date, 1, 4)),
      player_id = loser_id, player_name = loser_name,
      player_rank = loser_rank, player_rank_points = loser_rank_points,
      player_age = loser_age, player_hand = loser_hand,
      player_ht = loser_ht, player_ioc = loser_ioc,
      qual_opponent_id = winner_id, qual_opponent_rank = winner_rank,
      score, minutes,
      tour = tour_label
    )

  message("    Qualifying losers (player-tournament): ", nrow(ql))

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

  ql <- ql |>
    mutate(got_ll = as.integer(paste0(tourney_id, "_", player_id) %in%
                                 paste0(ll_entries$tourney_id, "_", ll_entries$ll_player_id)))

  # LL slots per tournament
  ll_slots <- ql |>
    group_by(tourney_id) |>
    summarise(n_ll_slots = sum(got_ll), .groups = "drop")

  ql <- ql |>
    left_join(ll_slots, by = "tourney_id") |>
    mutate(n_ll_slots = replace_na(n_ll_slots, 0L))

  # Rank among losers (for non-GS IV)
  ql <- ql |>
    group_by(tourney_id) |>
    mutate(
      n_losers_at_event = n(),
      rank_among_losers = rank(ifelse(is.na(player_rank), 9999, player_rank),
                                ties.method = "min")
    ) |>
    ungroup()

  # LL main draw match details
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

  ql <- ql |>
    left_join(ll_summary_t |> rename(player_id = ll_player_id),
              by = c("tourney_id", "player_id")) |>
    mutate(
      ll_matches_played = replace_na(ll_matches_played, 0L),
      ll_matches_won = replace_na(ll_matches_won, 0L)
    )

  message("    Total: ", nrow(ql), " (LL: ", sum(ql$got_ll), ")")
  ql
}

atp_losers <- build_qualifiers(atp_main, atp_qual, "ATP")
wta_losers <- build_qualifiers(wta_main, wta_qual, "WTA")
all_losers <- bind_rows(atp_losers, wta_losers)

message("  Combined qualifying losers: ", nrow(all_losers),
        " (ATP: ", sum(all_losers$tour == "ATP"),
        ", WTA: ", sum(all_losers$tour == "WTA"), ")")

# Parse event date
all_losers <- all_losers |>
  mutate(event_date = as.Date(as.character(tourney_date), format = "%Y%m%d"))

# --- 1b. Map GS tournament names to 4 slams ----------------------------------
map_slam <- function(tname, tid) {
  tname_lower <- str_to_lower(tname)
  slam <- case_when(
    str_detect(tname_lower, "australian|aus open|melbourne") ~ "Australian Open",
    str_detect(tname_lower, "roland|french|paris") ~ "Roland Garros",
    str_detect(tname_lower, "wimbledon") ~ "Wimbledon",
    str_detect(tname_lower, "us open|u\\.s\\. open|flushing|us\\.open") ~ "US Open",
    TRUE ~ NA_character_
  )
  # Fallback on tourney_id
  slam <- case_when(
    !is.na(slam) ~ slam,
    str_detect(tid, "580") ~ "Australian Open",
    str_detect(tid, "520") ~ "Roland Garros",
    str_detect(tid, "540") ~ "Wimbledon",
    str_detect(tid, "560") ~ "US Open",
    TRUE ~ slam
  )
  slam
}

all_losers <- all_losers |>
  mutate(slam_name = if_else(tourney_level == "G",
                              map_slam(tourney_name, tourney_id),
                              NA_character_))

gs_unmapped <- all_losers |>
  filter(tourney_level == "G", is.na(slam_name))
if (nrow(gs_unmapped) > 0) {
  message("  WARNING: ", nrow(gs_unmapped), " unmapped GS observations")
}

# --- 1c. Apply sample restrictions --------------------------------------------

# GS sample: 2006+ only (lottery started after Wimbledon 2005)
n_gs_pre2006 <- sum(all_losers$tourney_level == "G" & all_losers$year < 2006)
message("  GS qualifying losers pre-2006 (dropped): ", n_gs_pre2006)
all_losers <- all_losers |>
  filter(!(tourney_level == "G" & year < 2006))

# Restrict to events where >= 1 LL was awarded
n_before_ll_filter <- nrow(all_losers)
all_losers_est <- all_losers |>
  filter(n_ll_slots > 0, !is.na(event_date))

n_after_ll_filter <- nrow(all_losers_est)
message("  Before LL>=1 filter: ", n_before_ll_filter)
message("  After LL>=1 filter: ", n_after_ll_filter)
message("  Dropped (0 LL events): ", n_before_ll_filter - n_after_ll_filter)

# Split by tournament level
# GS eligible pool: top 4 ranked final-round qualifying losers per event
# (Grand Slam Rulebook, Rule 24: "Lucky Losers shall be drawn from among
#  the four highest-ranked players who lost in the final round of qualifying")
gs_all_losers <- all_losers_est |> filter(tourney_level == "G")
n_gs_before_top4 <- nrow(gs_all_losers)
gs_sample <- gs_all_losers |> filter(rank_among_losers <= 4)
n_gs_after_top4 <- nrow(gs_sample)
message("  GS top-4 pool restriction: ", n_gs_before_top4, " -> ", n_gs_after_top4,
        " (dropped ", n_gs_before_top4 - n_gs_after_top4, " non-pool members)")
nongs_sample <- all_losers_est |> filter(tourney_level != "G")

slog("## Step 1: Sample Construction")
slog("- ATP GS (2006-2024): N = ", sum(gs_sample$tour == "ATP"),
     " (LL: ", sum(gs_sample$tour == "ATP" & gs_sample$got_ll == 1),
     ", Control: ", sum(gs_sample$tour == "ATP" & gs_sample$got_ll == 0), ")")
slog("- WTA GS (2006-2024): N = ", sum(gs_sample$tour == "WTA"),
     " (LL: ", sum(gs_sample$tour == "WTA" & gs_sample$got_ll == 1),
     ", Control: ", sum(gs_sample$tour == "WTA" & gs_sample$got_ll == 0), ")")
slog("- ATP non-GS: N = ", sum(nongs_sample$tour == "ATP"),
     " (LL: ", sum(nongs_sample$tour == "ATP" & nongs_sample$got_ll == 1), ")")
slog("- WTA non-GS: N = ", sum(nongs_sample$tour == "WTA"),
     " (LL: ", sum(nongs_sample$tour == "WTA" & nongs_sample$got_ll == 1), ")")
slog("- Unique ATP GS events: ", n_distinct(gs_sample$tourney_id[gs_sample$tour == "ATP"]))
slog("- Unique WTA GS events: ", n_distinct(gs_sample$tourney_id[gs_sample$tour == "WTA"]))
slog("- Unique ATP GS players: ", n_distinct(gs_sample$player_id[gs_sample$tour == "ATP"]))
slog("- Unique WTA GS players: ", n_distinct(gs_sample$player_id[gs_sample$tour == "WTA"]))
slog("")

# ==============================================================================
# STEP 2: CONSTRUCT PRE-TREATMENT VARIABLES AND RANKING/ELO TRAJECTORIES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 2: CONSTRUCT VARIABLES")
message(strrep("=", 70))

# --- 2a. Rankings at horizons -------------------------------------------------
atp_rankings <- atp_rankings_raw |>
  mutate(rank_date = as.Date(as.character(ranking_date), format = "%Y%m%d")) |>
  filter(!is.na(rank_date)) |>
  select(player_id = player, rank_date, rank, points)

wta_rankings <- wta_rankings_raw |>
  mutate(rank_date = as.Date(as.character(ranking_date), format = "%Y%m%d")) |>
  filter(!is.na(rank_date)) |>
  select(player_id = player, rank_date, rank, points)

horizons_weeks <- c(0, 4, 8, 12, 26, 52)

for (h in horizons_weeks) {
  col_pts  <- paste0("points_t", h)
  if (col_pts %in% names(all_losers_est)) {
    message("  Horizon t+", h, "w: already present, skipping")
    next
  }

  message("  Horizon t+", h, "w: merging rankings...")

  # ATP
  atp_targets <- all_losers_est |>
    filter(tour == "ATP") |>
    transmute(tourney_id, player_id, target_date = event_date + h * 7)

  atp_matched <- atp_targets |>
    inner_join(atp_rankings, by = "player_id", relationship = "many-to-many") |>
    filter(abs(as.numeric(rank_date - target_date)) <= 10) |>
    mutate(date_diff = abs(as.numeric(rank_date - target_date))) |>
    group_by(tourney_id, player_id) |>
    slice_min(date_diff, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(tourney_id, player_id,
           !!col_pts := points)

  # WTA
  wta_targets <- all_losers_est |>
    filter(tour == "WTA") |>
    transmute(tourney_id, player_id, target_date = event_date + h * 7)

  wta_matched <- wta_targets |>
    inner_join(wta_rankings, by = "player_id", relationship = "many-to-many") |>
    filter(abs(as.numeric(rank_date - target_date)) <= 10) |>
    mutate(date_diff = abs(as.numeric(rank_date - target_date))) |>
    group_by(tourney_id, player_id) |>
    slice_min(date_diff, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(tourney_id, player_id,
           !!col_pts := points)

  combined_matched <- bind_rows(atp_matched, wta_matched)
  all_losers_est <- all_losers_est |>
    left_join(combined_matched, by = c("tourney_id", "player_id"))
}

# Compute points changes
all_losers_est <- all_losers_est |>
  mutate(
    points_change_4w  = points_t4  - points_t0,
    points_change_8w  = points_t8  - points_t0,
    points_change_12w = points_t12 - points_t0,
    points_change_26w = points_t26 - points_t0,
    points_change_52w = points_t52 - points_t0
  )

# --- 2b. Elo at horizons -----------------------------------------------------
if (!is.null(elo_history)) {
  message("  Merging ATP Elo trajectories...")
  for (h in horizons_weeks) {
    col_elo <- paste0("elo_t", h)
    if (col_elo %in% names(all_losers_est)) next
    target <- all_losers_est |>
      filter(tour == "ATP") |>
      transmute(tourney_id, player_id, target_date = event_date + h * 7)
    elo_m <- target |>
      inner_join(elo_history, by = "player_id", relationship = "many-to-many") |>
      filter(abs(as.numeric(match_date - target_date)) <= 21) |>
      mutate(date_diff = abs(as.numeric(match_date - target_date))) |>
      group_by(tourney_id, player_id) |>
      slice_min(date_diff, n = 1, with_ties = FALSE) |>
      ungroup() |>
      select(tourney_id, player_id, !!col_elo := elo)
    all_losers_est <- all_losers_est |>
      left_join(elo_m, by = c("tourney_id", "player_id"))
  }
}

if (!is.null(wta_elo_history)) {
  message("  Merging WTA Elo trajectories...")
  for (h in horizons_weeks) {
    col_elo <- paste0("wta_elo_t", h)
    target <- all_losers_est |>
      filter(tour == "WTA") |>
      transmute(tourney_id, player_id, target_date = event_date + h * 7)
    elo_m <- target |>
      inner_join(wta_elo_history, by = "player_id", relationship = "many-to-many") |>
      filter(abs(as.numeric(match_date - target_date)) <= 21) |>
      mutate(date_diff = abs(as.numeric(match_date - target_date))) |>
      group_by(tourney_id, player_id) |>
      slice_min(date_diff, n = 1, with_ties = FALSE) |>
      ungroup() |>
      select(tourney_id, player_id, !!col_elo := elo)
    all_losers_est <- all_losers_est |>
      left_join(elo_m, by = c("tourney_id", "player_id"))
  }
}

# Unify Elo columns (ATP uses elo_tX, WTA uses wta_elo_tX)
for (h in horizons_weeks) {
  elo_col <- paste0("elo_t", h)
  wta_col <- paste0("wta_elo_t", h)
  if (elo_col %in% names(all_losers_est) && wta_col %in% names(all_losers_est)) {
    all_losers_est[[elo_col]] <- coalesce(all_losers_est[[elo_col]],
                                           all_losers_est[[wta_col]])
    all_losers_est[[wta_col]] <- NULL
  } else if (wta_col %in% names(all_losers_est)) {
    all_losers_est[[elo_col]] <- all_losers_est[[wta_col]]
    all_losers_est[[wta_col]] <- NULL
  }
}

# Compute Elo changes
for (h in c(4, 8, 12, 26, 52)) {
  elo_col <- paste0("elo_t", h)
  change_col <- paste0("elo_change_", h, "w")
  if (elo_col %in% names(all_losers_est) && "elo_t0" %in% names(all_losers_est)) {
    all_losers_est[[change_col]] <- all_losers_est[[elo_col]] - all_losers_est[["elo_t0"]]
  }
}

# --- 2c. Pre-treatment variables (Z^pre) --------------------------------------
all_losers_est <- all_losers_est |>
  mutate(
    pre_rank_pts    = coalesce(points_t0, player_rank_points, 0),
    pre_rank_pts_sq = pre_rank_pts^2
  )

if ("elo_t0" %in% names(all_losers_est)) {
  all_losers_est <- all_losers_est |>
    mutate(
      pre_elo    = coalesce(elo_t0, 1500),
      pre_elo_sq = pre_elo^2
    )
} else {
  all_losers_est <- all_losers_est |>
    mutate(
      pre_elo    = 1500,
      pre_elo_sq = pre_elo^2
    )
}

# --- 2d. LL history variables -------------------------------------------------
message("  Building LL history variables...")

# Sort by player and date, compute cumulative LL opportunities and wins
all_losers_est <- all_losers_est |>
  arrange(player_id, event_date) |>
  group_by(player_id) |>
  mutate(
    # Cumulative count of GS qualifying losses BEFORE this one
    cum_gs_opp   = cumsum(tourney_level == "G") - (tourney_level == "G"),
    cum_gs_ll    = cumsum(tourney_level == "G" & got_ll == 1) - (tourney_level == "G" & got_ll == 1),
    cum_nongs_opp = cumsum(tourney_level != "G") - (tourney_level != "G"),
    cum_nongs_ll  = cumsum(tourney_level != "G" & got_ll == 1) - (tourney_level != "G" & got_ll == 1),
    # Binary: ever had a prior LL spot of any type
    had_prior_ll = as.integer((cum_gs_ll + cum_nongs_ll) > 0)
  ) |>
  ungroup()

# Rename for clarity
all_losers_est <- all_losers_est |>
  rename(
    n_prior_gs_ll_opp  = cum_gs_opp,
    n_prior_gs_ll_won  = cum_gs_ll,
    n_prior_nongs_ll_opp = cum_nongs_opp,
    n_prior_nongs_ll_won = cum_nongs_ll
  )

# --- 2e. Immediate outcomes (event-level) -------------------------------------
message("  Building immediate outcomes...")

# Look up main draw results for each LL entry
all_main <- bind_rows(
  atp_main |> mutate(tour = "ATP"),
  wta_main |> mutate(tour = "WTA")
) |>
  mutate(
    year = as.integer(str_sub(tourney_date, 1, 4)),
    match_date = as.Date(as.character(tourney_date), format = "%Y%m%d")
  ) |>
  filter(year >= 2000)

ll_match_details <- bind_rows(
  all_main |>
    filter(winner_entry == "LL") |>
    transmute(tourney_id, player_id = winner_id, won_match = 1L, round),
  all_main |>
    filter(loser_entry == "LL") |>
    transmute(tourney_id, player_id = loser_id, won_match = 0L, round)
)

ll_event_summary <- ll_match_details |>
  group_by(tourney_id, player_id) |>
  summarise(
    md_matches_played = n(),
    md_matches_won    = sum(won_match),
    md_any_win        = as.integer(any(won_match == 1L)),
    .groups = "drop"
  )

# GS ranking points by round
gs_round_points <- tibble(
  round = c("R128", "R64", "R32", "R16", "QF", "SF", "F"),
  gs_pts = c(10, 45, 90, 180, 360, 720, 1200)
)

ll_gs_points <- ll_match_details |>
  left_join(gs_round_points, by = "round") |>
  group_by(tourney_id, player_id) |>
  summarise(
    deepest_round_pts = max(gs_pts, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(deepest_round_pts = ifelse(is.infinite(deepest_round_pts), 10, deepest_round_pts))

all_losers_est <- all_losers_est |>
  left_join(ll_event_summary, by = c("tourney_id", "player_id")) |>
  left_join(ll_gs_points, by = c("tourney_id", "player_id")) |>
  mutate(
    md_matches_played   = ifelse(got_ll == 1, replace_na(md_matches_played, 1L), 0L),
    md_matches_won      = ifelse(got_ll == 1, replace_na(md_matches_won, 0L), 0L),
    md_any_win          = ifelse(got_ll == 1, replace_na(md_any_win, 0L), 0L),
    md_points           = ifelse(got_ll == 1 & tourney_level == "G",
                                  replace_na(deepest_round_pts, 10), 0)
  )

# --- 2f. Dynamic outcomes (future main draws, matches at 250+) ----------------
message("  Building dynamic outcomes (future counts)...")

build_future_data <- function(main_df, tour_label) {
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
  # Include ATP (G, M, A) and WTA (G, PM, P, I) 250+ equivalent levels
  match_app <- all_app |> filter(tourney_level %in% c("G", "M", "A", "PM", "P", "I"))
  list(tourneys = tourney_app, matches = match_app)
}

atp_future <- build_future_data(atp_main, "ATP")
wta_future <- build_future_data(wta_main, "WTA")

all_tourney_app <- bind_rows(
  atp_future$tourneys |> mutate(tour = "ATP"),
  wta_future$tourneys |> mutate(tour = "WTA")
)
all_match_app <- bind_rows(
  atp_future$matches |> mutate(tour = "ATP"),
  wta_future$matches |> mutate(tour = "WTA")
)

# Pre-split by player for speed
tourney_by_player <- split(all_tourney_app, all_tourney_app$player_id)
match_by_player   <- split(all_match_app, all_match_app$player_id)

# Initialize outcome columns for ALL horizons
for (h in c(4, 8, 12, 26, 52)) {
  all_losers_est[[paste0("n_main_draws_", h, "w")]]     <- NA_integer_
  all_losers_est[[paste0("n_matches_250plus_", h, "w")]] <- NA_integer_
}

n_total <- nrow(all_losers_est)
message("  Computing future counts for ", n_total, " observations...")

for (i in seq_len(n_total)) {
  if (i %% 5000 == 0) message("    Row ", i, " / ", n_total)
  pid   <- all_losers_est$player_id[i]
  edate <- all_losers_est$event_date[i]
  if (is.na(edate)) next

  pt <- tourney_by_player[[as.character(pid)]]
  pm <- match_by_player[[as.character(pid)]]

  for (h in c(4, 8, 12, 26, 52)) {
    end_date <- edate + h * 7

    if (!is.null(pt)) {
      pt_f <- pt[!is.na(pt$match_date) & pt$match_date > edate & pt$match_date <= end_date, ]
      all_losers_est[[paste0("n_main_draws_", h, "w")]][i] <- n_distinct(pt_f$tourney_id)
    } else {
      all_losers_est[[paste0("n_main_draws_", h, "w")]][i] <- 0L
    }

    if (!is.null(pm)) {
      pm_f <- pm[!is.na(pm$match_date) & pm$match_date > edate & pm$match_date <= end_date, ]
      all_losers_est[[paste0("n_matches_250plus_", h, "w")]][i] <- nrow(pm_f)
    } else {
      all_losers_est[[paste0("n_matches_250plus_", h, "w")]][i] <- 0L
    }
  }
}

message("  Dynamic outcomes computed.")

# ==============================================================================
# STEP 3: MULTIPLE-TREATMENT ADJUSTMENT (CENSORING)
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 3: MULTIPLE-TREATMENT ADJUSTMENT")
message(strrep("=", 70))

# For each observation, find weeks to next qualifying loss at ANY event
all_losers_est <- all_losers_est |>
  arrange(player_id, event_date) |>
  group_by(player_id) |>
  mutate(
    next_event_date = lead(event_date),
    weeks_to_next_ll = as.numeric(difftime(next_event_date, event_date, units = "weeks"))
  ) |>
  ungroup()

# Censor outcomes at min(horizon, weeks_to_next_ll)
# If next qualifying loss happens before the horizon, set that horizon's outcome to NA
for (h in c(4, 8, 12, 26, 52)) {
  for (prefix in c("points_change_", "elo_change_")) {
    col <- paste0(prefix, h, "w")
    if (col %in% names(all_losers_est)) {
      all_losers_est[[paste0(col, "_cens")]] <- ifelse(
        !is.na(all_losers_est$weeks_to_next_ll) & all_losers_est$weeks_to_next_ll < h,
        NA_real_,
        all_losers_est[[col]]
      )
    }
  }
  for (prefix in c("n_main_draws_", "n_matches_250plus_")) {
    col <- paste0(prefix, h, "w")
    if (col %in% names(all_losers_est)) {
      all_losers_est[[paste0(col, "_cens")]] <- ifelse(
        !is.na(all_losers_est$weeks_to_next_ll) & all_losers_est$weeks_to_next_ll < h,
        NA_integer_,
        all_losers_est[[col]]
      )
    }
  }
}

n_censored <- sum(!is.na(all_losers_est$weeks_to_next_ll) &
                    all_losers_est$weeks_to_next_ll < 52, na.rm = TRUE)
slog("## Step 3: Censoring")
slog("- Observations censored at 52w horizon: ", n_censored, " / ", nrow(all_losers_est))
slog("")

# --- Create event FE variable -------------------------------------------------
all_losers_est <- all_losers_est |>
  mutate(
    slam_year = if_else(tourney_level == "G",
                         paste0(coalesce(slam_name, tourney_name), "_", year),
                         paste0(tourney_name, "_", year))
  )

# --- Split into final estimation samples --------------------------------------
# GS: restrict to top-4 ranked qualifying losers (the LL-eligible pool per Grand Slam rules)
gs_est  <- all_losers_est |> filter(tourney_level == "G", rank_among_losers <= 4)
nongs_est <- all_losers_est |> filter(tourney_level != "G")
message("  GS estimation sample (top-4 pool): ", nrow(gs_est),
        " (ATP: ", sum(gs_est$tour == "ATP"), ", WTA: ", sum(gs_est$tour == "WTA"), ")")

gs_atp <- gs_est |> filter(tour == "ATP")
gs_wta <- gs_est |> filter(tour == "WTA")

slog("## Final GS Estimation Sample")
slog("- ATP GS: N = ", nrow(gs_atp),
     " (LL: ", sum(gs_atp$got_ll), ", Control: ", sum(gs_atp$got_ll == 0), ")")
slog("- WTA GS: N = ", nrow(gs_wta),
     " (LL: ", sum(gs_wta$got_ll), ", Control: ", sum(gs_wta$got_ll == 0), ")")
slog("- Unique ATP events: ", n_distinct(gs_atp$tourney_id))
slog("- Unique WTA events: ", n_distinct(gs_wta$tourney_id))
slog("- Unique ATP players: ", n_distinct(gs_atp$player_id))
slog("- Unique WTA players: ", n_distinct(gs_wta$player_id))
slog("")

# Save estimation datasets
saveRDS(all_losers_est, file.path(CLEANED_DIR, "skeleton_all_losers_est.rds"))
saveRDS(gs_est, file.path(CLEANED_DIR, "skeleton_gs_est.rds"))
saveRDS(nongs_est, file.path(CLEANED_DIR, "skeleton_nongs_est.rds"))
message("  Saved estimation datasets.")


# ==============================================================================
# STEP 4: LL DISTRIBUTION TABLES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 4: LL DISTRIBUTION TABLES")
message(strrep("=", 70))

# --- Table: GS LL distribution ------------------------------------------------
gs_dist <- gs_est |>
  group_by(slam_name, tour) |>
  summarise(
    n_ll = sum(got_ll),
    n_control = sum(got_ll == 0),
    n_total = n(),
    .groups = "drop"
  )

# Pivot to wide: ATP and WTA side by side
gs_dist_wide <- gs_dist |>
  pivot_wider(
    names_from = tour,
    values_from = c(n_ll, n_control, n_total),
    values_fill = 0
  )

# Totals
gs_totals <- gs_est |>
  group_by(tour) |>
  summarise(n_ll = sum(got_ll), n_control = sum(got_ll == 0), n_total = n(), .groups = "drop") |>
  pivot_wider(names_from = tour, values_from = c(n_ll, n_control, n_total), values_fill = 0)

slam_order <- c("Australian Open", "Roland Garros", "Wimbledon", "US Open")

gs_tex <- c(
  "\\begin{tabular}{l rrr rrr}",
  "\\toprule",
  " & \\multicolumn{3}{c}{ATP} & \\multicolumn{3}{c}{WTA} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
  "Grand Slam & LL & Control & Total & LL & Control & Total \\\\",
  "\\midrule"
)

for (slam in slam_order) {
  r <- gs_dist_wide |> filter(slam_name == slam)
  if (nrow(r) == 0) {
    gs_tex <- c(gs_tex, paste0(slam, " & 0 & 0 & 0 & 0 & 0 & 0 \\\\"))
  } else {
    atp_ll  <- if ("n_ll_ATP" %in% names(r)) r$n_ll_ATP else 0
    atp_ct  <- if ("n_control_ATP" %in% names(r)) r$n_control_ATP else 0
    atp_tot <- if ("n_total_ATP" %in% names(r)) r$n_total_ATP else 0
    wta_ll  <- if ("n_ll_WTA" %in% names(r)) r$n_ll_WTA else 0
    wta_ct  <- if ("n_control_WTA" %in% names(r)) r$n_control_WTA else 0
    wta_tot <- if ("n_total_WTA" %in% names(r)) r$n_total_WTA else 0
    gs_tex <- c(gs_tex, paste0(slam, " & ", atp_ll, " & ", atp_ct, " & ", atp_tot,
                                " & ", wta_ll, " & ", wta_ct, " & ", wta_tot, " \\\\"))
  }
}

gs_tex <- c(gs_tex, "\\midrule",
  paste0("Total & ",
         if ("n_ll_ATP" %in% names(gs_totals)) gs_totals$n_ll_ATP else 0, " & ",
         if ("n_control_ATP" %in% names(gs_totals)) gs_totals$n_control_ATP else 0, " & ",
         if ("n_total_ATP" %in% names(gs_totals)) gs_totals$n_total_ATP else 0, " & ",
         if ("n_ll_WTA" %in% names(gs_totals)) gs_totals$n_ll_WTA else 0, " & ",
         if ("n_control_WTA" %in% names(gs_totals)) gs_totals$n_control_WTA else 0, " & ",
         if ("n_total_WTA" %in% names(gs_totals)) gs_totals$n_total_WTA else 0, " \\\\"),
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(gs_tex, file.path(TABLES_DIR, "table_ll_dist_gs.tex"))
message("  Saved: table_ll_dist_gs.tex")

# --- Table: Non-GS LL distribution -------------------------------------------
nongs_est <- nongs_est |>
  mutate(level_label = case_when(
    tourney_level == "M"  ~ "Masters 1000",
    tourney_level == "A"  ~ "ATP 250/500",
    tourney_level == "PM" ~ "WTA 1000 (Masters equiv.)",
    tourney_level == "P"  ~ "WTA 500",
    tourney_level == "I"  ~ "WTA 250",
    TRUE ~ "Other"
  ))

nongs_dist <- nongs_est |>
  group_by(level_label, tour) |>
  summarise(n_ll = sum(got_ll), n_control = sum(got_ll == 0), n_total = n(), .groups = "drop") |>
  pivot_wider(names_from = tour, values_from = c(n_ll, n_control, n_total), values_fill = 0)

nongs_totals <- nongs_est |>
  group_by(tour) |>
  summarise(n_ll = sum(got_ll), n_control = sum(got_ll == 0), n_total = n(), .groups = "drop") |>
  pivot_wider(names_from = tour, values_from = c(n_ll, n_control, n_total), values_fill = 0)

nongs_level_order <- c("Masters 1000", "ATP 250/500",
                        "WTA 1000 (Masters equiv.)", "WTA 500", "WTA 250")

nongs_tex <- c(
  "\\begin{tabular}{l rrr rrr}",
  "\\toprule",
  " & \\multicolumn{3}{c}{ATP} & \\multicolumn{3}{c}{WTA} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
  "Tournament Level & LL & Control & Total & LL & Control & Total \\\\",
  "\\midrule"
)

for (lev in nongs_level_order) {
  r <- nongs_dist |> filter(level_label == lev)
  if (nrow(r) == 0) {
    nongs_tex <- c(nongs_tex, paste0(lev, " & 0 & 0 & 0 & 0 & 0 & 0 \\\\"))
  } else {
    nongs_tex <- c(nongs_tex, paste0(lev, " & ",
      if ("n_ll_ATP" %in% names(r)) r$n_ll_ATP else 0, " & ",
      if ("n_control_ATP" %in% names(r)) r$n_control_ATP else 0, " & ",
      if ("n_total_ATP" %in% names(r)) r$n_total_ATP else 0, " & ",
      if ("n_ll_WTA" %in% names(r)) r$n_ll_WTA else 0, " & ",
      if ("n_control_WTA" %in% names(r)) r$n_control_WTA else 0, " & ",
      if ("n_total_WTA" %in% names(r)) r$n_total_WTA else 0, " \\\\"))
  }
}

nongs_tex <- c(nongs_tex, "\\midrule",
  paste0("Total & ",
         if ("n_ll_ATP" %in% names(nongs_totals)) nongs_totals$n_ll_ATP else 0, " & ",
         if ("n_control_ATP" %in% names(nongs_totals)) nongs_totals$n_control_ATP else 0, " & ",
         if ("n_total_ATP" %in% names(nongs_totals)) nongs_totals$n_total_ATP else 0, " & ",
         if ("n_ll_WTA" %in% names(nongs_totals)) nongs_totals$n_ll_WTA else 0, " & ",
         if ("n_control_WTA" %in% names(nongs_totals)) nongs_totals$n_control_WTA else 0, " & ",
         if ("n_total_WTA" %in% names(nongs_totals)) nongs_totals$n_total_WTA else 0, " \\\\"),
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(nongs_tex, file.path(TABLES_DIR, "table_ll_dist_nongs.tex"))
message("  Saved: table_ll_dist_nongs.tex")


# ==============================================================================
# STEP 5: SUMMARY STATISTICS TABLE
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 5: SUMMARY STATISTICS")
message(strrep("=", 70))

make_sumstats <- function(data, label) {
  vars <- c("pre_rank_pts", "pre_elo", "player_age",
            "had_prior_ll", "n_prior_gs_ll_opp", "n_prior_nongs_ll_opp",
            "md_any_win", "md_matches_played", "md_points",
            "points_change_4w", "points_change_12w", "points_change_26w", "points_change_52w",
            "n_main_draws_4w", "n_main_draws_12w", "n_main_draws_26w", "n_main_draws_52w",
            "n_matches_250plus_4w", "n_matches_250plus_12w",
            "n_matches_250plus_26w", "n_matches_250plus_52w")
  if ("elo_change_12w" %in% names(data)) vars <- c(vars, "elo_change_12w", "elo_change_26w")

  var_labels <- c(
    "pre_rank_pts" = "Ranking points",
    "pre_elo" = "Elo rating",
    "player_age" = "Age",
    "had_prior_ll" = "Had prior LL",
    "n_prior_gs_ll_opp" = "Prior GS qual. losses",
    "n_prior_nongs_ll_opp" = "Prior non-GS qual. losses",
    "md_any_win" = "Main-draw match win",
    "md_matches_played" = "Main-draw matches played",
    "md_points" = "Ranking points at event",
    "points_change_4w" = "Points change (4w)",
    "points_change_12w" = "Points change (12w)",
    "points_change_26w" = "Points change (26w)",
    "points_change_52w" = "Points change (52w)",
    "n_main_draws_4w" = "Main draws (4w)",
    "n_main_draws_12w" = "Main draws (12w)",
    "n_main_draws_26w" = "Main draws (26w)",
    "n_main_draws_52w" = "Main draws (52w)",
    "n_matches_250plus_4w" = "Matches 250+ (4w)",
    "n_matches_250plus_12w" = "Matches 250+ (12w)",
    "n_matches_250plus_26w" = "Matches 250+ (26w)",
    "n_matches_250plus_52w" = "Matches 250+ (52w)",
    "elo_change_12w" = "Elo change (12w)",
    "elo_change_26w" = "Elo change (26w)"
  )

  rows <- list()
  for (v in vars) {
    if (!v %in% names(data)) next
    x <- data[[v]]
    d <- data$got_ll
    ok <- !is.na(x)

    m_ll <- mean(x[ok & d == 1], na.rm = TRUE)
    s_ll <- sd(x[ok & d == 1], na.rm = TRUE)
    m_ct <- mean(x[ok & d == 0], na.rm = TRUE)
    s_ct <- sd(x[ok & d == 0], na.rm = TRUE)
    diff <- m_ll - m_ct
    tt <- tryCatch(t.test(x[ok & d == 1], x[ok & d == 0]), error = function(e) NULL)
    pv <- if (!is.null(tt)) tt$p.value else NA_real_

    rows[[v]] <- tibble(
      variable = v,
      label = var_labels[v],
      ll_mean = m_ll, ll_sd = s_ll,
      ct_mean = m_ct, ct_sd = s_ct,
      diff = diff, pvalue = pv,
      n_ll = sum(ok & d == 1), n_ct = sum(ok & d == 0)
    )
  }
  bind_rows(rows) |> mutate(sample = label)
}

ss_atp <- make_sumstats(gs_atp, "ATP")
ss_wta <- make_sumstats(gs_wta, "WTA")

# Build LaTeX table
build_sumstats_tex <- function(ss_list, filename) {
  tex <- c(
    "\\begin{tabular}{l rr rr rr}",
    "\\toprule",
    " & \\multicolumn{2}{c}{LL} & \\multicolumn{2}{c}{Control} & & \\\\",
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
    " & Mean & SD & Mean & SD & Diff & $p$ \\\\",
    "\\midrule"
  )

  for (samp_label in names(ss_list)) {
    ss <- ss_list[[samp_label]]
    tex <- c(tex, paste0("\\multicolumn{7}{l}{\\textit{Panel: ", samp_label, " (N = ",
                          ss$n_ll[1] + ss$n_ct[1], ", LL = ", ss$n_ll[1],
                          ", Control = ", ss$n_ct[1], ")}} \\\\"))
    tex <- c(tex, "\\addlinespace")

    for (i in seq_len(nrow(ss))) {
      r <- ss[i, ]
      tex <- c(tex, paste0("\\quad ", r$label,
                             " & ", fmt(r$ll_mean), " & ", fmt(r$ll_sd),
                             " & ", fmt(r$ct_mean), " & ", fmt(r$ct_sd),
                             " & ", fmt(r$diff), add_stars(r$pvalue),
                             " & ", fmt(r$pvalue, 3), " \\\\"))
    }
    tex <- c(tex, "\\addlinespace")
  }

  tex <- c(tex, "\\bottomrule", "\\end{tabular}")
  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

build_sumstats_tex(list("ATP" = ss_atp, "WTA" = ss_wta), "table_sumstats_gs.tex")

# Non-GS summary stats
ss_nongs_atp <- make_sumstats(nongs_est |> filter(tour == "ATP"), "ATP non-GS")
ss_nongs_wta <- make_sumstats(nongs_est |> filter(tour == "WTA"), "WTA non-GS")
build_sumstats_tex(list("ATP non-GS" = ss_nongs_atp, "WTA non-GS" = ss_nongs_wta),
                   "table_sumstats_nongs.tex")


# ==============================================================================
# STEP 6: IMMEDIATE EFFECTS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 6: IMMEDIATE EFFECTS")
message(strrep("=", 70))

run_immediate <- function(data, label_prefix) {
  immediate_outcomes <- c("md_any_win", "md_matches_played", "md_points")
  results <- list()

  # Check slam_year FE has variation
  sy_counts <- data |> count(slam_year) |> filter(n >= 2)
  data_fe <- data |> filter(slam_year %in% sy_counts$slam_year)

  for (out in immediate_outcomes) {
    if (!out %in% names(data_fe)) next
    y <- data_fe[[out]]
    d <- data_fe$got_ll
    ok <- !is.na(y) & !is.na(d)
    if (sum(ok & d == 1) < 2 || sum(ok & d == 0) < 2) next

    # t-test
    tt <- tryCatch(t.test(y[ok & d == 1], y[ok & d == 0]), error = function(e) NULL)

    # Event FE regression: Y = alpha_e + beta * Treat + epsilon
    # Cluster SE at player level
    fe_fit <- tryCatch({
      feols(as.formula(paste0(out, " ~ got_ll | slam_year")),
            data = data_fe[ok, ], vcov = ~player_id)
    }, error = function(e) {
      # Fallback: heteroskedasticity-robust
      tryCatch(
        feols(as.formula(paste0(out, " ~ got_ll | slam_year")),
              data = data_fe[ok, ], vcov = "hetero"),
        error = function(e2) NULL
      )
    })

    results[[out]] <- tibble(
      sample = label_prefix,
      outcome = out,
      mean_treated = mean(y[ok & d == 1]),
      mean_control = mean(y[ok & d == 0]),
      ttest_diff = mean(y[ok & d == 1]) - mean(y[ok & d == 0]),
      ttest_pv = if (!is.null(tt)) tt$p.value else NA_real_,
      fe_coef = if (!is.null(fe_fit)) coef(fe_fit)["got_ll"] else NA_real_,
      fe_se   = if (!is.null(fe_fit)) sqrt(vcov(fe_fit)["got_ll", "got_ll"]) else NA_real_,
      fe_pv   = if (!is.null(fe_fit)) {
        2 * pnorm(-abs(coef(fe_fit)["got_ll"] / sqrt(vcov(fe_fit)["got_ll", "got_ll"])))
      } else NA_real_,
      n_obs = sum(ok),
      n_treated = sum(ok & d == 1),
      n_control = sum(ok & d == 0)
    )
  }
  bind_rows(results)
}

imm_atp <- run_immediate(gs_atp, "ATP")
imm_wta <- run_immediate(gs_wta, "WTA")
immediate_all <- bind_rows(imm_atp, imm_wta)

saveRDS(immediate_all, file.path(CLEANED_DIR, "skeleton_immediate_effects.rds"))

# Build LaTeX table
immediate_labels <- c(
  "md_any_win" = "Main-draw match win",
  "md_matches_played" = "Main-draw matches played",
  "md_points" = "Ranking points at event"
)

imm_tex <- c(
  "\\begin{tabular}{l ccc ccc}",
  "\\toprule",
  " & \\multicolumn{3}{c}{$t$-test} & \\multicolumn{3}{c}{Event FE} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
  "Outcome & LL Mean & Ctrl Mean & $p$ & $\\hat{\\beta}$ & SE & $p$ \\\\",
  "\\midrule"
)

for (samp in c("ATP", "WTA")) {
  imm_tex <- c(imm_tex,
    paste0("\\multicolumn{7}{l}{\\textit{", samp, "}} \\\\"))
  sub <- immediate_all |> filter(sample == samp)
  for (out in c("md_any_win", "md_matches_played", "md_points")) {
    r <- sub |> filter(outcome == out)
    if (nrow(r) == 0) next
    lab <- immediate_labels[out]
    imm_tex <- c(imm_tex,
      paste0("\\quad ", lab, " & ",
             fmt(r$mean_treated), " & ",
             fmt(r$mean_control), " & ",
             fmt(r$ttest_pv, 3), " & ",
             fmt(r$fe_coef), add_stars(r$fe_pv), " & ",
             "(", fmt(r$fe_se), ") & ",
             fmt(r$fe_pv, 3), " \\\\"))
  }
  if (nrow(sub) > 0) {
    imm_tex <- c(imm_tex,
      paste0("\\quad $N$ & \\multicolumn{3}{c}{",
             sub$n_treated[1], " / ", sub$n_control[1],
             "} & \\multicolumn{3}{c}{", sub$n_obs[1], "} \\\\"))
  }
  if (samp == "ATP") imm_tex <- c(imm_tex, "\\addlinespace")
}

imm_tex <- c(imm_tex, "\\bottomrule", "\\end{tabular}")
writeLines(imm_tex, file.path(TABLES_DIR, "table_immediate.tex"))
message("  Saved: table_immediate.tex")

slog("## Step 6: Immediate Effects")
for (i in seq_len(nrow(immediate_all))) {
  r <- immediate_all[i, ]
  slog("- ", r$sample, " ", r$outcome, ": FE coef = ", fmt(r$fe_coef),
       ", p = ", fmt(r$fe_pv, 3), ", N = ", r$n_obs)
}
slog("")


# ==============================================================================
# STEP 7: POST-EPISODE DYNAMICS (GS)
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 7: POST-EPISODE DYNAMICS (GS)")
message(strrep("=", 70))

# Dynamic model: Y_{ie,h} = alpha_e + beta_h Treat + Gamma Z^pre + epsilon
# Z^pre = {pre_rank_pts, pre_rank_pts_sq, pre_elo, pre_elo_sq, player_age, had_prior_ll}
# Cluster at player level

dynamic_outcomes_by_horizon <- list()
for (h in c(4, 8, 12, 26, 52)) {
  dynamic_outcomes_by_horizon[[as.character(h)]] <- c(
    paste0("n_main_draws_", h, "w"),
    paste0("n_matches_250plus_", h, "w"),
    paste0("points_change_", h, "w")
  )
  if (paste0("elo_change_", h, "w") %in% names(gs_est)) {
    dynamic_outcomes_by_horizon[[as.character(h)]] <- c(
      dynamic_outcomes_by_horizon[[as.character(h)]],
      paste0("elo_change_", h, "w")
    )
  }
}

run_dynamic <- function(data, label_prefix) {
  # Check FE validity
  sy_counts <- data |> count(slam_year) |> filter(n >= 2)
  data_fe <- data |> filter(slam_year %in% sy_counts$slam_year)

  results <- list()

  for (h_str in names(dynamic_outcomes_by_horizon)) {
    outcomes_h <- dynamic_outcomes_by_horizon[[h_str]]
    for (out in outcomes_h) {
      if (!out %in% names(data_fe)) next
      y <- data_fe[[out]]
      d <- data_fe$got_ll
      ok <- !is.na(y) & !is.na(data_fe$pre_rank_pts) & !is.na(data_fe$player_age) &
            !is.na(d)
      if (sum(ok & d == 1) < 3 || sum(ok & d == 0) < 3) next

      # t-test
      tt <- tryCatch(t.test(y[ok & d == 1], y[ok & d == 0]), error = function(e) NULL)

      # Event FE + controls, clustered at player level
      # Build formula with available controls
      controls <- "pre_rank_pts + pre_rank_pts_sq + player_age + had_prior_ll"
      if ("pre_elo" %in% names(data_fe) && sum(!is.na(data_fe$pre_elo[ok])) > 10) {
        controls <- paste0(controls, " + pre_elo + pre_elo_sq")
      }

      fe_fit <- tryCatch({
        feols(as.formula(paste0(out, " ~ got_ll + ", controls, " | slam_year")),
              data = data_fe[ok, ], vcov = ~player_id)
      }, error = function(e) {
        # Fallback without Elo
        tryCatch(
          feols(as.formula(paste0(out, " ~ got_ll + pre_rank_pts + pre_rank_pts_sq + player_age + had_prior_ll | slam_year")),
                data = data_fe[ok, ], vcov = ~player_id),
          error = function(e2) {
            # Last fallback: no FE
            tryCatch(
              feols(as.formula(paste0(out, " ~ got_ll + pre_rank_pts + pre_rank_pts_sq + player_age")),
                    data = data_fe[ok, ], vcov = ~player_id),
              error = function(e3) NULL
            )
          }
        )
      })

      results[[paste0(out, "_", label_prefix)]] <- tibble(
        sample = label_prefix,
        outcome = out,
        horizon = h_str,
        mean_treated = mean(y[ok & d == 1], na.rm = TRUE),
        mean_control = mean(y[ok & d == 0], na.rm = TRUE),
        ttest_diff = mean(y[ok & d == 1]) - mean(y[ok & d == 0]),
        ttest_pv = if (!is.null(tt)) tt$p.value else NA_real_,
        fe_coef = if (!is.null(fe_fit)) coef(fe_fit)["got_ll"] else NA_real_,
        fe_se   = if (!is.null(fe_fit)) sqrt(vcov(fe_fit)["got_ll", "got_ll"]) else NA_real_,
        fe_pv   = if (!is.null(fe_fit)) {
          2 * pnorm(-abs(coef(fe_fit)["got_ll"] / sqrt(vcov(fe_fit)["got_ll", "got_ll"])))
        } else NA_real_,
        n_obs = sum(ok),
        n_treated = sum(ok & d == 1),
        n_control = sum(ok & d == 0)
      )
    }
  }
  bind_rows(results)
}

dyn_atp <- run_dynamic(gs_atp, "ATP")
dyn_wta <- run_dynamic(gs_wta, "WTA")

saveRDS(bind_rows(dyn_atp, dyn_wta), file.path(CLEANED_DIR, "skeleton_dynamic_gs.rds"))

# Build dynamic results LaTeX tables (one per tour)
build_dynamic_tex <- function(dyn_df, tour_label, filename) {
  # Determine available outcomes and horizons
  outcome_types <- c("n_main_draws", "n_matches_250plus", "points_change", "elo_change")
  outcome_labels <- c(
    "n_main_draws" = "Main draws entered",
    "n_matches_250plus" = "Matches at 250+",
    "points_change" = "Points change",
    "elo_change" = "Elo change"
  )

  horizons <- c("4", "8", "12", "26", "52")
  n_hor <- length(horizons)

  tex <- c(
    paste0("\\begin{tabular}{l", paste(rep(" cc", n_hor), collapse = ""), "}"),
    "\\toprule",
    paste0(" & ", paste(paste0("\\multicolumn{2}{c}{", horizons, "w}"), collapse = " & "), " \\\\"),
    paste(paste0("\\cmidrule(lr){", seq(2, 2*n_hor, 2), "-", seq(3, 2*n_hor+1, 2), "}"), collapse = " "),
    paste0("Outcome & ", paste(rep("$t$-test & FE", n_hor), collapse = " & "), " \\\\"),
    "\\midrule"
  )

  for (otype in outcome_types) {
    has_any <- any(str_detect(dyn_df$outcome, paste0("^", otype)))
    if (!has_any) next
    olab <- outcome_labels[otype]
    row_ttest <- paste0("\\quad ", olab, " ($t$-test)")
    row_fe    <- paste0("\\quad ", olab, " (FE)")

    cells_tt <- character()
    cells_fe <- character()
    for (h in horizons) {
      out_name <- paste0(otype, "_", h, "w")
      r <- dyn_df |> filter(outcome == out_name)
      if (nrow(r) == 0) {
        cells_tt <- c(cells_tt, " & ")
        cells_fe <- c(cells_fe, " & ")
      } else {
        cells_tt <- c(cells_tt, paste0(" & ", fmt(r$ttest_diff), add_stars(r$ttest_pv)))
        cells_fe <- c(cells_fe, paste0(" & ", fmt(r$fe_coef), add_stars(r$fe_pv),
                                        " & (", fmt(r$fe_se), ")"))
      }
    }

    # Simplified: show just FE coef with stars and SE below
    row_vals <- character()
    row_ses  <- character()
    for (h in horizons) {
      out_name <- paste0(otype, "_", h, "w")
      r <- dyn_df |> filter(outcome == out_name)
      if (nrow(r) == 0) {
        row_vals <- c(row_vals, " & & ")
        row_ses  <- c(row_ses, " & & ")
      } else {
        row_vals <- c(row_vals, paste0(" & ", fmt(r$ttest_diff), add_stars(r$ttest_pv),
                                        " & ", fmt(r$fe_coef), add_stars(r$fe_pv)))
        row_ses  <- c(row_ses, paste0(" & & (", fmt(r$fe_se), ")"))
      }
    }

    tex <- c(tex,
      paste0(olab, paste(row_vals, collapse = ""), " \\\\"),
      paste0("", paste(row_ses, collapse = ""), " \\\\"),
      "\\addlinespace"
    )
  }

  # N row
  n_row <- character()
  for (h in horizons) {
    # Find any outcome at this horizon
    h_rows <- dyn_df |> filter(horizon == h)
    if (nrow(h_rows) > 0) {
      n_row <- c(n_row, paste0(" & \\multicolumn{2}{c}{", h_rows$n_obs[1], "}"))
    } else {
      n_row <- c(n_row, " & \\multicolumn{2}{c}{--}")
    }
  }
  tex <- c(tex, paste0("$N$", paste(n_row, collapse = ""), " \\\\"))

  tex <- c(tex, "\\bottomrule", "\\end{tabular}")
  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

build_dynamic_tex(dyn_atp, "ATP", "table_dynamic_atp.tex")
build_dynamic_tex(dyn_wta, "WTA", "table_dynamic_wta.tex")

slog("## Step 7: Post-Episode Dynamics (GS)")
for (i in seq_len(nrow(dyn_atp))) {
  r <- dyn_atp[i, ]
  slog("- ATP ", r$outcome, ": FE coef = ", fmt(r$fe_coef), ", p = ", fmt(r$fe_pv, 3))
}
slog("")


# ==============================================================================
# STEP 8: HETEROGENEITY (GS)
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 8: HETEROGENEITY (GS)")
message(strrep("=", 70))

# Focus on 26w horizon for heterogeneity
hetero_outcomes <- c("n_main_draws_26w", "n_matches_250plus_26w",
                      "points_change_26w")
if ("elo_change_26w" %in% names(gs_est)) {
  hetero_outcomes <- c(hetero_outcomes, "elo_change_26w")
}

run_hetero_sub <- function(data, subgroup_label) {
  results <- list()
  sy_counts <- data |> count(slam_year) |> filter(n >= 2)
  data_fe <- data |> filter(slam_year %in% sy_counts$slam_year)

  for (out in hetero_outcomes) {
    if (!out %in% names(data_fe)) next
    y <- data_fe[[out]]
    d <- data_fe$got_ll
    ok <- !is.na(y) & !is.na(d)
    if (sum(ok & d == 1) < 2 || sum(ok & d == 0) < 2) next

    fe_fit <- tryCatch({
      feols(as.formula(paste0(out, " ~ got_ll + pre_rank_pts + pre_rank_pts_sq + player_age | slam_year")),
            data = data_fe[ok, ], vcov = ~player_id)
    }, error = function(e) {
      tryCatch(
        feols(as.formula(paste0(out, " ~ got_ll + pre_rank_pts + player_age")),
              data = data_fe[ok, ], vcov = "hetero"),
        error = function(e2) NULL
      )
    })

    results[[out]] <- tibble(
      subgroup = subgroup_label,
      outcome = out,
      fe_coef = if (!is.null(fe_fit)) coef(fe_fit)["got_ll"] else NA_real_,
      fe_se   = if (!is.null(fe_fit)) sqrt(vcov(fe_fit)["got_ll", "got_ll"]) else NA_real_,
      fe_pv   = if (!is.null(fe_fit)) {
        2 * pnorm(-abs(coef(fe_fit)["got_ll"] / sqrt(vcov(fe_fit)["got_ll", "got_ll"])))
      } else NA_real_,
      n_obs = sum(ok)
    )
  }
  bind_rows(results)
}

run_hetero_tour <- function(data, tour_label) {
  # a) By pre-episode ranking (above/below median)
  med_pts <- median(data$pre_rank_pts, na.rm = TRUE)
  h_rank_hi <- run_hetero_sub(data |> filter(pre_rank_pts >= med_pts), "High ranking pts")
  h_rank_lo <- run_hetero_sub(data |> filter(pre_rank_pts < med_pts), "Low ranking pts")

  # b) By age (above/below median)
  med_age <- median(data$player_age, na.rm = TRUE)
  h_age_old <- run_hetero_sub(data |> filter(player_age >= med_age), "Older")
  h_age_young <- run_hetero_sub(data |> filter(player_age < med_age), "Younger")

  # c) By prior LL
  h_prior_yes <- run_hetero_sub(data |> filter(had_prior_ll == 1), "Had prior LL")
  h_prior_no  <- run_hetero_sub(data |> filter(had_prior_ll == 0), "No prior LL")

  # d) By dose (matches won in the episode: 0, 1, 2+)
  # Note: only treated players have dose > 0, so this is treated-only analysis
  ll_data <- data |> filter(got_ll == 1)
  dose_summary <- tibble(
    subgroup = c("0 MD wins", "1 MD win", "2+ MD wins"),
    n = c(sum(ll_data$md_matches_won == 0),
          sum(ll_data$md_matches_won == 1),
          sum(ll_data$md_matches_won >= 2))
  )

  bind_rows(h_rank_hi, h_rank_lo, h_age_old, h_age_young,
            h_prior_yes, h_prior_no) |>
    mutate(tour = tour_label)
}

hetero_atp <- run_hetero_tour(gs_atp, "ATP")
hetero_wta <- run_hetero_tour(gs_wta, "WTA")

saveRDS(bind_rows(hetero_atp, hetero_wta),
        file.path(CLEANED_DIR, "skeleton_heterogeneity_gs.rds"))

# Build heterogeneity LaTeX table
build_hetero_tex <- function(hetero_df, tour_label, filename) {
  outcome_labels <- c(
    "n_main_draws_26w" = "Main draws (26w)",
    "n_matches_250plus_26w" = "Matches 250+ (26w)",
    "points_change_26w" = "Pts change (26w)",
    "elo_change_26w" = "Elo change (26w)"
  )

  subgroups <- unique(hetero_df$subgroup)
  avail_outcomes <- intersect(unique(hetero_df$outcome), names(outcome_labels))
  n_out <- length(avail_outcomes)

  tex <- c(
    paste0("\\begin{tabular}{l", paste(rep(" cc", n_out), collapse = ""), "}"),
    "\\toprule",
    paste0(" & ", paste(paste0("\\multicolumn{2}{c}{", outcome_labels[avail_outcomes], "}"),
                         collapse = " & "), " \\\\"),
    paste(paste0("\\cmidrule(lr){", seq(2, 2*n_out, 2), "-", seq(3, 2*n_out+1, 2), "}"),
          collapse = " "),
    paste0("Subgroup & ", paste(rep("Coef & $N$", n_out), collapse = " & "), " \\\\"),
    "\\midrule"
  )

  for (sg in subgroups) {
    cells <- character()
    for (out in avail_outcomes) {
      r <- hetero_df |> filter(subgroup == sg, outcome == out)
      if (nrow(r) == 0) {
        cells <- c(cells, " & & ")
      } else {
        cells <- c(cells, paste0(" & ", fmt(r$fe_coef), add_stars(r$fe_pv),
                                  " & ", r$n_obs))
      }
    }
    tex <- c(tex, paste0(sg, paste(cells, collapse = ""), " \\\\"))
  }

  tex <- c(tex, "\\bottomrule", "\\end{tabular}")
  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

build_hetero_tex(hetero_atp, "ATP", "table_hetero_atp.tex")
build_hetero_tex(hetero_wta, "WTA", "table_hetero_wta.tex")

slog("## Step 8: Heterogeneity")
slog("- ATP subgroups estimated: ", n_distinct(hetero_atp$subgroup))
slog("- WTA subgroups estimated: ", n_distinct(hetero_wta$subgroup))
slog("")


# ==============================================================================
# STEP 9: ROBUSTNESS -- FIRST-LL-ONLY
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 9: ROBUSTNESS -- FIRST-LL-ONLY")
message(strrep("=", 70))

# Restrict to each player's FIRST qualifying loss in our sample
first_ll_gs <- gs_est |>
  arrange(player_id, event_date) |>
  group_by(player_id) |>
  slice_head(n = 1) |>
  ungroup()

first_ll_atp <- first_ll_gs |> filter(tour == "ATP")
first_ll_wta <- first_ll_gs |> filter(tour == "WTA")

slog("## Step 9: First-LL-Only")
slog("- ATP first-LL: N = ", nrow(first_ll_atp),
     " (LL: ", sum(first_ll_atp$got_ll), ", Control: ", sum(first_ll_atp$got_ll == 0), ")")
slog("- WTA first-LL: N = ", nrow(first_ll_wta),
     " (LL: ", sum(first_ll_wta$got_ll), ", Control: ", sum(first_ll_wta$got_ll == 0), ")")
slog("")

# Re-run immediate and dynamic for first-LL-only
firstll_imm_atp <- run_immediate(first_ll_atp, "ATP first-LL")
firstll_imm_wta <- run_immediate(first_ll_wta, "WTA first-LL")
firstll_dyn_atp <- run_dynamic(first_ll_atp, "ATP first-LL")
firstll_dyn_wta <- run_dynamic(first_ll_wta, "WTA first-LL")

saveRDS(list(imm = bind_rows(firstll_imm_atp, firstll_imm_wta),
             dyn = bind_rows(firstll_dyn_atp, firstll_dyn_wta)),
        file.path(CLEANED_DIR, "skeleton_firstll_results.rds"))

# Build first-LL tables (abbreviated: just immediate + 26w)
build_firstll_tex <- function(imm_df, dyn_df, tour_label, filename) {
  tex <- c(
    "\\begin{tabular}{l cccc}",
    "\\toprule",
    "Outcome & LL Mean & Ctrl Mean & FE $\\hat{\\beta}$ & $N$ \\\\",
    "\\midrule",
    paste0("\\multicolumn{5}{l}{\\textit{Immediate effects}} \\\\")
  )

  for (out in c("md_any_win", "md_matches_played", "md_points")) {
    r <- imm_df |> filter(outcome == out)
    if (nrow(r) == 0) next
    lab <- c("md_any_win" = "Match win", "md_matches_played" = "Matches played",
             "md_points" = "Points")[out]
    tex <- c(tex, paste0("\\quad ", lab, " & ",
                          fmt(r$mean_treated), " & ", fmt(r$mean_control), " & ",
                          fmt(r$fe_coef), add_stars(r$fe_pv), " & ", r$n_obs, " \\\\"))
  }

  tex <- c(tex, "\\addlinespace",
    "\\multicolumn{5}{l}{\\textit{Post-episode dynamics (26w)}} \\\\")

  for (out in c("n_main_draws_26w", "n_matches_250plus_26w", "points_change_26w")) {
    r <- dyn_df |> filter(outcome == out)
    if (nrow(r) == 0) next
    lab <- c("n_main_draws_26w" = "Main draws",
             "n_matches_250plus_26w" = "Matches 250+",
             "points_change_26w" = "Points change")[out]
    tex <- c(tex, paste0("\\quad ", lab, " & ",
                          fmt(r$mean_treated), " & ", fmt(r$mean_control), " & ",
                          fmt(r$fe_coef), add_stars(r$fe_pv), " & ", r$n_obs, " \\\\"))
  }

  tex <- c(tex, "\\bottomrule", "\\end{tabular}")
  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

build_firstll_tex(firstll_imm_atp, firstll_dyn_atp, "ATP", "table_firstll_atp.tex")
build_firstll_tex(firstll_imm_wta, firstll_dyn_wta, "WTA", "table_firstll_wta.tex")


# ==============================================================================
# STEP 10: ROBUSTNESS -- VERIFIED LOTTERY
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 10: ROBUSTNESS -- VERIFIED LOTTERY")
message(strrep("=", 70))

# A "verified lottery" event is one where the LL selected was NOT the
# highest-ranked qualifying loser (proving random selection was used)
verified_events <- gs_est |>
  filter(got_ll == 1) |>
  group_by(tourney_id) |>
  summarise(min_ll_rank = min(rank_among_losers, na.rm = TRUE), .groups = "drop") |>
  filter(min_ll_rank > 1) |>
  pull(tourney_id)

message("  Verified lottery events: ", length(verified_events), " / ",
        n_distinct(gs_est$tourney_id))

verified_gs <- gs_est |> filter(tourney_id %in% verified_events)
verified_atp <- verified_gs |> filter(tour == "ATP")
verified_wta <- verified_gs |> filter(tour == "WTA")

slog("## Step 10: Verified Lottery")
slog("- Verified events: ", length(verified_events))
slog("- ATP verified: N = ", nrow(verified_atp),
     " (LL: ", sum(verified_atp$got_ll), ")")
slog("- WTA verified: N = ", nrow(verified_wta),
     " (LL: ", sum(verified_wta$got_ll), ")")
slog("")

# Run immediate + dynamic on verified subsample
ver_imm_atp <- run_immediate(verified_atp, "ATP verified")
ver_imm_wta <- run_immediate(verified_wta, "WTA verified")
ver_dyn_atp <- run_dynamic(verified_atp, "ATP verified")
ver_dyn_wta <- run_dynamic(verified_wta, "WTA verified")

saveRDS(list(imm = bind_rows(ver_imm_atp, ver_imm_wta),
             dyn = bind_rows(ver_dyn_atp, ver_dyn_wta)),
        file.path(CLEANED_DIR, "skeleton_verified_results.rds"))

# Build verified table (same format as first-LL)
build_firstll_tex(ver_imm_atp, ver_dyn_atp, "ATP", "table_verified_atp.tex")
build_firstll_tex(ver_imm_wta, ver_dyn_wta, "WTA", "table_verified_wta.tex")

# Combined verified table
ver_tex <- c(
  "\\begin{tabular}{l cccc cccc}",
  "\\toprule",
  " & \\multicolumn{4}{c}{ATP} & \\multicolumn{4}{c}{WTA} \\\\",
  "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9}",
  "Outcome & LL & Ctrl & $\\hat{\\beta}$ & $N$ & LL & Ctrl & $\\hat{\\beta}$ & $N$ \\\\",
  "\\midrule",
  "\\multicolumn{9}{l}{\\textit{Immediate effects}} \\\\"
)

for (out in c("md_any_win", "md_matches_played", "md_points")) {
  ra <- ver_imm_atp |> filter(outcome == out)
  rw <- ver_imm_wta |> filter(outcome == out)
  lab <- c("md_any_win" = "Match win", "md_matches_played" = "Matches played",
           "md_points" = "Points")[out]
  atp_str <- if (nrow(ra) > 0) paste0(fmt(ra$mean_treated), " & ", fmt(ra$mean_control),
                                        " & ", fmt(ra$fe_coef), add_stars(ra$fe_pv),
                                        " & ", ra$n_obs) else " & & & "
  wta_str <- if (nrow(rw) > 0) paste0(fmt(rw$mean_treated), " & ", fmt(rw$mean_control),
                                        " & ", fmt(rw$fe_coef), add_stars(rw$fe_pv),
                                        " & ", rw$n_obs) else " & & & "
  ver_tex <- c(ver_tex, paste0("\\quad ", lab, " & ", atp_str, " & ", wta_str, " \\\\"))
}

ver_tex <- c(ver_tex, "\\addlinespace",
  "\\multicolumn{9}{l}{\\textit{Post-episode dynamics (26w)}} \\\\")

for (out in c("n_main_draws_26w", "n_matches_250plus_26w", "points_change_26w")) {
  ra <- ver_dyn_atp |> filter(outcome == out)
  rw <- ver_dyn_wta |> filter(outcome == out)
  lab <- c("n_main_draws_26w" = "Main draws",
           "n_matches_250plus_26w" = "Matches 250+",
           "points_change_26w" = "Pts change")[out]
  atp_str <- if (nrow(ra) > 0) paste0(fmt(ra$mean_treated), " & ", fmt(ra$mean_control),
                                        " & ", fmt(ra$fe_coef), add_stars(ra$fe_pv),
                                        " & ", ra$n_obs) else " & & & "
  wta_str <- if (nrow(rw) > 0) paste0(fmt(rw$mean_treated), " & ", fmt(rw$mean_control),
                                        " & ", fmt(rw$fe_coef), add_stars(rw$fe_pv),
                                        " & ", rw$n_obs) else " & & & "
  ver_tex <- c(ver_tex, paste0("\\quad ", lab, " & ", atp_str, " & ", wta_str, " \\\\"))
}

ver_tex <- c(ver_tex, "\\bottomrule", "\\end{tabular}")
writeLines(ver_tex, file.path(TABLES_DIR, "table_verified.tex"))
message("  Saved: table_verified.tex")


# ==============================================================================
# STEP 11: BALANCE TABLE
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 11: BALANCE TABLE")
message(strrep("=", 70))

run_balance <- function(data, sample_label) {
  balance_vars <- c("pre_rank_pts", "player_age", "had_prior_ll",
                     "n_prior_gs_ll_opp", "n_prior_nongs_ll_opp")
  if ("pre_elo" %in% names(data)) balance_vars <- c(balance_vars, "pre_elo")

  bal_var_labels <- c(
    "pre_rank_pts" = "Ranking points",
    "player_age" = "Age",
    "had_prior_ll" = "Had prior LL",
    "n_prior_gs_ll_opp" = "Prior GS qual. losses",
    "n_prior_nongs_ll_opp" = "Prior non-GS qual. losses",
    "pre_elo" = "Elo rating"
  )

  rows <- list()
  for (v in balance_vars) {
    if (!v %in% names(data)) next
    x <- data[[v]]
    d <- data$got_ll
    ok <- !is.na(x)
    if (sum(ok & d == 1) < 3 || sum(ok & d == 0) < 3) next

    vlabel <- unname(bal_var_labels[v])
    tt <- tryCatch(t.test(x[ok & d == 1], x[ok & d == 0]), error = function(e) NULL)
    rows[[v]] <- tibble(
      variable = v, label = vlabel,
      ll_mean = mean(x[ok & d == 1]), ct_mean = mean(x[ok & d == 0]),
      diff = mean(x[ok & d == 1]) - mean(x[ok & d == 0]),
      pvalue = if (!is.null(tt)) tt$p.value else NA_real_
    )
  }
  bal <- bind_rows(rows)

  # Joint F-test: regress got_ll on all balance variables
  fml_str <- paste0("got_ll ~ ", paste(balance_vars[balance_vars %in% names(data)], collapse = " + "))
  f_test <- tryCatch({
    fit <- lm(as.formula(fml_str), data = data)
    fs <- summary(fit)$fstatistic
    f_pv <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)
    list(fstat = fs[1], pv = f_pv)
  }, error = function(e) list(fstat = NA, pv = NA))

  list(bal = bal |> mutate(sample = sample_label), f_test = f_test)
}

bal_atp <- run_balance(gs_atp, "ATP")
bal_wta <- run_balance(gs_wta, "WTA")

slog("## Step 11: Balance")
slog("- ATP joint F-test: F = ", fmt(bal_atp$f_test$fstat),
     ", p = ", fmt(bal_atp$f_test$pv, 3))
slog("- WTA joint F-test: F = ", fmt(bal_wta$f_test$fstat),
     ", p = ", fmt(bal_wta$f_test$pv, 3))
slog("")

# Build balance LaTeX table
bal_all <- bind_rows(bal_atp$bal, bal_wta$bal)
bal_tex <- c(
  "\\begin{tabular}{l rrr rrr}",
  "\\toprule",
  " & \\multicolumn{3}{c}{ATP} & \\multicolumn{3}{c}{WTA} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
  "Variable & LL & Control & $p$ & LL & Control & $p$ \\\\",
  "\\midrule"
)

for (v in unique(bal_all$variable)) {
  ra <- bal_all |> filter(variable == v, sample == "ATP")
  rw <- bal_all |> filter(variable == v, sample == "WTA")
  lab <- if (nrow(ra) > 0) ra$label[1] else rw$label[1]
  atp_str <- if (nrow(ra) > 0) paste0(fmt(ra$ll_mean), " & ", fmt(ra$ct_mean),
                                        " & ", fmt(ra$pvalue, 3)) else " & & "
  wta_str <- if (nrow(rw) > 0) paste0(fmt(rw$ll_mean), " & ", fmt(rw$ct_mean),
                                        " & ", fmt(rw$pvalue, 3)) else " & & "
  bal_tex <- c(bal_tex, paste0(lab, " & ", atp_str, " & ", wta_str, " \\\\"))
}

bal_tex <- c(bal_tex, "\\midrule",
  paste0("Joint $F$-test $p$-value & \\multicolumn{3}{c}{",
         fmt(bal_atp$f_test$pv, 3), "} & \\multicolumn{3}{c}{",
         fmt(bal_wta$f_test$pv, 3), "} \\\\"),
  "\\bottomrule", "\\end{tabular}")

writeLines(bal_tex, file.path(TABLES_DIR, "table_balance.tex"))
message("  Saved: table_balance.tex")


# ==============================================================================
# STEP 12: NON-GS IV (APPENDIX)
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 12: NON-GS IV")
message(strrep("=", 70))

# Build win probability models for instrument (one per tour)
build_win_model <- function(main_df, qual_df, tour_label) {
  message("  Building ", tour_label, " logit win model for instrument...")
  all_tmp <- bind_rows(
    main_df |> mutate(match_source = "main"),
    qual_df |> mutate(match_source = "qual")
  ) |>
    mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
    filter(year >= 2000, !is.na(winner_id), !is.na(loser_id))

  mp <- bind_rows(
    all_tmp |> filter(!is.na(winner_rank), !is.na(loser_rank)) |>
      transmute(player_rank = winner_rank, opponent_rank = loser_rank,
                player_age = winner_age, opponent_age = loser_age,
                player_ioc = winner_ioc, opponent_ioc = loser_ioc,
                surface, won = 1L, player_entry = winner_entry),
    all_tmp |> filter(!is.na(winner_rank), !is.na(loser_rank)) |>
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

  wm <- glm(
    won ~ log_rank_ratio + I(log_rank_ratio^2) + rank_diff +
      same_ioc + surface_clay + surface_grass + age_diff,
    data = mp, family = binomial(link = "logit")
  )
  message("  ", tour_label, " win model McFadden R2: ",
          round(1 - wm$deviance / wm$null.deviance, 4))
  wm
}

win_model_atp <- build_win_model(atp_main, atp_qual, "ATP")
win_model_wta <- build_win_model(wta_main, wta_qual, "WTA")

# Predict win probs for all qualifying round participants at non-GS events
# We need to get the full set of qualifiers (winners + losers) for non-GS
nongs_for_iv <- all_losers |>
  filter(tourney_level != "G", n_ll_slots > 0, !is.na(event_date)) |>
  mutate(
    log_rank_ratio = log(pmax(qual_opponent_rank, 1) / pmax(player_rank, 1)),
    rank_diff = qual_opponent_rank - player_rank,
    same_ioc = 0,  # Not available for all, use default
    age_diff = 0,  # Not available for all, use default
    surface_clay = as.integer(surface == "Clay"),
    surface_grass = as.integer(surface == "Grass")
  )

has_valid <- !is.na(nongs_for_iv$log_rank_ratio) & !is.na(nongs_for_iv$rank_diff)
nongs_for_iv$p_win_own_match <- 0.5

# Use tour-specific win model
is_atp <- nongs_for_iv$tour == "ATP"
is_wta <- nongs_for_iv$tour == "WTA"
nongs_for_iv$p_win_own_match[has_valid & is_atp] <- predict(
  win_model_atp, newdata = nongs_for_iv[has_valid & is_atp, ], type = "response"
)
nongs_for_iv$p_win_own_match[has_valid & is_wta] <- predict(
  win_model_wta, newdata = nongs_for_iv[has_valid & is_wta, ], type = "response"
)

# Compute peer_component instrument for each non-GS loser
message("  Computing peer_component for non-GS losers...")
nongs_tourney_ids <- unique(nongs_for_iv$tourney_id)
message("  Non-GS tournaments to process: ", length(nongs_tourney_ids))

sel_prob_results <- list()
counter <- 0

for (tid in nongs_tourney_ids) {
  counter <- counter + 1
  if (counter %% 100 == 0) message("    Tournament ", counter, " / ", length(nongs_tourney_ids))

  t_all <- nongs_for_iv |>
    filter(tourney_id == tid) |>
    mutate(
      rank_sort = ifelse(is.na(player_rank), 9999, player_rank),
      loss_prob = 1 - p_win_own_match
    ) |>
    arrange(rank_sort) |>
    mutate(rank_pos = row_number())

  c_t <- t_all$n_ll_slots[1]
  if (is.na(c_t) || c_t == 0) next

  for (idx in seq_len(nrow(t_all))) {
    player_id_i <- t_all$player_id[idx]
    rank_pos_i <- t_all$rank_pos[idx]

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
      peer_component = pr_ll_given_loss
    )
  }
}

sel_prob_full <- bind_rows(sel_prob_results)
message("  Selection probabilities computed: ", nrow(sel_prob_full))

# Merge to non-GS estimation sample
nongs_est <- nongs_est |>
  left_join(sel_prob_full, by = c("tourney_id", "player_id"))

n_with_sp <- sum(!is.na(nongs_est$peer_component))
message("  Non-GS losers with peer_component: ", n_with_sp, " / ", nrow(nongs_est))

# Run IV estimation
nongs_est <- nongs_est |>
  mutate(
    pre_rank_pts = coalesce(points_t0, player_rank_points, 0),
    pre_rank_pts_sq = pre_rank_pts^2
  )

# Ensure pre_elo is available for non-GS (impute with sample median if missing)
if (!"pre_elo" %in% names(nongs_est)) {
  nongs_est <- nongs_est |> mutate(pre_elo = 1500, pre_elo_sq = pre_elo^2)
} else {
  med_elo_nongs <- median(nongs_est$pre_elo, na.rm = TRUE)
  if (is.na(med_elo_nongs)) med_elo_nongs <- 1500
  nongs_est <- nongs_est |>
    mutate(
      pre_elo = coalesce(pre_elo, med_elo_nongs),
      pre_elo_sq = pre_elo^2
    )
}
if (!"had_prior_ll" %in% names(nongs_est)) {
  nongs_est <- nongs_est |> mutate(had_prior_ll = 0L)
}

# First stage -- ATP
nongs_iv_atp <- nongs_est |> filter(!is.na(peer_component), tour == "ATP")
# First stage -- WTA (separate win model needed; reuse ATP logit as approximation)
nongs_iv_wta <- nongs_est |> filter(!is.na(peer_component), tour == "WTA")

message("  ATP non-GS IV sample: ", nrow(nongs_iv_atp))
message("  WTA non-GS IV sample: ", nrow(nongs_iv_wta))

# Z^pre controls matching GS specification:
# pre_rank_pts + pre_rank_pts_sq + pre_elo + pre_elo_sq + player_age + had_prior_ll
nongs_zpre <- "pre_rank_pts + pre_rank_pts_sq + pre_elo + pre_elo_sq + player_age + had_prior_ll"

iv_first_stage <- tryCatch({
  feols(as.formula(paste0("got_ll ~ peer_component + ", nongs_zpre, " | year")),
        data = nongs_iv_atp, vcov = ~player_id)
}, error = function(e) { message("  First stage FAILED: ", e$message); NULL })

if (!is.null(iv_first_stage)) {
  fs_coef <- coef(iv_first_stage)["peer_component"]
  fs_se <- sqrt(vcov(iv_first_stage)["peer_component", "peer_component"])
  fs_fstat <- (fs_coef / fs_se)^2
  slog("## Step 12: Non-GS IV")
  slog("- ATP First stage coef: ", fmt(fs_coef, 4))
  slog("- ATP First stage F: ", fmt(fs_fstat, 1))
  slog("- ATP N: ", nrow(nongs_iv_atp))
  slog("")
}

# --- Generic IV runner for both tours -----------------------------------------
run_nongs_iv <- function(iv_data, tour_label, zpre_str) {
  iv_outcomes <- c("points_change_26w", "n_main_draws_26w", "n_matches_250plus_26w")
  if ("elo_change_26w" %in% names(iv_data)) iv_outcomes <- c(iv_outcomes, "elo_change_26w")

  iv_results <- list()
  for (out in iv_outcomes) {
    if (!out %in% names(iv_data)) next
    ok <- !is.na(iv_data[[out]])
    if (sum(ok) < 50) next

    tryCatch({
      iv_fit <- feols(
        as.formula(paste0(out, " ~ ", zpre_str, " | year | got_ll ~ peer_component")),
        data = iv_data[ok, ],
        vcov = ~player_id
      )
      iv_c <- coef(iv_fit)["fit_got_ll"]
      iv_s <- sqrt(vcov(iv_fit)["fit_got_ll", "fit_got_ll"])
      iv_p <- 2 * pnorm(-abs(iv_c / iv_s))

      iv_results[[out]] <- tibble(
        tour = tour_label,
        outcome = out, coef = iv_c, se = iv_s, pv = iv_p, n_obs = sum(ok)
      )
      slog("- IV ", tour_label, " ", out, ": coef = ", fmt(iv_c), ", SE = ", fmt(iv_s),
           ", p = ", fmt(iv_p, 3), ", N = ", sum(ok))
    }, error = function(e) message("  IV ", tour_label, " ", out, " FAILED: ", e$message))
  }
  bind_rows(iv_results)
}

# Second stage for key outcomes -- ATP
iv_results_atp <- run_nongs_iv(nongs_iv_atp, "ATP", nongs_zpre)

# Second stage for key outcomes -- WTA
iv_results_wta <- run_nongs_iv(nongs_iv_wta, "WTA", nongs_zpre)

iv_results_df <- bind_rows(iv_results_atp, iv_results_wta)
saveRDS(iv_results_df, file.path(CLEANED_DIR, "skeleton_iv_results.rds"))

# Build IV first stage table
fs_tex <- c(
  "\\begin{tabular}{lc}",
  "\\toprule",
  "Variable & Coefficient \\\\",
  "\\midrule"
)
if (!is.null(iv_first_stage)) {
  cf <- coef(iv_first_stage)
  se_v <- sqrt(diag(vcov(iv_first_stage)))
  for (v in names(cf)) {
    pv_v <- 2 * pnorm(-abs(cf[v] / se_v[v]))
    fs_tex <- c(fs_tex,
      paste0(v, " & ", fmt(cf[v], 4), add_stars(pv_v), " \\\\"),
      paste0(" & (", fmt(se_v[v], 4), ") \\\\"))
  }
  fs_tex <- c(fs_tex, "\\midrule",
    paste0("$F$-statistic & ", fmt(fs_fstat, 1), " \\\\"),
    paste0("$N$ & ", nrow(nongs_iv_atp), " \\\\"))
}
fs_tex <- c(fs_tex, "\\bottomrule", "\\end{tabular}")
writeLines(fs_tex, file.path(TABLES_DIR, "table_iv_first_stage.tex"))
message("  Saved: table_iv_first_stage.tex")

# Build IV second stage table -- ATP
iv_tex_atp <- c(
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "Outcome & IV Coef & SE & $N$ \\\\",
  "\\midrule"
)
out_labels <- c(
  "points_change_26w" = "Points change (26w)",
  "n_main_draws_26w" = "Main draws (26w)",
  "n_matches_250plus_26w" = "Matches 250+ (26w)",
  "elo_change_26w" = "Elo change (26w)"
)
for (i in seq_len(nrow(iv_results_atp))) {
  r <- iv_results_atp[i, ]
  lab <- if (r$outcome %in% names(out_labels)) out_labels[r$outcome] else r$outcome
  iv_tex_atp <- c(iv_tex_atp,
    paste0(lab, " & ", fmt(r$coef), add_stars(r$pv), " & (", fmt(r$se), ") & ", r$n_obs, " \\\\"))
}
iv_tex_atp <- c(iv_tex_atp, "\\bottomrule", "\\end{tabular}")
writeLines(iv_tex_atp, file.path(TABLES_DIR, "table_dynamic_nongs_iv.tex"))
message("  Saved: table_dynamic_nongs_iv.tex")

# Build IV second stage table -- WTA
iv_tex_wta <- c(
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "Outcome & IV Coef & SE & $N$ \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(iv_results_wta))) {
  r <- iv_results_wta[i, ]
  lab <- if (r$outcome %in% names(out_labels)) out_labels[r$outcome] else r$outcome
  iv_tex_wta <- c(iv_tex_wta,
    paste0(lab, " & ", fmt(r$coef), add_stars(r$pv), " & (", fmt(r$se), ") & ", r$n_obs, " \\\\"))
}
iv_tex_wta <- c(iv_tex_wta, "\\bottomrule", "\\end{tabular}")
writeLines(iv_tex_wta, file.path(TABLES_DIR, "table_dynamic_nongs_wta_iv.tex"))
message("  Saved: table_dynamic_nongs_wta_iv.tex")


# ==============================================================================
# STEP 13: EVENT STUDY FIGURES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 13: EVENT STUDY FIGURES")
message(strrep("=", 70))

# Build event study data: points trajectory from pre-event to post-event
build_event_study <- function(data, tour_label) {
  # Use points at each horizon
  horizons <- c(0, 4, 8, 12, 26, 52)
  es_data <- list()

  for (h in horizons) {
    col <- paste0("points_t", h)
    if (!col %in% names(data)) next
    es_data[[as.character(h)]] <- data |>
      filter(!is.na(.data[[col]])) |>
      group_by(got_ll) |>
      summarise(
        mean_pts = mean(.data[[col]], na.rm = TRUE),
        se_pts = sd(.data[[col]], na.rm = TRUE) / sqrt(n()),
        n = n(),
        .groups = "drop"
      ) |>
      mutate(weeks = h, tour = tour_label)
  }
  bind_rows(es_data)
}

es_atp <- build_event_study(gs_atp, "ATP")
es_wta <- build_event_study(gs_wta, "WTA")

# Normalize to baseline (t=0)
normalize_es <- function(es) {
  baseline <- es |> filter(weeks == 0) |> select(got_ll, baseline = mean_pts)
  es |>
    left_join(baseline, by = "got_ll") |>
    mutate(
      mean_pts_norm = mean_pts - baseline,
      group = ifelse(got_ll == 1, "LL", "Control")
    )
}

es_atp_norm <- normalize_es(es_atp)
es_wta_norm <- normalize_es(es_wta)

make_es_plot <- function(es_data, tour_label) {
  ggplot(es_data, aes(x = weeks, y = mean_pts_norm,
                       color = group, shape = group)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_line(linewidth = 0.8) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = mean_pts_norm - 1.96 * se_pts,
                       ymax = mean_pts_norm + 1.96 * se_pts),
                   width = 1.5, linewidth = 0.5) +
    scale_color_manual(values = c("LL" = col_treat, "Control" = col_control)) +
    scale_x_continuous(breaks = c(0, 4, 8, 12, 26, 52)) +
    labs(x = "Weeks after qualifying loss", y = "Change in ranking points") +
    theme_paper()
}

p_atp <- make_es_plot(es_atp_norm, "ATP")
p_wta <- make_es_plot(es_wta_norm, "WTA")

ggsave(file.path(FIGURES_DIR, "fig_event_study_atp.pdf"), p_atp,
       width = 7, height = 5)
ggsave(file.path(FIGURES_DIR, "fig_event_study_wta.pdf"), p_wta,
       width = 7, height = 5)
message("  Saved: fig_event_study_atp.pdf, fig_event_study_wta.pdf")


# ==============================================================================
# STEP 14: DOSE-RESPONSE FIGURE AND TABLE
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 14: DOSE-RESPONSE FIGURE AND TABLE")
message(strrep("=", 70))

# Among treated + control, show outcomes by dose (matches won in LL episode)
gs_dose <- gs_est |>
  mutate(dose_group = case_when(
    got_ll == 0             ~ "Control",
    md_matches_won == 0     ~ "0 wins",
    md_matches_won == 1     ~ "1 win",
    md_matches_won >= 2     ~ "2+ wins"
  ))

# Dose figure (treated only)
dose_data <- gs_dose |>
  filter(got_ll == 1, !is.na(points_change_26w)) |>
  group_by(dose_group, tour) |>
  summarise(
    mean_pts = mean(points_change_26w, na.rm = TRUE),
    se_pts = sd(points_change_26w, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

p_dose <- ggplot(dose_data, aes(x = dose_group, y = mean_pts, fill = tour)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_pts - 1.96 * se_pts,
                     ymax = mean_pts + 1.96 * se_pts),
                 position = position_dodge(width = 0.7), width = 0.3) +
  scale_fill_manual(values = c("ATP" = col_treat, "WTA" = col_control)) +
  labs(x = "Matches won as LL", y = "Points change at 26 weeks") +
  theme_paper()

ggsave(file.path(FIGURES_DIR, "fig_dose.pdf"), p_dose, width = 7, height = 5)
message("  Saved: fig_dose.pdf")

# --- Dose-response table with SE, stars, and OLS interactions -----------------
dose_outcomes <- c("points_change_26w", "points_change_52w",
                    "n_main_draws_26w", "n_matches_250plus_26w")
dose_labels <- c(
  "points_change_26w" = "Points $\\Delta$ 26w",
  "points_change_52w" = "Points $\\Delta$ 52w",
  "n_main_draws_26w" = "Main draws 26w",
  "n_matches_250plus_26w" = "Matches (250+) 26w"
)

dose_tex <- c(
  "\\begin{tabular}{l ccc cc c}",
  "\\toprule",
  " & \\multicolumn{3}{c}{Mean by Dose Group} & \\multicolumn{2}{c}{OLS Interaction} & \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-6}",
  "Outcome & 0 wins & 1 win & 2+ wins & LL coef. & Interaction & $N$ \\\\",
  "\\midrule"
)

ctrl_data <- gs_dose |> filter(dose_group == "Control")

for (out in dose_outcomes) {
  if (!out %in% names(gs_dose)) next
  d0 <- gs_dose |> filter(dose_group == "0 wins", !is.na(.data[[out]]))
  d1 <- gs_dose |> filter(dose_group == "1 win", !is.na(.data[[out]]))
  d2 <- gs_dose |> filter(dose_group == "2+ wins", !is.na(.data[[out]]))
  dc <- ctrl_data |> filter(!is.na(.data[[out]]))

  # Means and SE for each dose group
  m0 <- mean(d0[[out]], na.rm = TRUE); se0 <- sd(d0[[out]], na.rm = TRUE) / sqrt(nrow(d0))
  m1 <- mean(d1[[out]], na.rm = TRUE); se1 <- sd(d1[[out]], na.rm = TRUE) / sqrt(nrow(d1))
  m2 <- mean(d2[[out]], na.rm = TRUE); se2 <- sd(d2[[out]], na.rm = TRUE) / sqrt(nrow(d2))

  # t-test vs control for stars
  tt0 <- tryCatch(t.test(d0[[out]], dc[[out]]), error = function(e) NULL)
  tt1 <- tryCatch(t.test(d1[[out]], dc[[out]]), error = function(e) NULL)
  tt2 <- tryCatch(t.test(d2[[out]], dc[[out]]), error = function(e) NULL)
  pv0 <- if (!is.null(tt0)) tt0$p.value else NA_real_
  pv1 <- if (!is.null(tt1)) tt1$p.value else NA_real_
  pv2 <- if (!is.null(tt2)) tt2$p.value else NA_real_

  # OLS: outcome ~ got_ll + got_ll * md_matches_won
  # Use got_ll and interaction with md_matches_won among treated
  ols_data <- gs_dose |>
    filter(!is.na(.data[[out]])) |>
    mutate(ll_x_dose = got_ll * md_matches_won)
  ols_fit <- tryCatch(
    feols(as.formula(paste0(out, " ~ got_ll + ll_x_dose + pre_rank_pts + pre_rank_pts_sq + player_age | slam_year")),
          data = ols_data, vcov = ~player_id),
    error = function(e) NULL
  )
  ols_ll_coef <- if (!is.null(ols_fit)) coef(ols_fit)["got_ll"] else NA_real_
  ols_ll_se   <- if (!is.null(ols_fit)) sqrt(vcov(ols_fit)["got_ll", "got_ll"]) else NA_real_
  ols_ll_pv   <- if (!is.null(ols_fit)) 2 * pnorm(-abs(ols_ll_coef / ols_ll_se)) else NA_real_
  ols_int_coef <- if (!is.null(ols_fit)) coef(ols_fit)["ll_x_dose"] else NA_real_
  ols_int_se   <- if (!is.null(ols_fit)) sqrt(vcov(ols_fit)["ll_x_dose", "ll_x_dose"]) else NA_real_
  ols_int_pv   <- if (!is.null(ols_fit)) 2 * pnorm(-abs(ols_int_coef / ols_int_se)) else NA_real_
  n_ols <- if (!is.null(ols_fit)) nobs(ols_fit) else nrow(ols_data)

  lab <- dose_labels[out]
  # Row 1: coefficients with stars
  dose_tex <- c(dose_tex, paste0(
    lab,
    " & ", fmt(m0, 1), add_stars(pv0),
    " & ", fmt(m1, 1), add_stars(pv1),
    " & ", fmt(m2, 1), add_stars(pv2),
    " & ", fmt(ols_ll_coef), add_stars(ols_ll_pv),
    " & ", fmt(ols_int_coef), add_stars(ols_int_pv),
    " & ", n_ols, " \\\\"
  ))
  # Row 2: standard errors
  dose_tex <- c(dose_tex, paste0(
    " & (", fmt(se0, 1), ")",
    " & (", fmt(se1, 1), ")",
    " & (", fmt(se2, 1), ")",
    " & (", fmt(ols_ll_se), ")",
    " & (", fmt(ols_int_se), ")",
    " & \\\\"
  ))
  dose_tex <- c(dose_tex, "\\addlinespace")
}

dose_tex <- c(dose_tex, "\\bottomrule", "\\end{tabular}")
writeLines(dose_tex, file.path(TABLES_DIR, "table_dose_final.tex"))
message("  Saved: table_dose_final.tex")


# ==============================================================================
# STEP 14b: NON-GS MIRROR TABLES (APPENDIX)
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 14b: NON-GS MIRROR TABLES")
message(strrep("=", 70))

# Split non-GS by tour
nongs_atp_est <- nongs_est |> filter(tour == "ATP")
nongs_wta_est <- nongs_est |> filter(tour == "WTA")

message("  non-GS ATP: N=", nrow(nongs_atp_est), " (LL=", sum(nongs_atp_est$got_ll), ")")
message("  non-GS WTA: N=", nrow(nongs_wta_est), " (LL=", sum(nongs_wta_est$got_ll), ")")

# --- table_immediate_nongs.tex (mirrors table_immediate.tex) ------------------
# For non-GS, use IV instead of OLS event FE
run_immediate_nongs <- function(iv_data, label_prefix, zpre_str) {
  immediate_outcomes <- c("md_any_win", "md_matches_played", "md_points")
  results <- list()

  for (out in immediate_outcomes) {
    if (!out %in% names(iv_data)) next
    y <- iv_data[[out]]
    d <- iv_data$got_ll
    ok <- !is.na(y) & !is.na(d) & !is.na(iv_data$peer_component)
    if (sum(ok & d == 1) < 2 || sum(ok & d == 0) < 2) next

    tt <- tryCatch(t.test(y[ok & d == 1], y[ok & d == 0]), error = function(e) NULL)

    iv_fit <- tryCatch({
      feols(as.formula(paste0(out, " ~ ", zpre_str, " | year | got_ll ~ peer_component")),
            data = iv_data[ok, ], vcov = ~player_id)
    }, error = function(e) NULL)

    results[[out]] <- tibble(
      sample = label_prefix,
      outcome = out,
      mean_treated = mean(y[ok & d == 1]),
      mean_control = mean(y[ok & d == 0]),
      ttest_diff = mean(y[ok & d == 1]) - mean(y[ok & d == 0]),
      ttest_pv = if (!is.null(tt)) tt$p.value else NA_real_,
      fe_coef = if (!is.null(iv_fit)) coef(iv_fit)["fit_got_ll"] else NA_real_,
      fe_se   = if (!is.null(iv_fit)) sqrt(vcov(iv_fit)["fit_got_ll", "fit_got_ll"]) else NA_real_,
      fe_pv   = if (!is.null(iv_fit)) {
        2 * pnorm(-abs(coef(iv_fit)["fit_got_ll"] / sqrt(vcov(iv_fit)["fit_got_ll", "fit_got_ll"])))
      } else NA_real_,
      n_obs = sum(ok),
      n_treated = sum(ok & d == 1),
      n_control = sum(ok & d == 0)
    )
  }
  bind_rows(results)
}

imm_nongs_atp <- run_immediate_nongs(nongs_iv_atp, "ATP non-GS", nongs_zpre)
imm_nongs_wta <- run_immediate_nongs(nongs_iv_wta, "WTA non-GS", nongs_zpre)
immediate_nongs_all <- bind_rows(imm_nongs_atp, imm_nongs_wta)

# Build immediate nongs LaTeX (same format as table_immediate.tex)
imm_nongs_tex <- c(
  "\\begin{tabular}{l ccc ccc}",
  "\\toprule",
  " & \\multicolumn{3}{c}{$t$-test} & \\multicolumn{3}{c}{IV (2SLS)} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
  "Outcome & LL Mean & Ctrl Mean & $p$ & $\\hat{\\beta}_{IV}$ & SE & $p$ \\\\",
  "\\midrule"
)

for (samp in c("ATP non-GS", "WTA non-GS")) {
  imm_nongs_tex <- c(imm_nongs_tex,
    paste0("\\multicolumn{7}{l}{\\textit{", samp, "}} \\\\"))
  sub <- immediate_nongs_all |> filter(sample == samp)
  for (out in c("md_any_win", "md_matches_played", "md_points")) {
    r <- sub |> filter(outcome == out)
    if (nrow(r) == 0) next
    lab <- immediate_labels[out]
    imm_nongs_tex <- c(imm_nongs_tex,
      paste0("\\quad ", lab, " & ",
             fmt(r$mean_treated), " & ",
             fmt(r$mean_control), " & ",
             fmt(r$ttest_pv, 3), " & ",
             fmt(r$fe_coef), add_stars(r$fe_pv), " & ",
             "(", fmt(r$fe_se), ") & ",
             fmt(r$fe_pv, 3), " \\\\"))
  }
  if (nrow(sub) > 0) {
    imm_nongs_tex <- c(imm_nongs_tex,
      paste0("\\quad $N$ & \\multicolumn{3}{c}{",
             sub$n_treated[1], " / ", sub$n_control[1],
             "} & \\multicolumn{3}{c}{", sub$n_obs[1], "} \\\\"))
  }
  if (samp == "ATP non-GS") imm_nongs_tex <- c(imm_nongs_tex, "\\addlinespace")
}
imm_nongs_tex <- c(imm_nongs_tex, "\\bottomrule", "\\end{tabular}")
writeLines(imm_nongs_tex, file.path(TABLES_DIR, "table_immediate_nongs.tex"))
message("  Saved: table_immediate_nongs.tex")

# --- Dynamic non-GS tables (mirrors table_dynamic_atp/wta.tex) ---------------
# Use IV for non-GS dynamic estimates
run_dynamic_nongs <- function(iv_data, label_prefix, zpre_str) {
  results <- list()
  for (h_str in names(dynamic_outcomes_by_horizon)) {
    outcomes_h <- dynamic_outcomes_by_horizon[[h_str]]
    for (out in outcomes_h) {
      if (!out %in% names(iv_data)) next
      ok <- !is.na(iv_data[[out]]) & !is.na(iv_data$peer_component)
      if (sum(ok) < 50) next
      d <- iv_data$got_ll

      tt <- tryCatch(t.test(iv_data[[out]][ok & d == 1],
                              iv_data[[out]][ok & d == 0]),
                       error = function(e) NULL)

      iv_fit <- tryCatch({
        feols(as.formula(paste0(out, " ~ ", zpre_str, " | year | got_ll ~ peer_component")),
              data = iv_data[ok, ], vcov = ~player_id)
      }, error = function(e) NULL)

      results[[paste0(out, "_", label_prefix)]] <- tibble(
        sample = label_prefix,
        outcome = out,
        horizon = h_str,
        mean_treated = mean(iv_data[[out]][ok & d == 1], na.rm = TRUE),
        mean_control = mean(iv_data[[out]][ok & d == 0], na.rm = TRUE),
        ttest_diff = mean(iv_data[[out]][ok & d == 1]) - mean(iv_data[[out]][ok & d == 0]),
        ttest_pv = if (!is.null(tt)) tt$p.value else NA_real_,
        fe_coef = if (!is.null(iv_fit)) coef(iv_fit)["fit_got_ll"] else NA_real_,
        fe_se   = if (!is.null(iv_fit)) sqrt(vcov(iv_fit)["fit_got_ll", "fit_got_ll"]) else NA_real_,
        fe_pv   = if (!is.null(iv_fit)) {
          2 * pnorm(-abs(coef(iv_fit)["fit_got_ll"] / sqrt(vcov(iv_fit)["fit_got_ll", "fit_got_ll"])))
        } else NA_real_,
        n_obs = sum(ok),
        n_treated = sum(ok & d == 1),
        n_control = sum(ok & d == 0)
      )
    }
  }
  bind_rows(results)
}

dyn_nongs_atp <- run_dynamic_nongs(nongs_iv_atp, "ATP non-GS", nongs_zpre)
dyn_nongs_wta <- run_dynamic_nongs(nongs_iv_wta, "WTA non-GS", nongs_zpre)

saveRDS(bind_rows(dyn_nongs_atp, dyn_nongs_wta),
        file.path(CLEANED_DIR, "skeleton_dynamic_nongs.rds"))

# Reuse build_dynamic_tex for non-GS (same table format)
build_dynamic_tex(dyn_nongs_atp, "ATP non-GS", "table_dynamic_nongs.tex")
build_dynamic_tex(dyn_nongs_wta, "WTA non-GS", "table_dynamic_nongs_wta.tex")

# --- Heterogeneity non-GS (mirrors table_hetero_atp/wta.tex) -----------------
# Use OLS with event FE for heterogeneity (IV per subgroup is fragile)
run_hetero_nongs <- function(data, tour_label) {
  med_pts <- median(data$pre_rank_pts, na.rm = TRUE)
  h_rank_hi <- run_hetero_sub(data |> filter(pre_rank_pts >= med_pts), "High ranking pts")
  h_rank_lo <- run_hetero_sub(data |> filter(pre_rank_pts < med_pts), "Low ranking pts")

  med_age <- median(data$player_age, na.rm = TRUE)
  h_age_old <- run_hetero_sub(data |> filter(player_age >= med_age), "Older")
  h_age_young <- run_hetero_sub(data |> filter(player_age < med_age), "Younger")

  h_prior_yes <- run_hetero_sub(data |> filter(had_prior_ll == 1), "Had prior LL")
  h_prior_no  <- run_hetero_sub(data |> filter(had_prior_ll == 0), "No prior LL")

  bind_rows(h_rank_hi, h_rank_lo, h_age_old, h_age_young,
            h_prior_yes, h_prior_no) |>
    mutate(tour = tour_label)
}

hetero_nongs_atp <- run_hetero_nongs(nongs_atp_est, "ATP non-GS")
hetero_nongs_wta <- run_hetero_nongs(nongs_wta_est, "WTA non-GS")

saveRDS(bind_rows(hetero_nongs_atp, hetero_nongs_wta),
        file.path(CLEANED_DIR, "skeleton_heterogeneity_nongs.rds"))

build_hetero_tex(hetero_nongs_atp, "ATP non-GS", "table_hetero_nongs.tex")
build_hetero_tex(hetero_nongs_wta, "WTA non-GS", "table_hetero_nongs_wta.tex")

# --- First-LL-Only non-GS (mirrors table_firstll_atp/wta.tex) ----------------
first_ll_nongs <- nongs_est |>
  arrange(player_id, event_date) |>
  group_by(player_id) |>
  slice_head(n = 1) |>
  ungroup()

first_ll_nongs_atp <- first_ll_nongs |> filter(tour == "ATP")
first_ll_nongs_wta <- first_ll_nongs |> filter(tour == "WTA")

message("  First-LL non-GS ATP: N=", nrow(first_ll_nongs_atp),
        " (LL=", sum(first_ll_nongs_atp$got_ll), ")")
message("  First-LL non-GS WTA: N=", nrow(first_ll_nongs_wta),
        " (LL=", sum(first_ll_nongs_wta$got_ll), ")")

firstll_imm_nongs_atp <- run_immediate(first_ll_nongs_atp, "ATP first-LL non-GS")
firstll_imm_nongs_wta <- run_immediate(first_ll_nongs_wta, "WTA first-LL non-GS")
firstll_dyn_nongs_atp <- run_dynamic(first_ll_nongs_atp, "ATP first-LL non-GS")
firstll_dyn_nongs_wta <- run_dynamic(first_ll_nongs_wta, "WTA first-LL non-GS")

saveRDS(list(imm = bind_rows(firstll_imm_nongs_atp, firstll_imm_nongs_wta),
             dyn = bind_rows(firstll_dyn_nongs_atp, firstll_dyn_nongs_wta)),
        file.path(CLEANED_DIR, "skeleton_firstll_nongs_results.rds"))

build_firstll_tex(firstll_imm_nongs_atp, firstll_dyn_nongs_atp, "ATP", "table_firstll_nongs.tex")
build_firstll_tex(firstll_imm_nongs_wta, firstll_dyn_nongs_wta, "WTA", "table_firstll_nongs_wta.tex")

slog("## Step 14b: Non-GS Mirror Tables")
slog("- ATP non-GS immediate: ", nrow(imm_nongs_atp), " outcomes")
slog("- WTA non-GS immediate: ", nrow(imm_nongs_wta), " outcomes")
slog("- ATP non-GS dynamic: ", nrow(dyn_nongs_atp), " outcomes")
slog("- WTA non-GS dynamic: ", nrow(dyn_nongs_wta), " outcomes")
slog("- ATP non-GS heterogeneity: ", nrow(hetero_nongs_atp), " subgroup-outcomes")
slog("- WTA non-GS heterogeneity: ", nrow(hetero_nongs_wta), " subgroup-outcomes")
slog("- ATP first-LL non-GS: N = ", nrow(first_ll_nongs_atp))
slog("- WTA first-LL non-GS: N = ", nrow(first_ll_nongs_wta))
slog("")


# ==============================================================================
# STEP 15: SAMPLE COUNTS DOCUMENT
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 15: SAMPLE COUNTS DOCUMENT")
message(strrep("=", 70))

counts_lines <- c(
  "# Final Sample Counts (Skeleton Pipeline)",
  paste0("Generated: ", Sys.time()),
  "",
  "## GS Estimation Sample (ALL final-round qualifying losers, 2006+, events with >=1 LL)",
  "",
  "### ATP GS",
  paste0("- Total: ", nrow(gs_atp)),
  paste0("- LL (treated): ", sum(gs_atp$got_ll)),
  paste0("- Control: ", sum(gs_atp$got_ll == 0)),
  paste0("- Unique players: ", n_distinct(gs_atp$player_id)),
  paste0("- Unique events: ", n_distinct(gs_atp$tourney_id)),
  paste0("- Year range: ", min(gs_atp$year), "-", max(gs_atp$year)),
  "",
  "### WTA GS",
  paste0("- Total: ", nrow(gs_wta)),
  paste0("- LL (treated): ", sum(gs_wta$got_ll)),
  paste0("- Control: ", sum(gs_wta$got_ll == 0)),
  paste0("- Unique players: ", n_distinct(gs_wta$player_id)),
  paste0("- Unique events: ", n_distinct(gs_wta$tourney_id)),
  paste0("- Year range: ", min(gs_wta$year), "-", max(gs_wta$year)),
  "",
  "### First-LL-Only ATP GS",
  paste0("- Total: ", nrow(first_ll_atp)),
  paste0("- LL: ", sum(first_ll_atp$got_ll)),
  paste0("- Control: ", sum(first_ll_atp$got_ll == 0)),
  "",
  "### First-LL-Only WTA GS",
  paste0("- Total: ", nrow(first_ll_wta)),
  paste0("- LL: ", sum(first_ll_wta$got_ll)),
  paste0("- Control: ", sum(first_ll_wta$got_ll == 0)),
  "",
  "### Verified Lottery ATP GS",
  paste0("- Events: ", length(verified_events)),
  paste0("- ATP obs: ", nrow(verified_atp)),
  paste0("- ATP LL: ", sum(verified_atp$got_ll)),
  "",
  "### Verified Lottery WTA GS",
  paste0("- WTA obs: ", nrow(verified_wta)),
  paste0("- WTA LL: ", sum(verified_wta$got_ll)),
  "",
  "## Non-GS Estimation Sample (events with >=1 LL)",
  "",
  "### ATP Non-GS",
  paste0("- Total: ", sum(nongs_est$tour == "ATP")),
  paste0("- LL: ", sum(nongs_est$tour == "ATP" & nongs_est$got_ll == 1)),
  paste0("- Control: ", sum(nongs_est$tour == "ATP" & nongs_est$got_ll == 0)),
  paste0("- IV sample (with peer_component): ", nrow(nongs_iv_atp)),
  "",
  "### WTA Non-GS",
  paste0("- Total: ", sum(nongs_est$tour == "WTA")),
  paste0("- LL: ", sum(nongs_est$tour == "WTA" & nongs_est$got_ll == 1)),
  paste0("- Control: ", sum(nongs_est$tour == "WTA" & nongs_est$got_ll == 0)),
  paste0("- IV sample (with peer_component): ", nrow(nongs_iv_wta)),
  "",
  "## LL Distribution Table Cross-Check",
  paste0("- GS dist table ATP total: ",
         sum(gs_est$tour == "ATP"),
         " == estimation ATP N: ", nrow(gs_atp),
         " -> ", if (sum(gs_est$tour == "ATP") == nrow(gs_atp)) "MATCH" else "MISMATCH"),
  paste0("- GS dist table WTA total: ",
         sum(gs_est$tour == "WTA"),
         " == estimation WTA N: ", nrow(gs_wta),
         " -> ", if (sum(gs_est$tour == "WTA") == nrow(gs_wta)) "MATCH" else "MISMATCH"),
  "",
  "## KEY CHANGE FROM PREVIOUS PIPELINE",
  "The previous scripts restricted the GS sample to 'top-4 ranked losers'",
  "which produced N=248 (ATP) / N=132 (WTA).",
  "This pipeline uses ALL final-round qualifying losers per the skeleton spec,",
  paste0("producing N=", nrow(gs_atp), " (ATP) / N=", nrow(gs_wta), " (WTA).")
)

writeLines(counts_lines, file.path(OUTPUT_DIR, "final_sample_counts.md"))
message("  Saved: final_sample_counts.md")


# ==============================================================================
# STEP 16: VERIFY CONSISTENCY
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 16: VERIFY CONSISTENCY")
message(strrep("=", 70))

# Check 1: LL distribution table totals match estimation sample N
check1_atp <- sum(gs_est$tour == "ATP") == nrow(gs_atp)
check1_wta <- sum(gs_est$tour == "WTA") == nrow(gs_wta)
message("  GS dist table ATP matches est sample: ", check1_atp)
message("  GS dist table WTA matches est sample: ", check1_wta)

# Check 2: Summary stats N matches estimation N
check2_atp <- (ss_atp$n_ll[1] + ss_atp$n_ct[1]) == nrow(gs_atp)
check2_wta <- (ss_wta$n_ll[1] + ss_wta$n_ct[1]) == nrow(gs_wta)
message("  Sumstats ATP N matches: ", check2_atp)
message("  Sumstats WTA N matches: ", check2_wta)

# Check 3: Results table N <= estimation N
if (nrow(immediate_all) > 0) {
  check3 <- all(immediate_all$n_obs <= nrow(gs_est))
  message("  Results N <= estimation N: ", check3)
}

slog("## Step 16: Consistency Checks")
slog("- GS dist ATP = est N: ", check1_atp)
slog("- GS dist WTA = est N: ", check1_wta)
slog("- Sumstats ATP = est N: ", check2_atp, " (", ss_atp$n_ll[1] + ss_atp$n_ct[1], " vs ", nrow(gs_atp), ")")
slog("- Sumstats WTA = est N: ", check2_wta, " (", ss_wta$n_ll[1] + ss_wta$n_ct[1], " vs ", nrow(gs_wta), ")")
slog("")


# ==============================================================================
# WRITE SUMMARY
# ==============================================================================
summary_header <- c(
  "# Skeleton Pipeline Summary",
  paste0("Generated: ", Sys.time()),
  ""
)

writeLines(c(summary_header, summary_log),
           file.path(OUTPUT_DIR, "skeleton_pipeline_summary.md"))
message("\n  Saved: skeleton_pipeline_summary.md")

message("\n", strrep("=", 70))
message("PIPELINE COMPLETE")
message(strrep("=", 70))
message("  Tables: ", TABLES_DIR)
message("  Figures: ", FIGURES_DIR)
message("  Data: ", CLEANED_DIR)
message("  Summary: ", file.path(OUTPUT_DIR, "skeleton_pipeline_summary.md"))
message("  Counts: ", file.path(OUTPUT_DIR, "final_sample_counts.md"))
