# ==============================================================================
# 24_tournament_match_fix.R
# Four fixes for the tournament performance model:
#   FIX 1: Include ALL matches (main + qual + challenger), not just main draw
#   FIX 2: Add all covariate coefficients to tables
#   FIX 3: Regenerate 4-panel figure with corrected data
#   FIX 4: Points-weighted robustness with corrected data
#
# Inputs:
#   Data/cleaned/tournament_rebuild_results.rds (from script 22)
#   Data/raw/atp_main_matches.rds, atp_qual_chall_matches.rds
#   Data/raw/wta_main_matches.rds, wta_qual_itf_matches.rds
#   Data/cleaned/tournament_elo_cache.rds
#
# Outputs:
#   Tables/table_tournament_gs_atp.tex     (rewritten: all covariates)
#   Tables/table_tournament_gs_wta.tex     (rewritten: all covariates)
#   Tables/table_tournament_nongs_atp.tex  (rewritten: all covariates)
#   Tables/table_tournament_nongs_wta.tex  (rewritten: all covariates)
#   Figures/fig_tournament_4panel.pdf      (regenerated with corrected data)
#   Data/cleaned/tournament_match_fix_results.rds
#   Output/tournament_match_fix_summary.md
#
# Dependencies: dplyr, tidyr, readr, stringr, ggplot2, data.table, patchwork,
#               sandwich, here
# ==============================================================================

set.seed(20260326)

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(data.table)
library(patchwork)
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

# -- Points schedules (from script 22) ----------------------------------------
POINTS_SCHEDULE <- list(
  G       = c(R128 = 10, R64 = 45, R32 = 90, R16 = 180, QF = 360, SF = 720, F = 1200, W = 2000),
  M       = c(R64 = 10, R32 = 25, R16 = 45, QF = 90, SF = 180, F = 360, W = 600),
  PM      = c(R64 = 10, R32 = 25, R16 = 45, QF = 90, SF = 180, F = 360, W = 600),
  A500    = c(R32 = 0, R16 = 20, QF = 45, SF = 90, F = 180, W = 300),
  P       = c(R32 = 0, R16 = 20, QF = 45, SF = 90, F = 180, W = 300),
  A250    = c(R32 = 0, R16 = 20, QF = 45, SF = 90, F = 150, W = 250),
  I       = c(R32 = 0, R16 = 20, QF = 45, SF = 90, F = 150, W = 250)
)

summary_log <- character()

cat("\n")
message(strrep("=", 72))
message("  TOURNAMENT MATCH FIX (24_tournament_match_fix.R)")
message(strrep("=", 72))


###############################################################################
# PHASE 1: LOAD DATA
###############################################################################

message("\n[1] Loading tournament rebuild results and raw match data...")

R <- readRDS(file.path(CLEANED_DIR, "tournament_rebuild_results.rds"))

# Extract event tables (these are correct; the bug is only in match building)
gs_atp_ev    <- R$gs_atp$events
gs_wta_ev    <- R$gs_wta$events
nongs_atp_ev <- R$nongs_atp$events
nongs_wta_ev <- R$nongs_wta$events

# Also extract win models (needed for Bernoulli convolution, already computed)
atp_win_model <- R$win_models$atp
wta_win_model <- R$win_models$wta

# Load Elo cache
elo_cache_path <- here("Data", "cleaned", "tournament_elo_cache.rds")
if (!file.exists(elo_cache_path)) {
  stop("Elo cache not found at ", elo_cache_path)
}
elo_list <- readRDS(elo_cache_path)
message("  Loaded Elo cache: ", length(ls(elo_list$overall)), " players")

get_elo_at_date <- function(env, player, date) {
  df <- env[[player]]
  if (is.null(df)) return(1500)
  v <- df[df$date <= date, ]
  if (nrow(v) == 0) return(1500)
  tail(v$rating, 1)
}

# Load raw match data and re-harmonize (to get ALL match sources)
atp_main <- read_rds(file.path(RAW_DIR, "atp_main_matches.rds"))
atp_qual <- read_rds(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
wta_main <- read_rds(file.path(RAW_DIR, "wta_main_matches.rds"))
wta_qual <- read_rds(file.path(RAW_DIR, "wta_qual_itf_matches.rds"))

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
message("    main: ", sum(matches$match_source == "main"),
        ", qual/chall: ", sum(matches$match_source == "qual"))

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

slog("## Phase 1: Data Loaded")
slog("- Total combined matches: ", nrow(matches))
slog("- Main draw: ", sum(matches$match_source == "main"))
slog("- Qual/Challenger: ", sum(matches$match_source == "qual"))
slog("")

# Log OLD match counts from script 22 for comparison
old_gs_atp_n   <- nrow(R$gs_atp$matches)
old_gs_wta_n   <- nrow(R$gs_wta$matches)
old_nongs_atp_n <- nrow(R$nongs_atp$matches)
old_nongs_wta_n <- nrow(R$nongs_wta$matches)
old_gs_atp_players   <- n_distinct(R$gs_atp$matches$focal_pid)
old_gs_wta_players   <- n_distinct(R$gs_wta$matches$focal_pid)
old_nongs_atp_players <- n_distinct(R$nongs_atp$matches$focal_pid)
old_nongs_wta_players <- n_distinct(R$nongs_wta$matches$focal_pid)


###############################################################################
# PHASE 2: FIX 1 -- REBUILD MATCH-LEVEL DATA WITHOUT match_source FILTER
###############################################################################

message("\n[2] FIX 1: Rebuilding match-level data (ALL match sources)...")

build_match_level_fixed <- function(ev_table, all_matches, tour_label,
                                     calendar_cap_days = CALENDAR_CAP) {
  # FIX 1: Remove the match_source == "main" filter.
  # Include ALL matches (main draw, qualifying, challenger) played by LL
  # candidates in the observation window.
  md <- all_matches |>
    filter(tour == tour_label) |>
    mutate(
      tourney_date = if (inherits(tourney_date, "Date")) tourney_date
                     else as.Date(as.character(tourney_date), format = "%Y%m%d")
    )

  message("    Available matches (all sources) for ", tour_label, ": ", nrow(md))

  # Item 5: Truncation at next LL opportunity
  ev_sorted <- ev_table |> arrange(player_id, tourney_date)
  ev_sorted <- ev_sorted |>
    group_by(player_id) |>
    mutate(next_ll_opp_date = lead(tourney_date, default = as.Date("2099-12-31"))) |>
    ungroup()

  ev_sorted$cal_cap_date <- ev_sorted$tourney_date + calendar_cap_days

  # Build focal matches: player-centric view
  fm <- bind_rows(
    md |> transmute(
      focal_pid = winner_pid, opp_pid = loser_pid,
      tourney_id, tourney_date, match_num, surface, tourney_level,
      best_of, focal_age = winner_age, opp_age = loser_age,
      focal_rank = winner_rank, opp_rank = loser_rank,
      focal_ioc = winner_ioc, opp_ioc = loser_ioc,
      focal_ht = winner_ht, opp_ht = loser_ht,
      focal_hand = winner_hand, opp_hand = loser_hand,
      round, won = 1L, match_source),
    md |> transmute(
      focal_pid = loser_pid, opp_pid = winner_pid,
      tourney_id, tourney_date, match_num, surface, tourney_level,
      best_of, focal_age = loser_age, opp_age = winner_age,
      focal_rank = loser_rank, opp_rank = winner_rank,
      focal_ioc = loser_ioc, opp_ioc = winner_ioc,
      focal_ht = loser_ht, opp_ht = winner_ht,
      focal_hand = loser_hand, opp_hand = winner_hand,
      round, won = 0L, match_source)
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

  # Add match-level features
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
          " across ", n_distinct(match_df$event_id), " events",
          ", ", n_distinct(match_df$focal_pid), " unique players")

  match_df
}

message("  Building GS-ATP match data (ALL sources)...")
gs_atp_md <- build_match_level_fixed(gs_atp_ev, matches, "ATP")
message("  Building GS-WTA match data (ALL sources)...")
gs_wta_md <- build_match_level_fixed(gs_wta_ev, matches, "WTA")
message("  Building nonGS-ATP match data (ALL sources)...")
nongs_atp_md <- build_match_level_fixed(nongs_atp_ev, matches, "ATP")
message("  Building nonGS-WTA match data (ALL sources)...")
nongs_wta_md <- build_match_level_fixed(nongs_wta_ev, matches, "WTA")

slog("## FIX 1: Match-Level Datasets (ALL match sources)")
slog("- GS-ATP: ", nrow(gs_atp_md), " matches (was ", old_gs_atp_n, "), ",
     n_distinct(gs_atp_md$event_id), " events, ",
     n_distinct(gs_atp_md$focal_pid), " players (was ", old_gs_atp_players, ")")
slog("- GS-WTA: ", nrow(gs_wta_md), " matches (was ", old_gs_wta_n, "), ",
     n_distinct(gs_wta_md$event_id), " events, ",
     n_distinct(gs_wta_md$focal_pid), " players (was ", old_gs_wta_players, ")")
slog("- nonGS-ATP: ", nrow(nongs_atp_md), " matches (was ", old_nongs_atp_n, "), ",
     n_distinct(nongs_atp_md$event_id), " events, ",
     n_distinct(nongs_atp_md$focal_pid), " players (was ", old_nongs_atp_players, ")")
slog("- nonGS-WTA: ", nrow(nongs_wta_md), " matches (was ", old_nongs_wta_n, "), ",
     n_distinct(nongs_wta_md$event_id), " events, ",
     n_distinct(nongs_wta_md$focal_pid), " players (was ", old_nongs_wta_players, ")")
slog("")


###############################################################################
# PHASE 3: RE-ESTIMATE ALL MODELS WITH CORRECTED DATA
###############################################################################

message("\n[3] Re-estimating all models with corrected match data...")

# -- Helper: impute NAs -------------------------------------------------------
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

# -- Helper: get tournament tier -----------------------------------------------
get_tier <- function(tourney_level, draw_size = NA) {
  if (tourney_level == "G") return("G")
  if (tourney_level %in% c("M", "PM")) return("M")
  if (tourney_level == "P") return("P")
  if (tourney_level == "I") return("I")
  if (tourney_level == "A") {
    if (!is.na(draw_size) && draw_size >= 48) return("A500")
    return("A250")
  }
  "A250"
}

# -- Helper: compute E[RP] for a tournament trajectory -------------------------
compute_erp <- function(probs, tier) {
  sched <- POINTS_SCHEDULE[[tier]]
  if (is.null(sched)) sched <- POINTS_SCHEDULE[["A250"]]
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

# ==============================================================================
# GS MODELS: Simple logit (no v_hat), clustered SEs
# ==============================================================================

message("\n  --- GS Models (simple logit, lottery assignment) ---")

estimate_gs_model <- function(match_df, label, weighted = FALSE) {
  if (is.null(match_df) || nrow(match_df) < 30) {
    message("  ", label, ": insufficient observations (", nrow(match_df), ")")
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
    df$match_weight <- case_when(
      grepl("^F$", df$round)   ~ 10,
      grepl("^SF$", df$round)  ~ 5,
      grepl("^QF$", df$round)  ~ 3,
      grepl("^R16$", df$round) ~ 2,
      TRUE ~ 1
    )
    model <- glm(fml, family = binomial(link = "logit"), data = df, weights = match_weight)
  } else {
    model <- glm(fml, family = binomial(link = "logit"), data = df)
  }

  # Clustered SEs at player (focal_pid) level
  V <- vcovCL(model, cluster = df$focal_pid, type = "HC1")
  se_cl <- sqrt(diag(V))

  s <- summary(model)$coefficients
  delta   <- s["got_ll", "Estimate"]
  delta_se <- se_cl["got_ll"]
  delta_z  <- delta / delta_se
  delta_p  <- 2 * pnorm(-abs(delta_z))

  message(sprintf("  %s: delta=%.4f (clust SE=%.4f, p=%.4f), N=%d, players=%d",
                  label, delta, delta_se, delta_p, nrow(df),
                  n_distinct(df$focal_pid)))

  list(model = model, vcov_cl = V, data = df,
       delta = delta, delta_se = delta_se, delta_p = delta_p)
}

gs_atp_fit  <- estimate_gs_model(gs_atp_md, "GS-ATP")
gs_atp_wt   <- estimate_gs_model(gs_atp_md, "GS-ATP (weighted)", weighted = TRUE)
gs_wta_fit  <- estimate_gs_model(gs_wta_md, "GS-WTA")
gs_wta_wt   <- estimate_gs_model(gs_wta_md, "GS-WTA (weighted)", weighted = TRUE)

# ==============================================================================
# NON-GS MODELS: CF logit (with v_hat), bootstrap SEs
# ==============================================================================

message("\n  --- Non-GS Models (CF logit with v_hat) ---")

estimate_nongs_model <- function(match_df, label, weighted = FALSE) {
  if (is.null(match_df) || nrow(match_df) < 50) {
    message("  ", label, ": insufficient observations (", nrow(match_df), ")")
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
    df$match_weight <- case_when(
      grepl("^F$", df$round)   ~ 10,
      grepl("^SF$", df$round)  ~ 5,
      grepl("^QF$", df$round)  ~ 3,
      grepl("^R16$", df$round) ~ 2,
      TRUE ~ 1
    )
    model <- glm(fml, family = binomial(link = "logit"), data = df, weights = match_weight)
  } else {
    model <- glm(fml, family = binomial(link = "logit"), data = df)
  }

  s <- summary(model)$coefficients
  delta    <- s["got_ll", "Estimate"]
  delta_se <- s["got_ll", "Std. Error"]
  delta_p  <- s["got_ll", "Pr(>|z|)"]
  rho      <- s["v_hat", "Estimate"]
  rho_se   <- s["v_hat", "Std. Error"]
  rho_p    <- s["v_hat", "Pr(>|z|)"]

  message(sprintf("  %s: delta=%.4f (SE=%.4f, p=%.4f), rho=%.4f (p=%.4f), N=%d, players=%d",
                  label, delta, delta_se, delta_p, rho, rho_p, nrow(df),
                  n_distinct(df$focal_pid)))

  list(model = model, data = df, delta = delta, delta_se = delta_se,
       delta_p = delta_p, rho = rho, rho_se = rho_se, rho_p = rho_p)
}

nongs_atp_fit <- estimate_nongs_model(nongs_atp_md, "nonGS-ATP")
nongs_atp_wt  <- estimate_nongs_model(nongs_atp_md, "nonGS-ATP (weighted)", weighted = TRUE)
nongs_wta_fit <- estimate_nongs_model(nongs_wta_md, "nonGS-WTA")
nongs_wta_wt  <- estimate_nongs_model(nongs_wta_md, "nonGS-WTA (weighted)", weighted = TRUE)


###############################################################################
# PHASE 4: COUNTERFACTUALS
###############################################################################

message("\n[4] Computing counterfactual quantities...")

# -- GS counterfactuals (delta-method SEs from clustered vcov) -----------------
compute_gs_counterfactuals <- function(fit_obj, ev_table, label) {
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

  # 1. DeltaP(match win): average marginal effect
  avg_me <- mean(df$marginal_effect)
  N <- nrow(df)
  d_ame_d_delta <- mean(df$p1 * (1 - df$p1))
  var_delta <- V["got_ll", "got_ll"]
  se_me <- abs(d_ame_d_delta) * sqrt(var_delta)
  p_me  <- 2 * pnorm(-abs(avg_me / se_me))

  # 2. DeltaE[W]: expected additional wins per tournament
  df_sorted <- df |> arrange(event_id, tourney_id, round_num)

  tourney_effects <- df_sorted |>
    group_by(event_id, tourney_id) |>
    summarise(
      n_rounds = n(),
      ew_d0 = sum(cumprod(p0)),
      ew_d1 = sum(cumprod(p1)),
      delta_ew = sum(cumprod(p1)) - sum(cumprod(p0)),
      .groups = "drop"
    )
  avg_delta_ew <- mean(tourney_effects$delta_ew)

  # Numerical derivative for DeltaE[W]
  eps <- 1e-5
  df$p1_plus  <- plogis(linpred_d0 + delta + eps)
  df$p1_minus <- plogis(linpred_d0 + delta - eps)
  df_s2 <- df |> arrange(event_id, tourney_id, round_num)

  te_plus <- df_s2 |>
    group_by(event_id, tourney_id) |>
    summarise(ew_d1 = sum(cumprod(p1_plus)), ew_d0 = sum(cumprod(p0)), .groups = "drop") |>
    mutate(delta_ew = ew_d1 - ew_d0)
  te_minus <- df_s2 |>
    group_by(event_id, tourney_id) |>
    summarise(ew_d1 = sum(cumprod(p1_minus)), ew_d0 = sum(cumprod(p0)), .groups = "drop") |>
    mutate(delta_ew = ew_d1 - ew_d0)

  d_dew_d_delta <- (mean(te_plus$delta_ew) - mean(te_minus$delta_ew)) / (2 * eps)
  se_dew <- abs(d_dew_d_delta) * sqrt(var_delta)
  p_dew  <- 2 * pnorm(-abs(avg_delta_ew / se_dew))

  # 3. DeltaE[RP]: expected additional ranking points per tournament
  tourney_effects$erp_d0 <- NA_real_
  tourney_effects$erp_d1 <- NA_real_
  tourney_effects$erp_d1_plus  <- NA_real_
  tourney_effects$erp_d1_minus <- NA_real_

  for (k in seq_len(nrow(tourney_effects))) {
    eid <- tourney_effects$event_id[k]
    tid <- tourney_effects$tourney_id[k]
    sub <- df_s2 |> filter(event_id == eid, tourney_id == tid) |> arrange(round_num)
    tier <- get_tier(sub$tourney_level[1])
    tourney_effects$erp_d0[k]       <- compute_erp(sub$p0, tier)
    tourney_effects$erp_d1[k]       <- compute_erp(sub$p1, tier)
    tourney_effects$erp_d1_plus[k]  <- compute_erp(sub$p1_plus, tier)
    tourney_effects$erp_d1_minus[k] <- compute_erp(sub$p1_minus, tier)
  }
  tourney_effects$delta_erp       <- tourney_effects$erp_d1 - tourney_effects$erp_d0
  tourney_effects$delta_erp_plus  <- tourney_effects$erp_d1_plus - tourney_effects$erp_d0
  tourney_effects$delta_erp_minus <- tourney_effects$erp_d1_minus - tourney_effects$erp_d0

  avg_delta_erp <- mean(tourney_effects$delta_erp)
  d_derp_d_delta <- (mean(tourney_effects$delta_erp_plus) - mean(tourney_effects$delta_erp_minus)) / (2 * eps)
  se_derp <- abs(d_derp_d_delta) * sqrt(var_delta)
  p_derp  <- 2 * pnorm(-abs(avg_delta_erp / se_derp))

  message(sprintf("  %s: DeltaP=%.4f (SE=%.4f), DeltaE[W]=%.4f (SE=%.4f), DeltaE[RP]=%.1f (SE=%.1f)",
                  label, avg_me, se_me, avg_delta_ew, se_dew, avg_delta_erp, se_derp))

  list(
    tourney_effects = tourney_effects,
    match_cf = df,
    avg_me = avg_me, se_me = se_me, p_me = p_me,
    avg_delta_ew = avg_delta_ew, se_dew = se_dew, p_dew = p_dew,
    avg_delta_erp = avg_delta_erp, se_derp = se_derp, p_derp = p_derp
  )
}

gs_atp_cf <- compute_gs_counterfactuals(gs_atp_fit, gs_atp_ev, "GS-ATP")
gs_wta_cf <- compute_gs_counterfactuals(gs_wta_fit, gs_wta_ev, "GS-WTA")

# -- Non-GS counterfactuals (point estimates, SEs from bootstrap later) --------
compute_nongs_counterfactuals <- function(fit_obj, label) {
  if (is.null(fit_obj)) return(NULL)

  model <- fit_obj$model
  df <- fit_obj$data
  beta <- coef(model)
  delta <- beta["got_ll"]
  rho <- beta["v_hat"]

  linpred_obs <- predict(model, newdata = df, type = "link")
  linpred_d0 <- linpred_obs - delta * df$got_ll
  linpred_d1 <- linpred_d0 + delta

  df$p0 <- plogis(linpred_d0)
  df$p1 <- plogis(linpred_d1)
  df$marginal_effect <- df$p1 - df$p0

  avg_me <- mean(df$marginal_effect, na.rm = TRUE)

  df_dt <- as.data.table(df)
  setorder(df_dt, event_id, tourney_id, round_num)

  tourney_effects <- df_dt[, {
    R <- .N
    cp0 <- cumprod(p0)
    cp1 <- cumprod(p1)
    list(
      n_rounds = R,
      ew_d0 = sum(cp0),
      ew_d1 = sum(cp1),
      delta_ew = sum(cp1) - sum(cp0),
      tourney_level = tourney_level[1]
    )
  }, by = .(event_id, tourney_id)]

  avg_delta_ew <- mean(tourney_effects$delta_ew, na.rm = TRUE)

  # Compute E[RP] per tournament
  tourney_effects$erp_d0 <- NA_real_
  tourney_effects$erp_d1 <- NA_real_

  for (k in seq_len(nrow(tourney_effects))) {
    eid <- tourney_effects$event_id[k]
    tid <- tourney_effects$tourney_id[k]
    sub <- df_dt[event_id == eid & tourney_id == tid]
    setorder(sub, round_num)
    tl <- tourney_effects$tourney_level[k]
    if (is.na(tl)) tl <- sub$tourney_level[1]
    if (is.na(tl)) tl <- "A250"
    tier <- get_tier(tl)
    tourney_effects$erp_d0[k] <- compute_erp(sub$p0, tier)
    tourney_effects$erp_d1[k] <- compute_erp(sub$p1, tier)
  }
  tourney_effects$delta_erp <- tourney_effects$erp_d1 - tourney_effects$erp_d0
  avg_delta_erp <- mean(tourney_effects$delta_erp, na.rm = TRUE)

  message(sprintf("  %s: DeltaP=%.4f, DeltaE[W]=%.4f, DeltaE[RP]=%.1f",
                  label, avg_me, avg_delta_ew, avg_delta_erp))

  list(
    tourney_effects = as.data.frame(tourney_effects),
    match_cf = df,
    avg_me = avg_me,
    avg_delta_ew = avg_delta_ew,
    avg_delta_erp = avg_delta_erp
  )
}

nongs_atp_cf <- compute_nongs_counterfactuals(nongs_atp_fit, "nonGS-ATP")
nongs_wta_cf <- compute_nongs_counterfactuals(nongs_wta_fit, "nonGS-WTA")


###############################################################################
# PHASE 5: BOOTSTRAP FOR NON-GS (200 reps)
###############################################################################

message("\n[5] Bootstrap inference for non-GS models (", N_BOOT, " reps)...")

bootstrap_nongs <- function(ev_table, match_df, n_boot = N_BOOT, label = "") {
  if (is.null(match_df) || nrow(match_df) < 50) {
    message("  ", label, ": skipping bootstrap (insufficient data)")
    return(NULL)
  }

  players <- unique(ev_table$player_id)
  n_players <- length(players)
  ev_by_player <- split(ev_table, ev_table$player_id)

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
      boot_ev <- bind_rows(lapply(boot_players, function(p) ev_by_player[[as.character(p)]]))
      if (nrow(boot_ev) < 20) next

      boot_md <- match_df |> filter(event_id %in% boot_ev$event_id)
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

      boot_md <- impute_match_data(boot_md)

      fit <- glm(won ~ got_ll + v_hat +
                   log_rank_ratio + log_rank_ratio_sq + rank_diff +
                   same_ioc + is_clay + is_grass + age_diff + height_diff +
                   hand_mismatch + h2h_smoothed + n_h2h +
                   pre_elo + opp_elo + pre_rank_pts +
                   player_age_at_event + had_prior_ll,
                 family = binomial(link = "logit"), data = boot_md)

      if (!fit$converged) next

      beta_b <- coef(fit)
      delta_b <- unname(beta_b["got_ll"])
      rho_b <- unname(beta_b["v_hat"])

      lp_obs <- predict(fit, newdata = boot_md, type = "link")
      lp_d0 <- lp_obs - delta_b * boot_md$got_ll
      lp_d1 <- lp_d0 + delta_b
      p0 <- plogis(lp_d0)
      p1 <- plogis(lp_d1)
      avg_me_b <- mean(p1 - p0)

      boot_md$p0 <- p0
      boot_md$p1 <- p1
      te <- boot_md |>
        arrange(event_id, tourney_id, round_num) |>
        group_by(event_id, tourney_id) |>
        summarise(delta_ew = sum(cumprod(p1)) - sum(cumprod(p0)), .groups = "drop")
      avg_dew_b <- mean(te$delta_ew)

      boot_results[[b]] <- c(delta = delta_b, rho = rho_b,
                              avg_me = avg_me_b, avg_dew = avg_dew_b)
    }, error = function(e) NULL)
  }

  boot_results <- boot_results[!sapply(boot_results, is.null)]
  n_success <- length(boot_results)
  elapsed_total <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
  message(sprintf("  %s: %d/%d successful in %.1f min", label, n_success, n_boot, elapsed_total))

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

  list(boot_mat = boot_mat, boot_se = boot_se, boot_ci = boot_ci, n_success = n_success)
}

nongs_atp_boot <- bootstrap_nongs(nongs_atp_ev, nongs_atp_md, N_BOOT, "nonGS-ATP")
nongs_wta_boot <- bootstrap_nongs(nongs_wta_ev, nongs_wta_md, N_BOOT, "nonGS-WTA")

# Derive non-GS counterfactual inference from bootstrap
get_nongs_cf_inference <- function(cf_obj, boot_obj, label) {
  if (is.null(cf_obj) || is.null(boot_obj)) return(NULL)

  avg_me <- cf_obj$avg_me
  se_me <- boot_obj$boot_se["avg_me"]
  p_me <- 2 * pnorm(-abs(avg_me / se_me))

  avg_dew <- cf_obj$avg_delta_ew
  se_dew <- boot_obj$boot_se["avg_dew"]
  p_dew <- if (!is.na(avg_dew) && !is.na(se_dew) && se_dew > 0) {
    2 * pnorm(-abs(avg_dew / se_dew))
  } else NA_real_

  avg_derp <- cf_obj$avg_delta_erp
  if (!is.na(avg_dew) && abs(avg_dew) > 1e-8) {
    cv <- se_dew / abs(avg_dew)
    se_derp <- abs(avg_derp) * cv
  } else {
    se_derp <- NA_real_
  }
  p_derp <- if (!is.na(se_derp) && se_derp > 0) 2 * pnorm(-abs(avg_derp / se_derp)) else NA_real_

  message(sprintf("  %s: DeltaP=%.4f (SE=%.4f), DeltaE[W]=%.4f (SE=%.4f), DeltaE[RP]=%.1f",
                  label, avg_me, se_me, avg_dew, se_dew, avg_derp))

  list(avg_me = avg_me, se_me = se_me, p_me = p_me,
       avg_dew = avg_dew, se_dew = se_dew, p_dew = p_dew,
       avg_derp = avg_derp, se_derp = se_derp, p_derp = p_derp)
}

nongs_atp_cf_inf <- get_nongs_cf_inference(nongs_atp_cf, nongs_atp_boot, "nonGS-ATP")
nongs_wta_cf_inf <- get_nongs_cf_inference(nongs_wta_cf, nongs_wta_boot, "nonGS-WTA")

slog("## Phase 3-5: Re-estimation Results")
for (obj in list(
  list(f = gs_atp_fit, l = "GS-ATP"),
  list(f = gs_wta_fit, l = "GS-WTA")
)) {
  if (!is.null(obj$f)) {
    slog(sprintf("- %s: delta=%.4f (SE=%.4f, p=%.4f), N=%d",
                 obj$l, obj$f$delta, obj$f$delta_se, obj$f$delta_p, nrow(obj$f$data)))
  }
}
for (obj in list(
  list(f = nongs_atp_fit, b = nongs_atp_boot, l = "nonGS-ATP"),
  list(f = nongs_wta_fit, b = nongs_wta_boot, l = "nonGS-WTA")
)) {
  if (!is.null(obj$f)) {
    se_use <- if (!is.null(obj$b)) obj$b$boot_se["delta"] else obj$f$delta_se
    slog(sprintf("- %s: delta=%.4f (bootSE=%.4f), rho=%.4f, N=%d",
                 obj$l, obj$f$delta, se_use, obj$f$rho, nrow(obj$f$data)))
  }
}
slog("")


###############################################################################
# PHASE 6: FIX 2 -- TABLES WITH ALL COVARIATE COEFFICIENTS
###############################################################################

message("\n[6] FIX 2: Generating tables with ALL covariate coefficients...")

# Covariate display names and order
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
  "player_age_at_event" = "Player age",
  "had_prior_ll"        = "Had prior LL entry"
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
  "player_age_at_event" = "Player age",
  "had_prior_ll"        = "Had prior LL entry"
)


# ---- GS TABLE WRITER (full covariates, clustered SEs) -----------------------
write_gs_table_full <- function(fit_obj, fit_wt, cf_obj, ev_table, filepath, label) {
  if (is.null(fit_obj)) {
    message("  ", label, ": no fit, skipping")
    return()
  }

  model <- fit_obj$model
  V     <- fit_obj$vcov_cl
  beta  <- coef(model)
  se_cl <- sqrt(diag(V))

  n_matches <- nrow(fit_obj$data)
  n_events  <- n_distinct(fit_obj$data$event_id)
  n_players <- n_distinct(ev_table$player_id)

  lines <- c(
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & Estimate & SE \\\\",
    "\\midrule",
    "\\multicolumn{3}{l}{\\textit{Panel A: Logit Coefficients (Lottery Assignment)}} \\\\[3pt]"
  )

  # All covariates
  for (vname in names(COVAR_NAMES_GS)) {
    if (vname %in% names(beta)) {
      est <- beta[vname]
      se  <- se_cl[vname]
      z   <- est / se
      pv  <- 2 * pnorm(-abs(z))
      lines <- c(lines,
        sprintf("%s & %s%s & (%s) \\\\",
                COVAR_NAMES_GS[vname], fmt(est, 4), add_stars(pv), fmt(se, 4))
      )
    }
  }

  # Panel B: Counterfactual quantities
  lines <- c(lines,
    "\\midrule",
    "\\multicolumn{3}{l}{\\textit{Panel B: Counterfactual Quantities}} \\\\[3pt]"
  )

  if (!is.null(cf_obj)) {
    lines <- c(lines,
      sprintf("$\\Delta P(\\text{match win})$ & %s%s & (%s) \\\\",
              fmt(cf_obj$avg_me, 4), add_stars(cf_obj$p_me), fmt(cf_obj$se_me, 4)),
      sprintf("$\\Delta E[W]$ (wins/tournament) & %s%s & (%s) \\\\",
              fmt(cf_obj$avg_delta_ew, 4), add_stars(cf_obj$p_dew), fmt(cf_obj$se_dew, 4)),
      sprintf("$\\Delta E[RP]$ (ranking points) & %s%s & (%s) \\\\",
              fmt(cf_obj$avg_delta_erp, 1), add_stars(cf_obj$p_derp), fmt(cf_obj$se_derp, 1))
    )
  }

  # Panel C: Points-weighted robustness
  if (!is.null(fit_wt)) {
    delta_wt    <- fit_wt$delta
    delta_wt_se <- fit_wt$delta_se
    delta_wt_p  <- fit_wt$delta_p
    lines <- c(lines,
      "\\midrule",
      "\\multicolumn{3}{l}{\\textit{Panel C: Points-Weighted Logit}} \\\\[3pt]",
      sprintf("$\\hat{\\delta}$ (points-weighted) & %s%s & (%s) \\\\",
              fmt(delta_wt, 4), add_stars(delta_wt_p), fmt(delta_wt_se, 4))
    )
  }

  # Sample info
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


# ---- NON-GS TABLE WRITER (full covariates, bootstrap SEs) -------------------
write_nongs_table_full <- function(fit_obj, fit_wt, cf_inf, boot_obj,
                                    ev_table, filepath, label) {
  if (is.null(fit_obj)) {
    message("  ", label, ": no fit, skipping")
    return()
  }

  model <- fit_obj$model
  beta  <- coef(model)

  # Use bootstrap SEs where available
  if (!is.null(boot_obj)) {
    delta_se <- boot_obj$boot_se["delta"]
    rho_se   <- boot_obj$boot_se["rho"]
  } else {
    delta_se <- fit_obj$delta_se
    rho_se   <- fit_obj$rho_se
  }

  # For non-key covariates, use model SEs (bootstrap is for key params)
  s <- summary(model)$coefficients
  model_se <- s[, "Std. Error"]

  n_matches <- nrow(fit_obj$data)
  n_events  <- n_distinct(fit_obj$data$event_id)
  n_players <- n_distinct(ev_table$player_id)

  lines <- c(
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & Estimate & SE \\\\",
    "\\midrule",
    "\\multicolumn{3}{l}{\\textit{Panel A: Second-Stage CF Logit Coefficients}} \\\\[3pt]"
  )

  for (vname in names(COVAR_NAMES_NONGS)) {
    if (vname %in% names(beta)) {
      est <- beta[vname]
      # Use bootstrap SE for delta and rho, model SE for others
      if (vname == "got_ll") {
        se <- delta_se
      } else if (vname == "v_hat") {
        se <- rho_se
      } else {
        se <- model_se[vname]
      }
      z  <- est / se
      pv <- 2 * pnorm(-abs(z))
      lines <- c(lines,
        sprintf("%s & %s%s & (%s) \\\\",
                COVAR_NAMES_NONGS[vname], fmt(est, 4), add_stars(pv), fmt(se, 4))
      )
    }
  }

  # Panel B: Counterfactual quantities
  lines <- c(lines,
    "\\midrule",
    "\\multicolumn{3}{l}{\\textit{Panel B: Counterfactual Quantities}} \\\\[3pt]"
  )

  if (!is.null(cf_inf)) {
    lines <- c(lines,
      sprintf("$\\Delta P(\\text{match win})$ & %s%s & (%s) \\\\",
              fmt(cf_inf$avg_me, 4), add_stars(cf_inf$p_me), fmt(cf_inf$se_me, 4)),
      sprintf("$\\Delta E[W]$ (wins/tournament) & %s%s & (%s) \\\\",
              fmt(cf_inf$avg_dew, 4), add_stars(cf_inf$p_dew), fmt(cf_inf$se_dew, 4)),
      sprintf("$\\Delta E[RP]$ (ranking points) & %s%s & (%s) \\\\",
              fmt(cf_inf$avg_derp, 1), add_stars(cf_inf$p_derp),
              if (!is.na(cf_inf$se_derp)) fmt(cf_inf$se_derp, 1) else "---")
    )
  }

  # Panel C: Points-weighted
  if (!is.null(fit_wt)) {
    delta_wt    <- fit_wt$delta
    delta_wt_se <- fit_wt$delta_se
    delta_wt_p  <- fit_wt$delta_p
    lines <- c(lines,
      "\\midrule",
      "\\multicolumn{3}{l}{\\textit{Panel C: Points-Weighted CF Logit}} \\\\[3pt]",
      sprintf("$\\hat{\\delta}$ (points-weighted) & %s%s & (%s) \\\\",
              fmt(delta_wt, 4), add_stars(delta_wt_p), fmt(delta_wt_se, 4))
    )
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


# Write all four tables
write_gs_table_full(gs_atp_fit, gs_atp_wt, gs_atp_cf,
                    gs_atp_ev, file.path(TABLE_DIR, "table_tournament_gs_atp.tex"), "GS-ATP")
write_gs_table_full(gs_wta_fit, gs_wta_wt, gs_wta_cf,
                    gs_wta_ev, file.path(TABLE_DIR, "table_tournament_gs_wta.tex"), "GS-WTA")
write_nongs_table_full(nongs_atp_fit, nongs_atp_wt, nongs_atp_cf_inf, nongs_atp_boot,
                       nongs_atp_ev, file.path(TABLE_DIR, "table_tournament_nongs_atp.tex"), "nonGS-ATP")
write_nongs_table_full(nongs_wta_fit, nongs_wta_wt, nongs_wta_cf_inf, nongs_wta_boot,
                       nongs_wta_ev, file.path(TABLE_DIR, "table_tournament_nongs_wta.tex"), "nonGS-WTA")


###############################################################################
# PHASE 7: FIX 3 -- REGENERATE 4-PANEL FIGURE WITH CORRECTED DATA
###############################################################################

message("\n[7] FIX 3: Regenerating 4-panel figure with corrected match data...")

make_calendar_horizon_figure <- function(fit_obj, cf_obj, label) {
  if (is.null(fit_obj) || is.null(cf_obj)) return(NULL)

  horizons_weeks <- c(4, 8, 12, 26, 52)
  df <- cf_obj$match_cf

  # Assign calendar horizon bins
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

  # Tournament-level quantities by horizon
  te <- cf_obj$tourney_effects
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

  # Panel (c): Cumulative DeltaE[RP]
  panel_c_data <- data.frame(cal_horizon = paste0(horizons_weeks, "w"),
                              cum_derp = NA_real_)
  for (idx in seq_along(horizons_weeks)) {
    h <- horizons_weeks[idx]
    sub <- te |> filter(as.numeric(gsub("w", "", as.character(cal_horizon))) <= h)
    panel_c_data$cum_derp[idx] <- sum(sub$delta_erp, na.rm = TRUE) / max(n_distinct(sub$event_id), 1)
  }
  panel_c_data$cal_horizon <- factor(panel_c_data$cal_horizon,
                                      levels = paste0(horizons_weeks, "w"))

  p_c <- ggplot(panel_c_data, aes(x = cal_horizon, y = cum_derp, group = 1)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_line(color = col_treat, linewidth = 0.8) +
    geom_point(color = col_treat, size = 3) +
    labs(x = "Calendar Horizon", y = expression("Cumulative "*Delta*"E[RP]")) +
    theme_paper()

  # Panel (d): Cumulative DeltaE[W]
  panel_d_data <- data.frame(cal_horizon = paste0(horizons_weeks, "w"),
                              cum_dew = NA_real_)
  for (idx in seq_along(horizons_weeks)) {
    h <- horizons_weeks[idx]
    sub <- te |> filter(as.numeric(gsub("w", "", as.character(cal_horizon))) <= h)
    panel_d_data$cum_dew[idx] <- sum(sub$delta_ew, na.rm = TRUE) / max(n_distinct(sub$event_id), 1)
  }
  panel_d_data$cal_horizon <- factor(panel_d_data$cal_horizon,
                                      levels = paste0(horizons_weeks, "w"))

  p_d <- ggplot(panel_d_data, aes(x = cal_horizon, y = cum_dew, group = 1)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_line(color = col_treat, linewidth = 0.8) +
    geom_point(color = col_treat, size = 3) +
    labs(x = "Calendar Horizon", y = expression("Cumulative "*Delta*"E[W]")) +
    theme_paper()

  combined <- (p_a + p_b) / (p_c + p_d) +
    plot_annotation(tag_levels = list(c("(a)", "(b)", "(c)", "(d)"))) &
    theme(plot.tag = element_text(family = "serif", size = 12))

  combined
}

# Try each sample in order of preference
fig_obj <- NULL
for (triple in list(
  list(f = gs_atp_fit, c = gs_atp_cf, l = "GS-ATP"),
  list(f = nongs_atp_fit, c = nongs_atp_cf, l = "nonGS-ATP"),
  list(f = gs_wta_fit, c = gs_wta_cf, l = "GS-WTA")
)) {
  fig_obj <- make_calendar_horizon_figure(triple$f, triple$c, triple$l)
  if (!is.null(fig_obj)) {
    message("  Using ", triple$l, " for four-panel figure")
    break
  }
}

if (!is.null(fig_obj)) {
  ggsave(file.path(FIG_DIR, "fig_tournament_4panel.pdf"), fig_obj,
         width = 10, height = 8, device = cairo_pdf)
  message("  Saved: Figures/fig_tournament_4panel.pdf")
} else {
  message("  WARNING: Could not generate four-panel figure")
}


###############################################################################
# PHASE 8: SAVE RESULTS AND SUMMARY
###############################################################################

message("\n[8] Saving results...")

fix_results <- list(
  gs_atp = list(events = gs_atp_ev, matches = gs_atp_md,
                fit = gs_atp_fit, fit_wt = gs_atp_wt, cf = gs_atp_cf),
  gs_wta = list(events = gs_wta_ev, matches = gs_wta_md,
                fit = gs_wta_fit, fit_wt = gs_wta_wt, cf = gs_wta_cf),
  nongs_atp = list(events = nongs_atp_ev, matches = nongs_atp_md,
                   fit = nongs_atp_fit, fit_wt = nongs_atp_wt,
                   cf = nongs_atp_cf, boot = nongs_atp_boot,
                   cf_inf = nongs_atp_cf_inf),
  nongs_wta = list(events = nongs_wta_ev, matches = nongs_wta_md,
                   fit = nongs_wta_fit, fit_wt = nongs_wta_wt,
                   cf = nongs_wta_cf, boot = nongs_wta_boot,
                   cf_inf = nongs_wta_cf_inf)
)

saveRDS(fix_results, file.path(CLEANED_DIR, "tournament_match_fix_results.rds"))
message("  Saved: Data/cleaned/tournament_match_fix_results.rds")

# Write summary
summary_text <- c(
  "# Tournament Match Fix Summary (Script 24)",
  paste0("Generated: ", Sys.time()),
  "",
  "## Fixes Applied",
  "",
  "### FIX 1: Include ALL matches (main + qual + challenger)",
  "The build_match_level() function in script 22 filtered match_source == 'main',",
  "dropping all qualifying and challenger matches. Players who return to challengers",
  "after a GS LL spot were invisible. Fix: removed the filter.",
  "",
  "### FIX 2: All covariate coefficients in tables",
  "Tables now report ALL covariates (log_rank_ratio, surface, H2H, etc.),",
  "not just delta and rho.",
  "",
  "### FIX 3: Regenerated 4-panel figure with corrected data",
  "fig_tournament_4panel.pdf now uses the full match dataset.",
  "",
  "### FIX 4: Points-weighted robustness with corrected data",
  "Panel C of each table re-estimated with corrected match data.",
  "",
  "## Match Count Comparison (OLD vs NEW)",
  "",
  sprintf("| Sample | Old Matches | New Matches | Old Players | New Players |"),
  sprintf("|--------|-------------|-------------|-------------|-------------|"),
  sprintf("| GS-ATP | %s | %s | %d | %d |",
          format(old_gs_atp_n, big.mark = ","),
          format(nrow(gs_atp_md), big.mark = ","),
          old_gs_atp_players,
          n_distinct(gs_atp_md$focal_pid)),
  sprintf("| GS-WTA | %s | %s | %d | %d |",
          format(old_gs_wta_n, big.mark = ","),
          format(nrow(gs_wta_md), big.mark = ","),
          old_gs_wta_players,
          n_distinct(gs_wta_md$focal_pid)),
  sprintf("| nonGS-ATP | %s | %s | %d | %d |",
          format(old_nongs_atp_n, big.mark = ","),
          format(nrow(nongs_atp_md), big.mark = ","),
          old_nongs_atp_players,
          n_distinct(nongs_atp_md$focal_pid)),
  sprintf("| nonGS-WTA | %s | %s | %d | %d |",
          format(old_nongs_wta_n, big.mark = ","),
          format(nrow(nongs_wta_md), big.mark = ","),
          old_nongs_wta_players,
          n_distinct(nongs_wta_md$focal_pid)),
  "",
  "## Delta Estimates (NEW, corrected data)",
  "",
  summary_log,
  ""
)

writeLines(summary_text, file.path(OUTPUT_DIR, "tournament_match_fix_summary.md"))
message("  Saved: Output/tournament_match_fix_summary.md")

message("\n", strrep("=", 72))
message("  DONE: 24_tournament_match_fix.R")
message(strrep("=", 72))
