# ==============================================================================
# 04_compute_elo.R
# Compute Elo ratings for all ATP players from match history
# Following Sackmann's methodology: K=32 for new players, K=24 for established
# Surface-specific Elo as robustness
# Project: Lucky Losers and Career Trajectories
# ==============================================================================

library(readr)
library(dplyr)
library(stringr)

set.seed(20260321)

RAW_DIR     <- here::here("Data", "raw")
CLEANED_DIR <- here::here("Data", "cleaned")

# --- 1. Load all matches (main + qual) sorted chronologically ----------------
message("=== Loading and preparing match data ===")

atp_main <- read_rds(file.path(RAW_DIR, "atp_main_matches.rds"))
atp_qual <- read_rds(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))

# Combine and sort chronologically
all_matches <- bind_rows(
  atp_main |> mutate(match_source = "main"),
  atp_qual |> mutate(match_source = "qual")
) |>
  mutate(
    tourney_date_num = as.numeric(tourney_date),
    # Round ordering for within-tournament sort
    round_order = case_when(
      round == "Q1" ~ 1, round == "Q2" ~ 2, round == "Q3" ~ 3,
      round == "R128" ~ 4, round == "R64" ~ 5, round == "R32" ~ 6,
      round == "R16" ~ 7, round == "QF" ~ 8, round == "SF" ~ 9,
      round == "F" ~ 10, round == "RR" ~ 11,
      TRUE ~ 0
    )
  ) |>
  arrange(tourney_date_num, tourney_id, round_order, match_num) |>
  filter(!is.na(winner_id), !is.na(loser_id))

message("  Total matches for Elo: ", nrow(all_matches))

# --- 2. Elo computation -------------------------------------------------------
message("=== Computing Elo ratings ===")

# Elo parameters
INITIAL_ELO <- 1500
K_NEW       <- 32     # K-factor for players with < 20 matches
K_ESTAB     <- 24     # K-factor for established players
MATCH_THRESHOLD <- 20 # Matches before switching K-factor

# Storage: player_id -> current Elo, match count
elo_env <- new.env(hash = TRUE)

get_elo <- function(pid) {
  val <- elo_env[[as.character(pid)]]
  if (is.null(val)) return(list(elo = INITIAL_ELO, n = 0L))
  val
}

set_elo <- function(pid, elo_val, n_val) {
  elo_env[[as.character(pid)]] <- list(elo = elo_val, n = n_val)
}

# Expected score
expected <- function(elo_a, elo_b) {
  1 / (1 + 10^((elo_b - elo_a) / 400))
}

# Process matches and record Elo at each match
n_matches <- nrow(all_matches)
winner_elo_pre  <- numeric(n_matches)
loser_elo_pre   <- numeric(n_matches)
winner_elo_post <- numeric(n_matches)
loser_elo_post  <- numeric(n_matches)

# Progress reporting
report_interval <- 100000L

message("  Processing ", n_matches, " matches...")

for (i in seq_len(n_matches)) {
  if (i %% report_interval == 0) message("    ", i, " / ", n_matches)

  wid <- all_matches$winner_id[i]
  lid <- all_matches$loser_id[i]

  w_state <- get_elo(wid)
  l_state <- get_elo(lid)

  winner_elo_pre[i] <- w_state$elo
  loser_elo_pre[i]  <- l_state$elo

  # K-factors
  k_w <- if (w_state$n < MATCH_THRESHOLD) K_NEW else K_ESTAB
  k_l <- if (l_state$n < MATCH_THRESHOLD) K_NEW else K_ESTAB

  # Expected outcomes
  exp_w <- expected(w_state$elo, l_state$elo)
  exp_l <- 1 - exp_w

  # Update
  new_w_elo <- w_state$elo + k_w * (1 - exp_w)
  new_l_elo <- l_state$elo + k_l * (0 - exp_l)

  winner_elo_post[i] <- new_w_elo
  loser_elo_post[i]  <- new_l_elo

  set_elo(wid, new_w_elo, w_state$n + 1L)
  set_elo(lid, new_l_elo, l_state$n + 1L)
}

message("  Elo computation complete.")

# Add to match data
all_matches$winner_elo_pre  <- winner_elo_pre
all_matches$loser_elo_pre   <- loser_elo_pre
all_matches$winner_elo_post <- winner_elo_post
all_matches$loser_elo_post  <- loser_elo_post

# --- 3. Build player-date Elo lookup -----------------------------------------
message("=== Building player-date Elo lookup ===")

# For each player, their Elo after each match
# Winner side
elo_winner <- all_matches |>
  transmute(
    player_id = winner_id,
    match_date = as.Date(as.character(tourney_date_num), format = "%Y%m%d"),
    elo = winner_elo_post
  )

# Loser side
elo_loser <- all_matches |>
  transmute(
    player_id = loser_id,
    match_date = as.Date(as.character(tourney_date_num), format = "%Y%m%d"),
    elo = loser_elo_post
  )

# Combine and keep latest Elo per player-date
elo_history <- bind_rows(elo_winner, elo_loser) |>
  group_by(player_id, match_date) |>
  summarise(elo = last(elo), .groups = "drop") |>
  arrange(player_id, match_date)

message("  Elo history rows: ", nrow(elo_history))

# --- 4. Merge Elo with estimation sample at each horizon ----------------------
message("=== Merging Elo with estimation sample ===")

est_sample <- read_rds(file.path(CLEANED_DIR, "estimation_sample_with_rankings.rds"))

horizons <- c(0, 4, 8, 12, 26, 52)

for (h in horizons) {
  message("  Horizon: t+", h, " weeks")

  target <- est_sample |>
    filter(!is.na(event_date)) |>
    transmute(
      tourney_id, player_id,
      target_date = event_date + h * 7
    )

  # Find closest Elo within +-21 days of target
  matched <- target |>
    inner_join(elo_history, by = "player_id", relationship = "many-to-many") |>
    filter(abs(as.numeric(match_date - target_date)) <= 21) |>
    mutate(date_diff = abs(as.numeric(match_date - target_date))) |>
    group_by(tourney_id, player_id) |>
    slice_min(date_diff, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(tourney_id, player_id, !!paste0("elo_t", h) := elo)

  est_sample <- est_sample |>
    left_join(matched, by = c("tourney_id", "player_id"))
}

# Compute Elo changes
est_sample <- est_sample |>
  mutate(
    elo_change_4w  = elo_t4  - elo_t0,
    elo_change_8w  = elo_t8  - elo_t0,
    elo_change_12w = elo_t12 - elo_t0,
    elo_change_26w = elo_t26 - elo_t0,
    elo_change_52w = elo_t52 - elo_t0
  )

# --- 5. Summary stats --------------------------------------------------------
message("\n=== Elo summary near cutoff (|R_tilde| <= 2) ===")
near_cutoff <- est_sample |>
  filter(n_ll_slots > 0, abs(R_tilde) <= 2)

elo_desc <- near_cutoff |>
  group_by(got_ll) |>
  summarise(
    n = n(),
    mean_elo_t0 = mean(elo_t0, na.rm = TRUE),
    mean_elo_change_12w = mean(elo_change_12w, na.rm = TRUE),
    mean_elo_change_26w = mean(elo_change_26w, na.rm = TRUE),
    mean_elo_change_52w = mean(elo_change_52w, na.rm = TRUE),
    .groups = "drop"
  )
message("  Treatment vs Control Elo:")
print(elo_desc)

# --- 6. Save ------------------------------------------------------------------
message("=== Saving ===")
write_rds(est_sample, file.path(CLEANED_DIR, "estimation_sample_final.rds"))
write_rds(elo_history, file.path(CLEANED_DIR, "elo_history.rds"))
message("  Final estimation sample saved: ", nrow(est_sample), " rows, ", ncol(est_sample), " cols")
message("  Elo history saved: ", nrow(elo_history), " rows")
