# ==============================================================================
# 03_merge_rankings.R
# Merge estimation sample with weekly rankings panel to build outcome
# trajectories: ranking at t+4, t+8, t+12, t+26, t+52 weeks
# Project: Lucky Losers and Career Trajectories
# ==============================================================================

library(readr)
library(dplyr)
library(stringr)

RAW_DIR     <- here::here("Data", "raw")
CLEANED_DIR <- here::here("Data", "cleaned")

# --- 1. Load data -------------------------------------------------------------
message("=== Loading data ===")
qual_losers  <- read_rds(file.path(CLEANED_DIR, "estimation_sample.rds"))
atp_rankings <- read_rds(file.path(RAW_DIR, "atp_rankings.rds"))

# --- 2. Parse dates -----------------------------------------------------------
message("=== Parsing dates ===")

# Rankings: ranking_date is numeric YYYYMMDD
atp_rankings <- atp_rankings |>
  mutate(
    rank_date = as.Date(as.character(ranking_date), format = "%Y%m%d")
  ) |>
  filter(!is.na(rank_date))

message("  Rankings rows with valid date: ", nrow(atp_rankings))
message("  Date range: ", min(atp_rankings$rank_date), " to ", max(atp_rankings$rank_date))

# Tournament date: tourney_date is numeric YYYYMMDD
qual_losers <- qual_losers |>
  mutate(
    event_date = as.Date(as.character(tourney_date), format = "%Y%m%d")
  )

message("  Qualifying losers with valid event date: ", sum(!is.na(qual_losers$event_date)))

# --- 3. For each qualifying loser, find ranking at specific horizons ----------
message("=== Building ranking trajectories ===")

# Horizons in weeks
horizons <- c(0, 4, 8, 12, 26, 52)

# Vectorized approach: for each horizon, do a range join
# More efficient than row-by-row lookup

# Rename ranking columns for clarity
rankings_slim <- atp_rankings |>
  select(player, rank_date, rank, points)

message("  Building trajectory for each horizon...")

# Initialize output
trajectory_list <- list()

for (h in horizons) {
  message("    Horizon: t+", h, " weeks")

  target_df <- qual_losers |>
    filter(!is.na(event_date)) |>
    transmute(
      tourney_id,
      player_id,
      target_date = event_date + h * 7
    )

  # Range join: find rankings within +-10 days of target
  matched <- target_df |>
    inner_join(
      rankings_slim |> rename(player_id = player),
      by = "player_id",
      relationship = "many-to-many"
    ) |>
    filter(abs(as.numeric(rank_date - target_date)) <= 10) |>
    mutate(date_diff = abs(as.numeric(rank_date - target_date))) |>
    group_by(tourney_id, player_id) |>
    slice_min(date_diff, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(tourney_id, player_id,
           !!paste0("rank_t", h) := rank,
           !!paste0("points_t", h) := points)

  trajectory_list[[paste0("t", h)]] <- matched
}

# --- 4. Join all horizons to estimation sample --------------------------------
message("=== Joining trajectories to estimation sample ===")

result <- qual_losers
for (h in horizons) {
  df <- trajectory_list[[paste0("t", h)]]
  result <- result |>
    left_join(df, by = c("tourney_id", "player_id"))
}

# --- 5. Compute outcome variables ---------------------------------------------
message("=== Computing outcome variables ===")

result <- result |>
  mutate(
    # Ranking changes (negative = improvement)
    rank_change_4w  = rank_t4  - rank_t0,
    rank_change_8w  = rank_t8  - rank_t0,
    rank_change_12w = rank_t12 - rank_t0,
    rank_change_26w = rank_t26 - rank_t0,
    rank_change_52w = rank_t52 - rank_t0,
    # Points changes (positive = improvement)
    points_change_4w  = points_t4  - points_t0,
    points_change_8w  = points_t8  - points_t0,
    points_change_12w = points_t12 - points_t0,
    points_change_26w = points_t26 - points_t0,
    points_change_52w = points_t52 - points_t0,
    # Attrition indicators (player not found in rankings)
    attrit_4w  = is.na(rank_t4),
    attrit_8w  = is.na(rank_t8),
    attrit_12w = is.na(rank_t12),
    attrit_26w = is.na(rank_t26),
    attrit_52w = is.na(rank_t52)
  )

# --- 6. Attrition check -------------------------------------------------------
message("=== Attrition by treatment status ===")
attrition <- result |>
  filter(n_ll_slots > 0) |>
  group_by(got_ll) |>
  summarise(
    n = n(),
    attrit_4w  = mean(attrit_4w),
    attrit_8w  = mean(attrit_8w),
    attrit_12w = mean(attrit_12w),
    attrit_26w = mean(attrit_26w),
    attrit_52w = mean(attrit_52w),
    .groups = "drop"
  )
message("  Attrition rates:")
print(attrition)

# --- 7. Descriptive statistics at the cutoff ----------------------------------
message("\n=== Descriptive stats near cutoff (|R_tilde| <= 2) ===")
near_cutoff <- result |>
  filter(n_ll_slots > 0, abs(R_tilde) <= 2)

desc_stats <- near_cutoff |>
  group_by(got_ll) |>
  summarise(
    n = n(),
    mean_rank = mean(player_rank, na.rm = TRUE),
    mean_age = mean(player_age, na.rm = TRUE),
    mean_rank_change_12w = mean(rank_change_12w, na.rm = TRUE),
    mean_rank_change_26w = mean(rank_change_26w, na.rm = TRUE),
    mean_rank_change_52w = mean(rank_change_52w, na.rm = TRUE),
    mean_points_change_12w = mean(points_change_12w, na.rm = TRUE),
    mean_points_change_26w = mean(points_change_26w, na.rm = TRUE),
    .groups = "drop"
  )
message("  Treatment vs Control (near cutoff):")
print(desc_stats)

# --- 8. Save ------------------------------------------------------------------
message("=== Saving ===")
write_rds(result, file.path(CLEANED_DIR, "estimation_sample_with_rankings.rds"))
write_rds(attrition, file.path(CLEANED_DIR, "attrition_check.rds"))
message("  Saved to: ", file.path(CLEANED_DIR, "estimation_sample_with_rankings.rds"))
message("  Total rows: ", nrow(result))
message("  Columns: ", ncol(result))
