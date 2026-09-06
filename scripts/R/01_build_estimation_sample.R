# ==============================================================================
# 01_build_estimation_sample.R
# Construct the estimation sample: identify LL entries, final-round qualifying
# losers, and build the running variable for the RDD
# Project: Lucky Losers and Career Trajectories
# ==============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)

set.seed(20260321)

RAW_DIR     <- here::here("Data", "raw")
CLEANED_DIR <- here::here("Data", "cleaned")
dir.create(CLEANED_DIR, recursive = TRUE, showWarnings = FALSE)

# --- 1. Load data -------------------------------------------------------------
message("=== Loading data ===")
atp_main <- read_rds(file.path(RAW_DIR, "atp_main_matches.rds"))
atp_qual <- read_rds(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
atp_rankings <- read_rds(file.path(RAW_DIR, "atp_rankings.rds"))
atp_players  <- read_rds(file.path(RAW_DIR, "atp_players.rds"))

# --- 2. Audit LL entry field --------------------------------------------------
message("=== Auditing LL entry field ===")

# Count entry types in main draw matches
entry_audit <- atp_main |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(year >= 2000) |>
  pivot_longer(
    cols = c(winner_entry, loser_entry),
    names_to = "role",
    values_to = "entry"
  ) |>
  filter(!is.na(entry) & entry != "") |>
  count(year, entry) |>
  arrange(year, entry)

# LL counts by year
ll_by_year <- entry_audit |>
  filter(entry == "LL") |>
  arrange(year)

message("  LL entries by year (main draw):")
print(ll_by_year, n = 30)

# Check for "Alt" entries that might be misclassified LLs
alt_by_year <- entry_audit |>
  filter(str_detect(entry, regex("alt|AL", ignore_case = TRUE)))

if (nrow(alt_by_year) > 0) {
  message("  WARNING: Found 'Alt' entries that may be LL:")
  print(alt_by_year)
}

# --- 3. Identify final qualifying round per tournament ------------------------
message("=== Identifying final qualifying rounds ===")

# Filter to tour-level qualifying matches only (exclude challengers, ITF)
# tourney_level: G = Grand Slam, M = Masters, A = ATP tour-level
atp_qual_tour <- atp_qual |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(
    year >= 2000,
    tourney_level %in% c("G", "M", "A"),
    str_detect(round, "^Q")           # qualifying rounds only
  )

message("  Tour-level qualifying matches (2000+): ", nrow(atp_qual_tour))

# Determine the final qualifying round per tournament
# Q1, Q2, Q3 — the highest number is the final round
final_qual_round <- atp_qual_tour |>
  group_by(tourney_id) |>
  summarise(
    max_qual_round = max(round),
    n_qual_matches = n(),
    .groups = "drop"
  )

message("  Final qualifying round distribution:")
print(count(final_qual_round, max_qual_round))

# --- 4. Extract final-round qualifying losers ---------------------------------
message("=== Extracting final-round qualifying losers ===")

# Join to get only final-round qualifying matches
final_qual_matches <- atp_qual_tour |>
  inner_join(
    final_qual_round |> select(tourney_id, max_qual_round),
    by = "tourney_id"
  ) |>
  filter(round == max_qual_round)

message("  Final qualifying round matches: ", nrow(final_qual_matches))

# The LOSERS of these matches are the potential LL candidates
# Build a player-tournament level dataset of qualifying losers
qual_losers <- final_qual_matches |>
  transmute(
    tourney_id,
    tourney_name,
    tourney_date,
    tourney_level,
    surface,
    draw_size,
    year = as.integer(str_sub(tourney_date, 1, 4)),
    # Loser info
    player_id = loser_id,
    player_name = loser_name,
    player_rank = loser_rank,
    player_rank_points = loser_rank_points,
    player_age = loser_age,
    player_hand = loser_hand,
    player_ht = loser_ht,
    player_ioc = loser_ioc,
    player_seed = loser_seed,
    # Match info from qualifying final
    qual_opponent_id = winner_id,
    qual_opponent_rank = winner_rank,
    qual_score = score,
    qual_minutes = minutes,
    # Stats from qualifying match (may be NA pre-2011)
    qual_w_ace = l_ace,
    qual_w_df = l_df,
    qual_w_svpt = l_svpt,
    qual_w_1stIn = l_1stIn,
    qual_w_1stWon = l_1stWon,
    qual_w_2ndWon = l_2ndWon,
    qual_w_bpSaved = l_bpSaved,
    qual_w_bpFaced = l_bpFaced
  )

message("  Qualifying losers (player-tournament): ", nrow(qual_losers))

# --- 5. Build the running variable --------------------------------------------
message("=== Building running variable ===")

# Rank qualifying losers by ATP ranking within each tournament
# Lower rank number = better ranked = higher priority for LL
qual_losers <- qual_losers |>
  group_by(tourney_id) |>
  mutate(
    n_losers = n(),
    # Rank among losers: 1 = highest-ranked (best position for LL)
    # Players without ranking go to the bottom
    rank_among_losers = rank(
      ifelse(is.na(player_rank), 9999, player_rank),
      ties.method = "min"
    )
  ) |>
  ungroup()

# --- 6. Identify who actually got LL entry ------------------------------------
message("=== Identifying actual LL entries ===")

# Find all LL entries in main draw
ll_winners <- atp_main |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(year >= 2000, winner_entry == "LL") |>
  transmute(
    tourney_id, tourney_name, tourney_date, year,
    ll_player_id = winner_id,
    ll_player_name = winner_name,
    ll_round = round,
    ll_won_match = 1L
  )

ll_losers <- atp_main |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(year >= 2000, loser_entry == "LL") |>
  transmute(
    tourney_id, tourney_name, tourney_date, year,
    ll_player_id = loser_id,
    ll_player_name = loser_name,
    ll_round = round,
    ll_won_match = 0L
  )

ll_entries <- bind_rows(ll_winners, ll_losers)

# Deduplicate: one row per player-tournament LL entry
# (a player may appear in multiple rounds as LL)
ll_entries_unique <- ll_entries |>
  group_by(tourney_id, ll_player_id) |>
  summarise(
    ll_player_name = first(ll_player_name),
    ll_matches_played = n(),
    ll_matches_won = sum(ll_won_match),
    ll_best_round = last(ll_round),
    .groups = "drop"
  )

message("  Unique LL entries (player-tournament): ", nrow(ll_entries_unique))

# --- 7. Match LL entries to qualifying losers ---------------------------------
message("=== Matching LL entries to qualifying losers ===")

# Flag which qualifying losers actually received LL entry
qual_losers <- qual_losers |>
  left_join(
    ll_entries_unique |>
      transmute(tourney_id, player_id = ll_player_id, got_ll = 1L,
                ll_matches_played, ll_matches_won, ll_best_round),
    by = c("tourney_id", "player_id")
  ) |>
  mutate(got_ll = replace_na(got_ll, 0L))

# --- 8. Count LL slots per tournament -----------------------------------------
ll_slots <- qual_losers |>
  group_by(tourney_id) |>
  summarise(
    n_ll_slots = sum(got_ll),
    .groups = "drop"
  )

qual_losers <- qual_losers |>
  left_join(ll_slots, by = "tourney_id")

# --- 9. Build normalized running variable -------------------------------------
# R_tilde = rank_among_losers - n_ll_slots
# Players with R_tilde <= 0 should be treated; R_tilde > 0 should be control
qual_losers <- qual_losers |>
  mutate(
    R_tilde = rank_among_losers - n_ll_slots,
    # Predicted treatment based on ranking (for concordance check)
    predicted_ll = as.integer(rank_among_losers <= n_ll_slots)
  )

# --- 10. Concordance check: does ranking predict LL status? -------------------
message("=== Concordance check ===")
concordance <- qual_losers |>
  filter(n_ll_slots > 0) |>        # only tournaments with at least one LL slot
  summarise(
    n = n(),
    n_correct = sum(got_ll == predicted_ll),
    concordance_rate = mean(got_ll == predicted_ll),
    n_treated = sum(got_ll),
    n_predicted_treated = sum(predicted_ll)
  )
message("  Concordance rate: ", round(concordance$concordance_rate, 3))
message("  N observations: ", concordance$n)
message("  Actual LL: ", concordance$n_treated, "; Predicted LL: ", concordance$n_predicted_treated)

# Concordance by year
concordance_by_year <- qual_losers |>
  filter(n_ll_slots > 0) |>
  group_by(year) |>
  summarise(
    n = n(),
    concordance = mean(got_ll == predicted_ll),
    n_ll = sum(got_ll),
    .groups = "drop"
  )
message("  Concordance by year:")
print(concordance_by_year, n = 30)

# --- 11. Running variable distribution ----------------------------------------
message("=== Running variable distribution (pooled) ===")
rv_dist <- qual_losers |>
  filter(n_ll_slots > 0) |>
  count(R_tilde) |>
  arrange(R_tilde)
message("  Mass points near cutoff:")
print(filter(rv_dist, abs(R_tilde) <= 5), n = 20)

# --- 12. Tournament-level summary --------------------------------------------
message("=== Tournament-level summary ===")
tourney_summary <- qual_losers |>
  group_by(tourney_id, tourney_name, year, tourney_level, surface) |>
  summarise(
    n_qual_losers = n(),
    n_ll_slots = first(n_ll_slots),
    n_actual_ll = sum(got_ll),
    mean_rank = mean(player_rank, na.rm = TRUE),
    .groups = "drop"
  )

message("  Tournaments with qualifying data: ", nrow(tourney_summary))
message("  Tournaments with LL slots: ", sum(tourney_summary$n_ll_slots > 0))
message("  Distribution of LL slots:")
print(count(tourney_summary, n_ll_slots))

message("  By tournament level:")
tourney_summary |>
  group_by(tourney_level) |>
  summarise(
    n_tournaments = n(),
    mean_qual_losers = mean(n_qual_losers),
    mean_ll_slots = mean(n_ll_slots),
    .groups = "drop"
  ) |>
  print()

# --- 13. Grand Slam subsample ------------------------------------------------
message("=== Grand Slam LL subsample ===")
gs_subsample <- qual_losers |>
  filter(tourney_level == "G")

gs_ll <- gs_subsample |>
  filter(got_ll == 1)

message("  Grand Slam qualifying losers: ", nrow(gs_subsample))
message("  Grand Slam LL entries: ", nrow(gs_ll))
message("  Grand Slam LL by tournament:")
gs_ll |>
  mutate(slam = str_extract(tourney_name, "Australian|Roland|Wimbledon|US")) |>
  count(slam, year) |>
  pivot_wider(names_from = slam, values_from = n, values_fill = 0) |>
  print(n = 30)

# --- 14. Save estimation sample -----------------------------------------------
message("=== Saving estimation sample ===")
write_rds(qual_losers, file.path(CLEANED_DIR, "estimation_sample.rds"))
write_rds(tourney_summary, file.path(CLEANED_DIR, "tournament_summary.rds"))
write_rds(ll_entries_unique, file.path(CLEANED_DIR, "ll_entries.rds"))
write_rds(concordance_by_year, file.path(CLEANED_DIR, "concordance_by_year.rds"))
write_rds(rv_dist, file.path(CLEANED_DIR, "running_variable_distribution.rds"))

# Save audit report
audit <- list(
  entry_audit = entry_audit,
  ll_by_year = ll_by_year,
  concordance = concordance,
  concordance_by_year = concordance_by_year,
  rv_dist = rv_dist,
  tourney_summary = tourney_summary
)
write_rds(audit, file.path(CLEANED_DIR, "data_audit.rds"))

message("=== Estimation sample construction complete ===")
message("  Total qualifying losers: ", nrow(qual_losers))
message("  LL recipients: ", sum(qual_losers$got_ll))
message("  Control (non-LL): ", sum(!qual_losers$got_ll))
message("  Years covered: ", min(qual_losers$year), "-", max(qual_losers$year))
message("  Files saved to: ", CLEANED_DIR)
