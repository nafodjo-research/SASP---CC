# ==============================================================================
# 02_investigate_alt_entries.R
# Investigate whether "Alt"/"ALT" entries are misclassified Lucky Losers
# Project: Lucky Losers and Career Trajectories
# ==============================================================================

library(readr)
library(dplyr)
library(stringr)

RAW_DIR     <- here::here("Data", "raw")
CLEANED_DIR <- here::here("Data", "cleaned")

atp_main <- read_rds(file.path(RAW_DIR, "atp_main_matches.rds"))
atp_qual <- read_rds(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))

# --- 1. All Alt entries in main draw ------------------------------------------
message("=== Alt entries in ATP main draw ===")
alt_matches <- atp_main |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(year >= 2000) |>
  filter(
    str_detect(winner_entry, regex("^alt$", ignore_case = TRUE)) |
    str_detect(loser_entry, regex("^alt$", ignore_case = TRUE))
  )

message("  Total matches with Alt entry: ", nrow(alt_matches))

# Extract Alt players
alt_as_winner <- alt_matches |>
  filter(str_detect(winner_entry, regex("^alt$", ignore_case = TRUE))) |>
  transmute(
    tourney_id, tourney_name, tourney_date, year,
    tourney_level, round, surface,
    player_id = winner_id, player_name = winner_name,
    player_rank = winner_rank, entry = winner_entry,
    role = "winner"
  )

alt_as_loser <- alt_matches |>
  filter(str_detect(loser_entry, regex("^alt$", ignore_case = TRUE))) |>
  transmute(
    tourney_id, tourney_name, tourney_date, year,
    tourney_level, round, surface,
    player_id = loser_id, player_name = loser_name,
    player_rank = loser_rank, entry = loser_entry,
    role = "loser"
  )

alt_players <- bind_rows(alt_as_winner, alt_as_loser)
message("  Alt player-match observations: ", nrow(alt_players))

# Unique Alt player-tournament entries
alt_unique <- alt_players |>
  group_by(tourney_id, player_id) |>
  summarise(
    player_name = first(player_name),
    tourney_name = first(tourney_name),
    year = first(year),
    tourney_level = first(tourney_level),
    player_rank = first(player_rank),
    n_matches = n(),
    .groups = "drop"
  )

message("  Unique Alt player-tournament entries: ", nrow(alt_unique))
message("\n  Alt entries by year:")
print(count(alt_unique, year))

# --- 2. Check if Alt players appear in qualifying ----------------------------
message("\n=== Checking if Alt players lost in qualifying ===")

# Get all qualifying losers
qual_losers_all <- atp_qual |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(year >= 2000, str_detect(round, "^Q")) |>
  transmute(
    tourney_id, player_id = loser_id, player_name = loser_name,
    qual_round = round
  )

# Match Alt players to qualifying losers
alt_in_qual <- alt_unique |>
  inner_join(
    qual_losers_all,
    by = c("tourney_id", "player_id")
  )

message("  Alt players who also lost in qualifying at same tournament: ",
        nrow(alt_in_qual), " of ", nrow(alt_unique))

if (nrow(alt_in_qual) > 0) {
  message("\n  These Alt entries are likely Lucky Losers:")
  alt_in_qual |>
    select(year, tourney_name, player_name, player_rank, qual_round) |>
    print(n = 60)
}

# Alt players NOT in qualifying (true alternates / special entries)
alt_not_in_qual <- alt_unique |>
  anti_join(qual_losers_all, by = c("tourney_id", "player_id"))

message("\n  Alt players NOT found in qualifying: ", nrow(alt_not_in_qual))
if (nrow(alt_not_in_qual) > 0) {
  message("  These may be true alternates (not from qualifying):")
  alt_not_in_qual |>
    select(year, tourney_name, player_name, player_rank) |>
    print(n = 30)
}

# --- 3. Check if Alt players also appear as LL --------------------------------
message("\n=== Cross-check: do any Alt entries also have LL coding? ===")
ll_matches <- atp_main |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(year >= 2000) |>
  filter(winner_entry == "LL" | loser_entry == "LL")

# Players who are coded Alt in some matches and LL in others at the same tournament
alt_player_ids <- alt_unique$player_id
alt_also_ll <- atp_main |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(year >= 2000) |>
  filter(
    (winner_id %in% alt_player_ids & winner_entry == "LL") |
    (loser_id %in% alt_player_ids & loser_entry == "LL")
  )

message("  Matches where Alt-coded players also appear as LL: ", nrow(alt_also_ll))

# --- 4. Summary and recommendation -------------------------------------------
message("\n=== SUMMARY ===")
message("  Total Alt entries: ", nrow(alt_unique))
message("  Alt entries that lost in qualifying (likely LL): ", nrow(alt_in_qual))
message("  Alt entries not in qualifying (true alternates): ", nrow(alt_not_in_qual))
message("  Percentage likely LL: ",
        round(100 * nrow(alt_in_qual) / nrow(alt_unique), 1), "%")

# --- 5. Save results ---------------------------------------------------------
alt_analysis <- list(
  alt_unique = alt_unique,
  alt_in_qual = alt_in_qual,
  alt_not_in_qual = alt_not_in_qual,
  recommendation = ifelse(
    nrow(alt_in_qual) > nrow(alt_not_in_qual),
    "RECLASSIFY: Most Alt entries are likely LLs. Reclassify alt_in_qual as LL.",
    "KEEP SEPARATE: Most Alt entries are not from qualifying."
  )
)
write_rds(alt_analysis, file.path(CLEANED_DIR, "alt_entry_analysis.rds"))
message("  Analysis saved to: ", file.path(CLEANED_DIR, "alt_entry_analysis.rds"))
