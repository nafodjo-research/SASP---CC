# Quick check of main draw data structure
atp_md <- readRDS("Data/raw/atp_main_matches.rds")
cat("=== ATP Main Draw ===\n")
cat("Rows:", nrow(atp_md), "\n")
cat("Columns:\n")
print(names(atp_md))
cat("\nTourney levels:\n")
print(table(atp_md$tourney_level))
cat("\nRounds:\n")
print(table(atp_md$round))
cat("\nRank points avail:", sum(!is.na(atp_md$winner_rank_points)), "/", nrow(atp_md), "\n")
cat("Service pts avail:", sum(!is.na(atp_md$w_svpt)), "/", nrow(atp_md), "\n")

wta_md <- readRDS("Data/raw/wta_main_matches.rds")
cat("\n=== WTA Main Draw ===\n")
cat("Rows:", nrow(wta_md), "\n")
cat("Tourney levels:\n")
print(table(wta_md$tourney_level))

# Check GS event table from script 22
if (file.exists("Data/cleaned/tournament_rebuild_results.rds")) {
  tr <- readRDS("Data/cleaned/tournament_rebuild_results.rds")
  cat("\n=== Tournament rebuild results ===\n")
  cat("Class:", class(tr), "\n")
  if (is.list(tr)) cat("Names:", paste(names(tr), collapse=", "), "\n")
}

# Check what event tables exist
evt_files <- list.files("Data/cleaned/", pattern="event_table|tournament_model|tournament_rebuild", full.names=TRUE)
cat("\n=== Event table files ===\n")
cat(paste(evt_files, collapse="\n"), "\n")
