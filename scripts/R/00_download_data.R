# ==============================================================================
# 00_download_data.R
# Download ATP and WTA match data from Jeff Sackmann's GitHub repositories
# Project: Lucky Losers and Career Trajectories
# ==============================================================================

library(readr)
library(dplyr)
library(purrr)

# --- Configuration -----------------------------------------------------------
RAW_DIR <- here::here("Data", "raw")
dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)

ATP_BASE <- "https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/"
WTA_BASE <- "https://raw.githubusercontent.com/JeffSackmann/tennis_wta/master/"

# Sample period for main analysis
YEAR_START_MAIN <- 1990
YEAR_END        <- 2024

# Qualifying/challenger data starts later
YEAR_START_QUAL_ATP <- 1978
YEAR_START_QUAL_WTA <- 1968

# --- Helper: safe download with type coercion ---------------------------------
safe_read_csv <- function(url, ...) {
  tryCatch({
    df <- read_csv(url, show_col_types = FALSE, ...)
    # Coerce seed columns to character (mixed types across years)
    if ("winner_seed" %in% names(df)) df$winner_seed <- as.character(df$winner_seed)
    if ("loser_seed" %in% names(df))  df$loser_seed  <- as.character(df$loser_seed)
    df
  },
  error = function(e) {
    message("  Skipping (not found): ", basename(url))
    tibble()
  })
}

# --- Download ATP main draw matches -------------------------------------------
message("=== Downloading ATP main draw matches ===")
atp_main_files <- paste0(ATP_BASE, "atp_matches_", YEAR_START_MAIN:YEAR_END, ".csv")
atp_main <- map(atp_main_files, safe_read_csv, .progress = TRUE) |>
  list_rbind()
message("  ATP main draw: ", nrow(atp_main), " matches")

# --- Download ATP qualifying/challenger matches -------------------------------
message("=== Downloading ATP qualifying/challenger matches ===")
atp_qual_files <- paste0(ATP_BASE, "atp_matches_qual_chall_", YEAR_START_QUAL_ATP:YEAR_END, ".csv")
atp_qual <- map(atp_qual_files, safe_read_csv, .progress = TRUE) |>
  list_rbind()
message("  ATP qual/chall: ", nrow(atp_qual), " matches")

# --- Download WTA main draw matches -------------------------------------------
message("=== Downloading WTA main draw matches ===")
wta_main_files <- paste0(WTA_BASE, "wta_matches_", YEAR_START_MAIN:YEAR_END, ".csv")
wta_main <- map(wta_main_files, safe_read_csv, .progress = TRUE) |>
  list_rbind()
message("  WTA main draw: ", nrow(wta_main), " matches")

# --- Download WTA qualifying/ITF matches -------------------------------------
message("=== Downloading WTA qualifying/ITF matches ===")
wta_qual_files <- paste0(WTA_BASE, "wta_matches_qual_itf_", YEAR_START_QUAL_WTA:YEAR_END, ".csv")
wta_qual <- map(wta_qual_files, safe_read_csv, .progress = TRUE) |>
  list_rbind()
message("  WTA qual/ITF: ", nrow(wta_qual), " matches")

# --- Download ATP rankings ----------------------------------------------------
message("=== Downloading ATP rankings ===")
atp_rank_files <- c(
  paste0(ATP_BASE, "atp_rankings_", c("70s", "80s", "90s", "00s", "10s", "20s"), ".csv"),
  paste0(ATP_BASE, "atp_rankings_current.csv")
)
atp_rankings <- map(atp_rank_files, safe_read_csv, .progress = TRUE) |>
  list_rbind()
message("  ATP rankings: ", nrow(atp_rankings), " rows")

# --- Download WTA rankings ----------------------------------------------------
message("=== Downloading WTA rankings ===")
wta_rank_files <- c(
  paste0(WTA_BASE, "wta_rankings_", c("70s", "80s", "90s", "00s", "10s", "20s"), ".csv"),
  paste0(WTA_BASE, "wta_rankings_current.csv")
)
wta_rankings <- map(wta_rank_files, safe_read_csv, .progress = TRUE) |>
  list_rbind()
message("  WTA rankings: ", nrow(wta_rankings), " rows")

# --- Download player biographical data ----------------------------------------
message("=== Downloading player data ===")
atp_players <- safe_read_csv(paste0(ATP_BASE, "atp_players.csv"))
wta_players <- safe_read_csv(paste0(WTA_BASE, "wta_players.csv"))
message("  ATP players: ", nrow(atp_players), "; WTA players: ", nrow(wta_players))

# --- Add tour indicator and save ----------------------------------------------
atp_main$tour <- "ATP"
atp_qual$tour <- "ATP"
wta_main$tour <- "WTA"
wta_qual$tour <- "WTA"

message("=== Saving to ", RAW_DIR, " ===")
write_rds(atp_main, file.path(RAW_DIR, "atp_main_matches.rds"))
write_rds(atp_qual, file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
write_rds(wta_main, file.path(RAW_DIR, "wta_main_matches.rds"))
write_rds(wta_qual, file.path(RAW_DIR, "wta_qual_itf_matches.rds"))
write_rds(atp_rankings, file.path(RAW_DIR, "atp_rankings.rds"))
write_rds(wta_rankings, file.path(RAW_DIR, "wta_rankings.rds"))
write_rds(atp_players, file.path(RAW_DIR, "atp_players.rds"))
write_rds(wta_players, file.path(RAW_DIR, "wta_players.rds"))

message("=== Download complete ===")
message("  Files saved to: ", RAW_DIR)
