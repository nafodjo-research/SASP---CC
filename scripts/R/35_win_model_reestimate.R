# ==============================================================================
# 35_win_model_reestimate.R
# Re-estimate match-winning logit on ALL qualifying rounds with:
#   1. Weighted MLE (weights = total points in play; round number as fallback)
#   2. Elo-based covariates matching manuscript description
#   3. Summary table of estimation results
#   4. Re-compute P_i^{LL} using new model
#   5. Re-run all non-GS analyses
#
# Covariates (matching manuscript Section 4.3 and Appendix):
#   X_{ijm}: elo_diff, surface_elo_diff, h2h_win_prop, h2h_count,
#            age_diff, ht_diff, hand_mismatch
#   Z_{ie}:  tournament level dummies, surface dummies
#
# Inputs:
#   Data/raw/atp_qual_chall_matches.rds
#   Data/raw/wta_qual_itf_matches.rds
#   Data/cleaned/elo_history.rds
#   Data/cleaned/wta_elo_history.rds
#   Data/cleaned/tournament_elo_cache.rds  (surface Elo)
#   Data/cleaned/skeleton_nongs_est_v2.rds
#
# Outputs:
#   Data/cleaned/win_model_v3.rds          (new weighted logit)
#   Data/cleaned/p_ll_corrected_v2.rds     (P_i^{LL} from new model)
#   Data/cleaned/skeleton_nongs_est_v5.rds (estimation data with new P_i^{LL})
#   Tables/table_win_model.tex             (logit summary table)
#   Tables/table_dynamic_stacked_nongs_atp.tex  (re-estimated)
#   Tables/table_dynamic_stacked_nongs_wta.tex  (re-estimated)
#
# Dependencies: dplyr, tidyr, fixest, data.table, modelsummary, here, stringr
# ==============================================================================

set.seed(20260327)

# --- Packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(data.table)
library(fixest)
library(modelsummary)
library(here)
library(stringr)

source(here("scripts", "R", "utils.R"))
summary_log <- character()

# --- Paths -------------------------------------------------------------------
RAW_DIR     <- here("Data", "raw")
CLEANED_DIR <- here("Data", "cleaned")
TABLES_DIR  <- here("Tables")
OUTPUT_DIR  <- here("Output")
for (d in c(CLEANED_DIR, TABLES_DIR, OUTPUT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# --- Outcome definitions (from prior scripts) --------------------------------
outcomes_base  <- c("points_change", "n_main_draws", "n_matches_250plus", "elo_change")
outcome_labels <- c(
  "points_change"     = "Ranking points $\\Delta$",
  "n_main_draws"      = "Main draws entered",
  "n_matches_250plus" = "Matches at 250+",
  "elo_change"        = "Elo $\\Delta$"
)
HORIZONS     <- c(4, 8, 12, 26, 52)
HORIZON_LABS <- paste0(HORIZONS, "w")

ZPRE_FULL <- paste0("pre_rank_pts + pre_rank_pts_sq + pre_elo + pre_elo_sq",
                     " + n_prior_gs_ll_won + n_prior_gs_ll_notwon",
                     " + n_prior_nongs_ll_won + n_prior_nongs_ll_notwon",
                     " + player_age")


# ==============================================================================
# STEP 1: LOAD RAW DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 1: LOADING DATA")
message(strrep("=", 70))

atp_qual <- readRDS(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
wta_qual <- readRDS(file.path(RAW_DIR, "wta_qual_itf_matches.rds"))

# Load Elo histories
atp_elo <- readRDS(file.path(CLEANED_DIR, "elo_history.rds"))
wta_elo <- readRDS(file.path(CLEANED_DIR, "wta_elo_history.rds"))

# Load surface Elo cache
elo_cache <- readRDS(file.path(CLEANED_DIR, "tournament_elo_cache.rds"))

# Load estimation sample (for P_i^{LL} computation later)
nongs_est <- readRDS(file.path(CLEANED_DIR, "skeleton_nongs_est_v2.rds"))
nongs_est$peer_component_old <- nongs_est$peer_component

message("  ATP qualifying matches: ", nrow(atp_qual))
message("  WTA qualifying matches: ", nrow(wta_qual))
message("  ATP Elo history: ", nrow(atp_elo), " entries")
message("  WTA Elo history: ", nrow(wta_elo), " entries")
message("  Non-GS estimation sample: ", nrow(nongs_est), " rows")


# ==============================================================================
# STEP 2: PREPARE ALL QUALIFYING ROUND MATCHES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 2: PREPARE ALL QUALIFYING ROUND MATCHES")
message(strrep("=", 70))

qual_rounds <- c("Q1", "Q2", "Q3", "Q4", "Q5", "QF")

atp_q <- atp_qual |>
  filter(round %in% qual_rounds) |>
  mutate(tour = "ATP")

wta_q <- wta_qual |>
  filter(round %in% qual_rounds) |>
  mutate(tour = "WTA")

all_qual <- bind_rows(atp_q, wta_q)
rm(atp_q, wta_q)

# Parse qualifying round number for weighting
all_qual$round_num <- as.integer(gsub("Q|QF", "", all_qual$round))
# QF maps to "" after gsub -> NA; treat QF as the highest round at that event
all_qual$round_num[all_qual$round == "QF"] <- NA_integer_

# For each event, determine the max round number to assign QF appropriately
max_round_by_event <- all_qual |>
  filter(!is.na(round_num)) |>
  group_by(tourney_id) |>
  summarise(max_q_round = max(round_num), .groups = "drop")

all_qual <- all_qual |>
  left_join(max_round_by_event, by = "tourney_id") |>
  mutate(round_num = ifelse(is.na(round_num), max_q_round + 1L, round_num))

# Compute total points in play (sum of service points for both players)
all_qual$total_pts <- all_qual$w_svpt + all_qual$l_svpt

# Weights: total points in play when available, round number otherwise
# Normalize within each weight type to avoid scale issues
has_pts <- !is.na(all_qual$total_pts)
message("  Matches with service points: ", sum(has_pts), " / ", nrow(all_qual))
message("  Matches using round number as weight: ", sum(!has_pts))

all_qual$weight <- ifelse(has_pts, all_qual$total_pts, all_qual$round_num)
# Standardize weights to mean 1 for numerical stability
all_qual$weight <- all_qual$weight / mean(all_qual$weight, na.rm = TRUE)
# Any remaining NAs get weight 1
all_qual$weight[is.na(all_qual$weight)] <- 1.0

message("  Total qualifying matches for estimation: ", nrow(all_qual))
message("  Weight summary:")
print(summary(all_qual$weight))

slog("## Step 2: Qualifying matches prepared")
slog("- Total matches: ", nrow(all_qual))
slog("- ATP: ", sum(all_qual$tour == "ATP"), ", WTA: ", sum(all_qual$tour == "WTA"))
slog("- With service points: ", sum(has_pts))
slog("")


# ==============================================================================
# STEP 3: MERGE ELO RATINGS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 3: MERGE ELO RATINGS")
message(strrep("=", 70))

# Convert to data.table for efficient rolling join
atp_elo_dt <- as.data.table(atp_elo)
wta_elo_dt <- as.data.table(wta_elo)
setnames(atp_elo_dt, c("player_id", "match_date", "elo"))
setnames(wta_elo_dt, c("player_id", "match_date", "elo"))

# Combine Elo histories
all_elo_dt <- rbind(atp_elo_dt, wta_elo_dt)
setkey(all_elo_dt, player_id, match_date)

# Function to get pre-match Elo using rolling join (most recent before match date)
get_pre_match_elo <- function(player_ids, match_dates) {
  lookup_dt <- data.table(
    player_id = player_ids,
    match_date = as.Date(match_dates),
    dummy = 1L
  )
  setkey(lookup_dt, player_id, match_date)

  # Rolling join: for each lookup row, find the last Elo entry on or before match_date
  joined <- all_elo_dt[lookup_dt, on = .(player_id, match_date), roll = TRUE]
  joined$elo
}

# Parse match dates
all_qual$match_date <- as.Date(all_qual$tourney_date)

message("  Merging winner Elo...")
all_qual$winner_elo <- get_pre_match_elo(all_qual$winner_id, all_qual$match_date)

message("  Merging loser Elo...")
all_qual$loser_elo <- get_pre_match_elo(all_qual$loser_id, all_qual$match_date)

n_winner_elo <- sum(!is.na(all_qual$winner_elo))
n_loser_elo <- sum(!is.na(all_qual$loser_elo))
message("  Winner Elo available: ", n_winner_elo, " / ", nrow(all_qual))
message("  Loser Elo available: ", n_loser_elo, " / ", nrow(all_qual))

# Default Elo for missing: 1500 (standard starting Elo)
all_qual$winner_elo[is.na(all_qual$winner_elo)] <- 1500
all_qual$loser_elo[is.na(all_qual$loser_elo)] <- 1500


# ==============================================================================
# STEP 3b: MERGE SURFACE-SPECIFIC ELO
# ==============================================================================
message("\n  Merging surface-specific Elo...")

# The tournament_elo_cache has environments keyed by "TOUR_PLAYERID"
# with data frames containing (rating, date)
get_surface_elo <- function(player_ids, match_dates, surfaces, tours) {
  elo_vals <- rep(NA_real_, length(player_ids))
  for (i in seq_along(player_ids)) {
    surf <- tolower(surfaces[i])
    if (is.na(surf) || !surf %in% c("hard", "clay", "grass")) next

    env <- elo_cache[[surf]]
    if (is.null(env)) next

    key <- paste0(toupper(tours[i]), "_", player_ids[i])
    if (!exists(key, envir = env)) next

    elo_df <- get(key, envir = env)
    if (is.null(elo_df) || nrow(elo_df) == 0) next

    # Find most recent rating before match date
    md <- as.Date(match_dates[i])
    elo_df$date <- as.Date(elo_df$date)
    prior <- elo_df[elo_df$date <= md, , drop = FALSE]
    if (nrow(prior) > 0) {
      elo_vals[i] <- prior$rating[which.max(prior$date)]
    }
  }
  elo_vals
}

# This is slow for large datasets; process in chunks
message("    Processing surface Elo (this may take a few minutes)...")
chunk_size <- 50000
n_total <- nrow(all_qual)
all_qual$winner_surface_elo <- NA_real_
all_qual$loser_surface_elo <- NA_real_

for (start in seq(1, n_total, by = chunk_size)) {
  end <- min(start + chunk_size - 1, n_total)
  idx <- start:end
  if (start %% 100000 == 1) message("    Chunk ", start, "-", end, " of ", n_total)

  all_qual$winner_surface_elo[idx] <- get_surface_elo(
    all_qual$winner_id[idx], all_qual$match_date[idx],
    all_qual$surface[idx], all_qual$tour[idx]
  )
  all_qual$loser_surface_elo[idx] <- get_surface_elo(
    all_qual$loser_id[idx], all_qual$match_date[idx],
    all_qual$surface[idx], all_qual$tour[idx]
  )
}

n_w_surf <- sum(!is.na(all_qual$winner_surface_elo))
n_l_surf <- sum(!is.na(all_qual$loser_surface_elo))
message("  Winner surface Elo available: ", n_w_surf, " / ", nrow(all_qual))
message("  Loser surface Elo available: ", n_l_surf, " / ", nrow(all_qual))

# Default surface Elo: use overall Elo when surface Elo unavailable
all_qual$winner_surface_elo[is.na(all_qual$winner_surface_elo)] <-
  all_qual$winner_elo[is.na(all_qual$winner_surface_elo)]
all_qual$loser_surface_elo[is.na(all_qual$loser_surface_elo)] <-
  all_qual$loser_elo[is.na(all_qual$loser_surface_elo)]

slog("## Step 3: Elo merged")
slog("- Overall Elo: winner ", n_winner_elo, ", loser ", n_loser_elo)
slog("- Surface Elo: winner ", n_w_surf, ", loser ", n_l_surf)
slog("")


# ==============================================================================
# STEP 4: COMPUTE HEAD-TO-HEAD RECORDS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 4: COMPUTE HEAD-TO-HEAD RECORDS")
message(strrep("=", 70))

# Load ALL match data for H2H computation
atp_all <- readRDS(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
wta_all <- readRDS(file.path(RAW_DIR, "wta_qual_itf_matches.rds"))

# Build cumulative H2H: for each pair (winner, loser) at match date,
# how many prior meetings and what was the win proportion?
# Using data.table for speed
message("  Building H2H from all match data...")

all_matches <- rbind(
  atp_all[, c("winner_id", "loser_id", "tourney_date")],
  wta_all[, c("winner_id", "loser_id", "tourney_date")]
)
rm(atp_all, wta_all)
gc()

all_matches$match_date <- as.Date(all_matches$tourney_date)
all_matches_dt <- as.data.table(all_matches)
rm(all_matches)

# Create a directional match record: player_a beat player_b
# We need bidirectional lookup
h2h_dt <- all_matches_dt[, .(player_a = winner_id, player_b = loser_id, match_date)]
setkey(h2h_dt, player_a, player_b, match_date)

# For each qualifying match, compute H2H between winner and loser
# prior to that match date
message("  Computing H2H for qualifying matches...")

compute_h2h <- function(winner_ids, loser_ids, match_dates) {
  n <- length(winner_ids)
  h2h_wins <- rep(0L, n)
  h2h_total <- rep(0L, n)

  # Batch approach: build lookup for unique pairs
  pairs <- data.table(
    idx = 1:n,
    w_id = winner_ids,
    l_id = loser_ids,
    md = as.Date(match_dates)
  )

  # For each qualifying match: count prior wins of winner over loser
  # and prior wins of loser over winner
  for (i in seq_len(n)) {
    if (i %% 50000 == 0) message("    H2H pair ", i, " / ", n)
    w <- winner_ids[i]
    l <- loser_ids[i]
    d <- as.Date(match_dates[i])

    # Winner beat loser before
    n_w_beat_l <- h2h_dt[player_a == w & player_b == l & match_date < d, .N]
    # Loser beat winner before
    n_l_beat_w <- h2h_dt[player_a == l & player_b == w & match_date < d, .N]

    h2h_wins[i] <- n_w_beat_l
    h2h_total[i] <- n_w_beat_l + n_l_beat_w
  }

  list(h2h_wins = h2h_wins, h2h_total = h2h_total)
}

# This is potentially slow for 300k+ matches. Use a smarter approach:
# Pre-aggregate H2H counts for unique pairs
message("  Pre-aggregating H2H counts for all pairs...")
h2h_agg <- h2h_dt[, .(dates = list(match_date)), by = .(player_a, player_b)]
setkey(h2h_agg, player_a, player_b)

# Vectorized H2H lookup
get_h2h_fast <- function(winner_ids, loser_ids, match_dates) {
  n <- length(winner_ids)
  h2h_wins_w <- integer(n)
  h2h_wins_l <- integer(n)

  # Process in vectorized batches
  for (i in seq_len(n)) {
    if (i %% 100000 == 0) message("    H2H ", i, " / ", n)
    w <- winner_ids[i]
    l <- loser_ids[i]
    d <- as.Date(match_dates[i])

    # Winner beat loser before this date
    row_wl <- h2h_agg[.(w, l)]
    if (!is.na(row_wl$player_a[1])) {
      dates_wl <- row_wl$dates[[1]]
      h2h_wins_w[i] <- sum(dates_wl < d)
    }

    # Loser beat winner before this date
    row_lw <- h2h_agg[.(l, w)]
    if (!is.na(row_lw$player_a[1])) {
      dates_lw <- row_lw$dates[[1]]
      h2h_wins_l[i] <- sum(dates_lw < d)
    }
  }

  total <- h2h_wins_w + h2h_wins_l
  # Laplace-smoothed win proportion (add 1 win and 1 loss)
  h2h_prop <- (h2h_wins_w + 1) / (total + 2)

  list(h2h_wins = h2h_wins_w, h2h_total = total, h2h_prop = h2h_prop)
}

h2h_result <- get_h2h_fast(all_qual$winner_id, all_qual$loser_id, all_qual$match_date)
all_qual$h2h_win_prop <- h2h_result$h2h_prop
all_qual$h2h_count <- h2h_result$h2h_total

message("  H2H computed. Matches with prior H2H > 0: ",
        sum(all_qual$h2h_count > 0), " / ", nrow(all_qual))

rm(h2h_dt, h2h_agg, all_matches_dt, h2h_result)
gc()

slog("## Step 4: H2H computed")
slog("- Matches with prior H2H: ", sum(all_qual$h2h_count > 0))
slog("")


# ==============================================================================
# STEP 5: BUILD ESTIMATION DATASET FOR LOGIT
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 5: BUILD ESTIMATION DATASET")
message(strrep("=", 70))

# One row per match: randomly assign "focal player" perspective to avoid
# conditioning on the winner identity. This ensures proper logit estimation.
# With prob 0.5, focal = winner (won=1); with prob 0.5, focal = loser (won=0).
flip <- rbinom(nrow(all_qual), 1, 0.5) == 1L

est_data <- all_qual |>
  mutate(
    flip = flip,
    won = as.integer(!flip),  # won=1 if focal is winner, won=0 if focal is loser
    # Assign focal vs opponent
    focal_elo = ifelse(flip, loser_elo, winner_elo),
    opp_elo   = ifelse(flip, winner_elo, loser_elo),
    focal_surface_elo = ifelse(flip, loser_surface_elo, winner_surface_elo),
    opp_surface_elo   = ifelse(flip, winner_surface_elo, loser_surface_elo),
    focal_age = ifelse(flip, as.numeric(loser_age), as.numeric(winner_age)),
    opp_age   = ifelse(flip, as.numeric(winner_age), as.numeric(loser_age)),
    focal_ht  = ifelse(flip, as.numeric(loser_ht), as.numeric(winner_ht)),
    opp_ht    = ifelse(flip, as.numeric(winner_ht), as.numeric(loser_ht)),
    focal_hand = ifelse(flip, loser_hand, winner_hand),
    opp_hand   = ifelse(flip, winner_hand, loser_hand),
    # H2H: flip perspective when focal is the loser
    focal_h2h_win_prop = ifelse(flip, 1 - h2h_win_prop, h2h_win_prop)
  ) |>
  transmute(
    tourney_id, match_num, tour, surface, tourney_level, round, round_num,
    match_date, weight, won,
    elo_diff = (focal_elo - opp_elo) / 100,
    surface_elo_diff = (focal_surface_elo - opp_surface_elo) / 100,
    h2h_win_prop = focal_h2h_win_prop,
    h2h_count,
    age_diff = focal_age - opp_age,
    ht_diff = focal_ht - opp_ht,
    hand_mismatch = as.integer(
      (focal_hand == "R" & opp_hand == "L") |
      (focal_hand == "L" & opp_hand == "R")
    ),
    surface_clay = as.integer(surface == "Clay"),
    surface_grass = as.integer(surface == "Grass"),
    is_masters = as.integer(tourney_level == "M"),
    is_gs = as.integer(tourney_level == "G")
  )

# Handle NAs
est_data$ht_diff[is.na(est_data$ht_diff)] <- 0
est_data$hand_mismatch[is.na(est_data$hand_mismatch)] <- 0
est_data$age_diff[is.na(est_data$age_diff)] <- 0
est_data$h2h_count[is.na(est_data$h2h_count)] <- 0
est_data$h2h_win_prop[is.na(est_data$h2h_win_prop)] <- 0.5

# Drop rows where key variables are missing
valid <- !is.na(est_data$elo_diff) & !is.na(est_data$surface_elo_diff)
est_data <- est_data[valid, ]

message("  Estimation dataset: ", nrow(est_data), " matches")
message("  Won distribution: ", sum(est_data$won), " wins, ",
        sum(est_data$won == 0), " losses")

slog("## Step 5: Estimation dataset built")
slog("- Rows: ", nrow(est_data), " (from ", nrow(est_data)/2, " matches)")
slog("")


# ==============================================================================
# STEP 6: ESTIMATE WEIGHTED LOGIT
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 6: ESTIMATE WEIGHTED LOGIT")
message(strrep("=", 70))

# Model matching manuscript: Elo diff, surface Elo diff, H2H, age, height,
# handedness, tournament level, surface
win_model_v3 <- glm(
  won ~ elo_diff + surface_elo_diff + h2h_win_prop + h2h_count +
    age_diff + ht_diff + hand_mismatch +
    surface_clay + surface_grass + is_masters + is_gs,
  data = est_data,
  family = binomial(link = "logit"),
  weights = weight
)

message("\n  Model summary:")
print(summary(win_model_v3))

message("\n  N observations: ", nobs(win_model_v3))
message("  AIC: ", round(AIC(win_model_v3), 1))

slog("## Step 6: Weighted logit estimated")
slog("- N obs: ", nobs(win_model_v3))
slog("- AIC: ", round(AIC(win_model_v3), 1))
slog("- Coefficients:")
for (nm in names(coef(win_model_v3))) {
  slog("  ", nm, ": ", round(coef(win_model_v3)[nm], 6))
}
slog("")


# ==============================================================================
# STEP 7: PRODUCE SUMMARY TABLE
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 7: PRODUCE SUMMARY TABLE")
message(strrep("=", 70))

# Create publication-quality table using modelsummary
coef_map <- c(
  "elo_diff"         = "Elo difference / 100",
  "surface_elo_diff" = "Surface Elo difference / 100",
  "h2h_win_prop"     = "H2H win proportion (Laplace)",
  "h2h_count"        = "H2H encounter count",
  "age_diff"         = "Age difference (years)",
  "ht_diff"          = "Height difference (cm)",
  "hand_mismatch"    = "Handedness mismatch",
  "surface_clay"     = "Clay surface",
  "surface_grass"    = "Grass surface",
  "is_masters"       = "Masters 1000",
  "is_gs"            = "Grand Slam",
  "(Intercept)"      = "Intercept"
)

# Build table manually for maximum control and compatibility
sm <- summary(win_model_v3)
ct <- coef(sm)

coef_order <- c("elo_diff", "surface_elo_diff", "h2h_win_prop", "h2h_count",
                "age_diff", "ht_diff", "hand_mismatch",
                "surface_clay", "surface_grass", "is_masters", "is_gs",
                "(Intercept)")

lines <- character()
lines <- c(lines, "\\begin{tabular}{lc}")
lines <- c(lines, "\\toprule")
lines <- c(lines, " & Match Win (Logit) \\\\")
lines <- c(lines, "\\midrule")

for (v in coef_order) {
  lab <- coef_map[v]
  est <- ct[v, "Estimate"]
  se  <- ct[v, "Std. Error"]
  pv  <- ct[v, "Pr(>|z|)"]
  stars <- add_stars(pv)
  lines <- c(lines, paste0(lab, " & ", fmt(est, 4), stars, " \\\\"))
  lines <- c(lines, paste0(" & (", fmt(se, 4), ") \\\\"))
}

lines <- c(lines, "\\midrule")
lines <- c(lines, paste0("Observations & ", format(nobs(win_model_v3), big.mark = ","), " \\\\"))
lines <- c(lines, paste0("AIC & ", format(round(AIC(win_model_v3), 0), big.mark = ","), " \\\\"))
lines <- c(lines, paste0("Log-likelihood & ", format(round(logLik(win_model_v3), 0), big.mark = ","), " \\\\"))
lines <- c(lines, "\\bottomrule")
lines <- c(lines, "\\end{tabular}")

table_tex <- paste(lines, collapse = "\n")
writeLines(table_tex, file.path(TABLES_DIR, "table_win_model.tex"))
message("  Saved: Tables/table_win_model.tex")

slog("## Step 7: Summary table saved")
slog("")


# ==============================================================================
# STEP 8: SAVE NEW MODEL
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 8: SAVE NEW MODEL")
message(strrep("=", 70))

saveRDS(win_model_v3, file.path(CLEANED_DIR, "win_model_v3.rds"))
message("  Saved: Data/cleaned/win_model_v3.rds")


# ==============================================================================
# STEP 9: RE-COMPUTE P_i^{LL} WITH NEW MODEL
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 9: RE-COMPUTE P_i^{LL} WITH NEW MODEL")
message(strrep("=", 70))

# Re-load qualifying data for the estimation events
atp_qual <- readRDS(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
wta_qual <- readRDS(file.path(RAW_DIR, "wta_qual_itf_matches.rds"))

est_tids_atp <- unique(nongs_est$tourney_id[nongs_est$tour == "ATP"])
est_tids_wta <- unique(nongs_est$tourney_id[nongs_est$tour == "WTA"])

qual_rounds_all <- c("Q1", "Q2", "Q3", "Q4", "Q5", "QF")

atp_q <- atp_qual |>
  filter(tourney_id %in% est_tids_atp, round %in% qual_rounds_all) |>
  mutate(tour = "ATP")
wta_q <- wta_qual |>
  filter(tourney_id %in% est_tids_wta, round %in% qual_rounds_all) |>
  mutate(tour = "WTA")
rm(atp_qual, wta_qual)

all_qual_est <- bind_rows(atp_q, wta_q)
rm(atp_q, wta_q)

# Identify final qualifying round per event
final_round_map <- all_qual_est |>
  group_by(tourney_id) |>
  summarise(final_round = max(round), .groups = "drop")

final_qual <- all_qual_est |>
  inner_join(final_round_map, by = "tourney_id") |>
  filter(round == final_round)

message("  Final qualifying round matches at estimation events: ", nrow(final_qual))

# Merge Elo for final qualifying round matches
final_qual$match_date <- as.Date(final_qual$tourney_date)
final_qual$winner_elo <- get_pre_match_elo(final_qual$winner_id, final_qual$match_date)
final_qual$loser_elo <- get_pre_match_elo(final_qual$loser_id, final_qual$match_date)
final_qual$winner_elo[is.na(final_qual$winner_elo)] <- 1500
final_qual$loser_elo[is.na(final_qual$loser_elo)] <- 1500

# Surface Elo
message("  Merging surface Elo for final round matches...")
final_qual$winner_surface_elo <- get_surface_elo(
  final_qual$winner_id, final_qual$match_date,
  final_qual$surface, final_qual$tour
)
final_qual$loser_surface_elo <- get_surface_elo(
  final_qual$loser_id, final_qual$match_date,
  final_qual$surface, final_qual$tour
)
final_qual$winner_surface_elo[is.na(final_qual$winner_surface_elo)] <-
  final_qual$winner_elo[is.na(final_qual$winner_surface_elo)]
final_qual$loser_surface_elo[is.na(final_qual$loser_surface_elo)] <-
  final_qual$loser_elo[is.na(final_qual$loser_surface_elo)]

# H2H for final round matches
message("  Computing H2H for final round matches...")
# Reload H2H aggregation data
atp_all <- readRDS(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
wta_all <- readRDS(file.path(RAW_DIR, "wta_qual_itf_matches.rds"))
all_m <- rbind(
  atp_all[, c("winner_id", "loser_id", "tourney_date")],
  wta_all[, c("winner_id", "loser_id", "tourney_date")]
)
rm(atp_all, wta_all)
all_m$match_date <- as.Date(all_m$tourney_date)
h2h_dt <- as.data.table(all_m[, c("winner_id", "loser_id", "match_date")])
setnames(h2h_dt, c("player_a", "player_b", "match_date"))
setkey(h2h_dt, player_a, player_b)
h2h_agg <- h2h_dt[, .(dates = list(match_date)), by = .(player_a, player_b)]
setkey(h2h_agg, player_a, player_b)
rm(all_m, h2h_dt)

h2h_res <- get_h2h_fast(final_qual$winner_id, final_qual$loser_id, final_qual$match_date)
final_qual$h2h_win_prop <- h2h_res$h2h_prop
final_qual$h2h_count <- h2h_res$h2h_total
rm(h2h_res, h2h_agg)
gc()

# Build prediction features from WINNER's perspective (P(winner beats loser))
match_pred <- final_qual |>
  transmute(
    tourney_id, match_num,
    winner_id, loser_id,
    winner_rank = as.numeric(winner_rank),
    loser_rank = as.numeric(loser_rank),
    elo_diff = (winner_elo - loser_elo) / 100,
    surface_elo_diff = (winner_surface_elo - loser_surface_elo) / 100,
    h2h_win_prop,
    h2h_count = ifelse(is.na(h2h_count), 0, h2h_count),
    age_diff = as.numeric(winner_age) - as.numeric(loser_age),
    ht_diff = as.numeric(winner_ht) - as.numeric(loser_ht),
    hand_mismatch = as.integer(
      (winner_hand == "R" & loser_hand == "L") |
      (winner_hand == "L" & loser_hand == "R")
    ),
    surface_clay = as.integer(surface == "Clay"),
    surface_grass = as.integer(surface == "Grass"),
    is_masters = as.integer(tourney_level == "M"),
    is_gs = as.integer(tourney_level == "G"),
    tour
  )

# Handle NAs
match_pred$ht_diff[is.na(match_pred$ht_diff)] <- 0
match_pred$hand_mismatch[is.na(match_pred$hand_mismatch)] <- 0
match_pred$age_diff[is.na(match_pred$age_diff)] <- 0
match_pred$h2h_win_prop[is.na(match_pred$h2h_win_prop)] <- 0.5

# Predict P(winner beats loser) from new model
match_pred$p_winner_wins <- predict(win_model_v3, newdata = match_pred, type = "response")

message("  Win probability summary (from new model):")
print(summary(match_pred$p_winner_wins))


# ==============================================================================
# STEP 10: EXACT ENUMERATION OF P_i^{LL}
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 10: EXACT ENUMERATION OF P_i^{LL}")
message(strrep("=", 70))

# Reuse the compute_p_ll_event function from script 33
compute_p_ll_event <- function(matches_df, losers_df, c_slots) {
  n_matches <- nrow(matches_df)
  if (n_matches == 0 || c_slots == 0) {
    return(data.frame(player_id = losers_df$player_id, p_ll = NA_real_,
                      computation_method = "none", stringsAsFactors = FALSE))
  }

  results <- data.frame(
    player_id = losers_df$player_id,
    p_ll = NA_real_,
    computation_method = NA_character_,
    stringsAsFactors = FALSE
  )

  m_player_a <- matches_df$player_a
  m_player_b <- matches_df$player_b
  m_rank_a   <- matches_df$rank_a
  m_rank_b   <- matches_df$rank_b
  m_p_a_wins <- matches_df$p_a_wins

  for (idx in seq_len(nrow(losers_df))) {
    pid <- losers_df$player_id[idx]
    prank <- losers_df$player_rank[idx]
    if (is.na(prank)) prank <- 9999

    match_idx <- which(m_player_a == pid | m_player_b == pid)
    if (length(match_idx) == 0) {
      results$p_ll[idx] <- NA_real_
      results$computation_method[idx] <- "not_found"
      next
    }

    mi <- match_idx[1]
    other_idx <- setdiff(seq_len(n_matches), mi)
    n_other <- length(other_idx)

    if (n_other == 0) {
      results$p_ll[idx] <- if (c_slots >= 1) 1.0 else 0.0
      results$computation_method[idx] <- "trivial"
      next
    }

    o_p_a   <- m_p_a_wins[other_idx]
    o_rank_a <- m_rank_a[other_idx]
    o_rank_b <- m_rank_b[other_idx]
    o_rank_a[is.na(o_rank_a)] <- 9999
    o_rank_b[is.na(o_rank_b)] <- 9999

    use_mc <- (n_other > 16)

    if (!use_mc) {
      n_outcomes <- 2^n_other
      k_seq <- 0:(n_outcomes - 1)
      bit_matrix <- matrix(0L, nrow = n_outcomes, ncol = n_other)
      for (j in seq_len(n_other)) {
        bit_matrix[, j] <- as.integer(bitwAnd(bitwShiftR(k_seq, j - 1L), 1L))
      }

      log_pa <- log(o_p_a)
      log_1mpa <- log(1 - o_p_a)
      log_probs <- bit_matrix %*% log_pa + (1 - bit_matrix) %*% log_1mpa
      probs <- exp(log_probs)

      loser_rank_matrix <- bit_matrix * rep(o_rank_b, each = n_outcomes) +
                           (1 - bit_matrix) * rep(o_rank_a, each = n_outcomes)
      n_better_matrix <- rowSums(loser_rank_matrix < prank)
      pos_i <- n_better_matrix + 1L
      results$p_ll[idx] <- sum(probs[pos_i <= c_slots])
      results$computation_method[idx] <- "exact"
    } else {
      n_sims <- 100000
      U <- matrix(runif(n_sims * n_other), nrow = n_sims, ncol = n_other)
      bit_matrix <- U < rep(o_p_a, each = n_sims)
      loser_rank_matrix <- bit_matrix * rep(o_rank_b, each = n_sims) +
                           (!bit_matrix) * rep(o_rank_a, each = n_sims)
      n_better <- rowSums(loser_rank_matrix < prank)
      pos_i <- n_better + 1L
      results$p_ll[idx] <- mean(pos_i <= c_slots)
      results$computation_method[idx] <- "montecarlo"
    }
  }
  return(results)
}

# Build event-level match structure
event_matches <- match_pred |>
  transmute(
    tourney_id,
    match_id = paste0(tourney_id, "_", match_num),
    player_a = winner_id,
    player_b = loser_id,
    rank_a = winner_rank,
    rank_b = loser_rank,
    p_a_wins = p_winner_wins
  )

# Get LL slots per event
ll_slots <- nongs_est |>
  group_by(tourney_id) |>
  summarise(n_ll_slots = n_ll_slots[1], .groups = "drop")

# Estimation sample players
est_players <- nongs_est |>
  select(tourney_id, player_id, player_rank) |>
  distinct()

event_ids <- unique(event_matches$tourney_id)
n_events <- length(event_ids)
message("  Events to process: ", n_events)

p_ll_results <- list()
counter <- 0
n_exact <- 0; n_mc <- 0; n_not_found <- 0; n_trivial <- 0

for (tid in event_ids) {
  counter <- counter + 1
  if (counter %% 50 == 0) message("    Event ", counter, " / ", n_events)

  evt_matches <- event_matches |> filter(tourney_id == tid)
  evt_losers <- est_players |> filter(tourney_id == tid)
  c_t <- ll_slots$n_ll_slots[ll_slots$tourney_id == tid]
  if (length(c_t) == 0 || is.na(c_t) || c_t == 0) next

  result <- compute_p_ll_event(evt_matches, evt_losers, c_t)
  result$tourney_id <- tid
  result$n_matches_at_event <- nrow(evt_matches)
  p_ll_results[[counter]] <- result

  methods <- result$computation_method
  n_exact <- n_exact + sum(methods == "exact", na.rm = TRUE)
  n_mc <- n_mc + sum(methods == "montecarlo", na.rm = TRUE)
  n_not_found <- n_not_found + sum(methods == "not_found", na.rm = TRUE)
  n_trivial <- n_trivial + sum(methods == "trivial", na.rm = TRUE)
}

p_ll_all <- bind_rows(p_ll_results)

message("\n  P_i^{LL} computation (new model):")
message("  Total: ", nrow(p_ll_all))
message("  Exact: ", n_exact, ", MC: ", n_mc, ", Not found: ", n_not_found,
        ", Trivial: ", n_trivial)
message("  P_i^{LL} summary:")
print(summary(p_ll_all$p_ll))

saveRDS(p_ll_all, file.path(CLEANED_DIR, "p_ll_corrected_v2.rds"))
message("  Saved: Data/cleaned/p_ll_corrected_v2.rds")

slog("## Step 10: P_i^{LL} computed with new model")
slog("- Total: ", nrow(p_ll_all))
slog("- Exact: ", n_exact, ", MC: ", n_mc)
slog("- Mean P_i^{LL}: ", round(mean(p_ll_all$p_ll, na.rm = TRUE), 4))
slog("")


# ==============================================================================
# STEP 11: MERGE INTO ESTIMATION DATA AND RECOMPUTE RESIDUALS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 11: MERGE AND RECOMPUTE RESIDUALS")
message(strrep("=", 70))

p_ll_merge <- p_ll_all |>
  select(tourney_id, player_id, p_ll, computation_method) |>
  distinct(tourney_id, player_id, .keep_all = TRUE)

nongs_est <- nongs_est |>
  left_join(p_ll_merge, by = c("tourney_id", "player_id"))

n_replaced <- sum(!is.na(nongs_est$p_ll))
n_kept_old <- sum(is.na(nongs_est$p_ll))
message("  Replaced: ", n_replaced, ", Kept old: ", n_kept_old)

nongs_est$peer_component <- ifelse(!is.na(nongs_est$p_ll),
                                   nongs_est$p_ll,
                                   nongs_est$peer_component_old)

# Generalized residual
compute_gen_residual <- function(D, P) {
  P <- pmax(pmin(P, 0.9999), 0.0001)
  probit_P <- qnorm(P)
  phi_val  <- dnorm(probit_P)
  D * phi_val / P - (1 - D) * phi_val / (1 - P)
}

nongs_est$v_hat <- compute_gen_residual(nongs_est$got_ll, nongs_est$peer_component)

# Compare old vs new
compare <- nongs_est |> filter(!is.na(peer_component_old), !is.na(p_ll))
if (nrow(compare) > 0) {
  corr <- cor(compare$peer_component_old, compare$p_ll, use = "complete.obs")
  message("  Old vs new P_i^{LL} correlation: ", round(corr, 4))
  message("  Old mean: ", round(mean(compare$peer_component_old), 4),
          "  New mean: ", round(mean(compare$p_ll, na.rm = TRUE), 4))
}

# Clean up and save
nongs_est$p_ll <- NULL
nongs_est$computation_method <- NULL

saveRDS(nongs_est, file.path(CLEANED_DIR, "skeleton_nongs_est_v5.rds"))
message("  Saved: Data/cleaned/skeleton_nongs_est_v5.rds")

slog("## Step 11: Estimation data updated")
slog("- Replaced: ", n_replaced, " observations")
slog("")


# ==============================================================================
# STEP 12: RE-ESTIMATE NON-GS STACKED DYNAMIC TABLES WITH CF
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 12: RE-ESTIMATE NON-GS DYNAMICS")
message(strrep("=", 70))

stack_horizons_v2 <- function(data, outcomes_base, horizons = c(4, 8, 12, 26, 52)) {
  stacked <- list()
  for (h in horizons) {
    h_label <- paste0(h, "w")
    row_data <- data |>
      dplyr::transmute(
        player_id, tourney_id, tour, slam_year, got_ll,
        pre_rank_pts, pre_rank_pts_sq, player_age,
        pre_elo, pre_elo_sq,
        n_prior_gs_ll_won, n_prior_gs_ll_notwon,
        n_prior_nongs_ll_won, n_prior_nongs_ll_notwon,
        had_prior_ll = if ("had_prior_ll" %in% names(data)) had_prior_ll else NA_integer_,
        md_matches_won = if ("md_matches_won" %in% names(data)) md_matches_won else NA_integer_,
        peer_component = if ("peer_component" %in% names(data)) peer_component else NA_real_,
        v_hat = if ("v_hat" %in% names(data)) v_hat else NA_real_,
        year = if ("year" %in% names(data)) year else NA_integer_,
        horizon = h_label,
        horizon_num = h
      )
    for (ob in outcomes_base) {
      col_name <- paste0(ob, "_", h, "w")
      if (col_name %in% names(data)) {
        row_data[[ob]] <- data[[col_name]]
      } else {
        row_data[[ob]] <- NA_real_
      }
    }
    stacked[[h_label]] <- row_data
  }
  dplyr::bind_rows(stacked) |>
    dplyr::mutate(horizon = factor(horizon, levels = paste0(horizons, "w")))
}

nongs_atp <- nongs_est |> filter(tour == "ATP", !is.na(peer_component))
nongs_wta <- nongs_est |> filter(tour == "WTA", !is.na(peer_component))
message("  Non-GS ATP with CF: N = ", nrow(nongs_atp))
message("  Non-GS WTA with CF: N = ", nrow(nongs_wta))

stacked_nongs_atp <- stack_horizons_v2(nongs_atp, outcomes_base)
stacked_nongs_wta <- stack_horizons_v2(nongs_wta, outcomes_base)

# CF estimation function
run_stacked_nongs_cf <- function(stacked_data, tour_label) {
  results <- list()
  model_objects <- list()
  rho_results <- list()

  for (ob in outcomes_base) {
    if (!ob %in% names(stacked_data)) next

    sdata <- stacked_data |>
      filter(!is.na(.data[[ob]]),
             !is.na(pre_rank_pts), !is.na(player_age),
             !is.na(pre_elo), !is.na(v_hat))
    if (nrow(sdata) < 50) next

    fml <- as.formula(paste0(
      ob, " ~ got_ll:horizon + v_hat:horizon + ", ZPRE_FULL,
      " | tourney_id + horizon"
    ))

    mod <- tryCatch(
      feols(fml, data = sdata, cluster = ~player_id),
      error = function(e) NULL
    )

    if (is.null(mod)) next
    model_objects[[ob]] <- mod

    ct <- as.data.frame(coeftable(mod))
    ct$var <- rownames(ct)

    for (h in HORIZON_LABS) {
      treat_name <- paste0("got_ll:horizon", h)
      rho_name   <- paste0("v_hat:horizon", h)

      if (treat_name %in% ct$var) {
        row_t <- ct[ct$var == treat_name, ]
        results[[paste0(ob, "_", h)]] <- data.frame(
          tour = tour_label, outcome = ob, horizon = h,
          coef = row_t$Estimate, se = row_t$`Std. Error`,
          pval = row_t$`Pr(>|t|)`, stringsAsFactors = FALSE
        )
      }

      if (rho_name %in% ct$var) {
        row_r <- ct[ct$var == rho_name, ]
        rho_results[[paste0(ob, "_", h)]] <- data.frame(
          tour = tour_label, outcome = ob, horizon = h,
          rho = row_r$Estimate, rho_se = row_r$`Std. Error`,
          rho_pval = row_r$`Pr(>|t|)`, stringsAsFactors = FALSE
        )
      }
    }
  }

  list(results = bind_rows(results),
       rho_results = bind_rows(rho_results),
       models = model_objects)
}

atp_cf <- run_stacked_nongs_cf(stacked_nongs_atp, "ATP")
wta_cf <- run_stacked_nongs_cf(stacked_nongs_wta, "WTA")

message("\n  === ATP Non-GS CF Results (New Model) ===")
if (nrow(atp_cf$results) > 0) {
  for (i in seq_len(nrow(atp_cf$results))) {
    r <- atp_cf$results[i, ]
    stars <- ifelse(r$pval < 0.01, "***", ifelse(r$pval < 0.05, "**", ifelse(r$pval < 0.1, "*", "")))
    message(sprintf("  %s @ %s: %.1f%s (SE=%.1f, p=%.3f)",
                    r$outcome, r$horizon, r$coef, stars, r$se, r$pval))
  }
}

message("\n  === WTA Non-GS CF Results (New Model) ===")
if (nrow(wta_cf$results) > 0) {
  for (i in seq_len(nrow(wta_cf$results))) {
    r <- wta_cf$results[i, ]
    stars <- ifelse(r$pval < 0.01, "***", ifelse(r$pval < 0.05, "**", ifelse(r$pval < 0.1, "*", "")))
    message(sprintf("  %s @ %s: %.1f%s (SE=%.1f, p=%.3f)",
                    r$outcome, r$horizon, r$coef, stars, r$se, r$pval))
  }
}


# ==============================================================================
# STEP 13: GENERATE LATEX TABLES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 13: GENERATE LATEX TABLES")
message(strrep("=", 70))

generate_nongs_table <- function(cf_results, rho_results_df, tour_label) {
  outcomes_order <- c("points_change", "elo_change", "n_main_draws", "n_matches_250plus")
  out_labels <- c(
    "points_change"     = "Ranking Pts $\\Delta$",
    "elo_change"        = "Elo $\\Delta$",
    "n_main_draws"      = "Main Draws",
    "n_matches_250plus" = "Matches 250+"
  )

  lines <- character()
  lines <- c(lines, "\\begin{tabular}{l*{5}{c}}")
  lines <- c(lines, "\\toprule")
  lines <- c(lines, paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"))
  lines <- c(lines, "\\midrule")

  for (ob in outcomes_order) {
    ob_res <- cf_results |> filter(outcome == ob)
    if (nrow(ob_res) == 0) next

    coef_vals <- se_vals <- character()
    for (h in HORIZON_LABS) {
      r <- ob_res |> filter(horizon == h)
      if (nrow(r) == 0) {
        coef_vals <- c(coef_vals, "")
        se_vals <- c(se_vals, "")
      } else {
        stars <- add_stars(r$pval[1])
        coef_vals <- c(coef_vals, paste0(fmt(r$coef[1], 1), stars))
        se_vals <- c(se_vals, paste0("(", fmt(r$se[1], 1), ")"))
      }
    }

    lines <- c(lines, paste0(out_labels[ob], " & ", paste(coef_vals, collapse = " & "), " \\\\"))
    lines <- c(lines, paste0(" & ", paste(se_vals, collapse = " & "), " \\\\[0.3em]"))
  }

  # Rho row
  lines <- c(lines, "\\midrule")
  rho_vals <- character()
  for (h in HORIZON_LABS) {
    rho_row <- rho_results_df |> dplyr::filter(outcome == "points_change", horizon == h)
    if (nrow(rho_row) == 0) {
      rho_vals <- c(rho_vals, "")
    } else {
      stars <- add_stars(rho_row$rho_pval[1])
      rho_vals <- c(rho_vals, paste0(fmt(rho_row$rho[1], 2), stars))
    }
  }
  lines <- c(lines, paste0("$\\hat{\\rho}$ (Pts) & ", paste(rho_vals, collapse = " & "), " \\\\"))

  lines <- c(lines, "\\bottomrule")
  lines <- c(lines, "\\end{tabular}")

  paste(lines, collapse = "\n")
}

atp_table <- generate_nongs_table(atp_cf$results, atp_cf$rho_results, "ATP")
wta_table <- generate_nongs_table(wta_cf$results, wta_cf$rho_results, "WTA")

writeLines(atp_table, file.path(TABLES_DIR, "table_dynamic_stacked_nongs_atp.tex"))
writeLines(wta_table, file.path(TABLES_DIR, "table_dynamic_stacked_nongs_wta.tex"))
message("  Saved ATP and WTA non-GS tables")

slog("## Step 13: Tables generated")
slog("")


# ==============================================================================
# STEP 14: SUMMARY
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 14: SUMMARY")
message(strrep("=", 70))

message("\n  === Win Model v3 (Weighted, All Qualifying Rounds) ===")
message("  Observations: ", nobs(win_model_v3))
message("  Covariates: elo_diff, surface_elo_diff, h2h_win_prop, h2h_count,")
message("    age_diff, ht_diff, hand_mismatch, surface_clay, surface_grass,")
message("    is_masters, is_gs")
message("  Weights: total points in play (fallback: qualifying round number)")
message("")
message("  Key coefficients:")
message("    Elo diff / 100:         ", round(coef(win_model_v3)["elo_diff"], 4))
message("    Surface Elo diff / 100: ", round(coef(win_model_v3)["surface_elo_diff"], 4))
message("    H2H win proportion:     ", round(coef(win_model_v3)["h2h_win_prop"], 4))

message("\n  === P_i^{LL} (New Model) ===")
message("  Mean: ", round(mean(p_ll_all$p_ll, na.rm = TRUE), 4))
message("  Median: ", round(median(p_ll_all$p_ll, na.rm = TRUE), 4))

# Save summary log
writeLines(summary_log, file.path(OUTPUT_DIR, "35_win_model_reestimate_log.md"))
message("\n  Log saved: Output/35_win_model_reestimate_log.md")

message("\n", strrep("=", 70))
message("DONE: Script 35 complete")
message(strrep("=", 70))
