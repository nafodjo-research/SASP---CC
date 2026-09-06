# ==============================================================================
# 33_fix_p_ll.R
# CRITICAL FIX: Rebuild the selection probability P_i^{LL} correctly from
# full match pairings at each non-GS event.
#
# The OLD computation (scripts 17/18/30) used each loser's OWN loss probability
# and a Bernoulli convolution over ALL higher-ranked losers. This is WRONG
# because it conditions only on whether higher-ranked players lose, but does not
# account for WHICH players lose -- the loser set depends on the full
# combination of match outcomes.
#
# The CORRECT computation:
#   For each non-GS event with n final qualifying round matches m_1,...,m_n:
#   - Each match m_j involves players a_j and b_j
#   - P(a_j beats b_j) from a match-level logit
#   - There are 2^{n-1} possible outcomes for the n-1 matches excluding i's
#   - For each outcome: determine which players lose, rank losers by ATP ranking,
#     check if player i is within the top c
#   - P_i^{LL} = sum of probs of outcomes where i is in the top c losers
#
# Inputs:
#   Data/raw/atp_qual_chall_matches.rds
#   Data/raw/wta_qual_itf_matches.rds
#   Data/cleaned/skeleton_nongs_est_v2.rds
#   Data/cleaned/win_model_v2.rds
#
# Outputs:
#   Data/cleaned/p_ll_corrected.rds
#   Data/cleaned/skeleton_nongs_est_v4.rds
#   Tables/table_dynamic_stacked_nongs_atp.tex (re-estimated with corrected P_i^{LL})
#   Tables/table_dynamic_stacked_nongs_wta.tex
#   Output/p_ll_correction_summary.md
#
# Dependencies: dplyr, tidyr, fixest, ggplot2, here, stringr
# ==============================================================================

set.seed(20260327)

# --- Packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(fixest)
library(ggplot2)
library(here)
library(stringr)

# --- Shared helpers ----------------------------------------------------------
source(here("scripts", "R", "utils.R"))
summary_log <- character()

# --- Paths -------------------------------------------------------------------
RAW_DIR     <- here("Data", "raw")
CLEANED_DIR <- here("Data", "cleaned")
TABLES_DIR  <- here("Tables")
FIGURES_DIR <- here("Figures")
OUTPUT_DIR  <- here("Output")
for (d in c(CLEANED_DIR, TABLES_DIR, FIGURES_DIR, OUTPUT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# --- Outcome definitions (from script 30) ------------------------------------
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
# STEP 1: LOAD DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 1: LOADING DATA")
message(strrep("=", 70))

nongs_est <- readRDS(file.path(CLEANED_DIR, "skeleton_nongs_est_v2.rds"))
message("  Non-GS estimation sample: N = ", nrow(nongs_est), " rows, ",
        length(unique(nongs_est$tourney_id)), " events")

# Save old peer_component for comparison
nongs_est$peer_component_old <- nongs_est$peer_component

# Load raw qualifying data
atp_qual <- readRDS(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
wta_qual <- readRDS(file.path(RAW_DIR, "wta_qual_itf_matches.rds"))
message("  ATP qualifying matches: ", nrow(atp_qual))
message("  WTA qualifying matches: ", nrow(wta_qual))

# Load win model
win_model <- readRDS(file.path(CLEANED_DIR, "win_model_v2.rds"))
message("  Win model loaded: ", length(coef(win_model)), " coefficients")
message("  Win model formula: ", deparse(formula(win_model)))

slog("## Step 1: Data loaded")
slog("- Non-GS estimation sample: N = ", nrow(nongs_est))
slog("- ATP qualifying matches: ", nrow(atp_qual))
slog("- WTA qualifying matches: ", nrow(wta_qual))
slog("")


# ==============================================================================
# STEP 2: IDENTIFY FINAL QUALIFYING ROUND MATCHES FOR EACH EVENT
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 2: IDENTIFY FINAL QUALIFYING ROUND MATCHES")
message(strrep("=", 70))

# Get unique tourney_ids from estimation sample, split by tour
est_tids_atp <- unique(nongs_est$tourney_id[nongs_est$tour == "ATP"])
est_tids_wta <- unique(nongs_est$tourney_id[nongs_est$tour == "WTA"])
message("  ATP events in estimation sample: ", length(est_tids_atp))
message("  WTA events in estimation sample: ", length(est_tids_wta))

# Filter qualifying data to relevant events and qualifying rounds only
qual_rounds <- c("Q1", "Q2", "Q3", "Q4", "Q5")

atp_q <- atp_qual |>
  filter(tourney_id %in% est_tids_atp, round %in% qual_rounds) |>
  mutate(tour = "ATP")

wta_q <- wta_qual |>
  filter(tourney_id %in% est_tids_wta, round %in% qual_rounds) |>
  mutate(tour = "WTA")

message("  ATP qualifying matches at estimation events: ", nrow(atp_q))
message("  WTA qualifying matches at estimation events: ", nrow(wta_q))

# Free memory
rm(atp_qual, wta_qual)
gc()

# Combine
all_qual <- bind_rows(atp_q, wta_q)
rm(atp_q, wta_q)
gc()

# For each event, identify the final qualifying round (max Q round)
final_round_map <- all_qual |>
  group_by(tourney_id) |>
  summarise(final_round = max(round), .groups = "drop")

message("  Events with qualifying data: ", nrow(final_round_map))
message("  Final round distribution:")
print(table(final_round_map$final_round))

# Keep only final qualifying round matches
final_qual <- all_qual |>
  inner_join(final_round_map, by = "tourney_id") |>
  filter(round == final_round)

message("  Final qualifying round matches: ", nrow(final_qual))

# Check how many estimation events have qualifying data
est_all_tids <- unique(nongs_est$tourney_id)
n_with_qual <- sum(est_all_tids %in% final_round_map$tourney_id)
n_without_qual <- sum(!(est_all_tids %in% final_round_map$tourney_id))
message("  Estimation events WITH qualifying data: ", n_with_qual)
message("  Estimation events WITHOUT qualifying data: ", n_without_qual)

slog("## Step 2: Final qualifying round matches identified")
slog("- Events with qualifying data: ", nrow(final_round_map))
slog("- Final qualifying round matches: ", nrow(final_qual))
slog("- Events without qualifying data: ", n_without_qual)
slog("")


# ==============================================================================
# STEP 3: BUILD MATCH-LEVEL WIN PROBABILITIES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 3: BUILD MATCH-LEVEL WIN PROBABILITIES")
message(strrep("=", 70))

# For each final qualifying round match, predict P(winner beats loser)
# using the win model. The model uses:
#   log_rank_ratio, I(log_rank_ratio^2), rank_diff, same_ioc,
#   surface_clay, surface_grass, age_diff, player_win_rate,
#   opponent_win_rate, h2h_win_rate, has_h2h, is_gs, is_masters,
#   is_qual, ht_diff, hand_mismatch

# Prepare match-level features from the perspective of the winner
match_features <- final_qual |>
  transmute(
    tourney_id,
    match_num,
    winner_id,
    loser_id,
    winner_rank = as.numeric(winner_rank),
    loser_rank = as.numeric(loser_rank),
    winner_age = as.numeric(winner_age),
    loser_age = as.numeric(loser_age),
    winner_ioc,
    loser_ioc,
    winner_ht = as.numeric(winner_ht),
    loser_ht = as.numeric(loser_ht),
    winner_hand,
    loser_hand,
    surface,
    tourney_level,
    tour,
    # Compute features from winner's perspective
    log_rank_ratio = log(pmax(loser_rank, 1) / pmax(winner_rank, 1)),
    rank_diff = loser_rank - winner_rank,
    same_ioc = as.integer(winner_ioc == loser_ioc),
    age_diff = winner_age - loser_age,
    surface_clay = as.integer(surface == "Clay"),
    surface_grass = as.integer(surface == "Grass"),
    ht_diff = winner_ht - loser_ht,
    hand_mismatch = as.integer(
      (winner_hand == "R" & loser_hand == "L") |
      (winner_hand == "L" & loser_hand == "R")
    ),
    # These are harder to compute from qualifying data alone;
    # set to neutral values where unavailable
    player_win_rate = 0.5,
    opponent_win_rate = 0.5,
    h2h_win_rate = 0.5,
    has_h2h = 0L,
    is_gs = 0L,
    is_masters = as.integer(tourney_level == "M"),
    is_qual = 1L
  )

# Handle NAs in key variables
has_valid <- !is.na(match_features$log_rank_ratio) &
             !is.na(match_features$rank_diff)

message("  Matches with valid rank data: ", sum(has_valid), " / ", nrow(match_features))

# Default probability for matches without sufficient data
match_features$p_winner_wins <- 0.5

# Predict using win model (from winner's perspective, so output is P(winner beats loser))
if (sum(has_valid) > 0) {
  # Handle NA in auxiliary features
  match_features$ht_diff[is.na(match_features$ht_diff)] <- 0
  match_features$hand_mismatch[is.na(match_features$hand_mismatch)] <- 0
  match_features$same_ioc[is.na(match_features$same_ioc)] <- 0
  match_features$age_diff[is.na(match_features$age_diff)] <- 0

  has_valid <- !is.na(match_features$log_rank_ratio) &
               !is.na(match_features$rank_diff)

  match_features$p_winner_wins[has_valid] <- predict(
    win_model,
    newdata = match_features[has_valid, ],
    type = "response"
  )
}

message("  Win probability summary:")
print(summary(match_features$p_winner_wins))

slog("## Step 3: Win probabilities computed")
slog("- Valid predictions: ", sum(has_valid), " / ", nrow(match_features))
slog("- Mean P(winner wins): ", round(mean(match_features$p_winner_wins), 3))
slog("")


# ==============================================================================
# STEP 4: COMPUTE CORRECTED P_i^{LL} FOR EACH LOSER AT EACH EVENT
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 4: COMPUTE CORRECTED P_i^{LL}")
message(strrep("=", 70))

# Get n_ll_slots per event from estimation data
ll_slots <- nongs_est |>
  group_by(tourney_id) |>
  summarise(n_ll_slots = n_ll_slots[1], .groups = "drop")

# Build the event-level match structure:
# For each event, we need the list of matches (player_a, player_b, rank_a, rank_b, p_a_wins)
# player_a is the "higher-seeded" / "first-listed" player, player_b the other

event_matches <- match_features |>
  transmute(
    tourney_id,
    match_id = paste0(tourney_id, "_", match_num),
    # We store both players; winner_id won in reality but we need to consider
    # all possible outcomes
    player_a = winner_id,
    player_b = loser_id,
    rank_a = winner_rank,
    rank_b = loser_rank,
    # P(a beats b) = p_winner_wins (since a is the winner in actuality,
    # we predicted from their perspective)
    p_a_wins = p_winner_wins
  )

# Get unique events with matches
event_ids <- unique(event_matches$tourney_id)
n_events <- length(event_ids)
message("  Events with final qualifying round matches: ", n_events)

# For each event, compute P_i^{LL} for every loser in our estimation sample
# A loser at the event is someone who appears in the estimation sample for that event

# Build a lookup: for each event, which players are in our estimation sample?
est_players <- nongs_est |>
  select(tourney_id, player_id, player_rank) |>
  distinct()

# Core computation function -- VECTORIZED for speed
compute_p_ll_event <- function(matches_df, losers_df, c_slots) {
  # matches_df: data frame with columns player_a, player_b, rank_a, rank_b, p_a_wins
  # losers_df: data frame of players in estimation sample at this event
  #            (player_id, player_rank)
  # c_slots: number of LL slots

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

  # Pre-extract vectors for speed
  m_player_a <- matches_df$player_a
  m_player_b <- matches_df$player_b
  m_rank_a   <- matches_df$rank_a
  m_rank_b   <- matches_df$rank_b
  m_p_a_wins <- matches_df$p_a_wins

  for (idx in seq_len(nrow(losers_df))) {
    pid <- losers_df$player_id[idx]
    prank <- losers_df$player_rank[idx]
    if (is.na(prank)) prank <- 9999

    # Find which match this player was in
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

    # Extract other match data as vectors
    o_p_a   <- m_p_a_wins[other_idx]
    o_rank_a <- m_rank_a[other_idx]
    o_rank_b <- m_rank_b[other_idx]
    o_rank_a[is.na(o_rank_a)] <- 9999
    o_rank_b[is.na(o_rank_b)] <- 9999

    use_mc <- (n_other > 16)

    if (!use_mc) {
      # EXACT enumeration using VECTORIZED matrix approach
      n_outcomes <- 2^n_other

      # Build binary matrix: rows = outcomes, cols = matches
      # bit_matrix[k,j] = 1 means player_a wins match j in outcome k
      k_seq <- 0:(n_outcomes - 1)
      bit_matrix <- matrix(0L, nrow = n_outcomes, ncol = n_other)
      for (j in seq_len(n_other)) {
        bit_matrix[, j] <- as.integer(bitwAnd(bitwShiftR(k_seq, j - 1L), 1L))
      }

      # Compute log probability of each outcome (vectorized)
      # log_prob = sum over j of: bit*log(p_a) + (1-bit)*log(1-p_a)
      log_pa <- log(o_p_a)
      log_1mpa <- log(1 - o_p_a)
      # Each row: sum of bit_matrix[k,j]*log_pa[j] + (1-bit_matrix[k,j])*log_1mpa[j]
      log_probs <- bit_matrix %*% log_pa + (1 - bit_matrix) %*% log_1mpa
      probs <- exp(log_probs)

      # Compute loser ranks for each outcome (vectorized)
      # If bit=1 (a wins): loser rank = rank_b; if bit=0 (b wins): loser rank = rank_a
      # loser_rank_matrix[k,j] = bit*rank_b + (1-bit)*rank_a
      loser_rank_matrix <- bit_matrix * rep(o_rank_b, each = n_outcomes) +
                           (1 - bit_matrix) * rep(o_rank_a, each = n_outcomes)

      # For each outcome, count how many loser ranks are strictly < prank
      n_better_matrix <- rowSums(loser_rank_matrix < prank)

      # Player i's position = n_better + 1
      pos_i <- n_better_matrix + 1L

      # P_i^{LL} = sum of probs where pos_i <= c_slots
      results$p_ll[idx] <- sum(probs[pos_i <= c_slots])
      results$computation_method[idx] <- "exact"

    } else {
      # MONTE CARLO: simulate 100,000 draws (vectorized)
      n_sims <- 100000

      # Draw uniform random numbers: matrix n_sims x n_other
      U <- matrix(runif(n_sims * n_other), nrow = n_sims, ncol = n_other)

      # bit_matrix: 1 if player_a wins (U < p_a)
      bit_matrix <- U < rep(o_p_a, each = n_sims)

      # Loser ranks
      loser_rank_matrix <- bit_matrix * rep(o_rank_b, each = n_sims) +
                           (!bit_matrix) * rep(o_rank_a, each = n_sims)

      # Count how many have rank < prank
      n_better <- rowSums(loser_rank_matrix < prank)
      pos_i <- n_better + 1L

      results$p_ll[idx] <- mean(pos_i <= c_slots)
      results$computation_method[idx] <- "montecarlo"
    }
  }

  return(results)
}


# Process all events
message("  Processing ", n_events, " events...")

p_ll_results <- list()
counter <- 0
n_exact <- 0
n_mc <- 0
n_not_found <- 0
n_trivial <- 0

for (tid in event_ids) {
  counter <- counter + 1
  if (counter %% 50 == 0) message("    Event ", counter, " / ", n_events)

  # Get matches for this event
  evt_matches <- event_matches |> filter(tourney_id == tid)

  # Get losers from estimation sample
  evt_losers <- est_players |> filter(tourney_id == tid)

  # Get LL slots
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

message("\n  P_i^{LL} computation complete:")
message("  Total observations: ", nrow(p_ll_all))
message("  Exact enumeration: ", n_exact)
message("  Monte Carlo: ", n_mc)
message("  Not found in qualifying data: ", n_not_found)
message("  Trivial (single match): ", n_trivial)
message("  P_i^{LL} summary:")
print(summary(p_ll_all$p_ll))

slog("## Step 4: P_i^{LL} computed")
slog("- Total observations: ", nrow(p_ll_all))
slog("- Exact enumeration: ", n_exact)
slog("- Monte Carlo: ", n_mc)
slog("- Not found in qualifying data: ", n_not_found)
slog("- Trivial (single match): ", n_trivial)
slog("")


# ==============================================================================
# STEP 5: SAVE CORRECTED P_i^{LL}
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 5: SAVE CORRECTED P_i^{LL}")
message(strrep("=", 70))

saveRDS(p_ll_all, file.path(CLEANED_DIR, "p_ll_corrected.rds"))
message("  Saved: Data/cleaned/p_ll_corrected.rds")


# ==============================================================================
# STEP 6: MERGE INTO ESTIMATION DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 6: MERGE INTO ESTIMATION DATA")
message(strrep("=", 70))

# Deduplicate p_ll_all (one per player-event)
p_ll_merge <- p_ll_all |>
  select(tourney_id, player_id, p_ll, n_matches_at_event, computation_method) |>
  distinct(tourney_id, player_id, .keep_all = TRUE)

# Merge
nongs_est <- nongs_est |>
  left_join(p_ll_merge, by = c("tourney_id", "player_id"))

# Replace peer_component with corrected p_ll
n_replaced <- sum(!is.na(nongs_est$p_ll))
n_kept_old <- sum(is.na(nongs_est$p_ll))
message("  Replaced peer_component: ", n_replaced, " observations")
message("  Kept old (no qualifying data): ", n_kept_old, " observations")

# Use corrected p_ll where available; keep old where not
nongs_est$peer_component <- ifelse(!is.na(nongs_est$p_ll),
                                   nongs_est$p_ll,
                                   nongs_est$peer_component_old)

slog("## Step 6: Merged into estimation data")
slog("- Replaced: ", n_replaced, " observations")
slog("- Kept old: ", n_kept_old, " observations")
slog("")


# ==============================================================================
# STEP 7: DIAGNOSTIC -- OLD vs NEW P_i^{LL}
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 7: DIAGNOSTIC -- OLD vs NEW P_i^{LL}")
message(strrep("=", 70))

compare <- nongs_est |>
  filter(!is.na(peer_component_old), !is.na(p_ll))

if (nrow(compare) > 0) {
  corr <- cor(compare$peer_component_old, compare$p_ll, use = "complete.obs")
  mad <- mean(abs(compare$peer_component_old - compare$p_ll), na.rm = TRUE)
  rmsd <- sqrt(mean((compare$peer_component_old - compare$p_ll)^2, na.rm = TRUE))
  max_diff <- max(abs(compare$peer_component_old - compare$p_ll), na.rm = TRUE)

  message("  Comparison (", nrow(compare), " observations):")
  message("  Correlation: ", round(corr, 4))
  message("  Mean absolute difference: ", round(mad, 4))
  message("  RMSD: ", round(rmsd, 4))
  message("  Max absolute difference: ", round(max_diff, 4))

  message("\n  Old P summary:")
  print(summary(compare$peer_component_old))
  message("  New P summary:")
  print(summary(compare$p_ll))

  # Check direction of change
  n_higher <- sum(compare$p_ll > compare$peer_component_old + 0.001)
  n_lower <- sum(compare$p_ll < compare$peer_component_old - 0.001)
  n_same <- nrow(compare) - n_higher - n_lower
  message("\n  Direction: ", n_higher, " higher, ", n_lower, " lower, ", n_same, " ~same")

  slog("## Step 7: Diagnostic comparison")
  slog("- N compared: ", nrow(compare))
  slog("- Correlation: ", round(corr, 4))
  slog("- Mean absolute difference: ", round(mad, 4))
  slog("- RMSD: ", round(rmsd, 4))
  slog("- Max absolute difference: ", round(max_diff, 4))
  slog("- Direction: ", n_higher, " higher, ", n_lower, " lower, ", n_same, " ~same")
  slog("")
} else {
  message("  WARNING: No overlapping observations for comparison")
  slog("## Step 7: No overlapping observations for comparison")
  slog("")
}


# ==============================================================================
# STEP 8: RECOMPUTE GENERALIZED RESIDUALS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 8: RECOMPUTE GENERALIZED RESIDUALS")
message(strrep("=", 70))

compute_gen_residual <- function(D, P) {
  P <- pmax(pmin(P, 0.9999), 0.0001)
  probit_P <- qnorm(P)
  phi_val  <- dnorm(probit_P)
  v <- D * phi_val / P - (1 - D) * phi_val / (1 - P)
  v
}

nongs_est$v_hat <- compute_gen_residual(nongs_est$got_ll, nongs_est$peer_component)

message("  v_hat computed for ", sum(!is.na(nongs_est$v_hat)), " observations")
message("  v_hat range: [", round(min(nongs_est$v_hat, na.rm = TRUE), 3),
        ", ", round(max(nongs_est$v_hat, na.rm = TRUE), 3), "]")

slog("## Step 8: Generalized residuals recomputed")
slog("- v_hat range: [", round(min(nongs_est$v_hat, na.rm = TRUE), 3),
     ", ", round(max(nongs_est$v_hat, na.rm = TRUE), 3), "]")
slog("")


# ==============================================================================
# STEP 9: SAVE UPDATED ESTIMATION DATASET
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 9: SAVE UPDATED ESTIMATION DATASET")
message(strrep("=", 70))

# Drop temporary columns
nongs_est$p_ll <- NULL
nongs_est$computation_method <- NULL
nongs_est$n_matches_at_event <- NULL

saveRDS(nongs_est, file.path(CLEANED_DIR, "skeleton_nongs_est_v4.rds"))
message("  Saved: Data/cleaned/skeleton_nongs_est_v4.rds")


# ==============================================================================
# STEP 10: RE-ESTIMATE NON-GS STACKED DYNAMIC TABLES WITH CF
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 10: RE-ESTIMATE NON-GS STACKED DYNAMICS WITH CF")
message(strrep("=", 70))

# Use stack_horizons_v2 from script 30
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

message("  Stacked non-GS ATP: ", nrow(stacked_nongs_atp), " rows")
message("  Stacked non-GS WTA: ", nrow(stacked_nongs_wta), " rows")

# CF estimation function (from script 30)
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
      " | slam_year + horizon"
    ))

    fit <- tryCatch(
      feols(fml, data = sdata, vcov = ~player_id),
      error = function(e) {
        message("    CF error for ", ob, " (", tour_label, "): ", e$message)
        NULL
      }
    )
    if (is.null(fit)) next

    model_objects[[ob]] <- fit
    cf <- coef(fit)
    se_vec <- sqrt(diag(vcov(fit)))

    for (h_lab in HORIZON_LABS) {
      coef_name <- paste0("got_ll:horizon", h_lab)
      if (coef_name %in% names(cf)) {
        beta <- cf[coef_name]
        se_val <- se_vec[coef_name]
        pv <- 2 * pnorm(-abs(beta / se_val))
        n_h <- sum(!is.na(sdata[[ob]]) & sdata$horizon == h_lab)

        results[[paste0(ob, "_", h_lab)]] <- tibble(
          tour = tour_label, outcome = ob, horizon = h_lab,
          coef = beta, se = se_val, pvalue = pv,
          n_obs_total = nrow(sdata), n_obs_horizon = n_h,
          n_units = n_distinct(paste0(sdata$player_id, "_", sdata$tourney_id))
        )
      }

      # Endogeneity test: rho on v_hat
      rho_name <- paste0("v_hat:horizon", h_lab)
      rho_name_alt <- paste0("horizon", h_lab, ":v_hat")
      rho_nm <- if (rho_name %in% names(cf)) rho_name else if (rho_name_alt %in% names(cf)) rho_name_alt else NA_character_
      if (!is.na(rho_nm)) {
        rho <- cf[rho_nm]
        rho_se <- se_vec[rho_nm]
        rho_pv <- 2 * pnorm(-abs(rho / rho_se))
        rho_results[[paste0(ob, "_", h_lab)]] <- tibble(
          tour = tour_label, outcome = ob, horizon = h_lab,
          rho = rho, rho_se = rho_se, rho_pvalue = rho_pv
        )
      }
    }
  }
  list(results = bind_rows(results), models = model_objects, rho = bind_rows(rho_results))
}

out_nongs_atp <- run_stacked_nongs_cf(stacked_nongs_atp, "ATP non-GS")
out_nongs_wta <- run_stacked_nongs_cf(stacked_nongs_wta, "WTA non-GS")

stacked_nongs_res_atp <- out_nongs_atp$results
stacked_nongs_res_wta <- out_nongs_wta$results
rho_nongs_atp <- out_nongs_atp$rho
rho_nongs_wta <- out_nongs_wta$rho

slog("## Step 10: Non-GS stacked dynamics with corrected CF")
for (i in seq_len(nrow(stacked_nongs_res_atp))) {
  r <- stacked_nongs_res_atp[i, ]
  slog("- ATP non-GS CF ", r$outcome, " @ ", r$horizon,
       ": coef = ", fmt(r$coef), ", SE = ", fmt(r$se),
       ", p = ", fmt(r$pvalue, 3))
}
for (i in seq_len(nrow(stacked_nongs_res_wta))) {
  r <- stacked_nongs_res_wta[i, ]
  slog("- WTA non-GS CF ", r$outcome, " @ ", r$horizon,
       ": coef = ", fmt(r$coef), ", SE = ", fmt(r$se),
       ", p = ", fmt(r$pvalue, 3))
}
slog("")

# Log endogeneity test
slog("## Endogeneity test (rho on v_hat) -- CORRECTED P_i^{LL}:")
for (i in seq_len(nrow(rho_nongs_atp))) {
  r <- rho_nongs_atp[i, ]
  sig <- if (r$rho_pvalue < 0.05) "SIGNIFICANT" else "not significant"
  slog("- ATP rho ", r$outcome, " @ ", r$horizon,
       ": rho = ", fmt(r$rho, 3), ", p = ", fmt(r$rho_pvalue, 3), " -- ", sig)
}
for (i in seq_len(nrow(rho_nongs_wta))) {
  r <- rho_nongs_wta[i, ]
  sig <- if (r$rho_pvalue < 0.05) "SIGNIFICANT" else "not significant"
  slog("- WTA rho ", r$outcome, " @ ", r$horizon,
       ": rho = ", fmt(r$rho, 3), ", p = ", fmt(r$rho_pvalue, 3), " -- ", sig)
}
slog("")


# ==============================================================================
# STEP 11: BUILD TABLES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 11: BUILD STACKED DYNAMIC TABLES")
message(strrep("=", 70))

build_stacked_tex <- function(res_df, tour_label, filename, is_cf = FALSE) {
  n_hor <- length(HORIZON_LABS)

  tex <- c(
    paste0("\\begin{tabular}{l", paste(rep(" c", n_hor), collapse = ""), "}"),
    "\\toprule",
    paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"),
    "\\midrule"
  )

  avail_outcomes <- intersect(unique(res_df$outcome), names(outcome_labels))
  for (ob in names(outcome_labels)) {
    if (!ob %in% avail_outcomes) next
    olab <- outcome_labels[ob]

    cells <- character()
    se_cells <- character()
    for (h in HORIZON_LABS) {
      r <- res_df |> filter(outcome == ob, horizon == h)
      if (nrow(r) == 0) {
        cells <- c(cells, "")
        se_cells <- c(se_cells, "")
      } else {
        cells <- c(cells, paste0(fmt(r$coef), add_stars(r$pvalue)))
        se_cells <- c(se_cells, paste0("(", fmt(r$se), ")"))
      }
    }
    tex <- c(tex,
      paste0(olab, " & ", paste(cells, collapse = " & "), " \\\\"),
      paste0(" & ", paste(se_cells, collapse = " & "), " \\\\"),
      "\\addlinespace"
    )
  }

  if (nrow(res_df) > 0) {
    n_units <- res_df$n_units[1]
    n_obs <- res_df$n_obs_total[1]

    tex <- c(tex, "\\midrule")

    h_n_cells <- character()
    for (h in HORIZON_LABS) {
      r <- res_df |> filter(horizon == h)
      if (nrow(r) > 0) {
        h_n_cells <- c(h_n_cells, as.character(r$n_obs_horizon[1]))
      } else {
        h_n_cells <- c(h_n_cells, "--")
      }
    }
    tex <- c(tex,
      paste0("$N$ (per horizon) & ", paste(h_n_cells, collapse = " & "), " \\\\"),
      paste0("$N$ (stacked total) & \\multicolumn{", n_hor, "}{c}{", n_obs, "} \\\\"),
      paste0("Player-events & \\multicolumn{", n_hor, "}{c}{", n_units, "} \\\\"),
      paste0("Event FE & \\multicolumn{", n_hor, "}{c}{Yes} \\\\"),
      paste0("Horizon FE & \\multicolumn{", n_hor, "}{c}{Yes} \\\\")
    )
    if (is_cf) {
      tex <- c(tex,
        paste0("Control function & \\multicolumn{", n_hor, "}{c}{Yes} \\\\")
      )
    }
  }

  tex <- c(tex, "\\bottomrule", "\\end{tabular}")
  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

build_stacked_tex(stacked_nongs_res_atp, "ATP non-GS",
                  "table_dynamic_stacked_nongs_atp.tex", is_cf = TRUE)
build_stacked_tex(stacked_nongs_res_wta, "WTA non-GS",
                  "table_dynamic_stacked_nongs_wta.tex", is_cf = TRUE)


# ==============================================================================
# STEP 12: SAVE SUMMARY
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 12: SAVE SUMMARY")
message(strrep("=", 70))

# Save all results
saveRDS(list(
  p_ll_corrected = p_ll_all,
  nongs_res_atp = stacked_nongs_res_atp,
  nongs_res_wta = stacked_nongs_res_wta,
  rho_atp = rho_nongs_atp,
  rho_wta = rho_nongs_wta
), file.path(CLEANED_DIR, "p_ll_correction_results.rds"))

# Write summary
writeLines(summary_log, file.path(OUTPUT_DIR, "p_ll_correction_summary.md"))
message("  Saved: Output/p_ll_correction_summary.md")

message("\n", strrep("=", 70))
message("DONE: P_i^{LL} correction complete")
message(strrep("=", 70))
