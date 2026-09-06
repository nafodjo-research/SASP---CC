# ==============================================================================
# F00_sample_construction.R
# CRITICAL: All other FirstLL scripts depend on this.
#
# Purpose: Construct the first-LL estimation samples by restricting to
#   players with had_prior_ll == 0 (no prior LL of any type).
#   Both treated (got_ll == 1) and controls (got_ll == 0) are LL-naive.
#
# Steps:
#   1. Load final skeletons (v5 GS, v8 nonGS) from main pipeline
#   2. Filter both to had_prior_ll == 0
#   3. For non-GS: re-estimate win model on restricted events, recompute
#      P_i^{LL} and v_hat (control function residual)
#   4. Merge performance dose (filtered to first-LL players)
#   5. Apply ensure_scaled() and save
#
# Inputs:
#   Data/cleaned/skeleton_gs_est_v5.rds
#   Data/cleaned/skeleton_nongs_est_v8.rds
#   Data/cleaned/performance_dose.rds
#   Data/cleaned/elo_history.rds, wta_elo_history.rds
#   Data/cleaned/tournament_elo_cache.rds
#   Data/raw/atp_qual_chall_matches.rds, wta_qual_itf_matches.rds
#
# Outputs:
#   Data/cleaned/firstll/firstll_gs_est.rds
#   Data/cleaned/firstll/firstll_nongs_est.rds
#   Data/cleaned/firstll/firstll_win_model_atp.rds
#   Data/cleaned/firstll/firstll_win_model_wta.rds
#   Data/cleaned/firstll/firstll_p_ll.rds
#   Data/cleaned/firstll/firstll_performance_dose.rds
#   Output_FirstLL/F00_sample_diagnostics.txt
#
# Dependencies: dplyr, data.table, fixest, here
# ==============================================================================

set.seed(20260416)

library(dplyr)
library(data.table)
library(here)

source(here("scripts", "R", "firstll", "firstll_helpers.R"))
source(here("scripts", "R", "win_model_helpers.R"))
summary_log <- character()


# ==============================================================================
# STEP 1: LOAD AND FILTER TO FIRST-LL
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 1: LOAD FINAL SKELETONS AND FILTER TO had_prior_ll == 0")
message(strrep("=", 70))

gs_full  <- readRDS(file.path(CLEANED_DIR, "skeleton_gs_est_v5.rds"))
ngs_full <- readRDS(file.path(CLEANED_DIR, "skeleton_nongs_est_v8.rds"))

slog("Full GS sample:  ", nrow(gs_full),
     " (treated: ", sum(gs_full$got_ll == 1), ")")
slog("Full NGS sample: ", nrow(ngs_full),
     " (treated: ", sum(ngs_full$got_ll == 1), ")")

# Apply first-LL restriction
gs  <- gs_full[gs_full$had_prior_ll == 0, ]
ngs <- ngs_full[ngs_full$had_prior_ll == 0, ]

slog("")
slog("First-LL GS:  ", nrow(gs),
     " (treated: ", sum(gs$got_ll == 1),
     ", control: ", sum(gs$got_ll == 0), ")")
slog("First-LL NGS: ", nrow(ngs),
     " (treated: ", sum(ngs$got_ll == 1),
     ", control: ", sum(ngs$got_ll == 0), ")")

# Tour-level breakdowns
for (t in c("ATP", "WTA")) {
  gs_t  <- gs[gs$tour == t, ]
  ngs_t <- ngs[ngs$tour == t, ]
  slog("  ", t, " GS:  N=", nrow(gs_t),
       " (T=", sum(gs_t$got_ll == 1), ", C=", sum(gs_t$got_ll == 0), ")")
  slog("  ", t, " NGS: N=", nrow(ngs_t),
       " (T=", sum(ngs_t$got_ll == 1), ", C=", sum(ngs_t$got_ll == 0), ")")
}

# Sanity check: no one should have prior LL
stopifnot(all(gs$had_prior_ll == 0))
stopifnot(all(ngs$had_prior_ll == 0))
slog("\nSanity check PASSED: all had_prior_ll == 0")

# Check: how many events still have variation in got_ll
gs_events <- gs |>
  group_by(tourney_id) |>
  summarise(has_treated = any(got_ll == 1),
            has_control = any(got_ll == 0), .groups = "drop")
ngs_events <- ngs |>
  group_by(tourney_id) |>
  summarise(has_treated = any(got_ll == 1),
            has_control = any(got_ll == 0), .groups = "drop")

slog("GS events with both T and C: ",
     sum(gs_events$has_treated & gs_events$has_control), " / ",
     nrow(gs_events))
slog("NGS events with both T and C: ",
     sum(ngs_events$has_treated & ngs_events$has_control), " / ",
     nrow(ngs_events))

# Flag if non-GS sample is very small
for (t in c("ATP", "WTA")) {
  n_t <- sum(ngs$tour == t & ngs$got_ll == 1)
  if (n_t < 50) {
    slog("WARNING: ", t, " non-GS treated N = ", n_t,
         " (< 50). CF analysis may lack power.")
  }
}


# ==============================================================================
# STEP 2: RE-ESTIMATE WIN MODEL ON RESTRICTED NON-GS EVENTS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 2: RE-ESTIMATE WIN MODEL ON FIRST-LL NON-GS EVENTS")
message(strrep("=", 70))

# Load qualifying match data and Elo
atp_qual_raw <- readRDS(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
wta_qual_raw <- readRDS(file.path(RAW_DIR, "wta_qual_itf_matches.rds"))

atp_elo <- as.data.table(readRDS(file.path(CLEANED_DIR, "elo_history.rds")))
wta_elo <- as.data.table(readRDS(file.path(CLEANED_DIR, "wta_elo_history.rds")))
setnames(atp_elo, c("player_id", "match_date", "elo"))
setnames(wta_elo, c("player_id", "match_date", "elo"))
setkey(atp_elo, player_id, match_date)
setkey(wta_elo, player_id, match_date)

elo_cache <- readRDS(file.path(CLEANED_DIR, "tournament_elo_cache.rds"))

# Get event IDs from the restricted non-GS sample
ngs_atp_ids <- unique(ngs$tourney_id[ngs$tour == "ATP"])
ngs_wta_ids <- unique(ngs$tourney_id[ngs$tour == "WTA"])

qual_rounds <- c("Q1", "Q2", "Q3", "Q4", "Q5", "QF")

atp_q <- atp_qual_raw[atp_qual_raw$tourney_id %in% ngs_atp_ids &
                       atp_qual_raw$round %in% qual_rounds, ]
atp_q$tour <- "ATP"

wta_q <- wta_qual_raw[wta_qual_raw$tourney_id %in% ngs_wta_ids &
                       wta_qual_raw$round %in% qual_rounds, ]
wta_q$tour <- "WTA"

slog("ATP qualifying matches at first-LL events: ", nrow(atp_q))
slog("WTA qualifying matches at first-LL events: ", nrow(wta_q))

# Parse round number for weighting fallback
parse_round_num <- function(df) {
  df$round_num <- as.integer(gsub("Q|QF", "", df$round))
  df$round_num[df$round == "QF"] <- NA_integer_
  max_by_event <- tapply(df$round_num[!is.na(df$round_num)],
                         df$tourney_id[!is.na(df$round_num)], max)
  for (tid in names(max_by_event)) {
    idx <- df$tourney_id == tid & is.na(df$round_num)
    df$round_num[idx] <- max_by_event[tid] + 1L
  }
  df
}

atp_q <- parse_round_num(atp_q)
wta_q <- parse_round_num(wta_q)

# Weights: total service points when available, round number otherwise
add_weights <- function(df) {
  df$total_pts <- df$w_svpt + df$l_svpt
  has_pts <- !is.na(df$total_pts)
  df$weight <- ifelse(has_pts, df$total_pts, df$round_num)
  df$weight <- df$weight / mean(df$weight, na.rm = TRUE)
  df$weight[is.na(df$weight)] <- 1.0
  df
}

atp_q <- add_weights(atp_q)
wta_q <- add_weights(wta_q)

# Merge Elo
message("  ATP Elo merge...")
atp_q <- merge_elo(atp_q, atp_elo, "ATP")
message("  WTA Elo merge...")
wta_q <- merge_elo(wta_q, wta_elo, "WTA")

# H2H
message("  Building ATP H2H index...")
atp_h2h_idx <- build_h2h_index(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
message("  Computing ATP H2H...")
atp_h2h <- compute_h2h(atp_q$winner_id, atp_q$loser_id, atp_q$match_date, atp_h2h_idx)
atp_q$h2h_win_prop <- atp_h2h$h2h_prop
atp_q$h2h_count <- atp_h2h$h2h_count

message("  Building WTA H2H index...")
wta_h2h_idx <- build_h2h_index(file.path(RAW_DIR, "wta_qual_itf_matches.rds"))
message("  Computing WTA H2H...")
wta_h2h <- compute_h2h(wta_q$winner_id, wta_q$loser_id, wta_q$match_date, wta_h2h_idx)
wta_q$h2h_win_prop <- wta_h2h$h2h_prop
wta_q$h2h_count <- wta_h2h$h2h_count

rm(atp_h2h_idx, wta_h2h_idx, atp_h2h, wta_h2h)
gc()

# Build estimation data
atp_est_data <- build_est_data(atp_q)
wta_est_data <- build_est_data(wta_q)

atp_est_data <- atp_est_data[complete.cases(
  atp_est_data[, c("elo_diff", "surface_elo_diff")]), ]
wta_est_data <- wta_est_data[complete.cases(
  wta_est_data[, c("elo_diff", "surface_elo_diff")]), ]

slog("ATP win model estimation data: ", nrow(atp_est_data), " matches")
slog("WTA win model estimation data: ", nrow(wta_est_data), " matches")

# Estimate weighted logit (same LOGIT_FORMULA from win_model_helpers.R)
message("\n  --- ATP Model (First-LL events) ---")
atp_model <- glm(LOGIT_FORMULA, data = atp_est_data,
                 family = binomial(link = "logit"), weights = weight)
slog("ATP model: N=", nobs(atp_model), ", AIC=", round(AIC(atp_model)))

message("\n  --- WTA Model (First-LL events) ---")
wta_model <- glm(LOGIT_FORMULA, data = wta_est_data,
                 family = binomial(link = "logit"), weights = weight)
slog("WTA model: N=", nobs(wta_model), ", AIC=", round(AIC(wta_model)))

saveRDS(atp_model, file.path(FIRSTLL_CLEANED, "firstll_win_model_atp.rds"))
saveRDS(wta_model, file.path(FIRSTLL_CLEANED, "firstll_win_model_wta.rds"))


# ==============================================================================
# STEP 3: RECOMPUTE P_i^{LL} ON RESTRICTED SAMPLE
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 3: RECOMPUTE P_i^{LL}")
message(strrep("=", 70))

# Identify final qualifying round for each event
final_round_map_atp <- atp_q |>
  group_by(tourney_id) |>
  summarise(final_round = max(round), .groups = "drop")
final_round_map_wta <- wta_q |>
  group_by(tourney_id) |>
  summarise(final_round = max(round), .groups = "drop")

final_atp <- atp_q |>
  inner_join(final_round_map_atp, by = "tourney_id") |>
  filter(round == final_round)
final_wta <- wta_q |>
  inner_join(final_round_map_wta, by = "tourney_id") |>
  filter(round == final_round)

# Predict P(winner wins) using tour-specific model
final_atp$p_winner_wins <- predict_match_probs(final_atp, atp_model)
final_wta$p_winner_wins <- predict_match_probs(final_wta, wta_model)

# Build event match structure
build_event_matches <- function(final_df) {
  data.frame(
    tourney_id = final_df$tourney_id,
    player_a = final_df$winner_id,
    player_b = final_df$loser_id,
    rank_a = as.numeric(final_df$winner_rank),
    rank_b = as.numeric(final_df$loser_rank),
    p_a_wins = final_df$p_winner_wins,
    stringsAsFactors = FALSE
  )
}

atp_evt_matches <- build_event_matches(final_atp)
wta_evt_matches <- build_event_matches(final_wta)

# LL slots per event (from restricted sample)
ll_slots <- ngs |>
  group_by(tourney_id) |>
  summarise(n_ll_slots = n_ll_slots[1], .groups = "drop")

# Players in the restricted sample
est_players <- ngs |>
  select(tourney_id, player_id, player_rank, tour) |>
  distinct()

# Exact enumeration of P_i^{LL}
# (same compute_p_ll_event function from 35d)
compute_p_ll_event <- function(matches_df, losers_df, c_slots) {
  n_matches <- nrow(matches_df)
  if (n_matches == 0 || c_slots == 0) {
    return(data.frame(player_id = losers_df$player_id, p_ll = NA_real_,
                      computation_method = "none", stringsAsFactors = FALSE))
  }
  results <- data.frame(player_id = losers_df$player_id, p_ll = NA_real_,
                        computation_method = NA_character_, stringsAsFactors = FALSE)
  m_a <- matches_df$player_a; m_b <- matches_df$player_b
  m_ra <- matches_df$rank_a; m_rb <- matches_df$rank_b
  m_pa <- matches_df$p_a_wins

  for (idx in seq_len(nrow(losers_df))) {
    pid <- losers_df$player_id[idx]
    prank <- losers_df$player_rank[idx]
    if (is.na(prank)) prank <- 9999
    mi <- which(m_a == pid | m_b == pid)
    if (length(mi) == 0) {
      results$p_ll[idx] <- NA
      results$computation_method[idx] <- "not_found"
      next
    }
    mi <- mi[1]
    other <- setdiff(seq_len(n_matches), mi)
    n_o <- length(other)
    if (n_o == 0) {
      results$p_ll[idx] <- if (c_slots >= 1) 1 else 0
      results$computation_method[idx] <- "trivial"
      next
    }
    o_pa <- m_pa[other]; o_ra <- m_ra[other]; o_rb <- m_rb[other]
    o_ra[is.na(o_ra)] <- 9999; o_rb[is.na(o_rb)] <- 9999

    if (n_o <= 16) {
      n_out <- 2^n_o; k_seq <- 0:(n_out - 1)
      bm <- matrix(0L, n_out, n_o)
      for (j in seq_len(n_o)) bm[, j] <- as.integer(bitwAnd(bitwShiftR(k_seq, j - 1L), 1L))
      lp <- bm %*% log(o_pa) + (1 - bm) %*% log(1 - o_pa)
      probs <- exp(lp)
      lr <- bm * rep(o_rb, each = n_out) + (1 - bm) * rep(o_ra, each = n_out)
      pos <- rowSums(lr < prank) + 1L
      results$p_ll[idx] <- sum(probs[pos <= c_slots])
      results$computation_method[idx] <- "exact"
    } else {
      ns <- 100000
      U <- matrix(runif(ns * n_o), ns, n_o)
      bm <- U < rep(o_pa, each = ns)
      lr <- bm * rep(o_rb, each = ns) + (!bm) * rep(o_ra, each = ns)
      pos <- rowSums(lr < prank) + 1L
      results$p_ll[idx] <- mean(pos <= c_slots)
      results$computation_method[idx] <- "montecarlo"
    }
  }
  results
}

process_p_ll <- function(evt_matches, est_players_tour, ll_slots_df, tour_label) {
  event_ids <- unique(evt_matches$tourney_id)
  n_events <- length(event_ids)
  message("  Processing ", n_events, " ", tour_label, " events...")
  p_ll_results <- list()
  for (i in seq_along(event_ids)) {
    tid <- event_ids[i]
    if (i %% 50 == 0) message("    Event ", i, " / ", n_events)
    em <- evt_matches[evt_matches$tourney_id == tid, ]
    el <- est_players_tour[est_players_tour$tourney_id == tid, ]
    c_t <- ll_slots_df$n_ll_slots[ll_slots_df$tourney_id == tid]
    if (length(c_t) == 0 || is.na(c_t) || c_t == 0) next
    result <- compute_p_ll_event(em, el, c_t)
    result$tourney_id <- tid
    p_ll_results[[i]] <- result
  }
  bind_rows(p_ll_results)
}

atp_est_players <- est_players[est_players$tour == "ATP", ]
wta_est_players <- est_players[est_players$tour == "WTA", ]

p_ll_atp <- process_p_ll(atp_evt_matches, atp_est_players, ll_slots, "ATP")
p_ll_wta <- process_p_ll(wta_evt_matches, wta_est_players, ll_slots, "WTA")
p_ll_all <- bind_rows(p_ll_atp, p_ll_wta)

slog("\nP_i^{LL} summary (first-LL sample):")
slog("  ATP mean: ", round(mean(p_ll_atp$p_ll, na.rm = TRUE), 4),
     "  median: ", round(median(p_ll_atp$p_ll, na.rm = TRUE), 4))
slog("  WTA mean: ", round(mean(p_ll_wta$p_ll, na.rm = TRUE), 4),
     "  median: ", round(median(p_ll_wta$p_ll, na.rm = TRUE), 4))

saveRDS(p_ll_all, file.path(FIRSTLL_CLEANED, "firstll_p_ll.rds"))


# ==============================================================================
# STEP 4: MERGE P_i^{LL} AND COMPUTE v_hat
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 4: MERGE AND COMPUTE GENERALIZED RESIDUALS")
message(strrep("=", 70))

p_ll_merge <- p_ll_all |>
  select(tourney_id, player_id, p_ll) |>
  distinct(tourney_id, player_id, .keep_all = TRUE)

# Save old peer_component for comparison
ngs$peer_component_old <- ngs$peer_component

ngs <- ngs |>
  left_join(p_ll_merge, by = c("tourney_id", "player_id"))

n_rep <- sum(!is.na(ngs$p_ll))
slog("P_i^{LL} merged: ", n_rep, " / ", nrow(ngs))

# Update peer_component with new p_ll where available
ngs$peer_component <- ifelse(!is.na(ngs$p_ll),
                             ngs$p_ll,
                             ngs$peer_component_old)

# Compute generalized residual
ngs$v_hat <- compute_gen_residual(ngs$got_ll, ngs$peer_component)

# Compare old vs new
cmp <- ngs[!is.na(ngs$peer_component_old) & !is.na(ngs$p_ll), ]
if (nrow(cmp) > 0) {
  slog("Old vs new P_i^{LL} correlation: ",
       round(cor(cmp$peer_component_old, cmp$p_ll), 4))
}

ngs$p_ll <- NULL
ngs$peer_component_old <- NULL


# ==============================================================================
# STEP 5: MERGE PERFORMANCE DOSE
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 5: MERGE PERFORMANCE DOSE")
message(strrep("=", 70))

dose_full <- readRDS(file.path(CLEANED_DIR, "performance_dose.rds"))

# Filter dose to first-LL players only (match on player_id + tourney_id)
firstll_key_gs <- paste0(gs$player_id, "_", gs$tourney_id)
firstll_key_ngs <- paste0(ngs$player_id, "_", ngs$tourney_id)
dose_full$key <- paste0(dose_full$player_id, "_", dose_full$tourney_id)

dose_firstll <- dose_full[dose_full$key %in% c(firstll_key_gs, firstll_key_ngs), ]
dose_firstll$key <- NULL

slog("Performance dose rows (first-LL): ", nrow(dose_firstll), " / ", nrow(dose_full))

saveRDS(dose_firstll, file.path(FIRSTLL_CLEANED, "firstll_performance_dose.rds"))

# Merge into skeletons
gs  <- merge_dose(gs, dose_firstll, "GS")
ngs <- merge_dose(ngs, dose_firstll, "nonGS")


# ==============================================================================
# STEP 6: ENSURE SCALED VARIABLES AND SAVE
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 6: SCALE AND SAVE")
message(strrep("=", 70))

gs  <- ensure_scaled(gs)
ngs <- ensure_scaled(ngs)

# Ensure n_prior_gs_ll_notwon exists (some scripts reference it)
if (!"n_prior_gs_ll_notwon" %in% names(gs)) {
  gs$n_prior_gs_ll_notwon <- gs$n_prior_gs_ll_opp - gs$n_prior_gs_ll_won
}
if (!"n_prior_nongs_ll_notwon" %in% names(gs)) {
  gs$n_prior_nongs_ll_notwon <- gs$n_prior_nongs_ll_opp - gs$n_prior_nongs_ll_won
}
if (!"n_prior_gs_ll_notwon" %in% names(ngs)) {
  ngs$n_prior_gs_ll_notwon <- ngs$n_prior_gs_ll_opp - ngs$n_prior_gs_ll_won
}
if (!"n_prior_nongs_ll_notwon" %in% names(ngs)) {
  ngs$n_prior_nongs_ll_notwon <- ngs$n_prior_nongs_ll_opp - ngs$n_prior_nongs_ll_won
}

saveRDS(gs,  file.path(FIRSTLL_CLEANED, "firstll_gs_est.rds"))
saveRDS(ngs, file.path(FIRSTLL_CLEANED, "firstll_nongs_est.rds"))

slog("\n", strrep("=", 70))
slog("FINAL SAMPLE SIZES")
slog(strrep("=", 70))
for (t in c("ATP", "WTA")) {
  gs_t  <- gs[gs$tour == t, ]
  ngs_t <- ngs[ngs$tour == t, ]
  slog(t, " GS:  N=", nrow(gs_t),
       " (T=", sum(gs_t$got_ll == 1),
       ", C=", sum(gs_t$got_ll == 0), ")")
  slog(t, " NGS: N=", nrow(ngs_t),
       " (T=", sum(ngs_t$got_ll == 1),
       ", C=", sum(ngs_t$got_ll == 0), ")")
  slog("  NGS ", t, " events with variation: ",
       ngs_t |> group_by(tourney_id) |>
         summarise(v = any(got_ll == 1) & any(got_ll == 0), .groups = "drop") |>
         pull(v) |> sum())
}
slog("v_hat range: [", round(min(ngs$v_hat, na.rm = TRUE), 3), ", ",
     round(max(ngs$v_hat, na.rm = TRUE), 3), "]")
slog("v_hat NAs: ", sum(is.na(ngs$v_hat)))

# Save diagnostics
writeLines(summary_log, file.path(FIRSTLL_OUTPUT, "F00_sample_diagnostics.txt"))
message("\nDiagnostics saved to Output_FirstLL/F00_sample_diagnostics.txt")
message("DONE.")
