# ==============================================================================
# 35d_win_model_final.R
# Re-estimate match-winning logit for P_i^{LL} construction
#
# Corrections vs 35:
#   1. Estimation sample = qualifying rounds of non-GS LL-granting events ONLY
#   2. ATP and WTA estimated SEPARATELY
#   3. Z^{pre}_{ie} covariates added (ranking points, Elo, prior LL, age)
#   4. Weighted MLE (total points in play; round number fallback)
#
# Full covariate list:
#   X_{ijm}: elo_diff/100, surface_elo_diff/100, h2h_win_prop, h2h_count,
#            age_diff, ht_diff, hand_mismatch
#   Z_{ie}:  surface_clay, surface_grass, is_masters
#   Z^{pre}_{ie} (focal player): pre_rank_pts/1000, (pre_rank_pts/1000)^2,
#            pre_elo/100, (pre_elo/100)^2,
#            n_prior_gs_ll_won, n_prior_gs_ll_notwon,
#            n_prior_nongs_ll_won, n_prior_nongs_ll_notwon, player_age
#
# Outputs:
#   Data/cleaned/win_model_atp_v4.rds
#   Data/cleaned/win_model_wta_v4.rds
#   Data/cleaned/p_ll_corrected_v3.rds
#   Data/cleaned/skeleton_nongs_est_v6.rds
#   Tables/table_win_model.tex
#   Tables/table_dynamic_stacked_nongs_atp.tex
#   Tables/table_dynamic_stacked_nongs_wta.tex
# ==============================================================================

set.seed(20260327)

library(dplyr)
library(tidyr)
library(data.table)
library(fixest)
library(here)
library(stringr)

source(here("scripts", "R", "utils.R"))
source(here("scripts", "R", "win_model_helpers.R"))
summary_log <- character()

RAW_DIR     <- here("Data", "raw")
CLEANED_DIR <- here("Data", "cleaned")
TABLES_DIR  <- here("Tables")
OUTPUT_DIR  <- here("Output")
for (d in c(CLEANED_DIR, TABLES_DIR, OUTPUT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

outcomes_base  <- c("points_change", "n_main_draws", "n_matches_250plus", "elo_change")
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
message("STEP 1: LOAD DATA")
message(strrep("=", 70))

nongs_est <- readRDS(file.path(CLEANED_DIR, "skeleton_nongs_est_v2.rds"))
nongs_est$peer_component_old <- nongs_est$peer_component

atp_qual_raw <- readRDS(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
wta_qual_raw <- readRDS(file.path(RAW_DIR, "wta_qual_itf_matches.rds"))

atp_elo <- as.data.table(readRDS(file.path(CLEANED_DIR, "elo_history.rds")))
wta_elo <- as.data.table(readRDS(file.path(CLEANED_DIR, "wta_elo_history.rds")))
setnames(atp_elo, c("player_id", "match_date", "elo"))
setnames(wta_elo, c("player_id", "match_date", "elo"))
setkey(atp_elo, player_id, match_date)
setkey(wta_elo, player_id, match_date)

elo_cache <- readRDS(file.path(CLEANED_DIR, "tournament_elo_cache.rds"))

message("  Estimation sample: ", nrow(nongs_est), " rows")
message("  ATP events: ", length(unique(nongs_est$tourney_id[nongs_est$tour == "ATP"])))
message("  WTA events: ", length(unique(nongs_est$tourney_id[nongs_est$tour == "WTA"])))


# ==============================================================================
# STEP 2: FILTER TO QUALIFYING ROUNDS AT ESTIMATION EVENTS ONLY
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 2: FILTER TO ESTIMATION EVENT QUALIFYING ROUNDS")
message(strrep("=", 70))

atp_est_ids <- unique(nongs_est$tourney_id[nongs_est$tour == "ATP"])
wta_est_ids <- unique(nongs_est$tourney_id[nongs_est$tour == "WTA"])

qual_rounds <- c("Q1", "Q2", "Q3", "Q4", "Q5", "QF")

atp_q <- atp_qual_raw[atp_qual_raw$tourney_id %in% atp_est_ids &
                       atp_qual_raw$round %in% qual_rounds, ]
atp_q$tour <- "ATP"

wta_q <- wta_qual_raw[wta_qual_raw$tourney_id %in% wta_est_ids &
                       wta_qual_raw$round %in% qual_rounds, ]
wta_q$tour <- "WTA"

message("  ATP qualifying matches at estimation events: ", nrow(atp_q))
message("  WTA qualifying matches at estimation events: ", nrow(wta_q))

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


# ==============================================================================
# STEP 3: MERGE ELO RATINGS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 3: MERGE ELO RATINGS")
message(strrep("=", 70))

# Functions get_pre_match_elo(), get_surface_elo(), merge_elo() are in win_model_helpers.R

message("  ATP Elo merge...")
atp_q <- merge_elo(atp_q, atp_elo, "ATP")
message("  WTA Elo merge...")
wta_q <- merge_elo(wta_q, wta_elo, "WTA")


# ==============================================================================
# STEP 4: COMPUTE H2H
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 4: COMPUTE H2H")
message(strrep("=", 70))

# Functions build_h2h_index(), compute_h2h() are in win_model_helpers.R

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


# ==============================================================================
# STEP 5: BUILD ESTIMATION DATA WITH Z^{pre}
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 5: BUILD ESTIMATION DATA WITH Z^{pre}")
message(strrep("=", 70))

# build_est_data() is in win_model_helpers.R

atp_est <- build_est_data(atp_q)
wta_est <- build_est_data(wta_q)

# Drop any rows with NA in key variables
atp_est <- atp_est[complete.cases(atp_est[, c("elo_diff", "surface_elo_diff")]), ]
wta_est <- wta_est[complete.cases(wta_est[, c("elo_diff", "surface_elo_diff")]), ]

message("  ATP estimation data: ", nrow(atp_est), " matches")
message("  WTA estimation data: ", nrow(wta_est), " matches")


# ==============================================================================
# STEP 6: ESTIMATE WEIGHTED LOGIT (ATP AND WTA SEPARATELY)
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 6: ESTIMATE WEIGHTED LOGIT")
message(strrep("=", 70))

# Using LOGIT_FORMULA from win_model_helpers.R (no is_masters)

message("\n  --- ATP Model ---")
atp_model <- glm(LOGIT_FORMULA, data = atp_est,
                 family = binomial(link = "logit"), weights = weight)
message("  N: ", nobs(atp_model), "  AIC: ", round(AIC(atp_model)))
print(summary(atp_model)$coefficients)

message("\n  --- WTA Model ---")
wta_model <- glm(LOGIT_FORMULA, data = wta_est,
                 family = binomial(link = "logit"), weights = weight)
message("  N: ", nobs(wta_model), "  AIC: ", round(AIC(wta_model)))
print(summary(wta_model)$coefficients)

saveRDS(atp_model, file.path(CLEANED_DIR, "win_model_atp_v4.rds"))
saveRDS(wta_model, file.path(CLEANED_DIR, "win_model_wta_v4.rds"))
message("  Models saved.")

slog("## Step 6: Models estimated")
slog("- ATP: N=", nobs(atp_model), ", AIC=", round(AIC(atp_model)))
slog("- WTA: N=", nobs(wta_model), ", AIC=", round(AIC(wta_model)))
slog("")


# ==============================================================================
# STEP 7: SUMMARY TABLE (ATP | WTA)
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 7: SUMMARY TABLE")
message(strrep("=", 70))

# Using generate_logit_table() from win_model_helpers.R (COEF_MAP, COEF_ORDER defined there)
table_tex <- generate_logit_table(atp_model, wta_model)
writeLines(table_tex, file.path(TABLES_DIR, "table_win_model.tex"))
message("  Table saved: Tables/table_win_model.tex")


# ==============================================================================
# STEP 8: RE-COMPUTE P_i^{LL}
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 8: RE-COMPUTE P_i^{LL}")
message(strrep("=", 70))

# Identify final qualifying round for each estimation event
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

message("  Final round ATP matches: ", nrow(final_atp))
message("  Final round WTA matches: ", nrow(final_wta))

# Predict P(winner beats loser) for final round matches using tour-specific model
# predict_match_probs() is in win_model_helpers.R

final_atp$p_winner_wins <- predict_match_probs(final_atp, atp_model)
final_wta$p_winner_wins <- predict_match_probs(final_wta, wta_model)

message("  ATP win prob summary:")
print(summary(final_atp$p_winner_wins))
message("  WTA win prob summary:")
print(summary(final_wta$p_winner_wins))

# --- P_i^{LL} exact enumeration (same function as script 33) ---
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
    if (length(mi) == 0) { results$p_ll[idx] <- NA; results$computation_method[idx] <- "not_found"; next }
    mi <- mi[1]
    other <- setdiff(seq_len(n_matches), mi)
    n_o <- length(other)
    if (n_o == 0) { results$p_ll[idx] <- if (c_slots >= 1) 1 else 0; results$computation_method[idx] <- "trivial"; next }
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

# Build event-level match structure
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

ll_slots <- nongs_est |>
  group_by(tourney_id) |>
  summarise(n_ll_slots = n_ll_slots[1], .groups = "drop")

est_players <- nongs_est |>
  select(tourney_id, player_id, player_rank, tour) |>
  distinct()

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

message("\n  P_i^{LL} summary:")
message("  ATP mean: ", round(mean(p_ll_atp$p_ll, na.rm = TRUE), 4),
        "  median: ", round(median(p_ll_atp$p_ll, na.rm = TRUE), 4))
message("  WTA mean: ", round(mean(p_ll_wta$p_ll, na.rm = TRUE), 4),
        "  median: ", round(median(p_ll_wta$p_ll, na.rm = TRUE), 4))

saveRDS(p_ll_all, file.path(CLEANED_DIR, "p_ll_corrected_v3.rds"))


# ==============================================================================
# STEP 9: MERGE, RESIDUALS, RE-ESTIMATE OUTCOMES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 9: MERGE AND RE-ESTIMATE")
message(strrep("=", 70))

p_ll_merge <- p_ll_all |>
  select(tourney_id, player_id, p_ll) |>
  distinct(tourney_id, player_id, .keep_all = TRUE)

nongs_est <- nongs_est |>
  left_join(p_ll_merge, by = c("tourney_id", "player_id"))

n_rep <- sum(!is.na(nongs_est$p_ll))
message("  Replaced: ", n_rep, " / ", nrow(nongs_est))

nongs_est$peer_component <- ifelse(!is.na(nongs_est$p_ll),
                                   nongs_est$p_ll,
                                   nongs_est$peer_component_old)

compute_gen_residual <- function(D, P) {
  P <- pmax(pmin(P, 0.9999), 0.0001)
  pp <- qnorm(P); phi <- dnorm(pp)
  D * phi / P - (1 - D) * phi / (1 - P)
}

nongs_est$v_hat <- compute_gen_residual(nongs_est$got_ll, nongs_est$peer_component)

# Compare
cmp <- nongs_est[!is.na(nongs_est$peer_component_old) & !is.na(nongs_est$p_ll), ]
if (nrow(cmp) > 0) {
  message("  Old vs new correlation: ", round(cor(cmp$peer_component_old, cmp$p_ll), 4))
  message("  Old mean: ", round(mean(cmp$peer_component_old), 4),
          "  New mean: ", round(mean(cmp$p_ll, na.rm = TRUE), 4))
}

nongs_est$p_ll <- NULL
saveRDS(nongs_est, file.path(CLEANED_DIR, "skeleton_nongs_est_v6.rds"))

# --- Re-estimate non-GS outcomes ---
stack_horizons_v2 <- function(data, outcomes_base, horizons = c(4, 8, 12, 26, 52)) {
  stacked <- list()
  for (h in horizons) {
    h_label <- paste0(h, "w")
    row_data <- data[, c("player_id", "tourney_id", "tour", "slam_year", "got_ll",
                         "pre_rank_pts", "pre_rank_pts_sq", "player_age",
                         "pre_elo", "pre_elo_sq",
                         "n_prior_gs_ll_won", "n_prior_gs_ll_notwon",
                         "n_prior_nongs_ll_won", "n_prior_nongs_ll_notwon",
                         "peer_component", "v_hat"), drop = FALSE]
    row_data$horizon <- h_label
    row_data$horizon_num <- h
    for (ob in outcomes_base) {
      col_name <- paste0(ob, "_", h, "w")
      if (col_name %in% names(data)) row_data[[ob]] <- data[[col_name]]
      else row_data[[ob]] <- NA_real_
    }
    stacked[[h_label]] <- row_data
  }
  result <- do.call(rbind, stacked)
  result$horizon <- factor(result$horizon, levels = paste0(horizons, "w"))
  result
}

nongs_atp <- nongs_est[nongs_est$tour == "ATP" & !is.na(nongs_est$peer_component), ]
nongs_wta <- nongs_est[nongs_est$tour == "WTA" & !is.na(nongs_est$peer_component), ]
message("  ATP CF sample: ", nrow(nongs_atp))
message("  WTA CF sample: ", nrow(nongs_wta))

stacked_atp <- stack_horizons_v2(nongs_atp, outcomes_base)
stacked_wta <- stack_horizons_v2(nongs_wta, outcomes_base)

run_cf <- function(stacked_data, tour_label) {
  results <- list(); rho_results <- list()
  for (ob in outcomes_base) {
    if (!ob %in% names(stacked_data)) next
    sdata <- stacked_data[!is.na(stacked_data[[ob]]) &
                          !is.na(stacked_data$pre_rank_pts) &
                          !is.na(stacked_data$player_age) &
                          !is.na(stacked_data$pre_elo) &
                          !is.na(stacked_data$v_hat), ]
    if (nrow(sdata) < 50) next
    fml <- as.formula(paste0(ob, " ~ got_ll:horizon + v_hat:horizon + ", ZPRE_FULL,
                             " | tourney_id + horizon"))
    mod <- tryCatch(feols(fml, data = sdata, cluster = ~player_id), error = function(e) NULL)
    if (is.null(mod)) next
    ct <- as.data.frame(coeftable(mod)); ct$var <- rownames(ct)
    for (h in HORIZON_LABS) {
      tn <- paste0("got_ll:horizon", h)
      rn <- paste0("v_hat:horizon", h)
      if (tn %in% ct$var) {
        rt <- ct[ct$var == tn, ]
        results[[paste0(ob, "_", h)]] <- data.frame(
          tour = tour_label, outcome = ob, horizon = h,
          coef = rt$Estimate, se = rt[["Std. Error"]],
          pval = rt[["Pr(>|t|)"]], stringsAsFactors = FALSE)
      }
      if (rn %in% ct$var) {
        rr <- ct[ct$var == rn, ]
        rho_results[[paste0(ob, "_", h)]] <- data.frame(
          tour = tour_label, outcome = ob, horizon = h,
          rho = rr$Estimate, rho_se = rr[["Std. Error"]],
          rho_pval = rr[["Pr(>|t|)"]], stringsAsFactors = FALSE)
      }
    }
  }
  list(results = do.call(rbind, results), rho_results = do.call(rbind, rho_results))
}

atp_cf <- run_cf(stacked_atp, "ATP")
wta_cf <- run_cf(stacked_wta, "WTA")

message("\n  === ATP Non-GS CF Results ===")
for (i in seq_len(nrow(atp_cf$results))) {
  r <- atp_cf$results[i, ]
  st <- ifelse(r$pval < 0.01, "***", ifelse(r$pval < 0.05, "**", ifelse(r$pval < 0.1, "*", "")))
  message(sprintf("  %s @ %s: %.1f%s (SE=%.1f, p=%.3f)", r$outcome, r$horizon, r$coef, st, r$se, r$pval))
}

message("\n  === WTA Non-GS CF Results ===")
for (i in seq_len(nrow(wta_cf$results))) {
  r <- wta_cf$results[i, ]
  st <- ifelse(r$pval < 0.01, "***", ifelse(r$pval < 0.05, "**", ifelse(r$pval < 0.1, "*", "")))
  message(sprintf("  %s @ %s: %.1f%s (SE=%.1f, p=%.3f)", r$outcome, r$horizon, r$coef, st, r$se, r$pval))
}


# ==============================================================================
# STEP 10: OUTCOME TABLES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 10: OUTCOME TABLES")
message(strrep("=", 70))

make_table <- function(cf_res, rho_res, tour_label) {
  outcomes_order <- c("points_change", "elo_change", "n_main_draws", "n_matches_250plus")
  out_labs <- c(points_change = "Ranking Pts $\\Delta$", elo_change = "Elo $\\Delta$",
                n_main_draws = "Main Draws", n_matches_250plus = "Matches 250+")
  L <- c("\\begin{tabular}{l*{5}{c}}", "\\toprule",
         paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"), "\\midrule")
  for (ob in outcomes_order) {
    ob_r <- cf_res[cf_res$outcome == ob, ]
    if (nrow(ob_r) == 0) next
    cv <- sv <- character()
    for (h in HORIZON_LABS) {
      r <- ob_r[ob_r$horizon == h, ]
      if (nrow(r) == 0) { cv <- c(cv, ""); sv <- c(sv, "") }
      else {
        cv <- c(cv, paste0(fmt(r$coef[1], 1), add_stars(r$pval[1])))
        sv <- c(sv, paste0("(", fmt(r$se[1], 1), ")"))
      }
    }
    L <- c(L, paste0(out_labs[ob], " & ", paste(cv, collapse = " & "), " \\\\"))
    L <- c(L, paste0(" & ", paste(sv, collapse = " & "), " \\\\[0.3em]"))
  }
  L <- c(L, "\\midrule")
  rv <- character()
  for (h in HORIZON_LABS) {
    rr <- rho_res[rho_res$outcome == "points_change" & rho_res$horizon == h, ]
    if (is.null(rr) || nrow(rr) == 0) rv <- c(rv, "")
    else rv <- c(rv, paste0(fmt(rr$rho[1], 2), add_stars(rr$rho_pval[1])))
  }
  L <- c(L, paste0("$\\hat{\\rho}$ (Pts) & ", paste(rv, collapse = " & "), " \\\\"))
  L <- c(L, "\\bottomrule", "\\end{tabular}")
  paste(L, collapse = "\n")
}

writeLines(make_table(atp_cf$results, atp_cf$rho_results, "ATP"),
           file.path(TABLES_DIR, "table_dynamic_stacked_nongs_atp.tex"))
writeLines(make_table(wta_cf$results, wta_cf$rho_results, "WTA"),
           file.path(TABLES_DIR, "table_dynamic_stacked_nongs_wta.tex"))
message("  Tables saved.")

# Save summary
writeLines(summary_log, file.path(OUTPUT_DIR, "35d_win_model_final_log.md"))
message("\nDONE.")
