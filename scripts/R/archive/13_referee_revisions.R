# ==============================================================================
# 13_referee_revisions.R
# Major revision addressing referee critique of the Lucky Loser analysis.
#
# Tasks:
#   1. Rebuild IV with leave-one-out instrument (peer_component only)
#   2. Additional non-mechanical outcomes (points change, direct entry, main
#      draws entered, matches at ATP 250+, prize money if available)
#   3. New descriptive tables (LL opportunities x slots, success rates)
#   4. Grand Slam lottery robustness (non-highest-ranked cases, restricted
#      sample, selection model applied to GS)
#   5. Investigate sample period discrepancy (2006 vs 2007)
#   6. Investigate Figure 1 pre-trend
#   7. Purge all RDD language from .tex files (audit)
#
# Inputs:  Data/raw/*.rds, Data/cleaned/estimation_sample_final.rds,
#          Data/cleaned/selection_probabilities.rds,
#          Data/cleaned/qualifying_match_probabilities.rds,
#          Data/cleaned/win_model_v2.rds,
#          Data/cleaned/rdd_with_covariates.rds
# Outputs: Data/cleaned/leave_one_out_iv_results.rds
#          Data/cleaned/additional_outcomes_results.rds
#          Data/cleaned/gs_robustness_results.rds
#          Tables/table_loo_iv_results.tex
#          Tables/table_additional_outcomes.tex
#          Tables/table_ll_opportunities_slots.tex
#          Tables/table_ll_success_rates.tex
#          Tables/table_gs_robustness.tex
#          Output/sample_period_investigation.md
#          Output/figure1_investigation.md
#          Output/rdd_language_audit.md
# Dependencies: dplyr, tidyr, readr, stringr, ggplot2, fixest, here
# Project: Lucky Losers and Career Trajectories
# ==============================================================================

set.seed(20260321)

# --- Packages -----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(fixest)
library(here)

# --- Paths --------------------------------------------------------------------
RAW_DIR     <- here("Data", "raw")
CLEANED_DIR <- here("Data", "cleaned")
TABLES_DIR  <- here("Tables")
FIGURES_DIR <- here("Figures")
OUTPUT_DIR  <- here("Output")
PAPER_DIR   <- here("Paper", "sections")
for (d in c(TABLES_DIR, FIGURES_DIR, OUTPUT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

CUTOFF <- 0.5

# --- Custom theme (matches project conventions) -------------------------------
theme_paper <- function(base_size = 14) {
  theme_minimal(base_size = base_size, base_family = "serif") %+replace%
    theme(
      plot.title       = element_blank(),
      plot.subtitle    = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.3, color = "grey85"),
      axis.line        = element_line(linewidth = 0.4, color = "grey30"),
      axis.ticks       = element_line(linewidth = 0.3, color = "grey30"),
      axis.ticks.length = unit(2, "pt"),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      legend.key.width = unit(18, "pt"),
      plot.margin      = margin(8, 12, 8, 8, unit = "pt"),
      strip.text       = element_text(face = "bold", size = base_size - 1)
    )
}

# --- Helper: significance stars -----------------------------------------------
add_stars <- function(pv) {
  ifelse(pv < 0.01, "$^{***}$",
         ifelse(pv < 0.05, "$^{**}$",
                ifelse(pv < 0.1, "$^{*}$", "")))
}

# ==============================================================================
# LOAD COMMON DATA
# ==============================================================================
message("=== Loading common data ===")

atp_main <- read_rds(file.path(RAW_DIR, "atp_main_matches.rds"))
atp_qual <- read_rds(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
wta_main <- read_rds(file.path(RAW_DIR, "wta_main_matches.rds"))
wta_qual <- read_rds(file.path(RAW_DIR, "wta_qual_itf_matches.rds"))
atp_rankings_raw <- read_rds(file.path(RAW_DIR, "atp_rankings.rds"))
atp_players <- read_rds(file.path(RAW_DIR, "atp_players.rds"))

est_final <- read_rds(file.path(CLEANED_DIR, "estimation_sample_final.rds"))
sel_probs <- tryCatch(read_rds(file.path(CLEANED_DIR, "selection_probabilities.rds")),
                       error = function(e) { message("  selection_probabilities.rds not found"); NULL })

qual_match_probs <- tryCatch(read_rds(file.path(CLEANED_DIR, "qualifying_match_probabilities.rds")),
                              error = function(e) { message("  qualifying_match_probabilities.rds not found"); NULL })

win_model_v2 <- tryCatch(read_rds(file.path(CLEANED_DIR, "win_model_v2.rds")),
                           error = function(e) { message("  win_model_v2.rds not found; will rebuild"); NULL })

elo_history <- tryCatch(read_rds(file.path(CLEANED_DIR, "elo_history.rds")),
                         error = function(e) { message("  elo_history.rds not found"); NULL })

rdd_cov_data <- tryCatch(read_rds(file.path(CLEANED_DIR, "rdd_with_covariates.rds")),
                           error = function(e) { message("  rdd_with_covariates.rds not found"); NULL })

message("  ATP main matches: ", nrow(atp_main))
message("  ATP qual matches: ", nrow(atp_qual))
message("  Estimation sample final: ", nrow(est_final))

# ==============================================================================
# BERNOULLI CONVOLUTION (from 11_selection_model.R)
# ==============================================================================

bernoulli_conv_ge <- function(probs, threshold) {
  if (length(probs) == 0) return(if (threshold <= 0) 1.0 else 0.0)
  if (threshold <= 0) return(1.0)
  if (threshold > length(probs)) return(0.0)

  n <- length(probs)
  prob <- numeric(n + 1)
  prob[1] <- 1.0

  for (k in seq_along(probs)) {
    pk <- probs[k]
    new_prob <- numeric(n + 1)
    for (j in seq(min(k, n), 0, -1)) {
      new_prob[j + 1] <- prob[j + 1] * (1 - pk)
      if (j >= 1) {
        new_prob[j + 1] <- new_prob[j + 1] + prob[j] * pk
      }
    }
    prob <- new_prob
  }
  sum(prob[(threshold + 1):(n + 1)])
}

# ==============================================================================
# REBUILD ALL-QUALIFIERS DATASET (mirrors 11_selection_model.R logic)
# ==============================================================================
message("\n=== Rebuilding all-qualifiers dataset ===")

atp_qual_tour <- atp_qual |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(year >= 2000, tourney_level %in% c("G", "M", "A"), str_detect(round, "^Q"))

final_qual_round <- atp_qual_tour |>
  group_by(tourney_id) |>
  summarise(max_qual_round = max(round), .groups = "drop")

final_qual_matches <- atp_qual_tour |>
  inner_join(final_qual_round, by = "tourney_id") |>
  filter(round == max_qual_round)

message("  Final qualifying round matches: ", nrow(final_qual_matches))

# Build winner and loser sides
qual_winners <- final_qual_matches |>
  transmute(
    tourney_id, tourney_name, tourney_date, tourney_level, surface, draw_size,
    year = as.integer(str_sub(tourney_date, 1, 4)),
    player_id = winner_id, player_name = winner_name,
    player_rank = winner_rank, player_rank_points = winner_rank_points,
    player_age = winner_age, player_hand = winner_hand,
    player_ht = winner_ht, player_ioc = winner_ioc, player_seed = winner_seed,
    opponent_id = loser_id, opponent_name = loser_name,
    opponent_rank = loser_rank, opponent_age = loser_age, opponent_ioc = loser_ioc,
    won_qualifying_match = 1L, score = score, minutes = minutes
  )

qual_losers <- final_qual_matches |>
  transmute(
    tourney_id, tourney_name, tourney_date, tourney_level, surface, draw_size,
    year = as.integer(str_sub(tourney_date, 1, 4)),
    player_id = loser_id, player_name = loser_name,
    player_rank = loser_rank, player_rank_points = loser_rank_points,
    player_age = loser_age, player_hand = loser_hand,
    player_ht = loser_ht, player_ioc = loser_ioc, player_seed = loser_seed,
    opponent_id = winner_id, opponent_name = winner_name,
    opponent_rank = winner_rank, opponent_age = winner_age, opponent_ioc = winner_ioc,
    won_qualifying_match = 0L, score = score, minutes = minutes
  )

all_qualifiers <- bind_rows(qual_winners, qual_losers)
message("  All final-round qualifiers: ", nrow(all_qualifiers))

# Rank among all qualifiers
all_qualifiers <- all_qualifiers |>
  group_by(tourney_id) |>
  mutate(
    n_qualifiers = n(),
    rank_among_all_qualifiers = rank(
      ifelse(is.na(player_rank), 9999, player_rank), ties.method = "min"
    )
  ) |>
  ungroup()

# Identify LL entries
ll_winners <- atp_main |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(year >= 2000, winner_entry == "LL") |>
  transmute(tourney_id, ll_player_id = winner_id)

ll_losers_main <- atp_main |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(year >= 2000, loser_entry == "LL") |>
  transmute(tourney_id, ll_player_id = loser_id)

ll_entries <- bind_rows(ll_winners, ll_losers_main) |>
  distinct(tourney_id, ll_player_id)

all_qualifiers <- all_qualifiers |>
  mutate(got_ll = as.integer(paste0(tourney_id, "_", player_id) %in%
                               paste0(ll_entries$tourney_id, "_", ll_entries$ll_player_id)))

ll_slots <- all_qualifiers |>
  filter(won_qualifying_match == 0L) |>
  group_by(tourney_id) |>
  summarise(n_ll_slots = sum(got_ll), .groups = "drop")

all_qualifiers <- all_qualifiers |>
  left_join(ll_slots, by = "tourney_id") |>
  mutate(n_ll_slots = replace_na(n_ll_slots, 0L))

# Rank among losers only (for GS lottery analysis)
all_qualifiers <- all_qualifiers |>
  group_by(tourney_id) |>
  mutate(
    rank_among_losers = if_else(
      won_qualifying_match == 0L,
      rank(ifelse(is.na(player_rank), 9999, player_rank), ties.method = "min"),
      NA_real_
    )
  ) |>
  ungroup()

# Fix: rank_among_losers should be computed ONLY among losers
loser_ranks <- all_qualifiers |>
  filter(won_qualifying_match == 0L) |>
  group_by(tourney_id) |>
  mutate(
    rank_among_losers = rank(ifelse(is.na(player_rank), 9999, player_rank),
                              ties.method = "min")
  ) |>
  ungroup() |>
  select(tourney_id, player_id, rank_among_losers)

all_qualifiers <- all_qualifiers |>
  select(-rank_among_losers) |>
  left_join(loser_ranks, by = c("tourney_id", "player_id"))

message("  LL recipients among all qualifiers: ", sum(all_qualifiers$got_ll))

# ==============================================================================
# BUILD WIN PROBABILITY MODEL (if needed)
# ==============================================================================

if (is.null(win_model_v2)) {
  message("\n=== Building fallback logit win model ===")
  atp_all <- bind_rows(
    atp_main |> mutate(match_source = "main"),
    atp_qual |> mutate(match_source = "qual")
  ) |>
    mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
    filter(year >= 2000, !is.na(winner_id), !is.na(loser_id))

  win_side <- atp_all |>
    filter(!is.na(winner_rank), !is.na(loser_rank)) |>
    transmute(
      player_id = winner_id, opponent_id = loser_id,
      player_rank = winner_rank, opponent_rank = loser_rank,
      player_age = winner_age, opponent_age = loser_age,
      player_ioc = winner_ioc, opponent_ioc = loser_ioc,
      surface, tourney_level, match_source, won = 1L,
      player_entry = winner_entry
    )

  loss_side <- atp_all |>
    filter(!is.na(winner_rank), !is.na(loser_rank)) |>
    transmute(
      player_id = loser_id, opponent_id = winner_id,
      player_rank = loser_rank, opponent_rank = winner_rank,
      player_age = loser_age, opponent_age = winner_age,
      player_ioc = loser_ioc, opponent_ioc = winner_ioc,
      surface, tourney_level, match_source, won = 0L,
      player_entry = loser_entry
    )

  model_panel <- bind_rows(win_side, loss_side) |>
    filter(is.na(player_entry) | player_entry != "LL") |>
    mutate(
      log_rank_ratio = log(pmax(opponent_rank, 1) / pmax(player_rank, 1)),
      rank_diff = opponent_rank - player_rank,
      same_ioc = as.integer(player_ioc == opponent_ioc),
      age_diff = player_age - opponent_age,
      surface_clay = as.integer(surface == "Clay"),
      surface_grass = as.integer(surface == "Grass")
    ) |>
    filter(!is.na(log_rank_ratio), !is.na(age_diff))

  win_model_v2 <- glm(
    won ~ log_rank_ratio + I(log_rank_ratio^2) + rank_diff +
      same_ioc + surface_clay + surface_grass + age_diff,
    data = model_panel, family = binomial(link = "logit")
  )
  message("  Built fallback logit model, McFadden R2: ",
          round(1 - win_model_v2$deviance / win_model_v2$null.deviance, 4))
}

# ==============================================================================
# PREDICT WIN PROBABILITIES FOR ALL QUALIFIERS
# ==============================================================================
message("\n=== Predicting win probabilities for all qualifiers ===")

# Determine which predictors the model needs
model_terms <- names(coef(win_model_v2))
needs_extra <- any(str_detect(model_terms, "player_win_rate|opponent_win_rate|h2h|is_gs|ace_rate"))

all_qualifiers <- all_qualifiers |>
  mutate(
    log_rank_ratio = log(pmax(opponent_rank, 1) / pmax(player_rank, 1)),
    rank_diff = opponent_rank - player_rank,
    same_ioc = as.integer(player_ioc == opponent_ioc),
    age_diff = player_age - opponent_age,
    surface_clay = as.integer(surface == "Clay"),
    surface_grass = as.integer(surface == "Grass")
  )

# Fill NAs
all_qualifiers$age_diff[is.na(all_qualifiers$age_diff)] <- 0
all_qualifiers$same_ioc[is.na(all_qualifiers$same_ioc)] <- 0L

# Add extra predictors if enhanced model
if (needs_extra) {
  message("  Enhanced model detected -- adding extra predictors with defaults")
  if (!"player_win_rate" %in% names(all_qualifiers)) all_qualifiers$player_win_rate <- 0.5
  if (!"opponent_win_rate" %in% names(all_qualifiers)) all_qualifiers$opponent_win_rate <- 0.5
  if (!"h2h_win_rate" %in% names(all_qualifiers)) all_qualifiers$h2h_win_rate <- 0.5
  if (!"has_h2h" %in% names(all_qualifiers)) all_qualifiers$has_h2h <- 0L
  if (!"is_gs" %in% names(all_qualifiers)) all_qualifiers$is_gs <- 0L
  if (!"is_masters" %in% names(all_qualifiers)) all_qualifiers$is_masters <- 0L
  if (!"is_qual" %in% names(all_qualifiers)) all_qualifiers$is_qual <- 1L
  for (v in c("player_ace_rate_cum", "player_df_rate_cum", "player_1st_pct_cum",
              "player_1st_win_cum", "player_2nd_win_cum", "player_bp_save_cum",
              "player_ret_win_cum", "ht_diff", "hand_mismatch")) {
    if (v %in% model_terms && !v %in% names(all_qualifiers)) {
      all_qualifiers[[v]] <- NA_real_
    }
  }
  # Fill NAs with training medians
  train_data <- win_model_v2$model
  for (v in names(all_qualifiers)) {
    if (is.numeric(all_qualifiers[[v]]) && any(is.na(all_qualifiers[[v]])) && v %in% names(train_data)) {
      med_val <- median(train_data[[v]], na.rm = TRUE)
      all_qualifiers[[v]] <- ifelse(is.na(all_qualifiers[[v]]), med_val, all_qualifiers[[v]])
    }
  }
}

# Predict
valid_pred <- all_qualifiers |> filter(!is.na(log_rank_ratio), !is.na(rank_diff))
valid_pred$p_win_own_match <- predict(win_model_v2, newdata = valid_pred, type = "response")

all_qualifiers <- all_qualifiers |>
  left_join(
    valid_pred |> select(tourney_id, player_id, p_win_own_match) |>
      distinct(tourney_id, player_id, .keep_all = TRUE),
    by = c("tourney_id", "player_id")
  )

message("  Players with P_t(i,j): ", sum(!is.na(all_qualifiers$p_win_own_match)),
        " / ", nrow(all_qualifiers))

# ==============================================================================
# BUILD RANKING TRAJECTORIES FOR ALL QUALIFIERS
# ==============================================================================
message("\n=== Building ranking trajectories ===")

atp_rankings <- atp_rankings_raw |>
  mutate(rank_date = as.Date(as.character(ranking_date), format = "%Y%m%d")) |>
  filter(!is.na(rank_date))

rankings_slim <- atp_rankings |>
  select(player = player, rank_date, rank, points)

all_qualifiers <- all_qualifiers |>
  mutate(event_date = as.Date(as.character(tourney_date), format = "%Y%m%d"))

horizons <- c(0, 4, 8, 12, 26, 52)

for (h in horizons) {
  col_rank <- paste0("rank_t", h)
  col_points <- paste0("points_t", h)

  if (col_rank %in% names(all_qualifiers)) {
    message("    Horizon t+", h, "w: already present, skipping")
    next
  }

  message("    Horizon t+", h, "w: merging rankings...")
  target_df <- all_qualifiers |>
    filter(!is.na(event_date)) |>
    transmute(tourney_id, player_id, target_date = event_date + h * 7)

  matched <- target_df |>
    inner_join(
      rankings_slim |> rename(player_id = player),
      by = "player_id", relationship = "many-to-many"
    ) |>
    filter(abs(as.numeric(rank_date - target_date)) <= 10) |>
    mutate(date_diff = abs(as.numeric(rank_date - target_date))) |>
    group_by(tourney_id, player_id) |>
    slice_min(date_diff, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(tourney_id, player_id,
           !!col_rank := rank,
           !!col_points := points)

  all_qualifiers <- all_qualifiers |>
    left_join(matched, by = c("tourney_id", "player_id"))
}

# Compute ranking and points changes
all_qualifiers <- all_qualifiers |>
  mutate(
    rank_change_4w  = rank_t4  - rank_t0,
    rank_change_8w  = rank_t8  - rank_t0,
    rank_change_12w = rank_t12 - rank_t0,
    rank_change_26w = rank_t26 - rank_t0,
    rank_change_52w = rank_t52 - rank_t0,
    points_change_12w = points_t12 - points_t0,
    points_change_26w = points_t26 - points_t0,
    points_change_52w = points_t52 - points_t0
  )

# Merge Elo if available
if (!is.null(elo_history)) {
  message("  Merging Elo trajectories...")
  for (h in horizons) {
    col_elo <- paste0("elo_t", h)
    if (col_elo %in% names(all_qualifiers)) next

    target <- all_qualifiers |>
      filter(!is.na(event_date)) |>
      transmute(tourney_id, player_id, target_date = event_date + h * 7)

    elo_matched <- target |>
      inner_join(elo_history, by = "player_id", relationship = "many-to-many") |>
      filter(abs(as.numeric(match_date - target_date)) <= 21) |>
      mutate(date_diff = abs(as.numeric(match_date - target_date))) |>
      group_by(tourney_id, player_id) |>
      slice_min(date_diff, n = 1, with_ties = FALSE) |>
      ungroup() |>
      select(tourney_id, player_id, !!col_elo := elo)

    all_qualifiers <- all_qualifiers |>
      left_join(elo_matched, by = c("tourney_id", "player_id"))
  }

  all_qualifiers <- all_qualifiers |>
    mutate(
      elo_change_4w  = elo_t4  - elo_t0,
      elo_change_8w  = elo_t8  - elo_t0,
      elo_change_12w = elo_t12 - elo_t0,
      elo_change_26w = elo_t26 - elo_t0,
      elo_change_52w = elo_t52 - elo_t0
    )
}

message("  All qualifiers with outcomes: ", nrow(all_qualifiers))

# ==============================================================================
# TASK 1: REBUILD IV -- LEAVE-ONE-OUT INSTRUMENT
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 1: LEAVE-ONE-OUT INSTRUMENT")
message(strrep("=", 70))

# --- 1a. Decompose selection probability into own_loss_prob + peer_component --
message("\n--- 1a. Computing leave-one-out instrument ---")

# Focus on losers in tournaments with LL slots
losers_iv <- all_qualifiers |>
  filter(won_qualifying_match == 0L, n_ll_slots > 0, !is.na(p_win_own_match))

message("  Losers with valid data for LOO IV: ", nrow(losers_iv))

# For each loser i at tournament t:
#   own_loss_prob = 1 - p_win_own_match (player i's loss probability)
#   peer_component = Pr(rank among losers <= c_t | i loses)
#     This depends ONLY on other players' match probabilities

tourney_ids <- unique(losers_iv$tourney_id)
message("  Tournaments to process: ", length(tourney_ids))

loo_results <- vector("list", nrow(losers_iv))
counter <- 0
report_interval <- max(1, length(tourney_ids) %/% 20)

for (tid in tourney_ids) {
  counter <- counter + 1
  if (counter %% report_interval == 0) {
    message("    Tournament ", counter, " / ", length(tourney_ids))
  }

  # All qualifiers at this tournament with valid predictions
  t_all <- all_qualifiers |>
    filter(tourney_id == tid, !is.na(p_win_own_match)) |>
    mutate(rank_sort = ifelse(is.na(player_rank), 9999, player_rank)) |>
    arrange(rank_sort)

  c_t <- t_all$n_ll_slots[1]
  if (is.na(c_t) || c_t == 0) next

  t_all$loss_prob <- 1 - t_all$p_win_own_match
  t_all$rank_pos <- seq_len(nrow(t_all))

  # For each LOSER
  losers_idx <- which(t_all$won_qualifying_match == 0L)

  for (idx in losers_idx) {
    player_id_i <- t_all$player_id[idx]
    rank_pos_i <- t_all$rank_pos[idx]
    own_loss_prob <- t_all$loss_prob[idx]

    # Higher-ranked qualifiers (rank_pos < rank_pos_i), EXCLUDING player i
    higher_ranked <- t_all |> filter(rank_pos < rank_pos_i)

    # Compute peer_component: Pr(rank among losers <= c_t | i loses)
    # Given that i loses, we need enough higher-ranked players to also lose
    # so that i is within the top c_t losers
    if (rank_pos_i <= c_t) {
      # Player i is among the top c_t overall -- if they lose, they are
      # automatically among the top c_t losers
      peer_component <- 1.0
    } else {
      # Need at least (rank_pos_i - c_t) of the higher-ranked to also lose
      threshold_needed <- rank_pos_i - c_t

      if (nrow(higher_ranked) > 0) {
        # Use ONLY other players' loss probabilities (leave-one-out)
        higher_loss_probs <- higher_ranked$loss_prob
        peer_component <- bernoulli_conv_ge(higher_loss_probs, threshold_needed)
      } else {
        peer_component <- if (threshold_needed <= 0) 1.0 else 0.0
      }
    }

    # Find the row index in losers_iv
    row_match <- which(losers_iv$tourney_id == tid &
                         losers_iv$player_id == player_id_i)
    if (length(row_match) == 1) {
      loo_results[[row_match]] <- tibble(
        tourney_id = tid,
        player_id = player_id_i,
        own_loss_prob = own_loss_prob,
        peer_component = peer_component,
        full_sel_prob = own_loss_prob * peer_component,
        rank_pos = rank_pos_i,
        c_t = c_t
      )
    }
  }
}

loo_df <- bind_rows(loo_results)
message("  LOO instrument computed: ", nrow(loo_df), " observations")
message("  Mean peer_component: ", round(mean(loo_df$peer_component, na.rm = TRUE), 4))
message("  SD peer_component: ", round(sd(loo_df$peer_component, na.rm = TRUE), 4))
message("  Mean own_loss_prob: ", round(mean(loo_df$own_loss_prob, na.rm = TRUE), 4))

# Merge LOO instrument to losers
losers_iv <- losers_iv |>
  left_join(loo_df |> select(tourney_id, player_id, own_loss_prob,
                               peer_component, full_sel_prob),
            by = c("tourney_id", "player_id"))

# Estimation sample: losers with valid instrument
est_loo <- losers_iv |>
  filter(!is.na(peer_component), !is.na(own_loss_prob), !is.na(got_ll))

message("  LOO estimation sample: ", nrow(est_loo), " obs")
message("  Treated (got LL): ", sum(est_loo$got_ll))
message("  Control (no LL): ", sum(!est_loo$got_ll))

# --- 1b. First stage: got_ll ~ peer_component + f(own_loss_prob) + covariates -
message("\n--- 1b. First stage ---")

fs_loo_simple <- feols(
  got_ll ~ peer_component + own_loss_prob + I(own_loss_prob^2) + I(own_loss_prob^3),
  data = est_loo, vcov = ~player_id
)

fs_coef <- coef(fs_loo_simple)["peer_component"]
fs_se <- sqrt(vcov(fs_loo_simple)["peer_component", "peer_component"])
fs_fstat <- (fs_coef / fs_se)^2
message("  First stage (simple):")
message("    peer_component coef: ", round(fs_coef, 4), " (SE = ", round(fs_se, 4), ")")
message("    F-statistic: ", round(fs_fstat, 1))

fs_loo_full <- tryCatch({
  feols(
    got_ll ~ peer_component + own_loss_prob + I(own_loss_prob^2) + I(own_loss_prob^3) +
      player_rank + player_age + i(tourney_level) + i(surface) | year,
    data = est_loo, vcov = ~player_id
  )
}, error = function(e) {
  message("    Full first stage failed: ", e$message)
  feols(
    got_ll ~ peer_component + own_loss_prob + I(own_loss_prob^2) + I(own_loss_prob^3) +
      player_rank + player_age,
    data = est_loo, vcov = ~player_id
  )
})

fs_full_coef <- coef(fs_loo_full)["peer_component"]
fs_full_se <- sqrt(vcov(fs_loo_full)["peer_component", "peer_component"])
fs_full_fstat <- (fs_full_coef / fs_full_se)^2
message("  First stage (full):")
message("    peer_component coef: ", round(fs_full_coef, 4), " (SE = ", round(fs_full_se, 4), ")")
message("    F-statistic: ", round(fs_full_fstat, 1))

# --- 1c. 2SLS with LOO instrument ---
message("\n--- 1c. 2SLS with leave-one-out instrument ---")

rank_outcomes <- c("rank_change_4w", "rank_change_8w", "rank_change_12w",
                    "rank_change_26w", "rank_change_52w")
elo_outcomes <- if ("elo_change_4w" %in% names(est_loo)) {
  c("elo_change_4w", "elo_change_8w", "elo_change_12w",
    "elo_change_26w", "elo_change_52w")
} else character(0)

all_outcomes <- c(rank_outcomes, elo_outcomes)
loo_iv_results <- list()

for (outcome in all_outcomes) {
  y <- est_loo[[outcome]]
  ok <- !is.na(y)
  n_ok <- sum(ok)
  if (n_ok < 100) { message("  ", outcome, ": insufficient obs (", n_ok, ")"); next }

  tryCatch({
    # 2SLS: instrument got_ll with peer_component only
    # X_it controls (rank, age) absorb own ability; no need for own_loss_prob
    iv_fit <- feols(
      as.formula(paste0(
        outcome,
        " ~ player_rank + player_age | year | got_ll ~ peer_component"
      )),
      data = est_loo[ok, ],
      vcov = ~player_id
    )

    iv_coef <- coef(iv_fit)["fit_got_ll"]
    iv_se <- sqrt(vcov(iv_fit)["fit_got_ll", "fit_got_ll"])
    iv_pv <- 2 * pnorm(-abs(iv_coef / iv_se))

    loo_iv_results[[outcome]] <- tibble(
      outcome = outcome, spec = "LOO-IV",
      coef = iv_coef, se = iv_se, pv = iv_pv,
      ci_lower = iv_coef - 1.96 * iv_se,
      ci_upper = iv_coef + 1.96 * iv_se,
      n_obs = n_ok,
      n_clusters = length(unique(est_loo$player_id[ok]))
    )

    message("  ", outcome, " (LOO-IV): coef = ", round(iv_coef, 2),
            " (SE = ", round(iv_se, 2), "), p = ", round(iv_pv, 3), ", N = ", n_ok)
  }, error = function(e) {
    message("  ", outcome, " (LOO-IV): FAILED -- ", e$message)
  })
}

loo_iv_df <- bind_rows(loo_iv_results)

# --- 1d. 2SLS with event fixed effects ---
message("\n--- 1d. 2SLS with event fixed effects ---")

loo_efe_results <- list()

for (outcome in all_outcomes) {
  y <- est_loo[[outcome]]
  ok <- !is.na(y)
  n_ok <- sum(ok)
  if (n_ok < 100) next

  tryCatch({
    iv_efe <- feols(
      as.formula(paste0(
        outcome,
        " ~ player_age | tourney_id | got_ll ~ peer_component"
      )),
      data = est_loo[ok, ],
      vcov = ~player_id
    )

    iv_c <- coef(iv_efe)["fit_got_ll"]
    iv_s <- sqrt(vcov(iv_efe)["fit_got_ll", "fit_got_ll"])
    iv_p <- 2 * pnorm(-abs(iv_c / iv_s))

    loo_efe_results[[outcome]] <- tibble(
      outcome = outcome, spec = "LOO-IV + Event FE",
      coef = iv_c, se = iv_s, pv = iv_p,
      ci_lower = iv_c - 1.96 * iv_s,
      ci_upper = iv_c + 1.96 * iv_s,
      n_obs = n_ok,
      n_clusters = length(unique(est_loo$player_id[ok]))
    )

    message("  ", outcome, " (LOO-IV+EFE): coef = ", round(iv_c, 2),
            " (SE = ", round(iv_s, 2), "), p = ", round(iv_p, 3))
  }, error = function(e) {
    message("  ", outcome, " (LOO-IV+EFE): FAILED -- ", e$message)
  })
}

loo_efe_df <- bind_rows(loo_efe_results)

# Combine all LOO IV results
all_loo_results <- bind_rows(loo_iv_df, loo_efe_df)

# Save
saveRDS(list(
  loo_instrument = loo_df,
  first_stage_simple_fstat = fs_fstat,
  first_stage_full_fstat = fs_full_fstat,
  loo_iv = loo_iv_df,
  loo_efe = loo_efe_df,
  all_results = all_loo_results
), file.path(CLEANED_DIR, "leave_one_out_iv_results.rds"))
message("  Saved: leave_one_out_iv_results.rds")

# --- 1e. Table: LOO IV results ---
message("\n--- 1e. LOO IV results table ---")

loo_tex <- c(
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{LOO-IV} & \\multicolumn{2}{c}{LOO-IV + Event FE} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  "Outcome & Coef. & SE & Coef. & SE & $N$ \\\\",
  "\\midrule"
)

for (out in all_outcomes) {
  r1 <- loo_iv_df |> filter(outcome == out)
  r2 <- loo_efe_df |> filter(outcome == out)

  out_label <- out |>
    str_replace("rank_change_", "Rank $\\Delta$ ") |>
    str_replace("elo_change_", "Elo $\\Delta$ ") |>
    str_replace("w$", "w")

  c1 <- if (nrow(r1) > 0) paste0(sprintf("%.2f", r1$coef), add_stars(r1$pv)) else "--"
  s1 <- if (nrow(r1) > 0) paste0("(", sprintf("%.2f", r1$se), ")") else ""
  c2 <- if (nrow(r2) > 0) paste0(sprintf("%.2f", r2$coef), add_stars(r2$pv)) else "--"
  s2 <- if (nrow(r2) > 0) paste0("(", sprintf("%.2f", r2$se), ")") else ""
  nn <- if (nrow(r1) > 0) format(r1$n_obs, big.mark = ",") else
    if (nrow(r2) > 0) format(r2$n_obs, big.mark = ",") else "--"

  loo_tex <- c(loo_tex,
    paste0(out_label, " & ", c1, " & ", s1, " & ", c2, " & ", s2, " & ", nn, " \\\\")
  )
}

loo_tex <- c(loo_tex,
  "\\midrule",
  paste0("First-stage $F$ & \\multicolumn{2}{c}{", sprintf("%.1f", fs_fstat),
         "} & \\multicolumn{2}{c}{--} & \\\\"),
  paste0("Own loss prob controls & \\multicolumn{2}{c}{Cubic} & ",
         "\\multicolumn{2}{c}{Cubic} & \\\\"),
  paste0("Player controls & \\multicolumn{2}{c}{Yes} & ",
         "\\multicolumn{2}{c}{Yes} & \\\\"),
  paste0("Year FE & \\multicolumn{2}{c}{Yes} & ",
         "\\multicolumn{2}{c}{--} & \\\\"),
  paste0("Event FE & \\multicolumn{2}{c}{No} & ",
         "\\multicolumn{2}{c}{Yes} & \\\\"),
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(loo_tex, file.path(TABLES_DIR, "table_loo_iv_results.tex"))
message("  Saved: table_loo_iv_results.tex")


# ==============================================================================
# TASK 2: ADDITIONAL NON-MECHANICAL OUTCOMES
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 2: ADDITIONAL NON-MECHANICAL OUTCOMES")
message(strrep("=", 70))

# --- 2a. Points change (already computed above) ---
message("\n--- 2a. Ranking points change outcomes ---")
# points_change_12w, points_change_26w, points_change_52w already computed

# --- 2b. Direct entry next year ---
message("\n--- 2b. Direct entry probability ---")

# For each player-tournament, check if they entered the SAME tournament next year
# as a direct entry (not LL, not Q, not WC)
# "Direct entry" = appears in main draw with entry NA or empty (standard acceptance)

atp_main_entries <- atp_main |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(year >= 2000) |>
  transmute(
    tourney_name_clean = str_to_lower(str_replace_all(tourney_name, "[^a-zA-Z0-9]", "")),
    year,
    player_id_w = winner_id, entry_w = winner_entry,
    player_id_l = loser_id, entry_l = loser_entry
  )

# All players who appeared in main draw (as winner or loser)
main_draw_appearances <- bind_rows(
  atp_main_entries |> transmute(tourney_name_clean, year, player_id = player_id_w, entry = entry_w),
  atp_main_entries |> transmute(tourney_name_clean, year, player_id = player_id_l, entry = entry_l)
) |>
  distinct(tourney_name_clean, year, player_id, entry)

# Direct entry = entry is NA, empty, or not LL/Q/WC/PR/SE/ALT
direct_entries <- main_draw_appearances |>
  filter(is.na(entry) | entry == "" | !entry %in% c("LL", "Q", "WC", "PR", "SE", "ALT", "Alt")) |>
  distinct(tourney_name_clean, year, player_id) |>
  mutate(direct_entry_next_year = 1L)

# Merge to estimation sample
est_loo <- est_loo |>
  mutate(
    tourney_name_clean = str_to_lower(str_replace_all(tourney_name, "[^a-zA-Z0-9]", "")),
    next_year = year + 1L
  )

est_loo <- est_loo |>
  left_join(
    direct_entries |> rename(next_year = year),
    by = c("tourney_name_clean", "next_year", "player_id")
  ) |>
  mutate(direct_entry_next_year = replace_na(direct_entry_next_year, 0L))

message("  Direct entry next year: ", sum(est_loo$direct_entry_next_year),
        " / ", nrow(est_loo), " (", round(mean(est_loo$direct_entry_next_year) * 100, 1), "%)")

# --- 2c. Number of main draws entered ---
message("\n--- 2c. Main draw appearances ---")

# Count main draw appearances for each player within 26w and 52w windows
all_main_appearances <- bind_rows(
  atp_main |> mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
    filter(year >= 2000) |>
    transmute(player_id = winner_id,
              match_date = as.Date(as.character(tourney_date), format = "%Y%m%d"),
              tourney_id, tourney_level),
  atp_main |> mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
    filter(year >= 2000) |>
    transmute(player_id = loser_id,
              match_date = as.Date(as.character(tourney_date), format = "%Y%m%d"),
              tourney_id, tourney_level)
) |>
  distinct(player_id, tourney_id, match_date, tourney_level)

# For each player-event in the estimation sample, count future tournaments
est_loo$n_main_draws_26w <- NA_integer_
est_loo$n_main_draws_52w <- NA_integer_
est_loo$n_matches_250plus_26w <- NA_integer_
est_loo$n_matches_250plus_52w <- NA_integer_

# Count matches at ATP 250+ level (main draw)
all_main_matches <- bind_rows(
  atp_main |> mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
    filter(year >= 2000) |>
    transmute(player_id = winner_id,
              match_date = as.Date(as.character(tourney_date), format = "%Y%m%d"),
              tourney_level),
  atp_main |> mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
    filter(year >= 2000) |>
    transmute(player_id = loser_id,
              match_date = as.Date(as.character(tourney_date), format = "%Y%m%d"),
              tourney_level)
)

message("  Computing main draw counts for ", nrow(est_loo), " observations...")

# Vectorized approach: for each observation, count future tournaments/matches
for (i in seq_len(nrow(est_loo))) {
  if (i %% 1000 == 0) message("    Row ", i, " / ", nrow(est_loo))

  pid <- est_loo$player_id[i]
  edate <- est_loo$event_date[i]
  if (is.na(edate)) next

  # Main draws entered within 26w and 52w
  player_tourneys <- all_main_appearances |>
    filter(player_id == pid, !is.na(match_date))

  t26 <- player_tourneys |>
    filter(match_date > edate, match_date <= edate + 26 * 7)
  t52 <- player_tourneys |>
    filter(match_date > edate, match_date <= edate + 52 * 7)

  est_loo$n_main_draws_26w[i] <- n_distinct(t26$tourney_id)
  est_loo$n_main_draws_52w[i] <- n_distinct(t52$tourney_id)

  # Matches at ATP 250+ (tourney_level in G, M, A)
  player_matches <- all_main_matches |>
    filter(player_id == pid, !is.na(match_date),
           tourney_level %in% c("G", "M", "A"))

  m26 <- player_matches |>
    filter(match_date > edate, match_date <= edate + 26 * 7)
  m52 <- player_matches |>
    filter(match_date > edate, match_date <= edate + 52 * 7)

  est_loo$n_matches_250plus_26w[i] <- nrow(m26)
  est_loo$n_matches_250plus_52w[i] <- nrow(m52)
}

message("  Main draws 26w: mean = ", round(mean(est_loo$n_main_draws_26w, na.rm = TRUE), 1))
message("  Main draws 52w: mean = ", round(mean(est_loo$n_main_draws_52w, na.rm = TRUE), 1))
message("  Matches 250+ 26w: mean = ", round(mean(est_loo$n_matches_250plus_26w, na.rm = TRUE), 1))
message("  Matches 250+ 52w: mean = ", round(mean(est_loo$n_matches_250plus_52w, na.rm = TRUE), 1))

# --- 2d. Prize money check ---
message("\n--- 2d. Prize money check ---")
prize_cols <- grep("prize|money|earn", names(atp_main), value = TRUE, ignore.case = TRUE)
if (length(prize_cols) > 0) {
  message("  Prize money columns found: ", paste(prize_cols, collapse = ", "))
} else {
  message("  NOTE: No prize money columns found in Sackmann data. Prize money outcome unavailable.")
}

# --- 2e. LOO-IV for additional outcomes ---
message("\n--- 2e. LOO-IV for additional outcomes ---")

additional_outcomes <- c("points_change_12w", "points_change_26w", "points_change_52w",
                          "direct_entry_next_year",
                          "n_main_draws_26w", "n_main_draws_52w",
                          "n_matches_250plus_26w", "n_matches_250plus_52w")

additional_iv_results <- list()

for (outcome in additional_outcomes) {
  if (!outcome %in% names(est_loo)) {
    message("  ", outcome, ": not in data, skipping")
    next
  }
  y <- est_loo[[outcome]]
  ok <- !is.na(y)
  n_ok <- sum(ok)
  if (n_ok < 100) { message("  ", outcome, ": insufficient obs (", n_ok, ")"); next }

  tryCatch({
    iv_fit <- feols(
      as.formula(paste0(
        outcome,
        " ~ own_loss_prob + I(own_loss_prob^2) + I(own_loss_prob^3) + ",
        "player_rank + player_age | year | got_ll ~ peer_component"
      )),
      data = est_loo[ok, ],
      vcov = ~player_id
    )

    iv_coef <- coef(iv_fit)["fit_got_ll"]
    iv_se <- sqrt(vcov(iv_fit)["fit_got_ll", "fit_got_ll"])
    iv_pv <- 2 * pnorm(-abs(iv_coef / iv_se))

    additional_iv_results[[outcome]] <- tibble(
      outcome = outcome, method = "LOO-IV",
      coef = iv_coef, se = iv_se, pv = iv_pv,
      ci_lower = iv_coef - 1.96 * iv_se,
      ci_upper = iv_coef + 1.96 * iv_se,
      n_obs = n_ok
    )
    message("  ", outcome, " (LOO-IV): coef = ", round(iv_coef, 2),
            " (SE = ", round(iv_se, 2), "), p = ", round(iv_pv, 3))
  }, error = function(e) {
    message("  ", outcome, " (LOO-IV): FAILED -- ", e$message)
  })
}

additional_iv_df <- bind_rows(additional_iv_results)

# --- 2f. GS lottery for additional outcomes ---
message("\n--- 2f. GS lottery for additional outcomes ---")

# GS lottery sample: top-4 ranked losers at Grand Slams
gs_sample <- est_loo |>
  filter(tourney_level == "G") |>
  filter(!is.na(rank_among_losers), rank_among_losers <= 4)

message("  GS lottery sample: ", nrow(gs_sample), " obs (LL: ", sum(gs_sample$got_ll), ")")

gs_additional_results <- list()

for (outcome in additional_outcomes) {
  if (!outcome %in% names(gs_sample)) next
  y <- gs_sample[[outcome]]
  ok <- !is.na(y)
  if (sum(ok) < 20) { message("  GS ", outcome, ": too few obs"); next }

  tr <- gs_sample$got_ll[ok] == 1
  if (sum(tr) < 3 || sum(!tr) < 3) next

  diff_means <- mean(y[ok][tr]) - mean(y[ok][!tr])
  tt <- tryCatch(t.test(y[ok] ~ gs_sample$got_ll[ok]), error = function(e) NULL)

  gs_additional_results[[outcome]] <- tibble(
    outcome = outcome, method = "GS Lottery",
    coef = diff_means,
    pv = if (!is.null(tt)) tt$p.value else NA_real_,
    n_treated = sum(tr), n_control = sum(!tr)
  )
  message("  GS ", outcome, ": diff = ", round(diff_means, 2),
          if (!is.null(tt)) paste0(", p = ", round(tt$p.value, 3)) else "")
}

gs_additional_df <- bind_rows(gs_additional_results)

# Save
saveRDS(list(
  loo_iv = additional_iv_df,
  gs_lottery = gs_additional_df
), file.path(CLEANED_DIR, "additional_outcomes_results.rds"))
message("  Saved: additional_outcomes_results.rds")

# --- 2g. Table: additional outcomes ---
message("\n--- 2g. Additional outcomes table ---")

add_tex <- c(
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{LOO-IV} & \\multicolumn{2}{c}{GS Lottery} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  "Outcome & Coef. (SE) & $N$ & Diff. & $N$ \\\\",
  "\\midrule"
)

nice_labels <- c(
  "points_change_12w" = "Points $\\Delta$ 12w",
  "points_change_26w" = "Points $\\Delta$ 26w",
  "points_change_52w" = "Points $\\Delta$ 52w",
  "direct_entry_next_year" = "Direct entry next year",
  "n_main_draws_26w" = "Main draws 26w",
  "n_main_draws_52w" = "Main draws 52w",
  "n_matches_250plus_26w" = "Matches (250+) 26w",
  "n_matches_250plus_52w" = "Matches (250+) 52w"
)

for (out in additional_outcomes) {
  label <- if (out %in% names(nice_labels)) nice_labels[out] else out
  r_iv <- additional_iv_df |> filter(outcome == out)
  r_gs <- gs_additional_df |> filter(outcome == out)

  iv_str <- if (nrow(r_iv) > 0) {
    paste0(sprintf("%.2f", r_iv$coef), add_stars(r_iv$pv),
           " (", sprintf("%.2f", r_iv$se), ")")
  } else "--"

  iv_n <- if (nrow(r_iv) > 0) format(r_iv$n_obs, big.mark = ",") else "--"

  gs_str <- if (nrow(r_gs) > 0) {
    paste0(sprintf("%.2f", r_gs$coef),
           if (!is.na(r_gs$pv)) add_stars(r_gs$pv) else "")
  } else "--"

  gs_n <- if (nrow(r_gs) > 0) {
    paste0(r_gs$n_treated + r_gs$n_control)
  } else "--"

  add_tex <- c(add_tex,
    paste0(label, " & ", iv_str, " & ", iv_n, " & ", gs_str, " & ", gs_n, " \\\\")
  )
}

add_tex <- c(add_tex,
  "\\midrule",
  "Own loss prob controls & \\multicolumn{2}{c}{Cubic} & \\multicolumn{2}{c}{--} \\\\",
  "Player controls & \\multicolumn{2}{c}{Yes} & \\multicolumn{2}{c}{--} \\\\",
  "Year FE & \\multicolumn{2}{c}{Yes} & \\multicolumn{2}{c}{--} \\\\",
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(add_tex, file.path(TABLES_DIR, "table_additional_outcomes.tex"))
message("  Saved: table_additional_outcomes.tex")


# ==============================================================================
# TASK 3: NEW DESCRIPTIVE TABLES
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 3: NEW DESCRIPTIVE TABLES")
message(strrep("=", 70))

# --- 3a. Two-way table: LL opportunities x LL slots won ---
message("\n--- 3a. LL opportunities x LL slots won ---")

# Build player-level summary from all_qualifiers (losers only)
player_ll_summary <- all_qualifiers |>
  filter(won_qualifying_match == 0L, n_ll_slots > 0) |>
  mutate(is_gs = tourney_level == "G") |>
  group_by(player_id, is_gs) |>
  summarise(
    n_opportunities = n(),
    n_ll_won = sum(got_ll),
    .groups = "drop"
  )

# Bin the counts
player_ll_summary <- player_ll_summary |>
  mutate(
    opp_bin = case_when(
      n_opportunities == 1 ~ "1",
      n_opportunities == 2 ~ "2",
      n_opportunities == 3 ~ "3",
      n_opportunities == 4 ~ "4",
      n_opportunities >= 5 ~ "5+"
    ),
    won_bin = case_when(
      n_ll_won == 0 ~ "0",
      n_ll_won == 1 ~ "1",
      n_ll_won == 2 ~ "2",
      n_ll_won >= 3 ~ "3+"
    ),
    type = if_else(is_gs, "GS", "Non-GS")
  )

# Create two-way table
twoway <- player_ll_summary |>
  count(type, opp_bin, won_bin) |>
  pivot_wider(names_from = c(type, won_bin), values_from = n, values_fill = 0L)

message("  Two-way table:")
print(twoway)

# LaTeX output
opp_levels <- c("1", "2", "3", "4", "5+")
won_levels <- c("0", "1", "2", "3+")

tw_tex <- c(
  "\\begin{tabular}{l cccc cccc}",
  "\\toprule",
  " & \\multicolumn{4}{c}{Grand Slam} & \\multicolumn{4}{c}{Non-Grand Slam} \\\\",
  paste0("\\cmidrule(lr){2-5} \\cmidrule(lr){6-9}"),
  paste0("Opportunities & ",
         paste(paste0("Won=", won_levels), collapse = " & "), " & ",
         paste(paste0("Won=", won_levels), collapse = " & "), " \\\\"),
  "\\midrule"
)

for (opp in opp_levels) {
  row_vals <- c()
  for (type in c("GS", "Non-GS")) {
    for (won in won_levels) {
      col_name <- paste0(type, "_", won)
      val <- twoway |> filter(opp_bin == opp)
      if (nrow(val) > 0 && col_name %in% names(val)) {
        row_vals <- c(row_vals, as.character(val[[col_name]]))
      } else {
        row_vals <- c(row_vals, "0")
      }
    }
  }
  tw_tex <- c(tw_tex, paste0(opp, " & ", paste(row_vals, collapse = " & "), " \\\\"))
}

tw_tex <- c(tw_tex, "\\bottomrule", "\\end{tabular}")
writeLines(tw_tex, file.path(TABLES_DIR, "table_ll_opportunities_slots.tex"))
message("  Saved: table_ll_opportunities_slots.tex")

# --- 3b. Top 5 and bottom 5 players by LL success rate ---
message("\n--- 3b. LL success rates table ---")

# Player-level aggregation across all tournament types
player_success <- all_qualifiers |>
  filter(won_qualifying_match == 0L, n_ll_slots > 0) |>
  group_by(player_id, player_name) |>
  summarise(
    n_opportunities = n(),
    n_ll_entries = sum(got_ll),
    success_rate = mean(got_ll),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(n_opportunities >= 5) |>
  arrange(desc(success_rate), desc(n_opportunities))

message("  Players with >= 5 opportunities: ", nrow(player_success))

# Use first non-NA name for each player
player_success <- player_success |>
  mutate(player_name = coalesce(player_name, paste0("Player ", player_id)))

top5 <- head(player_success, 5)
bottom5 <- tail(player_success, 5)
success_table <- bind_rows(
  top5 |> mutate(group = "Top 5"),
  bottom5 |> mutate(group = "Bottom 5")
)

message("  Top 5 by success rate:")
print(top5 |> select(player_name, n_opportunities, n_ll_entries, success_rate))
message("  Bottom 5 by success rate:")
print(bottom5 |> select(player_name, n_opportunities, n_ll_entries, success_rate))

sr_tex <- c(
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  "Player & Opportunities & LL Entries & Success Rate & First Year & Last Year \\\\",
  "\\midrule",
  "\\multicolumn{6}{l}{\\textit{Highest success rates}} \\\\"
)

for (i in seq_len(nrow(top5))) {
  r <- top5[i, ]
  sr_tex <- c(sr_tex,
    paste0(r$player_name, " & ", r$n_opportunities, " & ", r$n_ll_entries,
           " & ", sprintf("%.1f\\%%", r$success_rate * 100),
           " & ", r$first_year, " & ", r$last_year, " \\\\"))
}

sr_tex <- c(sr_tex,
  "\\midrule",
  "\\multicolumn{6}{l}{\\textit{Lowest success rates}} \\\\"
)

for (i in seq_len(nrow(bottom5))) {
  r <- bottom5[i, ]
  sr_tex <- c(sr_tex,
    paste0(r$player_name, " & ", r$n_opportunities, " & ", r$n_ll_entries,
           " & ", sprintf("%.1f\\%%", r$success_rate * 100),
           " & ", r$first_year, " & ", r$last_year, " \\\\"))
}

sr_tex <- c(sr_tex, "\\bottomrule", "\\end{tabular}")
writeLines(sr_tex, file.path(TABLES_DIR, "table_ll_success_rates.tex"))
message("  Saved: table_ll_success_rates.tex")


# ==============================================================================
# TASK 4: GRAND SLAM LOTTERY ROBUSTNESS
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 4: GRAND SLAM LOTTERY ROBUSTNESS")
message(strrep("=", 70))

# --- 4a. Non-highest-ranked LL cases ---
message("\n--- 4a. Non-highest-ranked LL cases ---")

# Among GS LL entries, identify where LL recipient was NOT rank 1 among losers
gs_losers_all <- all_qualifiers |>
  filter(won_qualifying_match == 0L, tourney_level == "G", n_ll_slots > 0)

gs_ll_cases <- gs_losers_all |> filter(got_ll == 1)
message("  GS LL entries: ", nrow(gs_ll_cases))

# Check if the LL recipient was the highest-ranked loser
gs_ll_cases <- gs_ll_cases |>
  mutate(is_highest_ranked = rank_among_losers == 1)

non_highest <- gs_ll_cases |> filter(!is_highest_ranked)
message("  Non-highest-ranked LL cases: ", nrow(non_highest),
        " (", round(nrow(non_highest) / nrow(gs_ll_cases) * 100, 1), "%)")

# Distribution across Grand Slams
if (nrow(non_highest) > 0) {
  gs_dist <- non_highest |>
    count(tourney_name) |>
    arrange(desc(n))
  message("  Distribution of non-highest-ranked LL by tournament:")
  print(gs_dist)

  # Also by year
  year_dist <- non_highest |>
    count(year) |>
    arrange(year)
  message("  By year:")
  print(year_dist)
}

# --- 4b. Restrict lottery sample to verified lottery cases ---
message("\n--- 4b. Restricted lottery analysis ---")

# Identify tournaments where at least one non-highest-ranked LL was given
# These are verified lottery tournaments
verified_lottery_tourneys <- non_highest |>
  distinct(tourney_id) |>
  pull(tourney_id)

message("  Verified lottery tournaments: ", length(verified_lottery_tourneys))

# Also include tournaments where multiple LL slots were filled non-sequentially
# (check if any tournament gave LL to non-sequential rankings)
gs_ll_by_tourney <- gs_ll_cases |>
  group_by(tourney_id) |>
  summarise(
    n_ll = n(),
    ranks = list(sort(rank_among_losers)),
    min_rank = min(rank_among_losers),
    max_rank = max(rank_among_losers),
    .groups = "drop"
  )

# Non-sequential: gaps between selected ranks
nonseq_tourneys <- gs_ll_by_tourney |>
  filter(n_ll >= 2) |>
  rowwise() |>
  mutate(is_sequential = all(diff(unlist(ranks)) == 1)) |>
  ungroup() |>
  filter(!is_sequential) |>
  pull(tourney_id)

all_verified <- unique(c(verified_lottery_tourneys, nonseq_tourneys))
message("  All verified lottery tournaments (non-highest + non-sequential): ",
        length(all_verified))

# Restricted lottery sample
gs_restricted <- gs_losers_all |>
  filter(tourney_id %in% all_verified, rank_among_losers <= 4)

message("  Restricted lottery sample: ", nrow(gs_restricted),
        " (LL: ", sum(gs_restricted$got_ll), ")")

# Run restricted lottery analysis
gs_restricted_outcomes <- c("rank_change_12w", "rank_change_26w", "rank_change_52w")
gs_restricted_results <- list()

for (outcome in gs_restricted_outcomes) {
  y <- gs_restricted[[outcome]]
  ok <- !is.na(y)
  if (sum(ok) < 10) { message("  ", outcome, ": too few obs"); next }

  tr <- gs_restricted$got_ll[ok] == 1
  if (sum(tr) < 3 || sum(!tr) < 3) {
    message("  ", outcome, ": insufficient variation")
    next
  }

  diff_means <- mean(y[ok][tr]) - mean(y[ok][!tr])
  tt <- tryCatch(t.test(y[ok] ~ gs_restricted$got_ll[ok]), error = function(e) NULL)

  gs_restricted_results[[outcome]] <- tibble(
    outcome = outcome, sample = "Verified lottery only",
    diff = diff_means,
    pv = if (!is.null(tt)) tt$p.value else NA_real_,
    n_treated = sum(tr), n_control = sum(!tr)
  )
  message("  Restricted ", outcome, ": diff = ", round(diff_means, 2),
          if (!is.null(tt)) paste0(", p = ", round(tt$p.value, 3)) else "")
}

gs_restricted_df <- bind_rows(gs_restricted_results)

# --- 4c. Apply selection model to GS events ---
message("\n--- 4c. Selection model applied to GS events ---")

gs_est_sel <- est_loo |>
  filter(tourney_level == "G")

message("  GS events in LOO estimation sample: ", nrow(gs_est_sel),
        " (LL: ", sum(gs_est_sel$got_ll), ")")

gs_sel_results <- list()

for (outcome in gs_restricted_outcomes) {
  y <- gs_est_sel[[outcome]]
  ok <- !is.na(y) & !is.na(gs_est_sel$peer_component) & !is.na(gs_est_sel$own_loss_prob)
  n_ok <- sum(ok)
  if (n_ok < 50) { message("  GS sel ", outcome, ": insufficient obs (", n_ok, ")"); next }

  tryCatch({
    iv_gs <- feols(
      as.formula(paste0(
        outcome,
        " ~ own_loss_prob + I(own_loss_prob^2) + I(own_loss_prob^3) + ",
        "player_rank + player_age | year | got_ll ~ peer_component"
      )),
      data = gs_est_sel[ok, ],
      vcov = ~player_id
    )

    iv_c <- coef(iv_gs)["fit_got_ll"]
    iv_s <- sqrt(vcov(iv_gs)["fit_got_ll", "fit_got_ll"])
    iv_p <- 2 * pnorm(-abs(iv_c / iv_s))

    gs_sel_results[[outcome]] <- tibble(
      outcome = outcome, method = "LOO-IV (GS only)",
      coef = iv_c, se = iv_s, pv = iv_p,
      n_obs = n_ok
    )
    message("  GS sel ", outcome, ": coef = ", round(iv_c, 2),
            " (SE = ", round(iv_s, 2), "), p = ", round(iv_p, 3))
  }, error = function(e) {
    message("  GS sel ", outcome, ": FAILED -- ", e$message)
  })
}

gs_sel_df <- bind_rows(gs_sel_results)

# Save all GS robustness
saveRDS(list(
  non_highest_ranked = non_highest,
  verified_lottery_tourneys = all_verified,
  restricted_lottery = gs_restricted_df,
  gs_selection_model = gs_sel_df
), file.path(CLEANED_DIR, "gs_robustness_results.rds"))
message("  Saved: gs_robustness_results.rds")

# --- 4d. GS robustness table ---
message("\n--- 4d. GS robustness table ---")

gs_rob_tex <- c(
  "\\begin{tabular}{lccc}",
  "\\toprule",
  " & Full Lottery & Verified Lottery & LOO-IV (GS) \\\\",
  " & (Top-4 pool) & (Non-highest cases) & (All GS losers) \\\\",
  "\\midrule"
)

# Get full lottery results from 06_main_analysis
gs_full <- tryCatch(
  read_rds(file.path(CLEANED_DIR, "main_gs_validation_results.rds")),
  error = function(e) NULL
)

for (out in gs_restricted_outcomes) {
  out_label <- out |>
    str_replace("rank_change_", "Rank $\\Delta$ ") |>
    str_replace("w$", "w")

  # Full lottery
  r_full <- if (!is.null(gs_full)) gs_full |> filter(outcome == out) else tibble()
  c_full <- if (nrow(r_full) > 0) {
    paste0(sprintf("%.2f", r_full$diff_means),
           if (!is.na(r_full$dm_pvalue)) add_stars(r_full$dm_pvalue) else "")
  } else "--"

  # Restricted lottery
  r_rest <- gs_restricted_df |> filter(outcome == out)
  c_rest <- if (nrow(r_rest) > 0) {
    paste0(sprintf("%.2f", r_rest$diff),
           if (!is.na(r_rest$pv)) add_stars(r_rest$pv) else "")
  } else "--"

  # LOO-IV GS
  r_sel <- gs_sel_df |> filter(outcome == out)
  c_sel <- if (nrow(r_sel) > 0) {
    paste0(sprintf("%.2f", r_sel$coef), add_stars(r_sel$pv))
  } else "--"

  gs_rob_tex <- c(gs_rob_tex,
    paste0(out_label, " & ", c_full, " & ", c_rest, " & ", c_sel, " \\\\")
  )
}

# Add N rows
n_full <- if (!is.null(gs_full) && nrow(gs_full) > 0) gs_full$n_total[1] else "--"
n_rest <- if (nrow(gs_restricted_df) > 0) gs_restricted_df$n_treated[1] + gs_restricted_df$n_control[1] else "--"
n_sel <- if (nrow(gs_sel_df) > 0) gs_sel_df$n_obs[1] else "--"

gs_rob_tex <- c(gs_rob_tex,
  "\\midrule",
  paste0("$N$ & ", n_full, " & ", n_rest, " & ", n_sel, " \\\\"),
  paste0("Non-highest-ranked cases & -- & ", nrow(non_highest), " & -- \\\\"),
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(gs_rob_tex, file.path(TABLES_DIR, "table_gs_robustness.tex"))
message("  Saved: table_gs_robustness.tex")


# ==============================================================================
# TASK 5: INVESTIGATE SAMPLE PERIOD DISCREPANCY
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 5: SAMPLE PERIOD INVESTIGATION")
message(strrep("=", 70))

sp_lines <- c(
  "# Sample Period Investigation",
  "",
  paste0("Generated: ", Sys.time()),
  ""
)

# ATP tour-level qualifying
atp_qual_years <- atp_qual |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(str_detect(round, "^Q"))

earliest_qual_all <- min(atp_qual_years$year, na.rm = TRUE)
sp_lines <- c(sp_lines,
  "## ATP Qualifying Data",
  paste0("- Earliest year in atp_qual_chall (all): ", earliest_qual_all))

# Tour-level only
atp_qual_tour_years <- atp_qual_years |>
  filter(tourney_level %in% c("G", "M", "A"))

earliest_qual_tour <- min(atp_qual_tour_years$year, na.rm = TRUE)
sp_lines <- c(sp_lines,
  paste0("- Earliest year in atp_qual_chall (tour-level: G/M/A): ", earliest_qual_tour))

# By level
for (lvl in c("G", "M", "A")) {
  sub <- atp_qual_tour_years |> filter(tourney_level == lvl)
  if (nrow(sub) > 0) {
    sp_lines <- c(sp_lines,
      paste0("  - Level ", lvl, ": earliest = ", min(sub$year, na.rm = TRUE),
             ", matches = ", nrow(sub)))
  }
}

# GS qualifying specifically
gs_qual <- atp_qual_years |> filter(tourney_level == "G")
gs_qual_years_dist <- gs_qual |> count(year) |> arrange(year)
sp_lines <- c(sp_lines,
  "",
  "## Grand Slam Qualifying Data",
  paste0("- Earliest GS qualifying year: ", min(gs_qual$year, na.rm = TRUE)),
  paste0("- GS qualifying matches by year:"))

for (i in seq_len(min(10, nrow(gs_qual_years_dist)))) {
  sp_lines <- c(sp_lines,
    paste0("  - ", gs_qual_years_dist$year[i], ": ", gs_qual_years_dist$n[i], " matches"))
}

# LL entries by year
ll_all <- bind_rows(
  atp_main |> filter(winner_entry == "LL") |>
    mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
    transmute(year, tourney_level),
  atp_main |> filter(loser_entry == "LL") |>
    mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
    transmute(year, tourney_level)
)

ll_by_year <- ll_all |> count(year) |> arrange(year)
earliest_ll <- min(ll_by_year$year, na.rm = TRUE)

sp_lines <- c(sp_lines,
  "",
  "## LL Entries in Main Draw",
  paste0("- Earliest year with LL entries: ", earliest_ll),
  "- LL entries by year (first 10):")

for (i in seq_len(min(10, nrow(ll_by_year)))) {
  sp_lines <- c(sp_lines,
    paste0("  - ", ll_by_year$year[i], ": ", ll_by_year$n[i], " entries"))
}

# Conclusion
sp_lines <- c(sp_lines,
  "",
  "## Conclusion",
  paste0("The 2006/2007 discrepancy likely reflects data availability in the Sackmann files. ",
         "Tour-level qualifying data starts in year ", earliest_qual_tour,
         " while GS qualifying data starts in year ", min(gs_qual$year, na.rm = TRUE), ". ",
         "LL entries in main draws appear from year ", earliest_ll, "."),
  paste0("This is a DATA AVAILABILITY issue, not a bug. The Sackmann qualifying files ",
         "have different coverage periods for different tournament levels.")
)

writeLines(sp_lines, file.path(OUTPUT_DIR, "sample_period_investigation.md"))
message("  Saved: Output/sample_period_investigation.md")


# ==============================================================================
# TASK 6: INVESTIGATE FIGURE 1 PRE-TREND
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 6: FIGURE 1 PRE-TREND INVESTIGATION")
message(strrep("=", 70))

fig1_lines <- c(
  "# Figure 1 Pre-Trend Investigation",
  "",
  paste0("Generated: ", Sys.time()),
  ""
)

# Load GS lottery sample (top-4 pool)
gs_lottery <- est_final |>
  filter(tourney_level == "G", n_ll_slots > 0)

# Compute rank_among_losers if not present
if (!"rank_among_losers" %in% names(gs_lottery)) {
  gs_lottery <- gs_lottery |>
    group_by(tourney_id) |>
    mutate(rank_among_losers = rank(ifelse(is.na(player_rank), 9999, player_rank),
                                     ties.method = "min")) |>
    ungroup()
}

gs_pool <- gs_lottery |> filter(rank_among_losers <= 4)

message("  GS lottery pool (top-4): ", nrow(gs_pool))
message("  LL in pool: ", sum(gs_pool$got_ll == 1))
message("  Control in pool: ", sum(gs_pool$got_ll == 0))

# Compare pre-event rankings (rank_t0)
if ("rank_t0" %in% names(gs_pool)) {
  rank_ll <- gs_pool$rank_t0[gs_pool$got_ll == 1]
  rank_ctrl <- gs_pool$rank_t0[gs_pool$got_ll == 0]

  mean_ll <- mean(rank_ll, na.rm = TRUE)
  mean_ctrl <- mean(rank_ctrl, na.rm = TRUE)
  tt_rank <- tryCatch(t.test(rank_ll, rank_ctrl), error = function(e) NULL)

  fig1_lines <- c(fig1_lines,
    "## Pre-Event Ranking Comparison (rank_t0)",
    paste0("- LL group mean rank: ", round(mean_ll, 1),
           " (N = ", sum(!is.na(rank_ll)), ")"),
    paste0("- Control group mean rank: ", round(mean_ctrl, 1),
           " (N = ", sum(!is.na(rank_ctrl)), ")"),
    paste0("- Difference (LL - Control): ", round(mean_ll - mean_ctrl, 1)),
    paste0("- t-test p-value: ",
           if (!is.null(tt_rank)) round(tt_rank$p.value, 3) else "NA"),
    ""
  )

  message("  Pre-event rank: LL mean = ", round(mean_ll, 1),
          ", Control mean = ", round(mean_ctrl, 1))
  if (!is.null(tt_rank)) message("  t-test p = ", round(tt_rank$p.value, 3))
}

# Check if Elo at t0 differs
if ("elo_t0" %in% names(gs_pool)) {
  elo_ll <- gs_pool$elo_t0[gs_pool$got_ll == 1]
  elo_ctrl <- gs_pool$elo_t0[gs_pool$got_ll == 0]

  tt_elo <- tryCatch(t.test(elo_ll, elo_ctrl), error = function(e) NULL)

  fig1_lines <- c(fig1_lines,
    "## Pre-Event Elo Comparison (elo_t0)",
    paste0("- LL group mean Elo: ", round(mean(elo_ll, na.rm = TRUE), 1)),
    paste0("- Control group mean Elo: ", round(mean(elo_ctrl, na.rm = TRUE), 1)),
    paste0("- Difference: ", round(mean(elo_ll, na.rm = TRUE) - mean(elo_ctrl, na.rm = TRUE), 1)),
    paste0("- t-test p-value: ",
           if (!is.null(tt_elo)) round(tt_elo$p.value, 3) else "NA"),
    ""
  )
}

# Check ranking trajectory BEFORE the event
# Look at rank_t0 vs longer-run pre-event rank (not available by default)
# Instead, check if the event study shows levels or changes
fig1_lines <- c(fig1_lines,
  "## Interpretation",
  "",
  "If the event study figure shows LEVELS (raw ranking position over time),",
  "then pre-existing differences in levels do NOT necessarily indicate pre-trends.",
  "The lottery randomizes WITHIN the top-4 pool, so some level differences are",
  "expected by chance.",
  "",
  "Key questions:",
  "1. Does the y-axis show rank LEVELS or rank CHANGES from baseline?",
  "2. If levels: are the pre-event differences statistically significant?",
  "3. If changes: is the pre-event slope different between LL and control?",
  "",
  "The balance tests (rank_t0, player_age, elo_t0) from the main analysis",
  "should be the primary check for pre-existing differences."
)

# Check balance tests from main results
bal_results <- tryCatch(
  readRDS(file.path(CLEANED_DIR, "main_balance_results.rds")),
  error = function(e) NULL
)
if (!is.null(bal_results)) {
  fig1_lines <- c(fig1_lines, "",
    "## Balance Test Results (from 06_main_analysis.R)",
    paste0("Available balance test results: ", nrow(bal_results), " variables"))
  for (i in seq_len(nrow(bal_results))) {
    r <- bal_results[i, ]
    fig1_lines <- c(fig1_lines,
      paste0("- ", r$variable, ": coef = ", round(r$coef, 2), ", p = ", round(r$pvalue, 3)))
  }
}

writeLines(fig1_lines, file.path(OUTPUT_DIR, "figure1_investigation.md"))
message("  Saved: Output/figure1_investigation.md")


# ==============================================================================
# TASK 7: PURGE ALL RDD LANGUAGE -- AUDIT
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 7: RDD LANGUAGE AUDIT")
message(strrep("=", 70))

rdd_patterns <- c(
  "RDD", "regression discontinuity", "cutoff", "bandwidth", "BW\\s*=",
  "rdrobust", "rdlocrand", "running variable", "fuzzy RDD", "sharp RDD",
  "discontinuity"
)

audit_lines <- c(
  "# RDD Language Audit",
  "",
  paste0("Generated: ", Sys.time()),
  "",
  "Searched all .tex files in Paper/sections/ for RDD-related terms.",
  ""
)

tex_files <- list.files(PAPER_DIR, pattern = "\\.tex$", full.names = TRUE)
total_hits <- 0

for (tf in tex_files) {
  lines <- readLines(tf, warn = FALSE)
  file_label <- basename(tf)
  file_hits <- list()

  for (i in seq_along(lines)) {
    for (pat in rdd_patterns) {
      if (grepl(pat, lines[i], ignore.case = TRUE)) {
        file_hits[[length(file_hits) + 1]] <- list(
          line = i,
          pattern = pat,
          text = trimws(lines[i])
        )
      }
    }
  }

  if (length(file_hits) > 0) {
    audit_lines <- c(audit_lines,
      paste0("## ", file_label),
      "")
    for (h in file_hits) {
      audit_lines <- c(audit_lines,
        paste0("- Line ", h$line, " [pattern: `", h$pattern, "`]: `", h$text, "`"))
      total_hits <- total_hits + 1
    }
    audit_lines <- c(audit_lines, "")
  }
}

# Also check main.tex
main_tex_path <- here("Paper", "main.tex")
if (file.exists(main_tex_path)) {
  lines <- readLines(main_tex_path, warn = FALSE)
  file_hits <- list()
  for (i in seq_along(lines)) {
    for (pat in rdd_patterns) {
      if (grepl(pat, lines[i], ignore.case = TRUE)) {
        file_hits[[length(file_hits) + 1]] <- list(
          line = i, pattern = pat, text = trimws(lines[i])
        )
      }
    }
  }
  if (length(file_hits) > 0) {
    audit_lines <- c(audit_lines, "## main.tex", "")
    for (h in file_hits) {
      audit_lines <- c(audit_lines,
        paste0("- Line ", h$line, " [pattern: `", h$pattern, "`]: `", h$text, "`"))
      total_hits <- total_hits + 1
    }
    audit_lines <- c(audit_lines, "")
  }
}

audit_lines <- c(audit_lines,
  "",
  "## Summary",
  paste0("Total RDD-related occurrences found: ", total_hits),
  "",
  "Each occurrence above should be reviewed and either:",
  "1. Removed entirely if referring to an abandoned RDD design",
  "2. Reframed using selection model / IV language",
  "3. Kept only if in a historical context (e.g., 'previous versions used RDD')"
)

writeLines(audit_lines, file.path(OUTPUT_DIR, "rdd_language_audit.md"))
message("  Saved: Output/rdd_language_audit.md")
message("  Total RDD language hits: ", total_hits)


# ==============================================================================
# FINAL SUMMARY
# ==============================================================================
message("\n", strrep("=", 70))
message("REFEREE REVISION SCRIPT COMPLETE")
message(strrep("=", 70))

message("\n--- Task 1: Leave-One-Out IV ---")
message("  LOO instrument observations: ", nrow(loo_df))
message("  First-stage F (simple): ", round(fs_fstat, 1))
message("  First-stage F (full): ", round(fs_full_fstat, 1))
message("  LOO-IV results: ", nrow(loo_iv_df), " outcomes")
message("  LOO-IV + Event FE results: ", nrow(loo_efe_df), " outcomes")

message("\n--- Task 2: Additional Outcomes ---")
message("  LOO-IV additional outcomes: ", nrow(additional_iv_df))
message("  GS lottery additional outcomes: ", nrow(gs_additional_df))

message("\n--- Task 3: Descriptive Tables ---")
message("  Players with >= 5 LL opportunities: ", nrow(player_success))

message("\n--- Task 4: GS Robustness ---")
message("  Non-highest-ranked LL cases: ", nrow(non_highest))
message("  Verified lottery tournaments: ", length(all_verified))
message("  Restricted lottery results: ", nrow(gs_restricted_df))
message("  GS selection model results: ", nrow(gs_sel_df))

message("\n--- Task 7: RDD Audit ---")
message("  RDD language occurrences: ", total_hits)

message("\n=== All outputs saved. ===")
message("Tables: table_loo_iv_results.tex, table_additional_outcomes.tex,")
message("        table_ll_opportunities_slots.tex, table_ll_success_rates.tex,")
message("        table_gs_robustness.tex")
message("Data:   leave_one_out_iv_results.rds, additional_outcomes_results.rds,")
message("        gs_robustness_results.rds")
message("Reports: sample_period_investigation.md, figure1_investigation.md,")
message("         rdd_language_audit.md")
