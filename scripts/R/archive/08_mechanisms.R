# ==============================================================================
# 08_mechanisms.R
# Mechanism analysis: competitiveness index, tournament access, treatment dose
# Project: Lucky Losers and Career Trajectories
# ==============================================================================

set.seed(20260321)

# --- Packages -----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(rdrobust)
library(fixest)
library(here)

has_rdlocrand <- requireNamespace("rdlocrand", quietly = TRUE)
if (has_rdlocrand) library(rdlocrand)

# --- Paths --------------------------------------------------------------------
RAW_DIR     <- here("Data", "raw")
CLEANED_DIR <- here("Data", "cleaned")
TABLES_DIR  <- here("Tables")
FIGURES_DIR <- here("Figures")
OUTPUT_DIR  <- here("Output")
for (d in c(TABLES_DIR, FIGURES_DIR, OUTPUT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

CUTOFF <- 0.5

# --- Load data ----------------------------------------------------------------
message("=== Loading data ===")
est <- read_rds(file.path(CLEANED_DIR, "estimation_sample_final.rds"))
main_matches <- read_rds(file.path(RAW_DIR, "atp_main_matches.rds"))
qual_matches <- read_rds(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))

rdd <- est |> filter(n_ll_slots > 0)

# ==============================================================================
# PART A: COMPETITIVENESS INDEX
# ==============================================================================
message("\n", strrep("=", 70))
message("PART A: COMPETITIVENESS INDEX")
message(strrep("=", 70))

# Step 1: Build match-level dataset for first-stage logit
# Stack all matches into player-opponent pairs with outcome = win(1)/loss(0)
# EXCLUDE LL main draw matches to avoid circularity

all_matches <- bind_rows(main_matches, qual_matches) |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(year >= 2000, year <= 2024)

# Create player-level observations: each match generates 2 rows (winner, loser)
match_panel <- bind_rows(
  all_matches |>
    transmute(
      match_id    = paste(tourney_id, match_num, sep = "_"),
      tourney_id, tourney_date, surface, tourney_level, year,
      player_id   = winner_id,
      opponent_id = loser_id,
      player_rank = winner_rank,
      opponent_rank = loser_rank,
      player_age  = winner_age,
      opponent_age = loser_age,
      player_ioc  = winner_ioc,
      opponent_ioc = loser_ioc,
      player_entry = winner_entry,
      won = 1L
    ),
  all_matches |>
    transmute(
      match_id    = paste(tourney_id, match_num, sep = "_"),
      tourney_id, tourney_date, surface, tourney_level, year,
      player_id   = loser_id,
      opponent_id = winner_id,
      player_rank = loser_rank,
      opponent_rank = winner_rank,
      player_age  = loser_age,
      opponent_age = winner_age,
      player_ioc  = loser_ioc,
      opponent_ioc = winner_ioc,
      player_entry = loser_entry,
      won = 0L
    )
)

# Remove LL main draw matches to avoid circularity
match_panel <- match_panel |>
  filter(is.na(player_entry) | player_entry != "LL")

message("  Match panel: ", nrow(match_panel), " player-match obs (LL excluded)")

# Step 2: Construct covariates for the logit
# Rank difference (log scale), same nationality, surface indicators
match_panel <- match_panel |>
  mutate(
    log_rank_ratio = log(pmax(opponent_rank, 1) / pmax(player_rank, 1)),
    rank_diff      = opponent_rank - player_rank,
    same_ioc       = as.integer(player_ioc == opponent_ioc),
    surface_clay   = as.integer(surface == "Clay"),
    surface_grass  = as.integer(surface == "Grass"),
    age_diff       = player_age - opponent_age
  ) |>
  filter(!is.na(player_rank), !is.na(opponent_rank))

message("  After dropping missing ranks: ", nrow(match_panel), " obs")

# Step 3: Estimate the win probability model (logit)
# P(win) = f(log_rank_ratio, same_nationality, surface, age_diff)
message("  Estimating first-stage logit...")
win_model <- glm(
  won ~ log_rank_ratio + rank_diff + same_ioc +
    surface_clay + surface_grass + age_diff +
    I(log_rank_ratio^2),
  data = match_panel,
  family = binomial(link = "logit")
)

message("  Logit AUC proxy (McFadden R2): ",
        round(1 - win_model$deviance / win_model$null.deviance, 4))

# Step 4: Compute competitiveness index for each player-tournament in the RDD sample
# C_it = E_j[P(i beats j | X_it, X_jt)] averaged over a reference opponent pool
# Reference pool: all players in the same tournament's main draw

message("  Computing competitiveness index for RDD sample...")

# For each player-tournament in the estimation sample, we need:
# (a) The player's rank at the tournament date
# (b) A reference pool of opponents (main draw players at that tournament)

# Get main draw participant ranks at each tournament
# Stack winners and losers into a single opponent pool per tournament
main_draw_w <- all_matches |>
  filter(!is.na(winner_rank)) |>
  transmute(tourney_id, opponent_id = winner_id, opponent_rank = winner_rank,
            opponent_age = winner_age, opponent_ioc = winner_ioc)

main_draw_l <- all_matches |>
  filter(!is.na(loser_rank)) |>
  transmute(tourney_id, opponent_id = loser_id, opponent_rank = loser_rank,
            opponent_age = loser_age, opponent_ioc = loser_ioc)

main_draw_players <- bind_rows(main_draw_w, main_draw_l) |>
  distinct(tourney_id, opponent_id, .keep_all = TRUE)

# For each player in the RDD sample, compute their expected win probability
# against each main draw participant at the same tournament
comp_index <- rdd |>
  select(player_id, tourney_id, player_rank, player_age, player_ioc,
         surface, got_ll, R_tilde) |>
  inner_join(main_draw_players, by = "tourney_id", relationship = "many-to-many") |>
  filter(opponent_id != player_id) |>
  mutate(
    log_rank_ratio = log(pmax(opponent_rank, 1) / pmax(player_rank, 1)),
    rank_diff      = opponent_rank - player_rank,
    same_ioc       = as.integer(player_ioc == opponent_ioc),
    surface_clay   = as.integer(surface == "Clay"),
    surface_grass  = as.integer(surface == "Grass"),
    age_diff       = player_age - opponent_age
  ) |>
  filter(!is.na(log_rank_ratio))

# Predict win probability for each player-opponent pair
comp_index$p_win <- predict(win_model, newdata = comp_index, type = "response")

# Aggregate: C_it = mean predicted win probability across all opponents at tournament
C_it <- comp_index |>
  group_by(player_id, tourney_id) |>
  summarise(
    competitiveness_t0 = mean(p_win, na.rm = TRUE),
    n_opponents        = n(),
    .groups = "drop"
  )

message("  Competitiveness index computed for ", nrow(C_it), " player-tournament obs")
message("  Mean C_it: ", round(mean(C_it$competitiveness_t0, na.rm = TRUE), 4),
        "  SD: ", round(sd(C_it$competitiveness_t0, na.rm = TRUE), 4))

# Step 5: Now compute C_it at future horizons
# We need player ranks at t+12w, t+26w, etc. to recompute the index
# Use ranking data already merged in the estimation sample

compute_future_comp <- function(horizon_suffix, rank_var) {
  future_data <- rdd |>
    select(player_id, tourney_id, player_age, player_ioc,
           surface, got_ll, R_tilde) |>
    mutate(player_rank_future = rdd[[rank_var]]) |>
    filter(!is.na(player_rank_future)) |>
    inner_join(main_draw_players, by = "tourney_id", relationship = "many-to-many") |>
    filter(opponent_id != player_id) |>
    mutate(
      log_rank_ratio = log(pmax(opponent_rank, 1) / pmax(player_rank_future, 1)),
      rank_diff      = opponent_rank - player_rank_future,
      same_ioc       = as.integer(player_ioc == opponent_ioc),
      surface_clay   = as.integer(surface == "Clay"),
      surface_grass  = as.integer(surface == "Grass"),
      age_diff       = player_age - opponent_age
    ) |>
    filter(!is.na(log_rank_ratio))

  future_data$p_win <- predict(win_model, newdata = future_data, type = "response")

  future_data |>
    group_by(player_id, tourney_id) |>
    summarise(
      !!paste0("competitiveness_", horizon_suffix) := mean(p_win, na.rm = TRUE),
      .groups = "drop"
    )
}

message("  Computing future competitiveness indices...")
C_12w <- compute_future_comp("t12", "rank_t12")
C_26w <- compute_future_comp("t26", "rank_t26")
C_52w <- compute_future_comp("t52", "rank_t52")

# Merge all competitiveness measures back to RDD sample
rdd_comp <- rdd |>
  left_join(C_it, by = c("player_id", "tourney_id")) |>
  left_join(C_12w, by = c("player_id", "tourney_id")) |>
  left_join(C_26w, by = c("player_id", "tourney_id")) |>
  left_join(C_52w, by = c("player_id", "tourney_id")) |>
  mutate(
    comp_change_12w = competitiveness_t12 - competitiveness_t0,
    comp_change_26w = competitiveness_t26 - competitiveness_t0,
    comp_change_52w = competitiveness_t52 - competitiveness_t0
  )

message("  Competitiveness change 26w -- mean: ",
        round(mean(rdd_comp$comp_change_26w, na.rm = TRUE), 4),
        ", SD: ", round(sd(rdd_comp$comp_change_26w, na.rm = TRUE), 4))

# Step 6: RDD on competitiveness index change
message("\n--- RDD on Competitiveness Index ---")
comp_outcomes <- c("comp_change_12w", "comp_change_26w", "comp_change_52w")
comp_results <- list()

# Covariates
covs_comp <- as.matrix(rdd_comp[, c("player_rank", "player_age", "elo_t0")])
covs_comp[is.na(covs_comp)] <- apply(covs_comp, 2, median, na.rm = TRUE)[col(covs_comp)[is.na(covs_comp)]]

for (outcome in comp_outcomes) {
  y <- rdd_comp[[outcome]]
  ok <- !is.na(y)
  if (sum(ok) < 200) {
    message("  ", outcome, ": insufficient obs (", sum(ok), ")")
    next
  }

  tryCatch({
    rd <- rdrobust(y = y[ok], x = rdd_comp$R_tilde[ok], c = CUTOFF,
                   fuzzy = rdd_comp$got_ll[ok],
                   covs = covs_comp[ok, ],
                   cluster = rdd_comp$player_id[ok],
                   kernel = "triangular", bwselect = "mserd")
    comp_results[[outcome]] <- tibble(
      outcome   = outcome,
      coef      = rd$coef["Conventional", 1],
      se        = rd$se["Robust", 1],
      pval      = rd$pv["Robust", 1],
      ci_lo     = rd$ci["Robust", 1],
      ci_hi     = rd$ci["Robust", 2],
      bw        = rd$bws[1, 1],
      eff_n     = rd$N_h[1] + rd$N_h[2]
    )
    message("  ", outcome, ": LATE = ", round(rd$coef["Conventional", 1], 4),
            " (p = ", round(rd$pv["Robust", 1], 3), ")")
  }, error = function(e) {
    message("  ", outcome, " FAILED: ", e$message)
  })
}

comp_results_df <- bind_rows(comp_results)
saveRDS(comp_results_df, file.path(CLEANED_DIR, "mechanism_competitiveness.rds"))
saveRDS(rdd_comp, file.path(CLEANED_DIR, "rdd_with_competitiveness.rds"))

# Step 7: Also compute C_it excluding ranking from the logit (ranking-free version)
# This separates "ability" from ranking mechanical effects
message("\n  Computing ranking-free competitiveness index...")
win_model_nrank <- glm(
  won ~ same_ioc + surface_clay + surface_grass + age_diff,
  data = match_panel,
  family = binomial(link = "logit")
)

# This model has much less predictive power (no ranking info)
message("  Ranking-free logit McFadden R2: ",
        round(1 - win_model_nrank$deviance / win_model_nrank$null.deviance, 4))

# Compute ranking-free C_it at t0 and t26
comp_nrank_t0 <- comp_index |>
  mutate(p_win_nrank = predict(win_model_nrank, newdata = pick(everything()), type = "response")) |>
  group_by(player_id, tourney_id) |>
  summarise(comp_nrank_t0 = mean(p_win_nrank, na.rm = TRUE), .groups = "drop")

rdd_comp <- rdd_comp |>
  left_join(comp_nrank_t0, by = c("player_id", "tourney_id"))

message("  Ranking-free C_it -- mean: ",
        round(mean(rdd_comp$comp_nrank_t0, na.rm = TRUE), 4))

# ==============================================================================
# PART B: TOURNAMENT ACCESS AS OUTCOME
# ==============================================================================
message("\n", strrep("=", 70))
message("PART B: TOURNAMENT ACCESS OUTCOMES")
message(strrep("=", 70))

# Count main draw entries in the 26 and 52 weeks after the qualifying event
# A player who gets LL points may gain direct entry to more tournaments

# Get all main draw appearances per player-date
# Stack winners and losers
main_draw_app_w <- all_matches |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(year >= 2000) |>
  transmute(player_id = winner_id, tourney_id, tourney_date, tourney_level)

main_draw_app_l <- all_matches |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(year >= 2000) |>
  transmute(player_id = loser_id, tourney_id, tourney_date, tourney_level)

main_draw_appearances <- bind_rows(main_draw_app_w, main_draw_app_l) |>
  distinct(player_id, tourney_id, .keep_all = TRUE) |>
  mutate(match_date = as.Date(as.character(tourney_date), format = "%Y%m%d"))

message("  Main draw appearances: ", nrow(main_draw_appearances), " player-tournament obs")

# For each player-tournament in the RDD sample, count subsequent main draw entries
message("  Counting post-event main draw entries...")

rdd_access <- rdd |>
  mutate(event_dt = as.Date(as.character(tourney_date), format = "%Y%m%d")) |>
  select(player_id, tourney_id, event_dt, got_ll, R_tilde, n_ll_slots,
         player_rank, player_age, elo_t0)

# Join to future main draw appearances
access_counts <- rdd_access |>
  inner_join(
    main_draw_appearances |>
      select(player_id, future_tourney = tourney_id,
             future_date = match_date, future_level = tourney_level),
    by = "player_id",
    relationship = "many-to-many"
  ) |>
  filter(future_date > event_dt, future_tourney != tourney_id) |>
  mutate(weeks_after = as.numeric(difftime(future_date, event_dt, units = "weeks"))) |>
  group_by(player_id, tourney_id) |>
  summarise(
    main_draws_26w = sum(weeks_after <= 26, na.rm = TRUE),
    main_draws_52w = sum(weeks_after <= 52, na.rm = TRUE),
    main_draws_26w_250plus = sum(weeks_after <= 26 & future_level %in% c("A", "M", "G"), na.rm = TRUE),
    .groups = "drop"
  )

rdd_access <- rdd_access |>
  left_join(access_counts, by = c("player_id", "tourney_id")) |>
  mutate(across(starts_with("main_draws"), ~replace_na(., 0)))

message("  Main draws 26w -- mean: ", round(mean(rdd_access$main_draws_26w), 2),
        ", SD: ", round(sd(rdd_access$main_draws_26w), 2))
message("  Main draws 52w -- mean: ", round(mean(rdd_access$main_draws_52w), 2),
        ", SD: ", round(sd(rdd_access$main_draws_52w), 2))

# RDD on tournament access
message("\n--- RDD on Tournament Access ---")
access_outcomes <- c("main_draws_26w", "main_draws_52w", "main_draws_26w_250plus")
access_results <- list()

covs_acc <- as.matrix(rdd_access[, c("player_rank", "player_age", "elo_t0")])
covs_acc[is.na(covs_acc)] <- apply(covs_acc, 2, median, na.rm = TRUE)[col(covs_acc)[is.na(covs_acc)]]

for (outcome in access_outcomes) {
  y <- rdd_access[[outcome]]
  ok <- !is.na(y)

  tryCatch({
    rd <- rdrobust(y = y[ok], x = rdd_access$R_tilde[ok], c = CUTOFF,
                   fuzzy = rdd_access$got_ll[ok],
                   covs = covs_acc[ok, ],
                   cluster = rdd_access$player_id[ok],
                   kernel = "triangular", bwselect = "mserd")
    access_results[[outcome]] <- tibble(
      outcome   = outcome,
      coef      = rd$coef["Conventional", 1],
      se        = rd$se["Robust", 1],
      pval      = rd$pv["Robust", 1],
      ci_lo     = rd$ci["Robust", 1],
      ci_hi     = rd$ci["Robust", 2],
      bw        = rd$bws[1, 1],
      eff_n     = rd$N_h[1] + rd$N_h[2]
    )
    message("  ", outcome, ": LATE = ", round(rd$coef["Conventional", 1], 2),
            " (p = ", round(rd$pv["Robust", 1], 3), ")")
  }, error = function(e) {
    message("  ", outcome, " FAILED: ", e$message)
  })
}

access_results_df <- bind_rows(access_results)
saveRDS(access_results_df, file.path(CLEANED_DIR, "mechanism_tournament_access.rds"))
saveRDS(rdd_access, file.path(CLEANED_DIR, "rdd_with_access.rds"))


# ==============================================================================
# PART C: HETEROGENEITY BY TREATMENT DOSE
# ==============================================================================
message("\n", strrep("=", 70))
message("PART C: HETEROGENEITY BY TREATMENT DOSE")
message(strrep("=", 70))

# Among LL recipients, how does the effect vary by how many matches they won?
# This is NOT an RDD -- it's descriptive within the treated group
# But we can also define dose as points earned and test the extensive margin

# Approach 1: Among the RDD sample, interact treatment with dose
# The dose is only observed for treated players, so we define:
#   - Zero dose (control): got_ll = 0
#   - Low dose: got_ll = 1 & ll_matches_won == 0 (lost first round)
#   - High dose: got_ll = 1 & ll_matches_won >= 1 (won at least one match)

rdd_dose <- rdd |>
  mutate(
    dose_group = case_when(
      !got_ll ~ "control",
      got_ll & ll_matches_won == 0 ~ "low_dose",
      got_ll & ll_matches_won >= 1 ~ "high_dose",
      TRUE ~ NA_character_
    ),
    # Points earned in main draw (approximate from matches won and tourney level)
    # ATP 250: R1 = 20, R2 = 45, QF = 90, SF = 150, F = 250, W = 500 (approx)
    # ATP 500: multiply by ~1.5
    # Masters: R1 = 10, R2 = 45, R3 = 90, QF = 180, SF = 360
    # Grand Slam: R1 = 10, R2 = 45, R3 = 90, R4 = 180, QF = 360, SF = 720
    estimated_bonus_points = case_when(
      !got_ll ~ 0,
      tourney_level == "A" & ll_matches_won == 0 ~ 0,
      tourney_level == "A" & ll_matches_won == 1 ~ 20,
      tourney_level == "A" & ll_matches_won == 2 ~ 45,
      tourney_level == "A" & ll_matches_won >= 3 ~ 90,
      tourney_level == "M" & ll_matches_won == 0 ~ 10,
      tourney_level == "M" & ll_matches_won == 1 ~ 45,
      tourney_level == "M" & ll_matches_won == 2 ~ 90,
      tourney_level == "M" & ll_matches_won >= 3 ~ 180,
      tourney_level == "G" & ll_matches_won == 0 ~ 10,
      tourney_level == "G" & ll_matches_won == 1 ~ 45,
      tourney_level == "G" & ll_matches_won == 2 ~ 90,
      tourney_level == "G" & ll_matches_won >= 3 ~ 180,
      TRUE ~ 0
    ),
    # Points as fraction of player's total
    points_pct_boost = estimated_bonus_points / pmax(points_t0, 1)
  )

message("  Dose distribution among LL recipients:")
dose_tab <- rdd_dose |> filter(got_ll == 1) |> count(dose_group, ll_matches_won)
print(dose_tab, n = 20)

message("\n  Estimated bonus points among LL recipients:")
message("  Mean: ", round(mean(rdd_dose$estimated_bonus_points[rdd_dose$got_ll == 1], na.rm = TRUE), 1))
message("  Median: ", median(rdd_dose$estimated_bonus_points[rdd_dose$got_ll == 1], na.rm = TRUE))
message("  Max: ", max(rdd_dose$estimated_bonus_points[rdd_dose$got_ll == 1], na.rm = TRUE))

message("  Points % boost among LL recipients:")
message("  Mean: ", round(mean(rdd_dose$points_pct_boost[rdd_dose$got_ll == 1], na.rm = TRUE) * 100, 1), "%")

# Approach 2: Split the RDD by whether the LL won at least one match
# This tests: does the extensive margin (any win vs. first-round loss) matter?
# Caveat: this is endogenous (winning is not random), so interpret as descriptive

message("\n--- Descriptive: Outcomes by Dose Group ---")

dose_means <- rdd_dose |>
  filter(!is.na(dose_group)) |>
  group_by(dose_group) |>
  summarise(
    n = n(),
    rank_change_12w = mean(rank_change_12w, na.rm = TRUE),
    rank_change_26w = mean(rank_change_26w, na.rm = TRUE),
    rank_change_52w = mean(rank_change_52w, na.rm = TRUE),
    elo_change_26w  = mean(elo_change_26w, na.rm = TRUE),
    .groups = "drop"
  )

message("\n  Dose group means:")
print(as.data.frame(dose_means))

# Approach 3: RDD-style analysis with dose interaction
# Among all sample, estimate: Y = alpha + beta1*got_ll + beta2*got_ll*high_dose + ...
# Using OLS within bandwidth for interpretability (not rdrobust which can't handle
# interactions easily)
message("\n--- OLS within bandwidth: Dose Interactions ---")

# Use MSE-optimal bandwidth from the main analysis
main_fuzzy <- tryCatch(
  readRDS(file.path(CLEANED_DIR, "main_fuzzy_rdd_results.rds")),
  error = function(e) NULL
)
if (!is.null(main_fuzzy)) {
  bw_row <- main_fuzzy |>
    dplyr::filter(outcome == "rank_change_26w", covariates == "Yes")
  main_bw <- if (nrow(bw_row) > 0) bw_row$bw[1] else 3.0
  message("  Loaded MSE-optimal bandwidth from main results: ", round(main_bw, 2))
} else {
  main_bw <- 3.0
  message("  WARNING: main_fuzzy_rdd_results.rds not found, using fallback bw = 3.0")
}

bw_sample <- rdd_dose |>
  filter(abs(R_tilde - CUTOFF) <= main_bw) |>
  mutate(
    high_dose = as.integer(dose_group == "high_dose"),
    R_centered = R_tilde - CUTOFF
  )

message("  Within-bandwidth sample: ", nrow(bw_sample), " obs")

# OLS with dose interaction
if (nrow(bw_sample) > 50) {
  dose_ols <- feols(
    rank_change_26w ~ got_ll + got_ll:high_dose + R_centered +
      player_rank + player_age + elo_t0 | 0,
    data = bw_sample,
    vcov = ~player_id
  )

  message("\n  Dose OLS results (rank_change_26w):")
  ct <- summary(dose_ols)$coeftable
  # Find the got_ll coefficient (may be "got_ll" or "got_llTRUE")
  ll_coef_name <- grep("^got_ll$", rownames(ct), value = TRUE)
  if (length(ll_coef_name) == 1) {
    message("  got_ll (extensive margin): ", round(ct[ll_coef_name, 1], 2),
            " (p = ", round(ct[ll_coef_name, 4], 3), ")")
  }
  # Find the interaction coefficient
  int_coef_name <- grep("got_ll.*high_dose|high_dose.*got_ll", rownames(ct), value = TRUE)
  if (length(int_coef_name) >= 1) {
    message("  got_ll x high_dose (intensive margin): ",
            round(ct[int_coef_name[1], 1], 2),
            " (p = ", round(ct[int_coef_name[1], 4], 3), ")")
  }

  saveRDS(dose_ols, file.path(CLEANED_DIR, "mechanism_dose_ols.rds"))
}

# Save dose data
saveRDS(rdd_dose, file.path(CLEANED_DIR, "rdd_with_dose.rds"))
saveRDS(dose_means, file.path(CLEANED_DIR, "mechanism_dose_means.rds"))

# ==============================================================================
# PART D: MECHANISM SUMMARY TABLE
# ==============================================================================
message("\n", strrep("=", 70))
message("PART D: MECHANISM SUMMARY")
message(strrep("=", 70))

# Build a combined table of mechanism results
mechanism_rows <- list()

# Competitiveness index results
if (nrow(comp_results_df) > 0) {
  mechanism_rows <- c(mechanism_rows, list(
    comp_results_df |> mutate(category = "Competitiveness Index")
  ))
}

# Tournament access results
if (nrow(access_results_df) > 0) {
  mechanism_rows <- c(mechanism_rows, list(
    access_results_df |> mutate(category = "Tournament Access")
  ))
}

mechanism_summary <- bind_rows(mechanism_rows)
saveRDS(mechanism_summary, file.path(CLEANED_DIR, "mechanism_summary.rds"))

# --- LaTeX table for mechanisms -----------------------------------------------
message("  Writing mechanism table...")

mech_tex <- c(
  "\\begin{tabular}{lrrrrrr}",
  "\\toprule",
  "Outcome & LATE & SE & $p$-value & 95\\% CI & BW & Eff.\\ $N$ \\\\",
  "\\midrule",
  "\\multicolumn{7}{l}{\\textit{Panel A: Competitiveness Index (Fuzzy RDD)}} \\\\"
)

if (nrow(comp_results_df) > 0) {
  for (i in seq_len(nrow(comp_results_df))) {
    r <- comp_results_df[i, ]
    label <- gsub("comp_change_", "$\\\\Delta C_{", r$outcome)
    label <- gsub("w$", "w}$", label)
    mech_tex <- c(mech_tex, sprintf(
      "%s & %.4f & %.4f & %.3f & [%.4f, %.4f] & %.1f & %d \\\\",
      label, r$coef, r$se, r$pval, r$ci_lo, r$ci_hi, r$bw, r$eff_n
    ))
  }
} else {
  mech_tex <- c(mech_tex, "\\multicolumn{7}{c}{(estimation failed)} \\\\")
}

mech_tex <- c(mech_tex,
  "\\midrule",
  "\\multicolumn{7}{l}{\\textit{Panel B: Tournament Access (Fuzzy RDD)}} \\\\"
)

if (nrow(access_results_df) > 0) {
  labels_acc <- c(
    main_draws_26w = "Main draws (26w)",
    main_draws_52w = "Main draws (52w)",
    main_draws_26w_250plus = "Main draws 250+ (26w)"
  )
  for (i in seq_len(nrow(access_results_df))) {
    r <- access_results_df[i, ]
    label <- labels_acc[r$outcome]
    mech_tex <- c(mech_tex, sprintf(
      "%s & %.2f & %.2f & %.3f & [%.2f, %.2f] & %.1f & %d \\\\",
      label, r$coef, r$se, r$pval, r$ci_lo, r$ci_hi, r$bw, r$eff_n
    ))
  }
}

mech_tex <- c(mech_tex,
  "\\midrule",
  "\\multicolumn{7}{l}{\\textit{Panel C: Treatment Dose (Descriptive Means)}} \\\\",
  sprintf("Control & \\multicolumn{6}{l}{$\\bar{Y}_{26w}$ = %.1f ($N$ = %d)} \\\\",
          dose_means$rank_change_26w[dose_means$dose_group == "control"],
          dose_means$n[dose_means$dose_group == "control"]),
  sprintf("LL, first-round loss & \\multicolumn{6}{l}{$\\bar{Y}_{26w}$ = %.1f ($N$ = %d)} \\\\",
          dose_means$rank_change_26w[dose_means$dose_group == "low_dose"],
          dose_means$n[dose_means$dose_group == "low_dose"]),
  sprintf("LL, won $\\geq$1 match & \\multicolumn{6}{l}{$\\bar{Y}_{26w}$ = %.1f ($N$ = %d)} \\\\",
          dose_means$rank_change_26w[dose_means$dose_group == "high_dose"],
          dose_means$n[dose_means$dose_group == "high_dose"]),
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(mech_tex, file.path(TABLES_DIR, "table6_mechanisms.tex"))
message("  Saved: Tables/table6_mechanisms.tex")

# --- Summary output -----------------------------------------------------------
summary_lines <- c(
  "# Mechanism Analysis Results",
  paste0("Date: ", Sys.time()),
  "",
  "## Competitiveness Index",
  "First-stage logit: P(win) ~ log_rank_ratio + rank_diff + same_ioc + surface + age_diff",
  paste0("McFadden R2: ", round(1 - win_model$deviance / win_model$null.deviance, 4)),
  ""
)

if (nrow(comp_results_df) > 0) {
  for (i in seq_len(nrow(comp_results_df))) {
    r <- comp_results_df[i, ]
    summary_lines <- c(summary_lines, sprintf(
      "  %s: LATE = %.4f (p = %.3f)", r$outcome, r$coef, r$pval
    ))
  }
}

summary_lines <- c(summary_lines, "",
  "## Tournament Access",
  ""
)

if (nrow(access_results_df) > 0) {
  for (i in seq_len(nrow(access_results_df))) {
    r <- access_results_df[i, ]
    summary_lines <- c(summary_lines, sprintf(
      "  %s: LATE = %.2f (p = %.3f)", r$outcome, r$coef, r$pval
    ))
  }
}

summary_lines <- c(summary_lines, "",
  "## Treatment Dose (Descriptive)",
  ""
)

for (i in seq_len(nrow(dose_means))) {
  d <- dose_means[i, ]
  summary_lines <- c(summary_lines, sprintf(
    "  %s (N=%d): rank_26w = %.1f, rank_52w = %.1f, elo_26w = %.1f",
    d$dose_group, d$n, d$rank_change_26w, d$rank_change_52w, d$elo_change_26w
  ))
}

writeLines(summary_lines, file.path(OUTPUT_DIR, "mechanism_summary.md"))
message("\n=== DONE: mechanism_summary.md written ===")
