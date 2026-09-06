# ==============================================================================
# 05_first_rdd.R
# First RDD estimation: local linear RDD at the LL ranking cutoff
# Uses rdrobust for continuity-based inference
# Also reports simple difference-in-means at adjacent mass points
# Project: Lucky Losers and Career Trajectories
# ==============================================================================

library(readr)
library(dplyr)
library(stringr)
library(rdrobust)

CLEANED_DIR <- here::here("Data", "cleaned")

# --- 1. Load data -------------------------------------------------------------
message("=== Loading estimation sample ===")
est <- read_rds(file.path(CLEANED_DIR, "estimation_sample_final.rds"))

# Restrict to tournaments with at least one LL slot
rdd_sample <- est |>
  filter(n_ll_slots > 0)

message("  RDD sample: ", nrow(rdd_sample), " observations")
message("  Treated (got LL): ", sum(rdd_sample$got_ll))
message("  Control: ", sum(!rdd_sample$got_ll))

# --- 2. First stage: does ranking predict LL entry? ---------------------------
message("\n=== FIRST STAGE: Ranking predicts LL entry ===")

# Sharp RDD first stage
fs <- rdrobust(
  y = rdd_sample$got_ll,
  x = rdd_sample$R_tilde,
  c = 0.5,  # cutoff between 0 (treated) and 1 (control)
  cluster = rdd_sample$player_id,
  kernel = "triangular",
  bwselect = "mserd"
)
message("  First stage RDD:")
summary(fs)

# Simple tabulation: LL rate by R_tilde
message("\n  LL rate by running variable:")
rdd_sample |>
  filter(abs(R_tilde) <= 5) |>
  group_by(R_tilde) |>
  summarise(
    n = n(),
    pct_ll = mean(got_ll),
    .groups = "drop"
  ) |>
  print()

# --- 3. Balance test: pre-treatment covariates at cutoff ----------------------
message("\n=== BALANCE TESTS ===")

balance_vars <- c("player_rank", "player_age", "elo_t0")

for (var in balance_vars) {
  y_val <- rdd_sample[[var]]
  if (all(is.na(y_val))) next

  valid <- !is.na(y_val)
  tryCatch({
    bal <- rdrobust(
      y = y_val[valid],
      x = rdd_sample$R_tilde[valid],
      c = 0.5,
      cluster = rdd_sample$player_id[valid],
      kernel = "triangular",
      bwselect = "mserd"
    )
    message("  ", var, ": coef = ", round(bal$coef[1], 2),
            ", p = ", round(bal$pv[1], 3),
            ", bw = ", round(bal$bws[1], 2))
  }, error = function(e) {
    message("  ", var, ": rdrobust failed (", e$message, ")")
  })
}

# --- 4. Main RDD results: ranking outcomes ------------------------------------
message("\n=== MAIN RDD RESULTS ===")

outcomes <- c(
  "rank_change_4w", "rank_change_8w", "rank_change_12w",
  "rank_change_26w", "rank_change_52w",
  "points_change_12w", "points_change_26w",
  "elo_change_12w", "elo_change_26w", "elo_change_52w"
)

results <- list()

for (outcome in outcomes) {
  y_val <- rdd_sample[[outcome]]
  valid <- !is.na(y_val)

  if (sum(valid) < 100) {
    message("  ", outcome, ": insufficient observations (", sum(valid), ")")
    next
  }

  tryCatch({
    rd <- rdrobust(
      y = y_val[valid],
      x = rdd_sample$R_tilde[valid],
      c = 0.5,
      cluster = rdd_sample$player_id[valid],
      kernel = "triangular",
      bwselect = "mserd"
    )

    results[[outcome]] <- tibble(
      outcome = outcome,
      coef = rd$coef[1],        # conventional
      se = rd$se[3],            # robust bias-corrected
      pv = rd$pv[3],            # robust p-value
      ci_lower = rd$ci[3, 1],   # robust CI
      ci_upper = rd$ci[3, 2],
      bw = rd$bws[1],
      n_left = rd$N[1],
      n_right = rd$N[2],
      n_eff_left = rd$N_h[1],
      n_eff_right = rd$N_h[2]
    )

    message("  ", outcome,
            ": coef = ", round(rd$coef[1], 2),
            " (robust SE = ", round(rd$se[3], 2),
            ", p = ", round(rd$pv[3], 3),
            ", bw = ", round(rd$bws[1], 2),
            ", N_eff = ", rd$N_h[1], "+", rd$N_h[2], ")")
  }, error = function(e) {
    message("  ", outcome, ": rdrobust failed (", e$message, ")")
  })
}

# --- 5. Results table ---------------------------------------------------------
message("\n=== RESULTS SUMMARY TABLE ===")
if (length(results) > 0) {
  results_df <- bind_rows(results)
  print(results_df, width = 200)

  # Save
  write_rds(results_df, file.path(CLEANED_DIR, "rdd_results_first_pass.rds"))
}

# --- 6. Simple difference at adjacent mass points (nonparametric) -------------
message("\n=== SIMPLE DIFF AT CUTOFF: R_tilde = 0 vs R_tilde = 1 ===")

at_cutoff <- rdd_sample |>
  filter(R_tilde %in% c(0, 1)) |>
  mutate(treated = as.integer(R_tilde <= 0))

for (outcome in c("rank_change_12w", "rank_change_26w", "elo_change_12w", "elo_change_26w")) {
  t_test <- tryCatch(
    t.test(at_cutoff[[outcome]] ~ at_cutoff$treated),
    error = function(e) NULL
  )
  if (!is.null(t_test)) {
    message("  ", outcome,
            ": diff = ", round(t_test$estimate[2] - t_test$estimate[1], 2),
            " (p = ", round(t_test$p.value, 3), ")",
            " [treated mean = ", round(t_test$estimate[2], 2),
            ", control mean = ", round(t_test$estimate[1], 2), "]")
  }
}

# --- 7. Fuzzy RDD: instrument with ranking, treat = got_ll --------------------
message("\n=== FUZZY RDD (ranking as instrument for actual LL entry) ===")

for (outcome in c("rank_change_12w", "rank_change_26w", "elo_change_12w")) {
  y_val <- rdd_sample[[outcome]]
  valid <- !is.na(y_val)

  if (sum(valid) < 100) next

  tryCatch({
    frd <- rdrobust(
      y = y_val[valid],
      x = rdd_sample$R_tilde[valid],
      c = 0.5,
      fuzzy = rdd_sample$got_ll[valid],
      cluster = rdd_sample$player_id[valid],
      kernel = "triangular",
      bwselect = "mserd"
    )

    message("  FUZZY ", outcome,
            ": coef = ", round(frd$coef[1], 2),
            " (robust SE = ", round(frd$se[3], 2),
            ", p = ", round(frd$pv[3], 3),
            ", bw = ", round(frd$bws[1], 2), ")")
  }, error = function(e) {
    message("  FUZZY ", outcome, ": failed (", e$message, ")")
  })
}

message("\n=== First RDD estimation complete ===")
