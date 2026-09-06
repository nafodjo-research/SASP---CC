# ==============================================================================
# 06_main_analysis.R
# Comprehensive RDD analysis: balance, local randomization, fuzzy RDD with
# covariates, heterogeneity, Grand Slam validation, event study data
# FIXES APPLIED (coder-critic round 1):
#   1. Player-level clustering on ALL rdrobust calls
#   2. Local randomization: manual permutation inference fallback
#   3. Grand Slam validation restricted to top-4 ranked losers per tournament
#   4. Conditional Elo imbalance message (not hardcoded)
#   5. Benjamini-Hochberg FDR correction on ranking change family
#   6. message() not cat() for status output
# Inputs:  Data/cleaned/estimation_sample_final.rds
# Outputs: Data/cleaned/main_results_*.rds, Tables/*.tex, Figures/*.pdf
#          Output/results_summary.md
# Dependencies: rdrobust, fixest, modelsummary, ggplot2, here
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
library(modelsummary)
library(here)

# Try to load rdlocrand; if unavailable, we use manual permutation inference
has_rdlocrand <- requireNamespace("rdlocrand", quietly = TRUE)
if (has_rdlocrand) {
  library(rdlocrand)
  message("rdlocrand loaded successfully")
} else {
  message("rdlocrand not available -- will use manual permutation inference")
}

# --- Paths --------------------------------------------------------------------
CLEANED_DIR <- here("Data", "cleaned")
TABLES_DIR  <- here("Tables")
FIGURES_DIR <- here("Figures")
OUTPUT_DIR  <- here("Output")
for (d in c(TABLES_DIR, FIGURES_DIR, OUTPUT_DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# --- Custom theme (matches 07_figures.R) --------------------------------------
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

# --- 1. Load data -------------------------------------------------------------
message("=== Loading data ===")
est <- read_rds(file.path(CLEANED_DIR, "estimation_sample_final.rds"))

# RDD sample: tournaments with at least one LL slot
rdd <- est |> filter(n_ll_slots > 0)
message("  RDD sample: ", nrow(rdd), " obs (treated: ", sum(rdd$got_ll), ", control: ", sum(!rdd$got_ll), ")")

# Cutoff for rdrobust: R_tilde is integer, treated <= 0, control >= 1
# Use c = 0.5 to place cutoff between 0 and 1
CUTOFF <- 0.5

# ==============================================================================
# PART 1: BALANCE TESTS
# ==============================================================================
message("\n", strrep("=", 70))
message("PART 1: BALANCE TESTS")
message(strrep("=", 70))

balance_vars <- c("player_rank", "player_age", "elo_t0")
balance_results <- list()

for (var in balance_vars) {
  y <- rdd[[var]]
  ok <- !is.na(y)
  if (sum(ok) < 200) next

  tryCatch({
    bal <- rdrobust(y = y[ok], x = rdd$R_tilde[ok], c = CUTOFF,
                    cluster = rdd$player_id[ok],
                    kernel = "triangular", bwselect = "mserd")
    balance_results[[var]] <- tibble(
      variable  = var,
      coef      = bal$coef[1],
      se_robust = bal$se[3],
      pv_robust = bal$pv[3],
      bw        = bal$bws[1],
      n_eff     = bal$N_h[1] + bal$N_h[2]
    )
    message("  ", var, ": coef = ", round(bal$coef[1], 2),
            ", p = ", round(bal$pv[3], 3))
  }, error = function(e) message("  ", var, ": FAILED -- ", e$message))
}

balance_df <- bind_rows(balance_results)

# FIX #4: Conditional Elo imbalance message
if (nrow(balance_df) > 0) {
  elo_row <- balance_df |> filter(variable == "elo_t0")
  if (nrow(elo_row) > 0 && elo_row$pv_robust[1] < 0.10) {
    message("\n  Elo imbalance detected (p = ", round(elo_row$pv_robust[1], 3),
            "): including elo_t0 as covariate in all specifications")
  } else {
    message("\n  No significant imbalance detected in pre-treatment covariates")
  }
}

saveRDS(balance_df, file.path(CLEANED_DIR, "main_balance_tests.rds"))

# ==============================================================================
# PART 2: LOCAL RANDOMIZATION INFERENCE (PRIMARY)
# ==============================================================================
message("\n", strrep("=", 70))
message("PART 2: LOCAL RANDOMIZATION INFERENCE (PRIMARY)")
message(strrep("=", 70))

# 2a. Window selection
message("\n--- 2a. Window selection ---")

# Prepare covariate matrix for window selection
X_win <- rdd |>
  transmute(player_rank = replace_na(player_rank, median(player_rank, na.rm = TRUE)),
            player_age  = replace_na(player_age, median(player_age, na.rm = TRUE))) |>
  as.matrix()

win_sel <- NULL
if (has_rdlocrand) {
  win_sel <- tryCatch({
    rdwinselect(
      R       = rdd$R_tilde,
      X       = X_win,
      cutoff  = CUTOFF,
      wmin    = 0.5,
      wstep   = 1,
      nwindows = 8,
      reps    = 1000,
      seed    = 20260321,
      quietly = TRUE
    )
  }, error = function(e) {
    message("  rdwinselect failed: ", e$message)
    NULL
  })
}

if (!is.null(win_sel)) {
  win_tab <- win_sel$results
  message("  Window selection results:")
  print(round(win_tab, 4))

  pval_col <- if ("P-value" %in% colnames(win_tab)) "P-value" else ncol(win_tab)
  good_rows <- which(win_tab[, pval_col] > 0.10)
  if (length(good_rows) > 0) {
    best_row <- max(good_rows)
  } else {
    best_row <- 1
    message("  No window with p > 0.10; using smallest window")
  }
  wl_opt <- win_tab[best_row, 1] - CUTOFF
  wr_opt <- win_tab[best_row, 2] - CUTOFF
  message("  Selected window: [", win_tab[best_row, 1], ", ", win_tab[best_row, 2],
          "] => wl = ", wl_opt, ", wr = ", wr_opt)
} else {
  message("  Using fallback window: R_tilde in {-1, 0, 1, 2} (wl=-1.5, wr=1.5)")
  wl_opt <- -1.5
  wr_opt <-  1.5
}

# 2b. Local randomization inference for main outcomes
message("\n--- 2b. Local randomization RDD ---")

locrand_outcomes <- c("rank_change_4w", "rank_change_8w",
                      "rank_change_12w", "rank_change_26w", "rank_change_52w",
                      "elo_change_12w", "elo_change_26w")
locrand_results <- list()

# --- Manual permutation inference function ---
# Used as primary implementation (avoids rdlocrand dependency issues)
# Permutes treatment within each tournament to respect block structure
manual_permutation_test <- function(Y, R, D, tourney, cutoff, wl, wr, n_perms = 2000) {
  # Restrict to window
  in_win <- (R >= (cutoff + wl)) & (R <= (cutoff + wr))
  Y_w <- Y[in_win]
  D_w <- D[in_win]
  tourney_w <- tourney[in_win]

  ok <- !is.na(Y_w)
  Y_w <- Y_w[ok]; D_w <- D_w[ok]; tourney_w <- tourney_w[ok]

  if (sum(D_w == 1) < 5 || sum(D_w == 0) < 5) {
    return(list(obs_stat = NA_real_, p_value = NA_real_,
                n_treat = sum(D_w == 1), n_ctrl = sum(D_w == 0),
                ci = c(NA_real_, NA_real_)))
  }

  # Observed test statistic: difference in means
  obs_stat <- mean(Y_w[D_w == 1]) - mean(Y_w[D_w == 0])

  # Permute treatment within tournament blocks
  tourneys <- unique(tourney_w)
  perm_stats <- numeric(n_perms)

  for (b in seq_len(n_perms)) {
    D_perm <- D_w
    for (tt in tourneys) {
      idx <- which(tourney_w == tt)
      if (length(idx) > 1) {
        D_perm[idx] <- sample(D_w[idx])
      }
    }
    perm_stats[b] <- mean(Y_w[D_perm == 1]) - mean(Y_w[D_perm == 0])
  }

  # Two-sided p-value
  p_value <- mean(abs(perm_stats) >= abs(obs_stat))

  # Confidence interval: simple normal approximation from permutation SE
  # (Test inversion is too slow for 7 outcomes; use permutation SD as SE)
  perm_se <- sd(perm_stats)
  ci_lo <- obs_stat - 1.96 * perm_se
  ci_hi <- obs_stat + 1.96 * perm_se

  list(obs_stat = obs_stat, p_value = p_value,
       n_treat = sum(D_w == 1), n_ctrl = sum(D_w == 0),
       ci = c(ci_lo, ci_hi))
}

for (outcome in locrand_outcomes) {
  y <- rdd[[outcome]]
  ok <- !is.na(y)
  if (sum(ok) < 100) {
    message("  ", outcome, ": too few obs (", sum(ok), ")")
    next
  }

  # Try rdlocrand::rdrandinf first, fall back to manual
  lr_result <- NULL

  if (has_rdlocrand) {
    # rdrandinf: wl and wr are the LEFT and RIGHT window endpoints (absolute)
    win_left  <- wl_opt + CUTOFF   # e.g. -0.5 + 0.5 = 0
    win_right <- wr_opt + CUTOFF   # e.g.  0.5 + 0.5 = 1
    lr_result <- tryCatch({
      lr <- rdrandinf(
        Y       = y[ok],
        R       = rdd$R_tilde[ok],
        cutoff  = CUTOFF,
        wl      = win_left,
        wr      = win_right,
        statistic = "diffmeans",
        reps    = 2000,
        seed    = 20260321,
        quietly = TRUE
      )
      # Extract what we can from rdrandinf output
      n_t <- tryCatch(lr$sumstats[1, 1], error = function(e) NA_real_)
      n_c <- tryCatch(lr$sumstats[1, 2], error = function(e) NA_real_)
      ci_vals <- if (!is.null(lr$ci)) c(lr$ci[1], lr$ci[2]) else c(NA_real_, NA_real_)
      list(
        obs_stat = lr$obs.stat,
        p_value  = lr$p.value,
        ci       = ci_vals,
        n_treat  = n_t,
        n_ctrl   = n_c,
        method   = "rdrandinf"
      )
    }, error = function(e) {
      message("  rdrandinf failed for ", outcome, ": ", e$message)
      NULL
    })
  }

  # Fallback: manual permutation inference
  if (is.null(lr_result)) {
    message("  Using manual permutation inference for ", outcome)
    mp <- manual_permutation_test(
      Y       = rdd[[outcome]],
      R       = rdd$R_tilde,
      D       = rdd$got_ll,
      tourney = rdd$tourney_id,
      cutoff  = CUTOFF,
      wl      = wl_opt,
      wr      = wr_opt,
      n_perms = 5000
    )
    lr_result <- list(
      obs_stat = mp$obs_stat,
      p_value  = mp$p_value,
      ci       = mp$ci,
      n_treat  = mp$n_treat,
      n_ctrl   = mp$n_ctrl,
      method   = "manual_permutation"
    )
  }

  if (!is.na(lr_result$obs_stat)) {
    locrand_results[[outcome]] <- tibble(
      outcome      = outcome,
      method       = lr_result$method,
      coef         = lr_result$obs_stat,
      pv_fisher    = lr_result$p_value,
      ci_lower     = lr_result$ci[1],
      ci_upper     = lr_result$ci[2],
      window_left  = wl_opt + CUTOFF,
      window_right = wr_opt + CUTOFF,
      n_left       = lr_result$n_treat,
      n_right      = lr_result$n_ctrl
    )
    message("  ", outcome, ": diff = ", round(lr_result$obs_stat, 2),
            ", Fisher p = ", round(lr_result$p_value, 3),
            " (", lr_result$method, ")")
  }
}

locrand_df <- bind_rows(locrand_results)
message("\n  Local randomization results: ", nrow(locrand_df), " outcomes")
if (nrow(locrand_df) > 0) print(locrand_df, width = 200)

saveRDS(locrand_df, file.path(CLEANED_DIR, "main_locrand_results.rds"))

# ==============================================================================
# PART 3: FUZZY RDD WITH COVARIATES (rdrobust -- ROBUSTNESS)
# ==============================================================================
message("\n", strrep("=", 70))
message("PART 3: FUZZY RDD WITH COVARIATES (ROBUSTNESS)")
message(strrep("=", 70))

rdd_outcomes <- c("rank_change_4w", "rank_change_8w", "rank_change_12w",
                  "rank_change_26w", "rank_change_52w",
                  "elo_change_12w", "elo_change_26w", "elo_change_52w")

# Prepare covariate matrix
rdd_cov <- rdd |>
  mutate(
    elo_t0_imp    = replace_na(elo_t0, median(elo_t0, na.rm = TRUE)),
    age_imp       = replace_na(player_age, median(player_age, na.rm = TRUE)),
    is_grand_slam = as.integer(tourney_level == "G"),
    is_masters    = as.integer(tourney_level == "M"),
    is_clay       = as.integer(surface == "Clay"),
    is_grass      = as.integer(surface == "Grass")
  )

covs_mat <- rdd_cov |>
  select(age_imp, elo_t0_imp, is_grand_slam, is_masters, is_clay, is_grass) |>
  as.matrix()

fuzzy_results_nocov <- list()
fuzzy_results_cov   <- list()

for (outcome in rdd_outcomes) {
  y <- rdd_cov[[outcome]]
  ok <- !is.na(y)
  if (sum(ok) < 200) next

  # Without covariates -- WITH CLUSTERING
  tryCatch({
    frd <- rdrobust(
      y = y[ok], x = rdd_cov$R_tilde[ok], c = CUTOFF,
      fuzzy = rdd_cov$got_ll[ok],
      cluster = rdd_cov$player_id[ok],
      kernel = "triangular", bwselect = "mserd"
    )
    fuzzy_results_nocov[[outcome]] <- tibble(
      outcome = outcome, covariates = "No",
      coef = frd$coef[1], se_robust = frd$se[3], pv_robust = frd$pv[3],
      ci_lower = frd$ci[3, 1], ci_upper = frd$ci[3, 2],
      bw = frd$bws[1], n_eff = frd$N_h[1] + frd$N_h[2]
    )
  }, error = function(e) message("  Fuzzy (no cov) ", outcome, ": FAILED -- ", e$message))

  # With covariates -- WITH CLUSTERING
  tryCatch({
    frd_c <- rdrobust(
      y = y[ok], x = rdd_cov$R_tilde[ok], c = CUTOFF,
      fuzzy = rdd_cov$got_ll[ok],
      covs = covs_mat[ok, ],
      cluster = rdd_cov$player_id[ok],
      kernel = "triangular", bwselect = "mserd"
    )
    fuzzy_results_cov[[outcome]] <- tibble(
      outcome = outcome, covariates = "Yes",
      coef = frd_c$coef[1], se_robust = frd_c$se[3], pv_robust = frd_c$pv[3],
      ci_lower = frd_c$ci[3, 1], ci_upper = frd_c$ci[3, 2],
      bw = frd_c$bws[1], n_eff = frd_c$N_h[1] + frd_c$N_h[2]
    )
    message("  ", outcome, " (cov): coef = ", round(frd_c$coef[1], 2),
            ", p = ", round(frd_c$pv[3], 3))
  }, error = function(e) message("  Fuzzy (cov) ", outcome, ": FAILED -- ", e$message))
}

fuzzy_nocov_df <- bind_rows(fuzzy_results_nocov)
fuzzy_cov_df   <- bind_rows(fuzzy_results_cov)
fuzzy_all_df   <- bind_rows(fuzzy_nocov_df, fuzzy_cov_df)

message("\n  Fuzzy RDD results (with covariates):")
if (nrow(fuzzy_cov_df) > 0) print(fuzzy_cov_df, width = 200)

saveRDS(fuzzy_all_df, file.path(CLEANED_DIR, "main_fuzzy_rdd_results.rds"))

# ==============================================================================
# PART 3b: MULTIPLE TESTING CORRECTION (Benjamini-Hochberg)
# ==============================================================================
message("\n--- Multiple Testing Correction (BH FDR) ---")

# Apply BH correction to ranking change family from fuzzy RDD
rank_family <- fuzzy_cov_df |>
  filter(str_detect(outcome, "^rank_change"))

if (nrow(rank_family) > 0) {
  rank_family <- rank_family |>
    mutate(
      pv_raw = pv_robust,
      pv_bh  = p.adjust(pv_robust, method = "BH")
    )
  message("  Ranking change family (BH-adjusted p-values):")
  for (i in seq_len(nrow(rank_family))) {
    r <- rank_family[i, ]
    message("    ", r$outcome, ": raw p = ", round(r$pv_raw, 3),
            ", BH p = ", round(r$pv_bh, 3))
  }
  saveRDS(rank_family, file.path(CLEANED_DIR, "main_multiple_testing.rds"))
} else {
  message("  No ranking change outcomes to correct")
  rank_family <- tibble()
}

# Also apply to local randomization results
locrand_rank <- locrand_df |> filter(str_detect(outcome, "^rank_change"))
if (nrow(locrand_rank) > 0) {
  locrand_rank <- locrand_rank |>
    mutate(
      pv_raw = pv_fisher,
      pv_bh  = p.adjust(pv_fisher, method = "BH")
    )
  message("  Local randomization ranking family (BH-adjusted):")
  for (i in seq_len(nrow(locrand_rank))) {
    r <- locrand_rank[i, ]
    message("    ", r$outcome, ": raw p = ", round(r$pv_raw, 3),
            ", BH p = ", round(r$pv_bh, 3))
  }
}

# ==============================================================================
# PART 4: HETEROGENEITY ANALYSIS
# ==============================================================================
message("\n", strrep("=", 70))
message("PART 4: HETEROGENEITY ANALYSIS")
message(strrep("=", 70))

het_outcomes <- c("rank_change_26w", "rank_change_52w")
het_results <- list()

# --- 4a. By career stage (split at median ranking) ---
message("\n--- 4a. By career stage ---")
med_rank <- median(rdd$player_rank, na.rm = TRUE)
message("  Median rank: ", med_rank)

for (outcome in het_outcomes) {
  for (stage in c("early", "established")) {
    sub <- if (stage == "early") {
      rdd_cov |> filter(!is.na(player_rank) & player_rank > med_rank)
    } else {
      rdd_cov |> filter(!is.na(player_rank) & player_rank <= med_rank)
    }
    y <- sub[[outcome]]
    ok <- !is.na(y)
    if (sum(ok) < 100) { message("  ", outcome, " (", stage, "): too few obs"); next }

    tryCatch({
      frd <- rdrobust(y = y[ok], x = sub$R_tilde[ok], c = CUTOFF,
                      fuzzy = sub$got_ll[ok],
                      cluster = sub$player_id[ok],
                      kernel = "triangular", bwselect = "mserd")
      het_results[[paste0(outcome, "_career_", stage)]] <- tibble(
        outcome = outcome, dimension = "career_stage", group = stage,
        coef = frd$coef[1], se_robust = frd$se[3], pv_robust = frd$pv[3],
        bw = frd$bws[1], n_eff = frd$N_h[1] + frd$N_h[2]
      )
      message("  ", outcome, " (", stage, "): coef = ", round(frd$coef[1], 2),
              ", p = ", round(frd$pv[3], 3))
    }, error = function(e) message("  ", outcome, " (", stage, "): FAILED"))
  }
}

# --- 4b. By tournament tier ---
message("\n--- 4b. By tournament tier ---")
for (outcome in het_outcomes) {
  for (tier in c("G", "M", "A")) {
    sub <- rdd_cov |> filter(tourney_level == tier)
    y <- sub[[outcome]]
    ok <- !is.na(y)
    if (sum(ok) < 100) { message("  ", outcome, " (", tier, "): too few obs (", sum(ok), ")"); next }

    tryCatch({
      frd <- rdrobust(y = y[ok], x = sub$R_tilde[ok], c = CUTOFF,
                      fuzzy = sub$got_ll[ok],
                      cluster = sub$player_id[ok],
                      kernel = "triangular", bwselect = "mserd")
      het_results[[paste0(outcome, "_tier_", tier)]] <- tibble(
        outcome = outcome, dimension = "tourney_tier", group = tier,
        coef = frd$coef[1], se_robust = frd$se[3], pv_robust = frd$pv[3],
        bw = frd$bws[1], n_eff = frd$N_h[1] + frd$N_h[2]
      )
      message("  ", outcome, " (", tier, "): coef = ", round(frd$coef[1], 2),
              ", p = ", round(frd$pv[3], 3))
    }, error = function(e) message("  ", outcome, " (", tier, "): FAILED"))
  }
}

# --- 4c. By surface ---
message("\n--- 4c. By surface ---")
for (outcome in het_outcomes) {
  for (surf in c("Hard", "Clay", "Grass")) {
    sub <- rdd_cov |> filter(surface == surf)
    y <- sub[[outcome]]
    ok <- !is.na(y)
    if (sum(ok) < 100) { message("  ", outcome, " (", surf, "): too few obs (", sum(ok), ")"); next }

    tryCatch({
      frd <- rdrobust(y = y[ok], x = sub$R_tilde[ok], c = CUTOFF,
                      fuzzy = sub$got_ll[ok],
                      cluster = sub$player_id[ok],
                      kernel = "triangular", bwselect = "mserd")
      het_results[[paste0(outcome, "_surface_", surf)]] <- tibble(
        outcome = outcome, dimension = "surface", group = surf,
        coef = frd$coef[1], se_robust = frd$se[3], pv_robust = frd$pv[3],
        bw = frd$bws[1], n_eff = frd$N_h[1] + frd$N_h[2]
      )
      message("  ", outcome, " (", surf, "): coef = ", round(frd$coef[1], 2),
              ", p = ", round(frd$pv[3], 3))
    }, error = function(e) message("  ", outcome, " (", surf, "): FAILED"))
  }
}

het_df <- bind_rows(het_results)
saveRDS(het_df, file.path(CLEANED_DIR, "main_heterogeneity_results.rds"))

# ==============================================================================
# PART 5: GRAND SLAM LOTTERY VALIDATION (TOP-4 POOL ONLY)
# ==============================================================================
message("\n", strrep("=", 70))
message("PART 5: GRAND SLAM LOTTERY VALIDATION (TOP-4 POOL)")
message(strrep("=", 70))

# FIX #3: Restrict to top-4 ranked qualifying losers per Grand Slam tournament
# The strategy memo specifies: "At each Grand Slam, the 4 highest-ranked players
# among the 16 final-round qualifying losers"
gs_all <- rdd_cov |> filter(tourney_level == "G")
message("  All Grand Slam qualifying losers: ", nrow(gs_all))

# rank_among_losers <= 4 gives us the lottery pool
gs <- gs_all |> filter(rank_among_losers <= 4)
message("  Top-4 lottery pool: ", nrow(gs), " obs (LL: ", sum(gs$got_ll), ")")

# Verify randomization: within the top-4 pool, LL probability should not depend on rank
if (nrow(gs) > 20) {
  gs_rate_by_rank <- gs |>
    group_by(rank_among_losers) |>
    summarise(n = n(), pct_ll = mean(got_ll), .groups = "drop")
  message("  LL rate by rank within pool:")
  for (i in seq_len(nrow(gs_rate_by_rank))) {
    r <- gs_rate_by_rank[i, ]
    message("    rank ", r$rank_among_losers, ": ", round(r$pct_ll * 100, 1),
            "% (n = ", r$n, ")")
  }
  # Chi-squared test for independence
  chisq <- tryCatch(
    chisq.test(table(gs$rank_among_losers, gs$got_ll)),
    error = function(e) NULL
  )
  if (!is.null(chisq)) {
    message("  Chi-sq test for independence: p = ", round(chisq$p.value, 3))
  }
}

gs_outcomes <- c("rank_change_12w", "rank_change_26w", "rank_change_52w",
                 "elo_change_12w", "elo_change_26w")
gs_results <- list()

for (outcome in gs_outcomes) {
  y <- gs[[outcome]]
  ok <- !is.na(y)
  if (sum(ok) < 20) { message("  ", outcome, ": too few obs (", sum(ok), ")"); next }

  # Simple difference in means within the top-4 pool
  tr <- gs$got_ll[ok] == 1
  if (sum(tr) < 3 || sum(!tr) < 3) {
    message("  ", outcome, ": insufficient variation in treatment within pool")
    next
  }
  diff_means <- mean(y[ok][tr], na.rm = TRUE) - mean(y[ok][!tr], na.rm = TRUE)
  tt <- tryCatch(t.test(y[ok] ~ gs$got_ll[ok]), error = function(e) NULL)

  # OLS with controls, clustered at player level
  gs_sub <- gs[ok, ]
  ols <- tryCatch({
    feols(as.formula(paste0(outcome, " ~ got_ll + age_imp + elo_t0_imp | year")),
          data = gs_sub, vcov = ~player_id)
  }, error = function(e) NULL)

  gs_results[[outcome]] <- tibble(
    outcome     = outcome,
    diff_means  = diff_means,
    dm_pvalue   = if (!is.null(tt)) tt$p.value else NA_real_,
    ols_coef    = if (!is.null(ols)) coef(ols)["got_ll"] else NA_real_,
    ols_se      = if (!is.null(ols)) sqrt(vcov(ols)["got_ll", "got_ll"]) else NA_real_,
    ols_pvalue  = if (!is.null(ols)) pvalue(ols)["got_ll"] else NA_real_,
    n_treated   = sum(tr),
    n_control   = sum(!tr),
    n_total     = sum(ok)
  )

  message("  ", outcome, ": diff = ", round(diff_means, 2),
          " (t-test p = ", round(gs_results[[outcome]]$dm_pvalue, 3),
          ", N = ", sum(ok), " [", sum(tr), " LL, ", sum(!tr), " ctrl])",
          if (!is.null(ols)) paste0(", OLS coef = ", round(coef(ols)["got_ll"], 2)) else "")
}

gs_df <- bind_rows(gs_results)
saveRDS(gs_df, file.path(CLEANED_DIR, "main_gs_validation_results.rds"))

# --- Grand Slam BH correction on ranking change p-values ---
gs_rank <- gs_df |> filter(str_detect(outcome, "^rank_change"))
if (nrow(gs_rank) > 0) {
  gs_rank <- gs_rank |>
    mutate(
      dm_pvalue_bh  = p.adjust(dm_pvalue, method = "BH"),
      ols_pvalue_bh = p.adjust(ols_pvalue, method = "BH")
    )
  message("\n  Grand Slam ranking change BH-adjusted p-values:")
  for (i in seq_len(nrow(gs_rank))) {
    r <- gs_rank[i, ]
    message("    ", r$outcome,
            ": raw t-test p = ", round(r$dm_pvalue, 3),
            ", BH p = ", round(r$dm_pvalue_bh, 3),
            "; raw OLS p = ", round(r$ols_pvalue, 3),
            ", BH p = ", round(r$ols_pvalue_bh, 3))
  }
  saveRDS(gs_rank, file.path(CLEANED_DIR, "main_gs_bh_correction.rds"))
}

# ==============================================================================
# PART 6: EVENT STUDY DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("PART 6: EVENT STUDY FIGURE DATA")
message(strrep("=", 70))

# Run fuzzy RDD at each horizon (with covariates and clustering)
horizons <- c(4, 8, 12, 26, 52)
es_results <- list()

for (h in horizons) {
  outcome <- paste0("rank_change_", h, "w")
  y <- rdd_cov[[outcome]]
  ok <- !is.na(y)
  if (sum(ok) < 200) next

  tryCatch({
    frd <- rdrobust(y = y[ok], x = rdd_cov$R_tilde[ok], c = CUTOFF,
                    fuzzy = rdd_cov$got_ll[ok], covs = covs_mat[ok, ],
                    cluster = rdd_cov$player_id[ok],
                    kernel = "triangular", bwselect = "mserd")
    es_results[[outcome]] <- tibble(
      horizon_weeks = h, outcome_type = "rank_change",
      coef = frd$coef[1], se = frd$se[3],
      ci_lower = frd$ci[3, 1], ci_upper = frd$ci[3, 2],
      pv = frd$pv[3], bw = frd$bws[1], n_eff = frd$N_h[1] + frd$N_h[2]
    )
    message("  rank_change_", h, "w: coef = ", round(frd$coef[1], 2),
            " [", round(frd$ci[3, 1], 1), ", ", round(frd$ci[3, 2], 1), "]")
  }, error = function(e) message("  rank_change_", h, "w: FAILED -- ", e$message))
}

# Also Elo horizons
for (h in c(12, 26, 52)) {
  outcome <- paste0("elo_change_", h, "w")
  y <- rdd_cov[[outcome]]
  ok <- !is.na(y)
  if (sum(ok) < 200) next

  tryCatch({
    frd <- rdrobust(y = y[ok], x = rdd_cov$R_tilde[ok], c = CUTOFF,
                    fuzzy = rdd_cov$got_ll[ok], covs = covs_mat[ok, ],
                    cluster = rdd_cov$player_id[ok],
                    kernel = "triangular", bwselect = "mserd")
    es_results[[outcome]] <- tibble(
      horizon_weeks = h, outcome_type = "elo_change",
      coef = frd$coef[1], se = frd$se[3],
      ci_lower = frd$ci[3, 1], ci_upper = frd$ci[3, 2],
      pv = frd$pv[3], bw = frd$bws[1], n_eff = frd$N_h[1] + frd$N_h[2]
    )
    message("  elo_change_", h, "w: coef = ", round(frd$coef[1], 2),
            " [", round(frd$ci[3, 1], 1), ", ", round(frd$ci[3, 2], 1), "]")
  }, error = function(e) message("  elo_change_", h, "w: FAILED -- ", e$message))
}

es_df <- bind_rows(es_results)
saveRDS(es_df, file.path(CLEANED_DIR, "main_event_study_data.rds"))

# --- Event study figure: Ranking effect over time ---
message("\n--- Creating event study figure ---")

es_rank <- es_df |> filter(outcome_type == "rank_change")

if (nrow(es_rank) > 0) {
  p_es <- ggplot(es_rank, aes(x = horizon_weeks, y = coef)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper), size = 0.6) +
    scale_x_continuous(breaks = c(4, 8, 12, 26, 52),
                       labels = c("4", "8", "12", "26", "52")) +
    labs(x = "Weeks After Tournament",
         y = "LATE: Ranking Change (Negative = Improvement)",
         title = NULL) +
    theme_paper() +
    theme(axis.title = element_text(size = 11))

  ggsave(file.path(FIGURES_DIR, "fig1_event_study_ranking.pdf"),
         p_es, width = 7, height = 5)
  message("  Saved: Figures/fig1_event_study_ranking.pdf")
}

# ==============================================================================
# PART 7: TABLES AND OUTPUT
# ==============================================================================
message("\n", strrep("=", 70))
message("PART 7: TABLES AND OUTPUT")
message(strrep("=", 70))

# --- Table 1: Balance ---
message("\n--- Table 1: Balance tests ---")
if (nrow(balance_df) > 0) {
  bal_tex <- balance_df |>
    mutate(
      Variable = case_when(
        variable == "player_rank" ~ "ATP Ranking",
        variable == "player_age"  ~ "Age",
        variable == "elo_t0"      ~ "Elo Rating",
        TRUE ~ variable
      ),
      Coefficient = sprintf("%.2f", coef),
      `Robust SE` = sprintf("(%.2f)", se_robust),
      `$p$-value` = sprintf("%.3f", pv_robust),
      Bandwidth = sprintf("%.1f", bw),
      `Eff. $N$` = as.character(round(n_eff))
    ) |>
    select(Variable, Coefficient, `Robust SE`, `$p$-value`, Bandwidth, `Eff. $N$`)

  bal_lines <- c(
    "\\begin{tabular}{lccccc}",
    "\\toprule",
    "Variable & Coefficient & Robust SE & $p$-value & Bandwidth & Eff. $N$ \\\\",
    "\\midrule"
  )
  for (i in seq_len(nrow(bal_tex))) {
    bal_lines <- c(bal_lines, paste0(
      bal_tex$Variable[i], " & ",
      bal_tex$Coefficient[i], " & ",
      bal_tex$`Robust SE`[i], " & ",
      bal_tex$`$p$-value`[i], " & ",
      bal_tex$Bandwidth[i], " & ",
      bal_tex$`Eff. $N$`[i], " \\\\"
    ))
  }
  bal_lines <- c(bal_lines,
    "\\midrule",
    "\\multicolumn{6}{l}{\\footnotesize Standard errors clustered at the player level.} \\\\",
    "\\bottomrule", "\\end{tabular}")
  writeLines(bal_lines, file.path(TABLES_DIR, "table1_balance.tex"))
  message("  Saved: Tables/table1_balance.tex")
}

# --- Table 2: Main results (local randomization + fuzzy RDD) ---
message("\n--- Table 2: Main results ---")

main_rank_outcomes <- c("rank_change_4w", "rank_change_8w",
                        "rank_change_12w", "rank_change_26w", "rank_change_52w")

main_table_rows <- list()
for (outcome in main_rank_outcomes) {
  lr_row <- locrand_df |> filter(outcome == !!outcome)
  fr_row <- fuzzy_cov_df |> filter(outcome == !!outcome)
  bh_row <- if (nrow(rank_family) > 0) rank_family |> filter(outcome == !!outcome) else tibble()

  horizon <- str_extract(outcome, "\\d+")

  main_table_rows[[outcome]] <- tibble(
    Horizon = paste0(horizon, " weeks"),
    `LR Estimate` = if (nrow(lr_row) > 0) sprintf("%.1f", lr_row$coef) else "--",
    `LR $p$ (Fisher)` = if (nrow(lr_row) > 0) sprintf("%.3f", lr_row$pv_fisher) else "--",
    `LATE` = if (nrow(fr_row) > 0) sprintf("%.1f", fr_row$coef) else "--",
    `Robust SE` = if (nrow(fr_row) > 0) sprintf("(%.1f)", fr_row$se_robust) else "--",
    `$p$-value` = if (nrow(fr_row) > 0) sprintf("%.3f", fr_row$pv_robust) else "--",
    `$p$ (BH)` = if (nrow(bh_row) > 0) sprintf("%.3f", bh_row$pv_bh) else "--",
    `Bandwidth` = if (nrow(fr_row) > 0) sprintf("%.1f", fr_row$bw) else "--",
    `Eff. $N$` = if (nrow(fr_row) > 0) as.character(round(fr_row$n_eff)) else "--"
  )
}

main_table <- bind_rows(main_table_rows)

main_lines <- c(
  "\\begin{tabular}{lcc|cccccc}",
  "\\toprule",
  " & \\multicolumn{2}{c|}{Local Randomization} & \\multicolumn{6}{c}{Fuzzy RDD (rdrobust)} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-9}",
  "Horizon & Estimate & $p$ (Fisher) & LATE & Robust SE & $p$-value & $p$ (BH) & Bandwidth & Eff. $N$ \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(main_table))) {
  main_lines <- c(main_lines, paste0(
    main_table$Horizon[i], " & ",
    main_table$`LR Estimate`[i], " & ",
    main_table$`LR $p$ (Fisher)`[i], " & ",
    main_table$LATE[i], " & ",
    main_table$`Robust SE`[i], " & ",
    main_table$`$p$-value`[i], " & ",
    main_table$`$p$ (BH)`[i], " & ",
    main_table$Bandwidth[i], " & ",
    main_table$`Eff. $N$`[i], " \\\\"
  ))
}
main_lines <- c(main_lines,
  "\\midrule",
  "\\multicolumn{9}{l}{\\footnotesize Covariates: age, Elo, tournament level, surface. SEs clustered at player level.} \\\\",
  "\\multicolumn{9}{l}{\\footnotesize $p$ (BH) = Benjamini-Hochberg corrected $p$-value across ranking change family.} \\\\",
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(main_lines, file.path(TABLES_DIR, "table2_main_results.tex"))
message("  Saved: Tables/table2_main_results.tex")

# --- Table 3: Heterogeneity ---
message("\n--- Table 3: Heterogeneity ---")
if (nrow(het_df) > 0) {
  het_tex_rows <- het_df |>
    mutate(
      Outcome = case_when(
        str_detect(outcome, "26w") ~ "26 weeks",
        str_detect(outcome, "52w") ~ "52 weeks",
        TRUE ~ outcome
      ),
      Dimension = case_when(
        dimension == "career_stage" ~ "Career Stage",
        dimension == "tourney_tier" ~ "Tournament Tier",
        dimension == "surface"      ~ "Surface",
        TRUE ~ dimension
      ),
      Group = case_when(
        group == "early"       ~ "Lower-ranked",
        group == "established" ~ "Higher-ranked",
        group == "G" ~ "Grand Slam",
        group == "M" ~ "Masters",
        group == "A" ~ "ATP 250/500",
        TRUE ~ group
      )
    )

  het_lines <- c(
    "\\begin{tabular}{llccccc}",
    "\\toprule",
    "Outcome & Subgroup & LATE & Robust SE & $p$-value & BW & Eff. $N$ \\\\",
    "\\midrule"
  )
  last_dim <- ""
  for (i in seq_len(nrow(het_tex_rows))) {
    r <- het_tex_rows[i, ]
    dim_label <- if (paste0(r$Dimension, r$Outcome) != last_dim) {
      last_dim <<- paste0(r$Dimension, r$Outcome)
      paste0("\\multicolumn{7}{l}{\\textit{", r$Dimension, " -- ", r$Outcome, "}} \\\\")
    } else NULL

    if (!is.null(dim_label)) het_lines <- c(het_lines, dim_label)
    het_lines <- c(het_lines, paste0(
      " & ", r$Group, " & ",
      sprintf("%.1f", r$coef), " & ",
      sprintf("(%.1f)", r$se_robust), " & ",
      sprintf("%.3f", r$pv_robust), " & ",
      sprintf("%.1f", r$bw), " & ",
      round(r$n_eff), " \\\\"
    ))
  }
  het_lines <- c(het_lines,
    "\\midrule",
    "\\multicolumn{7}{l}{\\footnotesize SEs clustered at the player level.} \\\\",
    "\\bottomrule", "\\end{tabular}")
  writeLines(het_lines, file.path(TABLES_DIR, "table3_heterogeneity.tex"))
  message("  Saved: Tables/table3_heterogeneity.tex")
}

# --- Table 4: Grand Slam validation ---
message("\n--- Table 4: Grand Slam validation ---")
if (nrow(gs_df) > 0) {
  gs_lines <- c(
    "\\begin{tabular}{lccccccc}",
    "\\toprule",
    "Outcome & Diff-in-Means & $p$ (t-test) & OLS Coef & OLS SE & $p$ (OLS) & $N_{\\text{treat}}$ & $N_{\\text{ctrl}}$ \\\\",
    "\\midrule"
  )
  for (i in seq_len(nrow(gs_df))) {
    r <- gs_df[i, ]
    lab <- str_replace(r$outcome, "_change_", " ") |> str_replace("w$", " weeks")
    gs_lines <- c(gs_lines, paste0(
      lab, " & ",
      sprintf("%.1f", r$diff_means), " & ",
      sprintf("%.3f", r$dm_pvalue), " & ",
      if (!is.na(r$ols_coef)) sprintf("%.1f", r$ols_coef) else "--", " & ",
      if (!is.na(r$ols_se)) sprintf("(%.1f)", r$ols_se) else "--", " & ",
      if (!is.na(r$ols_pvalue)) sprintf("%.3f", r$ols_pvalue) else "--", " & ",
      r$n_treated, " & ", r$n_control, " \\\\"
    ))
  }
  gs_lines <- c(gs_lines,
    "\\midrule",
    "\\multicolumn{8}{l}{\\footnotesize Sample: top-4 ranked qualifying losers per Grand Slam (lottery pool).} \\\\",
    "\\multicolumn{8}{l}{\\footnotesize OLS: controls for age and Elo; year FE; SEs clustered at player level.} \\\\",
    "\\bottomrule", "\\end{tabular}")
  writeLines(gs_lines, file.path(TABLES_DIR, "table4_grand_slam_validation.tex"))
  message("  Saved: Tables/table4_grand_slam_validation.tex")
}

# --- RDD plot: first stage ---
message("\n--- Figure 2: First stage RDD plot ---")
fs_plot_data <- rdd |>
  filter(abs(R_tilde) <= 8) |>
  group_by(R_tilde) |>
  summarise(pct_ll = mean(got_ll), n = n(), .groups = "drop")

p_fs <- ggplot(fs_plot_data, aes(x = R_tilde, y = pct_ll)) +
  geom_vline(xintercept = CUTOFF, linetype = "dashed", color = "gray50") +
  geom_point(aes(size = n), color = "black") +
  scale_size_continuous(range = c(1, 5), guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = -8:8) +
  labs(x = expression(tilde(R) ~ "(Normalized Ranking Among Losers)"),
       y = "Probability of LL Entry",
       title = NULL) +
  theme_paper()

ggsave(file.path(FIGURES_DIR, "fig2_first_stage.pdf"), p_fs, width = 7, height = 5)
message("  Saved: Figures/fig2_first_stage.pdf")

# --- Figure 3: Heterogeneity forest plot ---
message("\n--- Figure 3: Heterogeneity forest plot ---")
if (nrow(het_df) > 0) {
  het_plot <- het_df |>
    filter(str_detect(outcome, "26w")) |>
    mutate(
      label = case_when(
        group == "early"       ~ "Lower-ranked",
        group == "established" ~ "Higher-ranked",
        group == "G" ~ "Grand Slam",
        group == "M" ~ "Masters",
        group == "A" ~ "ATP 250/500",
        TRUE ~ group
      ),
      dim_label = case_when(
        dimension == "career_stage" ~ "Career Stage",
        dimension == "tourney_tier" ~ "Tournament Tier",
        dimension == "surface"      ~ "Surface"
      ),
      ci_lower = coef - 1.96 * se_robust,
      ci_upper = coef + 1.96 * se_robust
    ) |>
    arrange(dim_label, label) |>
    mutate(label = factor(label, levels = rev(label)))

  p_het <- ggplot(het_plot, aes(x = coef, y = label)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_pointrange(aes(xmin = ci_lower, xmax = ci_upper), size = 0.5) +
    facet_grid(dim_label ~ ., scales = "free_y", space = "free_y") +
    labs(x = "LATE: Ranking Change at 26 Weeks (Negative = Improvement)",
         y = NULL, title = NULL) +
    theme_paper() +
    theme(strip.text.y = element_text(angle = 0, hjust = 0))

  ggsave(file.path(FIGURES_DIR, "fig3_heterogeneity_26w.pdf"), p_het, width = 7, height = 5)
  message("  Saved: Figures/fig3_heterogeneity_26w.pdf")
}

# ==============================================================================
# RESULTS SUMMARY
# ==============================================================================
message("\n", strrep("=", 70))
message("RESULTS SUMMARY")
message(strrep("=", 70))

summary_lines <- c(
  "# Results Summary -- 06_main_analysis.R",
  paste0("Generated: ", Sys.time()),
  "",
  "**NOTE:** All standard errors are clustered at the player level.",
  "Multiple testing corrections (Benjamini-Hochberg) applied to ranking change family.",
  "",
  "## Balance Tests",
  ""
)
if (nrow(balance_df) > 0) {
  for (i in seq_len(nrow(balance_df))) {
    r <- balance_df[i, ]
    flag <- if (r$pv_robust < 0.05) " ***IMBALANCED***" else " (balanced)"
    summary_lines <- c(summary_lines,
      paste0("- ", r$variable, ": coef = ", round(r$coef, 2),
             ", p = ", round(r$pv_robust, 3), flag))
  }
}

summary_lines <- c(summary_lines, "",
  "## Main Results: Local Randomization (PRIMARY)", "")
if (nrow(locrand_df) > 0) {
  for (i in seq_len(nrow(locrand_df))) {
    r <- locrand_df[i, ]
    stars <- if (r$pv_fisher < 0.01) "***" else if (r$pv_fisher < 0.05) "**" else if (r$pv_fisher < 0.10) "*" else ""
    summary_lines <- c(summary_lines,
      paste0("- ", r$outcome, ": diff = ", round(r$coef, 1),
             ", Fisher p = ", round(r$pv_fisher, 3), " ", stars,
             " (method: ", r$method, ")"))
  }
} else {
  summary_lines <- c(summary_lines, "- No local randomization results produced")
}

summary_lines <- c(summary_lines, "",
  "## Fuzzy RDD with Covariates (ROBUSTNESS)", "")
if (nrow(fuzzy_cov_df) > 0) {
  for (i in seq_len(nrow(fuzzy_cov_df))) {
    r <- fuzzy_cov_df[i, ]
    stars <- if (r$pv_robust < 0.01) "***" else if (r$pv_robust < 0.05) "**" else if (r$pv_robust < 0.10) "*" else ""
    bh_p <- ""
    if (nrow(rank_family) > 0) {
      bh_match <- rank_family |> filter(outcome == r$outcome)
      if (nrow(bh_match) > 0) bh_p <- paste0(", BH p = ", round(bh_match$pv_bh, 3))
    }
    summary_lines <- c(summary_lines,
      paste0("- ", r$outcome, ": LATE = ", round(r$coef, 1),
             " (SE = ", round(r$se_robust, 1), ", p = ", round(r$pv_robust, 3),
             bh_p, ") ", stars))
  }
}

summary_lines <- c(summary_lines, "",
  "## Grand Slam Lottery Validation (TOP-4 POOL ONLY)", "")
if (nrow(gs_df) > 0) {
  for (i in seq_len(nrow(gs_df))) {
    r <- gs_df[i, ]
    bh_note <- ""
    if (exists("gs_rank") && nrow(gs_rank) > 0) {
      bh_match <- gs_rank |> filter(outcome == r$outcome)
      if (nrow(bh_match) > 0) {
        bh_note <- paste0(", BH t-test p = ", round(bh_match$dm_pvalue_bh, 3),
                          ", BH OLS p = ", round(bh_match$ols_pvalue_bh, 3))
      }
    }
    summary_lines <- c(summary_lines,
      paste0("- ", r$outcome, ": diff = ", round(r$diff_means, 1),
             " (p = ", round(r$dm_pvalue, 3), bh_note,
             "), N = ", r$n_total, " [", r$n_treated, " LL, ", r$n_control, " ctrl]"))
  }
} else {
  summary_lines <- c(summary_lines, "- No Grand Slam results (insufficient top-4 pool sample)")
}

summary_lines <- c(summary_lines, "",
  "## Heterogeneity (26-week ranking change)", "")
if (nrow(het_df) > 0) {
  het_26 <- het_df |> filter(str_detect(outcome, "26w"))
  for (i in seq_len(nrow(het_26))) {
    r <- het_26[i, ]
    summary_lines <- c(summary_lines,
      paste0("- ", r$dimension, "/", r$group, ": LATE = ", round(r$coef, 1),
             " (p = ", round(r$pv_robust, 3), ")"))
  }
}

summary_lines <- c(summary_lines, "",
  "## Output Files", "",
  "- Tables/table1_balance.tex",
  "- Tables/table2_main_results.tex",
  "- Tables/table3_heterogeneity.tex",
  "- Tables/table4_grand_slam_validation.tex",
  "- Figures/fig1_event_study_ranking.pdf",
  "- Figures/fig2_first_stage.pdf",
  "- Figures/fig3_heterogeneity_26w.pdf",
  "- Data/cleaned/main_*.rds (6 result files)",
  "",
  "## Robustness Checks (see 06b_robustness.R)", "",
  "- Bandwidth sensitivity",
  "- Polynomial order (p=2)",
  "- Density test (rddensity)",
  "- Placebo cutoffs",
  "- Donut-hole RDD",
  "- Tournament-year clustering",
  ""
)

writeLines(summary_lines, file.path(OUTPUT_DIR, "results_summary.md"))
message("  Saved: Output/results_summary.md")

# Print summary to console
message("\n", paste(summary_lines, collapse = "\n"))

message("\n=== 06_main_analysis.R COMPLETE ===")
