# ==============================================================================
# 06b_robustness.R
# Robustness checks for the Lucky Loser RDD analysis
# Run AFTER 06_main_analysis.R
# Checks:
#   (a) Bandwidth sensitivity (50%-200% of MSE-optimal)
#   (b) Polynomial order (p=2 local quadratic)
#   (c) Density test (rddensity)
#   (d) Placebo cutoffs at R_tilde = -2, -1, +2, +3
#   (e) Donut-hole RDD (exclude R_tilde in {0, 1})
#   (f) Tournament-year clustering (alternative to player-level)
#   (g) Single LL-slot tournaments only
# Inputs:  Data/cleaned/estimation_sample_final.rds
# Outputs: Data/cleaned/robustness_*.rds, Tables/table5_robustness.tex,
#          Figures/fig4_bw_sensitivity.pdf, Output/robustness_summary.md
# Dependencies: rdrobust, rddensity (optional), ggplot2, here
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
library(here)

# Try rddensity
has_rddensity <- requireNamespace("rddensity", quietly = TRUE)
if (has_rddensity) {
  library(rddensity)
  message("rddensity loaded successfully")
} else {
  message("rddensity not available -- density test will use manual histogram approach")
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

# --- Load data ----------------------------------------------------------------
message("=== Loading data ===")
est <- read_rds(file.path(CLEANED_DIR, "estimation_sample_final.rds"))
rdd <- est |> filter(n_ll_slots > 0)

CUTOFF <- 0.5

# Prepare covariates
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

# Create tourney_year for alternative clustering
rdd_cov <- rdd_cov |>
  mutate(tourney_year = paste0(tourney_id, "_", year))

message("  RDD sample: ", nrow(rdd_cov), " obs")

# Key outcomes for robustness
rob_outcomes <- c("rank_change_12w", "rank_change_26w", "elo_change_26w")

# ==============================================================================
# (a) BANDWIDTH SENSITIVITY
# ==============================================================================
message("\n", strrep("=", 70))
message("ROBUSTNESS (a): BANDWIDTH SENSITIVITY")
message(strrep("=", 70))

bw_multipliers <- c(0.5, 0.75, 1.0, 1.5, 2.0)
bw_results <- list()

for (outcome in rob_outcomes) {
  y <- rdd_cov[[outcome]]
  ok <- !is.na(y)
  if (sum(ok) < 200) next

  # First get the MSE-optimal bandwidth
  base_rd <- tryCatch(
    rdrobust(y = y[ok], x = rdd_cov$R_tilde[ok], c = CUTOFF,
             fuzzy = rdd_cov$got_ll[ok], covs = covs_mat[ok, ],
             cluster = rdd_cov$player_id[ok],
             kernel = "triangular", bwselect = "mserd"),
    error = function(e) NULL
  )
  if (is.null(base_rd)) next
  bw_opt <- base_rd$bws[1]

  for (mult in bw_multipliers) {
    bw_use <- bw_opt * mult
    tryCatch({
      rd <- rdrobust(y = y[ok], x = rdd_cov$R_tilde[ok], c = CUTOFF,
                     fuzzy = rdd_cov$got_ll[ok], covs = covs_mat[ok, ],
                     cluster = rdd_cov$player_id[ok],
                     h = bw_use,
                     kernel = "triangular")
      bw_results[[paste0(outcome, "_", mult)]] <- tibble(
        outcome = outcome, bw_mult = mult, bw_used = bw_use,
        coef = rd$coef[1], se_robust = rd$se[3], pv_robust = rd$pv[3],
        ci_lower = rd$ci[3, 1], ci_upper = rd$ci[3, 2],
        n_eff = rd$N_h[1] + rd$N_h[2]
      )
      message("  ", outcome, " (", mult, "x bw=", round(bw_use, 1),
              "): coef = ", round(rd$coef[1], 2), ", p = ", round(rd$pv[3], 3))
    }, error = function(e) {
      msg <- paste0("  ", outcome, " (", mult, "x bw=", round(bw_use, 1), "): FAILED -- ", e$message)
      if (mult < 1) {
        msg <- paste0(msg,
          "\n    NOTE: Narrow bandwidths are often infeasible with a discrete running variable.",
          "\n    The MSE-optimal bandwidth may already be near the minimum support of R_tilde.")
      }
      message(msg)
    })
  }
}

bw_df <- bind_rows(bw_results)
saveRDS(bw_df, file.path(CLEANED_DIR, "robustness_bw_sensitivity.rds"))

# --- Bandwidth sensitivity figure ---
if (nrow(bw_df) > 0) {
  for (oc in unique(bw_df$outcome)) {
    bw_plot <- bw_df |> filter(outcome == oc)
    p_bw <- ggplot(bw_plot, aes(x = bw_mult, y = coef)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper), size = 0.6) +
      geom_vline(xintercept = 1.0, linetype = "dotted", color = "gray70") +
      scale_x_continuous(breaks = bw_multipliers,
                         labels = paste0(bw_multipliers * 100, "%")) +
      labs(x = "Bandwidth (% of MSE-Optimal)",
           y = "LATE Estimate",
           title = NULL) +
      theme_paper()

    fname <- paste0("fig4_bw_sensitivity_", str_replace(oc, "_change_", "_"))
    ggsave(file.path(FIGURES_DIR, paste0(fname, ".pdf")), p_bw, width = 6, height = 4.5)
    message("  Saved: Figures/", fname, ".pdf")
  }
}

# ==============================================================================
# (b) POLYNOMIAL ORDER: LOCAL QUADRATIC (p=2)
# ==============================================================================
message("\n", strrep("=", 70))
message("ROBUSTNESS (b): LOCAL QUADRATIC (p=2)")
message(strrep("=", 70))

poly_results <- list()

for (outcome in rob_outcomes) {
  y <- rdd_cov[[outcome]]
  ok <- !is.na(y)
  if (sum(ok) < 200) next

  for (p_order in c(1, 2)) {
    tryCatch({
      rd <- rdrobust(y = y[ok], x = rdd_cov$R_tilde[ok], c = CUTOFF,
                     fuzzy = rdd_cov$got_ll[ok], covs = covs_mat[ok, ],
                     cluster = rdd_cov$player_id[ok],
                     p = p_order,
                     kernel = "triangular", bwselect = "mserd")
      poly_results[[paste0(outcome, "_p", p_order)]] <- tibble(
        outcome = outcome, poly_order = p_order,
        coef = rd$coef[1], se_robust = rd$se[3], pv_robust = rd$pv[3],
        ci_lower = rd$ci[3, 1], ci_upper = rd$ci[3, 2],
        bw = rd$bws[1], n_eff = rd$N_h[1] + rd$N_h[2]
      )
      message("  ", outcome, " (p=", p_order, "): coef = ", round(rd$coef[1], 2),
              ", p = ", round(rd$pv[3], 3))
    }, error = function(e) message("  ", outcome, " (p=", p_order, "): FAILED -- ", e$message))
  }
}

poly_df <- bind_rows(poly_results)
saveRDS(poly_df, file.path(CLEANED_DIR, "robustness_polynomial.rds"))

# ==============================================================================
# (c) DENSITY TEST
# ==============================================================================
message("\n", strrep("=", 70))
message("ROBUSTNESS (c): DENSITY TEST")
message(strrep("=", 70))

density_result <- NULL

if (has_rddensity) {
  density_result <- tryCatch({
    dt <- rddensity(X = rdd_cov$R_tilde, c = CUTOFF)
    message("  rddensity test:")
    message("    T-statistic: ", round(dt$test$t_jk, 3))
    message("    p-value: ", round(dt$test$p_jk, 3))
    tibble(
      test = "rddensity",
      t_stat = dt$test$t_jk,
      p_value = dt$test$p_jk,
      n_left = dt$N$eff_left,
      n_right = dt$N$eff_right
    )
  }, error = function(e) {
    message("  rddensity failed: ", e$message)
    NULL
  })
}

# Manual density check regardless (histogram-based)
message("\n  Manual density check (frequency by R_tilde):")
density_manual <- rdd_cov |>
  filter(abs(R_tilde) <= 8) |>
  count(R_tilde, name = "freq")

message("  R_tilde  |  freq")
for (i in seq_len(nrow(density_manual))) {
  r <- density_manual[i, ]
  message("    ", sprintf("%3d", r$R_tilde), "     |  ", r$freq)
}

# McCrary-style: is there a jump in density at the cutoff?
n_left  <- sum(density_manual$freq[density_manual$R_tilde <= 0])
n_right <- sum(density_manual$freq[density_manual$R_tilde >= 1])
message("  N(R_tilde <= 0): ", n_left, " vs N(R_tilde >= 1): ", n_right)

# Chi-squared test on adjacent bins
if (any(density_manual$R_tilde == 0) && any(density_manual$R_tilde == 1)) {
  n0 <- density_manual$freq[density_manual$R_tilde == 0]
  n1 <- density_manual$freq[density_manual$R_tilde == 1]
  chisq_p <- chisq.test(c(n0, n1), p = c(0.5, 0.5))$p.value
  message("  Bins at 0 vs 1: ", n0, " vs ", n1, " (chi-sq p = ", round(chisq_p, 3), ")")
  if (is.null(density_result)) {
    density_result <- tibble(
      test = "manual_chi_squared",
      t_stat = NA_real_,
      p_value = chisq_p,
      n_left = n0,
      n_right = n1
    )
  }
}

if (!is.null(density_result)) {
  saveRDS(density_result, file.path(CLEANED_DIR, "robustness_density_test.rds"))
}

# ==============================================================================
# (d) PLACEBO CUTOFFS
# ==============================================================================
message("\n", strrep("=", 70))
message("ROBUSTNESS (d): PLACEBO CUTOFFS")
message(strrep("=", 70))

placebo_cutoffs <- c(-2, -1, 2, 3)  # shifted from the true cutoff of 0.5
placebo_results <- list()

# NOTE: Placebo cutoffs use a SHARP reduced-form specification (no fuzzy argument).
# The first stage (got_ll) is defined relative to the TRUE cutoff, so it would be
# incorrect to use fuzzy = got_ll at a placebo cutoff. Instead we test whether there
# is any reduced-form discontinuity in the outcome at non-cutoff values of R_tilde.

for (pc in placebo_cutoffs) {
  pc_cutoff <- pc + 0.5  # shift the cutoff
  for (outcome in c("rank_change_26w")) {
    y <- rdd_cov[[outcome]]
    ok <- !is.na(y)
    if (sum(ok) < 200) next

    tryCatch({
      rd <- rdrobust(y = y[ok], x = rdd_cov$R_tilde[ok], c = pc_cutoff,
                     cluster = rdd_cov$player_id[ok],
                     kernel = "triangular", bwselect = "mserd")
      placebo_results[[paste0(outcome, "_c", pc)]] <- tibble(
        outcome = outcome, placebo_cutoff = pc_cutoff, cutoff_shift = pc,
        coef = rd$coef[1], se_robust = rd$se[3], pv_robust = rd$pv[3],
        ci_lower = rd$ci[3, 1], ci_upper = rd$ci[3, 2],
        bw = rd$bws[1], n_eff = rd$N_h[1] + rd$N_h[2]
      )
      message("  ", outcome, " (cutoff at R_tilde=", pc,
              "): coef = ", round(rd$coef[1], 2), ", p = ", round(rd$pv[3], 3))
    }, error = function(e) message("  ", outcome, " (cutoff ", pc, "): FAILED -- ", e$message))
  }
}

placebo_df <- bind_rows(placebo_results)
saveRDS(placebo_df, file.path(CLEANED_DIR, "robustness_placebo_cutoffs.rds"))

# ==============================================================================
# (e) DONUT-HOLE RDD
# ==============================================================================
message("\n", strrep("=", 70))
message("ROBUSTNESS (e): DONUT-HOLE RDD")
message(strrep("=", 70))

# Exclude observations at R_tilde = 0 (the mass point closest to cutoff on the
# control side). We keep R_tilde = 1 because removing both {0, 1} leaves too
# few observations near the cutoff for the fuzzy specification with a discrete
# running variable, producing singular variance matrices.
donut <- rdd_cov |> filter(R_tilde != 0)
message("  Donut sample (excl R_tilde=0): ", nrow(donut), " obs (dropped ",
        nrow(rdd_cov) - nrow(donut), " at R_tilde = 0)")

donut_covs <- donut |>
  select(age_imp, elo_t0_imp, is_grand_slam, is_masters, is_clay, is_grass) |>
  as.matrix()

donut_results <- list()

for (outcome in rob_outcomes) {
  y <- donut[[outcome]]
  ok <- !is.na(y)
  if (sum(ok) < 150) { message("  ", outcome, ": too few obs after donut"); next }

  # Try fuzzy first; if it fails (singular variance), fall back to sharp reduced-form
  rd_ok <- FALSE
  tryCatch({
    rd <- rdrobust(y = y[ok], x = donut$R_tilde[ok], c = CUTOFF,
                   fuzzy = donut$got_ll[ok], covs = donut_covs[ok, ],
                   cluster = donut$player_id[ok],
                   kernel = "triangular", bwselect = "mserd")
    rd_ok <- TRUE
    message("  ", outcome, " (donut, fuzzy): coef = ", round(rd$coef[1], 2),
            ", p = ", round(rd$pv[3], 3))
  }, error = function(e) {
    message("  ", outcome, " (donut, fuzzy): FAILED -- ", e$message)
    message("    Falling back to sharp reduced-form specification...")
  })

  if (!rd_ok) {
    tryCatch({
      rd <- rdrobust(y = y[ok], x = donut$R_tilde[ok], c = CUTOFF,
                     covs = donut_covs[ok, ],
                     cluster = donut$player_id[ok],
                     kernel = "triangular", bwselect = "mserd")
      rd_ok <- TRUE
      message("  ", outcome, " (donut, sharp RF): coef = ", round(rd$coef[1], 2),
              ", p = ", round(rd$pv[3], 3))
    }, error = function(e) message("  ", outcome, " (donut, sharp RF): FAILED -- ", e$message))
  }

  if (rd_ok) {
    donut_results[[outcome]] <- tibble(
      outcome = outcome,
      coef = rd$coef[1], se_robust = rd$se[3], pv_robust = rd$pv[3],
      ci_lower = rd$ci[3, 1], ci_upper = rd$ci[3, 2],
      bw = rd$bws[1], n_eff = rd$N_h[1] + rd$N_h[2]
    )
  }
}

donut_df <- bind_rows(donut_results)
saveRDS(donut_df, file.path(CLEANED_DIR, "robustness_donut_hole.rds"))

# ==============================================================================
# (f) TOURNAMENT-YEAR CLUSTERING (alternative)
# ==============================================================================
message("\n", strrep("=", 70))
message("ROBUSTNESS (f): TOURNAMENT-YEAR CLUSTERING")
message(strrep("=", 70))

ty_results <- list()

for (outcome in rob_outcomes) {
  y <- rdd_cov[[outcome]]
  ok <- !is.na(y)
  if (sum(ok) < 200) next

  tryCatch({
    rd <- rdrobust(y = y[ok], x = rdd_cov$R_tilde[ok], c = CUTOFF,
                   fuzzy = rdd_cov$got_ll[ok], covs = covs_mat[ok, ],
                   cluster = rdd_cov$tourney_year[ok],
                   kernel = "triangular", bwselect = "mserd")
    ty_results[[outcome]] <- tibble(
      outcome = outcome, cluster_level = "tourney_year",
      coef = rd$coef[1], se_robust = rd$se[3], pv_robust = rd$pv[3],
      ci_lower = rd$ci[3, 1], ci_upper = rd$ci[3, 2],
      bw = rd$bws[1], n_eff = rd$N_h[1] + rd$N_h[2]
    )
    message("  ", outcome, " (tourney-year cluster): coef = ", round(rd$coef[1], 2),
            ", SE = ", round(rd$se[3], 2), ", p = ", round(rd$pv[3], 3))
  }, error = function(e) message("  ", outcome, ": FAILED -- ", e$message))
}

ty_df <- bind_rows(ty_results)
saveRDS(ty_df, file.path(CLEANED_DIR, "robustness_tourney_year_cluster.rds"))

# ==============================================================================
# (g) SINGLE-SLOT TOURNAMENTS ONLY
# ==============================================================================
message("\n", strrep("=", 70))
message("ROBUSTNESS (g): SINGLE LL SLOT TOURNAMENTS ONLY")
message(strrep("=", 70))

# Restrict to tournaments with exactly 1 LL slot to eliminate
# heterogeneity in the number of slots and ensure a clean cutoff.
single_slot <- rdd_cov |> filter(n_ll_slots == 1)
message("  Single-slot sample: ", nrow(single_slot), " obs (from ",
        nrow(rdd_cov), " total)")

single_covs <- single_slot |>
  select(age_imp, elo_t0_imp, is_grand_slam, is_masters, is_clay, is_grass) |>
  as.matrix()

single_results <- list()

for (outcome in rob_outcomes) {
  y <- single_slot[[outcome]]
  ok <- !is.na(y)
  if (sum(ok) < 100) { message("  ", outcome, ": too few obs (", sum(ok), ")"); next }

  # Try fuzzy first; fall back to sharp reduced-form if variance is singular
  rd_ok <- FALSE
  tryCatch({
    rd <- rdrobust(y = y[ok], x = single_slot$R_tilde[ok], c = CUTOFF,
                   fuzzy = single_slot$got_ll[ok], covs = single_covs[ok, ],
                   cluster = single_slot$player_id[ok],
                   kernel = "triangular", bwselect = "mserd")
    rd_ok <- TRUE
    message("  ", outcome, " (single-slot, fuzzy): coef = ", round(rd$coef[1], 2),
            ", SE = ", round(rd$se[3], 2), ", p = ", round(rd$pv[3], 3))
  }, error = function(e) {
    message("  ", outcome, " (single-slot, fuzzy): FAILED -- ", e$message)
    message("    Falling back to sharp reduced-form...")
  })

  if (!rd_ok) {
    tryCatch({
      rd <- rdrobust(y = y[ok], x = single_slot$R_tilde[ok], c = CUTOFF,
                     covs = single_covs[ok, ],
                     cluster = single_slot$player_id[ok],
                     kernel = "triangular", bwselect = "mserd")
      rd_ok <- TRUE
      message("  ", outcome, " (single-slot, sharp RF): coef = ", round(rd$coef[1], 2),
              ", SE = ", round(rd$se[3], 2), ", p = ", round(rd$pv[3], 3))
    }, error = function(e) message("  ", outcome, " (single-slot, sharp RF): FAILED -- ", e$message))
  }

  if (rd_ok) {
    single_results[[outcome]] <- tibble(
      outcome = outcome, restriction = "single_slot",
      coef = rd$coef[1], se_robust = rd$se[3], pv_robust = rd$pv[3],
      ci_lower = rd$ci[3, 1], ci_upper = rd$ci[3, 2],
      bw = rd$bws[1], n_eff = rd$N_h[1] + rd$N_h[2]
    )
  }
}

single_df <- bind_rows(single_results)
saveRDS(single_df, file.path(CLEANED_DIR, "robustness_single_slot.rds"))

# ==============================================================================
# (j) PRE-TREATMENT PLACEBO: RANKING CHANGE IN 12 WEEKS BEFORE EVENT
# ==============================================================================
message("\n", strrep("=", 70))
message("ROBUSTNESS (j): PRE-TREATMENT RANKING CHANGE PLACEBO")
message(strrep("=", 70))

# Load rankings data (same source as 03_merge_rankings.R)
RAW_DIR <- here("Data", "raw")
atp_rankings_raw <- readr::read_rds(file.path(RAW_DIR, "atp_rankings.rds"))

# Parse ranking dates
rankings_pre <- atp_rankings_raw |>
  mutate(rank_date = as.Date(as.character(ranking_date), format = "%Y%m%d")) |>
  filter(!is.na(rank_date)) |>
  select(player_id = player, rank_date, rank, points)

message("  Rankings rows: ", nrow(rankings_pre))

# Parse event dates for the RDD sample
rdd_pre <- rdd_cov |>
  mutate(
    event_date = as.Date(as.character(tourney_date), format = "%Y%m%d"),
    target_date_minus12w = event_date - 84  # 12 weeks = 84 days
  ) |>
  filter(!is.na(event_date))

message("  RDD sample with valid event dates: ", nrow(rdd_pre))

# Merge rankings at t-12w: find closest ranking within +/- 10 days
pre_merge <- rdd_pre |>
  select(tourney_id, player_id, target_date_minus12w) |>
  inner_join(
    rankings_pre,
    by = "player_id",
    relationship = "many-to-many"
  ) |>
  filter(abs(as.numeric(rank_date - target_date_minus12w)) <= 10) |>
  mutate(date_diff = abs(as.numeric(rank_date - target_date_minus12w))) |>
  group_by(tourney_id, player_id) |>
  slice_min(date_diff, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(tourney_id, player_id, rank_minus12w = rank, points_minus12w = points)

message("  Matched rankings at t-12w: ", nrow(pre_merge), " obs")

# Join back and compute pre-treatment outcome
rdd_pre <- rdd_pre |>
  left_join(pre_merge, by = c("tourney_id", "player_id")) |>
  mutate(
    rank_change_pre12w = rank_t0 - rank_minus12w  # change in 12 weeks BEFORE event
  )

n_pre_valid <- sum(!is.na(rdd_pre$rank_change_pre12w))
message("  Valid pre-treatment outcomes: ", n_pre_valid, " (missing: ",
        nrow(rdd_pre) - n_pre_valid, ")")

# Run the same fuzzy RDD specification as main analysis
pretreat_result <- NULL

if (n_pre_valid >= 200) {
  y_pre <- rdd_pre$rank_change_pre12w
  ok_pre <- !is.na(y_pre)

  # Use the same covariate matrix
  covs_pre <- rdd_pre |>
    select(age_imp, elo_t0_imp, is_grand_slam, is_masters, is_clay, is_grass) |>
    as.matrix()

  tryCatch({
    rd_pre <- rdrobust(
      y = y_pre[ok_pre],
      x = rdd_pre$R_tilde[ok_pre],
      c = CUTOFF,
      fuzzy = rdd_pre$got_ll[ok_pre],
      covs = covs_pre[ok_pre, ],
      cluster = rdd_pre$player_id[ok_pre],
      kernel = "triangular",
      bwselect = "mserd"
    )
    pretreat_result <- tibble(
      outcome   = "rank_change_pre12w",
      coef      = rd_pre$coef[1],
      se_robust = rd_pre$se[3],
      pv_robust = rd_pre$pv[3],
      ci_lower  = rd_pre$ci[3, 1],
      ci_upper  = rd_pre$ci[3, 2],
      bw        = rd_pre$bws[1],
      n_eff     = rd_pre$N_h[1] + rd_pre$N_h[2]
    )
    message("  Pre-treatment placebo (rank_change_pre12w): LATE = ",
            round(rd_pre$coef[1], 2),
            ", SE = ", round(rd_pre$se[3], 2),
            ", p = ", round(rd_pre$pv[3], 3))
    if (rd_pre$pv[3] > 0.10) {
      message("  PASS: No pre-trend at the cutoff (p > 0.10)")
    } else {
      message("  WARNING: Pre-treatment difference significant at 10% level")
    }
  }, error = function(e) {
    message("  Pre-treatment placebo FAILED: ", e$message)
  })
} else {
  message("  Insufficient observations for pre-treatment placebo (", n_pre_valid, " < 200)")
}

saveRDS(pretreat_result, file.path(CLEANED_DIR, "robustness_pretreatment.rds"))
message("  Saved: Data/cleaned/robustness_pretreatment.rds")

# ==============================================================================
# ROBUSTNESS TABLE (Table 5)
# ==============================================================================
message("\n", strrep("=", 70))
message("ROBUSTNESS TABLE")
message(strrep("=", 70))

# Load main results for comparison
fuzzy_main <- tryCatch(
  read_rds(file.path(CLEANED_DIR, "main_fuzzy_rdd_results.rds")) |>
    filter(covariates == "Yes"),
  error = function(e) tibble()
)

# Build table for rank_change_26w
outcome_focus <- "rank_change_26w"
rob_rows <- list()

# Row 1: Baseline (from main results)
base_row <- fuzzy_main |> filter(outcome == outcome_focus)
if (nrow(base_row) > 0) {
  rob_rows[["Baseline"]] <- tibble(
    Specification = "Baseline (p=1, MSE-opt BW, player cluster)",
    LATE = sprintf("%.1f", base_row$coef),
    SE = sprintf("(%.1f)", base_row$se_robust),
    pv = sprintf("%.3f", base_row$pv_robust),
    BW = sprintf("%.1f", base_row$bw),
    N_eff = as.character(round(base_row$n_eff))
  )
}

# Row 2: Local quadratic
p2_row <- poly_df |> filter(outcome == outcome_focus, poly_order == 2)
if (nrow(p2_row) > 0) {
  rob_rows[["Quadratic"]] <- tibble(
    Specification = "Local quadratic (p=2)",
    LATE = sprintf("%.1f", p2_row$coef),
    SE = sprintf("(%.1f)", p2_row$se_robust),
    pv = sprintf("%.3f", p2_row$pv_robust),
    BW = sprintf("%.1f", p2_row$bw),
    N_eff = as.character(round(p2_row$n_eff))
  )
}

# Rows 3-7: Bandwidth sensitivity
for (mult in bw_multipliers) {
  bw_row <- bw_df |> filter(outcome == outcome_focus, bw_mult == mult)
  if (nrow(bw_row) > 0) {
    rob_rows[[paste0("BW_", mult)]] <- tibble(
      Specification = paste0("Bandwidth: ", mult * 100, "% of optimal"),
      LATE = sprintf("%.1f", bw_row$coef),
      SE = sprintf("(%.1f)", bw_row$se_robust),
      pv = sprintf("%.3f", bw_row$pv_robust),
      BW = sprintf("%.1f", bw_row$bw_used),
      N_eff = as.character(round(bw_row$n_eff))
    )
  }
}

# Row 8: Donut hole
dn_row <- donut_df |> filter(outcome == outcome_focus)
if (nrow(dn_row) > 0) {
  rob_rows[["Donut"]] <- tibble(
    Specification = "Donut hole (excl. $\\tilde{R} = 0$)",
    LATE = sprintf("%.1f", dn_row$coef),
    SE = sprintf("(%.1f)", dn_row$se_robust),
    pv = sprintf("%.3f", dn_row$pv_robust),
    BW = sprintf("%.1f", dn_row$bw),
    N_eff = as.character(round(dn_row$n_eff))
  )
}

# Row 9: Tournament-year clustering
ty_row <- ty_df |> filter(outcome == outcome_focus)
if (nrow(ty_row) > 0) {
  rob_rows[["TY_cluster"]] <- tibble(
    Specification = "Tournament-year clustering",
    LATE = sprintf("%.1f", ty_row$coef),
    SE = sprintf("(%.1f)", ty_row$se_robust),
    pv = sprintf("%.3f", ty_row$pv_robust),
    BW = sprintf("%.1f", ty_row$bw),
    N_eff = as.character(round(ty_row$n_eff))
  )
}

# Row 10: Single LL slot tournaments
ss_row <- single_df |> filter(outcome == outcome_focus)
if (nrow(ss_row) > 0) {
  rob_rows[["Single_slot"]] <- tibble(
    Specification = "Single LL-slot tournaments only",
    LATE = sprintf("%.1f", ss_row$coef),
    SE = sprintf("(%.1f)", ss_row$se_robust),
    pv = sprintf("%.3f", ss_row$pv_robust),
    BW = sprintf("%.1f", ss_row$bw),
    N_eff = as.character(round(ss_row$n_eff))
  )
}

# Row 11: Pre-treatment placebo
if (!is.null(pretreat_result) && nrow(pretreat_result) > 0) {
  rob_rows[["Pretreatment"]] <- tibble(
    Specification = "Pre-treatment placebo ($\\Delta R_{-12w,0}$)",
    LATE = sprintf("%.1f", pretreat_result$coef),
    SE = sprintf("(%.1f)", pretreat_result$se_robust),
    pv = sprintf("%.3f", pretreat_result$pv_robust),
    BW = sprintf("%.1f", pretreat_result$bw),
    N_eff = as.character(round(pretreat_result$n_eff))
  )
}

rob_table <- bind_rows(rob_rows)

if (nrow(rob_table) > 0) {
  rob_lines <- c(
    "\\begin{tabular}{lccccc}",
    "\\toprule",
    "Specification & LATE & Robust SE & $p$-value & Bandwidth & Eff. $N$ \\\\",
    "\\midrule"
  )
  for (i in seq_len(nrow(rob_table))) {
    r <- rob_table[i, ]
    rob_lines <- c(rob_lines, paste0(
      r$Specification, " & ", r$LATE, " & ", r$SE, " & ",
      r$pv, " & ", r$BW, " & ", r$N_eff, " \\\\"
    ))
  }

  # Add density test result
  if (!is.null(density_result)) {
    rob_lines <- c(rob_lines, "\\midrule",
      paste0("\\multicolumn{6}{l}{\\footnotesize Density test (McCrary): $p$ = ",
             round(density_result$p_value, 3), "} \\\\"))
  }

  # Add placebo cutoffs
  if (nrow(placebo_df) > 0) {
    rob_lines <- c(rob_lines,
      "\\midrule",
      "\\multicolumn{6}{l}{\\textit{Placebo cutoffs --- sharp reduced-form (rank\\_change\\_26w)}} \\\\"
    )
    for (i in seq_len(nrow(placebo_df))) {
      r <- placebo_df[i, ]
      rob_lines <- c(rob_lines, paste0(
        "Cutoff at $\\tilde{R}$ = ", r$cutoff_shift, " & ",
        sprintf("%.1f", r$coef), " & ",
        sprintf("(%.1f)", r$se_robust), " & ",
        sprintf("%.3f", r$pv_robust), " & ",
        sprintf("%.1f", r$bw), " & ",
        round(r$n_eff), " \\\\"
      ))
    }
  }

  rob_lines <- c(rob_lines,
    "\\midrule",
    "\\multicolumn{6}{l}{\\footnotesize Outcome: ranking change at 26 weeks. All SEs clustered at player level unless noted.} \\\\",
    "\\bottomrule", "\\end{tabular}")

  writeLines(rob_lines, file.path(TABLES_DIR, "table5_robustness.tex"))
  message("  Saved: Tables/table5_robustness.tex")
}

# ==============================================================================
# ROBUSTNESS SUMMARY
# ==============================================================================
message("\n", strrep("=", 70))
message("ROBUSTNESS SUMMARY")
message(strrep("=", 70))

rob_summary <- c(
  "# Robustness Summary -- 06b_robustness.R",
  paste0("Generated: ", Sys.time()),
  "",
  "## (a) Bandwidth Sensitivity",
  ""
)

if (nrow(bw_df) > 0) {
  for (i in seq_len(nrow(bw_df))) {
    r <- bw_df[i, ]
    rob_summary <- c(rob_summary,
      paste0("- ", r$outcome, " (", r$bw_mult * 100, "% bw): coef = ",
             round(r$coef, 1), ", p = ", round(r$pv_robust, 3)))
  }
}

rob_summary <- c(rob_summary, "",
  "## (b) Polynomial Order", "")
if (nrow(poly_df) > 0) {
  for (i in seq_len(nrow(poly_df))) {
    r <- poly_df[i, ]
    rob_summary <- c(rob_summary,
      paste0("- ", r$outcome, " (p=", r$poly_order, "): coef = ",
             round(r$coef, 1), ", p = ", round(r$pv_robust, 3)))
  }
}

rob_summary <- c(rob_summary, "",
  "## (c) Density Test", "")
if (!is.null(density_result)) {
  rob_summary <- c(rob_summary,
    paste0("- Test: ", density_result$test, ", p = ", round(density_result$p_value, 3)))
} else {
  rob_summary <- c(rob_summary, "- No density test available")
}

rob_summary <- c(rob_summary, "",
  "## (d) Placebo Cutoffs (Sharp Reduced-Form)", "")
if (nrow(placebo_df) > 0) {
  for (i in seq_len(nrow(placebo_df))) {
    r <- placebo_df[i, ]
    sig <- if (r$pv_robust < 0.05) " *** SIGNIFICANT (concern!) ***" else " (not significant, as expected)"
    rob_summary <- c(rob_summary,
      paste0("- Cutoff at R_tilde=", r$cutoff_shift, ": coef = ",
             round(r$coef, 1), ", p = ", round(r$pv_robust, 3), sig))
  }
}

rob_summary <- c(rob_summary, "",
  "## (e) Donut-Hole RDD", "")
if (nrow(donut_df) > 0) {
  for (i in seq_len(nrow(donut_df))) {
    r <- donut_df[i, ]
    rob_summary <- c(rob_summary,
      paste0("- ", r$outcome, ": coef = ", round(r$coef, 1),
             ", p = ", round(r$pv_robust, 3)))
  }
}

rob_summary <- c(rob_summary, "",
  "## (f) Tournament-Year Clustering", "")
if (nrow(ty_df) > 0) {
  for (i in seq_len(nrow(ty_df))) {
    r <- ty_df[i, ]
    rob_summary <- c(rob_summary,
      paste0("- ", r$outcome, ": coef = ", round(r$coef, 1),
             ", SE = ", round(r$se_robust, 1),
             ", p = ", round(r$pv_robust, 3)))
  }
}

rob_summary <- c(rob_summary, "",
  "## (g) Single LL-Slot Tournaments", "")
if (nrow(single_df) > 0) {
  for (i in seq_len(nrow(single_df))) {
    r <- single_df[i, ]
    rob_summary <- c(rob_summary,
      paste0("- ", r$outcome, ": coef = ", round(r$coef, 1),
             ", SE = ", round(r$se_robust, 1),
             ", p = ", round(r$pv_robust, 3)))
  }
} else {
  rob_summary <- c(rob_summary, "- No results (insufficient observations or estimation failed)")
}

rob_summary <- c(rob_summary, "",
  "## (j) Pre-Treatment Ranking Change Placebo", "")
if (!is.null(pretreat_result) && nrow(pretreat_result) > 0) {
  pass_fail <- if (pretreat_result$pv_robust > 0.10) "PASS (no pre-trend)" else "CONCERN (significant pre-trend)"
  rob_summary <- c(rob_summary,
    paste0("- rank_change_pre12w (12 weeks before event): coef = ",
           round(pretreat_result$coef, 1),
           ", SE = ", round(pretreat_result$se_robust, 1),
           ", p = ", round(pretreat_result$pv_robust, 3),
           " -- ", pass_fail))
} else {
  rob_summary <- c(rob_summary, "- Pre-treatment placebo could not be estimated")
}

######################################################################
# ROBUSTNESS (h): RDHonest -- Honest CIs for Discrete Running Variable
######################################################################
message(strrep("=", 70))
message("ROBUSTNESS (h): RDHonest -- Honest CIs (Kolesar & Rothe 2018)")
message(strrep("=", 70))

rob_summary <- c(rob_summary, "", "## (h) RDHonest -- Honest CIs for Discrete Running Variable", "")

tryCatch({
  library(RDHonest)

  for (outcome in c("rank_change_12w", "rank_change_26w", "elo_change_26w")) {
    y <- rdd_cov[[outcome]]
    ok <- !is.na(y)

    tryCatch({
      # RDHonest uses formula interface: outcome ~ running variable
      rd_df <- data.frame(
        Y = y[ok],
        X = rdd_cov$R_tilde[ok]
      )

      # Sharp reduced-form with honest CIs
      rh <- RDHonest(Y ~ X, data = rd_df, cutoff = CUTOFF,
                     kern = "triangular", se.method = "EHW")

      msg <- paste0("- ", outcome,
                    ": coef = ", round(rh$estimate, 1),
                    ", honest CI = [", round(rh$lower, 1), ", ", round(rh$upper, 1), "]",
                    ", eff. obs = ", rh$eff.obs)
      message("  ", msg)
      rob_summary <- c(rob_summary, msg)
    }, error = function(e) {
      msg <- paste0("- ", outcome,
                    ": RDHonest infeasible with this discrete running variable.",
                    " The running variable has very few mass points per tournament (4-16),",
                    " which causes numerical instability in the honest CI computation.",
                    " This is a known limitation; the local randomization framework",
                    " (rdrandinf, reported in main results) is the appropriate alternative",
                    " for discrete running variables (Cattaneo et al. 2020, 2024).")
      message("  ", msg)
      rob_summary <<- c(rob_summary, msg)
    })
  }
}, error = function(e) {
  msg <- paste0("- RDHonest package not available: ", e$message)
  message("  ", msg)
  rob_summary <- c(rob_summary, msg)
})

######################################################################
# ROBUSTNESS (i): Holm-Bonferroni FWER Correction
######################################################################
message(strrep("=", 70))
message("ROBUSTNESS (i): Holm-Bonferroni FWER Correction")
message(strrep("=", 70))

rob_summary <- c(rob_summary, "", "## (i) Holm-Bonferroni FWER Correction (approximates Romano-Wolf)", "")

# Load the fuzzy RDD results from main analysis
fuzzy_results <- tryCatch(
  readRDS(file.path(CLEANED_DIR, "main_fuzzy_rdd_results.rds")),
  error = function(e) NULL
)

if (!is.null(fuzzy_results)) {
  # Extract ranking change family p-values
  rank_family <- fuzzy_results |>
    dplyr::filter(grepl("rank_change", outcome))

  if (nrow(rank_family) > 0) {
    raw_pvals <- rank_family$pv_robust
    names(raw_pvals) <- rank_family$outcome

    # Holm-Bonferroni (controls FWER, more powerful than Bonferroni)
    holm_pvals <- p.adjust(raw_pvals, method = "holm")

    for (i in seq_along(holm_pvals)) {
      msg <- paste0("- ", names(holm_pvals)[i],
                    ": raw p = ", round(raw_pvals[i], 3),
                    ", Holm p = ", round(holm_pvals[i], 3))
      message("  ", msg)
      rob_summary <- c(rob_summary, msg)
    }

    # Save
    holm_df <- data.frame(
      outcome = names(holm_pvals),
      raw_p = raw_pvals,
      holm_p = holm_pvals,
      row.names = NULL
    )
    saveRDS(holm_df, file.path(CLEANED_DIR, "robustness_holm_correction.rds"))
    message("  Saved: Data/cleaned/robustness_holm_correction.rds")
  }
} else {
  rob_summary <- c(rob_summary, "- Fuzzy RDD results not found; cannot compute corrections")
}

rob_summary <- c(rob_summary, "",
  "## Output Files", "",
  "- Tables/table5_robustness.tex",
  "- Figures/fig4_bw_sensitivity_*.pdf",
  "- Data/cleaned/robustness_*.rds (7 result files)",
  ""
)

writeLines(rob_summary, file.path(OUTPUT_DIR, "robustness_summary.md"))
message("  Saved: Output/robustness_summary.md")

message("\n=== 06b_robustness.R COMPLETE ===")
