# ==============================================================================
# 09_revisions.R
# Referee-requested revisions:
#   Task 1: Add WTA lottery data and pool ATP+WTA Grand Slam lottery samples
#   Task 2: Lottery event study figure (ranking trajectories)
#   Task 3: Placebo cutoff investigation (full distribution)
#   Task 4: Table 5 SE fix (consistent bias-corrected robust SEs)
#   Task 5: Expanded balance tests (single BW, additional covariates)
#   Task 6: Power analysis (lottery MDE + RDD)
#   Task 7: Unique cluster counts
# Inputs:  Data/raw/*.rds, Data/cleaned/estimation_sample_final.rds
# Outputs: Data/cleaned/pooled_gs_lottery_results.rds,
#          Figures/fig_lottery_event_study.pdf,
#          Figures/fig_placebo_distribution.pdf,
#          Tables/table5_robustness.tex (fixed),
#          Tables/table1_balance.tex (expanded),
#          Output/power_analysis.md, Output/cluster_counts.md
# Dependencies: rdrobust, fixest, ggplot2, here
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

# --- Custom theme (matches 06_main_analysis.R) --------------------------------
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

# ==============================================================================
# LOAD COMMON DATA
# ==============================================================================
message("=== Loading common data ===")
est <- read_rds(file.path(CLEANED_DIR, "estimation_sample_final.rds"))
rdd <- est |> filter(n_ll_slots > 0)
message("  ATP RDD sample: ", nrow(rdd), " obs")

# Prepare RDD covariates (same as 06_main_analysis.R)
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

# ==============================================================================
# TASK 1: ADD WTA LOTTERY DATA AND POOL ATP+WTA GRAND SLAM SAMPLES
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 1: WTA GRAND SLAM LOTTERY SAMPLE")
message(strrep("=", 70))

# --- 1a. Load WTA data -------------------------------------------------------
message("\n--- 1a. Loading WTA data ---")
wta_main     <- read_rds(file.path(RAW_DIR, "wta_main_matches.rds"))
wta_qual     <- read_rds(file.path(RAW_DIR, "wta_qual_itf_matches.rds"))
wta_rankings <- read_rds(file.path(RAW_DIR, "wta_rankings.rds"))
wta_players  <- read_rds(file.path(RAW_DIR, "wta_players.rds"))
message("  WTA main matches: ", nrow(wta_main))
message("  WTA qual/ITF matches: ", nrow(wta_qual))
message("  WTA rankings rows: ", nrow(wta_rankings))

# --- 1b. Identify WTA final-round qualifying losers at Grand Slams -----------
message("\n--- 1b. Identifying WTA Grand Slam qualifying losers ---")

wta_qual_gs <- wta_qual |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(
    year >= 2000,
    tourney_level == "G",
    str_detect(round, "^Q")
  )
message("  WTA Grand Slam qualifying matches (2000+): ", nrow(wta_qual_gs))

# Determine final qualifying round per tournament
wta_final_qual_round <- wta_qual_gs |>
  group_by(tourney_id) |>
  summarise(max_qual_round = max(round), .groups = "drop")

wta_final_qual_matches <- wta_qual_gs |>
  inner_join(wta_final_qual_round, by = "tourney_id") |>
  filter(round == max_qual_round)

message("  WTA GS final qualifying round matches: ", nrow(wta_final_qual_matches))

# Extract losers
wta_qual_losers <- wta_final_qual_matches |>
  transmute(
    tourney_id, tourney_name, tourney_date,
    tourney_level, surface, draw_size,
    year = as.integer(str_sub(tourney_date, 1, 4)),
    player_id = loser_id,
    player_name = loser_name,
    player_rank = loser_rank,
    player_rank_points = loser_rank_points,
    player_age = loser_age,
    player_hand = loser_hand,
    player_ht = loser_ht,
    player_ioc = loser_ioc,
    tour = "WTA"
  )
message("  WTA GS qualifying losers: ", nrow(wta_qual_losers))

# --- 1c. Identify WTA LL entries in Grand Slams ------------------------------
message("\n--- 1c. Identifying WTA LL entries ---")

wta_ll_w <- wta_main |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(year >= 2000, tourney_level == "G", winner_entry == "LL") |>
  transmute(tourney_id, ll_player_id = winner_id, ll_won = 1L)

wta_ll_l <- wta_main |>
  mutate(year = as.integer(str_sub(tourney_date, 1, 4))) |>
  filter(year >= 2000, tourney_level == "G", loser_entry == "LL") |>
  transmute(tourney_id, ll_player_id = loser_id, ll_won = 0L)

wta_ll_all <- bind_rows(wta_ll_w, wta_ll_l) |>
  group_by(tourney_id, ll_player_id) |>
  summarise(ll_matches = n(), .groups = "drop")

message("  WTA GS LL entries (unique player-tournament): ", nrow(wta_ll_all))

# Flag LL status
wta_qual_losers <- wta_qual_losers |>
  left_join(
    wta_ll_all |> transmute(tourney_id, player_id = ll_player_id, got_ll = 1L),
    by = c("tourney_id", "player_id")
  ) |>
  mutate(got_ll = replace_na(got_ll, 0L))

message("  WTA GS qualifying losers who got LL: ", sum(wta_qual_losers$got_ll))

# --- 1d. Rank among losers and restrict to top-4 pool ------------------------
message("\n--- 1d. Building WTA top-4 lottery pool ---")

wta_qual_losers <- wta_qual_losers |>
  group_by(tourney_id) |>
  mutate(
    n_losers = n(),
    rank_among_losers = rank(
      ifelse(is.na(player_rank), 9999, player_rank),
      ties.method = "min"
    )
  ) |>
  ungroup()

wta_gs_pool <- wta_qual_losers |>
  filter(rank_among_losers <= 4)

message("  WTA GS top-4 pool: ", nrow(wta_gs_pool),
        " obs (LL: ", sum(wta_gs_pool$got_ll), ")")

# --- 1e. Merge WTA rankings for outcome trajectories -------------------------
message("\n--- 1e. Merging WTA rankings for trajectories ---")

wta_rankings_parsed <- wta_rankings |>
  mutate(rank_date = as.Date(as.character(ranking_date), format = "%Y%m%d")) |>
  filter(!is.na(rank_date)) |>
  select(player_id = player, rank_date, rank, points)

wta_gs_pool <- wta_gs_pool |>
  mutate(event_date = as.Date(as.character(tourney_date), format = "%Y%m%d"))

# Merge rankings at multiple horizons
wta_horizons <- c(-12, -8, -4, 0, 4, 8, 12, 26, 52)
wta_traj_list <- list()

for (h in wta_horizons) {
  h_label <- ifelse(h < 0, paste0("minus", abs(h)), as.character(h))
  target_df <- wta_gs_pool |>
    filter(!is.na(event_date)) |>
    transmute(tourney_id, player_id, target_date = event_date + h * 7)

  matched <- target_df |>
    inner_join(wta_rankings_parsed, by = "player_id", relationship = "many-to-many") |>
    filter(abs(as.numeric(rank_date - target_date)) <= 10) |>
    mutate(date_diff = abs(as.numeric(rank_date - target_date))) |>
    group_by(tourney_id, player_id) |>
    slice_min(date_diff, n = 1, with_ties = FALSE) |>
    ungroup()

  col_name <- paste0("rank_t", h_label)
  pts_name <- paste0("points_t", h_label)
  matched <- matched |>
    select(tourney_id, player_id, !!col_name := rank, !!pts_name := points)

  wta_traj_list[[h_label]] <- matched
}

# Join all horizons
for (h_label in names(wta_traj_list)) {
  wta_gs_pool <- wta_gs_pool |>
    left_join(wta_traj_list[[h_label]], by = c("tourney_id", "player_id"))
}

# Compute ranking changes
wta_gs_pool <- wta_gs_pool |>
  mutate(
    rank_change_12w = rank_t12 - rank_t0,
    rank_change_26w = rank_t26 - rank_t0,
    rank_change_52w = rank_t52 - rank_t0
  )

message("  WTA pool with trajectories: ", nrow(wta_gs_pool))
message("  Non-missing rank_change_26w: ", sum(!is.na(wta_gs_pool$rank_change_26w)))

# --- 1f. Build ATP Grand Slam top-4 pool (parallel structure) ----------------
message("\n--- 1f. Building ATP Grand Slam top-4 pool ---")

# Load ATP rankings for pre-event horizons
atp_rankings_raw <- read_rds(file.path(RAW_DIR, "atp_rankings.rds"))
atp_rankings_parsed <- atp_rankings_raw |>
  mutate(rank_date = as.Date(as.character(ranking_date), format = "%Y%m%d")) |>
  filter(!is.na(rank_date)) |>
  select(player_id = player, rank_date, rank, points)

atp_gs <- rdd_cov |>
  filter(tourney_level == "G", rank_among_losers <= 4) |>
  mutate(
    tour = "ATP",
    event_date = as.Date(as.character(tourney_date), format = "%Y%m%d")
  )

message("  ATP GS top-4 pool: ", nrow(atp_gs),
        " obs (LL: ", sum(atp_gs$got_ll), ")")

# Merge pre-event ranking horizons for ATP (we already have post-event from 03)
atp_pre_horizons <- c(-12, -8, -4)
for (h in atp_pre_horizons) {
  h_label <- paste0("minus", abs(h))
  target_df <- atp_gs |>
    filter(!is.na(event_date)) |>
    transmute(tourney_id, player_id, target_date = event_date + h * 7)

  matched <- target_df |>
    inner_join(atp_rankings_parsed, by = "player_id", relationship = "many-to-many") |>
    filter(abs(as.numeric(rank_date - target_date)) <= 10) |>
    mutate(date_diff = abs(as.numeric(rank_date - target_date))) |>
    group_by(tourney_id, player_id) |>
    slice_min(date_diff, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(tourney_id, player_id, !!paste0("rank_t", h_label) := rank)

  atp_gs <- atp_gs |>
    left_join(matched, by = c("tourney_id", "player_id"))
}

# --- 1g. Pool ATP + WTA Grand Slam lottery samples ---------------------------
message("\n--- 1g. Pooling ATP + WTA Grand Slam lottery samples ---")

# Harmonize columns -- use helper to safely get columns
safe_col <- function(df, col) {
  if (col %in% names(df)) df[[col]] else NA_real_
}

common_cols <- c("tour", "tourney_id", "tourney_name", "tourney_date", "year",
                 "player_id", "player_name", "player_rank", "player_age",
                 "player_hand", "player_ht", "player_ioc",
                 "got_ll", "rank_among_losers", "event_date")

rank_horizon_cols <- c("rank_tminus12", "rank_tminus8", "rank_tminus4",
                       "rank_t0", "rank_t4", "rank_t8", "rank_t12", "rank_t26", "rank_t52")
outcome_cols <- c("rank_change_12w", "rank_change_26w", "rank_change_52w")

build_pool <- function(df, tour_label) {
  out <- df |> select(any_of(common_cols))
  for (rc in rank_horizon_cols) {
    out[[rc]] <- safe_col(df, rc)
  }
  for (oc in outcome_cols) {
    out[[oc]] <- safe_col(df, oc)
  }
  out$elo_t0 <- safe_col(df, "elo_t0")
  out$tour <- tour_label
  out
}

atp_pool <- build_pool(atp_gs, "ATP")
wta_pool <- build_pool(wta_gs_pool, "WTA")

pooled_gs <- bind_rows(atp_pool, wta_pool)
message("  Pooled GS lottery sample: ", nrow(pooled_gs),
        " (ATP: ", sum(pooled_gs$tour == "ATP"),
        ", WTA: ", sum(pooled_gs$tour == "WTA"), ")")
message("  Pooled LL: ", sum(pooled_gs$got_ll),
        " (ATP: ", sum(pooled_gs$got_ll & pooled_gs$tour == "ATP"),
        ", WTA: ", sum(pooled_gs$got_ll & pooled_gs$tour == "WTA"), ")")

# --- 1h. Lottery difference-in-means and OLS with controls -------------------
message("\n--- 1h. Pooled lottery analysis ---")

pooled_gs <- pooled_gs |>
  mutate(
    age_imp = replace_na(player_age, median(player_age, na.rm = TRUE)),
    is_wta  = as.integer(tour == "WTA")
  )

gs_lottery_outcomes <- c("rank_change_12w", "rank_change_26w", "rank_change_52w")
pooled_results <- list()

for (outcome in gs_lottery_outcomes) {
  y <- pooled_gs[[outcome]]
  ok <- !is.na(y)
  if (sum(ok) < 20) {
    message("  ", outcome, ": too few obs (", sum(ok), ")")
    next
  }

  tr <- pooled_gs$got_ll[ok] == 1
  if (sum(tr) < 3 || sum(!tr) < 3) {
    message("  ", outcome, ": insufficient treatment variation")
    next
  }

  diff_means <- mean(y[ok][tr]) - mean(y[ok][!tr])
  tt <- tryCatch(t.test(y[ok] ~ pooled_gs$got_ll[ok]), error = function(e) NULL)

  # OLS with controls, clustered at player level
  gs_sub <- pooled_gs[ok, ]
  ols <- tryCatch({
    feols(as.formula(paste0(outcome, " ~ got_ll + age_imp + is_wta | year")),
          data = gs_sub, vcov = ~player_id)
  }, error = function(e) NULL)

  pooled_results[[outcome]] <- tibble(
    outcome     = outcome,
    sample      = "Pooled ATP+WTA",
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
          " (t-test p = ", round(pooled_results[[outcome]]$dm_pvalue, 3),
          ", N = ", sum(ok), " [", sum(tr), " LL, ", sum(!tr), " ctrl])",
          if (!is.null(ols)) paste0(", OLS coef = ", round(coef(ols)["got_ll"], 2)) else "")
}

pooled_df <- bind_rows(pooled_results)

# BH correction on pooled results
if (nrow(pooled_df) > 1) {
  pooled_df <- pooled_df |>
    mutate(
      dm_pvalue_bh  = p.adjust(dm_pvalue, method = "BH"),
      ols_pvalue_bh = p.adjust(ols_pvalue, method = "BH")
    )
  message("\n  BH-adjusted p-values (pooled):")
  for (i in seq_len(nrow(pooled_df))) {
    r <- pooled_df[i, ]
    message("    ", r$outcome,
            ": raw t-test p = ", round(r$dm_pvalue, 3),
            ", BH p = ", round(r$dm_pvalue_bh, 3))
  }
}

saveRDS(pooled_df, file.path(CLEANED_DIR, "pooled_gs_lottery_results.rds"))
saveRDS(pooled_gs, file.path(CLEANED_DIR, "pooled_gs_lottery_sample.rds"))
message("  Saved: Data/cleaned/pooled_gs_lottery_results.rds")
message("  Saved: Data/cleaned/pooled_gs_lottery_sample.rds")


# ==============================================================================
# TASK 2: LOTTERY EVENT STUDY FIGURE
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 2: LOTTERY EVENT STUDY FIGURE")
message(strrep("=", 70))

# Build trajectory data for event study plot
# Horizons: t-12w, t-8w, t-4w, t0, t+4w, t+8w, t+12w, t+26w, t+52w
rank_cols <- c("rank_tminus12", "rank_tminus8", "rank_tminus4",
               "rank_t0", "rank_t4", "rank_t8", "rank_t12", "rank_t26", "rank_t52")
week_vals <- c(-12, -8, -4, 0, 4, 8, 12, 26, 52)

# Check which columns exist
available_cols <- rank_cols[rank_cols %in% names(pooled_gs)]
available_weeks <- week_vals[rank_cols %in% names(pooled_gs)]
message("  Available horizon columns: ", paste(available_cols, collapse = ", "))

if (length(available_cols) >= 3) {
  es_long <- pooled_gs |>
    select(tourney_id, player_id, got_ll, tour, all_of(available_cols)) |>
    pivot_longer(
      cols = all_of(available_cols),
      names_to = "horizon_col",
      values_to = "ranking"
    ) |>
    mutate(
      weeks = case_when(
        horizon_col == "rank_tminus12" ~ -12L,
        horizon_col == "rank_tminus8"  ~ -8L,
        horizon_col == "rank_tminus4"  ~ -4L,
        horizon_col == "rank_t0"       ~ 0L,
        horizon_col == "rank_t4"       ~ 4L,
        horizon_col == "rank_t8"       ~ 8L,
        horizon_col == "rank_t12"      ~ 12L,
        horizon_col == "rank_t26"      ~ 26L,
        horizon_col == "rank_t52"      ~ 52L
      ),
      group = ifelse(got_ll == 1, "LL Selected", "Not Selected")
    ) |>
    filter(!is.na(ranking))

  # Compute means and CIs by group and horizon
  es_summary <- es_long |>
    group_by(weeks, group) |>
    summarise(
      mean_rank = mean(ranking, na.rm = TRUE),
      se_rank   = sd(ranking, na.rm = TRUE) / sqrt(n()),
      n         = n(),
      .groups   = "drop"
    ) |>
    mutate(
      ci_lower = mean_rank - 1.96 * se_rank,
      ci_upper = mean_rank + 1.96 * se_rank
    )

  message("  Event study data points: ", nrow(es_summary))
  print(es_summary)

  # Plot
  p_lottery_es <- ggplot(es_summary, aes(x = weeks, y = mean_rank,
                                          color = group, fill = group)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.15, color = NA) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    scale_y_reverse() +
    scale_x_continuous(breaks = available_weeks) +
    scale_color_manual(values = c("LL Selected" = "#2166AC", "Not Selected" = "#B2182B")) +
    scale_fill_manual(values = c("LL Selected" = "#2166AC", "Not Selected" = "#B2182B")) +
    labs(x = "Weeks Relative to Tournament",
         y = "Average Ranking (Lower = Better)",
         title = NULL) +
    theme_paper() +
    theme(legend.position = "bottom")

  ggsave(file.path(FIGURES_DIR, "fig_lottery_event_study.pdf"),
         p_lottery_es, width = 7, height = 5)
  message("  Saved: Figures/fig_lottery_event_study.pdf")
} else {
  message("  WARNING: Insufficient horizon data for event study figure")
}


# ==============================================================================
# TASK 3: PLACEBO CUTOFF INVESTIGATION (FULL DISTRIBUTION)
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 3: PLACEBO CUTOFF DISTRIBUTION")
message(strrep("=", 70))

# Run sharp reduced-form RDD at every integer cutoff from R_tilde = -3 to +5
placebo_cutoff_range <- -3:5
placebo_full_results <- list()

outcome_placebo <- "rank_change_26w"
y_placebo <- rdd_cov[[outcome_placebo]]
ok_placebo <- !is.na(y_placebo)

for (pc in placebo_cutoff_range) {
  pc_cutoff <- pc + 0.5  # place between integers

  tryCatch({
    rd <- rdrobust(
      y = y_placebo[ok_placebo],
      x = rdd_cov$R_tilde[ok_placebo],
      c = pc_cutoff,
      cluster = rdd_cov$player_id[ok_placebo],
      kernel = "triangular", bwselect = "mserd"
    )
    placebo_full_results[[as.character(pc)]] <- tibble(
      cutoff_shift  = pc,
      cutoff_value  = pc_cutoff,
      coef          = rd$coef[1],
      se_robust     = rd$se[3],
      pv_robust     = rd$pv[3],
      t_stat        = rd$coef[1] / rd$se[3],
      bw            = rd$bws[1],
      n_eff         = rd$N_h[1] + rd$N_h[2],
      is_true       = (pc == 0)
    )
    message("  Cutoff R_tilde=", pc, " (c=", pc_cutoff,
            "): coef = ", round(rd$coef[1], 1),
            ", t = ", round(rd$coef[1] / rd$se[3], 2),
            ", p = ", round(rd$pv[3], 3))
  }, error = function(e) {
    message("  Cutoff R_tilde=", pc, ": FAILED -- ", e$message)
  })
}

placebo_full_df <- bind_rows(placebo_full_results)
saveRDS(placebo_full_df, file.path(CLEANED_DIR, "revisions_placebo_full.rds"))

# Check whether placebo failures at R_tilde=2,3 are concentrated
message("\n--- Investigating placebo failures at R_tilde=2,3 ---")
for (pc_check in c(2, 3)) {
  pc_c <- pc_check + 0.5
  # By tournament tier
  for (tier in c("G", "M", "A")) {
    sub <- rdd_cov |> filter(tourney_level == tier)
    y_sub <- sub[[outcome_placebo]]
    ok_sub <- !is.na(y_sub)
    if (sum(ok_sub) < 100) next

    tryCatch({
      rd_sub <- rdrobust(
        y = y_sub[ok_sub], x = sub$R_tilde[ok_sub], c = pc_c,
        cluster = sub$player_id[ok_sub],
        kernel = "triangular", bwselect = "mserd"
      )
      message("  Placebo R_tilde=", pc_check, " | tier=", tier,
              ": coef = ", round(rd_sub$coef[1], 1),
              ", p = ", round(rd_sub$pv[3], 3))
    }, error = function(e) {
      message("  Placebo R_tilde=", pc_check, " | tier=", tier, ": FAILED")
    })
  }

  # By decade
  for (decade in c("2000s", "2010s", "2020s")) {
    yr_range <- switch(decade,
      "2000s" = 2000:2009,
      "2010s" = 2010:2019,
      "2020s" = 2020:2024
    )
    sub <- rdd_cov |> filter(year %in% yr_range)
    y_sub <- sub[[outcome_placebo]]
    ok_sub <- !is.na(y_sub)
    if (sum(ok_sub) < 100) next

    tryCatch({
      rd_sub <- rdrobust(
        y = y_sub[ok_sub], x = sub$R_tilde[ok_sub], c = pc_c,
        cluster = sub$player_id[ok_sub],
        kernel = "triangular", bwselect = "mserd"
      )
      message("  Placebo R_tilde=", pc_check, " | ", decade,
              ": coef = ", round(rd_sub$coef[1], 1),
              ", p = ", round(rd_sub$pv[3], 3))
    }, error = function(e) {
      message("  Placebo R_tilde=", pc_check, " | ", decade, ": FAILED")
    })
  }
}

# Plot placebo distribution
if (nrow(placebo_full_df) > 0) {
  p_placebo <- ggplot(placebo_full_df, aes(x = cutoff_shift, y = t_stat)) +
    geom_hline(yintercept = c(-1.96, 1.96), linetype = "dashed", color = "gray60") +
    geom_hline(yintercept = 0, color = "gray80") +
    geom_col(aes(fill = is_true), width = 0.6) +
    scale_fill_manual(values = c("FALSE" = "grey70", "TRUE" = "#2166AC"),
                      labels = c("FALSE" = "Placebo", "TRUE" = "True Cutoff"),
                      guide = "none") +
    scale_x_continuous(breaks = placebo_cutoff_range) +
    labs(x = expression(tilde(R) ~ "Cutoff Location"),
         y = "t-statistic (Sharp Reduced-Form)",
         title = NULL) +
    annotate("text", x = 0, y = max(abs(placebo_full_df$t_stat)) * 1.1,
             label = "True Cutoff", color = "#2166AC", fontface = "bold",
             family = "serif", size = 3.5) +
    theme_paper()

  ggsave(file.path(FIGURES_DIR, "fig_placebo_distribution.pdf"),
         p_placebo, width = 7, height = 5)
  message("  Saved: Figures/fig_placebo_distribution.pdf")
}


# ==============================================================================
# TASK 4: TABLE 5 FIX (CONSISTENT BIAS-CORRECTED ROBUST SEs)
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 4: TABLE 5 FIX (CONSISTENT SEs)")
message(strrep("=", 70))

# Re-run all robustness specifications ensuring we use se[3]/pv[3]/ci[3,]
# (bias-corrected robust) consistently

outcome_focus <- "rank_change_26w"
y_t5 <- rdd_cov[[outcome_focus]]
ok_t5 <- !is.na(y_t5)

rob_rows_fixed <- list()

# Row 1: Baseline with covariates
tryCatch({
  base_rd <- rdrobust(
    y = y_t5[ok_t5], x = rdd_cov$R_tilde[ok_t5], c = CUTOFF,
    fuzzy = rdd_cov$got_ll[ok_t5], covs = covs_mat[ok_t5, ],
    cluster = rdd_cov$player_id[ok_t5],
    kernel = "triangular", bwselect = "mserd"
  )
  bw_opt <- base_rd$bws[1]
  rob_rows_fixed[["Baseline"]] <- tibble(
    Specification = "Baseline (p=1, MSE-opt BW, player cluster)",
    LATE = sprintf("%.1f", base_rd$coef[1]),
    SE = sprintf("(%.1f)", base_rd$se[3]),
    pv = sprintf("%.3f", base_rd$pv[3]),
    BW = sprintf("%.1f", base_rd$bws[1]),
    N_eff = as.character(round(base_rd$N_h[1] + base_rd$N_h[2]))
  )
  message("  Baseline: coef = ", round(base_rd$coef[1], 1),
          ", SE(bc) = ", round(base_rd$se[3], 1),
          ", p(bc) = ", round(base_rd$pv[3], 3))
}, error = function(e) {
  message("  Baseline FAILED: ", e$message)
  bw_opt <<- 3.0  # fallback
})

# Row 2: Local quadratic
tryCatch({
  rd_p2 <- rdrobust(
    y = y_t5[ok_t5], x = rdd_cov$R_tilde[ok_t5], c = CUTOFF,
    fuzzy = rdd_cov$got_ll[ok_t5], covs = covs_mat[ok_t5, ],
    cluster = rdd_cov$player_id[ok_t5],
    p = 2, kernel = "triangular", bwselect = "mserd"
  )
  rob_rows_fixed[["Quadratic"]] <- tibble(
    Specification = "Local quadratic (p=2)",
    LATE = sprintf("%.1f", rd_p2$coef[1]),
    SE = sprintf("(%.1f)", rd_p2$se[3]),
    pv = sprintf("%.3f", rd_p2$pv[3]),
    BW = sprintf("%.1f", rd_p2$bws[1]),
    N_eff = as.character(round(rd_p2$N_h[1] + rd_p2$N_h[2]))
  )
  message("  Quadratic: coef = ", round(rd_p2$coef[1], 1), ", p = ", round(rd_p2$pv[3], 3))
}, error = function(e) message("  Quadratic FAILED: ", e$message))

# Rows 3-7: Bandwidth sensitivity -- ALL using se[3] (bias-corrected robust)
bw_multipliers <- c(0.5, 0.75, 1.0, 1.5, 2.0)
for (mult in bw_multipliers) {
  bw_use <- bw_opt * mult
  tryCatch({
    rd_bw <- rdrobust(
      y = y_t5[ok_t5], x = rdd_cov$R_tilde[ok_t5], c = CUTOFF,
      fuzzy = rdd_cov$got_ll[ok_t5], covs = covs_mat[ok_t5, ],
      cluster = rdd_cov$player_id[ok_t5],
      h = bw_use, kernel = "triangular"
    )
    rob_rows_fixed[[paste0("BW_", mult)]] <- tibble(
      Specification = paste0("Bandwidth: ", mult * 100, "\\% of optimal"),
      LATE = sprintf("%.1f", rd_bw$coef[1]),
      SE = sprintf("(%.1f)", rd_bw$se[3]),
      pv = sprintf("%.3f", rd_bw$pv[3]),
      BW = sprintf("%.1f", bw_use),
      N_eff = as.character(round(rd_bw$N_h[1] + rd_bw$N_h[2]))
    )
    message("  BW ", mult * 100, "%: coef = ", round(rd_bw$coef[1], 1),
            ", SE(bc) = ", round(rd_bw$se[3], 1),
            ", p(bc) = ", round(rd_bw$pv[3], 3))
  }, error = function(e) message("  BW ", mult * 100, "% FAILED: ", e$message))
}

# Donut hole
tryCatch({
  donut <- rdd_cov |> filter(R_tilde != 0)
  donut_covs <- donut |>
    select(age_imp, elo_t0_imp, is_grand_slam, is_masters, is_clay, is_grass) |>
    as.matrix()
  y_dn <- donut[[outcome_focus]]
  ok_dn <- !is.na(y_dn)

  rd_dn <- rdrobust(
    y = y_dn[ok_dn], x = donut$R_tilde[ok_dn], c = CUTOFF,
    fuzzy = donut$got_ll[ok_dn], covs = donut_covs[ok_dn, ],
    cluster = donut$player_id[ok_dn],
    kernel = "triangular", bwselect = "mserd"
  )
  rob_rows_fixed[["Donut"]] <- tibble(
    Specification = "Donut hole (excl. $\\tilde{R} = 0$)",
    LATE = sprintf("%.1f", rd_dn$coef[1]),
    SE = sprintf("(%.1f)", rd_dn$se[3]),
    pv = sprintf("%.3f", rd_dn$pv[3]),
    BW = sprintf("%.1f", rd_dn$bws[1]),
    N_eff = as.character(round(rd_dn$N_h[1] + rd_dn$N_h[2]))
  )
  message("  Donut: coef = ", round(rd_dn$coef[1], 1), ", p = ", round(rd_dn$pv[3], 3))
}, error = function(e) message("  Donut FAILED: ", e$message))

# Tournament-year clustering
tryCatch({
  rdd_cov_ty <- rdd_cov |> mutate(tourney_year = paste0(tourney_id, "_", year))
  rd_ty <- rdrobust(
    y = y_t5[ok_t5], x = rdd_cov$R_tilde[ok_t5], c = CUTOFF,
    fuzzy = rdd_cov$got_ll[ok_t5], covs = covs_mat[ok_t5, ],
    cluster = rdd_cov_ty$tourney_year[ok_t5],
    kernel = "triangular", bwselect = "mserd"
  )
  rob_rows_fixed[["TY_cluster"]] <- tibble(
    Specification = "Tournament-year clustering",
    LATE = sprintf("%.1f", rd_ty$coef[1]),
    SE = sprintf("(%.1f)", rd_ty$se[3]),
    pv = sprintf("%.3f", rd_ty$pv[3]),
    BW = sprintf("%.1f", rd_ty$bws[1]),
    N_eff = as.character(round(rd_ty$N_h[1] + rd_ty$N_h[2]))
  )
  message("  TY cluster: coef = ", round(rd_ty$coef[1], 1), ", SE = ", round(rd_ty$se[3], 1))
}, error = function(e) message("  TY cluster FAILED: ", e$message))

# Single LL-slot tournaments
tryCatch({
  single_slot <- rdd_cov |> filter(n_ll_slots == 1)
  ss_covs <- single_slot |>
    select(age_imp, elo_t0_imp, is_grand_slam, is_masters, is_clay, is_grass) |>
    as.matrix()
  y_ss <- single_slot[[outcome_focus]]
  ok_ss <- !is.na(y_ss)

  rd_ss <- rdrobust(
    y = y_ss[ok_ss], x = single_slot$R_tilde[ok_ss], c = CUTOFF,
    fuzzy = single_slot$got_ll[ok_ss], covs = ss_covs[ok_ss, ],
    cluster = single_slot$player_id[ok_ss],
    kernel = "triangular", bwselect = "mserd"
  )
  rob_rows_fixed[["Single_slot"]] <- tibble(
    Specification = "Single LL-slot tournaments only",
    LATE = sprintf("%.1f", rd_ss$coef[1]),
    SE = sprintf("(%.1f)", rd_ss$se[3]),
    pv = sprintf("%.3f", rd_ss$pv[3]),
    BW = sprintf("%.1f", rd_ss$bws[1]),
    N_eff = as.character(round(rd_ss$N_h[1] + rd_ss$N_h[2]))
  )
  message("  Single-slot: coef = ", round(rd_ss$coef[1], 1), ", p = ", round(rd_ss$pv[3], 3))
}, error = function(e) message("  Single-slot FAILED: ", e$message))

# Pre-treatment placebo
tryCatch({
  pretreat_result <- readRDS(file.path(CLEANED_DIR, "robustness_pretreatment.rds"))
  if (!is.null(pretreat_result) && nrow(pretreat_result) > 0) {
    rob_rows_fixed[["Pretreatment"]] <- tibble(
      Specification = "Pre-treatment placebo ($\\Delta R_{-12w,0}$)",
      LATE = sprintf("%.1f", pretreat_result$coef),
      SE = sprintf("(%.1f)", pretreat_result$se_robust),
      pv = sprintf("%.3f", pretreat_result$pv_robust),
      BW = sprintf("%.1f", pretreat_result$bw),
      N_eff = as.character(round(pretreat_result$n_eff))
    )
  }
}, error = function(e) message("  Pre-treatment load FAILED: ", e$message))

# Write Table 5
rob_table_fixed <- bind_rows(rob_rows_fixed)

if (nrow(rob_table_fixed) > 0) {
  # Load density test
  density_result <- tryCatch(readRDS(file.path(CLEANED_DIR, "robustness_density_test.rds")),
                              error = function(e) NULL)

  rob_lines <- c(
    "\\begin{tabular}{lccccc}",
    "\\toprule",
    "Specification & LATE & Robust SE & $p$-value & Bandwidth & Eff. $N$ \\\\",
    "\\midrule"
  )
  for (i in seq_len(nrow(rob_table_fixed))) {
    r <- rob_table_fixed[i, ]
    rob_lines <- c(rob_lines, paste0(
      r$Specification, " & ", r$LATE, " & ", r$SE, " & ",
      r$pv, " & ", r$BW, " & ", r$N_eff, " \\\\"
    ))
  }

  if (!is.null(density_result)) {
    rob_lines <- c(rob_lines, "\\midrule",
      paste0("\\multicolumn{6}{l}{\\footnotesize Density test (McCrary): $p$ = ",
             round(density_result$p_value, 3), "} \\\\"))
  }

  # Add placebo cutoffs from full distribution
  if (nrow(placebo_full_df) > 0) {
    placebo_non_true <- placebo_full_df |> filter(!is_true)
    rob_lines <- c(rob_lines,
      "\\midrule",
      "\\multicolumn{6}{l}{\\textit{Placebo cutoffs --- sharp reduced-form (rank\\_change\\_26w)}} \\\\"
    )
    for (i in seq_len(nrow(placebo_non_true))) {
      r <- placebo_non_true[i, ]
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
    "\\multicolumn{6}{l}{\\footnotesize Outcome: ranking change at 26 weeks. All SEs are bias-corrected robust.} \\\\",
    "\\multicolumn{6}{l}{\\footnotesize SEs clustered at player level unless noted.} \\\\",
    "\\bottomrule", "\\end{tabular}")

  writeLines(rob_lines, file.path(TABLES_DIR, "table5_robustness.tex"))
  message("  Saved: Tables/table5_robustness.tex (FIXED: consistent bias-corrected robust SEs)")
}


# ==============================================================================
# TASK 5: EXPANDED BALANCE TESTS
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 5: EXPANDED BALANCE TESTS")
message(strrep("=", 70))

# --- 5a. RDD balance: single BW=3.0, expanded covariates --------------------
message("\n--- 5a. RDD balance at fixed BW=3.0 ---")
FIXED_BW <- 3.0

balance_vars_expanded <- c("player_rank", "player_age", "elo_t0",
                           "player_ht", "player_hand", "player_ioc")

# Prepare hand and ioc as numeric
rdd_bal <- rdd_cov |>
  mutate(
    hand_right = as.integer(player_hand == "R"),
    # Create a few common nationality indicators
    ioc_usa = as.integer(player_ioc == "USA"),
    ioc_eur = as.integer(player_ioc %in% c("FRA", "GER", "ESP", "ITA", "GBR",
                                             "AUT", "SUI", "BEL", "NED", "CZE",
                                             "SRB", "CRO", "RUS", "SVK", "POL",
                                             "ROU", "GRE", "POR", "SWE", "NOR",
                                             "DEN", "FIN", "HUN", "UKR", "BUL"))
  )

balance_vars_final <- c("player_rank", "player_age", "elo_t0",
                         "player_ht", "hand_right", "ioc_usa", "ioc_eur")
balance_var_labels <- c("ATP Ranking", "Age", "Elo Rating",
                         "Height (cm)", "Right-Handed", "US Nationality", "European")

rdd_balance_results <- list()
for (j in seq_along(balance_vars_final)) {
  var <- balance_vars_final[j]
  y <- rdd_bal[[var]]
  ok <- !is.na(y)
  if (sum(ok) < 200) {
    message("  ", var, ": too few obs (", sum(ok), ")")
    next
  }

  tryCatch({
    bal <- rdrobust(
      y = y[ok], x = rdd_bal$R_tilde[ok], c = CUTOFF,
      h = FIXED_BW,
      cluster = rdd_bal$player_id[ok],
      kernel = "triangular"
    )
    rdd_balance_results[[var]] <- tibble(
      variable  = var,
      label     = balance_var_labels[j],
      coef      = bal$coef[1],
      se_robust = bal$se[3],
      pv_robust = bal$pv[3],
      bw        = FIXED_BW,
      n_eff     = bal$N_h[1] + bal$N_h[2]
    )
    message("  ", var, ": coef = ", round(bal$coef[1], 2),
            ", p = ", round(bal$pv[3], 3))
  }, error = function(e) message("  ", var, ": FAILED -- ", e$message))
}

rdd_balance_df <- bind_rows(rdd_balance_results)

# --- 5b. Lottery sample individual balance -----------------------------------
message("\n--- 5b. Lottery sample individual balance ---")

lottery_balance_vars <- c("player_rank", "player_age", "player_ht")
lottery_balance_labels <- c("Ranking", "Age", "Height (cm)")
lottery_balance_results <- list()

for (j in seq_along(lottery_balance_vars)) {
  var <- lottery_balance_vars[j]
  y <- pooled_gs[[var]]
  ok <- !is.na(y)
  if (sum(ok) < 20) next

  tt <- tryCatch(t.test(y[ok] ~ pooled_gs$got_ll[ok]), error = function(e) NULL)
  if (!is.null(tt)) {
    lottery_balance_results[[var]] <- tibble(
      variable  = var,
      label     = lottery_balance_labels[j],
      mean_ll   = mean(y[ok][pooled_gs$got_ll[ok] == 1]),
      mean_ctrl = mean(y[ok][pooled_gs$got_ll[ok] == 0]),
      diff      = tt$estimate[1] - tt$estimate[2],
      pv        = tt$p.value,
      n_ll      = sum(pooled_gs$got_ll[ok] == 1),
      n_ctrl    = sum(pooled_gs$got_ll[ok] == 0)
    )
    message("  Lottery ", var, ": diff = ", round(tt$estimate[1] - tt$estimate[2], 2),
            ", p = ", round(tt$p.value, 3))
  }
}

lottery_balance_df <- bind_rows(lottery_balance_results)

# Also check handedness balance in lottery
if ("player_hand" %in% names(pooled_gs)) {
  hand_tab <- table(pooled_gs$player_hand, pooled_gs$got_ll)
  if (nrow(hand_tab) > 1 && ncol(hand_tab) == 2) {
    hand_chi <- tryCatch(chisq.test(hand_tab), error = function(e) NULL)
    if (!is.null(hand_chi)) {
      message("  Lottery handedness chi-sq: p = ", round(hand_chi$p.value, 3))
      lottery_balance_df <- bind_rows(lottery_balance_df, tibble(
        variable = "player_hand", label = "Handedness (chi-sq)",
        mean_ll = NA_real_, mean_ctrl = NA_real_,
        diff = NA_real_, pv = hand_chi$p.value,
        n_ll = sum(pooled_gs$got_ll == 1), n_ctrl = sum(pooled_gs$got_ll == 0)
      ))
    }
  }
}

# --- 5c. Write expanded Table 1 ----------------------------------------------
message("\n--- 5c. Writing expanded Table 1 ---")

bal_lines <- c(
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  " & \\multicolumn{5}{c}{\\textit{Panel A: RDD Sample (BW = 3.0)}} \\\\",
  "\\cmidrule(lr){2-6}",
  "Variable & Coefficient & Robust SE & $p$-value & Bandwidth & Eff. $N$ \\\\",
  "\\midrule"
)

if (nrow(rdd_balance_df) > 0) {
  for (i in seq_len(nrow(rdd_balance_df))) {
    r <- rdd_balance_df[i, ]
    bal_lines <- c(bal_lines, paste0(
      r$label, " & ",
      sprintf("%.2f", r$coef), " & ",
      sprintf("(%.2f)", r$se_robust), " & ",
      sprintf("%.3f", r$pv_robust), " & ",
      sprintf("%.1f", r$bw), " & ",
      round(r$n_eff), " \\\\"
    ))
  }
}

bal_lines <- c(bal_lines,
  "\\midrule",
  " & \\multicolumn{5}{c}{\\textit{Panel B: Grand Slam Lottery (Pooled ATP+WTA, Top-4 Pool)}} \\\\",
  "\\cmidrule(lr){2-6}",
  "Variable & LL Mean & Control Mean & Difference & $p$-value & $N$ \\\\",
  "\\midrule"
)

if (nrow(lottery_balance_df) > 0) {
  for (i in seq_len(nrow(lottery_balance_df))) {
    r <- lottery_balance_df[i, ]
    if (!is.na(r$diff)) {
      bal_lines <- c(bal_lines, paste0(
        r$label, " & ",
        sprintf("%.1f", r$mean_ll), " & ",
        sprintf("%.1f", r$mean_ctrl), " & ",
        sprintf("%.2f", r$diff), " & ",
        sprintf("%.3f", r$pv), " & ",
        r$n_ll + r$n_ctrl, " \\\\"
      ))
    } else {
      # Chi-squared test row
      bal_lines <- c(bal_lines, paste0(
        r$label, " & -- & -- & -- & ",
        sprintf("%.3f", r$pv), " & ",
        r$n_ll + r$n_ctrl, " \\\\"
      ))
    }
  }
}

bal_lines <- c(bal_lines,
  "\\midrule",
  "\\multicolumn{6}{l}{\\footnotesize Panel A: RDD balance tests at fixed bandwidth 3.0. SEs clustered at player level.} \\\\",
  "\\multicolumn{6}{l}{\\footnotesize Panel B: t-tests for continuous variables; chi-squared for categorical.} \\\\",
  "\\bottomrule", "\\end{tabular}")

writeLines(bal_lines, file.path(TABLES_DIR, "table1_balance.tex"))
message("  Saved: Tables/table1_balance.tex (expanded)")

saveRDS(rdd_balance_df, file.path(CLEANED_DIR, "revisions_rdd_balance.rds"))
saveRDS(lottery_balance_df, file.path(CLEANED_DIR, "revisions_lottery_balance.rds"))


# ==============================================================================
# TASK 6: POWER ANALYSIS
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 6: POWER ANALYSIS")
message(strrep("=", 70))

power_lines <- c(
  "# Power Analysis",
  paste0("Generated: ", Sys.time()),
  ""
)

# --- 6a. Lottery power analysis (pooled ATP+WTA) -----------------------------
message("\n--- 6a. Lottery sample power ---")

# For difference in means: MDE = (z_alpha/2 + z_beta) * SE_pooled * sqrt(1/n1 + 1/n2)
y_power <- pooled_gs$rank_change_26w
ok_power <- !is.na(y_power)
n1_power <- sum(pooled_gs$got_ll[ok_power] == 1)
n0_power <- sum(pooled_gs$got_ll[ok_power] == 0)
sd_pooled <- sd(y_power[ok_power], na.rm = TRUE)

z_alpha <- qnorm(0.975)  # two-sided alpha = 0.05
z_beta  <- qnorm(0.80)   # power = 0.80

mde_lottery <- (z_alpha + z_beta) * sd_pooled * sqrt(1/n1_power + 1/n0_power)

power_lines <- c(power_lines,
  "## Lottery Sample (Pooled ATP+WTA, rank_change_26w)",
  paste0("- N treated: ", n1_power),
  paste0("- N control: ", n0_power),
  paste0("- SD of outcome: ", round(sd_pooled, 1)),
  paste0("- MDE (alpha=0.05, power=0.80): ", round(mde_lottery, 1), " ranking positions"),
  paste0("- MDE as fraction of SD: ", round(mde_lottery / sd_pooled, 2)),
  ""
)
message("  Lottery MDE (rank_change_26w): ", round(mde_lottery, 1),
        " ranking positions (", round(mde_lottery / sd_pooled, 2), " SD)")

# --- 6b. RDD power analysis -------------------------------------------------
message("\n--- 6b. RDD power analysis ---")

# Try rdpower package
has_rdpower <- requireNamespace("rdpower", quietly = TRUE)

if (has_rdpower) {
  library(rdpower)
  tryCatch({
    y_rdd_power <- rdd_cov[[outcome_focus]]
    ok_rdd <- !is.na(y_rdd_power)

    rp <- rdpower(
      y = y_rdd_power[ok_rdd],
      x = rdd_cov$R_tilde[ok_rdd],
      c = CUTOFF,
      cluster = rdd_cov$player_id[ok_rdd],
      tau = 0  # test for any effect
    )

    power_lines <- c(power_lines,
      "## RDD Power (rdpower package)",
      paste0("- Power at tau=0: ", round(rp$power, 3)),
      paste0("- MDE: ", round(rp$sampsi.h.l + rp$sampsi.h.r, 1)),
      ""
    )
    message("  RDD power (rdpower): ", round(rp$power, 3))
  }, error = function(e) {
    message("  rdpower failed: ", e$message)
    has_rdpower <<- FALSE
  })
}

if (!has_rdpower) {
  # Manual RDD power calculation
  # Use the effective N and SE from baseline specification
  base_result <- tryCatch(
    readRDS(file.path(CLEANED_DIR, "main_fuzzy_rdd_results.rds")) |>
      filter(covariates == "Yes", outcome == outcome_focus),
    error = function(e) NULL
  )

  if (!is.null(base_result) && nrow(base_result) > 0) {
    se_rdd <- base_result$se_robust[1]
    n_eff_rdd <- base_result$n_eff[1]
    mde_rdd <- (z_alpha + z_beta) * se_rdd

    power_lines <- c(power_lines,
      "## RDD Power (Manual Calculation)",
      paste0("- Effective N: ", round(n_eff_rdd)),
      paste0("- SE (bias-corrected robust): ", round(se_rdd, 1)),
      paste0("- MDE (alpha=0.05, power=0.80): ", round(mde_rdd, 1), " ranking positions"),
      paste0("- Note: Uses SE from main fuzzy RDD specification as basis for MDE."),
      ""
    )
    message("  RDD MDE (manual): ", round(mde_rdd, 1), " ranking positions")
  } else {
    power_lines <- c(power_lines,
      "## RDD Power",
      "- Could not compute: main results not found",
      ""
    )
  }
}

writeLines(power_lines, file.path(OUTPUT_DIR, "power_analysis.md"))
message("  Saved: Output/power_analysis.md")


# ==============================================================================
# TASK 7: UNIQUE CLUSTER COUNTS
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 7: UNIQUE CLUSTER COUNTS")
message(strrep("=", 70))

# RDD full sample
n_players_rdd_full <- n_distinct(rdd_cov$player_id)
message("  RDD full sample: ", n_players_rdd_full, " unique players")

# RDD within BW=3.0
rdd_bw3 <- rdd_cov |> filter(abs(R_tilde - CUTOFF) <= FIXED_BW)
n_players_rdd_bw3 <- n_distinct(rdd_bw3$player_id)
message("  RDD within BW=3.0: ", n_players_rdd_bw3, " unique players (",
        nrow(rdd_bw3), " obs)")

# ATP Grand Slam lottery
atp_gs_ll <- rdd_cov |> filter(tourney_level == "G", rank_among_losers <= 4)
n_players_atp_gs <- n_distinct(atp_gs_ll$player_id)
message("  ATP GS lottery (top-4 pool): ", n_players_atp_gs, " unique players (",
        nrow(atp_gs_ll), " obs)")

# Pooled Grand Slam lottery
n_players_pooled <- n_distinct(pooled_gs$player_id)
message("  Pooled GS lottery (top-4 pool): ", n_players_pooled, " unique players (",
        nrow(pooled_gs), " obs)")

# WTA Grand Slam lottery
n_players_wta_gs <- n_distinct(wta_gs_pool$player_id)
message("  WTA GS lottery (top-4 pool): ", n_players_wta_gs, " unique players (",
        nrow(wta_gs_pool), " obs)")

cluster_lines <- c(
  "# Unique Cluster Counts",
  paste0("Generated: ", Sys.time()),
  "",
  "## RDD Sample",
  paste0("- Full sample: ", n_players_rdd_full, " unique players (",
         nrow(rdd_cov), " obs)"),
  paste0("- Within BW=3.0 of cutoff: ", n_players_rdd_bw3, " unique players (",
         nrow(rdd_bw3), " obs)"),
  "",
  "## Grand Slam Lottery Sample (Top-4 Pool)",
  paste0("- ATP only: ", n_players_atp_gs, " unique players (",
         nrow(atp_gs_ll), " obs)"),
  paste0("- WTA only: ", n_players_wta_gs, " unique players (",
         nrow(wta_gs_pool), " obs)"),
  paste0("- Pooled ATP+WTA: ", n_players_pooled, " unique players (",
         nrow(pooled_gs), " obs)"),
  ""
)

writeLines(cluster_lines, file.path(OUTPUT_DIR, "cluster_counts.md"))
message("  Saved: Output/cluster_counts.md")


# ==============================================================================
# FINAL SUMMARY
# ==============================================================================
message("\n", strrep("=", 70))
message("09_revisions.R COMPLETE")
message(strrep("=", 70))

message("\nOutputs produced:")
message("  Data/cleaned/pooled_gs_lottery_results.rds")
message("  Data/cleaned/pooled_gs_lottery_sample.rds")
message("  Data/cleaned/revisions_placebo_full.rds")
message("  Data/cleaned/revisions_rdd_balance.rds")
message("  Data/cleaned/revisions_lottery_balance.rds")
message("  Figures/fig_lottery_event_study.pdf")
message("  Figures/fig_placebo_distribution.pdf")
message("  Tables/table5_robustness.tex (FIXED)")
message("  Tables/table1_balance.tex (EXPANDED)")
message("  Output/power_analysis.md")
message("  Output/cluster_counts.md")
