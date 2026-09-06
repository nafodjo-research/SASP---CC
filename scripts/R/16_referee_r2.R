# ==============================================================================
# 16_referee_r2.R
# Skeleton-aligned analyses: immediate effects, event FE, multiple-treatment
# adjustment, LL history, heterogeneity, non-GS replication, missing tables.
#
# Inputs:
#   Data/cleaned/estimation_sample_v2.rds
#   Data/cleaned/critical_fixes_results.rds
#   Data/raw/atp_main_matches.rds, wta_main_matches.rds
#   Data/raw/atp_qual_chall_matches.rds, wta_qual_itf_matches.rds
#   Data/raw/atp_rankings.rds, wta_rankings.rds
#   Data/raw/atp_players.rds, wta_players.rds
#
# Outputs: (see each TASK section)
#   Tables/table_immediate_effects.tex
#   Tables/table_dynamic_event_fe.tex
#   Tables/table_heterogeneity_gs.tex
#   Tables/table_immediate_effects_nongs.tex
#   Tables/table_dynamic_nongs.tex
#   Tables/table_heterogeneity_nongs.tex
#   Tables/table_summary_stats_nongs.tex
#   Data/cleaned/immediate_effects.rds
#   Data/cleaned/dynamic_event_fe_results.rds
#   Data/cleaned/multiple_treatment_adj_results.rds
#   Data/cleaned/ll_history_variables.rds
#   Data/cleaned/heterogeneity_results.rds
#   Output/skeleton_analysis_summary.md
#
# Dependencies: dplyr, tidyr, readr, stringr, fixest, here
# Project: Lucky Losers and Career Trajectories
# ==============================================================================

set.seed(20260321)

# --- Packages -----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(fixest)
library(here)

# --- Paths --------------------------------------------------------------------
RAW_DIR     <- here("Data", "raw")
CLEANED_DIR <- here("Data", "cleaned")
TABLES_DIR  <- here("Tables")
FIGURES_DIR <- here("Figures")
OUTPUT_DIR  <- here("Output")
for (d in c(CLEANED_DIR, TABLES_DIR, FIGURES_DIR, OUTPUT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# --- Helper: significance stars -----------------------------------------------
add_stars <- function(pv) {
  ifelse(is.na(pv), "",
    ifelse(pv < 0.01, "$^{***}$",
      ifelse(pv < 0.05, "$^{**}$",
        ifelse(pv < 0.1, "$^{*}$", ""))))
}

# --- Custom theme -------------------------------------------------------------
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

# --- Bernoulli convolution ---------------------------------------------------
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
      if (j >= 1) new_prob[j + 1] <- new_prob[j + 1] + prob[j] * pk
    }
    prob <- new_prob
  }
  sum(prob[(threshold + 1):(n + 1)])
}

# ==============================================================================
# LOAD DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("LOADING DATA")
message(strrep("=", 70))

est_v2 <- readRDS(file.path(CLEANED_DIR, "estimation_sample_v2.rds"))

gs_lottery   <- est_v2$gs_lottery
est_iv       <- est_v2$est_iv
losers_gs    <- est_v2$losers_gs
losers_nongs <- est_v2$losers_nongs
all_qualifiers <- est_v2$all_qualifiers

atp_main <- read_rds(file.path(RAW_DIR, "atp_main_matches.rds"))
wta_main <- read_rds(file.path(RAW_DIR, "wta_main_matches.rds"))
atp_qual <- read_rds(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
wta_qual <- read_rds(file.path(RAW_DIR, "wta_qual_itf_matches.rds"))
atp_rankings_raw <- read_rds(file.path(RAW_DIR, "atp_rankings.rds"))
wta_rankings_raw <- read_rds(file.path(RAW_DIR, "wta_rankings.rds"))

message("  gs_lottery: ", nrow(gs_lottery), " obs")
message("  est_iv: ", nrow(est_iv), " obs")
message("  losers_gs: ", nrow(losers_gs), " obs")
message("  losers_nongs: ", nrow(losers_nongs), " obs")
message("  all_qualifiers: ", nrow(all_qualifiers), " obs")

# Ensure key variables exist
gs_lottery <- gs_lottery |>
  mutate(
    pre_rank_pts = coalesce(points_t0, player_rank_points, 0),
    pre_rank_pts_sq = pre_rank_pts^2,
    slam_year = paste0(coalesce(slam_name, tourney_name), "_", year)
  )

# Build main draw match data for immediate effects lookup
all_main <- bind_rows(
  atp_main |> mutate(tour = "ATP"),
  wta_main |> mutate(tour = "WTA")
) |>
  mutate(
    year = as.integer(str_sub(tourney_date, 1, 4)),
    match_date = as.Date(as.character(tourney_date), format = "%Y%m%d")
  ) |>
  filter(year >= 2000)

# Summary accumulator
summary_lines <- c(
  "# Skeleton Analysis Summary",
  paste0("Generated: ", Sys.time()),
  ""
)

# ==============================================================================
# TASK 1: IMMEDIATE EFFECTS (EVENT-LEVEL)
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 1: IMMEDIATE EFFECTS (EVENT-LEVEL)")
message(strrep("=", 70))

# For each player-event in gs_lottery, look up main draw results at that event
# Treated: their main draw match results
# Controls: 0 (they did not enter main draw)

# Build LL main draw match details by tourney_id and player_id
ll_match_details <- bind_rows(
  all_main |>
    filter(winner_entry == "LL") |>
    transmute(tourney_id, player_id = winner_id, won_match = 1L,
              round, opp_rank = loser_rank,
              w_rank_pts = winner_rank_points, l_rank_pts = loser_rank_points),
  all_main |>
    filter(loser_entry == "LL") |>
    transmute(tourney_id, player_id = loser_id, won_match = 0L,
              round, opp_rank = winner_rank,
              w_rank_pts = winner_rank_points, l_rank_pts = loser_rank_points)
)

# Aggregate to player-event level
ll_event_summary <- ll_match_details |>
  group_by(tourney_id, player_id) |>
  summarise(
    md_matches_played = n(),
    md_matches_won    = sum(won_match),
    md_any_win        = as.integer(any(won_match == 1L)),
    .groups = "drop"
  )

# Ranking points earned at the event -- use points_t0 proxy
# We approximate ranking points at event from the match data
# For GS: R1 loss ~ 10 pts, R1 win ~ 45 pts, R2 win ~ 90 pts, etc.
# Better: difference in ranking points around the event window
# Use points shortly after event minus points at event (points_t4 - points_t0)
# But this captures other tournaments too. For now, use match count as proxy.
# Actually: use the ranking points mapping for Grand Slams
gs_round_points <- tibble(
  round = c("R128", "R64", "R32", "R16", "QF", "SF", "F"),
  gs_pts = c(10, 45, 90, 180, 360, 720, 1200)
)

# For each LL at a GS, compute ranking points earned based on deepest round
ll_gs_points <- ll_match_details |>
  filter(str_detect(tourney_id, "^\\d{4}-")) |>
  left_join(gs_round_points, by = "round") |>
  group_by(tourney_id, player_id) |>
  summarise(
    deepest_round_pts = max(gs_pts, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(deepest_round_pts = ifelse(is.infinite(deepest_round_pts), 10, deepest_round_pts))

# Merge to gs_lottery
gs_lottery <- gs_lottery |>
  left_join(ll_event_summary, by = c("tourney_id", "player_id")) |>
  left_join(ll_gs_points, by = c("tourney_id", "player_id")) |>
  mutate(
    md_matches_played   = ifelse(got_ll == 1, replace_na(md_matches_played, 1L), 0L),
    md_matches_won      = ifelse(got_ll == 1, replace_na(md_matches_won, 0L), 0L),
    md_any_win          = ifelse(got_ll == 1, replace_na(md_any_win, 0L), 0L),
    ranking_pts_at_event = ifelse(got_ll == 1, replace_na(deepest_round_pts, 10), 0)
  )

message("  Immediate outcomes constructed:")
message("    Treated md_matches_played: mean = ", round(mean(gs_lottery$md_matches_played[gs_lottery$got_ll == 1]), 2))
message("    Treated md_any_win: mean = ", round(mean(gs_lottery$md_any_win[gs_lottery$got_ll == 1]), 2))
message("    Treated ranking_pts_at_event: mean = ", round(mean(gs_lottery$ranking_pts_at_event[gs_lottery$got_ll == 1]), 1))

# --- 1a. Event FE regressions: outcome ~ got_ll | slam_year ---
immediate_outcomes <- c("md_any_win", "md_matches_played", "ranking_pts_at_event")
immediate_labels <- c(
  "md_any_win" = "Main-draw match win",
  "md_matches_played" = "Main-draw matches played",
  "ranking_pts_at_event" = "Ranking points at event"
)

# Check slam_year FE counts
slam_year_counts <- gs_lottery |> count(slam_year) |> filter(n >= 2)
gs_lottery_fe <- gs_lottery |> filter(slam_year %in% slam_year_counts$slam_year)
message("  Observations with valid slam_year FE (>=2 obs): ", nrow(gs_lottery_fe))

run_immediate <- function(data, label_prefix) {
  results <- list()
  for (out in immediate_outcomes) {
    if (!out %in% names(data)) next
    y <- data[[out]]
    d <- data$got_ll
    ok <- !is.na(y) & !is.na(d)
    if (sum(ok & d == 1) < 2 || sum(ok & d == 0) < 2) next

    # t-test
    tt <- tryCatch(t.test(y[ok & d == 1], y[ok & d == 0]), error = function(e) NULL)

    # Event FE regression
    fe_fit <- tryCatch({
      feols(as.formula(paste0(out, " ~ got_ll | slam_year")),
            data = data[ok, ], vcov = "hetero")
    }, error = function(e) NULL)

    results[[out]] <- tibble(
      sample = label_prefix,
      outcome = out,
      mean_treated = mean(y[ok & d == 1]),
      mean_control = mean(y[ok & d == 0]),
      ttest_diff = mean(y[ok & d == 1]) - mean(y[ok & d == 0]),
      ttest_pv = if (!is.null(tt)) tt$p.value else NA_real_,
      fe_coef = if (!is.null(fe_fit)) coef(fe_fit)["got_ll"] else NA_real_,
      fe_se   = if (!is.null(fe_fit)) sqrt(vcov(fe_fit)["got_ll", "got_ll"]) else NA_real_,
      fe_pv   = if (!is.null(fe_fit)) {
        2 * pnorm(-abs(coef(fe_fit)["got_ll"] / sqrt(vcov(fe_fit)["got_ll", "got_ll"])))
      } else NA_real_,
      n_treated = sum(ok & d == 1),
      n_control = sum(ok & d == 0)
    )
  }
  bind_rows(results)
}

# Split by tour
gs_atp <- gs_lottery_fe |> filter(tour == "ATP")
gs_wta <- gs_lottery_fe |> filter(tour == "WTA")

imm_atp    <- run_immediate(gs_atp, "ATP")
imm_wta    <- run_immediate(gs_wta, "WTA")
imm_pooled <- run_immediate(gs_lottery_fe, "Pooled")

immediate_all <- bind_rows(imm_atp, imm_wta, imm_pooled)
message("\n  Immediate effects results:")
print(immediate_all |> select(sample, outcome, ttest_diff, ttest_pv, fe_coef, fe_pv), n = 20)

saveRDS(immediate_all, file.path(CLEANED_DIR, "immediate_effects.rds"))
message("  Saved: immediate_effects.rds")

# --- Build LaTeX table ---
imm_tex <- c(
  "\\begin{tabular}{l ccc ccc}",
  "\\toprule",
  " & \\multicolumn{3}{c}{$t$-test} & \\multicolumn{3}{c}{Event FE} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
  "Outcome & LL Mean & Ctrl Mean & $p$ & $\\hat{\\beta}$ & SE & $p$ \\\\",
  "\\midrule"
)

for (samp in c("ATP", "WTA", "Pooled")) {
  imm_tex <- c(imm_tex,
    paste0("\\multicolumn{7}{l}{\\textit{", samp, "}} \\\\"))
  sub <- immediate_all |> filter(sample == samp)
  for (out in immediate_outcomes) {
    r <- sub |> filter(outcome == out)
    if (nrow(r) == 0) next
    lab <- immediate_labels[out]
    imm_tex <- c(imm_tex,
      paste0("\\quad ", lab, " & ",
             sprintf("%.2f", r$mean_treated), " & ",
             sprintf("%.2f", r$mean_control), " & ",
             sprintf("%.3f", r$ttest_pv), " & ",
             sprintf("%.2f", r$fe_coef), add_stars(r$fe_pv), " & ",
             sprintf("%.2f", r$fe_se), " & ",
             sprintf("%.3f", r$fe_pv), " \\\\"))
  }
  if (nrow(sub) > 0) {
    imm_tex <- c(imm_tex,
      paste0("\\quad $N$ & \\multicolumn{3}{c}{",
             sub$n_treated[1], " / ", sub$n_control[1],
             "} & \\multicolumn{3}{c}{",
             sub$n_treated[1] + sub$n_control[1], "} \\\\"))
  }
  if (samp != "Pooled") imm_tex <- c(imm_tex, "\\addlinespace")
}

imm_tex <- c(imm_tex, "\\bottomrule", "\\end{tabular}")
writeLines(imm_tex, file.path(TABLES_DIR, "table_immediate_effects.tex"))
message("  Saved: table_immediate_effects.tex")

summary_lines <- c(summary_lines,
  "## TASK 1: Immediate Effects",
  paste0("- GS lottery sample for FE analysis: N = ", nrow(gs_lottery_fe)),
  paste0("- ATP: ", nrow(gs_atp), ", WTA: ", nrow(gs_wta)),
  paste0("- Pooled: md_any_win FE coef = ",
         sprintf("%.3f", imm_pooled$fe_coef[imm_pooled$outcome == "md_any_win"]),
         ", p = ", sprintf("%.3f", imm_pooled$fe_pv[imm_pooled$outcome == "md_any_win"])),
  "")


# ==============================================================================
# TASK 2: EVENT FE IN DYNAMIC SPECS
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 2: EVENT FIXED EFFECTS IN DYNAMIC SPECS")
message(strrep("=", 70))

# Dynamic outcomes at multiple horizons with event (slam x year) FE
dynamic_outcomes <- c("n_matches_250plus_26w", "n_matches_250plus_52w",
                      "n_main_draws_26w", "n_main_draws_52w",
                      "points_change_12w", "points_change_26w", "points_change_52w",
                      "rank_change_4w", "rank_change_8w", "rank_change_12w",
                      "rank_change_26w", "rank_change_52w")
if ("elo_change_12w" %in% names(gs_lottery)) {
  dynamic_outcomes <- c(dynamic_outcomes, "elo_change_12w", "elo_change_26w")
}

dynamic_labels <- c(
  "n_matches_250plus_26w" = "Matches 250+ (26w)",
  "n_matches_250plus_52w" = "Matches 250+ (52w)",
  "n_main_draws_26w" = "Main draws (26w)",
  "n_main_draws_52w" = "Main draws (52w)",
  "points_change_12w" = "Points $\\Delta$ (12w)",
  "points_change_26w" = "Points $\\Delta$ (26w)",
  "points_change_52w" = "Points $\\Delta$ (52w)",
  "rank_change_4w" = "Rank $\\Delta$ (4w)",
  "rank_change_8w" = "Rank $\\Delta$ (8w)",
  "rank_change_12w" = "Rank $\\Delta$ (12w)",
  "rank_change_26w" = "Rank $\\Delta$ (26w)",
  "rank_change_52w" = "Rank $\\Delta$ (52w)",
  "elo_change_12w" = "Elo $\\Delta$ (12w)",
  "elo_change_26w" = "Elo $\\Delta$ (26w)"
)

run_dynamic_fe <- function(data, outcomes, label_prefix, fe_var = "slam_year") {
  results <- list()
  for (out in outcomes) {
    if (!out %in% names(data)) next
    y <- data[[out]]
    ok <- !is.na(y) & !is.na(data$pre_rank_pts) & !is.na(data$player_age) &
          !is.na(data$got_ll) & !is.na(data[[fe_var]])
    if (sum(ok & data$got_ll == 1) < 3 || sum(ok & data$got_ll == 0) < 3) next

    # t-test (simple difference)
    d <- data$got_ll
    tt_diff <- mean(y[ok & d == 1]) - mean(y[ok & d == 0])
    tt <- tryCatch(t.test(y[ok & d == 1], y[ok & d == 0]), error = function(e) NULL)

    # Event FE + controls, clustered at player level
    fe_fit <- tryCatch({
      feols(as.formula(paste0(out, " ~ got_ll + pre_rank_pts + pre_rank_pts_sq + player_age | ", fe_var)),
            data = data[ok, ], vcov = ~player_id)
    }, error = function(e) {
      # Fallback: no clustering if too few clusters
      tryCatch({
        feols(as.formula(paste0(out, " ~ got_ll + pre_rank_pts + pre_rank_pts_sq + player_age | ", fe_var)),
              data = data[ok, ], vcov = "hetero")
      }, error = function(e2) NULL)
    })

    results[[out]] <- tibble(
      sample = label_prefix,
      outcome = out,
      ttest_diff = tt_diff,
      ttest_pv = if (!is.null(tt)) tt$p.value else NA_real_,
      fe_coef = if (!is.null(fe_fit)) coef(fe_fit)["got_ll"] else NA_real_,
      fe_se   = if (!is.null(fe_fit)) {
        sqrt(vcov(fe_fit)["got_ll", "got_ll"])
      } else NA_real_,
      fe_pv   = if (!is.null(fe_fit)) {
        c_val <- coef(fe_fit)["got_ll"]
        s_val <- sqrt(vcov(fe_fit)["got_ll", "got_ll"])
        2 * pnorm(-abs(c_val / s_val))
      } else NA_real_,
      n_obs = sum(ok),
      n_treated = sum(ok & d == 1),
      n_control = sum(ok & d == 0)
    )
    message("  ", label_prefix, " ", out, ": FE coef = ",
            sprintf("%.2f", results[[out]]$fe_coef),
            " (SE=", sprintf("%.2f", results[[out]]$fe_se),
            "), p=", sprintf("%.3f", results[[out]]$fe_pv),
            ", N=", results[[out]]$n_obs)
  }
  bind_rows(results)
}

# Run for ATP, WTA, Pooled
dyn_atp    <- run_dynamic_fe(gs_atp, dynamic_outcomes, "ATP")
dyn_wta    <- run_dynamic_fe(gs_wta, dynamic_outcomes, "WTA")
dyn_pooled <- run_dynamic_fe(gs_lottery_fe, dynamic_outcomes, "Pooled")

dynamic_all <- bind_rows(dyn_atp, dyn_wta, dyn_pooled)

# BH correction within each sample
dynamic_all <- dynamic_all |>
  group_by(sample) |>
  mutate(pv_bh = p.adjust(fe_pv, method = "BH")) |>
  ungroup()

saveRDS(dynamic_all, file.path(CLEANED_DIR, "dynamic_event_fe_results.rds"))
message("  Saved: dynamic_event_fe_results.rds")

# --- Build LaTeX table: columns = key horizons, rows = outcomes ---
# Focus table on primary outcomes at key horizons

# Table layout: rows = outcome families, columns = t-test | event FE for pooled
dyn_tex <- c(
  "\\begin{tabular}{l cc cc cc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{ATP} & \\multicolumn{2}{c}{WTA} & \\multicolumn{2}{c}{Pooled} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
  "Outcome & $\\hat{\\beta}$ & SE & $\\hat{\\beta}$ & SE & $\\hat{\\beta}$ & SE \\\\",
  "\\midrule",
  "\\multicolumn{7}{l}{\\textit{Panel A: Access outcomes}} \\\\"
)

access_outs <- c("n_main_draws_26w", "n_main_draws_52w", "n_matches_250plus_26w", "n_matches_250plus_52w")
points_outs <- c("points_change_12w", "points_change_26w", "points_change_52w")
rank_outs   <- c("rank_change_12w", "rank_change_26w", "rank_change_52w")
elo_outs    <- c("elo_change_12w", "elo_change_26w")

for (out in c(access_outs, "BREAK_B", points_outs, "BREAK_C", rank_outs, "BREAK_D", elo_outs)) {
  if (out == "BREAK_B") {
    dyn_tex <- c(dyn_tex, "\\addlinespace",
                 "\\multicolumn{7}{l}{\\textit{Panel B: Ranking points}} \\\\")
    next
  }
  if (out == "BREAK_C") {
    dyn_tex <- c(dyn_tex, "\\addlinespace",
                 "\\multicolumn{7}{l}{\\textit{Panel C: Ranking position}} \\\\")
    next
  }
  if (out == "BREAK_D") {
    dyn_tex <- c(dyn_tex, "\\addlinespace",
                 "\\multicolumn{7}{l}{\\textit{Panel D: Elo rating}} \\\\")
    next
  }
  if (!out %in% dynamic_all$outcome) next

  lab <- if (out %in% names(dynamic_labels)) dynamic_labels[out] else out
  vals <- ""
  for (samp in c("ATP", "WTA", "Pooled")) {
    r <- dynamic_all |> filter(sample == samp, outcome == out)
    if (nrow(r) > 0 && !is.na(r$fe_coef)) {
      vals <- paste0(vals, sprintf("%.2f", r$fe_coef), add_stars(r$fe_pv),
                     " & (", sprintf("%.2f", r$fe_se), ")")
    } else {
      vals <- paste0(vals, "-- & --")
    }
    if (samp != "Pooled") vals <- paste0(vals, " & ")
  }
  dyn_tex <- c(dyn_tex, paste0("\\quad ", lab, " & ", vals, " \\\\"))
}

# N row
n_atp <- if (nrow(dyn_atp) > 0) dyn_atp$n_obs[1] else 0
n_wta <- if (nrow(dyn_wta) > 0) dyn_wta$n_obs[1] else 0
n_pool <- if (nrow(dyn_pooled) > 0) dyn_pooled$n_obs[1] else 0

dyn_tex <- c(dyn_tex,
  "\\midrule",
  paste0("$N$ & \\multicolumn{2}{c}{", n_atp,
         "} & \\multicolumn{2}{c}{", n_wta,
         "} & \\multicolumn{2}{c}{", n_pool, "} \\\\"),
  "FE & \\multicolumn{6}{c}{Grand Slam $\\times$ Year} \\\\",
  "Controls & \\multicolumn{6}{c}{Pre-rank points, points$^2$, age} \\\\",
  "Clustering & \\multicolumn{6}{c}{Player} \\\\",
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(dyn_tex, file.path(TABLES_DIR, "table_dynamic_event_fe.tex"))
message("  Saved: table_dynamic_event_fe.tex")

summary_lines <- c(summary_lines,
  "## TASK 2: Dynamic Event FE Results",
  paste0("- Pooled N (approx): ", n_pool),
  paste0("- Outcomes tested: ", length(dynamic_outcomes)),
  "")


# ==============================================================================
# TASK 3: MULTIPLE-TREATMENT ADJUSTMENT
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 3: MULTIPLE-TREATMENT ADJUSTMENT")
message(strrep("=", 70))

# For each player-event in losers_gs, compute time to next qualifying loss
# Then allow multiple events per player, censoring outcomes at horizon cap

losers_gs_sorted <- losers_gs |>
  arrange(player_id, event_date) |>
  group_by(player_id) |>
  mutate(
    next_event_date = lead(event_date),
    weeks_to_next = as.numeric(difftime(next_event_date, event_date, units = "weeks")),
    horizon_cap = pmin(52, coalesce(weeks_to_next, 52))
  ) |>
  ungroup()

message("  GS losers with multiple events: ", nrow(losers_gs_sorted))
message("  Mean weeks to next event: ",
        round(mean(losers_gs_sorted$weeks_to_next, na.rm = TRUE), 1))
message("  Median horizon cap: ",
        round(median(losers_gs_sorted$horizon_cap, na.rm = TRUE), 1))

# GS lottery sample with multiple events (not first-LL-only)
gs_multi <- losers_gs_sorted |>
  filter(!is.na(rank_among_losers), rank_among_losers <= 4) |>
  mutate(
    pre_rank_pts = coalesce(points_t0, player_rank_points, 0),
    pre_rank_pts_sq = pre_rank_pts^2,
    slam_year = paste0(coalesce(slam_name, tourney_name), "_", year)
  )

# Censor outcomes based on horizon cap
# If horizon_cap < 26, null out the 26w and 52w outcomes
# If horizon_cap < 52, null out the 52w outcomes
gs_multi <- gs_multi |>
  mutate(
    rank_change_26w_cens = ifelse(horizon_cap >= 26, rank_change_26w, NA_real_),
    rank_change_52w_cens = ifelse(horizon_cap >= 52, rank_change_52w, NA_real_),
    points_change_26w_cens = ifelse(horizon_cap >= 26, points_change_26w, NA_real_),
    points_change_52w_cens = ifelse(horizon_cap >= 52, points_change_52w, NA_real_),
    n_main_draws_26w_cens = ifelse(horizon_cap >= 26, n_main_draws_26w, NA_integer_),
    n_main_draws_52w_cens = ifelse(horizon_cap >= 52, n_main_draws_52w, NA_integer_),
    n_matches_250plus_26w_cens = ifelse(horizon_cap >= 26, n_matches_250plus_26w, NA_integer_),
    n_matches_250plus_52w_cens = ifelse(horizon_cap >= 52, n_matches_250plus_52w, NA_integer_),
    # Short horizons always valid
    rank_change_4w_cens  = ifelse(horizon_cap >= 4, rank_change_4w, NA_real_),
    rank_change_8w_cens  = ifelse(horizon_cap >= 8, rank_change_8w, NA_real_),
    rank_change_12w_cens = ifelse(horizon_cap >= 12, rank_change_12w, NA_real_),
    points_change_12w_cens = ifelse(horizon_cap >= 12, points_change_12w, NA_real_)
  )

if ("elo_change_12w" %in% names(gs_multi)) {
  gs_multi <- gs_multi |>
    mutate(
      elo_change_12w_cens = ifelse(horizon_cap >= 12, elo_change_12w, NA_real_),
      elo_change_26w_cens = ifelse(horizon_cap >= 26, elo_change_26w, NA_real_)
    )
}

message("  Multi-event GS lottery: N = ", nrow(gs_multi),
        " (treated = ", sum(gs_multi$got_ll), ")")
message("  Censored 26w obs available: ", sum(!is.na(gs_multi$rank_change_26w_cens)))
message("  Censored 52w obs available: ", sum(!is.na(gs_multi$rank_change_52w_cens)))

# Filter to valid FE cells
slam_year_multi <- gs_multi |> count(slam_year) |> filter(n >= 2)
gs_multi_fe <- gs_multi |> filter(slam_year %in% slam_year_multi$slam_year)

# Run censored dynamic specs
cens_outcomes <- c("rank_change_4w_cens", "rank_change_12w_cens",
                   "rank_change_26w_cens", "rank_change_52w_cens",
                   "points_change_12w_cens", "points_change_26w_cens", "points_change_52w_cens",
                   "n_main_draws_26w_cens", "n_main_draws_52w_cens",
                   "n_matches_250plus_26w_cens", "n_matches_250plus_52w_cens")

if ("elo_change_12w_cens" %in% names(gs_multi_fe)) {
  cens_outcomes <- c(cens_outcomes, "elo_change_12w_cens", "elo_change_26w_cens")
}

# Need to ensure gs_multi_fe has pre_rank_pts etc
multi_results <- list()
for (out in cens_outcomes) {
  if (!out %in% names(gs_multi_fe)) next
  y <- gs_multi_fe[[out]]
  ok <- !is.na(y) & !is.na(gs_multi_fe$pre_rank_pts) & !is.na(gs_multi_fe$player_age) &
        !is.na(gs_multi_fe$got_ll)
  if (sum(ok & gs_multi_fe$got_ll == 1) < 3 || sum(ok & gs_multi_fe$got_ll == 0) < 3) next

  fe_fit <- tryCatch({
    feols(as.formula(paste0(out, " ~ got_ll + pre_rank_pts + pre_rank_pts_sq + player_age | slam_year")),
          data = gs_multi_fe[ok, ], vcov = ~player_id)
  }, error = function(e) {
    tryCatch({
      feols(as.formula(paste0(out, " ~ got_ll + pre_rank_pts + pre_rank_pts_sq + player_age | slam_year")),
            data = gs_multi_fe[ok, ], vcov = "hetero")
    }, error = function(e2) NULL)
  })

  multi_results[[out]] <- tibble(
    outcome = out,
    spec = "Multi-event censored",
    fe_coef = if (!is.null(fe_fit)) coef(fe_fit)["got_ll"] else NA_real_,
    fe_se   = if (!is.null(fe_fit)) sqrt(vcov(fe_fit)["got_ll", "got_ll"]) else NA_real_,
    fe_pv   = if (!is.null(fe_fit)) {
      2 * pnorm(-abs(coef(fe_fit)["got_ll"] / sqrt(vcov(fe_fit)["got_ll", "got_ll"])))
    } else NA_real_,
    n_obs = sum(ok)
  )
  message("  ", out, ": coef = ", sprintf("%.2f", multi_results[[out]]$fe_coef),
          ", p = ", sprintf("%.3f", multi_results[[out]]$fe_pv),
          ", N = ", multi_results[[out]]$n_obs)
}

multi_df <- bind_rows(multi_results)

# Also run first-LL-only (the robustness from skeleton)
# gs_lottery already is first-LL-only
first_ll_results <- list()
for (out in dynamic_outcomes) {
  if (!out %in% names(gs_lottery_fe)) next
  y <- gs_lottery_fe[[out]]
  ok <- !is.na(y) & !is.na(gs_lottery_fe$pre_rank_pts) & !is.na(gs_lottery_fe$player_age) &
        !is.na(gs_lottery_fe$got_ll)
  if (sum(ok & gs_lottery_fe$got_ll == 1) < 3 || sum(ok & gs_lottery_fe$got_ll == 0) < 3) next

  fe_fit <- tryCatch({
    feols(as.formula(paste0(out, " ~ got_ll + pre_rank_pts + pre_rank_pts_sq + player_age | slam_year")),
          data = gs_lottery_fe[ok, ], vcov = ~player_id)
  }, error = function(e) {
    tryCatch({
      feols(as.formula(paste0(out, " ~ got_ll + pre_rank_pts + pre_rank_pts_sq + player_age | slam_year")),
            data = gs_lottery_fe[ok, ], vcov = "hetero")
    }, error = function(e2) NULL)
  })

  first_ll_results[[out]] <- tibble(
    outcome = out,
    spec = "First-LL-only",
    fe_coef = if (!is.null(fe_fit)) coef(fe_fit)["got_ll"] else NA_real_,
    fe_se   = if (!is.null(fe_fit)) sqrt(vcov(fe_fit)["got_ll", "got_ll"]) else NA_real_,
    fe_pv   = if (!is.null(fe_fit)) {
      2 * pnorm(-abs(coef(fe_fit)["got_ll"] / sqrt(vcov(fe_fit)["got_ll", "got_ll"])))
    } else NA_real_,
    n_obs = sum(ok)
  )
}

first_ll_df <- bind_rows(first_ll_results)

multi_treatment_adj <- bind_rows(multi_df, first_ll_df)
saveRDS(multi_treatment_adj, file.path(CLEANED_DIR, "multiple_treatment_adj_results.rds"))
message("  Saved: multiple_treatment_adj_results.rds")

summary_lines <- c(summary_lines,
  "## TASK 3: Multiple-Treatment Adjustment",
  paste0("- Multi-event censored sample: N = ", nrow(gs_multi_fe)),
  paste0("- First-LL-only sample: N = ", nrow(gs_lottery_fe)),
  "")


# ==============================================================================
# TASK 4: LL HISTORY VARIABLES
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 4: LL HISTORY VARIABLES")
message(strrep("=", 70))

# For each player-event, compute prior LL opportunities and wins
# Use all_qualifiers (all qualifying losses, not just estimation sample)

all_losers <- all_qualifiers |>
  filter(won_qualifying_match == 0L, !is.na(event_date)) |>
  arrange(player_id, event_date)

# For each obs, count prior GS and non-GS LL opportunities/wins
message("  Computing LL history for ", nrow(all_losers), " observations...")

all_losers <- all_losers |>
  group_by(player_id) |>
  mutate(
    # Cumulative counts BEFORE this event (lag)
    cum_gs_opp  = cumsum(tourney_level == "G") - (tourney_level == "G"),
    cum_gs_ll   = cumsum(tourney_level == "G" & got_ll == 1) - (tourney_level == "G" & got_ll == 1),
    cum_nongs_opp = cumsum(tourney_level != "G") - (tourney_level != "G"),
    cum_nongs_ll  = cumsum(tourney_level != "G" & got_ll == 1) - (tourney_level != "G" & got_ll == 1)
  ) |>
  ungroup() |>
  mutate(
    n_prior_gs_ll_opportunities = as.integer(cum_gs_opp),
    n_prior_gs_ll_won = as.integer(cum_gs_ll),
    n_prior_nongs_ll_opportunities = as.integer(cum_nongs_opp),
    n_prior_nongs_ll_won = as.integer(cum_nongs_ll),
    had_prior_ll = as.integer(cum_gs_ll > 0 | cum_nongs_ll > 0)
  )

message("  LL history computed.")
message("  Players with prior GS LL: ", sum(all_losers$n_prior_gs_ll_won > 0))
message("  Players with prior non-GS LL: ", sum(all_losers$n_prior_nongs_ll_won > 0))
message("  Players with any prior LL: ", sum(all_losers$had_prior_ll == 1))

# Merge LL history back to gs_lottery
ll_history_cols <- c("tourney_id", "player_id",
                     "n_prior_gs_ll_opportunities", "n_prior_gs_ll_won",
                     "n_prior_nongs_ll_opportunities", "n_prior_nongs_ll_won",
                     "had_prior_ll")

gs_lottery <- gs_lottery |>
  select(-any_of(setdiff(ll_history_cols, c("tourney_id", "player_id")))) |>
  left_join(
    all_losers |> select(all_of(ll_history_cols)) |> distinct(),
    by = c("tourney_id", "player_id")
  ) |>
  mutate(across(starts_with("n_prior_"), ~replace_na(.x, 0L)),
         had_prior_ll = replace_na(had_prior_ll, 0L))

message("  GS lottery with LL history: ", nrow(gs_lottery))
message("  had_prior_ll distribution: ",
        sum(gs_lottery$had_prior_ll == 1), " yes, ",
        sum(gs_lottery$had_prior_ll == 0), " no")

# Re-run main spec including LL history controls
gs_lottery_fe2 <- gs_lottery |>
  filter(slam_year %in% slam_year_counts$slam_year)

history_results <- list()
for (out in dynamic_outcomes) {
  if (!out %in% names(gs_lottery_fe2)) next
  y <- gs_lottery_fe2[[out]]
  ok <- !is.na(y) & !is.na(gs_lottery_fe2$pre_rank_pts) &
        !is.na(gs_lottery_fe2$player_age) & !is.na(gs_lottery_fe2$got_ll) &
        !is.na(gs_lottery_fe2$had_prior_ll)
  if (sum(ok & gs_lottery_fe2$got_ll == 1) < 3) next

  fe_fit <- tryCatch({
    feols(as.formula(paste0(out, " ~ got_ll + pre_rank_pts + pre_rank_pts_sq + player_age + had_prior_ll | slam_year")),
          data = gs_lottery_fe2[ok, ], vcov = ~player_id)
  }, error = function(e) {
    tryCatch({
      feols(as.formula(paste0(out, " ~ got_ll + pre_rank_pts + pre_rank_pts_sq + player_age + had_prior_ll | slam_year")),
            data = gs_lottery_fe2[ok, ], vcov = "hetero")
    }, error = function(e2) NULL)
  })

  history_results[[out]] <- tibble(
    outcome = out,
    spec = "With LL history",
    fe_coef = if (!is.null(fe_fit)) coef(fe_fit)["got_ll"] else NA_real_,
    fe_se   = if (!is.null(fe_fit)) sqrt(vcov(fe_fit)["got_ll", "got_ll"]) else NA_real_,
    fe_pv   = if (!is.null(fe_fit)) {
      2 * pnorm(-abs(coef(fe_fit)["got_ll"] / sqrt(vcov(fe_fit)["got_ll", "got_ll"])))
    } else NA_real_,
    had_prior_coef = if (!is.null(fe_fit) && "had_prior_ll" %in% names(coef(fe_fit))) {
      coef(fe_fit)["had_prior_ll"]
    } else NA_real_,
    n_obs = sum(ok)
  )
  message("  ", out, ": got_ll coef = ", sprintf("%.2f", history_results[[out]]$fe_coef),
          ", had_prior coef = ", sprintf("%.2f", history_results[[out]]$had_prior_coef))
}

ll_history_df <- bind_rows(history_results)

# Also merge to losers_nongs
losers_nongs <- losers_nongs |>
  select(-any_of(setdiff(ll_history_cols, c("tourney_id", "player_id")))) |>
  left_join(
    all_losers |> select(all_of(ll_history_cols)) |> distinct(),
    by = c("tourney_id", "player_id")
  ) |>
  mutate(across(starts_with("n_prior_"), ~replace_na(.x, 0L)),
         had_prior_ll = replace_na(had_prior_ll, 0L))

saveRDS(list(
  gs_lottery = gs_lottery,
  losers_nongs = losers_nongs,
  ll_history_regressions = ll_history_df,
  all_losers_with_history = all_losers |> select(all_of(ll_history_cols))
), file.path(CLEANED_DIR, "ll_history_variables.rds"))
message("  Saved: ll_history_variables.rds")

summary_lines <- c(summary_lines,
  "## TASK 4: LL History Variables",
  paste0("- Players with any prior LL: ", sum(gs_lottery$had_prior_ll == 1), " / ", nrow(gs_lottery)),
  "")


# ==============================================================================
# TASK 5: HETEROGENEITY BY SUBGROUPS
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 5: HETEROGENEITY BY SUBGROUPS")
message(strrep("=", 70))

# Key outcome for heterogeneity: use 26w horizon
het_outcomes <- c("rank_change_26w", "points_change_26w",
                  "n_main_draws_26w", "n_matches_250plus_26w")

# Subgroup definitions
gs_lottery <- gs_lottery |>
  mutate(
    # Ranking: above/below median
    rank_median = median(pre_rank_pts, na.rm = TRUE),
    rank_group = ifelse(pre_rank_pts >= rank_median, "High rank pts", "Low rank pts"),
    # Age: above/below median
    age_median = median(player_age, na.rm = TRUE),
    age_group = ifelse(player_age >= age_median, "Older", "Younger"),
    # Prior LL
    prior_ll_group = ifelse(had_prior_ll == 1, "Had prior LL", "No prior LL"),
    # Treatment dose (for treated only, 0 for controls)
    dose_group = case_when(
      got_ll == 0 ~ "Control",
      md_matches_won == 0 ~ "0 wins",
      md_matches_won == 1 ~ "1 win",
      md_matches_won >= 2 ~ "2+ wins",
      TRUE ~ "Control"
    )
  )

gs_lottery_fe3 <- gs_lottery |>
  filter(slam_year %in% slam_year_counts$slam_year)

run_het_subgroup <- function(data, group_var, outcomes, sample_label) {
  results <- list()
  groups <- unique(data[[group_var]])
  groups <- groups[!is.na(groups)]

  for (grp in groups) {
    sub <- data |> filter(.data[[group_var]] == grp)
    if (sum(sub$got_ll == 1) < 2 || sum(sub$got_ll == 0) < 2) next

    for (out in outcomes) {
      if (!out %in% names(sub)) next
      y <- sub[[out]]
      d <- sub$got_ll
      ok <- !is.na(y) & !is.na(d)
      if (sum(ok & d == 1) < 2 || sum(ok & d == 0) < 2) next

      tt <- tryCatch(t.test(y[ok & d == 1], y[ok & d == 0]), error = function(e) NULL)

      # Simple OLS with controls (FE may fail with small subgroups)
      ols_fit <- tryCatch({
        lm(as.formula(paste0(out, " ~ got_ll + pre_rank_pts + pre_rank_pts_sq + player_age")),
           data = sub[ok, ])
      }, error = function(e) NULL)

      results[[paste0(grp, "_", out)]] <- tibble(
        sample = sample_label,
        group_var = group_var,
        group_val = grp,
        outcome = out,
        n_obs = sum(ok),
        n_treated = sum(ok & d == 1),
        n_control = sum(ok & d == 0),
        mean_treated = mean(y[ok & d == 1]),
        mean_control = mean(y[ok & d == 0]),
        diff = mean(y[ok & d == 1]) - mean(y[ok & d == 0]),
        ttest_pv = if (!is.null(tt)) tt$p.value else NA_real_,
        ols_coef = if (!is.null(ols_fit)) coef(ols_fit)["got_ll"] else NA_real_,
        ols_se   = if (!is.null(ols_fit)) sqrt(vcov(ols_fit)["got_ll", "got_ll"]) else NA_real_,
        ols_pv   = if (!is.null(ols_fit)) {
          summary(ols_fit)$coefficients["got_ll", "Pr(>|t|)"]
        } else NA_real_
      )
    }
  }
  bind_rows(results)
}

# ATP heterogeneity
gs_atp3 <- gs_lottery_fe3 |> filter(tour == "ATP")
gs_wta3 <- gs_lottery_fe3 |> filter(tour == "WTA")

het_rank_atp <- run_het_subgroup(gs_atp3, "rank_group", het_outcomes, "ATP")
het_rank_wta <- run_het_subgroup(gs_wta3, "rank_group", het_outcomes, "WTA")
het_age_atp  <- run_het_subgroup(gs_atp3, "age_group", het_outcomes, "ATP")
het_age_wta  <- run_het_subgroup(gs_wta3, "age_group", het_outcomes, "WTA")
het_prior_atp <- run_het_subgroup(gs_atp3, "prior_ll_group", het_outcomes, "ATP")
het_prior_wta <- run_het_subgroup(gs_wta3, "prior_ll_group", het_outcomes, "WTA")
het_dose_atp <- run_het_subgroup(gs_atp3, "dose_group", het_outcomes, "ATP")
het_dose_wta <- run_het_subgroup(gs_wta3, "dose_group", het_outcomes, "WTA")

het_all <- bind_rows(
  het_rank_atp, het_rank_wta,
  het_age_atp, het_age_wta,
  het_prior_atp, het_prior_wta,
  het_dose_atp, het_dose_wta
)

message("  Heterogeneity results: ", nrow(het_all), " rows")
saveRDS(het_all, file.path(CLEANED_DIR, "heterogeneity_results.rds"))
message("  Saved: heterogeneity_results.rds")

# --- Build LaTeX heterogeneity table ---
# Focus on rank_change_26w
het_focus <- het_all |> filter(outcome == "rank_change_26w")

het_tex <- c(
  "\\begin{tabular}{ll cccc cccc}",
  "\\toprule",
  " & & \\multicolumn{4}{c}{ATP} & \\multicolumn{4}{c}{WTA} \\\\",
  "\\cmidrule(lr){3-6} \\cmidrule(lr){7-10}",
  "Dimension & Subgroup & $\\hat{\\beta}$ & SE & $p$ & $N$ & $\\hat{\\beta}$ & SE & $p$ & $N$ \\\\",
  "\\midrule"
)

dim_labels <- c(
  "rank_group" = "Pre-ranking",
  "age_group" = "Age",
  "prior_ll_group" = "Prior LL spot",
  "dose_group" = "Treatment dose"
)

for (gvar in c("rank_group", "age_group", "prior_ll_group", "dose_group")) {
  dim_lab <- dim_labels[gvar]
  sub_het <- het_focus |> filter(group_var == gvar)
  groups <- unique(sub_het$group_val)

  for (grp in groups) {
    r_atp <- sub_het |> filter(sample == "ATP", group_val == grp)
    r_wta <- sub_het |> filter(sample == "WTA", group_val == grp)

    atp_vals <- if (nrow(r_atp) > 0 && !is.na(r_atp$ols_coef)) {
      paste0(sprintf("%.1f", r_atp$ols_coef), add_stars(r_atp$ols_pv), " & ",
             sprintf("%.1f", r_atp$ols_se), " & ",
             sprintf("%.3f", r_atp$ols_pv), " & ", r_atp$n_obs)
    } else "-- & -- & -- & --"

    wta_vals <- if (nrow(r_wta) > 0 && !is.na(r_wta$ols_coef)) {
      paste0(sprintf("%.1f", r_wta$ols_coef), add_stars(r_wta$ols_pv), " & ",
             sprintf("%.1f", r_wta$ols_se), " & ",
             sprintf("%.3f", r_wta$ols_pv), " & ", r_wta$n_obs)
    } else "-- & -- & -- & --"

    het_tex <- c(het_tex,
      paste0(dim_lab, " & ", grp, " & ", atp_vals, " & ", wta_vals, " \\\\"))
    dim_lab <- ""  # Only show dimension label on first row
  }
  het_tex <- c(het_tex, "\\addlinespace")
}

het_tex <- c(het_tex,
  "\\midrule",
  "\\multicolumn{10}{l}{Outcome: Ranking change at 26 weeks. OLS with pre-ranking controls.} \\\\",
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(het_tex, file.path(TABLES_DIR, "table_heterogeneity_gs.tex"))
message("  Saved: table_heterogeneity_gs.tex")

summary_lines <- c(summary_lines,
  "## TASK 5: Heterogeneity",
  paste0("- Total heterogeneity rows: ", nrow(het_all)),
  "")


# ==============================================================================
# TASK 6: NON-GS TABLES (FOR APPENDIX)
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 6: NON-GS TABLES (APPENDIX)")
message(strrep("=", 70))

# Prepare non-GS IV sample
est_iv2 <- est_iv |>
  mutate(
    pre_rank_pts = coalesce(points_t0, player_rank_points, 0),
    pre_rank_pts_sq = pre_rank_pts^2
  )

# Merge LL history to est_iv2
est_iv2 <- est_iv2 |>
  select(-any_of(c("n_prior_gs_ll_opportunities", "n_prior_gs_ll_won",
                    "n_prior_nongs_ll_opportunities", "n_prior_nongs_ll_won",
                    "had_prior_ll"))) |>
  left_join(
    all_losers |> select(all_of(ll_history_cols)) |> distinct(),
    by = c("tourney_id", "player_id")
  ) |>
  mutate(across(starts_with("n_prior_"), ~replace_na(.x, 0L)),
         had_prior_ll = replace_na(had_prior_ll, 0L))

# Check for peer_component
has_iv <- "peer_component" %in% names(est_iv2)
message("  Non-GS IV sample: N = ", nrow(est_iv2))
message("  Has instrument (peer_component): ", has_iv)
if (has_iv) {
  message("  Treated: ", sum(est_iv2$got_ll))
  message("  Control: ", sum(!est_iv2$got_ll))
}

# --- 6a. Immediate effects (non-GS) ---
message("\n--- 6a. Non-GS immediate effects ---")

# For non-GS, we need to look up main draw match details similarly
# But for non-GS, entry type determines LL status
# Merge ll_match_details to est_iv2
est_iv2 <- est_iv2 |>
  left_join(ll_event_summary, by = c("tourney_id", "player_id")) |>
  mutate(
    md_matches_played    = ifelse(got_ll == 1, replace_na(md_matches_played, 1L), 0L),
    md_matches_won       = ifelse(got_ll == 1, replace_na(md_matches_won, 0L), 0L),
    md_any_win           = ifelse(got_ll == 1, replace_na(md_any_win, 0L), 0L),
    # Non-GS ranking points: approximate
    # 250: R1=20, R2=30; 500: R1=20, R2=45; Masters: R1=10, R2=45, R3=90
    ranking_pts_at_event = case_when(
      got_ll == 0 ~ 0,
      tourney_level == "M" & md_matches_won == 0 ~ 10,
      tourney_level == "M" & md_matches_won == 1 ~ 45,
      tourney_level == "M" & md_matches_won >= 2 ~ 90,
      tourney_level == "A" & md_matches_won == 0 ~ 20,
      tourney_level == "A" & md_matches_won == 1 ~ 30,
      tourney_level == "A" & md_matches_won >= 2 ~ 60,
      TRUE ~ 10
    )
  )

# Non-GS: IV regressions for immediate outcomes
nongs_imm_results <- list()
if (has_iv) {
  for (out in immediate_outcomes) {
    if (!out %in% names(est_iv2)) next
    y <- est_iv2[[out]]
    ok <- !is.na(y) & !is.na(est_iv2$peer_component) & !is.na(est_iv2$pre_rank_pts) &
          !is.na(est_iv2$player_age)
    if (sum(ok) < 50) next

    # t-test
    d <- est_iv2$got_ll
    tt <- tryCatch(t.test(y[ok & d == 1], y[ok & d == 0]), error = function(e) NULL)

    # IV
    iv_fit <- tryCatch({
      feols(as.formula(paste0(out, " ~ pre_rank_pts + pre_rank_pts_sq + player_age | year | got_ll ~ peer_component")),
            data = est_iv2[ok, ], vcov = ~player_id)
    }, error = function(e) NULL)

    nongs_imm_results[[out]] <- tibble(
      outcome = out,
      mean_treated = mean(y[ok & d == 1]),
      mean_control = mean(y[ok & d == 0]),
      ttest_diff = mean(y[ok & d == 1]) - mean(y[ok & d == 0]),
      ttest_pv = if (!is.null(tt)) tt$p.value else NA_real_,
      iv_coef = if (!is.null(iv_fit)) coef(iv_fit)["fit_got_ll"] else NA_real_,
      iv_se   = if (!is.null(iv_fit)) sqrt(vcov(iv_fit)["fit_got_ll", "fit_got_ll"]) else NA_real_,
      iv_pv   = if (!is.null(iv_fit)) {
        2 * pnorm(-abs(coef(iv_fit)["fit_got_ll"] / sqrt(vcov(iv_fit)["fit_got_ll", "fit_got_ll"])))
      } else NA_real_,
      n_obs = sum(ok)
    )
    message("  Non-GS ", out, ": IV coef = ", sprintf("%.2f", nongs_imm_results[[out]]$iv_coef),
            ", p = ", sprintf("%.3f", nongs_imm_results[[out]]$iv_pv))
  }
}

nongs_imm_df <- bind_rows(nongs_imm_results)

# LaTeX
nongs_imm_tex <- c(
  "\\begin{tabular}{l ccc ccc}",
  "\\toprule",
  " & \\multicolumn{3}{c}{$t$-test} & \\multicolumn{3}{c}{LOO-IV} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
  "Outcome & LL Mean & Ctrl Mean & $p$ & $\\hat{\\beta}_{IV}$ & SE & $p$ \\\\",
  "\\midrule"
)

for (out in immediate_outcomes) {
  r <- nongs_imm_df |> filter(outcome == out)
  if (nrow(r) == 0) next
  lab <- immediate_labels[out]
  nongs_imm_tex <- c(nongs_imm_tex,
    paste0(lab, " & ",
           sprintf("%.2f", r$mean_treated), " & ",
           sprintf("%.2f", r$mean_control), " & ",
           sprintf("%.3f", r$ttest_pv), " & ",
           if (!is.na(r$iv_coef)) paste0(sprintf("%.2f", r$iv_coef), add_stars(r$iv_pv)) else "--",
           " & ",
           if (!is.na(r$iv_se)) sprintf("%.2f", r$iv_se) else "--", " & ",
           if (!is.na(r$iv_pv)) sprintf("%.3f", r$iv_pv) else "--",
           " \\\\"))
}

nongs_imm_tex <- c(nongs_imm_tex,
  "\\midrule",
  paste0("$N$ & \\multicolumn{6}{c}{", if (nrow(nongs_imm_df) > 0) nongs_imm_df$n_obs[1] else "N/A", "} \\\\"),
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(nongs_imm_tex, file.path(TABLES_DIR, "table_immediate_effects_nongs.tex"))
message("  Saved: table_immediate_effects_nongs.tex")


# --- 6b. Dynamic outcomes (non-GS IV) ---
message("\n--- 6b. Non-GS dynamic outcomes ---")

nongs_dyn_results <- list()
if (has_iv) {
  iv_dyn_outcomes <- c(
    "n_main_draws_26w", "n_main_draws_52w",
    "n_matches_250plus_26w", "n_matches_250plus_52w",
    "points_change_12w", "points_change_26w", "points_change_52w",
    "rank_change_12w", "rank_change_26w", "rank_change_52w"
  )
  if ("elo_change_12w" %in% names(est_iv2)) {
    iv_dyn_outcomes <- c(iv_dyn_outcomes, "elo_change_12w", "elo_change_26w")
  }

  for (out in iv_dyn_outcomes) {
    if (!out %in% names(est_iv2)) next
    y <- est_iv2[[out]]
    ok <- !is.na(y) & !is.na(est_iv2$peer_component) & !is.na(est_iv2$pre_rank_pts) &
          !is.na(est_iv2$player_age)
    if (sum(ok) < 50) next

    iv_fit <- tryCatch({
      feols(as.formula(paste0(out, " ~ pre_rank_pts + pre_rank_pts_sq + player_age | year | got_ll ~ peer_component")),
            data = est_iv2[ok, ], vcov = ~player_id)
    }, error = function(e) NULL)

    nongs_dyn_results[[out]] <- tibble(
      outcome = out,
      iv_coef = if (!is.null(iv_fit)) coef(iv_fit)["fit_got_ll"] else NA_real_,
      iv_se   = if (!is.null(iv_fit)) sqrt(vcov(iv_fit)["fit_got_ll", "fit_got_ll"]) else NA_real_,
      iv_pv   = if (!is.null(iv_fit)) {
        2 * pnorm(-abs(coef(iv_fit)["fit_got_ll"] / sqrt(vcov(iv_fit)["fit_got_ll", "fit_got_ll"])))
      } else NA_real_,
      n_obs = sum(ok)
    )
    message("  Non-GS ", out, ": IV coef = ", sprintf("%.2f", nongs_dyn_results[[out]]$iv_coef),
            ", p = ", sprintf("%.3f", nongs_dyn_results[[out]]$iv_pv))
  }
}

nongs_dyn_df <- bind_rows(nongs_dyn_results)

# LaTeX
nongs_dyn_tex <- c(
  "\\begin{tabular}{l ccc}",
  "\\toprule",
  "Outcome & $\\hat{\\beta}_{IV}$ & SE & $p$ \\\\",
  "\\midrule",
  "\\multicolumn{4}{l}{\\textit{Panel A: Access outcomes}} \\\\"
)

for (out in c(access_outs, "BREAK_B", points_outs, "BREAK_C", rank_outs, "BREAK_D", elo_outs)) {
  if (out == "BREAK_B") {
    nongs_dyn_tex <- c(nongs_dyn_tex, "\\addlinespace",
                       "\\multicolumn{4}{l}{\\textit{Panel B: Ranking points}} \\\\")
    next
  }
  if (out == "BREAK_C") {
    nongs_dyn_tex <- c(nongs_dyn_tex, "\\addlinespace",
                       "\\multicolumn{4}{l}{\\textit{Panel C: Ranking position}} \\\\")
    next
  }
  if (out == "BREAK_D") {
    nongs_dyn_tex <- c(nongs_dyn_tex, "\\addlinespace",
                       "\\multicolumn{4}{l}{\\textit{Panel D: Elo rating}} \\\\")
    next
  }
  r <- nongs_dyn_df |> filter(outcome == out)
  if (nrow(r) == 0) next
  lab <- if (out %in% names(dynamic_labels)) dynamic_labels[out] else out
  nongs_dyn_tex <- c(nongs_dyn_tex,
    paste0("\\quad ", lab, " & ",
           if (!is.na(r$iv_coef)) paste0(sprintf("%.2f", r$iv_coef), add_stars(r$iv_pv)) else "--",
           " & ",
           if (!is.na(r$iv_se)) sprintf("%.2f", r$iv_se) else "--", " & ",
           if (!is.na(r$iv_pv)) sprintf("%.3f", r$iv_pv) else "--",
           " \\\\"))
}

nongs_dyn_tex <- c(nongs_dyn_tex,
  "\\midrule",
  paste0("$N$ & \\multicolumn{3}{c}{", if (nrow(nongs_dyn_df) > 0) nongs_dyn_df$n_obs[1] else "N/A", "} \\\\"),
  "Instrument & \\multicolumn{3}{c}{Peer component (LOO)} \\\\",
  "Controls & \\multicolumn{3}{c}{Pre-rank points, points$^2$, age} \\\\",
  "FE & \\multicolumn{3}{c}{Year} \\\\",
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(nongs_dyn_tex, file.path(TABLES_DIR, "table_dynamic_nongs.tex"))
message("  Saved: table_dynamic_nongs.tex")


# --- 6c. Non-GS heterogeneity ---
message("\n--- 6c. Non-GS heterogeneity ---")

est_iv2 <- est_iv2 |>
  mutate(
    rank_median_iv = median(pre_rank_pts, na.rm = TRUE),
    rank_group = ifelse(pre_rank_pts >= rank_median_iv, "High rank pts", "Low rank pts"),
    age_median_iv = median(player_age, na.rm = TRUE),
    age_group = ifelse(player_age >= age_median_iv, "Older", "Younger"),
    prior_ll_group = ifelse(had_prior_ll == 1, "Had prior LL", "No prior LL"),
    dose_group = case_when(
      got_ll == 0 ~ "Control",
      md_matches_won == 0 ~ "0 wins",
      md_matches_won == 1 ~ "1 win",
      md_matches_won >= 2 ~ "2+ wins",
      TRUE ~ "Control"
    )
  )

# Split by tour
est_iv2_atp <- est_iv2 |> filter(tour == "ATP")
est_iv2_wta <- est_iv2 |> filter(tour == "WTA")

nongs_het <- bind_rows(
  run_het_subgroup(est_iv2_atp, "rank_group", het_outcomes, "ATP"),
  run_het_subgroup(est_iv2_wta, "rank_group", het_outcomes, "WTA"),
  run_het_subgroup(est_iv2_atp, "age_group", het_outcomes, "ATP"),
  run_het_subgroup(est_iv2_wta, "age_group", het_outcomes, "WTA"),
  run_het_subgroup(est_iv2_atp, "prior_ll_group", het_outcomes, "ATP"),
  run_het_subgroup(est_iv2_wta, "prior_ll_group", het_outcomes, "WTA")
)

# LaTeX (same format as GS het table)
nongs_het_focus <- nongs_het |> filter(outcome == "rank_change_26w")

nongs_het_tex <- c(
  "\\begin{tabular}{ll cccc cccc}",
  "\\toprule",
  " & & \\multicolumn{4}{c}{ATP} & \\multicolumn{4}{c}{WTA} \\\\",
  "\\cmidrule(lr){3-6} \\cmidrule(lr){7-10}",
  "Dimension & Subgroup & $\\hat{\\beta}$ & SE & $p$ & $N$ & $\\hat{\\beta}$ & SE & $p$ & $N$ \\\\",
  "\\midrule"
)

for (gvar in c("rank_group", "age_group", "prior_ll_group")) {
  dim_lab <- dim_labels[gvar]
  sub_het <- nongs_het_focus |> filter(group_var == gvar)
  groups <- unique(sub_het$group_val)

  for (grp in groups) {
    r_atp <- sub_het |> filter(sample == "ATP", group_val == grp)
    r_wta <- sub_het |> filter(sample == "WTA", group_val == grp)

    atp_vals <- if (nrow(r_atp) > 0 && !is.na(r_atp$ols_coef)) {
      paste0(sprintf("%.1f", r_atp$ols_coef), add_stars(r_atp$ols_pv), " & ",
             sprintf("%.1f", r_atp$ols_se), " & ",
             sprintf("%.3f", r_atp$ols_pv), " & ", r_atp$n_obs)
    } else "-- & -- & -- & --"

    wta_vals <- if (nrow(r_wta) > 0 && !is.na(r_wta$ols_coef)) {
      paste0(sprintf("%.1f", r_wta$ols_coef), add_stars(r_wta$ols_pv), " & ",
             sprintf("%.1f", r_wta$ols_se), " & ",
             sprintf("%.3f", r_wta$ols_pv), " & ", r_wta$n_obs)
    } else "-- & -- & -- & --"

    nongs_het_tex <- c(nongs_het_tex,
      paste0(dim_lab, " & ", grp, " & ", atp_vals, " & ", wta_vals, " \\\\"))
    dim_lab <- ""
  }
  nongs_het_tex <- c(nongs_het_tex, "\\addlinespace")
}

nongs_het_tex <- c(nongs_het_tex,
  "\\midrule",
  "\\multicolumn{10}{l}{Outcome: Ranking change at 26 weeks. OLS (non-GS first-LL-only).} \\\\",
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(nongs_het_tex, file.path(TABLES_DIR, "table_heterogeneity_nongs.tex"))
message("  Saved: table_heterogeneity_nongs.tex")


# --- 6d. Non-GS summary statistics ---
message("\n--- 6d. Non-GS summary statistics ---")

nongs_ss_vars <- c("player_rank", "player_age", "pre_rank_pts")
nongs_ss_labels <- c("ATP/WTA Ranking", "Age", "Ranking Points")

nongs_ss_tex <- c(
  "\\begin{tabular}{l cccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{LL} & \\multicolumn{2}{c}{Control} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  "Variable & Mean & SD & Mean & SD \\\\",
  "\\midrule"
)

for (j in seq_along(nongs_ss_vars)) {
  v <- nongs_ss_vars[j]
  lab <- nongs_ss_labels[j]
  if (!v %in% names(est_iv2)) next
  t_vals <- est_iv2 |> filter(got_ll == 1) |> pull(!!sym(v))
  c_vals <- est_iv2 |> filter(got_ll == 0) |> pull(!!sym(v))
  nongs_ss_tex <- c(nongs_ss_tex,
    paste0(lab, " & ",
           sprintf("%.1f", mean(t_vals, na.rm = TRUE)), " & ",
           sprintf("%.1f", sd(t_vals, na.rm = TRUE)), " & ",
           sprintf("%.1f", mean(c_vals, na.rm = TRUE)), " & ",
           sprintf("%.1f", sd(c_vals, na.rm = TRUE)), " \\\\"))
}

nongs_ss_tex <- c(nongs_ss_tex,
  "\\midrule",
  paste0("$N$ & \\multicolumn{2}{c}{", sum(est_iv2$got_ll),
         "} & \\multicolumn{2}{c}{", sum(!est_iv2$got_ll), "} \\\\"),
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(nongs_ss_tex, file.path(TABLES_DIR, "table_summary_stats_nongs.tex"))
message("  Saved: table_summary_stats_nongs.tex")

summary_lines <- c(summary_lines,
  "## TASK 6: Non-GS Tables",
  paste0("- Non-GS IV sample: N = ", nrow(est_iv2)),
  paste0("- Non-GS immediate effects: ", nrow(nongs_imm_df), " outcomes"),
  paste0("- Non-GS dynamic outcomes: ", nrow(nongs_dyn_df), " outcomes"),
  paste0("- Non-GS heterogeneity: ", nrow(nongs_het), " rows"),
  "")


# ==============================================================================
# TASK 7: ENSURE ALL REFERENCED TABLES EXIST
# ==============================================================================
message("\n", strrep("=", 70))
message("TASK 7: VERIFY ALL TABLES EXIST")
message(strrep("=", 70))

required_tables <- c(
  "table_immediate_effects.tex",
  "table_dynamic_event_fe.tex",
  "table_heterogeneity_gs.tex",
  "table_ll_distribution_gs_v2.tex",
  "table_ll_distribution_nongs_v2.tex",
  "table_summary_stats_v2.tex",
  "table_balance_formal.tex",
  "table_verified_lottery.tex",
  "table_pooled_first_ll_results.tex",
  "table_fisher_inference.tex",
  "table_dose_final.tex",
  "table_immediate_effects_nongs.tex",
  "table_dynamic_nongs.tex",
  "table_heterogeneity_nongs.tex",
  "table_summary_stats_nongs.tex"
)

existing_tables <- list.files(TABLES_DIR, pattern = "\\.tex$")
missing_tables <- setdiff(required_tables, existing_tables)

message("  Required tables: ", length(required_tables))
message("  Existing tables: ", length(intersect(required_tables, existing_tables)))
message("  Missing tables: ", length(missing_tables))

if (length(missing_tables) > 0) {
  message("  Missing:")
  for (t in missing_tables) message("    - ", t)
}

# Check if dose_final exists, if not create a placeholder from dose results
if (!"table_dose_final.tex" %in% existing_tables) {
  message("  Creating table_dose_final.tex from dose analysis...")
  dose_res <- tryCatch(readRDS(file.path(CLEANED_DIR, "dose_analysis_results.rds")),
                        error = function(e) NULL)

  if (!is.null(dose_res)) {
    # Extract what we can
    dose_tex <- c(
      "\\begin{tabular}{l ccc}",
      "\\toprule",
      "Dose & Mean outcome & $N$ & Share \\\\",
      "\\midrule"
    )

    if (is.data.frame(dose_res)) {
      for (i in seq_len(min(nrow(dose_res), 10))) {
        r <- dose_res[i, ]
        dose_tex <- c(dose_tex,
          paste0(if ("dose" %in% names(r)) r$dose else i,
                 " & ", if ("mean_outcome" %in% names(r)) sprintf("%.2f", r$mean_outcome) else "--",
                 " & ", if ("n" %in% names(r)) r$n else "--",
                 " & -- \\\\"))
      }
    }

    dose_tex <- c(dose_tex, "\\bottomrule", "\\end{tabular}")
    writeLines(dose_tex, file.path(TABLES_DIR, "table_dose_final.tex"))
    message("  Saved: table_dose_final.tex (from dose_analysis_results)")
  } else {
    # Create from our immediate effects data
    dose_data <- gs_lottery |>
      filter(got_ll == 1) |>
      group_by(md_matches_won) |>
      summarise(
        n = n(),
        mean_rank_change_26w = mean(rank_change_26w, na.rm = TRUE),
        mean_pts_change_26w  = mean(points_change_26w, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(share = n / sum(n))

    dose_tex <- c(
      "\\begin{tabular}{l cccc}",
      "\\toprule",
      "Matches Won & $N$ & Share & Rank $\\Delta$ (26w) & Points $\\Delta$ (26w) \\\\",
      "\\midrule"
    )

    for (i in seq_len(nrow(dose_data))) {
      r <- dose_data[i, ]
      dose_tex <- c(dose_tex,
        paste0(r$md_matches_won, " & ", r$n, " & ",
               sprintf("%.2f", r$share), " & ",
               sprintf("%.1f", r$mean_rank_change_26w), " & ",
               sprintf("%.1f", r$mean_pts_change_26w), " \\\\"))
    }

    # Add controls row
    ctrl_data <- gs_lottery |>
      filter(got_ll == 0) |>
      summarise(
        n = n(),
        mean_rank_change_26w = mean(rank_change_26w, na.rm = TRUE),
        mean_pts_change_26w  = mean(points_change_26w, na.rm = TRUE)
      )

    dose_tex <- c(dose_tex, "\\midrule",
      paste0("Control & ", ctrl_data$n, " & -- & ",
             sprintf("%.1f", ctrl_data$mean_rank_change_26w), " & ",
             sprintf("%.1f", ctrl_data$mean_pts_change_26w), " \\\\"),
      "\\bottomrule",
      "\\end{tabular}"
    )

    writeLines(dose_tex, file.path(TABLES_DIR, "table_dose_final.tex"))
    message("  Saved: table_dose_final.tex (from gs_lottery dose)")
  }
}

summary_lines <- c(summary_lines,
  "## TASK 7: Table Verification",
  paste0("- Required: ", length(required_tables)),
  paste0("- Found: ", length(intersect(required_tables, existing_tables))),
  paste0("- Missing: ", length(missing_tables)),
  if (length(missing_tables) > 0) paste0("  - ", missing_tables) else "  - All present",
  "")


# ==============================================================================
# FINAL: SAVE SUMMARY
# ==============================================================================
message("\n", strrep("=", 70))
message("SAVING ANALYSIS SUMMARY")
message(strrep("=", 70))

# Add key results to summary
summary_lines <- c(summary_lines,
  "## Key Results",
  "",
  "### Immediate Effects (Pooled GS Lottery)",
  paste0("- Main-draw match win rate (treated): ",
         sprintf("%.1f%%", 100 * mean(gs_lottery$md_any_win[gs_lottery$got_ll == 1]))),
  paste0("- Mean matches played (treated): ",
         sprintf("%.2f", mean(gs_lottery$md_matches_played[gs_lottery$got_ll == 1]))),
  paste0("- Mean ranking pts at event (treated): ",
         sprintf("%.1f", mean(gs_lottery$ranking_pts_at_event[gs_lottery$got_ll == 1]))),
  "",
  "### Dynamic Effects (Pooled, Event FE)",
  ""
)

for (out in c("rank_change_26w", "points_change_26w", "n_main_draws_26w")) {
  r <- dyn_pooled |> filter(outcome == out)
  if (nrow(r) > 0) {
    summary_lines <- c(summary_lines,
      paste0("- ", out, ": beta = ", sprintf("%.2f", r$fe_coef),
             " (SE = ", sprintf("%.2f", r$fe_se),
             ", p = ", sprintf("%.3f", r$fe_pv), ")"))
  }
}

summary_lines <- c(summary_lines,
  "",
  "### Sample Sizes",
  paste0("- GS lottery (first-LL-only, FE): N = ", nrow(gs_lottery_fe)),
  paste0("- GS multi-event (censored): N = ", nrow(gs_multi_fe)),
  paste0("- Non-GS IV: N = ", nrow(est_iv2)),
  "",
  "### Tables Generated",
  paste0("- Total tables in Tables/: ", length(list.files(TABLES_DIR, pattern = "\\.tex$"))),
  ""
)

writeLines(summary_lines, file.path(OUTPUT_DIR, "skeleton_analysis_summary.md"))
message("  Saved: skeleton_analysis_summary.md")

message("\n", strrep("=", 70))
message("ALL TASKS COMPLETE")
message(strrep("=", 70))
message("  Tables dir: ", TABLES_DIR)
message("  Data dir: ", CLEANED_DIR)
message("  Summary: ", file.path(OUTPUT_DIR, "skeleton_analysis_summary.md"))
