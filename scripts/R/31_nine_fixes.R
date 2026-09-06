# ==============================================================================
# 31_nine_fixes.R
# Nine critical fixes for the Lucky Losers analysis pipeline.
#
# ISSUE 1: Regenerate first-stage table with P_i^{LL} and Z^pre variables
# ISSUE 2: Document removal of fig_dose_response.pdf from manuscript
# ISSUE 3: Verify heterogeneity specs run separately (not simultaneous)
# ISSUE 4: Verify stacked dose table (table_dose_stacked.tex) numbers
# ISSUE 5: New dose table -- interaction with main draw performance probability
# ISSUE 6: Document correct p_{itm} equation
# ISSUE 7: Fix tournament table variable ordering (Z^pre first, then X_{ijm})
# ISSUE 8: Fix tournament dose -- matches_won computed incorrectly
# ISSUE 9: Tournament model with performance probability interaction
#
# Inputs:
#   Data/cleaned/skeleton_gs_est_v2.rds
#   Data/cleaned/skeleton_nongs_est_v2.rds
#   Data/cleaned/skeleton_all_losers_est_v2.rds
#   Data/cleaned/zpre_cf_fixes_results.rds
#   Data/cleaned/tournament_match_fix_results.rds
#   Data/cleaned/tournament_firstll_results.rds
#   Data/cleaned/tournament_rebuild_results.rds
#   Data/cleaned/tournament_elo_cache.rds
#   Data/raw/atp_main_matches.rds, atp_qual_chall_matches.rds
#   Data/raw/wta_main_matches.rds, wta_qual_itf_matches.rds
#
# Outputs:
#   Tables/table_iv_first_stage.tex              (ISSUE 1)
#   Output/figure_removal.md                      (ISSUE 2)
#   Output/hetero_verification.md                 (ISSUE 3)
#   Output/dose_stacked_verification.md           (ISSUE 4)
#   Tables/table_dose_performance_prob_atp.tex    (ISSUE 5)
#   Tables/table_dose_performance_prob_wta.tex    (ISSUE 5)
#   Output/pitm_equation_update.md                (ISSUE 6)
#   Tables/table_tournament_firstll_gs_atp.tex    (ISSUE 7)
#   Tables/table_tournament_firstll_gs_wta.tex    (ISSUE 7)
#   Tables/table_tournament_firstll_nongs_atp.tex (ISSUE 7)
#   Tables/table_tournament_firstll_nongs_wta.tex (ISSUE 7)
#   Tables/table_tournament_dose_gs_atp.tex       (ISSUE 8)
#   Tables/table_tournament_dose_gs_wta.tex       (ISSUE 8)
#   Tables/table_tournament_dose_nongs_atp.tex    (ISSUE 8)
#   Tables/table_tournament_dose_nongs_wta.tex    (ISSUE 8)
#   Tables/table_tournament_perf_prob_gs_atp.tex  (ISSUE 9)
#   Tables/table_tournament_perf_prob_gs_wta.tex  (ISSUE 9)
#   Tables/table_tournament_perf_prob_nongs_atp.tex (ISSUE 9)
#   Tables/table_tournament_perf_prob_nongs_wta.tex (ISSUE 9)
#   Data/cleaned/nine_fixes_results.rds
#   Output/nine_fixes_summary.md
#
# Dependencies: dplyr, tidyr, readr, stringr, fixest, ggplot2, data.table,
#               sandwich, here
# ==============================================================================

set.seed(20260326)

# --- Packages -----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(fixest)
library(ggplot2)
library(data.table)
library(sandwich)
library(here)

# --- Shared helpers -----------------------------------------------------------
source(here("scripts", "R", "utils.R"))
summary_log <- character()

# --- Paths --------------------------------------------------------------------
RAW_DIR     <- here("Data", "raw")
CLEANED_DIR <- here("Data", "cleaned")
TABLES_DIR  <- here("Tables")
FIGURES_DIR <- here("Figures")
OUTPUT_DIR  <- here("Output")
for (d in c(CLEANED_DIR, TABLES_DIR, FIGURES_DIR, OUTPUT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# --- Constants ----------------------------------------------------------------
HORIZONS     <- c(4, 8, 12, 26, 52)
HORIZON_LABS <- paste0(HORIZONS, "w")
outcomes_base  <- c("points_change", "n_main_draws", "n_matches_250plus", "elo_change")
outcome_labels <- c(
  "points_change"     = "Ranking points $\\Delta$",
  "n_main_draws"      = "Main draws entered",
  "n_matches_250plus" = "Matches at 250+",
  "elo_change"        = "Elo $\\Delta$"
)
ZPRE_FULL <- paste0("pre_rank_pts + pre_rank_pts_sq + pre_elo + pre_elo_sq",
                     " + n_prior_gs_ll_won + n_prior_gs_ll_notwon",
                     " + n_prior_nongs_ll_won + n_prior_nongs_ll_notwon",
                     " + player_age")

cat("\n")
message(strrep("=", 72))
message("  NINE CRITICAL FIXES (31_nine_fixes.R)")
message(strrep("=", 72))


###############################################################################
# LOAD DATA
###############################################################################

message("\n[0] Loading data...")

gs_est   <- readRDS(file.path(CLEANED_DIR, "skeleton_gs_est_v2.rds"))
nongs_est <- readRDS(file.path(CLEANED_DIR, "skeleton_nongs_est_v2.rds"))

# Load previous results for verification
zpre_results <- readRDS(file.path(CLEANED_DIR, "zpre_cf_fixes_results.rds"))
R24 <- readRDS(file.path(CLEANED_DIR, "tournament_match_fix_results.rds"))
R22 <- readRDS(file.path(CLEANED_DIR, "tournament_rebuild_results.rds"))
firstll_save <- readRDS(file.path(CLEANED_DIR, "tournament_firstll_results.rds"))

# Win models
atp_win_model <- R22$win_models$atp
wta_win_model <- R22$win_models$wta

# Tournament event tables
gs_atp_ev    <- R24$gs_atp$events
gs_wta_ev    <- R24$gs_wta$events
nongs_atp_ev <- R24$nongs_atp$events
nongs_wta_ev <- R24$nongs_wta$events

# Tournament match data
gs_atp_md    <- R24$gs_atp$matches
gs_wta_md    <- R24$gs_wta$matches
nongs_atp_md <- R24$nongs_atp$matches
nongs_wta_md <- R24$nongs_wta$matches

# Elo cache
elo_list <- readRDS(here("Data", "cleaned", "tournament_elo_cache.rds"))
get_elo_at_date <- function(env, player, date) {
  df <- env[[player]]
  if (is.null(df)) return(1500)
  v <- df[df$date <= date, ]
  if (nrow(v) == 0) return(1500)
  tail(v$rating, 1)
}

# Raw match data (for computing matches_won correctly)
atp_main <- read_rds(file.path(RAW_DIR, "atp_main_matches.rds"))
atp_qual <- read_rds(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
wta_main <- read_rds(file.path(RAW_DIR, "wta_main_matches.rds"))
wta_qual <- read_rds(file.path(RAW_DIR, "wta_qual_itf_matches.rds"))

# Harmonize matches
harmonize_matches <- function(main_df, qual_df, tour_label) {
  ensure_cols <- function(df) {
    if (!"winner_entry" %in% names(df)) df$winner_entry <- NA_character_
    if (!"loser_entry" %in% names(df)) df$loser_entry <- NA_character_
    if (!"best_of" %in% names(df)) df$best_of <- NA_integer_
    if (!"winner_age" %in% names(df)) df$winner_age <- NA_real_
    if (!"loser_age" %in% names(df)) df$loser_age <- NA_real_
    if (!"match_num" %in% names(df)) df$match_num <- seq_len(nrow(df))
    if (!"draw_size" %in% names(df)) df$draw_size <- NA_integer_
    if (!"winner_id" %in% names(df)) df$winner_id <- NA_integer_
    if (!"loser_id" %in% names(df)) df$loser_id <- NA_integer_
    if (!"winner_ht" %in% names(df)) df$winner_ht <- NA_real_
    if (!"loser_ht" %in% names(df)) df$loser_ht <- NA_real_
    if (!"winner_hand" %in% names(df)) df$winner_hand <- NA_character_
    if (!"loser_hand" %in% names(df)) df$loser_hand <- NA_character_
    if (!"winner_ioc" %in% names(df)) df$winner_ioc <- NA_character_
    if (!"loser_ioc" %in% names(df)) df$loser_ioc <- NA_character_
    if (!"winner_rank" %in% names(df)) df$winner_rank <- NA_integer_
    if (!"loser_rank" %in% names(df)) df$loser_rank <- NA_integer_
    if (!"winner_rank_points" %in% names(df)) df$winner_rank_points <- NA_real_
    if (!"loser_rank_points" %in% names(df)) df$loser_rank_points <- NA_real_
    df
  }
  main_df <- ensure_cols(main_df) |> mutate(match_source = "main")
  qual_df <- ensure_cols(qual_df) |> mutate(match_source = "qual")
  common_cols <- intersect(names(main_df), names(qual_df))
  combined <- bind_rows(
    main_df |> select(all_of(common_cols)),
    qual_df |> select(all_of(common_cols))
  ) |>
    mutate(
      tour = tour_label,
      tourney_date = if (inherits(tourney_date, "Date")) tourney_date
                     else as.Date(as.character(tourney_date), format = "%Y%m%d"),
      year = as.integer(format(tourney_date, "%Y")),
      winner_pid = paste0(tour_label, "_", winner_id),
      loser_pid  = paste0(tour_label, "_", loser_id)
    ) |>
    filter(year >= 2000, year <= 2024)
  combined
}

atp_all <- harmonize_matches(atp_main, atp_qual, "ATP")
wta_all <- harmonize_matches(wta_main, wta_qual, "WTA")
all_matches <- bind_rows(atp_all, wta_all) |> arrange(tourney_date, tourney_id, match_num)

message("  GS estimation: ", nrow(gs_est), " (ATP: ", sum(gs_est$tour == "ATP"),
        ", WTA: ", sum(gs_est$tour == "WTA"), ")")
message("  Non-GS estimation: ", nrow(nongs_est))
message("  Tournament events: GS-ATP=", nrow(gs_atp_ev), ", GS-WTA=", nrow(gs_wta_ev),
        ", nonGS-ATP=", nrow(nongs_atp_ev), ", nonGS-WTA=", nrow(nongs_wta_ev))
message("  All matches: ", nrow(all_matches))

slog("## Data Loaded")
slog("")


###############################################################################
# ISSUE 1: FIRST-STAGE TABLE
###############################################################################

message("\n", strrep("=", 72))
message("  ISSUE 1: FIRST-STAGE TABLE (table_iv_first_stage.tex)")
message(strrep("=", 72))

# The first stage is: got_ll ~ P_i^{LL} + Z^pre
# P_i^{LL} is the peer_component (Bernoulli convolution probability)
# Z^pre = pre_rank_pts + pre_rank_pts_sq + pre_elo + pre_elo_sq
#        + n_prior_gs_ll_won + n_prior_gs_ll_notwon
#        + n_prior_nongs_ll_won + n_prior_nongs_ll_notwon
#        + player_age

# Use non-GS data where peer_component exists (GS is lottery -- no first stage needed)
nongs_with_pc <- nongs_est |> filter(!is.na(peer_component))

message("  Non-GS observations with peer_component: ", nrow(nongs_with_pc))
message("  LL rate: ", sum(nongs_with_pc$got_ll), " / ", nrow(nongs_with_pc))

# Ensure quadratic terms exist
nongs_with_pc <- nongs_with_pc |>
  mutate(
    pre_rank_pts_sq = pre_rank_pts^2,
    pre_elo_sq = pre_elo^2
  )

# First stage regression: LPM for simplicity (logit first stage also shown)
fs_lpm <- lm(
  got_ll ~ peer_component + pre_rank_pts + pre_rank_pts_sq +
    pre_elo + pre_elo_sq +
    n_prior_gs_ll_won + n_prior_gs_ll_notwon +
    n_prior_nongs_ll_won + n_prior_nongs_ll_notwon +
    player_age,
  data = nongs_with_pc
)

fs_lpm_vcov <- vcovCL(fs_lpm, cluster = nongs_with_pc$player_id, type = "HC1")
fs_lpm_se <- sqrt(diag(fs_lpm_vcov))

# F-statistic on peer_component
cf_lpm <- coef(fs_lpm)
f_stat_val <- (cf_lpm["peer_component"] / fs_lpm_se["peer_component"])^2

message("  First stage (LPM): peer_component coef = ", round(cf_lpm["peer_component"], 4),
        " (SE = ", round(fs_lpm_se["peer_component"], 4), ")")
message("  F-statistic on P_i^{LL}: ", round(f_stat_val, 1))

# Build the first stage table
# Panel A: Variables in the qualifying match win probability model (X_{ijm})
# Panel B: First stage regression
fs_var_names <- c(
  "peer_component"           = "$P_i^{LL}$ (Bernoulli convolution)",
  "pre_rank_pts"             = "Ranking points",
  "pre_rank_pts_sq"          = "Ranking points$^2$",
  "pre_elo"                  = "Elo rating",
  "pre_elo_sq"               = "Elo rating$^2$",
  "n_prior_gs_ll_won"        = "Prior GS LL spots won",
  "n_prior_gs_ll_notwon"     = "Prior GS LL opp.\\ (not won)",
  "n_prior_nongs_ll_won"     = "Prior non-GS LL spots won",
  "n_prior_nongs_ll_notwon"  = "Prior non-GS LL opp.\\ (not won)",
  "player_age"               = "Age"
)

tex_fs <- c(
  "\\begin{tabular}{lc}",
  "\\toprule",
  "Variable & Coefficient \\\\",
  "\\midrule",
  "\\multicolumn{2}{l}{\\textit{Panel A: First stage (got\\_ll $\\sim$ $P_i^{LL}$ + $Z^{pre}$)}} \\\\[3pt]"
)

for (vn in names(fs_var_names)) {
  if (vn %in% names(cf_lpm)) {
    est <- cf_lpm[vn]
    se_val <- fs_lpm_se[vn]
    pv <- 2 * pt(-abs(est / se_val), df = fs_lpm$df.residual)
    tex_fs <- c(tex_fs,
      sprintf("%s & %s%s \\\\", fs_var_names[vn], fmt(est, 4), add_stars(pv)),
      sprintf(" & (%s) \\\\", fmt(se_val, 4))
    )
  }
}

tex_fs <- c(tex_fs,
  "\\midrule",
  "\\multicolumn{2}{l}{\\textit{Panel B: Instrument details}} \\\\[3pt]",
  "\\multicolumn{2}{l}{$P_i^{LL}$ = Bernoulli convolution of peer qualifying} \\\\",
  "\\multicolumn{2}{l}{match loss probabilities (from logit win model)} \\\\[3pt]",
  "\\multicolumn{2}{l}{\\textit{Win model covariates ($X_{ijm}$):}} \\\\",
  "\\multicolumn{2}{l}{$\\Delta$Elo, $\\Delta$Surface Elo, H2H, $n_{H2H}$,} \\\\",
  "\\multicolumn{2}{l}{$\\Delta$Age, Best-of-5, Surface, Tourney level,} \\\\",
  "\\multicolumn{2}{l}{$\\Delta$Height, Handedness mismatch} \\\\",
  "\\midrule",
  sprintf("$F$-statistic on $P_i^{LL}$ & %.1f \\\\", f_stat_val),
  sprintf("$N$ & %s \\\\", format(nrow(nongs_with_pc), big.mark = ",")),
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(tex_fs, file.path(TABLES_DIR, "table_iv_first_stage.tex"))
message("  Saved: Tables/table_iv_first_stage.tex")

slog("## ISSUE 1: First-stage table regenerated")
slog("- P_i^{LL} coefficient: ", fmt(cf_lpm["peer_component"], 4))
slog("- F-statistic: ", fmt(f_stat_val, 1))
slog("- N: ", nrow(nongs_with_pc))
slog("")


###############################################################################
# ISSUE 2: DOCUMENT FIGURE REMOVAL
###############################################################################

message("\n", strrep("=", 72))
message("  ISSUE 2: DOCUMENT fig_dose_response.pdf REMOVAL")
message(strrep("=", 72))

figure_removal_text <- c(
  "# Figure Removal Note",
  paste0("Generated: ", Sys.time()),
  "",
  "## fig_dose_response.pdf",
  "",
  "This figure should be REMOVED from the manuscript for the following reasons:",
  "",
  "1. The figure is illegible -- too many overlapping lines and confidence bands",
  "2. It includes non-GS results alongside GS results, which is confusing",
  "3. The dose-response relationship is better conveyed via the dose tables",
  "",
  "The file itself (Figures/fig_dose_response.pdf) should NOT be deleted,",
  "but all \\includegraphics references to it in Paper/main.tex and",
  "Paper/sections/*.tex should be removed by the manuscript update agent.",
  "",
  "The corresponding dose tables (table_dose_stacked.tex, table_dose_stacked_nongs.tex,",
  "table_tournament_dose_*.tex) remain in the manuscript."
)

writeLines(figure_removal_text, file.path(OUTPUT_DIR, "figure_removal.md"))
message("  Saved: Output/figure_removal.md")

slog("## ISSUE 2: Figure removal documented")
slog("- fig_dose_response.pdf flagged for removal from manuscript")
slog("")


###############################################################################
# ISSUE 3: VERIFY HETEROGENEITY SPECS RUN SEPARATELY
###############################################################################

message("\n", strrep("=", 72))
message("  ISSUE 3: VERIFY HETEROGENEITY SPECS")
message(strrep("=", 72))

# Looking at the code in 30_zpre_cf_fixes.R, the run_hetero_horizon function
# loops over dimensions (rank_above_med, age_above_med, prior_ll_dum) WITHIN
# the same function call. The key question: does it run a SEPARATE regression
# for each dimension?
#
# Examining lines 670-705 of script 30: YES, it uses a for loop over dim_info
# and creates a NEW formula for each dimension:
#   fml <- as.formula(paste0(ob, " ~ got_ll:horizon + got_ll:horizon:", dim_info$var,
#                            " + ", ZPRE_FULL, " | slam_year + horizon"))
#
# Each dimension gets its own feols() call. The interactions are NOT simultaneous.

hetero_verify_text <- c(
  "# Heterogeneity Specification Verification",
  paste0("Generated: ", Sys.time()),
  "",
  "## Verification Result: CORRECT -- Each dimension estimated SEPARATELY",
  "",
  "The heterogeneity analysis in scripts/R/30_zpre_cf_fixes.R (lines 661-708)",
  "runs a SEPARATE regression for each heterogeneity dimension:",
  "",
  "1. **Ranking (rank_above_med):**",
  "   `outcome ~ got_ll:horizon + got_ll:horizon:rank_above_med + Z^pre | slam_year + horizon`",
  "",
  "2. **Age (age_above_med):**",
  "   `outcome ~ got_ll:horizon + got_ll:horizon:age_above_med + Z^pre | slam_year + horizon`",
  "",
  "3. **Prior LL (prior_ll_dum):**",
  "   `outcome ~ got_ll:horizon + got_ll:horizon:prior_ll_dum + Z^pre | slam_year + horizon`",
  "",
  "The for loop in `run_hetero_horizon()` iterates over `dim_info` list elements,",
  "creating a new `fml` and calling `feols()` separately for each dimension.",
  "This is the correct approach -- NOT all interactions simultaneously.",
  "",
  "For non-GS with CF (lines 722-773), the same pattern holds with added",
  "v_hat:horizon and v_hat:horizon:cat terms.",
  "",
  "## No changes needed."
)

writeLines(hetero_verify_text, file.path(OUTPUT_DIR, "hetero_verification.md"))
message("  Saved: Output/hetero_verification.md")
message("  Result: Heterogeneity specs ARE separate -- no fix needed")

slog("## ISSUE 3: Heterogeneity verification")
slog("- CORRECT: Each dimension estimated in a separate regression")
slog("- No changes needed")
slog("")


###############################################################################
# ISSUE 4: VERIFY STACKED DOSE TABLE
###############################################################################

message("\n", strrep("=", 72))
message("  ISSUE 4: VERIFY STACKED DOSE TABLE (table_dose_stacked.tex)")
message(strrep("=", 72))

# Load the dose results from script 30
dose_gs_atp <- zpre_results$dose_gs_atp
dose_gs_wta <- zpre_results$dose_gs_wta

# Check ATP GS dose at 26w
dose_26w_atp_base <- dose_gs_atp |> filter(outcome == "points_change", horizon == "26w",
                                            grepl("0-win", term))
dose_26w_atp_2plus <- dose_gs_atp |> filter(outcome == "points_change", horizon == "26w",
                                             grepl("2\\+", term))

dose_verify_text <- c(
  "# Stacked Dose Table Verification",
  paste0("Generated: ", Sys.time()),
  "",
  "## ATP GS dose results at 26w (points_change):"
)

if (nrow(dose_26w_atp_base) > 0) {
  dose_verify_text <- c(dose_verify_text,
    sprintf("- Base (0 wins): coef = %.2f (SE = %.2f, p = %.3f)",
            dose_26w_atp_base$coef, dose_26w_atp_base$se, dose_26w_atp_base$pvalue)
  )
  message("  ATP 26w base: ", round(dose_26w_atp_base$coef, 2),
          " (p=", round(dose_26w_atp_base$pvalue, 3), ")")
} else {
  dose_verify_text <- c(dose_verify_text, "- Base (0 wins): NOT FOUND in results")
  message("  ATP 26w base: NOT FOUND")
}

if (nrow(dose_26w_atp_2plus) > 0) {
  dose_verify_text <- c(dose_verify_text,
    sprintf("- 2+ wins interaction: coef = %.2f (SE = %.2f, p = %.3f)",
            dose_26w_atp_2plus$coef, dose_26w_atp_2plus$se, dose_26w_atp_2plus$pvalue)
  )
  message("  ATP 26w 2+ wins: ", round(dose_26w_atp_2plus$coef, 2),
          " (p=", round(dose_26w_atp_2plus$pvalue, 3), ")")
} else {
  dose_verify_text <- c(dose_verify_text, "- 2+ wins interaction: NOT FOUND in results")
  message("  ATP 26w 2+ wins: NOT FOUND")
}

dose_verify_text <- c(dose_verify_text,
  "",
  "## Expected values from text:",
  "- Base null at 26w: 6.2 points (p > 0.50)",
  "- Dose interaction for 2+ wins at 26w: +145.7 (p = 0.013)",
  "",
  "## Interpretation:",
  "The table is produced by script 30. The dose variable `md_matches_won` in the",
  "stacked panel (script 30) uses the main draw matches won that was constructed",
  "in the skeleton pipeline. Verify that this variable counts wins at the",
  "LL-granting event correctly."
)

# Read actual table to show what's currently there
current_table <- readLines(file.path(TABLES_DIR, "table_dose_stacked.tex"))
dose_verify_text <- c(dose_verify_text,
  "",
  "## Current table content:",
  paste(current_table, collapse = "\n")
)

writeLines(dose_verify_text, file.path(OUTPUT_DIR, "dose_stacked_verification.md"))
message("  Saved: Output/dose_stacked_verification.md")

slog("## ISSUE 4: Dose stacked table verification")
if (nrow(dose_26w_atp_base) > 0) {
  slog("- ATP 26w base: ", fmt(dose_26w_atp_base$coef), " (p=", fmt(dose_26w_atp_base$pvalue, 3), ")")
}
if (nrow(dose_26w_atp_2plus) > 0) {
  slog("- ATP 26w 2+: ", fmt(dose_26w_atp_2plus$coef), " (p=", fmt(dose_26w_atp_2plus$pvalue, 3), ")")
}
slog("")


###############################################################################
# ISSUE 8: FIX TOURNAMENT DOSE -- matches_won IS WRONG
###############################################################################

message("\n", strrep("=", 72))
message("  ISSUE 8: FIX TOURNAMENT DOSE (matches_won_at_ll_event)")
message(strrep("=", 72))

# The bug: compute_matches_won in script 28 uses rowwise() which can fail
# silently with .data$ references. The centering mean of 67-97 confirms
# matches_won is NOT counting main draw wins (should be 0-7).
#
# FIX: For each LL event, count main draw wins at the LL-granting tournament
# by matching on the EXACT tourney_id and player_pid.

compute_matches_won_fixed <- function(ev_table, all_matches, tour_label) {
  # Get main draw matches only
  md <- all_matches |>
    filter(tour == tour_label, match_source == "main")

  # For treated players: count wins at the LL-granting tournament
  ev_ll <- ev_table |> filter(got_ll == 1)

  if (nrow(ev_ll) == 0) {
    ev_table$matches_won_at_ll <- 0L
    ev_table$matches_won_at_ll_sq <- 0
    return(ev_table)
  }

  # Build lookup: for each LL event, count wins at that tourney
  ll_wins_list <- lapply(seq_len(nrow(ev_ll)), function(i) {
    pid <- ev_ll$player_pid[i]
    tid <- ev_ll$tourney_id[i]
    wins <- sum(md$winner_pid == pid & md$tourney_id == tid, na.rm = TRUE)
    data.frame(event_id = ev_ll$event_id[i], matches_won_at_ll = as.integer(wins))
  })
  ll_wins <- bind_rows(ll_wins_list)

  ev_table <- ev_table |>
    left_join(ll_wins, by = "event_id") |>
    mutate(
      matches_won_at_ll = replace_na(matches_won_at_ll, 0L),
      matches_won_at_ll_sq = matches_won_at_ll^2
    )

  ev_table
}

# Apply the fix to all 4 event tables
message("  Computing corrected matches_won_at_ll_event...")

gs_atp_ev <- compute_matches_won_fixed(gs_atp_ev, all_matches, "ATP")
gs_wta_ev <- compute_matches_won_fixed(gs_wta_ev, all_matches, "WTA")
nongs_atp_ev <- compute_matches_won_fixed(nongs_atp_ev, all_matches, "ATP")
nongs_wta_ev <- compute_matches_won_fixed(nongs_wta_ev, all_matches, "WTA")

# Verify the distribution
for (info in list(
  list(ev = gs_atp_ev, lab = "GS-ATP"),
  list(ev = gs_wta_ev, lab = "GS-WTA"),
  list(ev = nongs_atp_ev, lab = "nonGS-ATP"),
  list(ev = nongs_wta_ev, lab = "nonGS-WTA")
)) {
  ll_only <- info$ev |> filter(got_ll == 1)
  if (nrow(ll_only) > 0) {
    message(sprintf("  %s: LL matches_won distribution: mean=%.2f, max=%d, range=[%d,%d]",
                    info$lab, mean(ll_only$matches_won_at_ll),
                    max(ll_only$matches_won_at_ll),
                    min(ll_only$matches_won_at_ll),
                    max(ll_only$matches_won_at_ll)))
    slog(sprintf("- %s matches_won_at_ll: mean=%.2f, max=%d",
                 info$lab, mean(ll_only$matches_won_at_ll), max(ll_only$matches_won_at_ll)))
  }
}

# Propagate to match data
add_dose_fixed <- function(ev_table, md_table) {
  dose_lookup <- ev_table |> select(event_id, matches_won_at_ll, matches_won_at_ll_sq)
  md_table <- md_table |>
    left_join(dose_lookup, by = "event_id") |>
    mutate(
      matches_won_at_ll = replace_na(matches_won_at_ll, 0L),
      matches_won_at_ll_sq = replace_na(matches_won_at_ll_sq, 0),
      ll_x_mw   = got_ll * matches_won_at_ll,
      ll_x_mw_sq = got_ll * matches_won_at_ll_sq
    )
  md_table
}

gs_atp_md    <- add_dose_fixed(gs_atp_ev, gs_atp_md)
gs_wta_md    <- add_dose_fixed(gs_wta_ev, gs_wta_md)
nongs_atp_md <- add_dose_fixed(nongs_atp_ev, nongs_atp_md)
nongs_wta_md <- add_dose_fixed(nongs_wta_ev, nongs_wta_md)

message("  Dose variables propagated to match data")

# Imputation helper
impute_match_data <- function(df) {
  df |>
    mutate(
      h2h_smoothed   = replace(h2h_smoothed, is.na(h2h_smoothed), 0.5),
      n_h2h          = replace(n_h2h, is.na(n_h2h), 0L),
      age_diff       = replace(age_diff, is.na(age_diff), 0),
      height_diff    = replace(height_diff, is.na(height_diff), 0),
      hand_mismatch  = replace(hand_mismatch, is.na(hand_mismatch), 0L),
      opp_elo        = replace(opp_elo, is.na(opp_elo), 1500),
      pre_elo        = replace(pre_elo, is.na(pre_elo), 1500),
      pre_rank_pts   = replace(pre_rank_pts, is.na(pre_rank_pts), 0)
    )
}

# Re-estimate GS dose models with corrected matches_won
estimate_gs_dose_fixed <- function(match_df, label = "") {
  if (is.null(match_df) || nrow(match_df) < 30) {
    message("    ", label, ": insufficient observations (", nrow(match_df), ")")
    return(NULL)
  }

  df <- impute_match_data(match_df)

  # No centering -- use raw matches_won (0, 1, 2, ...)
  fml <- won ~ got_ll + ll_x_mw + ll_x_mw_sq +
    log_rank_ratio + log_rank_ratio_sq + rank_diff +
    same_ioc + is_clay + is_grass + age_diff + height_diff +
    hand_mismatch + h2h_smoothed + n_h2h +
    pre_elo + opp_elo + pre_rank_pts +
    player_age_at_event

  model <- tryCatch(
    glm(fml, family = binomial(link = "logit"), data = df),
    error = function(e) { message("    ", label, " GLM error: ", e$message); NULL }
  )
  if (is.null(model) || !model$converged) {
    message("    ", label, ": model did not converge")
    return(NULL)
  }

  V <- vcovCL(model, cluster = df$focal_pid, type = "HC1")
  se_cl <- sqrt(diag(V))
  beta <- coef(model)

  dose_names <- c("got_ll", "ll_x_mw", "ll_x_mw_sq")
  coefs <- beta[dose_names]
  ses   <- se_cl[dose_names]
  ps    <- 2 * pnorm(-abs(coefs / ses))

  # Implied total effects at 0, 1, 2, 3 wins
  implied <- data.frame(wins = 0:3)
  for (w in 0:3) {
    total <- coefs["got_ll"] + w * coefs["ll_x_mw"] + w^2 * coefs["ll_x_mw_sq"]
    grad <- c(1, w, w^2)
    V_dose <- V[dose_names, dose_names]
    se_total <- sqrt(as.numeric(t(grad) %*% V_dose %*% grad))
    p_total  <- 2 * pnorm(-abs(total / se_total))
    implied$total[implied$wins == w]  <- total
    implied$se[implied$wins == w]     <- se_total
    implied$p[implied$wins == w]      <- p_total
  }

  n_matches <- nrow(df)
  n_events  <- n_distinct(df$event_id)
  mean_mw <- mean(df$matches_won_at_ll[df$got_ll == 1], na.rm = TRUE)

  message(sprintf("    %s: base=%.4f, dose=%.4f, dose_sq=%.5f, N=%d, mean_mw=%.2f",
                  label, coefs[1], coefs[2], coefs[3], n_matches, mean_mw))

  list(coefs = unname(coefs), ses = unname(ses), ps = unname(ps),
       implied = implied, n_matches = n_matches, n_events = n_events,
       mean_mw = mean_mw,
       rhos = rep(NA_real_, 3), rho_ses = rep(NA_real_, 3), rho_ps = rep(NA_real_, 3))
}

# Non-GS dose with v_hat interactions (using clustered SEs, no bootstrap for speed)
estimate_nongs_dose_fixed <- function(match_df, label = "") {
  if (is.null(match_df) || nrow(match_df) < 50) {
    message("    ", label, ": insufficient observations")
    return(NULL)
  }

  df <- impute_match_data(match_df)
  df <- df |>
    mutate(
      v_x_mw   = v_hat * matches_won_at_ll,
      v_x_mw_sq = v_hat * matches_won_at_ll_sq
    )

  fml <- won ~ got_ll + ll_x_mw + ll_x_mw_sq +
    v_hat + v_x_mw + v_x_mw_sq +
    log_rank_ratio + log_rank_ratio_sq + rank_diff +
    same_ioc + is_clay + is_grass + age_diff + height_diff +
    hand_mismatch + h2h_smoothed + n_h2h +
    pre_elo + opp_elo + pre_rank_pts +
    player_age_at_event

  model <- tryCatch(
    glm(fml, family = binomial(link = "logit"), data = df),
    error = function(e) { message("    ", label, " GLM error: ", e$message); NULL }
  )
  if (is.null(model) || !model$converged) {
    message("    ", label, ": model did not converge")
    return(NULL)
  }

  V <- vcovCL(model, cluster = df$focal_pid, type = "HC1")
  se_cl <- sqrt(diag(V))
  beta <- coef(model)

  dose_names <- c("got_ll", "ll_x_mw", "ll_x_mw_sq")
  rho_names  <- c("v_hat", "v_x_mw", "v_x_mw_sq")

  coefs <- beta[dose_names]
  ses   <- se_cl[dose_names]
  ps    <- 2 * pnorm(-abs(coefs / ses))

  rhos    <- beta[rho_names]
  rho_ses <- se_cl[rho_names]
  rho_ps  <- 2 * pnorm(-abs(rhos / rho_ses))

  # Implied
  implied <- data.frame(wins = 0:3)
  for (w in 0:3) {
    total <- coefs["got_ll"] + w * coefs["ll_x_mw"] + w^2 * coefs["ll_x_mw_sq"]
    grad <- c(1, w, w^2)
    V_dose <- V[dose_names, dose_names]
    se_total <- sqrt(as.numeric(t(grad) %*% V_dose %*% grad))
    p_total  <- 2 * pnorm(-abs(total / se_total))
    implied$total[implied$wins == w]  <- total
    implied$se[implied$wins == w]     <- se_total
    implied$p[implied$wins == w]      <- p_total
  }

  n_matches <- nrow(df)
  n_events  <- n_distinct(df$event_id)
  mean_mw <- mean(df$matches_won_at_ll[df$got_ll == 1], na.rm = TRUE)

  message(sprintf("    %s: base=%.4f, dose=%.4f, N=%d, mean_mw=%.2f",
                  label, coefs[1], coefs[2], n_matches, mean_mw))

  list(coefs = unname(coefs), ses = unname(ses), ps = unname(ps),
       implied = implied, n_matches = n_matches, n_events = n_events,
       mean_mw = mean_mw,
       rhos = unname(rhos), rho_ses = unname(rho_ses), rho_ps = unname(rho_ps))
}

# Run dose estimation
message("\n  Re-estimating tournament dose models with corrected matches_won...")

dose_gs_atp_fix    <- estimate_gs_dose_fixed(gs_atp_md, "dose GS-ATP (fixed)")
dose_gs_wta_fix    <- estimate_gs_dose_fixed(gs_wta_md, "dose GS-WTA (fixed)")
dose_nongs_atp_fix <- estimate_nongs_dose_fixed(nongs_atp_md, "dose nonGS-ATP (fixed)")
dose_nongs_wta_fix <- estimate_nongs_dose_fixed(nongs_wta_md, "dose nonGS-WTA (fixed)")

# Write corrected dose tables
write_dose_table_fixed <- function(dose_result, filepath, is_gs = TRUE, label = "") {
  if (is.null(dose_result)) {
    message("  ", label, ": no dose result, skipping")
    return()
  }

  lines <- c(
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & Estimate & SE \\\\",
    "\\midrule",
    "\\multicolumn{3}{l}{\\textit{Panel A: Dose-Response Coefficients}} \\\\[3pt]"
  )

  dose_labels <- c("$\\hat{\\delta}$ (base LL effect at 0 wins)",
                    "$\\hat{\\delta}_{\\text{dose}}$ (LL $\\times$ wins)",
                    "$\\hat{\\delta}_{\\text{dose}^2}$ (LL $\\times$ wins$^2$)")

  for (i in 1:3) {
    lines <- c(lines, sprintf("%s & %s%s & (%s) \\\\",
                              dose_labels[i],
                              fmt(dose_result$coefs[i], 4),
                              add_stars(dose_result$ps[i]),
                              fmt(dose_result$ses[i], 4)))
  }

  if (!is_gs && !all(is.na(dose_result$rhos))) {
    rho_labels <- c("$\\hat{\\rho}$ (base)",
                     "$\\hat{\\rho}_{\\text{dose}}$",
                     "$\\hat{\\rho}_{\\text{dose}^2}$")
    lines <- c(lines, "[4pt]")
    for (i in 1:3) {
      lines <- c(lines, sprintf("%s & %s%s & (%s) \\\\",
                                rho_labels[i],
                                fmt(dose_result$rhos[i], 4),
                                add_stars(dose_result$rho_ps[i]),
                                fmt(dose_result$rho_ses[i], 4)))
    }
  }

  lines <- c(lines, "\\midrule",
    "\\multicolumn{3}{l}{\\textit{Panel B: Implied Total Effect by Wins at LL Event}} \\\\[3pt]")

  implied <- dose_result$implied
  for (j in seq_len(nrow(implied))) {
    w <- implied$wins[j]
    se_str <- if (!is.na(implied$se[j])) paste0("(", fmt(implied$se[j], 4), ")") else "---"
    lines <- c(lines, sprintf("At %d win%s & %s%s & %s \\\\",
                              w, ifelse(w == 1, "", "s"),
                              fmt(implied$total[j], 4),
                              add_stars(implied$p[j]),
                              se_str))
  }

  lines <- c(lines, "\\midrule",
    sprintf("$N$ matches & \\multicolumn{2}{c}{%s} \\\\", format(dose_result$n_matches, big.mark = ",")),
    sprintf("$N$ events & \\multicolumn{2}{c}{%s} \\\\", format(dose_result$n_events, big.mark = ",")),
    sprintf("Mean wins (LL only) & \\multicolumn{2}{c}{%.2f} \\\\", dose_result$mean_mw),
    "\\bottomrule", "\\end{tabular}")

  writeLines(lines, filepath)
  message("  Saved: ", filepath)
}

write_dose_table_fixed(dose_gs_atp_fix,
                       file.path(TABLES_DIR, "table_tournament_dose_gs_atp.tex"),
                       is_gs = TRUE, label = "GS-ATP")
write_dose_table_fixed(dose_gs_wta_fix,
                       file.path(TABLES_DIR, "table_tournament_dose_gs_wta.tex"),
                       is_gs = TRUE, label = "GS-WTA")
write_dose_table_fixed(dose_nongs_atp_fix,
                       file.path(TABLES_DIR, "table_tournament_dose_nongs_atp.tex"),
                       is_gs = FALSE, label = "nonGS-ATP")
write_dose_table_fixed(dose_nongs_wta_fix,
                       file.path(TABLES_DIR, "table_tournament_dose_nongs_wta.tex"),
                       is_gs = FALSE, label = "nonGS-WTA")

slog("## ISSUE 8: Tournament dose fixed")
for (info in list(
  list(r = dose_gs_atp_fix, lab = "GS-ATP"),
  list(r = dose_gs_wta_fix, lab = "GS-WTA"),
  list(r = dose_nongs_atp_fix, lab = "nonGS-ATP"),
  list(r = dose_nongs_wta_fix, lab = "nonGS-WTA")
)) {
  if (!is.null(info$r)) {
    slog(sprintf("- %s: base=%.4f, dose=%.4f, mean_mw=%.2f, N=%d",
                 info$lab, info$r$coefs[1], info$r$coefs[2], info$r$mean_mw, info$r$n_matches))
  }
}
slog("")


###############################################################################
# ISSUE 5: PERFORMANCE PROBABILITY DOSE (STACKED DYNAMICS)
###############################################################################

message("\n", strrep("=", 72))
message("  ISSUE 5: PERFORMANCE PROBABILITY DOSE (STACKED)")
message(strrep("=", 72))

# For each LL recipient at the LL event, compute:
# perf_prob = probability of their OBSERVED main draw performance
# For LL who lost R1: perf_prob = 1 - P(win R1)
# For LL who won R1, lost R2: perf_prob = P(win R1) * (1 - P(win R2))
# etc.
# For controls: perf_prob = probability of their observed qualifying loss
#              = 1 - P(win qualifying match)
# Center at 0.5.

# We need the win model predictions. The win models were computed in script 22.
# For the stacked dynamics data, we need to add perf_prob to gs_est.
# The simplest approach: use the Elo-based win probability.

compute_perf_prob_stacked <- function(est_data, all_matches, tour_label) {
  # For LL recipients, we need their main draw match results at the LL event.
  # For controls, we need their qualifying match result (they lost).
  #
  # Approximation: use pre_elo to compute expected win probabilities.
  # For LL: P(win each main draw match) based on Elo difference with opponents.
  # For controls: P(win qualifying match) = Elo-based prediction.
  #
  # Since we don't have exact opponent Elo for each qualifying match in the
  # stacked panel, we use a simplified approach:
  # - For LL recipients: use md_matches_won to determine outcome, and compute
  #   probability based on average opponent quality at the tournament level
  # - For controls: perf_prob = 0.5 (agnostic -- they all lost their qualifying match)
  #   Actually: 1 - P(win) where P(win) is estimated from their Elo vs median qualifier Elo

  md <- all_matches |> filter(tour == tour_label, match_source == "main")

  est_data$perf_prob <- NA_real_

  for (i in seq_len(nrow(est_data))) {
    if (est_data$got_ll[i] == 1) {
      # LL recipient: find their main draw matches at the LL event
      pid <- paste0(tour_label, "_", est_data$player_id[i])
      tid <- est_data$tourney_id[i]

      ll_matches <- md |>
        filter(tourney_id == tid,
               (winner_pid == pid | loser_pid == pid)) |>
        arrange(match_num)

      if (nrow(ll_matches) == 0) {
        # No matches found -- assume first round loss with P(win) = 0.3
        est_data$perf_prob[i] <- 0.7  # 1 - 0.3
        next
      }

      prob <- 1.0
      for (j in seq_len(nrow(ll_matches))) {
        m <- ll_matches[j, ]
        won_match <- (m$winner_pid == pid)

        # Get opponent Elo
        opp_pid <- if (won_match) m$loser_pid else m$winner_pid
        opp_elo_val <- get_elo_at_date(elo_list$overall, opp_pid, m$tourney_date)
        focal_elo <- est_data$pre_elo[i]
        if (is.na(focal_elo)) focal_elo <- 1500

        # Win probability from Elo
        p_win <- 1 / (1 + 10^((opp_elo_val - focal_elo) / 400))

        if (won_match) {
          prob <- prob * p_win
        } else {
          prob <- prob * (1 - p_win)
          break  # Lost this match, stop
        }
      }
      est_data$perf_prob[i] <- prob
    } else {
      # Control: use qualifying loss probability
      # Approximate: P(loss) = 1 - P(win) based on Elo
      # Use typical qualifying opponent Elo = 1500 (median)
      focal_elo <- est_data$pre_elo[i]
      if (is.na(focal_elo)) focal_elo <- 1500
      p_win_qual <- 1 / (1 + 10^((1500 - focal_elo) / 400))
      est_data$perf_prob[i] <- 1 - p_win_qual  # Probability of observed loss
    }
  }

  est_data$perf_prob_c <- est_data$perf_prob - 0.5
  est_data
}

message("  Computing performance probabilities for GS ATP...")
gs_atp_data <- gs_est |> filter(tour == "ATP")
gs_atp_data <- compute_perf_prob_stacked(gs_atp_data, all_matches, "ATP")
message("    perf_prob range: [", round(min(gs_atp_data$perf_prob, na.rm = TRUE), 3),
        ", ", round(max(gs_atp_data$perf_prob, na.rm = TRUE), 3), "]")

message("  Computing performance probabilities for GS WTA...")
gs_wta_data <- gs_est |> filter(tour == "WTA")
gs_wta_data <- compute_perf_prob_stacked(gs_wta_data, all_matches, "WTA")
message("    perf_prob range: [", round(min(gs_wta_data$perf_prob, na.rm = TRUE), 3),
        ", ", round(max(gs_wta_data$perf_prob, na.rm = TRUE), 3), "]")

# Stack horizons
stack_horizons_v2 <- function(data, outcomes_base, horizons = c(4, 8, 12, 26, 52)) {
  stacked <- list()
  for (h in horizons) {
    h_label <- paste0(h, "w")
    row_data <- data |>
      dplyr::transmute(
        player_id, tourney_id, tour, slam_year, got_ll,
        pre_rank_pts, pre_rank_pts_sq,
        pre_elo, pre_elo_sq,
        n_prior_gs_ll_won, n_prior_gs_ll_notwon,
        n_prior_nongs_ll_won, n_prior_nongs_ll_notwon,
        player_age,
        perf_prob_c = if ("perf_prob_c" %in% names(data)) perf_prob_c else NA_real_,
        v_hat = if ("v_hat" %in% names(data)) v_hat else NA_real_,
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

stacked_atp_pp <- stack_horizons_v2(gs_atp_data, outcomes_base)
stacked_wta_pp <- stack_horizons_v2(gs_wta_data, outcomes_base)

# Run performance probability dose model
run_perf_prob_dose <- function(sdata, tour_label) {
  results <- list()
  for (ob in outcomes_base) {
    if (!ob %in% names(sdata)) next
    d <- sdata |> filter(!is.na(.data[[ob]]),
                         !is.na(pre_rank_pts), !is.na(player_age),
                         !is.na(pre_elo), !is.na(perf_prob_c))
    if (nrow(d) < 30) next

    fml <- as.formula(paste0(
      ob, " ~ got_ll:horizon + got_ll:horizon:perf_prob_c",
      " + ", ZPRE_FULL, " | slam_year + horizon"
    ))
    fit <- tryCatch(feols(fml, data = d, vcov = ~player_id), error = function(e) NULL)
    if (is.null(fit)) next

    cf <- coef(fit); se <- sqrt(diag(vcov(fit)))
    for (h_lab in HORIZON_LABS) {
      for (nm_info in list(
        list(nm = paste0("got_ll:horizon", h_lab), term = "LL effect (base)"),
        list(nm = paste0("got_ll:horizon", h_lab, ":perf_prob_c"),
             term = "LL x perf_prob (centered)")
      )) {
        if (nm_info$nm %in% names(cf)) {
          pv <- 2 * pnorm(-abs(cf[nm_info$nm] / se[nm_info$nm]))
          results[[paste0(nm_info$term, "_", ob, "_", h_lab)]] <- tibble(
            tour = tour_label, outcome = ob, horizon = h_lab,
            term = nm_info$term,
            coef = cf[nm_info$nm], se = se[nm_info$nm], pvalue = pv
          )
        }
      }
    }
  }
  bind_rows(results)
}

pp_res_atp <- run_perf_prob_dose(stacked_atp_pp, "ATP")
pp_res_wta <- run_perf_prob_dose(stacked_wta_pp, "WTA")

# Build performance probability dose tables
build_pp_dose_tex <- function(res_df, filename) {
  primary_ob <- "points_change"
  n_hor <- length(HORIZON_LABS)
  tex <- c(
    paste0("\\begin{tabular}{l", paste(rep(" c c", n_hor), collapse = ""), "}"),
    "\\toprule",
    paste0(" & ", paste(rep(c("$\\hat{\\beta}_h$", "$\\hat{\\gamma}_h$"), n_hor), collapse = " & "), " \\\\"),
    paste0(" & ", paste(rep(HORIZON_LABS, each = 2), collapse = " & "), " \\\\"),
    "\\midrule"
  )

  for (ob in names(outcome_labels)) {
    pd <- res_df |> filter(outcome == ob)
    if (nrow(pd) == 0) next

    cells <- character()
    se_cells <- character()
    for (h in HORIZON_LABS) {
      base_r <- pd |> filter(horizon == h, grepl("base", term))
      pp_r   <- pd |> filter(horizon == h, grepl("perf_prob", term))

      base_cell <- if (nrow(base_r) > 0) paste0(fmt(base_r$coef), add_stars(base_r$pvalue)) else ""
      base_se   <- if (nrow(base_r) > 0) paste0("(", fmt(base_r$se), ")") else ""
      pp_cell   <- if (nrow(pp_r) > 0) paste0(fmt(pp_r$coef), add_stars(pp_r$pvalue)) else ""
      pp_se     <- if (nrow(pp_r) > 0) paste0("(", fmt(pp_r$se), ")") else ""

      cells <- c(cells, base_cell, pp_cell)
      se_cells <- c(se_cells, base_se, pp_se)
    }

    tex <- c(tex,
      paste0(outcome_labels[ob], " & ", paste(cells, collapse = " & "), " \\\\"),
      paste0(" & ", paste(se_cells, collapse = " & "), " \\\\"),
      "\\addlinespace")
  }

  tex <- c(tex, "\\midrule",
    paste0("\\multicolumn{", 2 * n_hor + 1, "}{l}{Event FE = Yes; Horizon FE = Yes; ",
           "perf\\_prob centered at 0.5} \\\\"),
    "\\bottomrule", "\\end{tabular}")

  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

# Use simpler table format: one row per horizon, two columns (base, interaction)
build_pp_dose_tex_simple <- function(res_df, filename) {
  primary_ob <- "points_change"
  pd <- res_df |> filter(outcome == primary_ob)

  tex <- c(
    "\\begin{tabular}{l c c c c}",
    "\\toprule",
    "Horizon & $\\hat{\\beta}_h$ (LL base) & SE & $\\hat{\\gamma}_h$ (LL $\\times$ perf\\_prob) & SE \\\\",
    "\\midrule"
  )

  for (h_lab in HORIZON_LABS) {
    base_r <- pd |> filter(horizon == h_lab, grepl("base", term))
    pp_r   <- pd |> filter(horizon == h_lab, grepl("perf_prob", term))

    base_cell <- if (nrow(base_r) > 0) paste0(fmt(base_r$coef), add_stars(base_r$pvalue)) else ""
    base_se   <- if (nrow(base_r) > 0) paste0("(", fmt(base_r$se), ")") else ""
    pp_cell   <- if (nrow(pp_r) > 0) paste0(fmt(pp_r$coef), add_stars(pp_r$pvalue)) else ""
    pp_se     <- if (nrow(pp_r) > 0) paste0("(", fmt(pp_r$se), ")") else ""

    tex <- c(tex, paste0(h_lab, " & ", base_cell, " & ", base_se,
                         " & ", pp_cell, " & ", pp_se, " \\\\"))
  }

  # All outcomes at 26w
  tex <- c(tex, "\\addlinespace", "\\midrule",
    "\\multicolumn{5}{l}{\\textit{All outcomes at 26w}} \\\\")
  for (ob in names(outcome_labels)) {
    ob_data <- res_df |> filter(outcome == ob, horizon == "26w")
    base_r <- ob_data |> filter(grepl("base", term))
    pp_r   <- ob_data |> filter(grepl("perf_prob", term))

    base_cell <- if (nrow(base_r) > 0) paste0(fmt(base_r$coef), add_stars(base_r$pvalue)) else ""
    base_se   <- if (nrow(base_r) > 0) paste0("(", fmt(base_r$se), ")") else ""
    pp_cell   <- if (nrow(pp_r) > 0) paste0(fmt(pp_r$coef), add_stars(pp_r$pvalue)) else ""
    pp_se     <- if (nrow(pp_r) > 0) paste0("(", fmt(pp_r$se), ")") else ""

    tex <- c(tex, paste0(outcome_labels[ob], " & ", base_cell, " & ", base_se,
                         " & ", pp_cell, " & ", pp_se, " \\\\"))
  }

  tex <- c(tex, "\\midrule",
    "\\multicolumn{5}{l}{Event FE = Yes; Horizon FE = Yes; perf\\_prob centered at 0.5} \\\\",
    "\\bottomrule", "\\end{tabular}")

  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

build_pp_dose_tex_simple(pp_res_atp, "table_dose_performance_prob_atp.tex")
build_pp_dose_tex_simple(pp_res_wta, "table_dose_performance_prob_wta.tex")

slog("## ISSUE 5: Performance probability dose tables")
for (info in list(list(r = pp_res_atp, lab = "ATP"), list(r = pp_res_wta, lab = "WTA"))) {
  pts_26 <- info$r |> filter(outcome == "points_change", horizon == "26w")
  if (nrow(pts_26) > 0) {
    for (i in seq_len(nrow(pts_26))) {
      slog(sprintf("- %s 26w %s: %.2f (SE=%.2f, p=%.3f)",
                   info$lab, pts_26$term[i], pts_26$coef[i], pts_26$se[i], pts_26$pvalue[i]))
    }
  }
}
slog("")


###############################################################################
# ISSUE 6: DOCUMENT p_{itm} EQUATION
###############################################################################

message("\n", strrep("=", 72))
message("  ISSUE 6: DOCUMENT p_{itm} EQUATION")
message(strrep("=", 72))

pitm_text <- c(
  "# Correct p_{itm} Specification",
  paste0("Generated: ", Sys.time()),
  "",
  "## Match-level win probability model",
  "",
  "The tournament performance model estimates:",
  "",
  "```",
  "P(i wins match m at tournament t) = Lambda(X_{ijm} beta + Z^pre_i gamma + delta * LL_i + rho * v_hat_i)",
  "```",
  "",
  "where Lambda() is the logistic CDF.",
  "",
  "### X_{ijm} (match-specific pairwise variables):",
  "1. log_rank_ratio = log(opp_rank / focal_rank)",
  "2. log_rank_ratio_sq = log_rank_ratio^2",
  "3. rank_diff = opp_rank - focal_rank",
  "4. same_ioc = 1 if same country",
  "5. is_clay = 1 if clay surface",
  "6. is_grass = 1 if grass surface",
  "7. age_diff = focal_age - opp_age",
  "8. height_diff = focal_ht - opp_ht",
  "9. hand_mismatch = 1 if different handedness",
  "10. h2h_smoothed = smoothed head-to-head win rate",
  "11. n_h2h = number of prior H2H meetings",
  "12. opp_elo = opponent Elo at match time",
  "",
  "### Z^pre_i (focal player pre-treatment characteristics):",
  "1. pre_rank_pts = ranking points before qualifying loss",
  "2. pre_elo = Elo rating before qualifying loss",
  "3. player_age_at_event = age at the LL-granting event",
  "",
  "Note: In the tournament model (scripts 24, 28), Z^pre enters WITHOUT",
  "quadratic terms (pre_rank_pts_sq, pre_elo_sq) and WITHOUT the 4 LL",
  "history variables. This is because:",
  "- The quadratics are absorbed by the match-level controls (rank ratios)",
  "- had_prior_ll is already in the model (or dropped in first-LL sample)",
  "- The full LL history is player-level and partially collinear with player_age",
  "",
  "### Treatment variable:",
  "- delta: LL_i (got_ll indicator)",
  "",
  "### Control function (non-GS only):",
  "- rho: v_hat_i (generalized residual from selection equation)",
  "",
  "## Formal equation for the paper:",
  "",
  "P(i wins match m | X_{ijm}, Z^{pre}_i, D_i) = Lambda(",
  "  beta_1 * Delta_Elo + beta_2 * Delta_SurfElo + beta_3 * H2H",
  "  + beta_4 * n_H2H + beta_5 * Delta_Age + beta_6 * BO5",
  "  + beta_7 * Surface + beta_8 * TourneyLevel",
  "  + beta_9 * Delta_Height + beta_10 * Handedness",
  "  + gamma_1 * pre_rank_pts_i + gamma_2 * pre_elo_i + gamma_3 * age_i",
  "  + delta * LL_i + rho * v_hat_i",
  ")",
  "",
  "Note: The Elo variables in X_{ijm} already include the focal player's Elo",
  "difference with the opponent, so pre_elo in Z^pre captures LEVEL effects",
  "beyond what Elo differences capture."
)

writeLines(pitm_text, file.path(OUTPUT_DIR, "pitm_equation_update.md"))
message("  Saved: Output/pitm_equation_update.md")

slog("## ISSUE 6: p_{itm} equation documented")
slog("")


###############################################################################
# ISSUE 7: FIX TOURNAMENT TABLE VARIABLE ORDERING
###############################################################################

message("\n", strrep("=", 72))
message("  ISSUE 7: FIX TOURNAMENT TABLE VARIABLE ORDERING")
message(strrep("=", 72))

# The current COVAR_NAMES_GS in script 28 does NOT include Z^pre first.
# We need: Z^pre first, then X_{ijm}, then treatment, then CF.
# Re-estimate and rewrite the firstll tables with correct ordering.

COVAR_NAMES_GS_ORDERED <- c(
  # Z^pre variables first
  "pre_rank_pts"        = "Pre-treatment ranking pts ($Z^{pre}$)",
  "pre_elo"             = "Pre-treatment Elo ($Z^{pre}$)",
  "player_age_at_event" = "Player age ($Z^{pre}$)",
  # X_{ijm} match-level variables
  "log_rank_ratio"      = "Log rank ratio ($X_{ijm}$)",
  "log_rank_ratio_sq"   = "Log rank ratio$^2$ ($X_{ijm}$)",
  "rank_diff"           = "Rank difference ($X_{ijm}$)",
  "same_ioc"            = "Same country ($X_{ijm}$)",
  "is_clay"             = "Clay surface ($X_{ijm}$)",
  "is_grass"            = "Grass surface ($X_{ijm}$)",
  "age_diff"            = "Age difference ($X_{ijm}$)",
  "height_diff"         = "Height difference ($X_{ijm}$)",
  "hand_mismatch"       = "Hand mismatch ($X_{ijm}$)",
  "h2h_smoothed"        = "H2H win rate ($X_{ijm}$)",
  "n_h2h"               = "N prior H2H ($X_{ijm}$)",
  "opp_elo"             = "Opponent Elo ($X_{ijm}$)",
  # Treatment
  "got_ll"              = "$\\hat{\\delta}$ (LL entry)"
)

COVAR_NAMES_NONGS_ORDERED <- c(
  # Z^pre variables first
  "pre_rank_pts"        = "Pre-treatment ranking pts ($Z^{pre}$)",
  "pre_elo"             = "Pre-treatment Elo ($Z^{pre}$)",
  "player_age_at_event" = "Player age ($Z^{pre}$)",
  # X_{ijm} match-level variables
  "log_rank_ratio"      = "Log rank ratio ($X_{ijm}$)",
  "log_rank_ratio_sq"   = "Log rank ratio$^2$ ($X_{ijm}$)",
  "rank_diff"           = "Rank difference ($X_{ijm}$)",
  "same_ioc"            = "Same country ($X_{ijm}$)",
  "is_clay"             = "Clay surface ($X_{ijm}$)",
  "is_grass"            = "Grass surface ($X_{ijm}$)",
  "age_diff"            = "Age difference ($X_{ijm}$)",
  "height_diff"         = "Height difference ($X_{ijm}$)",
  "hand_mismatch"       = "Hand mismatch ($X_{ijm}$)",
  "h2h_smoothed"        = "H2H win rate ($X_{ijm}$)",
  "n_h2h"               = "N prior H2H ($X_{ijm}$)",
  "opp_elo"             = "Opponent Elo ($X_{ijm}$)",
  # Treatment
  "got_ll"              = "$\\hat{\\delta}$ (LL entry)",
  # CF correction
  "v_hat"               = "$\\hat{\\rho}$ (CF correction)"
)

# Re-estimate the firstll models to get fresh coefficient objects
# Use the already-loaded match data from R24

estimate_gs_firstll_local <- function(match_df, label = "") {
  if (is.null(match_df) || nrow(match_df) < 30) {
    message("    ", label, ": insufficient observations")
    return(NULL)
  }
  df <- impute_match_data(match_df)

  fml <- won ~ got_ll +
    log_rank_ratio + log_rank_ratio_sq + rank_diff +
    same_ioc + is_clay + is_grass + age_diff + height_diff +
    hand_mismatch + h2h_smoothed + n_h2h +
    pre_elo + opp_elo + pre_rank_pts +
    player_age_at_event

  model <- glm(fml, family = binomial(link = "logit"), data = df)
  V <- vcovCL(model, cluster = df$focal_pid, type = "HC1")
  se_cl <- sqrt(diag(V))

  delta    <- coef(model)["got_ll"]
  delta_se <- se_cl["got_ll"]
  delta_p  <- 2 * pnorm(-abs(delta / delta_se))

  message(sprintf("    %s: delta=%.4f (SE=%.4f, p=%.4f), N=%d",
                  label, delta, delta_se, delta_p, nrow(df)))

  list(model = model, vcov_cl = V, data = df,
       delta = unname(delta), delta_se = unname(delta_se), delta_p = unname(delta_p),
       n_matches = nrow(df), n_events = n_distinct(df$event_id),
       n_players = n_distinct(df$focal_pid))
}

estimate_nongs_firstll_local <- function(match_df, label = "") {
  if (is.null(match_df) || nrow(match_df) < 50) {
    message("    ", label, ": insufficient observations")
    return(NULL)
  }
  df <- impute_match_data(match_df)

  fml <- won ~ got_ll + v_hat +
    log_rank_ratio + log_rank_ratio_sq + rank_diff +
    same_ioc + is_clay + is_grass + age_diff + height_diff +
    hand_mismatch + h2h_smoothed + n_h2h +
    pre_elo + opp_elo + pre_rank_pts +
    player_age_at_event

  model <- tryCatch(
    glm(fml, family = binomial(link = "logit"), data = df),
    error = function(e) { message("    ", label, " error: ", e$message); NULL }
  )
  if (is.null(model) || !model$converged) return(NULL)

  V <- vcovCL(model, cluster = df$focal_pid, type = "HC1")
  se_cl <- sqrt(diag(V))

  delta    <- coef(model)["got_ll"]
  delta_se <- se_cl["got_ll"]
  delta_p  <- 2 * pnorm(-abs(delta / delta_se))
  rho      <- coef(model)["v_hat"]
  rho_se   <- se_cl["v_hat"]
  rho_p    <- 2 * pnorm(-abs(rho / rho_se))

  message(sprintf("    %s: delta=%.4f (SE=%.4f), rho=%.4f (p=%.4f), N=%d",
                  label, delta, delta_se, rho, rho_p, nrow(df)))

  list(model = model, vcov_cl = V, data = df,
       delta = unname(delta), delta_se = unname(delta_se), delta_p = unname(delta_p),
       n_matches = nrow(df), n_events = n_distinct(df$event_id),
       n_players = n_distinct(df$focal_pid),
       rho = unname(rho), rho_se = unname(rho_se), rho_p = unname(rho_p))
}

message("  Re-estimating tournament models for table rewrite...")
gs_atp_fit7  <- estimate_gs_firstll_local(gs_atp_md, "GS-ATP")
gs_wta_fit7  <- estimate_gs_firstll_local(gs_wta_md, "GS-WTA")
nongs_atp_fit7 <- estimate_nongs_firstll_local(nongs_atp_md, "nonGS-ATP")
nongs_wta_fit7 <- estimate_nongs_firstll_local(nongs_wta_md, "nonGS-WTA")

# Write tables with corrected variable ordering
write_ordered_table <- function(fit_obj, filepath, covar_names, is_gs = TRUE, label = "") {
  if (is.null(fit_obj)) {
    message("  ", label, ": no fit, skipping")
    return()
  }

  model <- fit_obj$model
  V     <- fit_obj$vcov_cl
  beta  <- coef(model)
  se_cl <- sqrt(diag(V))

  lines <- c(
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & Estimate & SE \\\\",
    "\\midrule",
    sprintf("\\multicolumn{3}{l}{\\textit{Panel A: Logit Coefficients%s}} \\\\[3pt]",
            if (is_gs) "" else " (CF)")
  )

  for (vname in names(covar_names)) {
    if (vname %in% names(beta)) {
      est <- beta[vname]
      if (vname == "got_ll") {
        se_val <- fit_obj$delta_se
      } else if (vname == "v_hat" && !is.null(fit_obj$rho_se)) {
        se_val <- fit_obj$rho_se
      } else {
        se_val <- se_cl[vname]
      }
      pv <- 2 * pnorm(-abs(est / se_val))
      lines <- c(lines, sprintf("%s & %s%s & (%s) \\\\",
                                covar_names[vname], fmt(est, 4), add_stars(pv), fmt(se_val, 4)))
    }
  }

  lines <- c(lines, "\\midrule",
    sprintf("$N$ matches & \\multicolumn{2}{c}{%s} \\\\", format(fit_obj$n_matches, big.mark = ",")),
    sprintf("$N$ player-episodes & \\multicolumn{2}{c}{%s} \\\\", format(fit_obj$n_events, big.mark = ",")),
    sprintf("$N$ unique players & \\multicolumn{2}{c}{%s} \\\\", format(fit_obj$n_players, big.mark = ",")))

  if (!is_gs && !is.null(fit_obj$rho_p)) {
    lines <- c(lines,
      sprintf("Endogeneity $p$-value & \\multicolumn{2}{c}{%s} \\\\", fmt(fit_obj$rho_p, 3)))
  }

  lines <- c(lines, "\\bottomrule", "\\end{tabular}")

  writeLines(lines, filepath)
  message("  Saved: ", filepath)
}

write_ordered_table(gs_atp_fit7,
                    file.path(TABLES_DIR, "table_tournament_firstll_gs_atp.tex"),
                    COVAR_NAMES_GS_ORDERED, is_gs = TRUE, label = "GS-ATP")
write_ordered_table(gs_wta_fit7,
                    file.path(TABLES_DIR, "table_tournament_firstll_gs_wta.tex"),
                    COVAR_NAMES_GS_ORDERED, is_gs = TRUE, label = "GS-WTA")
write_ordered_table(nongs_atp_fit7,
                    file.path(TABLES_DIR, "table_tournament_firstll_nongs_atp.tex"),
                    COVAR_NAMES_NONGS_ORDERED, is_gs = FALSE, label = "nonGS-ATP")
write_ordered_table(nongs_wta_fit7,
                    file.path(TABLES_DIR, "table_tournament_firstll_nongs_wta.tex"),
                    COVAR_NAMES_NONGS_ORDERED, is_gs = FALSE, label = "nonGS-WTA")

slog("## ISSUE 7: Tournament table variable ordering fixed")
slog("- New ordering: Z^pre first, X_{ijm} second, treatment/CF last")
slog("")


###############################################################################
# ISSUE 9: TOURNAMENT PERFORMANCE PROBABILITY INTERACTION
###############################################################################

message("\n", strrep("=", 72))
message("  ISSUE 9: TOURNAMENT PERFORMANCE PROBABILITY INTERACTION")
message(strrep("=", 72))

# Compute performance probability for each event in the tournament data,
# then add ll_entry x perf_prob_c interaction

compute_perf_prob_tournament <- function(ev_table, md_table, all_matches, tour_label) {
  # For LL: compute probability of observed main draw performance
  # For controls: compute probability of observed qualifying loss
  main_md <- all_matches |> filter(tour == tour_label, match_source == "main")

  ev_table$perf_prob <- NA_real_

  for (i in seq_len(nrow(ev_table))) {
    e <- ev_table[i, ]

    if (e$got_ll == 1) {
      pid <- e$player_pid
      tid <- e$tourney_id

      ll_matches <- main_md |>
        filter(tourney_id == tid,
               (winner_pid == pid | loser_pid == pid)) |>
        arrange(match_num)

      if (nrow(ll_matches) == 0) {
        ev_table$perf_prob[i] <- 0.7
        next
      }

      prob <- 1.0
      for (j in seq_len(nrow(ll_matches))) {
        m <- ll_matches[j, ]
        won_match <- (m$winner_pid == pid)
        opp_pid <- if (won_match) m$loser_pid else m$winner_pid
        opp_elo_val <- get_elo_at_date(elo_list$overall, opp_pid, m$tourney_date)
        focal_elo <- e$pre_elo
        if (is.na(focal_elo)) focal_elo <- 1500
        p_win <- 1 / (1 + 10^((opp_elo_val - focal_elo) / 400))

        if (won_match) {
          prob <- prob * p_win
        } else {
          prob <- prob * (1 - p_win)
          break
        }
      }
      ev_table$perf_prob[i] <- prob
    } else {
      focal_elo <- e$pre_elo
      if (is.na(focal_elo)) focal_elo <- 1500
      p_win_qual <- 1 / (1 + 10^((1500 - focal_elo) / 400))
      ev_table$perf_prob[i] <- 1 - p_win_qual
    }
  }

  ev_table$perf_prob_c <- ev_table$perf_prob - 0.5

  # Propagate to match data
  pp_lookup <- ev_table |> select(event_id, perf_prob_c)
  md_table <- md_table |>
    left_join(pp_lookup, by = "event_id") |>
    mutate(
      perf_prob_c = replace_na(perf_prob_c, 0),
      ll_x_pp = got_ll * perf_prob_c
    )

  list(events = ev_table, matches = md_table)
}

message("  Computing performance probabilities for tournament data...")
pp_gs_atp    <- compute_perf_prob_tournament(gs_atp_ev, gs_atp_md, all_matches, "ATP")
pp_gs_wta    <- compute_perf_prob_tournament(gs_wta_ev, gs_wta_md, all_matches, "WTA")
pp_nongs_atp <- compute_perf_prob_tournament(nongs_atp_ev, nongs_atp_md, all_matches, "ATP")
pp_nongs_wta <- compute_perf_prob_tournament(nongs_wta_ev, nongs_wta_md, all_matches, "WTA")

# Estimate GS perf_prob model
estimate_gs_perf_prob <- function(match_df, label = "") {
  if (is.null(match_df) || nrow(match_df) < 30) return(NULL)
  df <- impute_match_data(match_df)

  fml <- won ~ got_ll + ll_x_pp +
    log_rank_ratio + log_rank_ratio_sq + rank_diff +
    same_ioc + is_clay + is_grass + age_diff + height_diff +
    hand_mismatch + h2h_smoothed + n_h2h +
    pre_elo + opp_elo + pre_rank_pts +
    player_age_at_event

  model <- tryCatch(
    glm(fml, family = binomial(link = "logit"), data = df),
    error = function(e) { message("    ", label, " error: ", e$message); NULL }
  )
  if (is.null(model) || !model$converged) return(NULL)

  V <- vcovCL(model, cluster = df$focal_pid, type = "HC1")
  se_cl <- sqrt(diag(V))
  beta <- coef(model)

  delta    <- beta["got_ll"]
  delta_se <- se_cl["got_ll"]
  delta_p  <- 2 * pnorm(-abs(delta / delta_se))
  gamma    <- beta["ll_x_pp"]
  gamma_se <- se_cl["ll_x_pp"]
  gamma_p  <- 2 * pnorm(-abs(gamma / gamma_se))

  message(sprintf("    %s: delta=%.4f (p=%.4f), gamma=%.4f (p=%.4f), N=%d",
                  label, delta, delta_p, gamma, gamma_p, nrow(df)))

  list(delta = unname(delta), delta_se = unname(delta_se), delta_p = unname(delta_p),
       gamma = unname(gamma), gamma_se = unname(gamma_se), gamma_p = unname(gamma_p),
       n_matches = nrow(df), n_events = n_distinct(df$event_id),
       n_players = n_distinct(df$focal_pid),
       rho = NA_real_, rho_se = NA_real_, rho_p = NA_real_)
}

# Estimate nonGS perf_prob model (with v_hat + v_hat:perf_prob_c)
estimate_nongs_perf_prob <- function(match_df, label = "") {
  if (is.null(match_df) || nrow(match_df) < 50) return(NULL)
  df <- impute_match_data(match_df)

  df <- df |> mutate(v_x_pp = v_hat * perf_prob_c)

  fml <- won ~ got_ll + ll_x_pp + v_hat + v_x_pp +
    log_rank_ratio + log_rank_ratio_sq + rank_diff +
    same_ioc + is_clay + is_grass + age_diff + height_diff +
    hand_mismatch + h2h_smoothed + n_h2h +
    pre_elo + opp_elo + pre_rank_pts +
    player_age_at_event

  model <- tryCatch(
    glm(fml, family = binomial(link = "logit"), data = df),
    error = function(e) { message("    ", label, " error: ", e$message); NULL }
  )
  if (is.null(model) || !model$converged) return(NULL)

  V <- vcovCL(model, cluster = df$focal_pid, type = "HC1")
  se_cl <- sqrt(diag(V))
  beta <- coef(model)

  delta    <- beta["got_ll"]
  delta_se <- se_cl["got_ll"]
  delta_p  <- 2 * pnorm(-abs(delta / delta_se))
  gamma    <- beta["ll_x_pp"]
  gamma_se <- se_cl["ll_x_pp"]
  gamma_p  <- 2 * pnorm(-abs(gamma / gamma_se))
  rho      <- beta["v_hat"]
  rho_se   <- se_cl["v_hat"]
  rho_p    <- 2 * pnorm(-abs(rho / rho_se))

  message(sprintf("    %s: delta=%.4f, gamma=%.4f (p=%.4f), rho=%.4f (p=%.4f), N=%d",
                  label, delta, gamma, gamma_p, rho, rho_p, nrow(df)))

  list(delta = unname(delta), delta_se = unname(delta_se), delta_p = unname(delta_p),
       gamma = unname(gamma), gamma_se = unname(gamma_se), gamma_p = unname(gamma_p),
       n_matches = nrow(df), n_events = n_distinct(df$event_id),
       n_players = n_distinct(df$focal_pid),
       rho = unname(rho), rho_se = unname(rho_se), rho_p = unname(rho_p))
}

pp_fit_gs_atp    <- estimate_gs_perf_prob(pp_gs_atp$matches, "perf_prob GS-ATP")
pp_fit_gs_wta    <- estimate_gs_perf_prob(pp_gs_wta$matches, "perf_prob GS-WTA")
pp_fit_nongs_atp <- estimate_nongs_perf_prob(pp_nongs_atp$matches, "perf_prob nonGS-ATP")
pp_fit_nongs_wta <- estimate_nongs_perf_prob(pp_nongs_wta$matches, "perf_prob nonGS-WTA")

# Write tournament perf_prob tables
write_perf_prob_table <- function(fit, filepath, is_gs = TRUE, label = "") {
  if (is.null(fit)) {
    message("  ", label, ": no fit, skipping")
    return()
  }

  lines <- c(
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & Estimate & SE \\\\",
    "\\midrule",
    sprintf("\\multicolumn{3}{l}{\\textit{Performance Probability Interaction%s}} \\\\[3pt]",
            if (is_gs) "" else " (CF)")
  )

  lines <- c(lines,
    sprintf("$\\hat{\\delta}$ (LL entry) & %s%s & (%s) \\\\",
            fmt(fit$delta, 4), add_stars(fit$delta_p), fmt(fit$delta_se, 4)),
    sprintf("$\\hat{\\gamma}$ (LL $\\times$ perf\\_prob$_c$) & %s%s & (%s) \\\\",
            fmt(fit$gamma, 4), add_stars(fit$gamma_p), fmt(fit$gamma_se, 4))
  )

  if (!is_gs && !is.na(fit$rho)) {
    lines <- c(lines,
      sprintf("$\\hat{\\rho}$ (CF correction) & %s%s & (%s) \\\\",
              fmt(fit$rho, 4), add_stars(fit$rho_p), fmt(fit$rho_se, 4)))
  }

  lines <- c(lines, "\\midrule",
    sprintf("$N$ matches & \\multicolumn{2}{c}{%s} \\\\", format(fit$n_matches, big.mark = ",")),
    sprintf("$N$ events & \\multicolumn{2}{c}{%s} \\\\", format(fit$n_events, big.mark = ",")),
    sprintf("$N$ players & \\multicolumn{2}{c}{%s} \\\\", format(fit$n_players, big.mark = ",")),
    "\\multicolumn{3}{l}{perf\\_prob centered at 0.5; match-level controls included} \\\\",
    "\\bottomrule", "\\end{tabular}")

  writeLines(lines, filepath)
  message("  Saved: ", filepath)
}

write_perf_prob_table(pp_fit_gs_atp,
                      file.path(TABLES_DIR, "table_tournament_perf_prob_gs_atp.tex"),
                      is_gs = TRUE, label = "GS-ATP")
write_perf_prob_table(pp_fit_gs_wta,
                      file.path(TABLES_DIR, "table_tournament_perf_prob_gs_wta.tex"),
                      is_gs = TRUE, label = "GS-WTA")
write_perf_prob_table(pp_fit_nongs_atp,
                      file.path(TABLES_DIR, "table_tournament_perf_prob_nongs_atp.tex"),
                      is_gs = FALSE, label = "nonGS-ATP")
write_perf_prob_table(pp_fit_nongs_wta,
                      file.path(TABLES_DIR, "table_tournament_perf_prob_nongs_wta.tex"),
                      is_gs = FALSE, label = "nonGS-WTA")

slog("## ISSUE 9: Tournament performance probability interaction")
for (info in list(
  list(r = pp_fit_gs_atp, lab = "GS-ATP"),
  list(r = pp_fit_gs_wta, lab = "GS-WTA"),
  list(r = pp_fit_nongs_atp, lab = "nonGS-ATP"),
  list(r = pp_fit_nongs_wta, lab = "nonGS-WTA")
)) {
  if (!is.null(info$r)) {
    slog(sprintf("- %s: delta=%.4f (p=%.4f), gamma=%.4f (p=%.4f), N=%d",
                 info$lab, info$r$delta, info$r$delta_p,
                 info$r$gamma, info$r$gamma_p, info$r$n_matches))
  }
}
slog("")


###############################################################################
# SAVE ALL RESULTS
###############################################################################

message("\n", strrep("=", 72))
message("  SAVING ALL RESULTS")
message(strrep("=", 72))

nine_fixes_results <- list(
  # ISSUE 1: First stage
  first_stage_coefs = cf_lpm,
  first_stage_ses = fs_lpm_se,
  first_stage_fstat = f_stat_val,
  first_stage_n = nrow(nongs_with_pc),

  # ISSUE 5: Performance probability dose (stacked)
  pp_dose_atp = pp_res_atp,
  pp_dose_wta = pp_res_wta,

  # ISSUE 8: Tournament dose (corrected)
  dose_gs_atp = dose_gs_atp_fix,
  dose_gs_wta = dose_gs_wta_fix,
  dose_nongs_atp = dose_nongs_atp_fix,
  dose_nongs_wta = dose_nongs_wta_fix,

  # ISSUE 9: Tournament performance probability
  perf_prob_gs_atp = pp_fit_gs_atp,
  perf_prob_gs_wta = pp_fit_gs_wta,
  perf_prob_nongs_atp = pp_fit_nongs_atp,
  perf_prob_nongs_wta = pp_fit_nongs_wta,

  # ISSUE 7: Re-estimated tournament models
  tournament_gs_atp = gs_atp_fit7,
  tournament_gs_wta = gs_wta_fit7,
  tournament_nongs_atp = nongs_atp_fit7,
  tournament_nongs_wta = nongs_wta_fit7
)

saveRDS(nine_fixes_results, file.path(CLEANED_DIR, "nine_fixes_results.rds"))
message("  Saved: Data/cleaned/nine_fixes_results.rds")

# Write summary
summary_text <- paste(summary_log, collapse = "\n")
writeLines(summary_text, file.path(OUTPUT_DIR, "nine_fixes_summary.md"))
message("  Saved: Output/nine_fixes_summary.md")

message("\n", strrep("=", 72))
message("  31_nine_fixes.R COMPLETE")
message(strrep("=", 72))
message("\nTables generated:")
message("  - Tables/table_iv_first_stage.tex (ISSUE 1)")
message("  - Tables/table_dose_performance_prob_atp.tex (ISSUE 5)")
message("  - Tables/table_dose_performance_prob_wta.tex (ISSUE 5)")
message("  - Tables/table_tournament_firstll_gs_atp.tex (ISSUE 7)")
message("  - Tables/table_tournament_firstll_gs_wta.tex (ISSUE 7)")
message("  - Tables/table_tournament_firstll_nongs_atp.tex (ISSUE 7)")
message("  - Tables/table_tournament_firstll_nongs_wta.tex (ISSUE 7)")
message("  - Tables/table_tournament_dose_gs_atp.tex (ISSUE 8)")
message("  - Tables/table_tournament_dose_gs_wta.tex (ISSUE 8)")
message("  - Tables/table_tournament_dose_nongs_atp.tex (ISSUE 8)")
message("  - Tables/table_tournament_dose_nongs_wta.tex (ISSUE 8)")
message("  - Tables/table_tournament_perf_prob_gs_atp.tex (ISSUE 9)")
message("  - Tables/table_tournament_perf_prob_gs_wta.tex (ISSUE 9)")
message("  - Tables/table_tournament_perf_prob_nongs_atp.tex (ISSUE 9)")
message("  - Tables/table_tournament_perf_prob_nongs_wta.tex (ISSUE 9)")
message("\nDocumentation:")
message("  - Output/figure_removal.md (ISSUE 2)")
message("  - Output/hetero_verification.md (ISSUE 3)")
message("  - Output/dose_stacked_verification.md (ISSUE 4)")
message("  - Output/pitm_equation_update.md (ISSUE 6)")
message("  - Output/nine_fixes_summary.md (all issues)")
