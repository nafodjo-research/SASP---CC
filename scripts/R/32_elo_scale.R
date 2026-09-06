# ==============================================================================
# 32_elo_scale.R
# Scale Elo variables by 1/100 so that coefficients are human-readable.
#
# Problem:  Raw Elo ~1500-2100 produces tiny coefficients; Elo^2 ~2.5M produces
#           microscopic coefficients. Dividing by 100 yields Elo/100 ~ 15-21
#           and (Elo/100)^2 ~ 225-441, giving interpretable magnitudes.
#
# Scope:
#   - pre_elo and pre_elo_sq in the estimation samples (covariate scaling)
#   - pre_elo and opp_elo in tournament match data (covariate scaling)
#   - NOT elo_change_*w columns (those are outcomes, not covariates)
#
# Inputs:
#   Data/cleaned/skeleton_gs_est_v2.rds
#   Data/cleaned/skeleton_nongs_est_v2.rds
#   Data/cleaned/tournament_match_fix_results.rds
#
# Outputs:
#   Data/cleaned/skeleton_gs_est_v3.rds
#   Data/cleaned/skeleton_nongs_est_v3.rds
#   Tables/table_sumstats_gs_atp.tex
#   Tables/table_sumstats_gs_wta.tex
#   Tables/table_sumstats_nongs_atp.tex
#   Tables/table_sumstats_nongs_wta.tex
#   Tables/table_tournament_firstll_gs_atp.tex
#   Tables/table_tournament_firstll_gs_wta.tex
#   Output/elo_scaling.md
#
# Dependencies: dplyr, readr, fixest, sandwich, here
# ==============================================================================

set.seed(20260327)

library(dplyr)
library(readr)
library(here)
library(sandwich)

source(here("scripts", "R", "utils.R"))

# --- Paths --------------------------------------------------------------------
CLEANED_DIR <- here("Data", "cleaned")
TABLES_DIR  <- here("Tables")
OUTPUT_DIR  <- here("Output")
for (d in c(CLEANED_DIR, TABLES_DIR, OUTPUT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

summary_log <- character()


###############################################################################
# SECTION 1: SCALE ELO IN ESTIMATION SAMPLES
###############################################################################

message("\n", strrep("=", 70))
message("SECTION 1: SCALE ELO IN ESTIMATION SAMPLES")
message(strrep("=", 70))

gs_est  <- readRDS(file.path(CLEANED_DIR, "skeleton_gs_est_v2.rds"))
nongs_est <- readRDS(file.path(CLEANED_DIR, "skeleton_nongs_est_v2.rds"))

message("  GS est: N = ", nrow(gs_est), " cols = ", ncol(gs_est))
message("  Non-GS est: N = ", nrow(nongs_est), " cols = ", ncol(nongs_est))

# Document pre-scaling ranges
gs_elo_range    <- range(gs_est$pre_elo, na.rm = TRUE)
nongs_elo_range <- range(nongs_est$pre_elo, na.rm = TRUE)
message("  GS pre_elo range (raw): [", round(gs_elo_range[1], 1), ", ", round(gs_elo_range[2], 1), "]")
message("  Non-GS pre_elo range (raw): [", round(nongs_elo_range[1], 1), ", ", round(nongs_elo_range[2], 1), "]")

# Scale: pre_elo /= 100, then recompute pre_elo_sq = (pre_elo/100)^2
scale_elo_est <- function(df) {
  df |>
    mutate(
      pre_elo    = pre_elo / 100,
      pre_elo_sq = pre_elo^2   # already divided, so this is (pre_elo/100)^2
    )
}

gs_est_v3    <- scale_elo_est(gs_est)
nongs_est_v3 <- scale_elo_est(nongs_est)

message("  GS pre_elo range (scaled): [", round(min(gs_est_v3$pre_elo, na.rm = TRUE), 2),
        ", ", round(max(gs_est_v3$pre_elo, na.rm = TRUE), 2), "]")
message("  Non-GS pre_elo range (scaled): [", round(min(nongs_est_v3$pre_elo, na.rm = TRUE), 2),
        ", ", round(max(nongs_est_v3$pre_elo, na.rm = TRUE), 2), "]")

# Verify elo_change columns are untouched
elo_change_cols <- grep("^elo_change_", names(gs_est_v3), value = TRUE)
for (cc in elo_change_cols) {
  orig <- gs_est[[cc]]
  scaled <- gs_est_v3[[cc]]
  stopifnot(identical(orig, scaled))
}
message("  Verified: elo_change_*w columns are unchanged")

saveRDS(gs_est_v3, file.path(CLEANED_DIR, "skeleton_gs_est_v3.rds"))
saveRDS(nongs_est_v3, file.path(CLEANED_DIR, "skeleton_nongs_est_v3.rds"))
message("  Saved: skeleton_gs_est_v3.rds, skeleton_nongs_est_v3.rds")

slog("## Section 1: Estimation sample Elo scaling")
slog("- pre_elo divided by 100; pre_elo_sq recomputed as (pre_elo/100)^2")
slog("- GS raw range: [", round(gs_elo_range[1], 1), ", ", round(gs_elo_range[2], 1), "]")
slog("- GS scaled range: [", round(min(gs_est_v3$pre_elo, na.rm = TRUE), 2),
     ", ", round(max(gs_est_v3$pre_elo, na.rm = TRUE), 2), "]")
slog("- elo_change_*w outcome columns NOT scaled (confirmed identical)")
slog("")


###############################################################################
# SECTION 2: REBUILD SUMMARY STATS TABLES
###############################################################################

message("\n", strrep("=", 70))
message("SECTION 2: REBUILD SUMMARY STATS TABLES")
message(strrep("=", 70))

# Split by tour
gs_atp <- gs_est_v3 |> filter(tour == "ATP")
gs_wta <- gs_est_v3 |> filter(tour == "WTA")
nongs_atp <- nongs_est_v3 |> filter(tour == "ATP")
nongs_wta <- nongs_est_v3 |> filter(tour == "WTA")

# Updated balance_vars: Elo label now says "Elo / 100"
balance_vars <- c(
  "player_age"              = "Age",
  "pre_rank_pts"            = "Ranking points",
  "pre_elo"                 = "Elo rating / 100",
  "player_ht"               = "Height (cm)",
  "n_prior_gs_ll_won"       = "Prior GS LL spots won",
  "n_prior_gs_ll_notwon"    = "Prior GS LL candidacies (not won)",
  "n_prior_nongs_ll_won"    = "Prior non-GS LL spots won",
  "n_prior_nongs_ll_notwon" = "Prior non-GS LL candidacies (not won)",
  "n_prior_gs_ll_opp"       = "Prior final-round qual. losses at LL-granting GS events",
  "n_prior_nongs_ll_opp"    = "Prior final-round qual. losses at LL-granting non-GS events"
)

build_balance_table <- function(data, filename, table_label) {
  ll_data  <- data |> filter(got_ll == 1)
  ctl_data <- data |> filter(got_ll == 0)

  tex <- c(
    "\\begin{tabular}{l c c c c}",
    "\\toprule",
    " & LL Mean (SD) & Control Mean (SD) & Difference & $p$-value \\\\",
    "\\midrule"
  )

  for (vn in names(balance_vars)) {
    if (!vn %in% names(data)) next
    vlab <- balance_vars[vn]

    ll_vals  <- ll_data[[vn]]
    ctl_vals <- ctl_data[[vn]]

    ll_mean  <- mean(ll_vals, na.rm = TRUE)
    ll_sd    <- sd(ll_vals, na.rm = TRUE)
    ctl_mean <- mean(ctl_vals, na.rm = TRUE)
    ctl_sd   <- sd(ctl_vals, na.rm = TRUE)
    diff_val <- ll_mean - ctl_mean

    pv <- tryCatch({
      t.test(ll_vals, ctl_vals)$p.value
    }, error = function(e) NA_real_)

    ll_cell  <- paste0(fmt(ll_mean), " (", fmt(ll_sd), ")")
    ctl_cell <- paste0(fmt(ctl_mean), " (", fmt(ctl_sd), ")")
    diff_cell <- fmt(diff_val)
    pv_cell  <- if (!is.na(pv)) fmt(pv, 3) else "--"

    tex <- c(tex, paste0(vlab, " & ", ll_cell, " & ", ctl_cell,
                         " & ", diff_cell, " & ", pv_cell, " \\\\"))
  }

  tex <- c(tex,
    "\\midrule",
    paste0("$N$ (LL) & \\multicolumn{4}{c}{", nrow(ll_data), "} \\\\"),
    paste0("$N$ (Control) & \\multicolumn{4}{c}{", nrow(ctl_data), "} \\\\"),
    "\\bottomrule",
    "\\end{tabular}"
  )

  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

build_balance_table(gs_atp,    "table_sumstats_gs_atp.tex",    "GS ATP")
build_balance_table(gs_wta,    "table_sumstats_gs_wta.tex",    "GS WTA")
build_balance_table(nongs_atp, "table_sumstats_nongs_atp.tex", "Non-GS ATP")
build_balance_table(nongs_wta, "table_sumstats_nongs_wta.tex", "Non-GS WTA")

slog("## Section 2: Summary stats tables rebuilt")
slog("- Elo row label updated to 'Elo rating / 100'")
slog("- 4 tables: table_sumstats_{gs,nongs}_{atp,wta}.tex")
slog("")


###############################################################################
# SECTION 3: RE-ESTIMATE TOURNAMENT FIRSTLL MODELS (GS ONLY)
###############################################################################

message("\n", strrep("=", 70))
message("SECTION 3: RE-ESTIMATE TOURNAMENT FIRSTLL MODELS (GS)")
message(strrep("=", 70))

# Load tournament match data
R24 <- readRDS(file.path(CLEANED_DIR, "tournament_match_fix_results.rds"))

gs_atp_md    <- R24$gs_atp$matches
gs_wta_md    <- R24$gs_wta$matches

message("  GS ATP matches: N = ", nrow(gs_atp_md))
message("  GS WTA matches: N = ", nrow(gs_wta_md))

# Scale Elo in match data: pre_elo and opp_elo both /100
scale_elo_match <- function(md) {
  md |>
    mutate(
      pre_elo = pre_elo / 100,
      opp_elo = opp_elo / 100
    )
}

gs_atp_md <- scale_elo_match(gs_atp_md)
gs_wta_md <- scale_elo_match(gs_wta_md)

message("  GS ATP pre_elo range (scaled): [",
        round(min(gs_atp_md$pre_elo, na.rm = TRUE), 2), ", ",
        round(max(gs_atp_md$pre_elo, na.rm = TRUE), 2), "]")
message("  GS ATP opp_elo range (scaled): [",
        round(min(gs_atp_md$opp_elo, na.rm = TRUE), 2), ", ",
        round(max(gs_atp_md$opp_elo, na.rm = TRUE), 2), "]")

# Imputation helper (same as script 31)
impute_match_data <- function(df) {
  df |>
    mutate(
      h2h_smoothed   = replace(h2h_smoothed, is.na(h2h_smoothed), 0.5),
      n_h2h          = replace(n_h2h, is.na(n_h2h), 0L),
      age_diff       = replace(age_diff, is.na(age_diff), 0),
      height_diff    = replace(height_diff, is.na(height_diff), 0),
      hand_mismatch  = replace(hand_mismatch, is.na(hand_mismatch), 0L),
      opp_elo        = replace(opp_elo, is.na(opp_elo), 15),   # 1500/100
      pre_elo        = replace(pre_elo, is.na(pre_elo), 15),   # 1500/100
      pre_rank_pts   = replace(pre_rank_pts, is.na(pre_rank_pts), 0)
    )
}

# GS firstll estimation (logit with clustered SEs, no CF)
estimate_gs_firstll <- function(match_df, label = "") {
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

gs_atp_fit <- estimate_gs_firstll(gs_atp_md, "GS-ATP")
gs_wta_fit <- estimate_gs_firstll(gs_wta_md, "GS-WTA")

# Variable labels for GS firstll tables (Elo labels updated)
COVAR_NAMES_GS_ORDERED <- c(
  # Z^pre variables first
  "pre_rank_pts"        = "Pre-treatment ranking pts ($Z^{pre}$)",
  "pre_elo"             = "Pre-treatment Elo / 100 ($Z^{pre}$)",
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
  "opp_elo"             = "Opponent Elo / 100 ($X_{ijm}$)",
  # Treatment
  "got_ll"              = "$\\hat{\\delta}$ (LL entry)"
)

# Write table function (same format as script 31)
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
  message("  Saved: ", basename(filepath))
}

write_ordered_table(gs_atp_fit,
                    file.path(TABLES_DIR, "table_tournament_firstll_gs_atp.tex"),
                    COVAR_NAMES_GS_ORDERED, is_gs = TRUE, label = "GS-ATP")
write_ordered_table(gs_wta_fit,
                    file.path(TABLES_DIR, "table_tournament_firstll_gs_wta.tex"),
                    COVAR_NAMES_GS_ORDERED, is_gs = TRUE, label = "GS-WTA")

slog("## Section 3: Tournament firstll GS tables re-estimated")
if (!is.null(gs_atp_fit)) {
  slog(sprintf("- GS ATP: delta=%.4f (SE=%.4f, p=%.4f), N=%d matches",
               gs_atp_fit$delta, gs_atp_fit$delta_se, gs_atp_fit$delta_p, gs_atp_fit$n_matches))
}
if (!is.null(gs_wta_fit)) {
  slog(sprintf("- GS WTA: delta=%.4f (SE=%.4f, p=%.4f), N=%d matches",
               gs_wta_fit$delta, gs_wta_fit$delta_se, gs_wta_fit$delta_p, gs_wta_fit$n_matches))
}
slog("- Elo labels updated to 'Elo / 100' in table rows")
slog("")


###############################################################################
# SECTION 4: DOCUMENTATION
###############################################################################

message("\n", strrep("=", 70))
message("SECTION 4: DOCUMENTATION")
message(strrep("=", 70))

doc <- c(
  "# Elo Scaling Documentation",
  paste0("Generated: ", Sys.time()),
  "",
  "## What was done",
  "",
  "All Elo variables used as COVARIATES (not outcomes) were divided by 100:",
  "",
  "### Estimation samples (skeleton_*_est_v3.rds)",
  "- `pre_elo` = `pre_elo_raw / 100`",
  "- `pre_elo_sq` = `(pre_elo_raw / 100)^2`",
  "",
  "### Tournament match data (in-memory for table re-estimation)",
  "- `pre_elo` = `pre_elo_raw / 100` (focal player Elo)",
  "- `opp_elo` = `opp_elo_raw / 100` (opponent Elo)",
  "",
  "## What was NOT scaled",
  "",
  "- `elo_change_4w`, `elo_change_8w`, `elo_change_12w`, `elo_change_26w`, `elo_change_52w`",
  "  These are OUTCOME variables measuring Elo change in raw points.",
  "  Raw Elo change in points (e.g., +30 points) is directly interpretable.",
  "",
  "- `elo_t0`, `elo_t4`, ... `elo_t52` (level outcomes) are also not scaled.",
  "",
  "## Why",
  "",
  "Raw Elo values are ~1500-2100. This means:",
  "- Coefficients on `pre_elo` were O(10^-3)",
  "- Coefficients on `pre_elo_sq` were O(10^-6)",
  "",
  "After scaling by 100:",
  "- `pre_elo` ranges from ~14-21",
  "- `pre_elo_sq` ranges from ~190-450",
  "- Coefficients are now O(10^-1) and O(10^-3) respectively",
  "",
  "## Pre-scaling ranges",
  "",
  sprintf("- GS pre_elo: [%.1f, %.1f]", gs_elo_range[1], gs_elo_range[2]),
  sprintf("- Non-GS pre_elo: [%.1f, %.1f]", nongs_elo_range[1], nongs_elo_range[2]),
  "",
  "## Post-scaling ranges",
  "",
  sprintf("- GS pre_elo: [%.2f, %.2f]", min(gs_est_v3$pre_elo, na.rm = TRUE),
          max(gs_est_v3$pre_elo, na.rm = TRUE)),
  sprintf("- Non-GS pre_elo: [%.2f, %.2f]", min(nongs_est_v3$pre_elo, na.rm = TRUE),
          max(nongs_est_v3$pre_elo, na.rm = TRUE)),
  "",
  "## Tables updated",
  "",
  "- `table_sumstats_gs_atp.tex` (Elo row label: 'Elo rating / 100')",
  "- `table_sumstats_gs_wta.tex`",
  "- `table_sumstats_nongs_atp.tex`",
  "- `table_sumstats_nongs_wta.tex`",
  "- `table_tournament_firstll_gs_atp.tex` (re-estimated with scaled Elo)",
  "- `table_tournament_firstll_gs_wta.tex` (re-estimated with scaled Elo)",
  "",
  "## Note on downstream scripts",
  "",
  "Any script that loads `skeleton_*_est_v3.rds` gets the scaled Elo.",
  "Scripts using the `ZPRE_FULL` formula string (`pre_elo + pre_elo_sq`) will",
  "automatically pick up the scaled values from the v3 datasets.",
  "",
  "The stacked dynamics models (scripts 30, 18) that use `elo_change` as an",
  "outcome should continue to use unscaled Elo change values. The v3 datasets",
  "preserve the original `elo_change_*w` columns."
)

writeLines(doc, file.path(OUTPUT_DIR, "elo_scaling.md"))
message("  Saved: Output/elo_scaling.md")

# Write summary log
writeLines(summary_log, file.path(OUTPUT_DIR, "elo_scaling_log.txt"))

message("\n", strrep("=", 70))
message("DONE: 32_elo_scale.R completed successfully")
message(strrep("=", 70))
message("\nOutputs:")
message("  Data/cleaned/skeleton_gs_est_v3.rds")
message("  Data/cleaned/skeleton_nongs_est_v3.rds")
message("  Tables/table_sumstats_gs_atp.tex")
message("  Tables/table_sumstats_gs_wta.tex")
message("  Tables/table_sumstats_nongs_atp.tex")
message("  Tables/table_sumstats_nongs_wta.tex")
message("  Tables/table_tournament_firstll_gs_atp.tex")
message("  Tables/table_tournament_firstll_gs_wta.tex")
message("  Output/elo_scaling.md")
