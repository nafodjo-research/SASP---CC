# ==============================================================================
# 19_equation_fixes.R
# Targeted equation fixes for the Lucky Losers pipeline:
#   FIX 1: Horizon fixed effects test (with vs without alpha_h)
#   FIX 2: Heterogeneity spec with D and D x category dummies
#   FIX 3: Verified lottery subsample -- stacked
#   FIX 4: First-LL-only restriction -- stacked (n_prior_gs_ll_won == 0)
#   FIX 5: WTA equivalents and non-GS mirrors for all GS tables
#
# Inputs:  Data/cleaned/skeleton_gs_est.rds,
#          Data/cleaned/skeleton_nongs_est.rds
# Outputs: Output/horizon_fe_test.md,
#          Tables/table_hetero_stacked_atp.tex,
#          Tables/table_hetero_stacked_wta.tex,
#          Tables/table_dose_stacked.tex,
#          Tables/table_verified_stacked.tex,
#          Tables/table_firstll_stacked_atp.tex,
#          Tables/table_firstll_stacked_wta.tex,
#          Tables/table_hetero_stacked_nongs_atp.tex,
#          Tables/table_hetero_stacked_nongs_wta.tex,
#          Tables/table_dose_stacked_nongs.tex,
#          Output/equation_fixes_summary.md
# Dependencies: dplyr, tidyr, fixest, here
# ==============================================================================

set.seed(20260325)

# --- Packages -----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(fixest)
library(here)

# --- Paths --------------------------------------------------------------------
CLEANED_DIR <- here("Data", "cleaned")
TABLES_DIR  <- here("Tables")
OUTPUT_DIR  <- here("Output")
for (d in c(TABLES_DIR, OUTPUT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# --- Helpers ------------------------------------------------------------------
add_stars <- function(pv) {
  ifelse(is.na(pv), "",
    ifelse(pv < 0.01, "$^{***}$",
      ifelse(pv < 0.05, "$^{**}$",
        ifelse(pv < 0.1, "$^{*}$", ""))))
}

fmt <- function(x, d = 2) sprintf(paste0("%.", d, "f"), x)

summary_log <- character()
slog <- function(...) {
  msg <- paste0(...)
  summary_log <<- c(summary_log, msg)
  message(msg)
}

# --- Load data ----------------------------------------------------------------
message("\n", strrep("=", 70))
message("LOADING DATA")
message(strrep("=", 70))

gs_est    <- readRDS(file.path(CLEANED_DIR, "skeleton_gs_est.rds"))
nongs_est <- readRDS(file.path(CLEANED_DIR, "skeleton_nongs_est.rds"))

gs_atp <- gs_est |> filter(tour == "ATP")
gs_wta <- gs_est |> filter(tour == "WTA")

message("  ATP GS: N = ", nrow(gs_atp), " (LL: ", sum(gs_atp$got_ll), ")")
message("  WTA GS: N = ", nrow(gs_wta), " (LL: ", sum(gs_wta$got_ll), ")")
message("  Non-GS: N = ", nrow(nongs_est))

# Ensure needed columns in nongs_est
if (!"pre_elo" %in% names(nongs_est)) {
  nongs_est <- nongs_est |> mutate(pre_elo = 1500, pre_elo_sq = 1500^2)
}
if (!"had_prior_ll" %in% names(nongs_est)) {
  nongs_est <- nongs_est |> mutate(had_prior_ll = 0L)
}
med_elo_nongs <- median(nongs_est$pre_elo, na.rm = TRUE)
if (is.na(med_elo_nongs)) med_elo_nongs <- 1500
nongs_est <- nongs_est |>
  mutate(pre_elo = coalesce(pre_elo, med_elo_nongs),
         pre_elo_sq = pre_elo^2)


# --- Stack horizons helper (from script 18) -----------------------------------
stack_horizons <- function(data, outcomes_base, horizons = c(4, 8, 12, 26, 52)) {
  stacked <- list()
  for (h in horizons) {
    h_label <- paste0(h, "w")
    row_data <- data |>
      transmute(
        player_id, tourney_id, tour, slam_year, got_ll,
        pre_rank_pts, pre_rank_pts_sq, player_age, had_prior_ll,
        pre_elo   = if ("pre_elo"   %in% names(data)) pre_elo   else NA_real_,
        pre_elo_sq = if ("pre_elo_sq" %in% names(data)) pre_elo_sq else NA_real_,
        md_matches_won = if ("md_matches_won" %in% names(data)) md_matches_won else NA_integer_,
        n_prior_gs_ll_won = if ("n_prior_gs_ll_won" %in% names(data)) n_prior_gs_ll_won else NA_integer_,
        rank_among_losers = if ("rank_among_losers" %in% names(data)) rank_among_losers else NA_integer_,
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
  bind_rows(stacked) |>
    mutate(horizon = factor(horizon, levels = paste0(horizons, "w")))
}

outcomes_base <- c("points_change", "n_main_draws", "n_matches_250plus", "elo_change")


# ==============================================================================
# FIX 1: HORIZON FIXED EFFECTS TEST
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 1: HORIZON FIXED EFFECTS TEST")
message(strrep("=", 70))

slog("## FIX 1: Horizon Fixed Effects Test\n")

# Stack GS data
stacked_atp <- stack_horizons(gs_atp, outcomes_base)
stacked_wta <- stack_horizons(gs_wta, outcomes_base)

# Create event_id = slam_year (this is the event FE in the stacked spec)
stacked_atp <- stacked_atp |> mutate(event_id = slam_year)
stacked_wta <- stacked_wta |> mutate(event_id = slam_year)

# Define BOTH specs
# Spec A (current): outcome ~ got_ll:factor(horizon) + controls | event_id
# Spec B (new):     outcome ~ got_ll:factor(horizon) + controls | event_id + horizon

horizon_fe_results <- list()

run_horizon_fe_test <- function(sdata, tour_label) {
  results <- list()
  for (ob in outcomes_base) {
    if (!ob %in% names(sdata)) next
    d <- sdata |> filter(!is.na(.data[[ob]]), !is.na(pre_rank_pts), !is.na(player_age))
    if (nrow(d) < 30) next

    # Spec A: no horizon FE (absorbed)
    fml_a <- as.formula(paste0(
      ob, " ~ got_ll:horizon + pre_rank_pts + pre_rank_pts_sq + player_age | event_id"
    ))
    # Spec B: with horizon FE
    fml_b <- as.formula(paste0(
      ob, " ~ got_ll:horizon + pre_rank_pts + pre_rank_pts_sq + player_age | event_id + horizon"
    ))

    fit_a <- tryCatch(feols(fml_a, data = d, vcov = ~player_id), error = function(e) NULL)
    fit_b <- tryCatch(feols(fml_b, data = d, vcov = ~player_id), error = function(e) NULL)

    if (is.null(fit_a) || is.null(fit_b)) next

    cf_a <- coef(fit_a)
    se_a <- sqrt(diag(vcov(fit_a)))
    cf_b <- coef(fit_b)
    se_b <- sqrt(diag(vcov(fit_b)))

    for (h in paste0(c(4, 8, 12, 26, 52), "w")) {
      cn <- paste0("got_ll:horizon", h)
      if (cn %in% names(cf_a) && cn %in% names(cf_b)) {
        results[[paste0(tour_label, "_", ob, "_", h)]] <- tibble(
          tour = tour_label, outcome = ob, horizon = h,
          coef_no_hfe = cf_a[cn], se_no_hfe = se_a[cn],
          coef_hfe    = cf_b[cn], se_hfe    = se_b[cn],
          diff_coef   = cf_b[cn] - cf_a[cn],
          pct_diff    = 100 * (cf_b[cn] - cf_a[cn]) / abs(cf_a[cn])
        )
      }
    }

    # Also store R-squared
    results[[paste0(tour_label, "_", ob, "_fit")]] <- tibble(
      tour = tour_label, outcome = ob, horizon = "fit_stats",
      coef_no_hfe = fitstat(fit_a, "r2")$r2,
      se_no_hfe   = NA_real_,
      coef_hfe    = fitstat(fit_b, "r2")$r2,
      se_hfe      = NA_real_,
      diff_coef   = fitstat(fit_b, "r2")$r2 - fitstat(fit_a, "r2")$r2,
      pct_diff    = NA_real_
    )
  }
  bind_rows(results)
}

hfe_atp <- run_horizon_fe_test(stacked_atp, "ATP")
hfe_wta <- run_horizon_fe_test(stacked_wta, "WTA")
hfe_all <- bind_rows(hfe_atp, hfe_wta)

# Determine if horizon FE matters
coef_rows <- hfe_all |> filter(horizon != "fit_stats")
max_pct_diff <- max(abs(coef_rows$pct_diff), na.rm = TRUE)
mean_pct_diff <- mean(abs(coef_rows$pct_diff), na.rm = TRUE)

horizon_fe_helps <- mean_pct_diff > 5  # threshold: 5% average change

slog("- Mean absolute % change in coefficients: ", fmt(mean_pct_diff, 1), "%")
slog("- Max absolute % change: ", fmt(max_pct_diff, 1), "%")
slog("- Horizon FE adoption decision: ", ifelse(horizon_fe_helps, "YES -- adopt horizon FE", "NO -- coefficients stable"))

# R-squared comparison
r2_rows <- hfe_all |> filter(horizon == "fit_stats")
for (i in seq_len(nrow(r2_rows))) {
  r <- r2_rows[i, ]
  slog("- ", r$tour, " ", r$outcome, ": R2 without HFE = ", fmt(r$coef_no_hfe, 4),
       ", R2 with HFE = ", fmt(r$coef_hfe, 4),
       ", improvement = ", fmt(r$diff_coef, 4))
}

# Write horizon FE test report
hfe_md <- c(
  "# Horizon Fixed Effects Test",
  paste0("Generated: ", Sys.time()),
  "",
  "## Specification Comparison",
  "- Spec A (no HFE): Y ~ got_ll:factor(horizon) + pre_rank_pts + pre_rank_pts_sq + player_age | event_id",
  "- Spec B (with HFE): Y ~ got_ll:factor(horizon) + pre_rank_pts + pre_rank_pts_sq + player_age | event_id + horizon",
  "",
  "## Coefficient Comparison",
  ""
)

for (i in seq_len(nrow(coef_rows))) {
  r <- coef_rows[i, ]
  hfe_md <- c(hfe_md, paste0(
    "- ", r$tour, " ", r$outcome, " @ ", r$horizon,
    ": no_HFE = ", fmt(r$coef_no_hfe),
    " (", fmt(r$se_no_hfe), ")",
    ", HFE = ", fmt(r$coef_hfe),
    " (", fmt(r$se_hfe), ")",
    ", diff = ", fmt(r$diff_coef),
    " (", fmt(r$pct_diff, 1), "%)"
  ))
}

hfe_md <- c(hfe_md, "",
  "## R-squared Comparison",
  ""
)
for (i in seq_len(nrow(r2_rows))) {
  r <- r2_rows[i, ]
  hfe_md <- c(hfe_md, paste0(
    "- ", r$tour, " ", r$outcome,
    ": no_HFE R2 = ", fmt(r$coef_no_hfe, 4),
    ", HFE R2 = ", fmt(r$coef_hfe, 4),
    ", improvement = ", fmt(r$diff_coef, 4)
  ))
}

hfe_md <- c(hfe_md, "",
  "## Decision",
  paste0("- Mean |%change| in treatment coefficients: ", fmt(mean_pct_diff, 1), "%"),
  paste0("- Threshold for adoption: 5%"),
  paste0("- Decision: ", ifelse(horizon_fe_helps,
    "ADOPT horizon FE -- coefficients change meaningfully",
    "DO NOT adopt -- coefficients are stable; horizon FE adds complexity without changing results"))
)

writeLines(hfe_md, file.path(OUTPUT_DIR, "horizon_fe_test.md"))
message("  Saved: horizon_fe_test.md")
slog("")

# Based on the decision, set the FE formula suffix for all subsequent specs
if (horizon_fe_helps) {
  FE_SUFFIX <- "event_id + horizon"
  slog(">>> Using event_id + horizon FE for all subsequent specs\n")
} else {
  FE_SUFFIX <- "event_id"
  slog(">>> Using event_id FE only for all subsequent specs\n")
}


# ==============================================================================
# FIX 2: HETEROGENEITY SPEC -- D and D x category dummies
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 2: HETEROGENEITY SPEC (D + D x category dummies)")
message(strrep("=", 70))

slog("## FIX 2: Heterogeneity with D + D x category dummies\n")

# Prepare stacked data with category variables
prepare_hetero_data <- function(data) {
  # Ranking quartiles
  qts <- quantile(data$pre_rank_pts, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  # Age median
  med_age <- median(data$player_age, na.rm = TRUE)

  data |>
    mutate(
      # Ranking quartiles: Q1 = lowest points (base), Q4 = highest
      rank_q = cut(pre_rank_pts, breaks = c(-Inf, qts[1], qts[2], qts[3], Inf),
                   labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE),
      rank_q2 = as.integer(rank_q == "Q2"),
      rank_q3 = as.integer(rank_q == "Q3"),
      rank_q4 = as.integer(rank_q == "Q4"),
      # Age: below-median (base), above-median
      age_above_med = as.integer(player_age >= med_age),
      # Prior LL: no prior (base), had prior
      prior_ll_dum = as.integer(had_prior_ll == 1),
      # Dose: 0 wins (base), 1 win, 2+ wins
      dose_1win  = as.integer(!is.na(md_matches_won) & md_matches_won == 1),
      dose_2plus = as.integer(!is.na(md_matches_won) & md_matches_won >= 2)
    )
}

gs_atp_h <- prepare_hetero_data(gs_atp)
gs_wta_h <- prepare_hetero_data(gs_wta)

# Stack with category variables preserved
stack_with_categories <- function(data) {
  stacked <- stack_horizons(data, outcomes_base)
  # Merge back category variables
  cats <- data |> select(player_id, tourney_id,
    rank_q, rank_q2, rank_q3, rank_q4,
    age_above_med, prior_ll_dum, dose_1win, dose_2plus)
  stacked |>
    left_join(cats, by = c("player_id", "tourney_id")) |>
    mutate(event_id = slam_year)
}

stacked_atp_h <- stack_with_categories(gs_atp_h)
stacked_wta_h <- stack_with_categories(gs_wta_h)

# Run heterogeneity regressions
# For each dimension, the spec is:
#   Y ~ got_ll + got_ll:cat_2 [+ got_ll:cat_3 ...] + controls | FE

run_hetero_interacted <- function(sdata, tour_label) {
  all_results <- list()

  for (ob in outcomes_base) {
    if (!ob %in% names(sdata)) next
    d <- sdata |> filter(!is.na(.data[[ob]]), !is.na(pre_rank_pts), !is.na(player_age))
    if (nrow(d) < 30) next

    controls <- "pre_rank_pts + pre_rank_pts_sq + player_age"

    # --- Ranking quartiles ---
    fml_rank <- as.formula(paste0(
      ob, " ~ got_ll + got_ll:rank_q2 + got_ll:rank_q3 + got_ll:rank_q4 + ",
      controls, " | ", FE_SUFFIX
    ))
    fit_rank <- tryCatch(feols(fml_rank, data = d, vcov = ~player_id), error = function(e) NULL)

    if (!is.null(fit_rank)) {
      cf <- coef(fit_rank)
      se <- sqrt(diag(vcov(fit_rank)))
      for (nm in c("got_ll", "got_ll:rank_q2", "got_ll:rank_q3", "got_ll:rank_q4")) {
        if (nm %in% names(cf)) {
          pv <- 2 * pnorm(-abs(cf[nm] / se[nm]))
          label <- if (nm == "got_ll") "D (Q1 base)" else paste0("D x ", gsub("got_ll:", "", nm))
          all_results[[paste0(tour_label, "_", ob, "_rank_", nm)]] <- tibble(
            tour = tour_label, outcome = ob, dimension = "Ranking",
            term = label, coef = cf[nm], se = se[nm], pvalue = pv,
            n_obs = nrow(d), n_units = n_distinct(paste0(d$player_id, "_", d$tourney_id))
          )
        }
      }
    }

    # --- Age ---
    fml_age <- as.formula(paste0(
      ob, " ~ got_ll + got_ll:age_above_med + ", controls, " | ", FE_SUFFIX
    ))
    fit_age <- tryCatch(feols(fml_age, data = d, vcov = ~player_id), error = function(e) NULL)

    if (!is.null(fit_age)) {
      cf <- coef(fit_age)
      se <- sqrt(diag(vcov(fit_age)))
      for (nm in c("got_ll", "got_ll:age_above_med")) {
        if (nm %in% names(cf)) {
          pv <- 2 * pnorm(-abs(cf[nm] / se[nm]))
          label <- if (nm == "got_ll") "D (young base)" else "D x Above-median age"
          all_results[[paste0(tour_label, "_", ob, "_age_", nm)]] <- tibble(
            tour = tour_label, outcome = ob, dimension = "Age",
            term = label, coef = cf[nm], se = se[nm], pvalue = pv,
            n_obs = nrow(d), n_units = n_distinct(paste0(d$player_id, "_", d$tourney_id))
          )
        }
      }
    }

    # --- Prior LL ---
    fml_prior <- as.formula(paste0(
      ob, " ~ got_ll + got_ll:prior_ll_dum + ", controls, " | ", FE_SUFFIX
    ))
    fit_prior <- tryCatch(feols(fml_prior, data = d, vcov = ~player_id), error = function(e) NULL)

    if (!is.null(fit_prior)) {
      cf <- coef(fit_prior)
      se <- sqrt(diag(vcov(fit_prior)))
      for (nm in c("got_ll", "got_ll:prior_ll_dum")) {
        if (nm %in% names(cf)) {
          pv <- 2 * pnorm(-abs(cf[nm] / se[nm]))
          label <- if (nm == "got_ll") "D (no prior base)" else "D x Had prior LL"
          all_results[[paste0(tour_label, "_", ob, "_prior_", nm)]] <- tibble(
            tour = tour_label, outcome = ob, dimension = "Prior LL",
            term = label, coef = cf[nm], se = se[nm], pvalue = pv,
            n_obs = nrow(d), n_units = n_distinct(paste0(d$player_id, "_", d$tourney_id))
          )
        }
      }
    }

    # --- Dose ---
    fml_dose <- as.formula(paste0(
      ob, " ~ got_ll + got_ll:dose_1win + got_ll:dose_2plus + ",
      controls, " | ", FE_SUFFIX
    ))
    fit_dose <- tryCatch(feols(fml_dose, data = d, vcov = ~player_id), error = function(e) NULL)

    if (!is.null(fit_dose)) {
      cf <- coef(fit_dose)
      se <- sqrt(diag(vcov(fit_dose)))
      for (nm in c("got_ll", "got_ll:dose_1win", "got_ll:dose_2plus")) {
        if (nm %in% names(cf)) {
          pv <- 2 * pnorm(-abs(cf[nm] / se[nm]))
          label <- if (nm == "got_ll") "D (0 wins base)" else if (grepl("1win", nm)) "D x 1 win" else "D x 2+ wins"
          all_results[[paste0(tour_label, "_", ob, "_dose_", nm)]] <- tibble(
            tour = tour_label, outcome = ob, dimension = "Dose",
            term = label, coef = cf[nm], se = se[nm], pvalue = pv,
            n_obs = nrow(d), n_units = n_distinct(paste0(d$player_id, "_", d$tourney_id))
          )
        }
      }
    }
  }
  bind_rows(all_results)
}

hetero_atp <- run_hetero_interacted(stacked_atp_h, "ATP")
hetero_wta <- run_hetero_interacted(stacked_wta_h, "WTA")

# Log key results
for (tour_df in list(hetero_atp, hetero_wta)) {
  if (nrow(tour_df) == 0) next
  tl <- tour_df$tour[1]
  # Focus on points_change
  pts <- tour_df |> filter(outcome == "points_change")
  for (i in seq_len(nrow(pts))) {
    r <- pts[i, ]
    slog("- ", tl, " points_change | ", r$dimension, " | ", r$term,
         ": coef = ", fmt(r$coef), " (", fmt(r$se), "), p = ", fmt(r$pvalue, 3))
  }
}
slog("")

# --- Build heterogeneity tables -----------------------------------------------
# Table structure: one panel per dimension, rows = D (base) + D x categories
# Columns = 4 outcomes (points, main_draws, matches_250plus, elo)

build_hetero_table <- function(res_df, tour_label, filename) {
  outcome_labels <- c(
    "points_change"     = "Points $\\Delta$",
    "n_main_draws"      = "Main draws",
    "n_matches_250plus" = "Matches 250+",
    "elo_change"        = "Elo $\\Delta$"
  )
  avail_outcomes <- intersect(unique(res_df$outcome), names(outcome_labels))
  n_cols <- length(avail_outcomes)

  tex <- c(
    paste0("\\begin{tabular}{l", paste(rep(" c", n_cols), collapse = ""), "}"),
    "\\toprule",
    paste0(" & ", paste(outcome_labels[avail_outcomes], collapse = " & "), " \\\\"),
    "\\midrule"
  )

  dimensions <- c("Ranking", "Age", "Prior LL", "Dose")

  for (dim in dimensions) {
    dim_data <- res_df |> filter(dimension == dim)
    if (nrow(dim_data) == 0) next

    tex <- c(tex, paste0("\\multicolumn{", n_cols + 1, "}{l}{\\textit{",
                         dim, " heterogeneity}} \\\\"))

    terms_order <- unique(dim_data$term)
    for (tm in terms_order) {
      cells <- character()
      se_cells <- character()
      for (ob in avail_outcomes) {
        r <- dim_data |> filter(outcome == ob, term == tm)
        if (nrow(r) == 0) {
          cells <- c(cells, "")
          se_cells <- c(se_cells, "")
        } else {
          cells <- c(cells, paste0(fmt(r$coef), add_stars(r$pvalue)))
          se_cells <- c(se_cells, paste0("(", fmt(r$se), ")"))
        }
      }
      # Clean term label for LaTeX
      tm_tex <- gsub("&", "\\\\&", tm)
      tm_tex <- gsub("x", "$\\\\times$", tm_tex)
      tex <- c(tex,
        paste0(tm_tex, " & ", paste(cells, collapse = " & "), " \\\\"),
        paste0(" & ", paste(se_cells, collapse = " & "), " \\\\")
      )
    }
    tex <- c(tex, "\\addlinespace")
  }

  # N row
  if (nrow(res_df) > 0) {
    n_obs <- res_df$n_obs[1]
    n_units <- res_df$n_units[1]
    tex <- c(tex, "\\midrule",
      paste0("$N$ (stacked) & \\multicolumn{", n_cols, "}{c}{", n_obs, "} \\\\"),
      paste0("Player-events & \\multicolumn{", n_cols, "}{c}{", n_units, "} \\\\"),
      paste0("Event FE & \\multicolumn{", n_cols, "}{c}{Yes} \\\\"),
      paste0("Horizon FE & \\multicolumn{", n_cols, "}{c}{",
             ifelse(horizon_fe_helps, "Yes", "No"), "} \\\\")
    )
  }

  tex <- c(tex, "\\bottomrule", "\\end{tabular}")
  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

if (nrow(hetero_atp) > 0) {
  build_hetero_table(hetero_atp, "ATP", "table_hetero_stacked_atp.tex")
}
if (nrow(hetero_wta) > 0) {
  build_hetero_table(hetero_wta, "WTA", "table_hetero_stacked_wta.tex")
}

# --- Build dose table (dedicated, with panels for ATP and WTA) ----------------
# Dose dimension only, all outcomes, both tours
build_dose_table <- function(atp_df, wta_df, filename) {
  outcome_labels <- c(
    "points_change"     = "Points $\\Delta$",
    "n_main_draws"      = "Main draws",
    "n_matches_250plus" = "Matches 250+",
    "elo_change"        = "Elo $\\Delta$"
  )

  dose_atp <- atp_df |> filter(dimension == "Dose")
  dose_wta <- wta_df |> filter(dimension == "Dose")
  avail_outcomes <- intersect(
    unique(c(dose_atp$outcome, dose_wta$outcome)),
    names(outcome_labels)
  )
  n_cols <- length(avail_outcomes)

  tex <- c(
    paste0("\\begin{tabular}{l", paste(rep(" c", n_cols), collapse = ""), "}"),
    "\\toprule",
    paste0(" & ", paste(outcome_labels[avail_outcomes], collapse = " & "), " \\\\"),
    "\\midrule"
  )

  for (panel_data in list(list(df = dose_atp, label = "Panel A: ATP GS"),
                          list(df = dose_wta, label = "Panel B: WTA GS"))) {
    dd <- panel_data$df
    if (nrow(dd) == 0) next
    tex <- c(tex, paste0("\\multicolumn{", n_cols + 1, "}{l}{\\textit{",
                         panel_data$label, "}} \\\\"))

    terms_order <- unique(dd$term)
    for (tm in terms_order) {
      cells <- character()
      se_cells <- character()
      for (ob in avail_outcomes) {
        r <- dd |> filter(outcome == ob, term == tm)
        if (nrow(r) == 0) {
          cells <- c(cells, "")
          se_cells <- c(se_cells, "")
        } else {
          cells <- c(cells, paste0(fmt(r$coef), add_stars(r$pvalue)))
          se_cells <- c(se_cells, paste0("(", fmt(r$se), ")"))
        }
      }
      tm_tex <- gsub("x", "$\\\\times$", tm)
      tex <- c(tex,
        paste0(tm_tex, " & ", paste(cells, collapse = " & "), " \\\\"),
        paste0(" & ", paste(se_cells, collapse = " & "), " \\\\")
      )
    }
    if (nrow(dd) > 0) {
      tex <- c(tex, paste0("$N$ & \\multicolumn{", n_cols, "}{c}{", dd$n_obs[1], "} \\\\"))
    }
    tex <- c(tex, "\\addlinespace")
  }

  tex <- c(tex,
    "\\midrule",
    paste0("Event FE & \\multicolumn{", n_cols, "}{c}{Yes} \\\\"),
    paste0("Horizon FE & \\multicolumn{", n_cols, "}{c}{",
           ifelse(horizon_fe_helps, "Yes", "No"), "} \\\\"),
    "\\bottomrule", "\\end{tabular}"
  )
  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

build_dose_table(hetero_atp, hetero_wta, "table_dose_stacked.tex")

# Save hetero results
saveRDS(list(atp = hetero_atp, wta = hetero_wta),
        file.path(CLEANED_DIR, "hetero_interacted_results.rds"))


# ==============================================================================
# FIX 3: VERIFIED LOTTERY SUBSAMPLE -- STACKED
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 3: VERIFIED LOTTERY SUBSAMPLE -- STACKED")
message(strrep("=", 70))

slog("## FIX 3: Verified Lottery Subsample (Stacked)\n")

# Identify verified events: events where LL recipient was NOT the highest-ranked
# qualifying loser (i.e., rank_among_losers > 1 for the LL recipient)
verified_events <- gs_est |>
  filter(got_ll == 1) |>
  group_by(tourney_id) |>
  summarise(min_ll_rank = min(rank_among_losers, na.rm = TRUE), .groups = "drop") |>
  filter(min_ll_rank > 1) |>
  pull(tourney_id)

message("  Verified lottery events: ", length(verified_events))

# Filter stacked data to verified events only
stacked_atp_ver <- stacked_atp_h |> filter(tourney_id %in% verified_events)
stacked_wta_ver <- stacked_wta_h |> filter(tourney_id %in% verified_events)

n_ver_atp <- n_distinct(paste0(stacked_atp_ver$player_id, "_", stacked_atp_ver$tourney_id))
n_ver_wta <- n_distinct(paste0(stacked_wta_ver$player_id, "_", stacked_wta_ver$tourney_id))

message("  ATP verified: ", nrow(stacked_atp_ver), " stacked rows, ",
        n_ver_atp, " player-events")
message("  WTA verified: ", nrow(stacked_wta_ver), " stacked rows, ",
        n_ver_wta, " player-events")

slog("- ATP verified: N_stacked = ", nrow(stacked_atp_ver), ", N_units = ", n_ver_atp)
slog("- WTA verified: N_stacked = ", nrow(stacked_wta_ver), ", N_units = ", n_ver_wta)

# Run stacked spec on verified subsample
run_verified_stacked <- function(sdata, tour_label) {
  results <- list()
  for (ob in outcomes_base) {
    if (!ob %in% names(sdata)) next
    d <- sdata |> filter(!is.na(.data[[ob]]), !is.na(pre_rank_pts), !is.na(player_age))
    # Need at least 2 levels of event_id for FE
    eid_counts <- d |> count(event_id) |> filter(n >= 2)
    d <- d |> filter(event_id %in% eid_counts$event_id)
    if (nrow(d) < 20 || sum(d$got_ll == 1) < 3) next

    fml <- as.formula(paste0(
      ob, " ~ got_ll:horizon + pre_rank_pts + pre_rank_pts_sq + player_age | ", FE_SUFFIX
    ))
    fit <- tryCatch(feols(fml, data = d, vcov = ~player_id), error = function(e) NULL)
    if (is.null(fit)) next

    cf <- coef(fit)
    se <- sqrt(diag(vcov(fit)))
    for (h in paste0(c(4, 8, 12, 26, 52), "w")) {
      cn <- paste0("got_ll:horizon", h)
      if (cn %in% names(cf)) {
        pv <- 2 * pnorm(-abs(cf[cn] / se[cn]))
        results[[paste0(ob, "_", h)]] <- tibble(
          tour = tour_label, outcome = ob, horizon = h,
          coef = cf[cn], se = se[cn], pvalue = pv,
          n_obs = nrow(d),
          n_units = n_distinct(paste0(d$player_id, "_", d$tourney_id))
        )
      }
    }
  }
  bind_rows(results)
}

ver_res_atp <- run_verified_stacked(stacked_atp_ver, "ATP")
ver_res_wta <- run_verified_stacked(stacked_wta_ver, "WTA")

for (tour_df in list(ver_res_atp, ver_res_wta)) {
  if (nrow(tour_df) == 0) next
  for (i in seq_len(nrow(tour_df))) {
    r <- tour_df[i, ]
    slog("- Verified ", r$tour, " ", r$outcome, " @ ", r$horizon,
         ": coef = ", fmt(r$coef), ", SE = ", fmt(r$se),
         ", p = ", fmt(r$pvalue, 3))
  }
}
slog("")

# Build verified stacked table (ATP and WTA panels)
build_verified_stacked_tex <- function(atp_res, wta_res, filename) {
  outcome_labels <- c(
    "points_change"     = "Points $\\Delta$",
    "n_main_draws"      = "Main draws",
    "n_matches_250plus" = "Matches 250+",
    "elo_change"        = "Elo $\\Delta$"
  )
  horizons <- c("4w", "8w", "12w", "26w", "52w")
  n_hor <- length(horizons)

  tex <- c(
    paste0("\\begin{tabular}{l", paste(rep(" c", n_hor), collapse = ""), "}"),
    "\\toprule",
    paste0(" & ", paste(horizons, collapse = " & "), " \\\\"),
    "\\midrule"
  )

  for (panel in list(list(df = atp_res, label = "Panel A: ATP GS"),
                     list(df = wta_res, label = "Panel B: WTA GS"))) {
    pd <- panel$df
    if (nrow(pd) == 0) next
    tex <- c(tex, paste0("\\multicolumn{", n_hor + 1, "}{l}{\\textit{",
                         panel$label, "}} \\\\"))

    for (ob in names(outcome_labels)) {
      ob_data <- pd |> filter(outcome == ob)
      if (nrow(ob_data) == 0) next

      cells <- character()
      se_cells <- character()
      for (h in horizons) {
        r <- ob_data |> filter(horizon == h)
        if (nrow(r) == 0) {
          cells <- c(cells, "")
          se_cells <- c(se_cells, "")
        } else {
          cells <- c(cells, paste0(fmt(r$coef), add_stars(r$pvalue)))
          se_cells <- c(se_cells, paste0("(", fmt(r$se), ")"))
        }
      }
      tex <- c(tex,
        paste0(outcome_labels[ob], " & ", paste(cells, collapse = " & "), " \\\\"),
        paste0(" & ", paste(se_cells, collapse = " & "), " \\\\"),
        "\\addlinespace"
      )
    }

    if (nrow(pd) > 0) {
      tex <- c(tex,
        paste0("$N$ (stacked) & \\multicolumn{", n_hor, "}{c}{", pd$n_obs[1], "} \\\\"),
        paste0("Player-events & \\multicolumn{", n_hor, "}{c}{", pd$n_units[1], "} \\\\"),
        "\\addlinespace"
      )
    }
  }

  tex <- c(tex,
    "\\midrule",
    paste0("Event FE & \\multicolumn{", n_hor, "}{c}{Yes} \\\\"),
    paste0("Horizon FE & \\multicolumn{", n_hor, "}{c}{",
           ifelse(horizon_fe_helps, "Yes", "No"), "} \\\\"),
    "\\bottomrule", "\\end{tabular}"
  )
  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

build_verified_stacked_tex(ver_res_atp, ver_res_wta, "table_verified_stacked.tex")


# ==============================================================================
# FIX 4: FIRST-LL-ONLY -- STACKED (n_prior_gs_ll_won == 0)
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 4: FIRST-LL-ONLY RESTRICTION -- STACKED")
message(strrep("=", 70))

slog("## FIX 4: First-LL-Only Stacked\n")

# Filter: keep player-events where n_prior_gs_ll_won == 0 at time of event
# For control players, n_prior_gs_ll_won should also be 0 (never had GS LL before)
gs_atp_first <- gs_atp |> filter(n_prior_gs_ll_won == 0)
gs_wta_first <- gs_wta |> filter(n_prior_gs_ll_won == 0)

message("  ATP first-LL: N = ", nrow(gs_atp_first),
        " (LL: ", sum(gs_atp_first$got_ll), ", Control: ", sum(gs_atp_first$got_ll == 0), ")")
message("  WTA first-LL: N = ", nrow(gs_wta_first),
        " (LL: ", sum(gs_wta_first$got_ll), ", Control: ", sum(gs_wta_first$got_ll == 0), ")")

slog("- ATP first-LL only: N = ", nrow(gs_atp_first),
     " (LL: ", sum(gs_atp_first$got_ll), ")")
slog("- WTA first-LL only: N = ", nrow(gs_wta_first),
     " (LL: ", sum(gs_wta_first$got_ll), ")")

# Stack and run
stacked_first_atp <- stack_horizons(gs_atp_first, outcomes_base) |> mutate(event_id = slam_year)
stacked_first_wta <- stack_horizons(gs_wta_first, outcomes_base) |> mutate(event_id = slam_year)

run_firstll_stacked <- function(sdata, tour_label) {
  results <- list()
  for (ob in outcomes_base) {
    if (!ob %in% names(sdata)) next
    d <- sdata |> filter(!is.na(.data[[ob]]), !is.na(pre_rank_pts), !is.na(player_age))
    eid_counts <- d |> count(event_id) |> filter(n >= 2)
    d <- d |> filter(event_id %in% eid_counts$event_id)
    if (nrow(d) < 20 || sum(d$got_ll == 1) < 3) next

    fml <- as.formula(paste0(
      ob, " ~ got_ll:horizon + pre_rank_pts + pre_rank_pts_sq + player_age | ", FE_SUFFIX
    ))
    fit <- tryCatch(feols(fml, data = d, vcov = ~player_id), error = function(e) NULL)
    if (is.null(fit)) next

    cf <- coef(fit)
    se <- sqrt(diag(vcov(fit)))
    for (h in paste0(c(4, 8, 12, 26, 52), "w")) {
      cn <- paste0("got_ll:horizon", h)
      if (cn %in% names(cf)) {
        pv <- 2 * pnorm(-abs(cf[cn] / se[cn]))
        results[[paste0(ob, "_", h)]] <- tibble(
          tour = tour_label, outcome = ob, horizon = h,
          coef = cf[cn], se = se[cn], pvalue = pv,
          n_obs = nrow(d),
          n_units = n_distinct(paste0(d$player_id, "_", d$tourney_id))
        )
      }
    }
  }
  bind_rows(results)
}

first_res_atp <- run_firstll_stacked(stacked_first_atp, "ATP")
first_res_wta <- run_firstll_stacked(stacked_first_wta, "WTA")

for (tour_df in list(first_res_atp, first_res_wta)) {
  if (nrow(tour_df) == 0) next
  for (i in seq_len(nrow(tour_df))) {
    r <- tour_df[i, ]
    slog("- First-LL ", r$tour, " ", r$outcome, " @ ", r$horizon,
         ": coef = ", fmt(r$coef), ", SE = ", fmt(r$se),
         ", p = ", fmt(r$pvalue, 3))
  }
}
slog("")

# Build first-LL tables
build_firstll_tex <- function(res_df, tour_label, filename) {
  outcome_labels <- c(
    "points_change"     = "Points $\\Delta$",
    "n_main_draws"      = "Main draws",
    "n_matches_250plus" = "Matches 250+",
    "elo_change"        = "Elo $\\Delta$"
  )
  horizons <- c("4w", "8w", "12w", "26w", "52w")
  n_hor <- length(horizons)
  avail <- intersect(unique(res_df$outcome), names(outcome_labels))

  tex <- c(
    paste0("\\begin{tabular}{l", paste(rep(" c", n_hor), collapse = ""), "}"),
    "\\toprule",
    paste0(" & ", paste(horizons, collapse = " & "), " \\\\"),
    "\\midrule"
  )

  for (ob in names(outcome_labels)) {
    ob_data <- res_df |> filter(outcome == ob)
    if (nrow(ob_data) == 0) next

    cells <- character()
    se_cells <- character()
    for (h in horizons) {
      r <- ob_data |> filter(horizon == h)
      if (nrow(r) == 0) {
        cells <- c(cells, "")
        se_cells <- c(se_cells, "")
      } else {
        cells <- c(cells, paste0(fmt(r$coef), add_stars(r$pvalue)))
        se_cells <- c(se_cells, paste0("(", fmt(r$se), ")"))
      }
    }
    tex <- c(tex,
      paste0(outcome_labels[ob], " & ", paste(cells, collapse = " & "), " \\\\"),
      paste0(" & ", paste(se_cells, collapse = " & "), " \\\\"),
      "\\addlinespace"
    )
  }

  if (nrow(res_df) > 0) {
    tex <- c(tex, "\\midrule",
      paste0("$N$ (stacked) & \\multicolumn{", n_hor, "}{c}{", res_df$n_obs[1], "} \\\\"),
      paste0("Player-events & \\multicolumn{", n_hor, "}{c}{", res_df$n_units[1], "} \\\\"),
      paste0("Event FE & \\multicolumn{", n_hor, "}{c}{Yes} \\\\"),
      paste0("Horizon FE & \\multicolumn{", n_hor, "}{c}{",
             ifelse(horizon_fe_helps, "Yes", "No"), "} \\\\")
    )
  }

  tex <- c(tex, "\\bottomrule", "\\end{tabular}")
  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

if (nrow(first_res_atp) > 0) {
  build_firstll_tex(first_res_atp, "ATP", "table_firstll_stacked_atp.tex")
}
if (nrow(first_res_wta) > 0) {
  build_firstll_tex(first_res_wta, "WTA", "table_firstll_stacked_wta.tex")
}


# ==============================================================================
# FIX 5: NON-GS EQUIVALENTS
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 5: NON-GS EQUIVALENTS (HETEROGENEITY, DOSE)")
message(strrep("=", 70))

slog("## FIX 5: Non-GS Equivalents\n")

# Non-GS uses IV (peer_component), separate regressions per horizon
nongs_atp <- nongs_est |> filter(tour == "ATP", !is.na(peer_component))
nongs_wta <- nongs_est |> filter(tour == "WTA", !is.na(peer_component))

message("  Non-GS ATP (with IV): N = ", nrow(nongs_atp))
message("  Non-GS WTA (with IV): N = ", nrow(nongs_wta))

# Prepare non-GS category variables
prepare_hetero_nongs <- function(data) {
  qts <- quantile(data$pre_rank_pts, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  med_age <- median(data$player_age, na.rm = TRUE)

  data |> mutate(
    rank_q = cut(pre_rank_pts, breaks = c(-Inf, qts[1], qts[2], qts[3], Inf),
                 labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE),
    rank_q2 = as.integer(rank_q == "Q2"),
    rank_q3 = as.integer(rank_q == "Q3"),
    rank_q4 = as.integer(rank_q == "Q4"),
    age_above_med = as.integer(player_age >= med_age),
    prior_ll_dum = as.integer(had_prior_ll == 1),
    dose_1win  = as.integer(!is.na(md_matches_won) & md_matches_won == 1),
    dose_2plus = as.integer(!is.na(md_matches_won) & md_matches_won >= 2)
  )
}

nongs_atp_h <- prepare_hetero_nongs(nongs_atp)
nongs_wta_h <- prepare_hetero_nongs(nongs_wta)

# Non-GS heterogeneity uses IV regressions per horizon
# For interactions, we instrument got_ll with peer_component and include interactions
# in the second stage as exogenous regressors.
# However, interacting endogenous D with exogenous dummies requires multiple instruments.
# Simpler approach: run the IV spec on subsamples (split-sample heterogeneity)
# This is the standard approach when D is instrumented.

run_hetero_nongs_iv <- function(data, tour_label) {
  results <- list()
  horizons <- c(4, 8, 12, 26, 52)

  zpre <- "pre_rank_pts + pre_rank_pts_sq + player_age"
  if (sum(!is.na(data$pre_elo)) > nrow(data) * 0.5) {
    zpre <- paste0(zpre, " + pre_elo + pre_elo_sq")
  }

  # Define subgroups
  med_pts <- median(data$pre_rank_pts, na.rm = TRUE)
  med_age <- median(data$player_age, na.rm = TRUE)

  subgroups <- list(
    list(label = "High ranking pts", data = data |> filter(pre_rank_pts >= med_pts), dim = "Ranking"),
    list(label = "Low ranking pts",  data = data |> filter(pre_rank_pts < med_pts),  dim = "Ranking"),
    list(label = "Older",            data = data |> filter(player_age >= med_age),   dim = "Age"),
    list(label = "Younger",          data = data |> filter(player_age < med_age),    dim = "Age"),
    list(label = "Had prior LL",     data = data |> filter(had_prior_ll == 1),       dim = "Prior LL"),
    list(label = "No prior LL",      data = data |> filter(had_prior_ll == 0),       dim = "Prior LL")
  )

  for (sg in subgroups) {
    sg_data <- sg$data
    if (nrow(sg_data) < 50) next

    for (h in horizons) {
      h_lab <- paste0(h, "w")
      for (ob in outcomes_base) {
        col <- paste0(ob, "_", h, "w")
        if (!col %in% names(sg_data)) next
        ok <- !is.na(sg_data[[col]]) & !is.na(sg_data$peer_component)
        if (sum(ok) < 30) next

        fit <- tryCatch(
          feols(as.formula(paste0(col, " ~ ", zpre, " | year | got_ll ~ peer_component")),
                data = sg_data[ok, ], vcov = ~player_id),
          error = function(e) NULL
        )
        if (is.null(fit) || !"fit_got_ll" %in% names(coef(fit))) next

        cf <- coef(fit)["fit_got_ll"]
        se_val <- sqrt(vcov(fit)["fit_got_ll", "fit_got_ll"])
        pv <- 2 * pnorm(-abs(cf / se_val))

        results[[paste0(sg$label, "_", ob, "_", h_lab)]] <- tibble(
          tour = tour_label, outcome = ob, horizon = h_lab,
          dimension = sg$dim, term = sg$label,
          coef = cf, se = se_val, pvalue = pv,
          n_obs = sum(ok), n_units = n_distinct(sg_data$player_id[ok])
        )
      }
    }
  }
  bind_rows(results)
}

hetero_nongs_atp <- run_hetero_nongs_iv(nongs_atp_h, "ATP non-GS")
hetero_nongs_wta <- run_hetero_nongs_iv(nongs_wta_h, "WTA non-GS")

# Log key results (26w points only)
for (tour_df in list(hetero_nongs_atp, hetero_nongs_wta)) {
  if (nrow(tour_df) == 0) next
  pts26 <- tour_df |> filter(outcome == "points_change", horizon == "26w")
  for (i in seq_len(nrow(pts26))) {
    r <- pts26[i, ]
    slog("- ", r$tour, " points_change@26w | ", r$dimension, " | ", r$term,
         ": coef = ", fmt(r$coef), ", SE = ", fmt(r$se), ", p = ", fmt(r$pvalue, 3))
  }
}
slog("")

# Build non-GS hetero tables
# Format: subgroup rows, horizon columns, for points_change outcome (main)
build_hetero_nongs_tex <- function(res_df, tour_label, filename) {
  horizons <- c("4w", "8w", "12w", "26w", "52w")
  n_hor <- length(horizons)

  # Focus on points_change for main table
  pts_df <- res_df |> filter(outcome == "points_change")
  if (nrow(pts_df) == 0) {
    message("  No points_change results for ", tour_label, " -- skipping table")
    return(invisible(NULL))
  }

  tex <- c(
    paste0("\\begin{tabular}{l", paste(rep(" c", n_hor), collapse = ""), "}"),
    "\\toprule",
    paste0("Subgroup & ", paste(horizons, collapse = " & "), " \\\\"),
    "\\midrule"
  )

  dims <- unique(pts_df$dimension)
  for (dm in dims) {
    dim_data <- pts_df |> filter(dimension == dm)
    tex <- c(tex, paste0("\\multicolumn{", n_hor + 1, "}{l}{\\textit{", dm, "}} \\\\"))

    for (tm in unique(dim_data$term)) {
      cells <- character()
      se_cells <- character()
      for (h in horizons) {
        r <- dim_data |> filter(term == tm, horizon == h)
        if (nrow(r) == 0) {
          cells <- c(cells, "")
          se_cells <- c(se_cells, "")
        } else {
          cells <- c(cells, paste0(fmt(r$coef), add_stars(r$pvalue)))
          se_cells <- c(se_cells, paste0("(", fmt(r$se), ")"))
        }
      }
      tex <- c(tex,
        paste0(tm, " & ", paste(cells, collapse = " & "), " \\\\"),
        paste0(" & ", paste(se_cells, collapse = " & "), " \\\\")
      )
    }
    tex <- c(tex, "\\addlinespace")
  }

  tex <- c(tex,
    "\\midrule",
    paste0("Year FE & \\multicolumn{", n_hor, "}{c}{Yes} \\\\"),
    paste0("IV (peer component) & \\multicolumn{", n_hor, "}{c}{Yes} \\\\"),
    "\\bottomrule", "\\end{tabular}"
  )
  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

if (nrow(hetero_nongs_atp) > 0) {
  build_hetero_nongs_tex(hetero_nongs_atp, "ATP non-GS", "table_hetero_stacked_nongs_atp.tex")
}
if (nrow(hetero_nongs_wta) > 0) {
  build_hetero_nongs_tex(hetero_nongs_wta, "WTA non-GS", "table_hetero_stacked_nongs_wta.tex")
}

# --- Non-GS dose table (IV, split by dose subgroup) --------------------------
# For dose in non-GS, we can't easily do D x dose in IV.
# Instead, run the standard IV on dose-defined subsamples.
run_dose_nongs_iv <- function(data, tour_label) {
  results <- list()
  horizons <- c(4, 8, 12, 26, 52)

  zpre <- "pre_rank_pts + pre_rank_pts_sq + player_age"
  if (sum(!is.na(data$pre_elo)) > nrow(data) * 0.5) {
    zpre <- paste0(zpre, " + pre_elo + pre_elo_sq")
  }

  dose_groups <- list(
    list(label = "0 MD wins", data = data |> filter(is.na(md_matches_won) | md_matches_won == 0)),
    list(label = "1 MD win",  data = data |> filter(!is.na(md_matches_won) & md_matches_won == 1)),
    list(label = "2+ MD wins", data = data |> filter(!is.na(md_matches_won) & md_matches_won >= 2))
  )

  for (dg in dose_groups) {
    dg_data <- dg$data
    if (nrow(dg_data) < 30) next

    for (h in horizons) {
      h_lab <- paste0(h, "w")
      for (ob in outcomes_base) {
        col <- paste0(ob, "_", h, "w")
        if (!col %in% names(dg_data)) next
        ok <- !is.na(dg_data[[col]]) & !is.na(dg_data$peer_component)
        if (sum(ok) < 20) next

        fit <- tryCatch(
          feols(as.formula(paste0(col, " ~ ", zpre, " | year | got_ll ~ peer_component")),
                data = dg_data[ok, ], vcov = ~player_id),
          error = function(e) NULL
        )
        if (is.null(fit) || !"fit_got_ll" %in% names(coef(fit))) next

        cf <- coef(fit)["fit_got_ll"]
        se_val <- sqrt(vcov(fit)["fit_got_ll", "fit_got_ll"])
        pv <- 2 * pnorm(-abs(cf / se_val))

        results[[paste0(dg$label, "_", ob, "_", h_lab)]] <- tibble(
          tour = tour_label, outcome = ob, horizon = h_lab,
          term = dg$label,
          coef = cf, se = se_val, pvalue = pv,
          n_obs = sum(ok), n_units = n_distinct(dg_data$player_id[ok])
        )
      }
    }
  }
  bind_rows(results)
}

dose_nongs_atp <- run_dose_nongs_iv(nongs_atp_h, "ATP non-GS")
dose_nongs_wta <- run_dose_nongs_iv(nongs_wta_h, "WTA non-GS")

# Build non-GS dose table
build_dose_nongs_tex <- function(atp_res, wta_res, filename) {
  outcome_labels <- c(
    "points_change"     = "Points $\\Delta$",
    "n_main_draws"      = "Main draws",
    "n_matches_250plus" = "Matches 250+",
    "elo_change"        = "Elo $\\Delta$"
  )
  horizons <- c("4w", "8w", "12w", "26w", "52w")
  n_hor <- length(horizons)

  tex <- c(
    paste0("\\begin{tabular}{l", paste(rep(" c", n_hor), collapse = ""), "}"),
    "\\toprule",
    paste0("Dose subgroup & ", paste(horizons, collapse = " & "), " \\\\"),
    "\\midrule"
  )

  for (panel in list(list(df = atp_res, label = "Panel A: ATP non-GS IV"),
                     list(df = wta_res, label = "Panel B: WTA non-GS IV"))) {
    pd <- panel$df |> filter(outcome == "points_change")
    if (nrow(pd) == 0) next
    tex <- c(tex, paste0("\\multicolumn{", n_hor + 1, "}{l}{\\textit{",
                         panel$label, "}} \\\\"))

    for (tm in unique(pd$term)) {
      cells <- character()
      se_cells <- character()
      for (h in horizons) {
        r <- pd |> filter(term == tm, horizon == h)
        if (nrow(r) == 0) {
          cells <- c(cells, "")
          se_cells <- c(se_cells, "")
        } else {
          cells <- c(cells, paste0(fmt(r$coef), add_stars(r$pvalue)))
          se_cells <- c(se_cells, paste0("(", fmt(r$se), ")"))
        }
      }
      tex <- c(tex,
        paste0(tm, " & ", paste(cells, collapse = " & "), " \\\\"),
        paste0(" & ", paste(se_cells, collapse = " & "), " \\\\")
      )
    }
    tex <- c(tex, "\\addlinespace")
  }

  tex <- c(tex,
    "\\midrule",
    paste0("Year FE & \\multicolumn{", n_hor, "}{c}{Yes} \\\\"),
    paste0("IV (peer component) & \\multicolumn{", n_hor, "}{c}{Yes} \\\\"),
    "\\bottomrule", "\\end{tabular}"
  )
  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

if (nrow(dose_nongs_atp) > 0 || nrow(dose_nongs_wta) > 0) {
  build_dose_nongs_tex(dose_nongs_atp, dose_nongs_wta, "table_dose_stacked_nongs.tex")
}

# Save all non-GS results
saveRDS(list(
  hetero_nongs_atp = hetero_nongs_atp,
  hetero_nongs_wta = hetero_nongs_wta,
  dose_nongs_atp = dose_nongs_atp,
  dose_nongs_wta = dose_nongs_wta
), file.path(CLEANED_DIR, "nongs_hetero_dose_results.rds"))


# ==============================================================================
# WRITE SUMMARY
# ==============================================================================
message("\n", strrep("=", 70))
message("WRITING SUMMARY")
message(strrep("=", 70))

summary_header <- c(
  "# Equation Fixes Summary (19_equation_fixes.R)",
  paste0("Generated: ", Sys.time()),
  ""
)

writeLines(c(summary_header, summary_log),
           file.path(OUTPUT_DIR, "equation_fixes_summary.md"))
message("  Saved: equation_fixes_summary.md")

message("\n", strrep("=", 70))
message("ALL EQUATION FIXES COMPLETE")
message(strrep("=", 70))
message("Tables saved:")
message("  - table_hetero_stacked_atp.tex")
message("  - table_hetero_stacked_wta.tex")
message("  - table_dose_stacked.tex")
message("  - table_verified_stacked.tex")
message("  - table_firstll_stacked_atp.tex")
message("  - table_firstll_stacked_wta.tex")
message("  - table_hetero_stacked_nongs_atp.tex")
message("  - table_hetero_stacked_nongs_wta.tex")
message("  - table_dose_stacked_nongs.tex")
message("Output saved:")
message("  - horizon_fe_test.md")
message("  - equation_fixes_summary.md")
