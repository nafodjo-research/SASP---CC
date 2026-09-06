# ==============================================================================
# 41_fixes.R
# Five targeted fixes:
#   FIX 1: Cross-tab table (LL eligibility x wins) for GS and NonGS
#   FIX 2: Add winning percentage outcome to skeletons + regenerate dynamic tables
#   FIX 3: Change dose from -log(pi) to -logit(pi)
#   FIX 4: Reformat heterogeneity tables (side-by-side base + interaction)
#
# Inputs:
#   Data/cleaned/skeleton_gs_est_v4.rds
#   Data/cleaned/skeleton_nongs_est_v7.rds
#   Data/cleaned/performance_dose.rds
#   Data/raw/atp_main_matches.rds, atp_qual_chall_matches.rds
#   Data/raw/wta_main_matches.rds, wta_qual_itf_matches.rds
#
# Outputs:
#   Tables/table_ll_careers_gs.tex
#   Tables/table_ll_careers_nongs.tex
#   Tables/table_dynamic_stacked_atp.tex  (updated with win_pct row)
#   Tables/table_dynamic_stacked_wta.tex
#   Tables/table_dynamic_stacked_nongs_atp.tex
#   Tables/table_dynamic_stacked_nongs_wta.tex
#   Tables/table_dose_stacked.tex  (updated with -logit dose)
#   Tables/table_perf_prob_stacked.tex
#   Tables/table_dose_stacked_nongs.tex
#   Tables/table_perf_prob_stacked_nongs.tex
#   Tables/table_hetero_stacked_atp.tex  (reformatted)
#   Tables/table_hetero_stacked_wta.tex
#   Tables/table_hetero_stacked_nongs_atp.tex
#   Tables/table_hetero_stacked_nongs_wta.tex
#   Data/cleaned/skeleton_gs_est_v5.rds
#   Data/cleaned/skeleton_nongs_est_v8.rds
#   Data/cleaned/performance_dose.rds  (updated)
#
# Dependencies: dplyr, fixest, data.table, here
# ==============================================================================

set.seed(20260327)

# --- Packages -----------------------------------------------------------------
library(dplyr)
library(fixest)
library(data.table)
library(here)

# --- Paths --------------------------------------------------------------------
RAW_DIR     <- here("Data", "raw")
CLEANED_DIR <- here("Data", "cleaned")
TABLES_DIR  <- here("Tables")
OUTPUT_DIR  <- here("Output")
for (d in c(CLEANED_DIR, TABLES_DIR, OUTPUT_DIR))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

# --- Helpers (inline) ---------------------------------------------------------
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

gs  <- readRDS(file.path(CLEANED_DIR, "skeleton_gs_est_v4.rds"))
ngs <- readRDS(file.path(CLEANED_DIR, "skeleton_nongs_est_v7.rds"))

slog("GS skeleton: ", nrow(gs), " rows (", sum(gs$tour == "ATP"), " ATP, ",
     sum(gs$tour == "WTA"), " WTA)")
slog("NonGS skeleton: ", nrow(ngs), " rows (", sum(ngs$tour == "ATP"), " ATP, ",
     sum(ngs$tour == "WTA"), " WTA)")

# ==============================================================================
# FIX 1: CROSS-TAB TABLES (LL eligibility x wins)
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 1: CROSS-TAB TABLES")
message(strrep("=", 70))

generate_crosstab_table <- function(df, label) {
  # For each player: count opportunities and wins
  player_summary <- df %>%
    group_by(tour, player_id) %>%
    summarise(
      n_opportunities = n(),
      n_won = sum(got_ll == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      opp_cat = ifelse(n_opportunities >= 5, "5+", as.character(n_opportunities)),
      won_cat = ifelse(n_won >= 3, "3+", as.character(n_won))
    )

  # Order categories
  opp_levels <- c("1", "2", "3", "4", "5+")
  won_levels <- c("0", "1", "2", "3+")

  player_summary$opp_cat <- factor(player_summary$opp_cat, levels = opp_levels)
  player_summary$won_cat <- factor(player_summary$won_cat, levels = won_levels)

  # Build cross-tab for each tour
  build_panel <- function(tour_label) {
    sub <- player_summary %>% filter(tour == tour_label)
    ct <- table(sub$opp_cat, sub$won_cat)
    # Ensure all levels present
    full_ct <- matrix(0L, nrow = length(opp_levels), ncol = length(won_levels),
                      dimnames = list(opp_levels, won_levels))
    for (r in rownames(ct)) {
      for (cc in colnames(ct)) {
        full_ct[r, cc] <- ct[r, cc]
      }
    }
    full_ct
  }

  ct_atp <- build_panel("ATP")
  ct_wta <- build_panel("WTA")

  slog("  ", label, " cross-tab -- ATP players: ",
       sum(ct_atp), ", WTA players: ", sum(ct_wta))

  # Generate LaTeX
  lines <- character()
  lines <- c(lines, "\\begin{tabular}{l cccc cccc}")
  lines <- c(lines, "\\toprule")
  lines <- c(lines, paste0(
    " & \\multicolumn{4}{c}{ATP} & \\multicolumn{4}{c}{WTA} \\\\"))
  lines <- c(lines, "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9}")
  lines <- c(lines, paste0(
    "Opportunities & Won=0 & Won=1 & Won=2 & Won=3+",
    " & Won=0 & Won=1 & Won=2 & Won=3+ \\\\"))
  lines <- c(lines, "\\midrule")

  for (r in opp_levels) {
    atp_vals <- paste(ct_atp[r, ], collapse = " & ")
    wta_vals <- paste(ct_wta[r, ], collapse = " & ")
    lines <- c(lines, paste0(r, " & ", atp_vals, " & ", wta_vals, " \\\\"))
  }

  # Totals row
  atp_totals <- colSums(ct_atp)
  wta_totals <- colSums(ct_wta)
  lines <- c(lines, "\\midrule")
  lines <- c(lines, paste0("Total & ",
    paste(atp_totals, collapse = " & "), " & ",
    paste(wta_totals, collapse = " & "), " \\\\"))

  lines <- c(lines, "\\bottomrule")
  lines <- c(lines, "\\end{tabular}")

  paste(lines, collapse = "\n")
}

# GS cross-tab
tex_ct_gs <- generate_crosstab_table(gs, "GS")
writeLines(tex_ct_gs, file.path(TABLES_DIR, "table_ll_careers_gs.tex"))
slog("  Saved table_ll_careers_gs.tex")

# NonGS cross-tab
tex_ct_ngs <- generate_crosstab_table(ngs, "NonGS")
writeLines(tex_ct_ngs, file.path(TABLES_DIR, "table_ll_careers_nongs.tex"))
slog("  Saved table_ll_careers_nongs.tex")

# ==============================================================================
# FIX 2: ADD WINNING PERCENTAGE OUTCOME
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 2: ADD WINNING PERCENTAGE")
message(strrep("=", 70))

# Load raw match data
message("  Loading raw match data...")
atp_main <- readRDS(file.path(RAW_DIR, "atp_main_matches.rds"))
atp_qual <- readRDS(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
wta_main <- readRDS(file.path(RAW_DIR, "wta_main_matches.rds"))
wta_qual <- readRDS(file.path(RAW_DIR, "wta_qual_itf_matches.rds"))

# Combine all matches by tour
all_atp <- rbind(
  atp_main[, c("tourney_date", "winner_id", "loser_id")],
  atp_qual[, c("tourney_date", "winner_id", "loser_id")]
)
all_wta <- rbind(
  wta_main[, c("tourney_date", "winner_id", "loser_id")],
  wta_qual[, c("tourney_date", "winner_id", "loser_id")]
)

# Convert tourney_date to Date
safe_as_date <- function(x) {
  if (is.numeric(x)) return(as.Date(as.character(x), format = "%Y%m%d"))
  if (is.character(x)) {
    # Try YYYYMMDD first, then standard
    d <- as.Date(x, format = "%Y%m%d")
    if (all(is.na(d))) d <- as.Date(x)
    return(d)
  }
  as.Date(x)
}

all_atp$match_date <- safe_as_date(all_atp$tourney_date)
all_wta$match_date <- safe_as_date(all_wta$tourney_date)

# Remove rows with NA dates
all_atp <- all_atp[!is.na(all_atp$match_date), ]
all_wta <- all_wta[!is.na(all_wta$match_date), ]

slog("  ATP matches: ", nrow(all_atp), ", WTA matches: ", nrow(all_wta))

# Convert to data.table for speed
dt_atp <- as.data.table(all_atp)
dt_wta <- as.data.table(all_wta)

# For each player, build a sorted match record: date, outcome (win/loss)
# Then for each skeleton event, do a binary-search window lookup

compute_win_pct_fast <- function(skeleton, dt_matches, horizons = c(4, 8, 12, 26, 52)) {
  message("    Computing win_pct for ", nrow(skeleton), " events...")

  # Build per-player sorted vectors of match dates and outcomes
  # Wins
  wins_by_player <- split(dt_matches$match_date, dt_matches$winner_id)
  wins_by_player <- lapply(wins_by_player, sort)
  # Losses
  losses_by_player <- split(dt_matches$match_date, dt_matches$loser_id)
  losses_by_player <- lapply(losses_by_player, sort)

  # Initialize columns
  for (h in horizons) {
    skeleton[[paste0("win_pct_", h, "w")]] <- NA_real_
  }

  # Convert skeleton dates
  if (is.numeric(skeleton$tourney_date)) {
    ev_dates <- as.Date(as.character(skeleton$tourney_date), format = "%Y%m%d")
  } else {
    ev_dates <- as.Date(skeleton$tourney_date)
  }

  pids <- as.character(skeleton$player_id)

  for (i in seq_len(nrow(skeleton))) {
    if (i %% 1000 == 0) message("      ", i, " / ", nrow(skeleton))
    pid <- pids[i]
    ev_d <- ev_dates[i]
    if (is.na(ev_d)) next

    # Get sorted win/loss dates for this player
    w_dates <- wins_by_player[[pid]]
    l_dates <- losses_by_player[[pid]]

    for (h in horizons) {
      end_d <- ev_d + h * 7L

      n_wins <- 0L
      if (!is.null(w_dates)) {
        # Count dates in (ev_d, end_d]
        lo <- findInterval(ev_d, w_dates)    # last index <= ev_d
        hi <- findInterval(end_d, w_dates)   # last index <= end_d
        n_wins <- as.integer(hi - lo)
      }

      n_losses <- 0L
      if (!is.null(l_dates)) {
        lo <- findInterval(ev_d, l_dates)
        hi <- findInterval(end_d, l_dates)
        n_losses <- as.integer(hi - lo)
      }

      total <- n_wins + n_losses
      skeleton[[paste0("win_pct_", h, "w")]][i] <- if (total > 0) n_wins / total else NA_real_
    }
  }

  skeleton
}

# Pre-add win_pct columns to full skeletons BEFORE splitting
for (h in c(4, 8, 12, 26, 52)) {
  col <- paste0("win_pct_", h, "w")
  gs[[col]]  <- NA_real_
  ngs[[col]] <- NA_real_
}

# Split by tour, compute, and assign back
gs_atp_idx  <- gs$tour == "ATP"
gs_wta_idx  <- gs$tour == "WTA"

message("  GS-ATP win_pct...")
gs[gs_atp_idx, ] <- compute_win_pct_fast(gs[gs_atp_idx, ], dt_atp)

message("  GS-WTA win_pct...")
gs[gs_wta_idx, ] <- compute_win_pct_fast(gs[gs_wta_idx, ], dt_wta)

ngs_atp_idx <- ngs$tour == "ATP"
ngs_wta_idx <- ngs$tour == "WTA"

message("  NonGS-ATP win_pct...")
ngs[ngs_atp_idx, ] <- compute_win_pct_fast(ngs[ngs_atp_idx, ], dt_atp)

message("  NonGS-WTA win_pct...")
ngs[ngs_wta_idx, ] <- compute_win_pct_fast(ngs[ngs_wta_idx, ], dt_wta)

# Check coverage
for (h in c(4, 8, 12, 26, 52)) {
  col <- paste0("win_pct_", h, "w")
  slog("  GS  win_pct_", h, "w: ",
       sum(!is.na(gs[[col]])), " / ", nrow(gs), " non-NA")
  slog("  NonGS win_pct_", h, "w: ",
       sum(!is.na(ngs[[col]])), " / ", nrow(ngs), " non-NA")
}

# Save updated skeletons
saveRDS(gs,  file.path(CLEANED_DIR, "skeleton_gs_est_v5.rds"))
saveRDS(ngs, file.path(CLEANED_DIR, "skeleton_nongs_est_v8.rds"))
slog("  Saved skeleton_gs_est_v5.rds and skeleton_nongs_est_v8.rds")

# ==============================================================================
# FIX 3: CHANGE DOSE FROM -log(pi) TO -logit(pi)
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 3: CHANGE DOSE TO -LOGIT(pi)")
message(strrep("=", 70))

dose_data <- readRDS(file.path(CLEANED_DIR, "performance_dose.rds"))
slog("  Dose data: ", nrow(dose_data), " rows")
slog("  Old dose range: [", fmt(min(dose_data$dose, na.rm = TRUE), 3),
     ", ", fmt(max(dose_data$dose, na.rm = TRUE), 3), "]")

# Old: dose = -log(pi_ie)
# New: dose = -logit(pi_ie) = log((1 - pi_ie) / pi_ie)
dose_data$dose_old <- dose_data$dose
dose_data$dose <- ifelse(
  !is.na(dose_data$pi_ie) & dose_data$pi_ie > 0 & dose_data$pi_ie < 1,
  log((1 - dose_data$pi_ie) / dose_data$pi_ie),
  0
)
# Controls (got_ll == 0 or no pi_ie) keep dose = 0
dose_data$dose[is.na(dose_data$dose)] <- 0

slog("  New dose range: [", fmt(min(dose_data$dose, na.rm = TRUE), 3),
     ", ", fmt(max(dose_data$dose, na.rm = TRUE), 3), "]")

saveRDS(dose_data, file.path(CLEANED_DIR, "performance_dose.rds"))
slog("  Saved updated performance_dose.rds")

# ==============================================================================
# COMMON SETUP: Scale variables, stack, estimate
# ==============================================================================
message("\n", strrep("=", 70))
message("COMMON SETUP: SCALE AND STACK")
message(strrep("=", 70))

# --- Scale for estimation (same logic as 40b) ---------------------------------
scale_for_estimation <- function(df, is_gs = TRUE) {
  df$pre_rank_pts_s    <- df$pre_rank_pts / 1000
  df$pre_rank_pts_sq_s <- df$pre_rank_pts_s^2
  if (is_gs) {
    df$pre_elo_s    <- df$pre_elo
    df$pre_elo_sq_s <- df$pre_elo_s^2
    df$pre_surf_elo_s <- ifelse(df$pre_surf_elo > 100,
                                df$pre_surf_elo / 100,
                                df$pre_surf_elo)
  } else {
    df$pre_elo_s    <- df$pre_elo / 100
    df$pre_elo_sq_s <- df$pre_elo_s^2
    df$pre_surf_elo_s <- df$pre_surf_elo / 100
  }
  df$pre_surf_elo_sq_s <- df$pre_surf_elo_s^2
  df
}

gs  <- scale_for_estimation(gs, is_gs = TRUE)
ngs <- scale_for_estimation(ngs, is_gs = FALSE)

# --- Z^{pre} strings ---------------------------------------------------------
ZPRE_GS <- paste0(
  "pre_rank_pts_s:horizon + pre_rank_pts_sq_s:horizon + ",
  "pre_elo_s:horizon + pre_elo_sq_s:horizon + ",
  "pre_surf_elo_s:horizon + pre_surf_elo_sq_s:horizon + ",
  "n_prior_gs_ll_won:horizon + n_prior_gs_ll_notwon:horizon + ",
  "n_prior_nongs_ll_won:horizon + n_prior_nongs_ll_notwon:horizon + ",
  "player_age:horizon"
)

ZPRE_NONGS <- paste0(
  "pre_rank_pts_s + pre_rank_pts_sq_s + pre_elo_s + pre_elo_sq_s + ",
  "pre_surf_elo_s + pre_surf_elo_sq_s + ",
  "n_prior_gs_ll_won + n_prior_gs_ll_notwon + ",
  "n_prior_nongs_ll_won + n_prior_nongs_ll_notwon + ",
  "player_age"
)

# --- Outcomes (now including win_pct) -----------------------------------------
OUTCOMES_BASE <- c("points_change", "elo_change",
                   "n_main_draws", "n_matches_250plus", "win_pct")
OUTCOME_LABELS <- c(
  "points_change"     = "Ranking Points $\\Delta$",
  "elo_change"        = "Elo $\\Delta$",
  "n_main_draws"      = "Main Draws",
  "n_matches_250plus" = "Matches (250+)",
  "win_pct"           = "Win \\%"
)

HORIZONS <- c(4, 8, 12, 26, 52)

# --- Stack horizons -----------------------------------------------------------
stack_horizons_full <- function(data, outcomes_base = OUTCOMES_BASE,
                                horizons = HORIZONS) {
  outcome_cols <- unlist(lapply(outcomes_base, function(ob) {
    paste0(ob, "_", horizons, "w")
  }))
  keep_cols <- setdiff(names(data), outcome_cols)

  stacked <- list()
  for (h in horizons) {
    h_label <- paste0(h, "w")
    row_data <- data[, keep_cols, drop = FALSE]
    row_data$horizon <- h_label
    row_data$horizon_num <- h
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
  result <- do.call(rbind, stacked)
  result$horizon <- factor(result$horizon, levels = paste0(horizons, "w"))
  rownames(result) <- NULL
  result
}

# Split by tour
gs_atp  <- gs[gs$tour == "ATP", ]
gs_wta  <- gs[gs$tour == "WTA", ]
ngs_atp <- ngs[ngs$tour == "ATP", ]
ngs_wta <- ngs[ngs$tour == "WTA", ]

# Merge dose into unstacked data
merge_dose <- function(df, dose_df) {
  dose_sub <- dose_df[, c("player_id", "tourney_id", "dose", "matches_won")]
  merged <- merge(df, dose_sub, by = c("player_id", "tourney_id"), all.x = TRUE)
  merged$dose[is.na(merged$dose)] <- 0
  merged$matches_won[is.na(merged$matches_won)] <- 0
  merged
}

gs_atp_d  <- merge_dose(gs_atp, dose_data)
gs_wta_d  <- merge_dose(gs_wta, dose_data)
ngs_atp_d <- merge_dose(ngs_atp, dose_data)
ngs_wta_d <- merge_dose(ngs_wta, dose_data)

# Stack
gs_atp_s  <- stack_horizons_full(gs_atp)
gs_wta_s  <- stack_horizons_full(gs_wta)
ngs_atp_s <- stack_horizons_full(ngs_atp)
ngs_wta_s <- stack_horizons_full(ngs_wta)

gs_atp_ds  <- stack_horizons_full(gs_atp_d)
gs_wta_ds  <- stack_horizons_full(gs_wta_d)
ngs_atp_ds <- stack_horizons_full(ngs_atp_d)
ngs_wta_ds <- stack_horizons_full(ngs_wta_d)

# Add hetero indicators
add_hetero_indicators <- function(df) {
  df$rank_above_med <- as.integer(
    df$pre_rank_pts >= median(df$pre_rank_pts, na.rm = TRUE))
  df$age_above_med <- as.integer(
    df$player_age >= median(df$player_age, na.rm = TRUE))
  df
}

gs_atp_s   <- add_hetero_indicators(gs_atp_s)
gs_wta_s   <- add_hetero_indicators(gs_wta_s)
ngs_atp_s  <- add_hetero_indicators(ngs_atp_s)
ngs_wta_s  <- add_hetero_indicators(ngs_wta_s)
gs_atp_ds  <- add_hetero_indicators(gs_atp_ds)
gs_wta_ds  <- add_hetero_indicators(gs_wta_ds)
ngs_atp_ds <- add_hetero_indicators(ngs_atp_ds)
ngs_wta_ds <- add_hetero_indicators(ngs_wta_ds)

slog("  Stacked GS-ATP: ", nrow(gs_atp_s), " rows")
slog("  Stacked NonGS-ATP: ", nrow(ngs_atp_s), " rows")

# --- Estimation helpers (same as 40b) ----------------------------------------
estimate_dynamic <- function(sdata, outcomes, zpre, fe_str, treat_var = "got_ll",
                             cf_term = NULL, label = "") {
  results <- list()
  for (ob in outcomes) {
    if (all(is.na(sdata[[ob]]))) {
      message("    Skipping ", ob, " -- all NA")
      next
    }
    rhs <- paste0(treat_var, ":horizon")
    if (!is.null(cf_term)) rhs <- paste0(rhs, " + ", cf_term, ":horizon")
    rhs <- paste0(rhs, " + ", zpre)
    fml_str <- paste0(ob, " ~ ", rhs, " | ", fe_str)
    fit <- tryCatch(
      feols(as.formula(fml_str), data = sdata, cluster = ~player_id),
      error = function(e) {
        message("    ERROR in ", label, " ", ob, ": ", e$message)
        NULL
      }
    )
    if (!is.null(fit)) results[[ob]] <- fit
  }
  results
}

extract_treat_coefs <- function(fit, treat_var = "got_ll", horizons = HORIZONS) {
  cf <- coeftable(fit)
  h_labels <- paste0(horizons, "w")
  out <- data.frame(
    horizon = h_labels, coef = NA_real_, se = NA_real_, pval = NA_real_,
    stringsAsFactors = FALSE)
  for (j in seq_along(h_labels)) {
    pat1 <- paste0(treat_var, ":horizon", h_labels[j])
    pat2 <- paste0("horizon", h_labels[j], ":", treat_var)
    idx <- which(rownames(cf) %in% c(pat1, pat2))
    if (length(idx) == 1) {
      out$coef[j] <- cf[idx, "Estimate"]
      out$se[j]   <- cf[idx, "Std. Error"]
      out$pval[j] <- cf[idx, "Pr(>|t|)"]
    }
  }
  out
}

extract_cf_coefs <- function(fit, cf_var = "v_hat", horizons = HORIZONS) {
  cf <- coeftable(fit)
  h_labels <- paste0(horizons, "w")
  out <- data.frame(
    horizon = h_labels, coef = NA_real_, se = NA_real_, pval = NA_real_,
    stringsAsFactors = FALSE)
  for (j in seq_along(h_labels)) {
    pat1 <- paste0(cf_var, ":horizon", h_labels[j])
    pat2 <- paste0("horizon", h_labels[j], ":", cf_var)
    idx <- which(rownames(cf) %in% c(pat1, pat2))
    if (length(idx) == 1) {
      out$coef[j] <- cf[idx, "Estimate"]
      out$se[j]   <- cf[idx, "Std. Error"]
      out$pval[j] <- cf[idx, "Pr(>|t|)"]
    }
  }
  out
}

extract_interact_coefs <- function(fit, interact_var, horizons = HORIZONS) {
  cf <- coeftable(fit)
  h_labels <- paste0(horizons, "w")
  out <- data.frame(
    horizon = h_labels, coef = NA_real_, se = NA_real_, pval = NA_real_,
    stringsAsFactors = FALSE)
  for (j in seq_along(h_labels)) {
    rn <- rownames(cf)
    parts <- c("got_ll", paste0("horizon", h_labels[j]), interact_var)
    for (perm in list(
      c(1,2,3), c(1,3,2), c(2,1,3), c(2,3,1), c(3,1,2), c(3,2,1)
    )) {
      pat <- paste(parts[perm], collapse = ":")
      idx <- which(rn == pat)
      if (length(idx) == 1) {
        out$coef[j] <- cf[idx, "Estimate"]
        out$se[j]   <- cf[idx, "Std. Error"]
        out$pval[j] <- cf[idx, "Pr(>|t|)"]
        break
      }
    }
  }
  out
}

# ==============================================================================
# RE-ESTIMATE DYNAMIC MODELS (FIX 2: now includes win_pct)
# ==============================================================================
message("\n", strrep("=", 70))
message("RE-ESTIMATE DYNAMIC MODELS (with win_pct)")
message(strrep("=", 70))

message("  GS-ATP...")
dyn_gs_atp <- estimate_dynamic(
  gs_atp_s, OUTCOMES_BASE, ZPRE_GS, "slam_year + horizon", label = "GS-ATP")

message("  GS-WTA...")
dyn_gs_wta <- estimate_dynamic(
  gs_wta_s, OUTCOMES_BASE, ZPRE_GS, "slam_year + horizon", label = "GS-WTA")

message("  NonGS-ATP...")
dyn_ngs_atp <- estimate_dynamic(
  ngs_atp_s, OUTCOMES_BASE, ZPRE_NONGS, "tourney_id + horizon",
  cf_term = "v_hat", label = "NonGS-ATP")

message("  NonGS-WTA...")
dyn_ngs_wta <- estimate_dynamic(
  ngs_wta_s, OUTCOMES_BASE, ZPRE_NONGS, "tourney_id + horizon",
  cf_term = "v_hat", label = "NonGS-WTA")

# --- Generate compact dynamic tables (with 5 outcomes) -----------------------
generate_compact_dynamic_table <- function(fits, outcomes, outcome_labels,
                                           horizons = HORIZONS,
                                           include_cf = FALSE) {
  h_labels <- paste0(horizons, "w")
  ncols <- length(h_labels)

  lines <- character()
  lines <- c(lines, paste0("\\begin{tabular}{l", paste(rep("c", ncols), collapse = ""), "}"))
  lines <- c(lines, "\\toprule")
  lines <- c(lines, paste0(" & ", paste(h_labels, collapse = " & "), " \\\\"))
  lines <- c(lines, "\\midrule")

  for (ob in outcomes) {
    if (!ob %in% names(fits)) next
    tc <- extract_treat_coefs(fits[[ob]])
    coef_str <- sapply(seq_along(h_labels), function(j) {
      if (is.na(tc$coef[j])) return("")
      paste0(fmt(tc$coef[j], 2), add_stars(tc$pval[j]))
    })
    se_str <- sapply(seq_along(h_labels), function(j) {
      if (is.na(tc$se[j])) return("")
      paste0("(", fmt(tc$se[j], 2), ")")
    })
    lab <- if (ob %in% names(outcome_labels)) outcome_labels[ob] else ob
    lines <- c(lines,
      paste0(lab, " & ", paste(coef_str, collapse = " & "), " \\\\"),
      paste0(" & ", paste(se_str, collapse = " & "), " \\\\[0.3em]")
    )
  }

  # CF rho row (nonGS only)
  if (include_cf) {
    lines <- c(lines, "\\midrule")
    for (ob in outcomes) {
      if (!ob %in% names(fits)) next
      rho <- extract_cf_coefs(fits[[ob]])
      rho_str <- sapply(seq_along(h_labels), function(j) {
        if (is.na(rho$coef[j])) return("")
        paste0(fmt(rho$coef[j], 2), add_stars(rho$pval[j]))
      })
      rho_se_str <- sapply(seq_along(h_labels), function(j) {
        if (is.na(rho$se[j])) return("")
        paste0("(", fmt(rho$se[j], 2), ")")
      })
      lab <- paste0("$\\hat{\\rho}_h$ (", outcome_labels[ob], ")")
      lines <- c(lines,
        paste0(lab, " & ", paste(rho_str, collapse = " & "), " \\\\"),
        paste0(" & ", paste(rho_se_str, collapse = " & "), " \\\\[0.3em]")
      )
    }
  }

  # Footer
  lines <- c(lines, "\\midrule")
  if (length(fits) > 0) {
    fit1 <- fits[[1]]
    n_obs <- nobs(fit1)
    lines <- c(lines, paste0("$N$ & \\multicolumn{", ncols, "}{c}{",
                              format(n_obs, big.mark = ","), "} \\\\"))
  }
  lines <- c(lines, paste0("$Z^{\\text{pre}}$ controls & \\multicolumn{", ncols,
                            "}{c}{Yes} \\\\"))
  lines <- c(lines, "\\bottomrule")
  lines <- c(lines, "\\end{tabular}")

  paste(lines, collapse = "\n")
}

# Save dynamic tables
tex_dyn_atp <- generate_compact_dynamic_table(
  dyn_gs_atp, OUTCOMES_BASE, OUTCOME_LABELS)
writeLines(tex_dyn_atp, file.path(TABLES_DIR, "table_dynamic_stacked_atp.tex"))
slog("  Saved table_dynamic_stacked_atp.tex")

tex_dyn_wta <- generate_compact_dynamic_table(
  dyn_gs_wta, OUTCOMES_BASE, OUTCOME_LABELS)
writeLines(tex_dyn_wta, file.path(TABLES_DIR, "table_dynamic_stacked_wta.tex"))
slog("  Saved table_dynamic_stacked_wta.tex")

tex_dyn_ngs_atp <- generate_compact_dynamic_table(
  dyn_ngs_atp, OUTCOMES_BASE, OUTCOME_LABELS, include_cf = TRUE)
writeLines(tex_dyn_ngs_atp, file.path(TABLES_DIR, "table_dynamic_stacked_nongs_atp.tex"))
slog("  Saved table_dynamic_stacked_nongs_atp.tex")

tex_dyn_ngs_wta <- generate_compact_dynamic_table(
  dyn_ngs_wta, OUTCOMES_BASE, OUTCOME_LABELS, include_cf = TRUE)
writeLines(tex_dyn_ngs_wta, file.path(TABLES_DIR, "table_dynamic_stacked_nongs_wta.tex"))
slog("  Saved table_dynamic_stacked_nongs_wta.tex")

# ==============================================================================
# RE-ESTIMATE DOSE MODELS (FIX 3: -logit dose)
# ==============================================================================
message("\n", strrep("=", 70))
message("RE-ESTIMATE DOSE MODELS (-logit dose)")
message(strrep("=", 70))

estimate_dose <- function(sdata, outcomes, zpre, fe_str, dose_var,
                          cf_term = NULL, label = "") {
  results <- list()
  for (ob in outcomes) {
    if (all(is.na(sdata[[ob]]))) next
    rhs <- paste0("got_ll:horizon + got_ll:horizon:", dose_var)
    if (!is.null(cf_term)) {
      rhs <- paste0(rhs, " + ", cf_term, ":horizon + ",
                     cf_term, ":horizon:", dose_var)
    }
    rhs <- paste0(rhs, " + ", zpre)
    fml_str <- paste0(ob, " ~ ", rhs, " | ", fe_str)
    fit <- tryCatch(
      feols(as.formula(fml_str), data = sdata, cluster = ~player_id),
      error = function(e) {
        message("    ERROR in ", label, " ", ob, ": ", e$message)
        NULL
      }
    )
    if (!is.null(fit)) results[[ob]] <- fit
  }
  results
}

generate_dose_table <- function(fits_mw, fits_dose, outcomes, outcome_labels,
                                horizons = HORIZONS, include_cf = FALSE) {
  h_labels <- paste0(horizons, "w")
  ncols <- length(h_labels)
  lines <- character()
  lines <- c(lines, paste0("\\begin{tabular}{l", paste(rep("c", ncols), collapse = ""), "}"))
  lines <- c(lines, "\\toprule")
  lines <- c(lines, paste0(" & ", paste(h_labels, collapse = " & "), " \\\\"))
  lines <- c(lines, "\\midrule")

  # Panel A: Matches won dose
  lines <- c(lines, paste0("\\multicolumn{", ncols + 1,
                            "}{l}{\\textit{Panel A: Matches won dose}} \\\\"))
  for (ob in outcomes) {
    if (!ob %in% names(fits_mw)) next
    tc <- extract_treat_coefs(fits_mw[[ob]])
    coef_str <- sapply(seq_along(h_labels), function(j) {
      if (is.na(tc$coef[j])) return("")
      paste0(fmt(tc$coef[j], 2), add_stars(tc$pval[j]))
    })
    se_str <- sapply(seq_along(h_labels), function(j) {
      if (is.na(tc$se[j])) return("")
      paste0("(", fmt(tc$se[j], 2), ")")
    })
    lab <- outcome_labels[ob]
    lines <- c(lines,
      paste0("\\quad ", lab, " (base) & ", paste(coef_str, collapse = " & "), " \\\\"),
      paste0(" & ", paste(se_str, collapse = " & "), " \\\\")
    )

    ic <- extract_interact_coefs(fits_mw[[ob]], "matches_won")
    ic_str <- sapply(seq_along(h_labels), function(j) {
      if (is.na(ic$coef[j])) return("")
      paste0(fmt(ic$coef[j], 2), add_stars(ic$pval[j]))
    })
    ic_se_str <- sapply(seq_along(h_labels), function(j) {
      if (is.na(ic$se[j])) return("")
      paste0("(", fmt(ic$se[j], 2), ")")
    })
    lines <- c(lines,
      paste0("\\quad \\quad $\\times$ Matches Won & ",
             paste(ic_str, collapse = " & "), " \\\\"),
      paste0(" & ", paste(ic_se_str, collapse = " & "), " \\\\[0.3em]")
    )
  }

  # Panel B: Performance probability dose (-logit)
  lines <- c(lines, "\\\\[0.5em]")
  lines <- c(lines, paste0("\\multicolumn{", ncols + 1,
                            "}{l}{\\textit{Panel B: Performance probability dose ($-\\text{logit}(\\pi)$)}} \\\\"))
  for (ob in outcomes) {
    if (!ob %in% names(fits_dose)) next
    tc <- extract_treat_coefs(fits_dose[[ob]])
    coef_str <- sapply(seq_along(h_labels), function(j) {
      if (is.na(tc$coef[j])) return("")
      paste0(fmt(tc$coef[j], 2), add_stars(tc$pval[j]))
    })
    se_str <- sapply(seq_along(h_labels), function(j) {
      if (is.na(tc$se[j])) return("")
      paste0("(", fmt(tc$se[j], 2), ")")
    })
    lab <- outcome_labels[ob]
    lines <- c(lines,
      paste0("\\quad ", lab, " (base) & ", paste(coef_str, collapse = " & "), " \\\\"),
      paste0(" & ", paste(se_str, collapse = " & "), " \\\\")
    )

    ic <- extract_interact_coefs(fits_dose[[ob]], "dose")
    ic_str <- sapply(seq_along(h_labels), function(j) {
      if (is.na(ic$coef[j])) return("")
      paste0(fmt(ic$coef[j], 2), add_stars(ic$pval[j]))
    })
    ic_se_str <- sapply(seq_along(h_labels), function(j) {
      if (is.na(ic$se[j])) return("")
      paste0("(", fmt(ic$se[j], 2), ")")
    })
    lines <- c(lines,
      paste0("\\quad \\quad $\\times$ Dose & ",
             paste(ic_str, collapse = " & "), " \\\\"),
      paste0(" & ", paste(ic_se_str, collapse = " & "), " \\\\[0.3em]")
    )
  }

  # Footer
  lines <- c(lines, "\\midrule")
  zpre_desc <- if (!include_cf) "Yes (horizon-interacted)" else "Yes (main effects)"
  lines <- c(lines, paste0("$Z^{\\text{pre}}$ controls & \\multicolumn{",
                            ncols, "}{c}{", zpre_desc, "} \\\\"))
  lines <- c(lines, "\\bottomrule")
  lines <- c(lines, "\\end{tabular}")

  paste(lines, collapse = "\n")
}

# GS dose models
message("  GS-ATP matches won dose...")
dose_gs_atp_mw <- estimate_dose(
  gs_atp_ds, OUTCOMES_BASE, ZPRE_GS, "slam_year + horizon",
  "matches_won", label = "GS-ATP-MW")

message("  GS-ATP performance prob dose...")
dose_gs_atp_pp <- estimate_dose(
  gs_atp_ds, OUTCOMES_BASE, ZPRE_GS, "slam_year + horizon",
  "dose", label = "GS-ATP-PP")

message("  GS-WTA matches won dose...")
dose_gs_wta_mw <- estimate_dose(
  gs_wta_ds, OUTCOMES_BASE, ZPRE_GS, "slam_year + horizon",
  "matches_won", label = "GS-WTA-MW")

message("  GS-WTA performance prob dose...")
dose_gs_wta_pp <- estimate_dose(
  gs_wta_ds, OUTCOMES_BASE, ZPRE_GS, "slam_year + horizon",
  "dose", label = "GS-WTA-PP")

tex_dose_gs <- generate_dose_table(
  dose_gs_atp_mw, dose_gs_atp_pp, OUTCOMES_BASE, OUTCOME_LABELS)
writeLines(tex_dose_gs, file.path(TABLES_DIR, "table_dose_stacked.tex"))
slog("  Saved table_dose_stacked.tex (GS-ATP)")

tex_perf_gs <- generate_dose_table(
  dose_gs_wta_mw, dose_gs_wta_pp, OUTCOMES_BASE, OUTCOME_LABELS)
writeLines(tex_perf_gs, file.path(TABLES_DIR, "table_perf_prob_stacked.tex"))
slog("  Saved table_perf_prob_stacked.tex (GS-WTA)")

# NonGS dose models
message("  NonGS-ATP matches won dose...")
dose_ngs_atp_mw <- estimate_dose(
  ngs_atp_ds, OUTCOMES_BASE, ZPRE_NONGS, "tourney_id + horizon",
  "matches_won", cf_term = "v_hat", label = "NonGS-ATP-MW")

message("  NonGS-ATP performance prob dose...")
dose_ngs_atp_pp <- estimate_dose(
  ngs_atp_ds, OUTCOMES_BASE, ZPRE_NONGS, "tourney_id + horizon",
  "dose", cf_term = "v_hat", label = "NonGS-ATP-PP")

message("  NonGS-WTA matches won dose...")
dose_ngs_wta_mw <- estimate_dose(
  ngs_wta_ds, OUTCOMES_BASE, ZPRE_NONGS, "tourney_id + horizon",
  "matches_won", cf_term = "v_hat", label = "NonGS-WTA-MW")

message("  NonGS-WTA performance prob dose...")
dose_ngs_wta_pp <- estimate_dose(
  ngs_wta_ds, OUTCOMES_BASE, ZPRE_NONGS, "tourney_id + horizon",
  "dose", cf_term = "v_hat", label = "NonGS-WTA-PP")

tex_dose_ngs <- generate_dose_table(
  dose_ngs_atp_mw, dose_ngs_atp_pp, OUTCOMES_BASE, OUTCOME_LABELS,
  include_cf = TRUE)
writeLines(tex_dose_ngs, file.path(TABLES_DIR, "table_dose_stacked_nongs.tex"))
slog("  Saved table_dose_stacked_nongs.tex (NonGS-ATP)")

tex_perf_ngs <- generate_dose_table(
  dose_ngs_wta_mw, dose_ngs_wta_pp, OUTCOMES_BASE, OUTCOME_LABELS,
  include_cf = TRUE)
writeLines(tex_perf_ngs, file.path(TABLES_DIR, "table_perf_prob_stacked_nongs.tex"))
slog("  Saved table_perf_prob_stacked_nongs.tex (NonGS-WTA)")

# ==============================================================================
# FIX 4: REFORMAT HETEROGENEITY TABLES (side-by-side base + interaction)
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 4: REFORMAT HETEROGENEITY TABLES")
message(strrep("=", 70))

# Estimation wrapper for heterogeneity
estimate_hetero <- function(sdata, outcomes, zpre, fe_str, dim_var,
                            cf_term = NULL, label = "") {
  results <- list()
  for (ob in outcomes) {
    if (all(is.na(sdata[[ob]]))) next
    rhs <- paste0("got_ll:horizon + got_ll:horizon:", dim_var)
    if (!is.null(cf_term)) {
      rhs <- paste0(rhs, " + ", cf_term, ":horizon + ",
                     cf_term, ":horizon:", dim_var)
    }
    rhs <- paste0(rhs, " + ", zpre)
    fml_str <- paste0(ob, " ~ ", rhs, " | ", fe_str)
    fit <- tryCatch(
      feols(as.formula(fml_str), data = sdata, cluster = ~player_id),
      error = function(e) {
        message("    ERROR in ", label, " ", ob, " x ", dim_var, ": ", e$message)
        NULL
      }
    )
    if (!is.null(fit)) results[[ob]] <- fit
  }
  results
}

HETERO_DIMS <- c("rank_above_med", "age_above_med", "had_prior_ll")
HETERO_LABELS <- c(
  "rank_above_med" = "Above-Median Rank",
  "age_above_med"  = "Above-Median Age",
  "had_prior_ll"   = "Had Prior LL"
)

run_all_hetero <- function(sdata, outcomes, zpre, fe_str, cf_term = NULL,
                           label = "") {
  all_fits <- list()
  for (dv in HETERO_DIMS) {
    message("    Dimension: ", dv)
    all_fits[[dv]] <- estimate_hetero(
      sdata, outcomes, zpre, fe_str, dv, cf_term = cf_term,
      label = paste(label, dv))
  }
  all_fits
}

message("  GS-ATP heterogeneity...")
hetero_gs_atp <- run_all_hetero(
  gs_atp_s, OUTCOMES_BASE, ZPRE_GS, "slam_year + horizon", label = "GS-ATP")

message("  GS-WTA heterogeneity...")
hetero_gs_wta <- run_all_hetero(
  gs_wta_s, OUTCOMES_BASE, ZPRE_GS, "slam_year + horizon", label = "GS-WTA")

message("  NonGS-ATP heterogeneity...")
hetero_ngs_atp <- run_all_hetero(
  ngs_atp_s, OUTCOMES_BASE, ZPRE_NONGS, "tourney_id + horizon",
  cf_term = "v_hat", label = "NonGS-ATP")

message("  NonGS-WTA heterogeneity...")
hetero_ngs_wta <- run_all_hetero(
  ngs_wta_s, OUTCOMES_BASE, ZPRE_NONGS, "tourney_id + horizon",
  cf_term = "v_hat", label = "NonGS-WTA")

# --- New format: side-by-side base + interaction per horizon ------------------
# For each dimension, one panel with outcomes as rows.
# Columns: pairs of (base, x Dim) for each horizon => 2 * 5 = 10 columns.

generate_hetero_table_v2 <- function(fits_list, dim_labels, outcomes,
                                     outcome_labels, horizons = HORIZONS,
                                     include_cf = FALSE) {
  h_labels <- paste0(horizons, "w")
  n_h <- length(h_labels)
  ncols <- 2 * n_h  # base + interaction per horizon

  lines <- character()
  lines <- c(lines, paste0("\\begin{tabular}{l", paste(rep("c", ncols), collapse = ""), "}"))
  lines <- c(lines, "\\toprule")

  # Top header: horizon spans
  top_header <- " "
  for (h in h_labels) {
    top_header <- paste0(top_header, " & \\multicolumn{2}{c}{", h, "}")
  }
  top_header <- paste0(top_header, " \\\\")
  lines <- c(lines, top_header)

  # Cmidrules
  cmidrule_str <- ""
  for (j in seq_along(h_labels)) {
    start_col <- 2 * (j - 1) + 2
    end_col   <- start_col + 1
    cmidrule_str <- paste0(cmidrule_str, "\\cmidrule(lr){", start_col, "-", end_col, "} ")
  }
  lines <- c(lines, cmidrule_str)

  # Sub-header: beta_h, beta_h x X_k repeated
  sub_header <- " "
  for (h in h_labels) {
    sub_header <- paste0(sub_header, " & $\\beta_h$ & $\\beta_h \\times X_k$")
  }
  sub_header <- paste0(sub_header, " \\\\")
  lines <- c(lines, sub_header)
  lines <- c(lines, "\\midrule")

  for (dim_name in names(fits_list)) {
    fits <- fits_list[[dim_name]]
    dim_label <- dim_labels[dim_name]

    lines <- c(lines, paste0("\\multicolumn{", ncols + 1,
                              "}{l}{\\textit{Interaction: ", dim_label, "}} \\\\"))

    for (ob in outcomes) {
      if (!ob %in% names(fits)) next

      # Base treatment coefficients
      tc <- extract_treat_coefs(fits[[ob]])
      # Interaction coefficients
      ic <- extract_interact_coefs(fits[[ob]], dim_name)

      lab <- if (ob %in% names(outcome_labels)) outcome_labels[ob] else ob

      # Coefficient row
      coef_cells <- character()
      for (j in seq_along(h_labels)) {
        base_str <- if (is.na(tc$coef[j])) "" else paste0(fmt(tc$coef[j], 2), add_stars(tc$pval[j]))
        int_str  <- if (is.na(ic$coef[j])) "" else paste0(fmt(ic$coef[j], 2), add_stars(ic$pval[j]))
        coef_cells <- c(coef_cells, base_str, int_str)
      }
      lines <- c(lines, paste0("\\quad ", lab, " & ",
                                paste(coef_cells, collapse = " & "), " \\\\"))

      # SE row
      se_cells <- character()
      for (j in seq_along(h_labels)) {
        base_se <- if (is.na(tc$se[j])) "" else paste0("(", fmt(tc$se[j], 2), ")")
        int_se  <- if (is.na(ic$se[j])) "" else paste0("(", fmt(ic$se[j], 2), ")")
        se_cells <- c(se_cells, base_se, int_se)
      }
      lines <- c(lines, paste0(" & ", paste(se_cells, collapse = " & "), " \\\\[0.3em]"))
    }
    lines <- c(lines, "\\\\[0.3em]")
  }

  # Footer
  lines <- c(lines, "\\midrule")
  zpre_desc <- if (!include_cf) "Yes (horizon-interacted)" else "Yes (main effects)"
  lines <- c(lines, paste0("$Z^{\\text{pre}}$ controls & \\multicolumn{",
                            ncols, "}{c}{", zpre_desc, "} \\\\"))
  lines <- c(lines, "\\bottomrule")
  lines <- c(lines, "\\end{tabular}")

  paste(lines, collapse = "\n")
}

# Generate reformatted heterogeneity tables
tex_hetero_atp <- generate_hetero_table_v2(
  hetero_gs_atp, HETERO_LABELS, OUTCOMES_BASE, OUTCOME_LABELS)
writeLines(tex_hetero_atp, file.path(TABLES_DIR, "table_hetero_stacked_atp.tex"))
slog("  Saved table_hetero_stacked_atp.tex (new format)")

tex_hetero_wta <- generate_hetero_table_v2(
  hetero_gs_wta, HETERO_LABELS, OUTCOMES_BASE, OUTCOME_LABELS)
writeLines(tex_hetero_wta, file.path(TABLES_DIR, "table_hetero_stacked_wta.tex"))
slog("  Saved table_hetero_stacked_wta.tex (new format)")

tex_hetero_ngs_atp <- generate_hetero_table_v2(
  hetero_ngs_atp, HETERO_LABELS, OUTCOMES_BASE, OUTCOME_LABELS,
  include_cf = TRUE)
writeLines(tex_hetero_ngs_atp, file.path(TABLES_DIR, "table_hetero_stacked_nongs_atp.tex"))
slog("  Saved table_hetero_stacked_nongs_atp.tex (new format)")

tex_hetero_ngs_wta <- generate_hetero_table_v2(
  hetero_ngs_wta, HETERO_LABELS, OUTCOMES_BASE, OUTCOME_LABELS,
  include_cf = TRUE)
writeLines(tex_hetero_ngs_wta, file.path(TABLES_DIR, "table_hetero_stacked_nongs_wta.tex"))
slog("  Saved table_hetero_stacked_nongs_wta.tex (new format)")

# ==============================================================================
# SAVE SUMMARY LOG
# ==============================================================================
message("\n", strrep("=", 70))
message("SAVING SUMMARY LOG")
message(strrep("=", 70))

summary_text <- c(
  "# 41_fixes.R Summary",
  paste0("Generated: ", Sys.time()),
  "",
  "## Fixes applied",
  "",
  "### FIX 1: Cross-tab tables (LL eligibility x wins)",
  "- table_ll_careers_gs.tex",
  "- table_ll_careers_nongs.tex",
  "",
  "### FIX 2: Add winning percentage outcome",
  "- Computed win_pct_4w/8w/12w/26w/52w from raw match data",
  "- Updated skeletons: v5 (GS), v8 (NonGS)",
  "- Regenerated dynamic stacked tables with 5th outcome row",
  "",
  "### FIX 3: Change dose from -log(pi) to -logit(pi)",
  "- dose = log((1-pi)/pi) instead of -log(pi)",
  "- Regenerated all dose tables",
  "",
  "### FIX 4: Reformat heterogeneity tables",
  "- New format: side-by-side base + interaction under each horizon",
  "- 2 columns per horizon (10 total) instead of 1 column per horizon",
  "",
  "## Key log messages",
  summary_log
)

writeLines(summary_text, file.path(OUTPUT_DIR, "41_fixes_summary.md"))
message("  Saved 41_fixes_summary.md")

message("\n", strrep("=", 70))
message("DONE: 41_fixes.R complete")
message(strrep("=", 70))
