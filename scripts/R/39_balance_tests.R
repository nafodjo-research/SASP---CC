# ==============================================================================
# 39_balance_tests.R
# Pre-treatment balance tests with FULL covariate set
#
# Tests at TWO levels:
#   (1) Player-episode level (one obs per player per GS appearance)
#   (2) Stacked panel level (one obs per player-episode-horizon)
#
# Variables tested (matching Z^{pre}_{ie}):
#   - Ranking points
#   - Ranking points^2
#   - Elo rating
#   - Elo^2
#   - Surface Elo (at event surface)
#   - Surface Elo^2
#   - Age
#   - Had prior LL
#   - Prior GS LL won
#   - Prior GS LL not won
#   - Prior non-GS LL won
#   - Prior non-GS LL not won
#
# Outputs:
#   Tables/table_balance.tex           (player-episode level)
#   Tables/table_balance_stacked.tex   (stacked panel level)
# ==============================================================================

set.seed(20260327)

library(dplyr)
library(here)

source(here("scripts", "R", "utils.R"))
summary_log <- character()

RAW_DIR     <- here("Data", "raw")
CLEANED_DIR <- here("Data", "cleaned")
TABLES_DIR  <- here("Tables")
OUTPUT_DIR  <- here("Output")

# Load FULL GS estimation sample (not the first-LL-restricted tournament rebuild)
gs_all <- readRDS(file.path(CLEANED_DIR, "skeleton_gs_est_v3.rds"))
gs_atp <- gs_all[gs_all$tour == "ATP", ]
gs_wta <- gs_all[gs_all$tour == "WTA", ]
rm(gs_all)

# Load Elo data for surface Elo
elo_cache <- readRDS(file.path(CLEANED_DIR, "tournament_elo_cache.rds"))

message("  GS-ATP events: ", nrow(gs_atp))
message("  GS-WTA events: ", nrow(gs_wta))

# ==============================================================================
# STEP 1: ADD MISSING COVARIATES TO EVENT TABLES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 1: ADD MISSING COVARIATES")
message(strrep("=", 70))

# Add surface Elo for the event surface at the event date
get_surface_elo_single <- function(player_id, event_date, surface, tour_prefix) {
  surf <- tolower(surface)
  if (is.na(surf) || !surf %in% c("hard", "clay", "grass")) return(NA_real_)
  env <- elo_cache[[surf]]
  if (is.null(env)) return(NA_real_)
  key <- paste0(tour_prefix, "_", player_id)
  if (!exists(key, envir = env)) return(NA_real_)
  elo_df <- get(key, envir = env)
  if (is.null(elo_df) || nrow(elo_df) == 0) return(NA_real_)
  md <- as.Date(event_date)
  elo_df$date <- as.Date(elo_df$date)
  prior <- elo_df[elo_df$date <= md, , drop = FALSE]
  if (nrow(prior) > 0) return(prior$rating[which.max(prior$date)])
  NA_real_
}

add_balance_vars <- function(df, tour_prefix) {
  # Quadratic terms
  df$pre_rank_pts_sq <- (df$pre_rank_pts / 1000)^2
  df$pre_rank_pts_scaled <- df$pre_rank_pts / 1000
  df$pre_elo_scaled <- df$pre_elo / 100
  df$pre_elo_sq <- (df$pre_elo / 100)^2

  # Surface Elo at event surface (handle numeric YYYYMMDD dates)
  df$pre_surf_elo <- NA_real_
  for (i in seq_len(nrow(df))) {
    td <- df$tourney_date[i]
    if (is.numeric(td)) td <- as.Date(as.character(td), format = "%Y%m%d")
    df$pre_surf_elo[i] <- get_surface_elo_single(
      df$player_id[i], td, df$surface[i], tour_prefix)
  }
  df$pre_surf_elo[is.na(df$pre_surf_elo)] <- df$pre_elo[is.na(df$pre_surf_elo)]
  df$pre_surf_elo_scaled <- df$pre_surf_elo / 100
  df$pre_surf_elo_sq <- (df$pre_surf_elo / 100)^2

  # Prior LL variables (ensure they exist)
  if (!"n_prior_gs_ll_opp" %in% names(df)) {
    # Compute from had_prior_ll or set to 0
    df$n_prior_gs_ll_opp <- 0L
  }
  if (!"n_prior_nongs_ll_opp" %in% names(df)) {
    df$n_prior_nongs_ll_opp <- 0L
  }

  # Prior LL variables already exist in skeleton_gs_est_v3.rds
  # (n_prior_gs_ll_won, n_prior_gs_ll_notwon, n_prior_nongs_ll_won, n_prior_nongs_ll_notwon)

  df
}

message("  Adding balance vars for ATP...")
gs_atp <- add_balance_vars(gs_atp, "ATP")
message("  Adding balance vars for WTA...")
gs_wta <- add_balance_vars(gs_wta, "WTA")


# ==============================================================================
# STEP 2: BALANCE TEST FUNCTION
# ==============================================================================

run_balance <- function(data, sample_label) {
  balance_vars <- c(
    "pre_rank_pts_scaled", "pre_rank_pts_sq",
    "pre_elo_scaled", "pre_elo_sq",
    "pre_surf_elo_scaled", "pre_surf_elo_sq",
    "player_age",
    "had_prior_ll",
    "n_prior_gs_ll_won", "n_prior_gs_ll_notwon",
    "n_prior_nongs_ll_won", "n_prior_nongs_ll_notwon"
  )

  bal_var_labels <- c(
    "pre_rank_pts_scaled" = "Ranking points / 1000",
    "pre_rank_pts_sq"     = "(Ranking points / 1000)$^2$",
    "pre_elo_scaled"      = "Elo / 100",
    "pre_elo_sq"          = "(Elo / 100)$^2$",
    "pre_surf_elo_scaled" = "Surface Elo / 100",
    "pre_surf_elo_sq"     = "(Surface Elo / 100)$^2$",
    "player_age"          = "Age",
    "had_prior_ll"        = "Had prior LL",
    "n_prior_gs_ll_won"   = "Prior GS LL won",
    "n_prior_gs_ll_notwon" = "Prior GS LL not won",
    "n_prior_nongs_ll_won" = "Prior non-GS LL won",
    "n_prior_nongs_ll_notwon" = "Prior non-GS LL not won"
  )

  rows <- list()
  for (v in balance_vars) {
    if (!v %in% names(data)) next
    x <- data[[v]]
    d <- data$got_ll
    ok <- !is.na(x)
    if (sum(ok & d == 1) < 3 || sum(ok & d == 0) < 3) next

    vlabel <- unname(bal_var_labels[v])
    tt <- tryCatch(t.test(x[ok & d == 1], x[ok & d == 0]), error = function(e) NULL)
    rows[[v]] <- data.frame(
      variable = v, label = vlabel,
      ll_mean = mean(x[ok & d == 1]), ct_mean = mean(x[ok & d == 0]),
      pvalue = if (!is.null(tt)) tt$p.value else NA_real_,
      stringsAsFactors = FALSE
    )
  }
  bal <- do.call(rbind, rows)

  # Joint F-test
  avail_vars <- balance_vars[balance_vars %in% names(data)]
  fml_str <- paste0("got_ll ~ ", paste(avail_vars, collapse = " + "))
  f_test <- tryCatch({
    fit <- lm(as.formula(fml_str), data = data)
    fs <- summary(fit)$fstatistic
    f_pv <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)
    list(fstat = fs[1], pv = f_pv)
  }, error = function(e) list(fstat = NA, pv = NA))

  list(bal = bal, f_test = f_test, n = nrow(data), n_treated = sum(data$got_ll == 1))
}


# ==============================================================================
# STEP 3: PLAYER-EPISODE LEVEL BALANCE
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 3: PLAYER-EPISODE LEVEL BALANCE")
message(strrep("=", 70))

bal_atp <- run_balance(gs_atp, "ATP")
bal_wta <- run_balance(gs_wta, "WTA")

message("  ATP: N=", bal_atp$n, " (", bal_atp$n_treated, " treated)")
message("  ATP joint F-test p = ", fmt(bal_atp$f_test$pv, 3))
message("  WTA: N=", bal_wta$n, " (", bal_wta$n_treated, " treated)")
message("  WTA joint F-test p = ", fmt(bal_wta$f_test$pv, 3))


# Stacked panel balance tests omitted: stacking mechanically multiplies
# observations 5x with identical covariates, inflating the F-statistic.
# The player-episode level is the appropriate unit for balance checks.


# ==============================================================================
# STEP 5: GENERATE LATEX TABLES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 5: GENERATE TABLES")
message(strrep("=", 70))

generate_balance_table <- function(bal_atp, bal_wta, label_suffix = "") {
  vars_order <- c("pre_rank_pts_scaled", "pre_rank_pts_sq",
                  "pre_elo_scaled", "pre_elo_sq",
                  "pre_surf_elo_scaled", "pre_surf_elo_sq",
                  "player_age", "had_prior_ll",
                  "n_prior_gs_ll_won", "n_prior_gs_ll_notwon",
                  "n_prior_nongs_ll_won", "n_prior_nongs_ll_notwon")

  lines <- c(
    "\\begin{tabular}{l rrr rrr}",
    "\\toprule",
    " & \\multicolumn{3}{c}{ATP} & \\multicolumn{3}{c}{WTA} \\\\",
    "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
    "Variable & LL & Control & $p$ & LL & Control & $p$ \\\\",
    "\\midrule"
  )

  for (v in vars_order) {
    ra <- bal_atp$bal[bal_atp$bal$variable == v, ]
    rw <- bal_wta$bal[bal_wta$bal$variable == v, ]
    if (nrow(ra) == 0 && nrow(rw) == 0) next

    lab <- if (nrow(ra) > 0) ra$label[1] else rw$label[1]

    atp_str <- if (nrow(ra) > 0) {
      paste0(fmt(ra$ll_mean), " & ", fmt(ra$ct_mean), " & ", fmt(ra$pvalue, 3))
    } else " & & "

    wta_str <- if (nrow(rw) > 0) {
      paste0(fmt(rw$ll_mean), " & ", fmt(rw$ct_mean), " & ", fmt(rw$pvalue, 3))
    } else " & & "

    lines <- c(lines, paste0(lab, " & ", atp_str, " & ", wta_str, " \\\\"))
  }

  lines <- c(lines, "\\midrule")
  lines <- c(lines, paste0(
    "Joint $F$-test $p$-value & \\multicolumn{3}{c}{",
    fmt(bal_atp$f_test$pv, 3), "} & \\multicolumn{3}{c}{",
    fmt(bal_wta$f_test$pv, 3), "} \\\\"
  ))
  lines <- c(lines, paste0(
    "$N$ & \\multicolumn{3}{c}{",
    bal_atp$n, "} & \\multicolumn{3}{c}{",
    bal_wta$n, "} \\\\"
  ))
  lines <- c(lines, "\\bottomrule", "\\end{tabular}")

  paste(lines, collapse = "\n")
}

# Player-episode level
table_ep <- generate_balance_table(bal_atp, bal_wta)
writeLines(table_ep, file.path(TABLES_DIR, "table_balance.tex"))
message("  Saved: table_balance.tex")

# Stacked panel balance test omitted (see note above).

# Print results for inspection
message("\n  === Player-Episode Balance ===")
message("  ATP (N=", bal_atp$n, "):")
for (i in seq_len(nrow(bal_atp$bal))) {
  r <- bal_atp$bal[i, ]
  star <- ifelse(r$pvalue < 0.05, " **", ifelse(r$pvalue < 0.10, " *", ""))
  message(sprintf("    %-30s  LL=%.2f  Ctrl=%.2f  p=%.3f%s",
                  r$label, r$ll_mean, r$ct_mean, r$pvalue, star))
}
message("  Joint F-test p = ", fmt(bal_atp$f_test$pv, 3))

message("\n  WTA (N=", bal_wta$n, "):")
for (i in seq_len(nrow(bal_wta$bal))) {
  r <- bal_wta$bal[i, ]
  star <- ifelse(r$pvalue < 0.05, " **", ifelse(r$pvalue < 0.10, " *", ""))
  message(sprintf("    %-30s  LL=%.2f  Ctrl=%.2f  p=%.3f%s",
                  r$label, r$ll_mean, r$ct_mean, r$pvalue, star))
}
message("  Joint F-test p = ", fmt(bal_wta$f_test$pv, 3))

message("\nDONE.")
