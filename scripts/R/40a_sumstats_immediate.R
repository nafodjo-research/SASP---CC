# ==============================================================================
# 40a_sumstats_immediate.R
# Summary statistics, immediate effects, and non-GS balance tables
#
# Purpose:
#   1. Summary statistics tables (4): GS ATP, GS WTA, non-GS ATP, non-GS WTA
#   2. Immediate effects tables (2): GS and non-GS (reformatted)
#   3. Non-GS balance test table (1)
#
# Inputs:
#   Data/cleaned/skeleton_gs_est_v3.rds
#   Data/cleaned/skeleton_nongs_est_v6.rds
#   Data/cleaned/tournament_elo_cache.rds
#
# Outputs:
#   Tables/table_sumstats_gs_atp.tex
#   Tables/table_sumstats_gs_wta.tex
#   Tables/table_sumstats_nongs_atp.tex
#   Tables/table_sumstats_nongs_wta.tex
#   Tables/table_immediate_effects.tex
#   Tables/table_immediate_effects_nongs.tex
#   Tables/table_balance_nongs.tex
#
# Dependencies: dplyr, here, fixest
# ==============================================================================

set.seed(20260327)

library(dplyr)
library(here)
library(fixest)

source(here("scripts", "R", "utils.R"))
summary_log <- character()

CLEANED_DIR <- here("Data", "cleaned")
TABLES_DIR  <- here("Tables")

# ==============================================================================
# LOAD DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("LOADING DATA")
message(strrep("=", 70))

gs_all  <- readRDS(file.path(CLEANED_DIR, "skeleton_gs_est_v3.rds"))
ngs_all <- readRDS(file.path(CLEANED_DIR, "skeleton_nongs_est_v6.rds"))
elo_cache <- readRDS(file.path(CLEANED_DIR, "tournament_elo_cache.rds"))

message("  GS: ", nrow(gs_all), " rows (ATP=", sum(gs_all$tour == "ATP"),
        ", WTA=", sum(gs_all$tour == "WTA"), ")")
message("  Non-GS: ", nrow(ngs_all), " rows (ATP=", sum(ngs_all$tour == "ATP"),
        ", WTA=", sum(ngs_all$tour == "WTA"), ")")

# ==============================================================================
# SURFACE ELO LOOKUP
# ==============================================================================

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

add_surface_elo <- function(df, tour_prefix) {
  df$pre_surf_elo <- NA_real_
  for (i in seq_len(nrow(df))) {
    td <- df$tourney_date[i]
    if (is.numeric(td)) td <- as.Date(as.character(td), format = "%Y%m%d")
    df$pre_surf_elo[i] <- get_surface_elo_single(
      df$player_id[i], td, df$surface[i], tour_prefix)
  }
  # Fallback: use overall Elo where surface Elo is missing
  df$pre_surf_elo[is.na(df$pre_surf_elo)] <- df$pre_elo[is.na(df$pre_surf_elo)]
  df
}

message("  Computing surface Elo for GS ATP...")
gs_atp <- add_surface_elo(gs_all[gs_all$tour == "ATP", ], "ATP")
message("  Computing surface Elo for GS WTA...")
gs_wta <- add_surface_elo(gs_all[gs_all$tour == "WTA", ], "WTA")

message("  Computing surface Elo for non-GS ATP...")
ngs_atp <- add_surface_elo(ngs_all[ngs_all$tour == "ATP", ], "ATP")
message("  Computing surface Elo for non-GS WTA...")
ngs_wta <- add_surface_elo(ngs_all[ngs_all$tour == "WTA", ], "WTA")

# ##############################################################################
# PART 1: SUMMARY STATISTICS TABLES
# ##############################################################################
message("\n", strrep("=", 70))
message("PART 1: SUMMARY STATISTICS TABLES")
message(strrep("=", 70))

generate_sumstats_table <- function(df, is_nongs = FALSE) {
  # Define variable list for Panel A
  # Note: GS pre_elo is stored as Elo/100 (~17); non-GS pre_elo is raw (~1700)
  elo_scale <- if (is_nongs) 1 else 100
  panel_a_vars <- list(
    list(col = "player_age",            label = "Age",                      scale = 1),
    list(col = "pre_rank_pts",          label = "Ranking points",           scale = 1),
    list(col = "pre_elo",               label = "Elo rating",              scale = elo_scale),
    list(col = "pre_surf_elo",          label = "Surface Elo",             scale = 1),
    list(col = "n_prior_gs_ll_won",     label = "Prior GS LL won",         scale = 1),
    list(col = "n_prior_gs_ll_notwon",  label = "Prior GS LL not won",     scale = 1),
    list(col = "n_prior_nongs_ll_won",  label = "Prior non-GS LL won",     scale = 1),
    list(col = "n_prior_nongs_ll_notwon", label = "Prior non-GS LL not won", scale = 1),
    list(col = "had_prior_ll",          label = "Had prior LL",            scale = 1)
  )
  if (is_nongs) {
    panel_a_vars <- c(panel_a_vars, list(
      list(col = "peer_component", label = "$P_i^{LL}$", scale = 1)
    ))
  }

  # Panel B: Outcomes
  panel_b_vars <- list(
    list(col = "points_change_4w",      label = "Ranking points $\\Delta$ (4w)"),
    list(col = "points_change_26w",     label = "Ranking points $\\Delta$ (26w)"),
    list(col = "points_change_52w",     label = "Ranking points $\\Delta$ (52w)"),
    list(col = "elo_change_4w",         label = "Elo $\\Delta$ (4w)"),
    list(col = "elo_change_26w",        label = "Elo $\\Delta$ (26w)"),
    list(col = "elo_change_52w",        label = "Elo $\\Delta$ (52w)"),
    list(col = "n_main_draws_26w",      label = "Main draws entered (26w)"),
    list(col = "n_matches_250plus_26w", label = "Matches at 250+ events (26w)")
  )

  ll  <- df[df$got_ll == 1, ]
  ctl <- df[df$got_ll == 0, ]

  make_row <- function(var_info) {
    col   <- var_info$col
    lab   <- var_info$label
    scale <- if (!is.null(var_info$scale)) var_info$scale else 1
    if (!col %in% names(df)) {
      return(paste0("\\quad ", lab, " & -- & -- & -- & -- & -- & -- \\\\"))
    }
    x_ll  <- ll[[col]] * scale;  x_ll  <- x_ll[!is.na(x_ll)]
    x_ctl <- ctl[[col]] * scale; x_ctl <- x_ctl[!is.na(x_ctl)]

    paste0("\\quad ", lab, " & ",
           fmt(mean(x_ll), 2), " & ", fmt(sd(x_ll), 2), " & ", length(x_ll), " & ",
           fmt(mean(x_ctl), 2), " & ", fmt(sd(x_ctl), 2), " & ", length(x_ctl), " \\\\")
  }

  ncol_span <- 7
  lines <- c(
    "\\begin{tabular}{p{4cm} rrr rrr}",
    "\\toprule",
    paste0(" & \\multicolumn{3}{c}{LL Selected} & \\multicolumn{3}{c}{Not Selected} \\\\"),
    "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
    "Variable & Mean & SD & $N$ & Mean & SD & $N$ \\\\",
    "\\midrule",
    paste0("\\multicolumn{", ncol_span, "}{l}{\\textit{Panel A: Pre-Treatment}} \\\\")
  )
  for (v in panel_a_vars) lines <- c(lines, make_row(v))

  lines <- c(lines,
    "\\midrule",
    paste0("\\multicolumn{", ncol_span, "}{l}{\\textit{Panel B: Outcomes}} \\\\")
  )
  for (v in panel_b_vars) lines <- c(lines, make_row(v))

  lines <- c(lines, "\\bottomrule", "\\end{tabular}")
  paste(lines, collapse = "\n")
}

# Generate 4 tables
tab_gs_atp <- generate_sumstats_table(gs_atp, is_nongs = FALSE)
writeLines(tab_gs_atp, file.path(TABLES_DIR, "table_sumstats_gs_atp.tex"))
message("  Saved: table_sumstats_gs_atp.tex")

tab_gs_wta <- generate_sumstats_table(gs_wta, is_nongs = FALSE)
writeLines(tab_gs_wta, file.path(TABLES_DIR, "table_sumstats_gs_wta.tex"))
message("  Saved: table_sumstats_gs_wta.tex")

tab_ngs_atp <- generate_sumstats_table(ngs_atp, is_nongs = TRUE)
writeLines(tab_ngs_atp, file.path(TABLES_DIR, "table_sumstats_nongs_atp.tex"))
message("  Saved: table_sumstats_nongs_atp.tex")

tab_ngs_wta <- generate_sumstats_table(ngs_wta, is_nongs = TRUE)
writeLines(tab_ngs_wta, file.path(TABLES_DIR, "table_sumstats_nongs_wta.tex"))
message("  Saved: table_sumstats_nongs_wta.tex")


# ##############################################################################
# PART 2: IMMEDIATE EFFECTS TABLES
# ##############################################################################
message("\n", strrep("=", 70))
message("PART 2: IMMEDIATE EFFECTS TABLES")
message(strrep("=", 70))

# --- 2a: GS Immediate Effects ------------------------------------------------
# Outcomes: md_any_win, md_matches_played, md_points
# For GS: t-test + event-FE regression

generate_immediate_gs <- function(gs_atp, gs_wta) {
  outcomes <- list(
    list(col = "md_any_win",         label = "Main-draw match win"),
    list(col = "md_matches_played",  label = "Main-draw matches played"),
    list(col = "md_points",          label = "Ranking points at event")
  )

  # slam_year FE for GS
  run_one_tour <- function(df, tour_label) {
    rows <- list()
    for (oc in outcomes) {
      y <- df[[oc$col]]
      d <- df$got_ll
      ok <- !is.na(y)

      ll_vals  <- y[ok & d == 1]
      ctl_vals <- y[ok & d == 0]
      ll_mean  <- mean(ll_vals)

      tt <- tryCatch(t.test(ll_vals, ctl_vals), error = function(e) NULL)
      pv_tt <- if (!is.null(tt)) tt$p.value else NA_real_

      # Event FE regression
      reg_df <- df[ok, ]
      reg_df$y <- y[ok]
      fit <- tryCatch(
        feols(y ~ got_ll | slam_year, data = reg_df),
        error = function(e) NULL
      )
      if (!is.null(fit)) {
        coef_val <- coef(fit)["got_ll"]
        se_val   <- sqrt(vcov(fit)["got_ll", "got_ll"])
        pv_reg   <- 2 * pt(-abs(coef_val / se_val), df = fit$nobs - fit$nparams)
      } else {
        coef_val <- NA; se_val <- NA; pv_reg <- NA
      }

      rows[[oc$col]] <- list(
        label = oc$label,
        ll_mean = ll_mean, pv_tt = pv_tt,
        coef = coef_val, se = se_val, pv_reg = pv_reg
      )
    }
    n_ll  <- sum(df$got_ll == 1)
    n_ctl <- sum(df$got_ll == 0)
    list(rows = rows, n_ll = n_ll, n_ctl = n_ctl, n_total = nrow(df))
  }

  atp_res <- run_one_tour(gs_atp, "ATP")
  wta_res <- run_one_tour(gs_wta, "WTA")

  # Pooled
  gs_pooled <- rbind(gs_atp, gs_wta)
  pool_res <- run_one_tour(gs_pooled, "Pooled")

  # Format table
  format_tour_rows <- function(res, tour_label) {
    lines <- c(paste0("\\multicolumn{5}{l}{\\textit{", tour_label, "}} \\\\"))
    for (r in res$rows) {
      ll_str <- paste0(fmt(r$ll_mean, 2), add_stars(r$pv_tt))
      coef_str <- paste0(fmt(r$coef, 2), add_stars(r$pv_reg))
      se_str <- fmt(r$se, 2)
      lines <- c(lines, paste0("\\quad ", r$label, " & ", ll_str,
                                " & ", coef_str, " & (", se_str, ") \\\\"))
    }
    lines <- c(lines, paste0("\\quad $N$ & \\multicolumn{1}{c}{",
                              res$n_ll, " / ", res$n_ctl,
                              "} & \\multicolumn{2}{c}{", res$n_total, "} \\\\"))
    lines
  }

  lines <- c(
    "\\begin{tabular}{l ccc}",
    "\\toprule",
    " & $t$-test & \\multicolumn{2}{c}{Event FE} \\\\",
    "\\cmidrule(lr){2-2} \\cmidrule(lr){3-4}",
    "Outcome & LL Mean & $\\hat{\\beta}$ & (SE) \\\\",
    "\\midrule"
  )
  lines <- c(lines, format_tour_rows(atp_res, "ATP"))
  lines <- c(lines, "\\addlinespace")
  lines <- c(lines, format_tour_rows(wta_res, "WTA"))
  lines <- c(lines, "\\addlinespace")
  lines <- c(lines, format_tour_rows(pool_res, "Pooled"))
  lines <- c(lines, "\\bottomrule", "\\end{tabular}")

  paste(lines, collapse = "\n")
}

tab_imm_gs <- generate_immediate_gs(gs_atp, gs_wta)
writeLines(tab_imm_gs, file.path(TABLES_DIR, "table_immediate_effects.tex"))
message("  Saved: table_immediate_effects.tex")

# --- 2b: Non-GS Immediate Effects --------------------------------------------
# For non-GS: t-test + OLS with controls (pre_rank_pts, pre_rank_pts_sq,
#   pre_elo, pre_elo_sq, player_age, had_prior_ll, peer_component)

generate_immediate_nongs <- function(ngs_atp, ngs_wta) {
  # For non-GS, md_points is all 0; use deepest_round_pts instead
  # Controls (got_ll==0) have NA for deepest_round_pts; treat as 0
  ngs_atp$event_points <- ifelse(is.na(ngs_atp$deepest_round_pts), 0, ngs_atp$deepest_round_pts)
  ngs_wta$event_points <- ifelse(is.na(ngs_wta$deepest_round_pts), 0, ngs_wta$deepest_round_pts)

  outcomes <- list(
    list(col = "md_any_win",         label = "Main-draw match win"),
    list(col = "md_matches_played",  label = "Main-draw matches played"),
    list(col = "event_points",       label = "Ranking points at event")
  )

  run_one_tour <- function(df, tour_label) {
    rows <- list()
    for (oc in outcomes) {
      y <- df[[oc$col]]
      d <- df$got_ll
      ok <- !is.na(y)

      ll_vals  <- y[ok & d == 1]
      ctl_vals <- y[ok & d == 0]
      ll_mean  <- mean(ll_vals)

      tt <- tryCatch(t.test(ll_vals, ctl_vals), error = function(e) NULL)
      pv_tt <- if (!is.null(tt)) tt$p.value else NA_real_

      # OLS with controls
      reg_df <- df[ok, ]
      reg_df$y <- y[ok]
      fit <- tryCatch(
        feols(y ~ got_ll + pre_rank_pts + pre_rank_pts_sq + pre_elo +
                pre_elo_sq + player_age + had_prior_ll + peer_component,
              data = reg_df, cluster = ~player_id),
        error = function(e) NULL
      )
      if (!is.null(fit)) {
        coef_val <- coef(fit)["got_ll"]
        se_val   <- sqrt(vcov(fit)["got_ll", "got_ll"])
        pv_reg   <- 2 * pnorm(-abs(coef_val / se_val))
      } else {
        coef_val <- NA; se_val <- NA; pv_reg <- NA
      }

      rows[[oc$col]] <- list(
        label = oc$label,
        ll_mean = ll_mean, pv_tt = pv_tt,
        coef = coef_val, se = se_val, pv_reg = pv_reg
      )
    }
    n_ll  <- sum(df$got_ll == 1)
    n_ctl <- sum(df$got_ll == 0)
    list(rows = rows, n_ll = n_ll, n_ctl = n_ctl, n_total = nrow(df))
  }

  atp_res <- run_one_tour(ngs_atp, "ATP")
  wta_res <- run_one_tour(ngs_wta, "WTA")

  # Pooled
  ngs_pooled <- rbind(ngs_atp, ngs_wta)
  pool_res <- run_one_tour(ngs_pooled, "Pooled")

  format_tour_rows <- function(res, tour_label) {
    lines <- c(paste0("\\multicolumn{5}{l}{\\textit{", tour_label, "}} \\\\"))
    for (r in res$rows) {
      ll_str <- paste0(fmt(r$ll_mean, 2), add_stars(r$pv_tt))
      coef_str <- paste0(fmt(r$coef, 2), add_stars(r$pv_reg))
      se_str <- fmt(r$se, 2)
      lines <- c(lines, paste0("\\quad ", r$label, " & ", ll_str,
                                " & ", coef_str, " & (", se_str, ") \\\\"))
    }
    lines <- c(lines, paste0("\\quad $N$ & \\multicolumn{1}{c}{",
                              res$n_ll, " / ", res$n_ctl,
                              "} & \\multicolumn{2}{c}{", res$n_total, "} \\\\"))
    lines
  }

  lines <- c(
    "\\begin{tabular}{l ccc}",
    "\\toprule",
    " & $t$-test & \\multicolumn{2}{c}{LOO-IV} \\\\",
    "\\cmidrule(lr){2-2} \\cmidrule(lr){3-4}",
    "Outcome & LL Mean & $\\hat{\\beta}_{IV}$ & (SE) \\\\",
    "\\midrule"
  )
  lines <- c(lines, format_tour_rows(atp_res, "ATP"))
  lines <- c(lines, "\\addlinespace")
  lines <- c(lines, format_tour_rows(wta_res, "WTA"))
  lines <- c(lines, "\\addlinespace")
  lines <- c(lines, format_tour_rows(pool_res, "Pooled"))
  lines <- c(lines, "\\bottomrule", "\\end{tabular}")

  paste(lines, collapse = "\n")
}

tab_imm_ngs <- generate_immediate_nongs(ngs_atp, ngs_wta)
writeLines(tab_imm_ngs, file.path(TABLES_DIR, "table_immediate_effects_nongs.tex"))
message("  Saved: table_immediate_effects_nongs.tex")


# ##############################################################################
# PART 3: NON-GS BALANCE TEST
# ##############################################################################
message("\n", strrep("=", 70))
message("PART 3: NON-GS BALANCE TEST")
message(strrep("=", 70))

# Add scaled covariates for balance
add_balance_vars_nongs <- function(df, tour_prefix) {
  df$pre_rank_pts_scaled <- df$pre_rank_pts / 1000
  df$pre_rank_pts_sq_bal <- (df$pre_rank_pts / 1000)^2
  df$pre_elo_scaled      <- df$pre_elo / 100
  df$pre_elo_sq_bal      <- (df$pre_elo / 100)^2
  df$pre_surf_elo_scaled <- df$pre_surf_elo / 100
  df$pre_surf_elo_sq_bal <- (df$pre_surf_elo / 100)^2
  df
}

ngs_atp <- add_balance_vars_nongs(ngs_atp, "ATP")
ngs_wta <- add_balance_vars_nongs(ngs_wta, "WTA")

run_balance_nongs <- function(data, sample_label) {
  balance_vars <- c(
    "pre_rank_pts_scaled", "pre_rank_pts_sq_bal",
    "pre_elo_scaled", "pre_elo_sq_bal",
    "pre_surf_elo_scaled", "pre_surf_elo_sq_bal",
    "player_age",
    "had_prior_ll",
    "n_prior_gs_ll_won", "n_prior_gs_ll_notwon",
    "n_prior_nongs_ll_won", "n_prior_nongs_ll_notwon"
  )

  bal_var_labels <- c(
    "pre_rank_pts_scaled"  = "Ranking points / 1000",
    "pre_rank_pts_sq_bal"  = "(Ranking points / 1000)$^2$",
    "pre_elo_scaled"       = "Elo / 100",
    "pre_elo_sq_bal"       = "(Elo / 100)$^2$",
    "pre_surf_elo_scaled"  = "Surface Elo / 100",
    "pre_surf_elo_sq_bal"  = "(Surface Elo / 100)$^2$",
    "player_age"           = "Age",
    "had_prior_ll"         = "Had prior LL",
    "n_prior_gs_ll_won"    = "Prior GS LL won",
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
      ll_mean  = mean(x[ok & d == 1]),
      ct_mean  = mean(x[ok & d == 0]),
      diff     = mean(x[ok & d == 1]) - mean(x[ok & d == 0]),
      pvalue   = if (!is.null(tt)) tt$p.value else NA_real_,
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

bal_ngs_atp <- run_balance_nongs(ngs_atp, "ATP")
bal_ngs_wta <- run_balance_nongs(ngs_wta, "WTA")

message("  Non-GS ATP: N=", bal_ngs_atp$n, " (", bal_ngs_atp$n_treated, " treated)")
message("  Non-GS ATP joint F-test p = ", fmt(bal_ngs_atp$f_test$pv, 3))
message("  Non-GS WTA: N=", bal_ngs_wta$n, " (", bal_ngs_wta$n_treated, " treated)")
message("  Non-GS WTA joint F-test p = ", fmt(bal_ngs_wta$f_test$pv, 3))

# Generate LaTeX table
generate_balance_nongs_table <- function(bal_atp, bal_wta) {
  vars_order <- c("pre_rank_pts_scaled", "pre_rank_pts_sq_bal",
                  "pre_elo_scaled", "pre_elo_sq_bal",
                  "pre_surf_elo_scaled", "pre_surf_elo_sq_bal",
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

tab_bal_ngs <- generate_balance_nongs_table(bal_ngs_atp, bal_ngs_wta)
writeLines(tab_bal_ngs, file.path(TABLES_DIR, "table_balance_nongs.tex"))
message("  Saved: table_balance_nongs.tex")


# ==============================================================================
# PRINT RESULTS SUMMARY
# ==============================================================================
message("\n", strrep("=", 70))
message("RESULTS SUMMARY")
message(strrep("=", 70))

message("\n  --- GS ATP Summary Stats (N=", nrow(gs_atp), ") ---")
message("    LL: ", sum(gs_atp$got_ll), " | Control: ", sum(gs_atp$got_ll == 0))

message("\n  --- GS WTA Summary Stats (N=", nrow(gs_wta), ") ---")
message("    LL: ", sum(gs_wta$got_ll), " | Control: ", sum(gs_wta$got_ll == 0))

message("\n  --- Non-GS ATP Summary Stats (N=", nrow(ngs_atp), ") ---")
message("    LL: ", sum(ngs_atp$got_ll), " | Control: ", sum(ngs_atp$got_ll == 0))

message("\n  --- Non-GS WTA Summary Stats (N=", nrow(ngs_wta), ") ---")
message("    LL: ", sum(ngs_wta$got_ll), " | Control: ", sum(ngs_wta$got_ll == 0))

message("\n  --- Non-GS Balance ---")
message("  ATP joint F-test: F=", fmt(bal_ngs_atp$f_test$fstat, 2),
        " p=", fmt(bal_ngs_atp$f_test$pv, 3))
message("  WTA joint F-test: F=", fmt(bal_ngs_wta$f_test$fstat, 2),
        " p=", fmt(bal_ngs_wta$f_test$pv, 3))

# Print balance details
for (tour_label in c("ATP", "WTA")) {
  bal_obj <- if (tour_label == "ATP") bal_ngs_atp else bal_ngs_wta
  message(sprintf("\n  Non-GS %s Balance (N=%d):", tour_label, bal_obj$n))
  for (i in seq_len(nrow(bal_obj$bal))) {
    r <- bal_obj$bal[i, ]
    star <- ifelse(r$pvalue < 0.01, " ***",
                   ifelse(r$pvalue < 0.05, " **",
                          ifelse(r$pvalue < 0.10, " *", "")))
    message(sprintf("    %-30s  LL=%.3f  Ctrl=%.3f  p=%.3f%s",
                    r$label, r$ll_mean, r$ct_mean, r$pvalue, star))
  }
}

message("\nDONE.")
