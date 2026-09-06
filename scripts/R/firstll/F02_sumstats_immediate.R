# ==============================================================================
# F02_sumstats_immediate.R
# Summary statistics and immediate effects tables for first-LL sample.
#
# Adapts 40a_sumstats_immediate.R for the first-LL restricted sample.
# Key difference: Panel A drops prior-LL variables (all zero by construction).
#
# Inputs:
#   Data/cleaned/firstll/firstll_gs_est.rds
#   Data/cleaned/firstll/firstll_nongs_est.rds
#
# Outputs:
#   Tables_FirstLL/table_sumstats_gs_atp.tex
#   Tables_FirstLL/table_sumstats_gs_wta.tex
#   Tables_FirstLL/table_sumstats_nongs_atp.tex
#   Tables_FirstLL/table_sumstats_nongs_wta.tex
#   Tables_FirstLL/table_immediate.tex          (GS)
#   Tables_FirstLL/table_immediate_nongs.tex    (nonGS)
#
# Dependencies: dplyr, fixest, here
# ==============================================================================

set.seed(20260416)

library(dplyr)
library(fixest)
library(here)

source(here("scripts", "R", "firstll", "firstll_helpers.R"))
summary_log <- character()

# ==============================================================================
# LOAD DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("F02: LOADING DATA")
message(strrep("=", 70))

gs  <- readRDS(file.path(FIRSTLL_CLEANED, "firstll_gs_est.rds"))
ngs <- readRDS(file.path(FIRSTLL_CLEANED, "firstll_nongs_est.rds"))

gs  <- ensure_scaled(gs)
ngs <- ensure_scaled(ngs)

# Split by tour
gs_atp  <- gs[gs$tour == "ATP", ]
gs_wta  <- gs[gs$tour == "WTA", ]
ngs_atp <- ngs[ngs$tour == "ATP", ]
ngs_wta <- ngs[ngs$tour == "WTA", ]

message("  GS-ATP: ", nrow(gs_atp), " | GS-WTA: ", nrow(gs_wta))
message("  NGS-ATP: ", nrow(ngs_atp), " | NGS-WTA: ", nrow(ngs_wta))


# ##############################################################################
# PART 1: SUMMARY STATISTICS TABLES
# ##############################################################################
message("\n", strrep("=", 70))
message("PART 1: SUMMARY STATISTICS TABLES")
message(strrep("=", 70))

generate_sumstats_table <- function(df, is_nongs = FALSE) {
  # Panel A: pre-treatment variables
  # First-LL: NO prior-LL vars (all zero by construction)
  panel_a_vars <- list(
    list(col = "player_age",   label = "Age",             scale = 1),
    list(col = "pre_rank_pts", label = "Ranking points",  scale = 1),
    list(col = "pre_elo",      label = "Elo rating",      scale = 1),
    list(col = "pre_surf_elo", label = "Surface Elo",     scale = 1)
  )
  if (is_nongs && "peer_component" %in% names(df)) {
    panel_a_vars <- c(panel_a_vars, list(
      list(col = "peer_component", label = "$P_i^{LL}$", scale = 1)
    ))
  }

  # Panel B: outcomes at 4w, 26w, 52w horizons
  panel_b_vars <- list(
    list(col = "points_change_4w",  label = "Ranking points $\\Delta$ (4w)"),
    list(col = "points_change_26w", label = "Ranking points $\\Delta$ (26w)"),
    list(col = "points_change_52w", label = "Ranking points $\\Delta$ (52w)"),
    list(col = "elo_change_4w",     label = "Elo $\\Delta$ (4w)"),
    list(col = "elo_change_26w",    label = "Elo $\\Delta$ (26w)"),
    list(col = "elo_change_52w",    label = "Elo $\\Delta$ (52w)"),
    list(col = "n_main_draws_26w",  label = "Main draws entered (26w)"),
    list(col = "n_main_draws_52w",  label = "Main draws entered (52w)")
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
for (info in list(
  list(df = gs_atp,  nongs = FALSE, file = "table_sumstats_gs_atp.tex"),
  list(df = gs_wta,  nongs = FALSE, file = "table_sumstats_gs_wta.tex"),
  list(df = ngs_atp, nongs = TRUE,  file = "table_sumstats_nongs_atp.tex"),
  list(df = ngs_wta, nongs = TRUE,  file = "table_sumstats_nongs_wta.tex")
)) {
  tab <- generate_sumstats_table(info$df, is_nongs = info$nongs)
  writeLines(tab, file.path(FIRSTLL_TABLES, info$file))
  message("  Saved: ", info$file)
}


# ##############################################################################
# PART 2: IMMEDIATE EFFECTS TABLES
# ##############################################################################
message("\n", strrep("=", 70))
message("PART 2: IMMEDIATE EFFECTS TABLES")
message(strrep("=", 70))

# --- 2a: GS Immediate Effects ------------------------------------------------
# Outcomes: md_any_win, md_matches_played, md_points_earned
# Model: feols(Y ~ got_ll | slam_year)

generate_immediate_gs <- function(gs_atp, gs_wta) {
  # Check which immediate outcome columns exist
  candidate_outcomes <- c("md_any_win", "md_matches_played", "md_points_earned",
                          "md_points", "md_matches_won")
  avail <- candidate_outcomes[candidate_outcomes %in% names(gs_atp)]

  outcomes <- list()
  if ("md_any_win" %in% avail)
    outcomes <- c(outcomes, list(list(col = "md_any_win", label = "Main-draw match win")))
  if ("md_matches_played" %in% avail)
    outcomes <- c(outcomes, list(list(col = "md_matches_played", label = "Main-draw matches played")))
  if ("md_points_earned" %in% avail) {
    outcomes <- c(outcomes, list(list(col = "md_points_earned", label = "Ranking points at event")))
  } else if ("md_points" %in% avail) {
    outcomes <- c(outcomes, list(list(col = "md_points", label = "Ranking points at event")))
  }

  if (length(outcomes) == 0) {
    message("  WARNING: No immediate outcome columns found. Skipping GS immediate table.")
    return(NULL)
  }

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

  atp_res  <- run_one_tour(gs_atp, "ATP")
  wta_res  <- run_one_tour(gs_wta, "WTA")
  pool_res <- run_one_tour(rbind(gs_atp, gs_wta), "Pooled")

  format_tour_rows <- function(res, tour_label) {
    lines <- c(paste0("\\multicolumn{4}{l}{\\textit{", tour_label, "}} \\\\"))
    for (r in res$rows) {
      ll_str   <- paste0(fmt(r$ll_mean, 2), add_stars(r$pv_tt))
      coef_str <- paste0(fmt(r$coef, 2), add_stars(r$pv_reg))
      se_str   <- fmt(r$se, 2)
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
if (!is.null(tab_imm_gs)) {
  writeLines(tab_imm_gs, file.path(FIRSTLL_TABLES, "table_immediate.tex"))
  message("  Saved: table_immediate.tex")
}

# --- 2b: Non-GS Immediate Effects --------------------------------------------
# For non-GS: feols(Y ~ got_ll + v_hat | tourney_id)

generate_immediate_nongs <- function(ngs_atp, ngs_wta) {
  candidate_outcomes <- c("md_any_win", "md_matches_played", "md_points_earned",
                          "md_points", "event_points", "deepest_round_pts")
  avail <- candidate_outcomes[candidate_outcomes %in% names(ngs_atp)]

  outcomes <- list()
  if ("md_any_win" %in% avail)
    outcomes <- c(outcomes, list(list(col = "md_any_win", label = "Main-draw match win")))
  if ("md_matches_played" %in% avail)
    outcomes <- c(outcomes, list(list(col = "md_matches_played", label = "Main-draw matches played")))

  # Points outcome: try several candidates
  pts_col <- NULL
  for (pc in c("md_points_earned", "md_points", "event_points", "deepest_round_pts")) {
    if (pc %in% avail) { pts_col <- pc; break }
  }
  if (!is.null(pts_col)) {
    outcomes <- c(outcomes, list(list(col = pts_col, label = "Ranking points at event")))
  }

  if (length(outcomes) == 0) {
    message("  WARNING: No immediate outcome columns found. Skipping nonGS immediate table.")
    return(NULL)
  }

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

      # Control function: got_ll + v_hat | tourney_id
      reg_df <- df[ok, ]
      reg_df$y <- y[ok]

      # Use v_hat if available, otherwise just tourney_id FE
      if ("v_hat" %in% names(reg_df)) {
        fit <- tryCatch(
          feols(y ~ got_ll + v_hat | tourney_id, data = reg_df),
          error = function(e) NULL
        )
      } else {
        fit <- tryCatch(
          feols(y ~ got_ll | tourney_id, data = reg_df),
          error = function(e) NULL
        )
      }

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

  atp_res  <- run_one_tour(ngs_atp, "ATP")
  wta_res  <- run_one_tour(ngs_wta, "WTA")
  pool_res <- run_one_tour(rbind(ngs_atp, ngs_wta), "Pooled")

  format_tour_rows <- function(res, tour_label) {
    lines <- c(paste0("\\multicolumn{4}{l}{\\textit{", tour_label, "}} \\\\"))
    for (r in res$rows) {
      ll_str   <- paste0(fmt(r$ll_mean, 2), add_stars(r$pv_tt))
      coef_str <- paste0(fmt(r$coef, 2), add_stars(r$pv_reg))
      se_str   <- fmt(r$se, 2)
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
    " & $t$-test & \\multicolumn{2}{c}{CF + Tourney FE} \\\\",
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

tab_imm_ngs <- generate_immediate_nongs(ngs_atp, ngs_wta)
if (!is.null(tab_imm_ngs)) {
  writeLines(tab_imm_ngs, file.path(FIRSTLL_TABLES, "table_immediate_nongs.tex"))
  message("  Saved: table_immediate_nongs.tex")
}


# ==============================================================================
# SUMMARY
# ==============================================================================
message("\n", strrep("=", 70))
message("F02 SUMMARY")
message(strrep("=", 70))
message("  GS-ATP:  N=", nrow(gs_atp), " (LL=", sum(gs_atp$got_ll), ")")
message("  GS-WTA:  N=", nrow(gs_wta), " (LL=", sum(gs_wta$got_ll), ")")
message("  NGS-ATP: N=", nrow(ngs_atp), " (LL=", sum(ngs_atp$got_ll), ")")
message("  NGS-WTA: N=", nrow(ngs_wta), " (LL=", sum(ngs_wta$got_ll), ")")
message("\nDONE.")
