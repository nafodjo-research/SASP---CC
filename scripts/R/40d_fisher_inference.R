# ==============================================================================
# 40d_fisher_inference.R
# Purpose: Fisher randomization inference for stacked horizon models.
#          Permutes got_ll within slam x year blocks (GS) or tourney_id blocks
#          (nonGS), re-estimates the FULL stacked model, and compares actual
#          got_ll:horizon coefficients to the permutation distribution.
#
# Inputs:
#   Data/cleaned/skeleton_gs_est_v4.rds   (GS sample with surface Elo)
#   Data/cleaned/skeleton_nongs_est_v7.rds (nonGS sample with surface Elo, v_hat)
#
# Outputs:
#   Tables/table_fisher_fixed.tex          (GS Fisher inference table)
#   Tables/table_fisher_nongs.tex          (nonGS Fisher inference table)
#   Data/cleaned/fisher_fixed.rds          (full results object)
#
# Dependencies: fixest, here, dplyr
# ==============================================================================

set.seed(20260327)

library(fixest)
library(here)
library(dplyr)

source(here("scripts", "R", "utils.R"))

# --- Constants ----------------------------------------------------------------
N_PERMS  <- 1000
HORIZONS <- c(4, 8, 12, 26, 52)
HORIZON_LABS <- paste0(HORIZONS, "w")

outcomes_base <- c("points_change", "n_main_draws", "n_matches_250plus", "elo_change")

outcome_labels <- c(
  points_change    = "Ranking pts $\\Delta$",
  n_main_draws     = "Main draws entered",
  n_matches_250plus = "Matches (250+)",
  elo_change       = "Elo $\\Delta$"
)

CLEANED_DIR <- here("Data", "cleaned")
TABLE_DIR   <- here("Tables")
dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)

# --- GS Z^pre (all interacted with horizon, scaled) --------------------------
ZPRE_GS <- paste0(
  "pre_rank_pts_s:horizon + pre_rank_pts_sq_s:horizon + ",
  "pre_elo_s:horizon + pre_elo_sq_s:horizon + ",
  "pre_surf_elo_s:horizon + pre_surf_elo_sq_s:horizon + ",
  "n_prior_gs_ll_won:horizon + n_prior_gs_ll_notwon:horizon + ",
  "n_prior_nongs_ll_won:horizon + n_prior_nongs_ll_notwon:horizon + ",
  "player_age:horizon"
)

# --- NonGS Z^pre (main effects) ----------------------------------------------
ZPRE_NONGS <- paste0(
  "pre_rank_pts_s + pre_rank_pts_sq_s + pre_elo_s + pre_elo_sq_s + ",
  "pre_surf_elo_s + pre_surf_elo_sq_s + ",
  "n_prior_gs_ll_won + n_prior_gs_ll_notwon + ",
  "n_prior_nongs_ll_won + n_prior_nongs_ll_notwon + ",
  "player_age"
)


# ==============================================================================
# SECTION 1: LOAD AND PREPARE DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("FISHER RANDOMIZATION INFERENCE")
message(strrep("=", 70))

# --- GS data ------------------------------------------------------------------
gs <- readRDS(file.path(CLEANED_DIR, "skeleton_gs_est_v4.rds"))
message("GS data loaded: ", nrow(gs), " rows")

# Add scaled variables
gs$pre_rank_pts_s    <- gs$pre_rank_pts / 1000
gs$pre_rank_pts_sq_s <- gs$pre_rank_pts_s^2
gs$pre_elo_s         <- gs$pre_elo / 100
gs$pre_elo_sq_s      <- gs$pre_elo_s^2
gs$pre_surf_elo_s    <- gs$pre_surf_elo / 100
gs$pre_surf_elo_sq_s <- gs$pre_surf_elo_s^2

# Split by tour
gs_atp <- gs[gs$tour == "ATP", ]
gs_wta <- gs[gs$tour == "WTA", ]
message("  GS ATP: ", nrow(gs_atp), " | GS WTA: ", nrow(gs_wta))

# --- NonGS data ---------------------------------------------------------------
nongs <- readRDS(file.path(CLEANED_DIR, "skeleton_nongs_est_v7.rds"))
message("NonGS data loaded: ", nrow(nongs), " rows")

# Add scaled variables
nongs$pre_rank_pts_s    <- nongs$pre_rank_pts / 1000
nongs$pre_rank_pts_sq_s <- nongs$pre_rank_pts_s^2
nongs$pre_elo_s         <- nongs$pre_elo / 100
nongs$pre_elo_sq_s      <- nongs$pre_elo_s^2
nongs$pre_surf_elo_s    <- nongs$pre_surf_elo / 100
nongs$pre_surf_elo_sq_s <- nongs$pre_surf_elo_s^2

nongs_atp <- nongs[nongs$tour == "ATP", ]
nongs_wta <- nongs[nongs$tour == "WTA", ]
message("  NonGS ATP: ", nrow(nongs_atp), " | NonGS WTA: ", nrow(nongs_wta))


# ==============================================================================
# SECTION 2: HELPER FUNCTIONS
# ==============================================================================

#' Stack horizons from an unstacked (player-event level) data frame.
#' Carries forward scaled variables and v_hat if present.
stack_for_fisher <- function(data, outcomes = outcomes_base,
                             horizons = HORIZONS) {
  rows <- vector("list", length(horizons))
  for (k in seq_along(horizons)) {
    h <- horizons[k]
    h_lab <- paste0(h, "w")

    # Core ID and treatment variables
    d <- data[, c("player_id", "tourney_id", "slam_year", "got_ll",
                   "pre_rank_pts_s", "pre_rank_pts_sq_s",
                   "pre_elo_s", "pre_elo_sq_s",
                   "pre_surf_elo_s", "pre_surf_elo_sq_s",
                   "n_prior_gs_ll_won", "n_prior_gs_ll_notwon",
                   "n_prior_nongs_ll_won", "n_prior_nongs_ll_notwon",
                   "player_age"), drop = FALSE]

    # Add v_hat if present (nonGS)
    if ("v_hat" %in% names(data)) {
      d$v_hat <- data$v_hat
    }

    d$horizon <- factor(h_lab, levels = paste0(horizons, "w"))

    # Add outcome columns
    for (ob in outcomes) {
      col_name <- paste0(ob, "_", h, "w")
      if (col_name %in% names(data)) {
        d[[ob]] <- data[[col_name]]
      } else {
        d[[ob]] <- NA_real_
      }
    }
    rows[[k]] <- d
  }
  do.call(rbind, rows)
}


#' Recompute v_hat from permuted got_ll and original peer_component.
#' v_hat = D * phi(Phi^{-1}(P)) / P - (1-D) * phi(Phi^{-1}(P)) / (1-P)
#' Clips P at [eps, 1-eps] to avoid division by zero.
recompute_v_hat <- function(got_ll_perm, peer_component, eps = 1e-6) {
  P <- pmax(pmin(peer_component, 1 - eps), eps)
  q <- qnorm(P)
  phi_q <- dnorm(q)
  got_ll_perm * phi_q / P - (1 - got_ll_perm) * phi_q / (1 - P)
}


#' Run the Fisher permutation test for one tour-sample combination.
#'
#' @param unstacked   Player-event level data (one row per player-event)
#' @param formula_str Formula string (without outcome, which is prepended)
#' @param fe_str      Fixed effects string (e.g., "slam_year + horizon")
#' @param perm_group  Column name to permute within (e.g., "slam_year")
#' @param has_cf      Logical: does the model include v_hat (control function)?
#' @param peer_col    Column with P_i^{LL} for v_hat recomputation
#' @return List with actual_coefs, perm_matrix, fisher_p, asymp_p per outcome
run_fisher <- function(unstacked, formula_str, fe_str,
                       perm_group = "slam_year",
                       has_cf = FALSE, peer_col = "peer_component") {

  # Stack the actual data
  stacked <- stack_for_fisher(unstacked)
  n_stacked <- nrow(stacked)
  message("  Stacked rows: ", n_stacked)

  # Pre-compute groups for permutation (at unstacked level)
  groups <- split(seq_len(nrow(unstacked)), unstacked[[perm_group]])

  results <- list()

  for (ob in outcomes_base) {
    message("    Outcome: ", ob)

    # --- Actual model ---
    fml_str <- paste0(ob, " ~ ", formula_str, " | ", fe_str)
    fml <- as.formula(fml_str)

    # Filter complete cases for this outcome
    sdata <- stacked[!is.na(stacked[[ob]]), ]

    fit <- tryCatch(
      feols(fml, data = sdata, cluster = ~player_id),
      error = function(e) {
        message("      ERROR in actual model: ", e$message)
        NULL
      }
    )
    if (is.null(fit)) {
      results[[ob]] <- NULL
      next
    }

    # Extract got_ll:horizon coefficients
    cn <- names(coef(fit))
    ll_idx <- grep("^got_ll:horizon", cn)
    if (length(ll_idx) == 0) {
      # fixest may name them "horizonXw:got_ll"
      ll_idx <- grep("horizon.*:got_ll$", cn)
    }
    actual_coefs <- coef(fit)[ll_idx]
    actual_se    <- sqrt(diag(vcov(fit)))[ll_idx]
    actual_pval  <- 2 * pnorm(-abs(actual_coefs / actual_se))

    message("      Actual coefs: ",
            paste(sprintf("%.3f", actual_coefs), collapse = ", "))

    # --- Permutation loop ---
    perm_matrix <- matrix(NA_real_, nrow = N_PERMS, ncol = length(ll_idx))

    for (b in seq_len(N_PERMS)) {
      if (b %% 200 == 0) message("      Permutation ", b, "/", N_PERMS)

      # Permute got_ll within groups at the UNSTACKED level
      perm_unstacked <- unstacked
      for (g in groups) {
        perm_unstacked$got_ll[g] <- sample(unstacked$got_ll[g])
      }

      # Recompute v_hat if control function model
      if (has_cf && "v_hat" %in% names(perm_unstacked)) {
        perm_unstacked$v_hat <- recompute_v_hat(
          perm_unstacked$got_ll,
          perm_unstacked[[peer_col]]
        )
      }

      # Stack the permuted data
      perm_stacked <- stack_for_fisher(perm_unstacked)
      perm_sdata <- perm_stacked[!is.na(perm_stacked[[ob]]), ]

      perm_fit <- tryCatch(
        feols(fml, data = perm_sdata, cluster = ~player_id),
        error = function(e) NULL
      )
      if (!is.null(perm_fit)) {
        perm_cn <- names(coef(perm_fit))
        perm_ll_idx <- grep("^got_ll:horizon", perm_cn)
        if (length(perm_ll_idx) == 0) {
          perm_ll_idx <- grep("horizon.*:got_ll$", perm_cn)
        }
        if (length(perm_ll_idx) == length(ll_idx)) {
          perm_matrix[b, ] <- coef(perm_fit)[perm_ll_idx]
        }
      }
    }

    # --- Fisher p-values ---
    fisher_p <- numeric(length(ll_idx))
    for (h in seq_along(ll_idx)) {
      valid_perms <- !is.na(perm_matrix[, h])
      fisher_p[h] <- mean(abs(perm_matrix[valid_perms, h]) >=
                            abs(actual_coefs[h]))
    }

    results[[ob]] <- list(
      actual_coefs = actual_coefs,
      actual_se    = actual_se,
      asymp_p      = actual_pval,
      fisher_p     = fisher_p,
      perm_matrix  = perm_matrix,
      n_obs        = nobs(fit),
      coef_names   = cn[ll_idx]
    )

    message("      Fisher p: ",
            paste(sprintf("%.3f", fisher_p), collapse = ", "))
  }

  results
}


# ==============================================================================
# SECTION 3: GS FISHER TEST
# ==============================================================================
message("\n", strrep("-", 70))
message("GS FISHER TEST -- ATP")
message(strrep("-", 70))

gs_formula <- paste0("got_ll:horizon + ", ZPRE_GS)
gs_fe      <- "slam_year + horizon"

fisher_gs_atp <- run_fisher(
  unstacked   = gs_atp,
  formula_str = gs_formula,
  fe_str      = gs_fe,
  perm_group  = "slam_year",
  has_cf      = FALSE
)

message("\n", strrep("-", 70))
message("GS FISHER TEST -- WTA")
message(strrep("-", 70))

fisher_gs_wta <- run_fisher(
  unstacked   = gs_wta,
  formula_str = gs_formula,
  fe_str      = gs_fe,
  perm_group  = "slam_year",
  has_cf      = FALSE
)


# ==============================================================================
# SECTION 4: NONGS FISHER TEST
# ==============================================================================
message("\n", strrep("-", 70))
message("NONGS FISHER TEST -- ATP")
message(strrep("-", 70))

nongs_formula <- paste0("got_ll:horizon + v_hat:horizon + ", ZPRE_NONGS)
nongs_fe      <- "slam_year + horizon"

fisher_nongs_atp <- run_fisher(
  unstacked   = nongs_atp,
  formula_str = nongs_formula,
  fe_str      = nongs_fe,
  perm_group  = "slam_year",
  has_cf      = TRUE,
  peer_col    = "peer_component"
)

message("\n", strrep("-", 70))
message("NONGS FISHER TEST -- WTA")
message(strrep("-", 70))

fisher_nongs_wta <- run_fisher(
  unstacked   = nongs_wta,
  formula_str = nongs_formula,
  fe_str      = nongs_fe,
  perm_group  = "slam_year",
  has_cf      = TRUE,
  peer_col    = "peer_component"
)


# ==============================================================================
# SECTION 5: GENERATE LATEX TABLES
# ==============================================================================

#' Build a Fisher inference LaTeX table for one sample (GS or nonGS).
#'
#' @param res_atp  Fisher results for ATP
#' @param res_wta  Fisher results for WTA
#' @param sample_label  "GS" or "nonGS" for the table note
#' @param perm_label  Description of permutation unit
#' @return Character vector of LaTeX lines (bare tabular)
build_fisher_table <- function(res_atp, res_wta, sample_label, perm_label) {

  lines <- character()
  lines <- c(lines, "\\begin{tabular}{l ccccc}")
  lines <- c(lines, "\\toprule")
  lines <- c(lines, paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"))
  lines <- c(lines, "\\midrule")

  # Helper: one panel (ATP or WTA)
  add_panel <- function(res, panel_label) {
    panel_lines <- character()
    panel_lines <- c(panel_lines,
      paste0("\\multicolumn{6}{l}{\\textit{", panel_label, "}} \\\\"))
    panel_lines <- c(panel_lines, "\\midrule")

    n_obs_str <- ""

    for (ob in outcomes_base) {
      if (is.null(res[[ob]])) next

      r <- res[[ob]]
      coefs <- r$actual_coefs
      se    <- r$actual_se
      ap    <- r$asymp_p
      fp    <- r$fisher_p

      if (ob == outcomes_base[1]) {
        n_obs_str <- formatC(round(r$n_obs / length(HORIZONS)),
                             format = "d", big.mark = ",")
      }

      # Coefficient row with stars (based on Fisher p)
      coef_cells <- vapply(seq_along(coefs), function(h) {
        stars <- add_stars(fp[h])
        paste0(fmt(coefs[h], 3), stars)
      }, character(1))

      # SE row
      se_cells <- vapply(se, function(s) {
        paste0("(", fmt(s, 3), ")")
      }, character(1))

      # Asymptotic p row
      ap_cells <- vapply(ap, function(p) fmt(p, 3), character(1))

      # Fisher p row
      fp_cells <- vapply(fp, function(p) fmt(p, 3), character(1))

      panel_lines <- c(panel_lines,
        paste0(outcome_labels[ob], " & ",
               paste(coef_cells, collapse = " & "), " \\\\"))
      panel_lines <- c(panel_lines,
        paste0("  & ", paste(se_cells, collapse = " & "), " \\\\"))
      panel_lines <- c(panel_lines,
        paste0("  Asymptotic $p$ & ",
               paste(ap_cells, collapse = " & "), " \\\\"))
      panel_lines <- c(panel_lines,
        paste0("  Fisher $p$ & ",
               paste(fp_cells, collapse = " & "), " \\\\[0.3em]"))
    }

    list(lines = panel_lines, n_obs = n_obs_str)
  }

  # ATP panel
  atp_out <- add_panel(res_atp, "Panel A: ATP")
  lines <- c(lines, atp_out$lines)

  # WTA panel
  lines <- c(lines, "")
  wta_out <- add_panel(res_wta, "Panel B: WTA")
  lines <- c(lines, wta_out$lines)

  # Footer
  lines <- c(lines, "\\midrule")
  lines <- c(lines,
    paste0("\\multicolumn{6}{l}{Permutations: ",
           formatC(N_PERMS, format = "d", big.mark = ","),
           "; treatment permuted within ", perm_label, "} \\\\"))
  lines <- c(lines,
    paste0("$N$ (ATP) & \\multicolumn{5}{c}{",
           atp_out$n_obs, " $\\times$ 5 horizons} \\\\"))
  lines <- c(lines,
    paste0("$N$ (WTA) & \\multicolumn{5}{c}{",
           wta_out$n_obs, " $\\times$ 5 horizons} \\\\"))
  lines <- c(lines, "\\bottomrule")
  lines <- c(lines, "\\end{tabular}")

  lines
}

# --- GS table -----------------------------------------------------------------
gs_table <- build_fisher_table(
  fisher_gs_atp, fisher_gs_wta,
  sample_label = "GS",
  perm_label   = "slam $\\times$ year"
)
writeLines(gs_table, file.path(TABLE_DIR, "table_fisher_fixed.tex"))
message("\nWrote: Tables/table_fisher_fixed.tex")

# --- NonGS table --------------------------------------------------------------
nongs_table <- build_fisher_table(
  fisher_nongs_atp, fisher_nongs_wta,
  sample_label = "nonGS",
  perm_label   = "tournament $\\times$ year"
)
writeLines(nongs_table, file.path(TABLE_DIR, "table_fisher_nongs.tex"))
message("Wrote: Tables/table_fisher_nongs.tex")


# ==============================================================================
# SECTION 6: SAVE RESULTS OBJECT
# ==============================================================================
fisher_results <- list(
  gs_atp       = fisher_gs_atp,
  gs_wta       = fisher_gs_wta,
  nongs_atp    = fisher_nongs_atp,
  nongs_wta    = fisher_nongs_wta,
  n_perms      = N_PERMS,
  seed         = 20260327,
  horizons     = HORIZONS,
  outcomes     = outcomes_base,
  gs_formula   = gs_formula,
  nongs_formula = nongs_formula,
  timestamp    = Sys.time()
)

saveRDS(fisher_results, file.path(CLEANED_DIR, "fisher_fixed.rds"))
message("Wrote: Data/cleaned/fisher_fixed.rds")

message("\n", strrep("=", 70))
message("FISHER INFERENCE COMPLETE")
message(strrep("=", 70))
