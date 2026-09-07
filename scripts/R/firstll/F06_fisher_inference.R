# ==============================================================================
# F06_fisher_inference.R
# Fisher randomization inference for the first-LL restricted sample.
#
# Adapts 40d_fisher_inference.R:
#   - ZPRE drops prior-LL variables (all zero by construction)
#   - Reads from firstll/ cleaned directory
#   - Outputs to Tables_FirstLL/
#
# For GS: 1000 permutations of got_ll within slam_year blocks.
# For nonGS: 1000 permutations of got_ll within tourney_id blocks,
#            with v_hat recomputed each permutation.
#
# Focus outcomes: points_change, elo_change, n_main_draws
#
# Inputs:
#   Data/cleaned/firstll/firstll_gs_est.rds
#   Data/cleaned/firstll/firstll_nongs_est.rds
#
# Outputs:
#   Tables_FirstLL/table_fisher_fixed.tex    (GS)
#   Tables_FirstLL/table_fisher_nongs.tex    (nonGS)
#   Data/cleaned/firstll/fisher_results.rds
#
# Dependencies: fixest, here, dplyr
# ==============================================================================

set.seed(20260416)

library(fixest)
library(here)
library(dplyr)

source(here("scripts", "R", "firstll", "firstll_helpers.R"))
summary_log <- character()

# --- Constants ----------------------------------------------------------------
# 5,000 permutations. At 1,000 the Monte Carlo SE of a p-value near 0.03 is
# about 0.005, which is uncomfortably wide when the reported p-values sit close
# to conventional thresholds; 5,000 cuts that to roughly 0.002.
N_PERMS <- 5000

# Focus outcomes for Fisher inference
fisher_outcomes <- c("points_change", "elo_change", "n_main_draws")

fisher_outcome_labels <- c(
  points_change = "Ranking pts $\\Delta$",
  elo_change    = "Elo $\\Delta$",
  n_main_draws  = "Main draws entered"
)

# --- First-LL ZPRE ------------------------------------------------------------
# The controls enter with horizon-INVARIANT coefficients, matching the headline
# dynamic specification in F03 and the model described in the paper's empirical
# strategy section. An earlier version of this script interacted every control
# with horizon, which fits a more flexible model than the one whose coefficients
# the paper reports; the Fisher p-values then referred to a different
# specification than the point estimates they were printed beside, producing
# coefficient discrepancies (74.95 versus 72.53 at 4 weeks, and similar
# elsewhere). Matching the headline specification removes that inconsistency.
ZPRE_GS_FISHER <- ZPRE_FIRSTLL

ZPRE_NONGS_FISHER <- paste0(
  "pre_rank_pts_s + pre_rank_pts_sq_s + ",
  "pre_elo_s + pre_elo_sq_s + ",
  "pre_surf_elo_s + pre_surf_elo_sq_s + ",
  "player_age"
)


# ==============================================================================
# SECTION 1: LOAD AND PREPARE DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("FISHER RANDOMIZATION INFERENCE (First-LL)")
message(strrep("=", 70))

# --- GS data ------------------------------------------------------------------
gs <- readRDS(file.path(FIRSTLL_CLEANED, "firstll_gs_est.rds"))
gs <- ensure_scaled(gs)

gs_atp <- gs[gs$tour == "ATP", ]
gs_wta <- gs[gs$tour == "WTA", ]
message("  GS ATP: ", nrow(gs_atp), " | GS WTA: ", nrow(gs_wta))

# --- NonGS data ---------------------------------------------------------------
ngs <- readRDS(file.path(FIRSTLL_CLEANED, "firstll_nongs_est.rds"))
ngs <- ensure_scaled(ngs)

ngs_atp <- ngs[ngs$tour == "ATP", ]
ngs_wta <- ngs[ngs$tour == "WTA", ]
message("  NonGS ATP: ", nrow(ngs_atp), " | NonGS WTA: ", nrow(ngs_wta))


# ==============================================================================
# SECTION 2: HELPER FUNCTIONS
# ==============================================================================

#' Stack horizons for Fisher permutation from unstacked data.
stack_for_fisher <- function(data, outcomes = fisher_outcomes,
                             horizons = HORIZONS) {
  # Identify columns to carry forward
  id_cols <- c("player_id", "tourney_id", "slam_year", "got_ll",
               "pre_rank_pts_s", "pre_rank_pts_sq_s",
               "pre_elo_s", "pre_elo_sq_s",
               "pre_surf_elo_s", "pre_surf_elo_sq_s",
               "player_age")
  id_cols <- id_cols[id_cols %in% names(data)]

  rows <- vector("list", length(horizons))
  for (k in seq_along(horizons)) {
    h <- horizons[k]
    h_lab <- paste0(h, "w")

    d <- data[, id_cols, drop = FALSE]

    # Carry v_hat if present (nonGS)
    if ("v_hat" %in% names(data)) d$v_hat <- data$v_hat

    d$horizon <- factor(h_lab, levels = paste0(horizons, "w"))

    for (ob in outcomes) {
      cn <- paste0(ob, "_", h, "w")
      if (cn %in% names(data)) d[[ob]] <- data[[cn]]
      else d[[ob]] <- NA_real_
    }
    rows[[k]] <- d
  }
  do.call(rbind, rows)
}


#' Recompute v_hat from permuted got_ll and original peer_component.
recompute_v_hat <- function(got_ll_perm, peer_component, eps = 1e-6) {
  P <- pmax(pmin(peer_component, 1 - eps), eps)
  q <- qnorm(P)
  phi_q <- dnorm(q)
  got_ll_perm * phi_q / P - (1 - got_ll_perm) * phi_q / (1 - P)
}


#' Run Fisher permutation test for one tour-sample combination.
run_fisher <- function(unstacked, formula_str, fe_str,
                       perm_group = "slam_year",
                       has_cf = FALSE, peer_col = "peer_component") {

  stacked <- stack_for_fisher(unstacked)
  n_stacked <- nrow(stacked)
  message("  Stacked rows: ", n_stacked)

  # Pre-compute groups for permutation (at unstacked level)
  groups <- split(seq_len(nrow(unstacked)), unstacked[[perm_group]])

  results <- list()

  for (ob in fisher_outcomes) {
    message("    Outcome: ", ob)

    fml_str <- paste0(ob, " ~ ", formula_str, " | ", fe_str)
    fml <- as.formula(fml_str)

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
    if (length(ll_idx) == 0) ll_idx <- grep("horizon.*:got_ll$", cn)

    actual_coefs <- coef(fit)[ll_idx]
    actual_se    <- sqrt(diag(vcov(fit)))[ll_idx]
    actual_tstat <- actual_coefs / actual_se

    message("      Actual coefs: ",
            paste(sprintf("%.3f", actual_coefs), collapse = ", "))

    # --- Permutation loop ---
    perm_tstat <- matrix(NA_real_, nrow = N_PERMS, ncol = length(ll_idx))

    for (b in seq_len(N_PERMS)) {
      if (b %% 200 == 0) message("      Permutation ", b, "/", N_PERMS)

      perm_unstacked <- unstacked
      for (g in groups) {
        perm_unstacked$got_ll[g] <- sample(unstacked$got_ll[g])
      }

      if (has_cf && "peer_component" %in% names(perm_unstacked)) {
        perm_unstacked$v_hat <- recompute_v_hat(
          perm_unstacked$got_ll,
          perm_unstacked[[peer_col]]
        )
      }

      perm_stacked <- stack_for_fisher(perm_unstacked)
      perm_sdata <- perm_stacked[!is.na(perm_stacked[[ob]]), ]

      perm_fit <- tryCatch(
        feols(fml, data = perm_sdata, cluster = ~player_id),
        error = function(e) NULL
      )
      if (!is.null(perm_fit)) {
        perm_cn <- names(coef(perm_fit))
        perm_ll_idx <- grep("^got_ll:horizon", perm_cn)
        if (length(perm_ll_idx) == 0) perm_ll_idx <- grep("horizon.*:got_ll$", perm_cn)
        if (length(perm_ll_idx) == length(ll_idx)) {
          perm_coefs <- coef(perm_fit)[perm_ll_idx]
          perm_se    <- sqrt(diag(vcov(perm_fit)))[perm_ll_idx]
          perm_tstat[b, ] <- perm_coefs / perm_se
        }
      }

      # Each iteration allocates a fresh copy of the unstacked frame, a stacked
      # frame, and a fixest object whose environment retains the model frame.
      # Without explicit release these accumulate and the process segfaults
      # partway through a long permutation run.
      rm(perm_unstacked, perm_stacked, perm_sdata, perm_fit)
      if (b %% 250 == 0) gc(verbose = FALSE)
    }
    gc(verbose = FALSE)

    # --- Fisher p-values (two-sided, based on |t|) ---
    fisher_p <- numeric(length(ll_idx))
    for (h in seq_along(ll_idx)) {
      valid <- !is.na(perm_tstat[, h])
      fisher_p[h] <- mean(abs(perm_tstat[valid, h]) >= abs(actual_tstat[h]))
    }

    results[[ob]] <- list(
      actual_coefs = actual_coefs,
      actual_se    = actual_se,
      fisher_p     = fisher_p,
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

gs_formula <- paste0("got_ll:horizon + ", ZPRE_GS_FISHER)
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

nongs_formula <- paste0("got_ll:horizon + v_hat:horizon + ", ZPRE_NONGS_FISHER)
nongs_fe      <- "tourney_id + horizon"

fisher_ngs_atp <- run_fisher(
  unstacked   = ngs_atp,
  formula_str = nongs_formula,
  fe_str      = nongs_fe,
  perm_group  = "tourney_id",
  has_cf      = TRUE,
  peer_col    = "peer_component"
)

message("\n", strrep("-", 70))
message("NONGS FISHER TEST -- WTA")
message(strrep("-", 70))

fisher_ngs_wta <- run_fisher(
  unstacked   = ngs_wta,
  formula_str = nongs_formula,
  fe_str      = nongs_fe,
  perm_group  = "tourney_id",
  has_cf      = TRUE,
  peer_col    = "peer_component"
)


# ==============================================================================
# SECTION 5: GENERATE LATEX TABLES
# ==============================================================================

build_fisher_table <- function(res_atp, res_wta, perm_label) {
  lines <- c(
    "\\begin{tabular}{l ccccc}",
    "\\toprule",
    paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"),
    "\\midrule"
  )

  add_panel <- function(res, panel_label) {
    panel_lines <- c(
      paste0("\\multicolumn{6}{l}{\\textit{", panel_label, "}} \\\\"),
      "\\midrule"
    )
    n_obs_str <- ""

    for (ob in fisher_outcomes) {
      if (is.null(res[[ob]])) next
      r <- res[[ob]]
      coefs <- r$actual_coefs
      se    <- r$actual_se
      fp    <- r$fisher_p

      if (ob == fisher_outcomes[1]) {
        n_obs_str <- formatC(round(r$n_obs / length(HORIZONS)),
                             format = "d", big.mark = ",")
      }

      # Coefficient row with stars based on Fisher p
      coef_cells <- vapply(seq_along(coefs), function(h) {
        paste0(fmt(coefs[h], 3), add_stars(fp[h]))
      }, character(1))

      # SE row
      se_cells <- vapply(se, function(s) paste0("(", fmt(s, 3), ")"), character(1))

      # Fisher p row
      fp_cells <- vapply(fp, function(p) fmt(p, 3), character(1))

      panel_lines <- c(panel_lines,
        paste0(fisher_outcome_labels[ob], " & ",
               paste(coef_cells, collapse = " & "), " \\\\"),
        paste0("  & ", paste(se_cells, collapse = " & "), " \\\\"),
        paste0("  Fisher $p$ & ",
               paste(fp_cells, collapse = " & "), " \\\\[0.3em]")
      )
    }
    list(lines = panel_lines, n_obs = n_obs_str)
  }

  atp_out <- add_panel(res_atp, "Panel A: ATP")
  lines <- c(lines, atp_out$lines, "")

  wta_out <- add_panel(res_wta, "Panel B: WTA")
  lines <- c(lines, wta_out$lines)

  lines <- c(lines,
    "\\midrule",
    paste0("\\multicolumn{6}{l}{Permutations: ",
           formatC(N_PERMS, format = "d", big.mark = ","),
           "; treatment permuted within ", perm_label, "} \\\\"),
    paste0("$N$ (ATP) & \\multicolumn{5}{c}{",
           atp_out$n_obs, " $\\times$ 5 horizons} \\\\"),
    paste0("$N$ (WTA) & \\multicolumn{5}{c}{",
           wta_out$n_obs, " $\\times$ 5 horizons} \\\\"),
    "\\bottomrule",
    "\\end{tabular}"
  )
  lines
}

# --- GS table -----------------------------------------------------------------
gs_table <- build_fisher_table(
  fisher_gs_atp, fisher_gs_wta,
  perm_label = "slam $\\times$ year"
)
writeLines(gs_table, file.path(FIRSTLL_TABLES, "table_fisher_fixed.tex"))
message("\nWrote: table_fisher_fixed.tex")

# --- NonGS table --------------------------------------------------------------
ngs_table <- build_fisher_table(
  fisher_ngs_atp, fisher_ngs_wta,
  perm_label = "tournament ID"
)
writeLines(ngs_table, file.path(FIRSTLL_TABLES, "table_fisher_nongs.tex"))
message("Wrote: table_fisher_nongs.tex")


# ==============================================================================
# SECTION 6: SAVE RESULTS OBJECT
# ==============================================================================
fisher_results <- list(
  gs_atp    = fisher_gs_atp,
  gs_wta    = fisher_gs_wta,
  ngs_atp   = fisher_ngs_atp,
  ngs_wta   = fisher_ngs_wta,
  n_perms   = N_PERMS,
  seed      = 20260416,
  horizons  = HORIZONS,
  outcomes  = fisher_outcomes,
  timestamp = Sys.time()
)

saveRDS(fisher_results, file.path(FIRSTLL_CLEANED, "fisher_results.rds"))
message("Wrote: fisher_results.rds")

message("\n", strrep("=", 70))
message("FISHER INFERENCE (First-LL) COMPLETE")
message(strrep("=", 70))
