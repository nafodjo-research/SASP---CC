# ==============================================================================
# 40b_dynamics_hetero.R
# Re-estimate ALL stacked dynamic, heterogeneity, dose, and first-LL models
# with the full Z^{pre} specification (including surface Elo).
#
# Inputs:
#   Data/cleaned/skeleton_gs_est_v3.rds
#   Data/cleaned/skeleton_nongs_est_v6.rds
#   Data/cleaned/performance_dose.rds
#   Data/cleaned/tournament_elo_cache.rds
#
# Outputs:
#   Data/cleaned/skeleton_gs_est_v4.rds
#   Data/cleaned/skeleton_nongs_est_v7.rds
#   Tables/table_dynamic_stacked_atp.tex
#   Tables/table_dynamic_stacked_wta.tex
#   Tables/table_dynamic_stacked_nongs_atp.tex
#   Tables/table_dynamic_stacked_nongs_wta.tex
#   Tables/table_dynamic_full_atp.tex
#   Tables/table_dynamic_full_wta.tex
#   Tables/table_dynamic_full_nongs_atp.tex
#   Tables/table_dynamic_full_nongs_wta.tex
#   Tables/table_hetero_stacked_atp.tex
#   Tables/table_hetero_stacked_wta.tex
#   Tables/table_hetero_stacked_nongs_atp.tex
#   Tables/table_hetero_stacked_nongs_wta.tex
#   Tables/table_dose_stacked.tex
#   Tables/table_dose_stacked_nongs.tex
#   Tables/table_perf_prob_stacked.tex
#   Tables/table_perf_prob_stacked_nongs.tex
#   Tables/table_firstll_stacked_atp.tex
#   Tables/table_firstll_stacked_wta.tex
#   Tables/table_firstll_stacked_nongs_atp.tex
#   Tables/table_firstll_stacked_nongs_wta.tex
#   Output/dynamics_hetero_summary.md
#
# Dependencies: dplyr, fixest, here
# ==============================================================================

set.seed(20260327)

# --- Packages -----------------------------------------------------------------
library(dplyr)
library(fixest)
library(here)

# --- Paths --------------------------------------------------------------------
CLEANED_DIR <- here("Data", "cleaned")
TABLES_DIR  <- here("Tables")
OUTPUT_DIR  <- here("Output")
for (d in c(CLEANED_DIR, TABLES_DIR, OUTPUT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# --- Helpers (inline to avoid sourcing issues) --------------------------------
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

# ==============================================================================
# STEP 1: ADD SURFACE ELO TO BOTH SKELETONS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 1: ADD SURFACE ELO TO SKELETONS")
message(strrep("=", 70))

gs  <- readRDS(file.path(CLEANED_DIR, "skeleton_gs_est_v3.rds"))
ngs <- readRDS(file.path(CLEANED_DIR, "skeleton_nongs_est_v6.rds"))
elo_cache <- readRDS(file.path(CLEANED_DIR, "tournament_elo_cache.rds"))

slog("GS skeleton: ", nrow(gs), " rows, ", ncol(gs), " cols")
slog("NonGS skeleton: ", nrow(ngs), " rows, ", ncol(ngs), " cols")

# --- Surface Elo lookup function ----------------------------------------------
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

add_surface_elo <- function(df) {
  df$pre_surf_elo <- NA_real_
  for (i in seq_len(nrow(df))) {
    tp <- df$tour[i]
    td <- df$tourney_date[i]
    if (is.numeric(td)) td <- as.Date(as.character(td), format = "%Y%m%d")
    df$pre_surf_elo[i] <- get_surface_elo_single(
      df$player_id[i], td, df$surface[i], tp)
  }
  # Fallback: use overall Elo if surface Elo unavailable
  # For GS, pre_elo is already /100 scale; for nonGS, pre_elo is raw
  df$pre_surf_elo[is.na(df$pre_surf_elo)] <- df$pre_elo[is.na(df$pre_surf_elo)]
  df
}

# Add surface Elo to GS
message("  Adding surface Elo to GS skeleton...")
gs <- add_surface_elo(gs)
# GS pre_elo is already on /100 scale (range ~15-21), so pre_surf_elo inherits
# that scale when falling back. For looked-up values, Elo cache returns raw.
# Need to check: are cache values raw or scaled?
# The cache stores raw Elo (like nonGS). But GS pre_elo is /100.
# So for GS: fallback pre_elo is /100 but cache values are raw.
# We must unify. Solution: if cache returns raw (>100), divide by 100 for GS.
# Actually, let's just rescale everything fresh for estimation (see Step 2).

gs$pre_surf_elo_sq <- NA_real_  # placeholder, will be filled after scaling

slog("  GS surface Elo NAs before fallback: ",
     sum(is.na(gs$pre_surf_elo)), " / ", nrow(gs))

# Add surface Elo to nonGS
message("  Adding surface Elo to NonGS skeleton...")
ngs <- add_surface_elo(ngs)
ngs$pre_surf_elo_sq <- NA_real_  # placeholder

slog("  NonGS surface Elo NAs before fallback: ",
     sum(is.na(ngs$pre_surf_elo)), " / ", nrow(ngs))

# Save augmented skeletons
saveRDS(gs, file.path(CLEANED_DIR, "skeleton_gs_est_v4.rds"))
saveRDS(ngs, file.path(CLEANED_DIR, "skeleton_nongs_est_v7.rds"))
slog("  Saved skeleton_gs_est_v4.rds and skeleton_nongs_est_v7.rds")

# ==============================================================================
# STEP 2: SCALE VARIABLES FOR ESTIMATION
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 2: SCALE VARIABLES FOR ESTIMATION")
message(strrep("=", 70))

# GS: pre_elo is /100 scale (~15-21). Surface Elo from cache is raw (~1500).
# To unify: convert everything to /100 scale for estimation.
# Detect if pre_surf_elo is raw (>100) and rescale.

scale_for_estimation <- function(df, is_gs = TRUE) {
  # Ranking points: /1000 and squared
  df$pre_rank_pts_s    <- df$pre_rank_pts / 1000
  df$pre_rank_pts_sq_s <- df$pre_rank_pts_s^2

  if (is_gs) {
    # GS pre_elo is already /100 scale
    df$pre_elo_s    <- df$pre_elo
    df$pre_elo_sq_s <- df$pre_elo_s^2
    # Surface Elo: might be raw from cache or /100 from fallback
    # If >100, it's raw; divide by 100
    df$pre_surf_elo_s <- ifelse(df$pre_surf_elo > 100,
                                df$pre_surf_elo / 100,
                                df$pre_surf_elo)
  } else {
    # NonGS pre_elo is raw
    df$pre_elo_s    <- df$pre_elo / 100
    df$pre_elo_sq_s <- df$pre_elo_s^2
    # Surface Elo: raw from cache, raw from fallback
    df$pre_surf_elo_s <- df$pre_surf_elo / 100
  }
  df$pre_surf_elo_sq_s <- df$pre_surf_elo_s^2

  df
}

gs  <- scale_for_estimation(gs, is_gs = TRUE)
ngs <- scale_for_estimation(ngs, is_gs = FALSE)

slog("  GS pre_elo_s range: [",
     fmt(min(gs$pre_elo_s, na.rm = TRUE), 2), ", ",
     fmt(max(gs$pre_elo_s, na.rm = TRUE), 2), "]")
slog("  GS pre_surf_elo_s range: [",
     fmt(min(gs$pre_surf_elo_s, na.rm = TRUE), 2), ", ",
     fmt(max(gs$pre_surf_elo_s, na.rm = TRUE), 2), "]")
slog("  NonGS pre_elo_s range: [",
     fmt(min(ngs$pre_elo_s, na.rm = TRUE), 2), ", ",
     fmt(max(ngs$pre_elo_s, na.rm = TRUE), 2), "]")
slog("  NonGS pre_surf_elo_s range: [",
     fmt(min(ngs$pre_surf_elo_s, na.rm = TRUE), 2), ", ",
     fmt(max(ngs$pre_surf_elo_s, na.rm = TRUE), 2), "]")

# ==============================================================================
# STEP 3: DEFINE Z^{pre} STRINGS AND STACK HORIZONS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 3: DEFINE Z^{pre} AND STACK HORIZONS")
message(strrep("=", 70))

# GS: all covariates interacted with horizon (absorbed by horizon FE)
ZPRE_GS <- paste0(
  "pre_rank_pts_s:horizon + pre_rank_pts_sq_s:horizon + ",
  "pre_elo_s:horizon + pre_elo_sq_s:horizon + ",
  "pre_surf_elo_s:horizon + pre_surf_elo_sq_s:horizon + ",
  "n_prior_gs_ll_won:horizon + n_prior_gs_ll_notwon:horizon + ",
  "n_prior_nongs_ll_won:horizon + n_prior_nongs_ll_notwon:horizon + ",
  "player_age:horizon"
)

# NonGS: main effects only (horizon FE absorb level differences)
ZPRE_NONGS <- paste0(
  "pre_rank_pts_s + pre_rank_pts_sq_s + pre_elo_s + pre_elo_sq_s + ",
  "pre_surf_elo_s + pre_surf_elo_sq_s + ",
  "n_prior_gs_ll_won + n_prior_gs_ll_notwon + ",
  "n_prior_nongs_ll_won + n_prior_nongs_ll_notwon + ",
  "player_age"
)

# Outcomes
OUTCOMES_BASE <- c("points_change", "elo_change",
                   "n_main_draws", "n_matches_250plus")
OUTCOME_LABELS <- c(
  "points_change"     = "Ranking Points $\\Delta$",
  "elo_change"        = "Elo $\\Delta$",
  "n_main_draws"      = "Main Draws",
  "n_matches_250plus" = "Matches (250+)"
)
HORIZONS <- c(4, 8, 12, 26, 52)

# --- Stack horizons function --------------------------------------------------
stack_horizons_full <- function(data, outcomes_base = OUTCOMES_BASE,
                                horizons = HORIZONS) {
  stacked <- list()
  # Identify columns to keep (everything except horizon-specific outcome cols)
  outcome_cols <- unlist(lapply(outcomes_base, function(ob) {
    paste0(ob, "_", horizons, "w")
  }))
  keep_cols <- setdiff(names(data), outcome_cols)

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

slog("  GS-ATP: ", nrow(gs_atp), " obs")
slog("  GS-WTA: ", nrow(gs_wta), " obs")
slog("  NonGS-ATP: ", nrow(ngs_atp), " obs")
slog("  NonGS-WTA: ", nrow(ngs_wta), " obs")

# Stack
gs_atp_s  <- stack_horizons_full(gs_atp)
gs_wta_s  <- stack_horizons_full(gs_wta)
ngs_atp_s <- stack_horizons_full(ngs_atp)
ngs_wta_s <- stack_horizons_full(ngs_wta)

slog("  Stacked GS-ATP: ", nrow(gs_atp_s), " rows")
slog("  Stacked GS-WTA: ", nrow(gs_wta_s), " rows")
slog("  Stacked NonGS-ATP: ", nrow(ngs_atp_s), " rows")
slog("  Stacked NonGS-WTA: ", nrow(ngs_wta_s), " rows")

# ==============================================================================
# STEP 4: RE-ESTIMATE STACKED DYNAMIC MODELS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 4: STACKED DYNAMIC MODELS")
message(strrep("=", 70))

# --- Estimation wrapper -------------------------------------------------------
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

# --- Extract treatment coefficients for compact table -------------------------
extract_treat_coefs <- function(fit, treat_var = "got_ll", horizons = HORIZONS) {
  cf <- coeftable(fit)
  h_labels <- paste0(horizons, "w")
  out <- data.frame(
    horizon = h_labels,
    coef    = NA_real_,
    se      = NA_real_,
    pval    = NA_real_,
    stringsAsFactors = FALSE
  )
  for (j in seq_along(h_labels)) {
    # Pattern: got_ll:horizonXw or horizonXw:got_ll
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

# --- Extract CF rho coefficients (nonGS only) ---------------------------------
extract_cf_coefs <- function(fit, cf_var = "v_hat", horizons = HORIZONS) {
  cf <- coeftable(fit)
  h_labels <- paste0(horizons, "w")
  out <- data.frame(
    horizon = h_labels,
    coef    = NA_real_,
    se      = NA_real_,
    pval    = NA_real_,
    stringsAsFactors = FALSE
  )
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

# --- Run GS models ------------------------------------------------------------
message("  Estimating GS-ATP dynamic models...")
dyn_gs_atp <- estimate_dynamic(
  gs_atp_s, OUTCOMES_BASE, ZPRE_GS, "slam_year + horizon",
  label = "GS-ATP")

message("  Estimating GS-WTA dynamic models...")
dyn_gs_wta <- estimate_dynamic(
  gs_wta_s, OUTCOMES_BASE, ZPRE_GS, "slam_year + horizon",
  label = "GS-WTA")

# --- Run NonGS models ---------------------------------------------------------
message("  Estimating NonGS-ATP dynamic models...")
dyn_ngs_atp <- estimate_dynamic(
  ngs_atp_s, OUTCOMES_BASE, ZPRE_NONGS, "tourney_id + horizon",
  cf_term = "v_hat", label = "NonGS-ATP")

message("  Estimating NonGS-WTA dynamic models...")
dyn_ngs_wta <- estimate_dynamic(
  ngs_wta_s, OUTCOMES_BASE, ZPRE_NONGS, "tourney_id + horizon",
  cf_term = "v_hat", label = "NonGS-WTA")

# ==============================================================================
# STEP 4a: GENERATE COMPACT DYNAMIC TABLES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 4a: COMPACT DYNAMIC TABLES")
message(strrep("=", 70))

generate_compact_dynamic_table <- function(fits, outcomes, outcome_labels,
                                           horizons = HORIZONS,
                                           include_cf = FALSE,
                                           cf_fits = NULL) {
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
  if (include_cf && !is.null(cf_fits)) {
    lines <- c(lines, "\\midrule")
    # Show rho for the first outcome that has it (points_change)
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

  # N and FE rows
  lines <- c(lines, "\\midrule")
  if (length(fits) > 0) {
    fit1 <- fits[[1]]
    n_obs <- nobs(fit1)
    n_players <- length(unique(model.matrix(fit1, type = "fixef")[, 1]))
    lines <- c(lines, paste0("$N$ & \\multicolumn{", ncols, "}{c}{",
                              format(n_obs, big.mark = ","), "} \\\\"))
  }
  lines <- c(lines, paste0("$Z^{\\text{pre}}$ controls & \\multicolumn{", ncols,
                            "}{c}{Yes} \\\\"))
  lines <- c(lines, "\\bottomrule")
  lines <- c(lines, "\\end{tabular}")

  paste(lines, collapse = "\n")
}

# GS compact tables
tex_dyn_atp <- generate_compact_dynamic_table(
  dyn_gs_atp, OUTCOMES_BASE, OUTCOME_LABELS)
writeLines(tex_dyn_atp, file.path(TABLES_DIR, "table_dynamic_stacked_atp.tex"))
slog("  Saved table_dynamic_stacked_atp.tex")

tex_dyn_wta <- generate_compact_dynamic_table(
  dyn_gs_wta, OUTCOMES_BASE, OUTCOME_LABELS)
writeLines(tex_dyn_wta, file.path(TABLES_DIR, "table_dynamic_stacked_wta.tex"))
slog("  Saved table_dynamic_stacked_wta.tex")

# NonGS compact tables (with CF rho)
tex_dyn_ngs_atp <- generate_compact_dynamic_table(
  dyn_ngs_atp, OUTCOMES_BASE, OUTCOME_LABELS,
  include_cf = TRUE)
writeLines(tex_dyn_ngs_atp, file.path(TABLES_DIR, "table_dynamic_stacked_nongs_atp.tex"))
slog("  Saved table_dynamic_stacked_nongs_atp.tex")

tex_dyn_ngs_wta <- generate_compact_dynamic_table(
  dyn_ngs_wta, OUTCOMES_BASE, OUTCOME_LABELS,
  include_cf = TRUE)
writeLines(tex_dyn_ngs_wta, file.path(TABLES_DIR, "table_dynamic_stacked_nongs_wta.tex"))
slog("  Saved table_dynamic_stacked_nongs_wta.tex")

# ==============================================================================
# STEP 4b: FULL REGRESSION TABLES (APPENDIX)
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 4b: FULL REGRESSION TABLES (APPENDIX)")
message(strrep("=", 70))

generate_full_dynamic_table <- function(fits, outcomes, outcome_labels,
                                        horizons = HORIZONS,
                                        is_gs = TRUE) {
  h_labels <- paste0(horizons, "w")
  n_outcomes <- length(intersect(outcomes, names(fits)))
  if (n_outcomes == 0) return("")

  lines <- character()
  lines <- c(lines, paste0("\\begin{tabular}{l",
                            paste(rep("c", n_outcomes), collapse = ""), "}"))
  lines <- c(lines, "\\toprule")

  # Column headers: outcome names
  obs_used <- intersect(outcomes, names(fits))
  header_labs <- sapply(obs_used, function(ob) {
    if (ob %in% names(outcome_labels)) outcome_labels[ob] else ob
  })
  lines <- c(lines, paste0(" & ", paste(header_labs, collapse = " & "), " \\\\"))
  lines <- c(lines, "\\midrule")

  # Treatment coefficients by horizon
  lines <- c(lines, paste0("\\multicolumn{", n_outcomes + 1,
                            "}{l}{\\textit{LL effect by horizon}} \\\\"))
  for (h in h_labels) {
    coef_str <- sapply(obs_used, function(ob) {
      tc <- extract_treat_coefs(fits[[ob]])
      j <- which(tc$horizon == h)
      if (length(j) == 0 || is.na(tc$coef[j])) return("")
      paste0(fmt(tc$coef[j], 2), add_stars(tc$pval[j]))
    })
    se_str <- sapply(obs_used, function(ob) {
      tc <- extract_treat_coefs(fits[[ob]])
      j <- which(tc$horizon == h)
      if (length(j) == 0 || is.na(tc$se[j])) return("")
      paste0("(", fmt(tc$se[j], 2), ")")
    })
    lines <- c(lines,
      paste0("\\quad LL $\\times$ ", h, " & ", paste(coef_str, collapse = " & "), " \\\\"),
      paste0(" & ", paste(se_str, collapse = " & "), " \\\\")
    )
  }
  lines <- c(lines, "\\\\[0.3em]")

  # CF rho by horizon (nonGS only)
  if (!is_gs) {
    lines <- c(lines, paste0("\\multicolumn{", n_outcomes + 1,
                              "}{l}{\\textit{Selection correction ($\\hat{\\rho}_h$)}} \\\\"))
    for (h in h_labels) {
      rho_str <- sapply(obs_used, function(ob) {
        rc <- extract_cf_coefs(fits[[ob]])
        j <- which(rc$horizon == h)
        if (length(j) == 0 || is.na(rc$coef[j])) return("")
        paste0(fmt(rc$coef[j], 2), add_stars(rc$pval[j]))
      })
      rho_se_str <- sapply(obs_used, function(ob) {
        rc <- extract_cf_coefs(fits[[ob]])
        j <- which(rc$horizon == h)
        if (length(j) == 0 || is.na(rc$se[j])) return("")
        paste0("(", fmt(rc$se[j], 2), ")")
      })
      lines <- c(lines,
        paste0("\\quad $\\hat{v}$ $\\times$ ", h, " & ",
               paste(rho_str, collapse = " & "), " \\\\"),
        paste0(" & ", paste(rho_se_str, collapse = " & "), " \\\\")
      )
    }
    lines <- c(lines, "\\\\[0.3em]")
  }

  # Footer
  lines <- c(lines, "\\midrule")
  if (length(fits) > 0) {
    fit1 <- fits[[obs_used[1]]]
    n_obs <- nobs(fit1)
    lines <- c(lines, paste0("$N$ & \\multicolumn{", n_outcomes, "}{c}{",
                              format(n_obs, big.mark = ","), "} \\\\"))
  }
  zpre_desc <- if (is_gs) "Yes (interacted with horizon)" else "Yes (main effects)"
  lines <- c(lines, paste0("$Z^{\\text{pre}}$ controls & \\multicolumn{",
                            n_outcomes, "}{c}{", zpre_desc, "} \\\\"))
  fe_desc <- if (is_gs) "Slam $\\times$ Year + Horizon" else "Tournament + Horizon"
  lines <- c(lines, paste0("Fixed effects & \\multicolumn{",
                            n_outcomes, "}{c}{", fe_desc, "} \\\\"))
  lines <- c(lines, paste0("Clustering & \\multicolumn{",
                            n_outcomes, "}{c}{Player} \\\\"))
  lines <- c(lines, "\\bottomrule")
  lines <- c(lines, "\\end{tabular}")

  paste(lines, collapse = "\n")
}

# GS full tables
tex_full_atp <- generate_full_dynamic_table(
  dyn_gs_atp, OUTCOMES_BASE, OUTCOME_LABELS, is_gs = TRUE)
writeLines(tex_full_atp, file.path(TABLES_DIR, "table_dynamic_full_atp.tex"))

tex_full_wta <- generate_full_dynamic_table(
  dyn_gs_wta, OUTCOMES_BASE, OUTCOME_LABELS, is_gs = TRUE)
writeLines(tex_full_wta, file.path(TABLES_DIR, "table_dynamic_full_wta.tex"))

# NonGS full tables
tex_full_ngs_atp <- generate_full_dynamic_table(
  dyn_ngs_atp, OUTCOMES_BASE, OUTCOME_LABELS, is_gs = FALSE)
writeLines(tex_full_ngs_atp, file.path(TABLES_DIR, "table_dynamic_full_nongs_atp.tex"))

tex_full_ngs_wta <- generate_full_dynamic_table(
  dyn_ngs_wta, OUTCOMES_BASE, OUTCOME_LABELS, is_gs = FALSE)
writeLines(tex_full_ngs_wta, file.path(TABLES_DIR, "table_dynamic_full_nongs_wta.tex"))

slog("  Saved 4 full dynamic tables (appendix)")

# ==============================================================================
# STEP 5: MERGE DOSE DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 5: MERGE DOSE DATA")
message(strrep("=", 70))

dose <- readRDS(file.path(CLEANED_DIR, "performance_dose.rds"))
slog("  Dose data: ", nrow(dose), " rows")

# Merge dose into unstacked skeletons, then re-stack
merge_dose <- function(df, dose_df) {
  dose_sub <- dose_df[, c("player_id", "tourney_id", "dose", "matches_won")]
  merged <- merge(df, dose_sub, by = c("player_id", "tourney_id"), all.x = TRUE)
  # Controls (got_ll == 0) get dose = 0, matches_won = 0
  merged$dose[is.na(merged$dose)] <- 0
  merged$matches_won[is.na(merged$matches_won)] <- 0
  merged
}

gs_atp_d  <- merge_dose(gs_atp, dose)
gs_wta_d  <- merge_dose(gs_wta, dose)
ngs_atp_d <- merge_dose(ngs_atp, dose)
ngs_wta_d <- merge_dose(ngs_wta, dose)

slog("  Dose merged -- GS-ATP: ", sum(gs_atp_d$dose > 0), " treated with dose > 0")
slog("  Dose merged -- NonGS-ATP: ", sum(ngs_atp_d$dose > 0), " treated with dose > 0")

# Stack dose-merged data
gs_atp_ds  <- stack_horizons_full(gs_atp_d)
gs_wta_ds  <- stack_horizons_full(gs_wta_d)
ngs_atp_ds <- stack_horizons_full(ngs_atp_d)
ngs_wta_ds <- stack_horizons_full(ngs_wta_d)

# ==============================================================================
# STEP 6: HETEROGENEITY MODELS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 6: HETEROGENEITY MODELS")
message(strrep("=", 70))

# Create median indicators
add_hetero_indicators <- function(df) {
  df$rank_above_med <- as.integer(
    df$pre_rank_pts >= median(df$pre_rank_pts, na.rm = TRUE))
  df$age_above_med <- as.integer(
    df$player_age >= median(df$player_age, na.rm = TRUE))
  df
}

gs_atp_s  <- add_hetero_indicators(gs_atp_s)
gs_wta_s  <- add_hetero_indicators(gs_wta_s)
ngs_atp_s <- add_hetero_indicators(ngs_atp_s)
ngs_wta_s <- add_hetero_indicators(ngs_wta_s)

# --- Estimation wrapper for heterogeneity ------------------------------------
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

# Extract interaction coefficients
extract_interact_coefs <- function(fit, interact_var, horizons = HORIZONS) {
  cf <- coeftable(fit)
  h_labels <- paste0(horizons, "w")
  out <- data.frame(
    horizon = h_labels,
    coef    = NA_real_,
    se      = NA_real_,
    pval    = NA_real_,
    stringsAsFactors = FALSE
  )
  for (j in seq_along(h_labels)) {
    # Match patterns like got_ll:horizonXw:var or any permutation
    rn <- rownames(cf)
    # Build all possible orderings of the three-way interaction
    parts <- c("got_ll", paste0("horizon", h_labels[j]), interact_var)
    # Try all 6 permutations
    found <- FALSE
    for (perm in list(
      c(1,2,3), c(1,3,2), c(2,1,3), c(2,3,1), c(3,1,2), c(3,2,1)
    )) {
      pat <- paste(parts[perm], collapse = ":")
      idx <- which(rn == pat)
      if (length(idx) == 1) {
        out$coef[j] <- cf[idx, "Estimate"]
        out$se[j]   <- cf[idx, "Std. Error"]
        out$pval[j] <- cf[idx, "Pr(>|t|)"]
        found <- TRUE
        break
      }
    }
  }
  out
}

# --- Generate heterogeneity table --------------------------------------------
generate_hetero_table <- function(fits_list, dim_labels, outcomes, outcome_labels,
                                  horizons = HORIZONS, include_cf = FALSE) {
  h_labels <- paste0(horizons, "w")
  ncols <- length(h_labels)
  lines <- character()
  lines <- c(lines, paste0("\\begin{tabular}{l", paste(rep("c", ncols), collapse = ""), "}"))
  lines <- c(lines, "\\toprule")
  lines <- c(lines, paste0(" & ", paste(h_labels, collapse = " & "), " \\\\"))
  lines <- c(lines, "\\midrule")

  for (dim_name in names(fits_list)) {
    fits <- fits_list[[dim_name]]
    dim_label <- dim_labels[dim_name]

    lines <- c(lines, paste0("\\multicolumn{", ncols + 1,
                              "}{l}{\\textit{Interaction: ", dim_label, "}} \\\\"))

    for (ob in outcomes) {
      if (!ob %in% names(fits)) next
      # Base treatment effect
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
        paste0("\\quad ", lab, " (base) & ", paste(coef_str, collapse = " & "), " \\\\"),
        paste0(" & ", paste(se_str, collapse = " & "), " \\\\")
      )

      # Interaction term
      ic <- extract_interact_coefs(fits[[ob]], dim_name)
      ic_str <- sapply(seq_along(h_labels), function(j) {
        if (is.na(ic$coef[j])) return("")
        paste0(fmt(ic$coef[j], 2), add_stars(ic$pval[j]))
      })
      ic_se_str <- sapply(seq_along(h_labels), function(j) {
        if (is.na(ic$se[j])) return("")
        paste0("(", fmt(ic$se[j], 2), ")")
      })
      lines <- c(lines,
        paste0("\\quad \\quad $\\times$ ", dim_label, " & ",
               paste(ic_str, collapse = " & "), " \\\\"),
        paste0(" & ", paste(ic_se_str, collapse = " & "), " \\\\[0.3em]")
      )
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

# --- Run heterogeneity for all 4 samples --------------------------------------
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

# Generate and save heterogeneity tables
tex_hetero_atp <- generate_hetero_table(
  hetero_gs_atp, HETERO_LABELS, OUTCOMES_BASE, OUTCOME_LABELS)
writeLines(tex_hetero_atp, file.path(TABLES_DIR, "table_hetero_stacked_atp.tex"))
slog("  Saved table_hetero_stacked_atp.tex")

tex_hetero_wta <- generate_hetero_table(
  hetero_gs_wta, HETERO_LABELS, OUTCOMES_BASE, OUTCOME_LABELS)
writeLines(tex_hetero_wta, file.path(TABLES_DIR, "table_hetero_stacked_wta.tex"))
slog("  Saved table_hetero_stacked_wta.tex")

tex_hetero_ngs_atp <- generate_hetero_table(
  hetero_ngs_atp, HETERO_LABELS, OUTCOMES_BASE, OUTCOME_LABELS,
  include_cf = TRUE)
writeLines(tex_hetero_ngs_atp, file.path(TABLES_DIR, "table_hetero_stacked_nongs_atp.tex"))
slog("  Saved table_hetero_stacked_nongs_atp.tex")

tex_hetero_ngs_wta <- generate_hetero_table(
  hetero_ngs_wta, HETERO_LABELS, OUTCOMES_BASE, OUTCOME_LABELS,
  include_cf = TRUE)
writeLines(tex_hetero_ngs_wta, file.path(TABLES_DIR, "table_hetero_stacked_nongs_wta.tex"))
slog("  Saved table_hetero_stacked_nongs_wta.tex")

# ==============================================================================
# STEP 7: TREATMENT DOSE TABLES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 7: TREATMENT DOSE TABLES")
message(strrep("=", 70))

# --- Estimation wrapper for dose models --------------------------------------
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

# --- Generate dose table ------------------------------------------------------
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

  # Panel B: Performance probability dose
  lines <- c(lines, "\\\\[0.5em]")
  lines <- c(lines, paste0("\\multicolumn{", ncols + 1,
                            "}{l}{\\textit{Panel B: Performance probability dose ($-\\log \\pi$)}} \\\\"))
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

# Add hetero indicators to dose-stacked data (for dose interactions)
gs_atp_ds  <- add_hetero_indicators(gs_atp_ds)
gs_wta_ds  <- add_hetero_indicators(gs_wta_ds)
ngs_atp_ds <- add_hetero_indicators(ngs_atp_ds)
ngs_wta_ds <- add_hetero_indicators(ngs_wta_ds)

# GS dose models (combined ATP + WTA for the dose table)
message("  Estimating GS dose models...")

# GS-ATP matches won dose
message("    GS-ATP matches won...")
dose_gs_atp_mw <- estimate_dose(
  gs_atp_ds, OUTCOMES_BASE, ZPRE_GS, "slam_year + horizon",
  "matches_won", label = "GS-ATP-MW")

# GS-ATP performance prob dose
message("    GS-ATP performance prob...")
dose_gs_atp_pp <- estimate_dose(
  gs_atp_ds, OUTCOMES_BASE, ZPRE_GS, "slam_year + horizon",
  "dose", label = "GS-ATP-PP")

# GS-WTA matches won dose
message("    GS-WTA matches won...")
dose_gs_wta_mw <- estimate_dose(
  gs_wta_ds, OUTCOMES_BASE, ZPRE_GS, "slam_year + horizon",
  "matches_won", label = "GS-WTA-MW")

# GS-WTA performance prob dose
message("    GS-WTA performance prob...")
dose_gs_wta_pp <- estimate_dose(
  gs_wta_ds, OUTCOMES_BASE, ZPRE_GS, "slam_year + horizon",
  "dose", label = "GS-WTA-PP")

# Generate combined GS dose table (ATP in one file, combined in another)
# Save separate ATP dose table
tex_dose_gs <- generate_dose_table(
  dose_gs_atp_mw, dose_gs_atp_pp, OUTCOMES_BASE, OUTCOME_LABELS)
writeLines(tex_dose_gs, file.path(TABLES_DIR, "table_dose_stacked.tex"))
slog("  Saved table_dose_stacked.tex (GS-ATP)")

tex_perf_gs <- generate_dose_table(
  dose_gs_wta_mw, dose_gs_wta_pp, OUTCOMES_BASE, OUTCOME_LABELS)
writeLines(tex_perf_gs, file.path(TABLES_DIR, "table_perf_prob_stacked.tex"))
slog("  Saved table_perf_prob_stacked.tex (GS-WTA)")

# NonGS dose models
message("  Estimating NonGS dose models...")

message("    NonGS-ATP matches won...")
dose_ngs_atp_mw <- estimate_dose(
  ngs_atp_ds, OUTCOMES_BASE, ZPRE_NONGS, "tourney_id + horizon",
  "matches_won", cf_term = "v_hat", label = "NonGS-ATP-MW")

message("    NonGS-ATP performance prob...")
dose_ngs_atp_pp <- estimate_dose(
  ngs_atp_ds, OUTCOMES_BASE, ZPRE_NONGS, "tourney_id + horizon",
  "dose", cf_term = "v_hat", label = "NonGS-ATP-PP")

message("    NonGS-WTA matches won...")
dose_ngs_wta_mw <- estimate_dose(
  ngs_wta_ds, OUTCOMES_BASE, ZPRE_NONGS, "tourney_id + horizon",
  "matches_won", cf_term = "v_hat", label = "NonGS-WTA-MW")

message("    NonGS-WTA performance prob...")
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
# STEP 8: FIRST-LL ROBUSTNESS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 8: FIRST-LL ROBUSTNESS")
message(strrep("=", 70))

# Filter to first-time LL candidates: no prior LL opportunities
# GS: n_prior_gs_ll_won == 0 & n_prior_gs_ll_notwon == 0
# Also no prior nonGS LL
filter_first_ll <- function(df) {
  df[df$n_prior_gs_ll_won == 0 &
     df$n_prior_gs_ll_notwon == 0 &
     df$n_prior_nongs_ll_won == 0 &
     df$n_prior_nongs_ll_notwon == 0, ]
}

gs_atp_first  <- filter_first_ll(gs_atp)
gs_wta_first  <- filter_first_ll(gs_wta)
ngs_atp_first <- filter_first_ll(ngs_atp)
ngs_wta_first <- filter_first_ll(ngs_wta)

slog("  First-LL GS-ATP: ", nrow(gs_atp_first), " / ", nrow(gs_atp))
slog("  First-LL GS-WTA: ", nrow(gs_wta_first), " / ", nrow(gs_wta))
slog("  First-LL NonGS-ATP: ", nrow(ngs_atp_first), " / ", nrow(ngs_atp))
slog("  First-LL NonGS-WTA: ", nrow(ngs_wta_first), " / ", nrow(ngs_wta))

# Stack
gs_atp_first_s  <- stack_horizons_full(gs_atp_first)
gs_wta_first_s  <- stack_horizons_full(gs_wta_first)
ngs_atp_first_s <- stack_horizons_full(ngs_atp_first)
ngs_wta_first_s <- stack_horizons_full(ngs_wta_first)

# For first-LL models, exclude LL history variables from Z^{pre}
# since they are all zero by construction
ZPRE_GS_FIRSTLL <- paste0(
  "pre_rank_pts_s:horizon + pre_rank_pts_sq_s:horizon + ",
  "pre_elo_s:horizon + pre_elo_sq_s:horizon + ",
  "pre_surf_elo_s:horizon + pre_surf_elo_sq_s:horizon + ",
  "player_age:horizon"
)

ZPRE_NONGS_FIRSTLL <- paste0(
  "pre_rank_pts_s + pre_rank_pts_sq_s + pre_elo_s + pre_elo_sq_s + ",
  "pre_surf_elo_s + pre_surf_elo_sq_s + ",
  "player_age"
)

# Estimate first-LL dynamic models
message("  Estimating first-LL GS-ATP...")
firstll_gs_atp <- estimate_dynamic(
  gs_atp_first_s, OUTCOMES_BASE, ZPRE_GS_FIRSTLL, "slam_year + horizon",
  label = "FirstLL-GS-ATP")

message("  Estimating first-LL GS-WTA...")
firstll_gs_wta <- estimate_dynamic(
  gs_wta_first_s, OUTCOMES_BASE, ZPRE_GS_FIRSTLL, "slam_year + horizon",
  label = "FirstLL-GS-WTA")

message("  Estimating first-LL NonGS-ATP...")
firstll_ngs_atp <- estimate_dynamic(
  ngs_atp_first_s, OUTCOMES_BASE, ZPRE_NONGS_FIRSTLL, "tourney_id + horizon",
  cf_term = "v_hat", label = "FirstLL-NonGS-ATP")

message("  Estimating first-LL NonGS-WTA...")
firstll_ngs_wta <- estimate_dynamic(
  ngs_wta_first_s, OUTCOMES_BASE, ZPRE_NONGS_FIRSTLL, "tourney_id + horizon",
  cf_term = "v_hat", label = "FirstLL-NonGS-WTA")

# Generate first-LL tables
tex_firstll_atp <- generate_compact_dynamic_table(
  firstll_gs_atp, OUTCOMES_BASE, OUTCOME_LABELS)
writeLines(tex_firstll_atp, file.path(TABLES_DIR, "table_firstll_stacked_atp.tex"))
slog("  Saved table_firstll_stacked_atp.tex")

tex_firstll_wta <- generate_compact_dynamic_table(
  firstll_gs_wta, OUTCOMES_BASE, OUTCOME_LABELS)
writeLines(tex_firstll_wta, file.path(TABLES_DIR, "table_firstll_stacked_wta.tex"))
slog("  Saved table_firstll_stacked_wta.tex")

tex_firstll_ngs_atp <- generate_compact_dynamic_table(
  firstll_ngs_atp, OUTCOMES_BASE, OUTCOME_LABELS,
  include_cf = TRUE)
writeLines(tex_firstll_ngs_atp, file.path(TABLES_DIR, "table_firstll_stacked_nongs_atp.tex"))
slog("  Saved table_firstll_stacked_nongs_atp.tex")

tex_firstll_ngs_wta <- generate_compact_dynamic_table(
  firstll_ngs_wta, OUTCOMES_BASE, OUTCOME_LABELS,
  include_cf = TRUE)
writeLines(tex_firstll_ngs_wta, file.path(TABLES_DIR, "table_firstll_stacked_nongs_wta.tex"))
slog("  Saved table_firstll_stacked_nongs_wta.tex")

# ==============================================================================
# STEP 9: PRINT KEY RESULTS TO CONSOLE
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 9: KEY RESULTS SUMMARY")
message(strrep("=", 70))

print_key_coefs <- function(fits, label) {
  message("\n  --- ", label, " ---")
  for (ob in names(fits)) {
    tc <- extract_treat_coefs(fits[[ob]])
    vals <- sapply(seq_len(nrow(tc)), function(j) {
      if (is.na(tc$coef[j])) return("NA")
      paste0(fmt(tc$coef[j], 2), add_stars(tc$pval[j]))
    })
    message("    ", ob, ": ", paste(vals, collapse = "  "))
  }
}

print_key_coefs(dyn_gs_atp, "GS-ATP Dynamic")
print_key_coefs(dyn_gs_wta, "GS-WTA Dynamic")
print_key_coefs(dyn_ngs_atp, "NonGS-ATP Dynamic")
print_key_coefs(dyn_ngs_wta, "NonGS-WTA Dynamic")

# ==============================================================================
# STEP 10: SAVE SUMMARY LOG
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 10: SAVE SUMMARY LOG")
message(strrep("=", 70))

summary_text <- c(
  "# Dynamics & Heterogeneity Re-estimation Summary",
  paste0("Generated: ", Sys.time()),
  "",
  "## Models estimated",
  "",
  "### Stacked Dynamic (4 samples x 4 outcomes = 16 models)",
  "- GS-ATP, GS-WTA, NonGS-ATP, NonGS-WTA",
  "- Outcomes: points_change, elo_change, n_main_draws, n_matches_250plus",
  "- Horizons: 4w, 8w, 12w, 26w, 52w",
  "",
  "### Heterogeneity (4 samples x 4 outcomes x 3 dimensions = 48 models)",
  "- Dimensions: rank_above_med, age_above_med, had_prior_ll",
  "",
  "### Dose (4 samples x 4 outcomes x 2 dose types = 32 models)",
  "- Dose types: matches_won, performance probability (-log pi)",
  "",
  "### First-LL Robustness (4 samples x 4 outcomes = 16 models)",
  "",
  "## Z^{pre} specification",
  "- GS: pre_rank_pts_s, pre_rank_pts_sq_s, pre_elo_s, pre_elo_sq_s,",
  "  pre_surf_elo_s, pre_surf_elo_sq_s, n_prior_gs_ll_won, n_prior_gs_ll_notwon,",
  "  n_prior_nongs_ll_won, n_prior_nongs_ll_notwon, player_age",
  "  ALL interacted with horizon",
  "- NonGS: Same variables as main effects (not interacted)",
  "",
  "## Scaling",
  "- pre_rank_pts: /1000",
  "- pre_elo: /100 (GS already on this scale; NonGS divided)",
  "- pre_surf_elo: /100",
  "",
  "## Key log messages",
  summary_log
)

writeLines(summary_text, file.path(OUTPUT_DIR, "dynamics_hetero_summary.md"))
message("  Saved dynamics_hetero_summary.md")

message("\n", strrep("=", 70))
message("DONE: 40b_dynamics_hetero.R complete")
message(strrep("=", 70))
