# ==============================================================================
# F05_tournament_dose_horizon.R
# Delta model with horizon x dose interactions for first-LL sample.
#
# Extends F04 by adding cumulative horizon indicators interacted with dose
# measures, following the structure of 42_final_fixes.R FIX 3-4.
#
# Specifications:
#   (1) Horizon x matches_won: got_ll:horizon + got_ll:horizon:matches_won_c
#   (2) Horizon x perf dose:   got_ll:horizon + got_ll:horizon:dose
#   (3) Total effects at selected dose values (from cumulative indicators)
#
# Tracking window: h* = min(52 weeks, next same-type LL opportunity)
# Inference: robust SEs from glm (no bootstrap)
#
# Inputs:
#   Data/cleaned/firstll/firstll_delta_model_results.rds (from F04)
#   -- OR if F04 has not run, attempts to build from raw data
#
# Outputs:
#   Tables_FirstLL/table_delta_dose_horizon_mw_{gs_atp,...}.tex  (4 tables)
#   Tables_FirstLL/table_delta_dose_horizon_pp_{gs_atp,...}.tex  (4 tables)
#   Figures_FirstLL/fig_delta_horizon_dose_gs_atp.pdf   (etc., 4 figures)
#   Data/cleaned/firstll/firstll_delta_dose_horizon_results.rds
#
# Dependencies: dplyr, ggplot2, here
# ==============================================================================

set.seed(20260416)

library(dplyr)
library(ggplot2)
library(here)

source(here("scripts", "R", "firstll", "firstll_helpers.R"))
summary_log <- character()


# ==============================================================================
# STEP 1: LOAD DELTA MODEL DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 1: LOAD DELTA MODEL RESULTS FROM F04")
message(strrep("=", 70))

# Load F04 results -- we need the model objects which contain data
f04_results <- tryCatch(
  readRDS(file.path(FIRSTLL_CLEANED, "firstll_delta_model_results.rds")),
  error = function(e) {
    message("  ERROR: firstll_delta_model_results.rds not found: ", e$message)
    NULL
  }
)

if (is.null(f04_results) || identical(f04_results$status, "placeholder")) {
  message("  F04 delta model results not available (placeholder or missing).")
  message("  Run F04_delta_model.R first to generate match-level data.")
  message("  Creating placeholder outputs.")

  placeholder_tex <- paste(c(
    "\\begin{tabular}{l*{5}{c}}", "\\toprule",
    paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"), "\\midrule",
    "\\multicolumn{6}{c}{\\textit{Placeholder: run F04 first}} \\\\",
    "\\bottomrule", "\\end{tabular}"
  ), collapse = "\n")

  for (tag in c("gs_atp", "gs_wta", "nongs_atp", "nongs_wta")) {
    writeLines(placeholder_tex,
               file.path(FIRSTLL_TABLES, paste0("table_delta_dose_horizon_mw_", tag, ".tex")))
    writeLines(placeholder_tex,
               file.path(FIRSTLL_TABLES, paste0("table_delta_dose_horizon_pp_", tag, ".tex")))
  }

  saveRDS(list(status = "placeholder", reason = "F04 not yet run"),
          file.path(FIRSTLL_CLEANED, "firstll_delta_dose_horizon_results.rds"))
  message("  8 placeholder tables written to Tables_FirstLL/")
  message("DONE (placeholder mode)")
  quit(save = "no", status = 0)
}

# F04 strips data from results before saving. We need the match-level data.
# Check if data is available in the results; if not, we need a different approach.
# The F04 models are saved without $data. We need to re-extract match data
# from the model objects' model frames.
message("  Checking F04 result structure...")

# Extract model data from the model objects (model.frame preserves the data)
extract_match_data <- function(res) {
  if (is.null(res)) return(NULL)
  # If data was stripped, try to get it from the model
  if (!is.null(res$data)) return(res$data)
  # Fall back to model frame from the horizon model (most complete)
  if (!is.null(res$horizon)) {
    md <- tryCatch(model.frame(res$horizon), error = function(e) NULL)
    if (!is.null(md)) return(md)
  }
  if (!is.null(res$pooled)) {
    md <- tryCatch(model.frame(res$pooled), error = function(e) NULL)
    if (!is.null(md)) return(md)
  }
  NULL
}

md_list <- list(
  gs_atp    = extract_match_data(f04_results$gs_atp),
  gs_wta    = extract_match_data(f04_results$gs_wta),
  nongs_atp = extract_match_data(f04_results$nongs_atp),
  nongs_wta = extract_match_data(f04_results$nongs_wta)
)

for (nm in names(md_list)) {
  if (!is.null(md_list[[nm]])) {
    message("  ", nm, ": ", nrow(md_list[[nm]]), " match rows recovered")
  } else {
    message("  ", nm, ": no data available")
  }
}


# ==============================================================================
# STEP 2: ESTIMATE HORIZON x DOSE MODELS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 2: ESTIMATE HORIZON x DOSE INTERACTIONS")
message(strrep("=", 70))

# Base covariates (same as F04 / script 37)
BASE_COVS <- paste0(
  "pts_diff + pts_diff_sq + elo_diff + elo_diff_sq + ",
  "surface_elo_diff + surface_elo_diff_sq + ",
  "h2h_win_prop + h2h_count + age_diff + ",
  "surface_clay + surface_grass + ",
  "focal_pts + focal_pts_sq + focal_elo + focal_elo_sq + ",
  "focal_surf_elo + focal_surf_elo_sq + focal_age"
)

estimate_dose_horizon <- function(md, label, has_cf = FALSE) {
  message("\n  --- ", label, " ---")
  if (is.null(md) || nrow(md) == 0) {
    message("    No data. Skipping.")
    return(NULL)
  }

  # Ensure required columns
  needed <- c("won", "got_ll", "horizon", "matches_won_c", "dose",
              "pts_diff", "elo_diff", "focal_pts", "focal_elo")
  if (has_cf) needed <- c(needed, "v_hat")
  avail <- needed[needed %in% names(md)]
  if (length(avail) < length(needed)) {
    missing_cols <- setdiff(needed, avail)
    message("    Missing columns: ", paste(missing_cols, collapse = ", "))
    message("    Skipping (model frame from F04 may lack dose/v_hat columns).")
    return(NULL)
  }

  md <- md[complete.cases(md[, c("won", "pts_diff", "elo_diff", "got_ll",
                                  "matches_won_c", "dose")]), ]
  if (has_cf) md <- md[!is.na(md$v_hat), ]
  if (nrow(md) < 50) {
    message("    Too few complete cases (N=", nrow(md), "). Skipping.")
    return(NULL)
  }

  # (1) Horizon x matches_won
  cf_mw <- if (has_cf) " + v_hat:horizon + v_hat:horizon:matches_won_c" else ""
  fml_mw <- as.formula(paste0(
    "won ~ ", BASE_COVS,
    " + got_ll:horizon + got_ll:horizon:matches_won_c", cf_mw
  ))
  mod_mw <- tryCatch(
    glm(fml_mw, data = md, family = binomial(link = "logit")),
    error = function(e) { message("    Error (mw): ", e$message); NULL }
  )

  # (2) Horizon x perf dose
  cf_pp <- if (has_cf) " + v_hat:horizon + v_hat:horizon:dose" else ""
  fml_pp <- as.formula(paste0(
    "won ~ ", BASE_COVS,
    " + got_ll:horizon + got_ll:horizon:dose", cf_pp
  ))
  mod_pp <- tryCatch(
    glm(fml_pp, data = md, family = binomial(link = "logit")),
    error = function(e) { message("    Error (pp): ", e$message); NULL }
  )

  if (!is.null(mod_mw)) {
    message("    Horizon x MW: N=", nobs(mod_mw))
  }
  if (!is.null(mod_pp)) {
    message("    Horizon x PP: N=", nobs(mod_pp))
  }

  list(dose_mw_horizon = mod_mw, dose_pp_horizon = mod_pp,
       n_matches = nrow(md), label = label, has_cf = has_cf)
}

res_gs_atp    <- estimate_dose_horizon(md_list$gs_atp, "GS-ATP", has_cf = FALSE)
res_gs_wta    <- estimate_dose_horizon(md_list$gs_wta, "GS-WTA", has_cf = FALSE)
res_nongs_atp <- estimate_dose_horizon(md_list$nongs_atp, "NonGS-ATP", has_cf = TRUE)
res_nongs_wta <- estimate_dose_horizon(md_list$nongs_wta, "NonGS-WTA", has_cf = TRUE)


# ==============================================================================
# STEP 3: GENERATE TABLES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 3: GENERATE TABLES")
message(strrep("=", 70))

generate_dose_horizon_table <- function(res, dose_type, dose_label, label) {
  # dose_type: "dose_mw_horizon" or "dose_pp_horizon"
  if (is.null(res) || is.null(res[[dose_type]])) {
    return(paste(c(
      "\\begin{tabular}{l*{5}{c}}", "\\toprule",
      paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"), "\\midrule",
      paste0("\\multicolumn{6}{c}{\\textit{", label, ": not estimated}} \\\\"),
      "\\bottomrule", "\\end{tabular}"
    ), collapse = "\n"))
  }

  mod <- res[[dose_type]]
  ct <- coef(summary(mod))
  dose_var <- if (dose_type == "dose_mw_horizon") "matches_won_c" else "dose"

  lines <- c("\\begin{tabular}{l*{5}{c}}", "\\toprule",
             paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"), "\\midrule")

  # Row 1: got_ll:horizon (baseline LL effect)
  ll_coefs <- ll_ses <- character()
  for (h in HORIZON_LABS) {
    cname <- paste0("got_ll:horizon", h)
    if (cname %in% rownames(ct)) {
      ll_coefs <- c(ll_coefs, paste0(fmt(ct[cname, 1], 4), add_stars(ct[cname, 4])))
      ll_ses   <- c(ll_ses, paste0("(", fmt(ct[cname, 2], 4), ")"))
    } else {
      ll_coefs <- c(ll_coefs, "")
      ll_ses   <- c(ll_ses, "")
    }
  }
  lines <- c(lines, paste0("$\\hat{\\delta}_h$ & ", paste(ll_coefs, collapse = " & "), " \\\\"))
  lines <- c(lines, paste0(" & ", paste(ll_ses, collapse = " & "), " \\\\[0.3em]"))

  # Row 2: got_ll:horizon:dose_var (dose interaction)
  dose_coefs <- dose_ses <- character()
  for (h in HORIZON_LABS) {
    # Try both orderings of interaction
    cname1 <- paste0("got_ll:horizon", h, ":", dose_var)
    cname2 <- paste0("horizon", h, ":got_ll:", dose_var)
    cname <- if (cname1 %in% rownames(ct)) cname1
             else if (cname2 %in% rownames(ct)) cname2
             else NA_character_

    if (!is.na(cname) && cname %in% rownames(ct)) {
      dose_coefs <- c(dose_coefs, paste0(fmt(ct[cname, 1], 4), add_stars(ct[cname, 4])))
      dose_ses   <- c(dose_ses, paste0("(", fmt(ct[cname, 2], 4), ")"))
    } else {
      dose_coefs <- c(dose_coefs, "")
      dose_ses   <- c(dose_ses, "")
    }
  }
  lines <- c(lines, paste0("$\\hat{\\delta}_h \\times$ ", dose_label,
                           " & ", paste(dose_coefs, collapse = " & "), " \\\\"))
  lines <- c(lines, paste0(" & ", paste(dose_ses, collapse = " & "), " \\\\"))

  lines <- c(lines, "\\midrule")
  lines <- c(lines, paste0("Matches & \\multicolumn{5}{c}{",
                           format(nobs(mod), big.mark = ","), "} \\\\"))
  lines <- c(lines, "\\bottomrule", "\\end{tabular}")
  paste(lines, collapse = "\n")
}

# Generate 8 tables (4 samples x 2 dose types)
for (info in list(
  list(res = res_gs_atp,    tag = "gs_atp",    label = "GS-ATP"),
  list(res = res_gs_wta,    tag = "gs_wta",    label = "GS-WTA"),
  list(res = res_nongs_atp, tag = "nongs_atp", label = "NonGS-ATP"),
  list(res = res_nongs_wta, tag = "nongs_wta", label = "NonGS-WTA")
)) {
  writeLines(
    generate_dose_horizon_table(info$res, "dose_mw_horizon", "MW", info$label),
    file.path(FIRSTLL_TABLES, paste0("table_delta_dose_horizon_mw_", info$tag, ".tex"))
  )
  writeLines(
    generate_dose_horizon_table(info$res, "dose_pp_horizon", "Dose", info$label),
    file.path(FIRSTLL_TABLES, paste0("table_delta_dose_horizon_pp_", info$tag, ".tex"))
  )
}
message("  8 tables saved to Tables_FirstLL/")


# ==============================================================================
# STEP 4: DOSE-HORIZON FIGURES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 4: DOSE-HORIZON FIGURES")
message(strrep("=", 70))

generate_dose_horizon_figure <- function(res, tag) {
  if (is.null(res) || is.null(res$dose_mw_horizon)) return(NULL)
  mod <- res$dose_mw_horizon
  ct <- coef(summary(mod))

  plot_data <- data.frame(
    horizon = HORIZONS,
    delta_base = NA_real_, delta_base_se = NA_real_,
    delta_dose = NA_real_, delta_dose_se = NA_real_
  )

  for (i in seq_along(HORIZON_LABS)) {
    h <- HORIZON_LABS[i]
    cname_base <- paste0("got_ll:horizon", h)
    if (cname_base %in% rownames(ct)) {
      plot_data$delta_base[i]    <- ct[cname_base, 1]
      plot_data$delta_base_se[i] <- ct[cname_base, 2]
    }

    cname_dose <- paste0("got_ll:horizon", h, ":matches_won_c")
    if (!cname_dose %in% rownames(ct)) {
      cname_dose <- paste0("horizon", h, ":got_ll:matches_won_c")
    }
    if (cname_dose %in% rownames(ct)) {
      plot_data$delta_dose[i]    <- ct[cname_dose, 1]
      plot_data$delta_dose_se[i] <- ct[cname_dose, 2]
    }
  }

  plot_data <- plot_data[!is.na(plot_data$delta_base), ]
  if (nrow(plot_data) == 0) return(NULL)

  # Total effect at dose = 0 (baseline) and dose = 1 (1 extra win)
  plot_long <- rbind(
    data.frame(horizon = plot_data$horizon,
               effect = plot_data$delta_base,
               lo = plot_data$delta_base - 1.96 * plot_data$delta_base_se,
               hi = plot_data$delta_base + 1.96 * plot_data$delta_base_se,
               group = "Baseline (0 wins)"),
    data.frame(horizon = plot_data$horizon,
               effect = plot_data$delta_base + plot_data$delta_dose,
               lo = (plot_data$delta_base + plot_data$delta_dose) -
                    1.96 * sqrt(plot_data$delta_base_se^2 + plot_data$delta_dose_se^2),
               hi = (plot_data$delta_base + plot_data$delta_dose) +
                    1.96 * sqrt(plot_data$delta_base_se^2 + plot_data$delta_dose_se^2),
               group = "+1 match won")
  )

  p <- ggplot(plot_long, aes(x = horizon, y = effect, color = group, fill = group)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.10, color = NA) +
    geom_point(size = 2, position = position_dodge(width = 1)) +
    geom_line(position = position_dodge(width = 1)) +
    scale_x_continuous(breaks = HORIZONS) +
    scale_color_manual(values = c(col_control, col_treat)) +
    scale_fill_manual(values = c(col_control, col_treat)) +
    labs(x = "Weeks after LL event", y = "Delta (log-odds)", color = NULL, fill = NULL) +
    theme_paper() +
    theme(legend.position = "bottom")

  fig_path <- file.path(FIRSTLL_FIGURES, paste0("fig_delta_horizon_dose_", tag, ".pdf"))
  ggsave(fig_path, p, width = 6.5, height = 4, device = cairo_pdf, bg = "transparent")
  message("  Figure saved: ", fig_path)
}

generate_dose_horizon_figure(res_gs_atp,    "gs_atp")
generate_dose_horizon_figure(res_gs_wta,    "gs_wta")
generate_dose_horizon_figure(res_nongs_atp, "nongs_atp")
generate_dose_horizon_figure(res_nongs_wta, "nongs_wta")


# ==============================================================================
# STEP 5: SAVE RESULTS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 5: SAVE RESULTS")
message(strrep("=", 70))

all_results <- list(
  gs_atp    = res_gs_atp,
  gs_wta    = res_gs_wta,
  nongs_atp = res_nongs_atp,
  nongs_wta = res_nongs_wta,
  seed      = 20260416,
  timestamp = Sys.time()
)

saveRDS(all_results, file.path(FIRSTLL_CLEANED, "firstll_delta_dose_horizon_results.rds"))
message("  Saved: Data/cleaned/firstll/firstll_delta_dose_horizon_results.rds")

message("\n", strrep("=", 70))
message("DONE: F05_tournament_dose_horizon.R complete")
message(strrep("=", 70))
