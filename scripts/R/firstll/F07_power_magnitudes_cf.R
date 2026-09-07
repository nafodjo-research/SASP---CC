# ==============================================================================
# F07_power_magnitudes_cf.R
# Power analysis, economic magnitudes, and CF validation for first-LL sample.
#
# Adapts 43_power_magnitudes_cf.R to first-LL restricted sample:
#   - Uses ZPRE_FIRSTLL (drops prior-LL variables, all zero by construction)
#   - Reads firstll_gs_est_v2.rds and firstll_nongs_est_v2.rds
#   - CF validation uses GS lottery subsample with v_hat_gs
#
# FIX 1: Power analysis / minimum detectable effects (MDE) at 80% power
# FIX 2: Economic magnitudes -- control group means, effect/mean ratios
# FIX 3: CF validation on GS lottery sample (v_hat should not change estimates)
#
# Inputs:
#   Data/cleaned/firstll/firstll_gs_est_v2.rds
#   Data/cleaned/firstll/firstll_nongs_est_v2.rds
#   Data/cleaned/firstll/firstll_delta_model_results.rds (optional, for delta MDE)
#
# Outputs:
#   Tables_FirstLL/table_power_analysis.tex
#   Tables_FirstLL/table_economic_magnitudes.tex
#   Tables_FirstLL/table_cf_validation.tex
#   Data/cleaned/firstll/firstll_power_magnitudes_cf_results.rds
#
# Dependencies: fixest, dplyr, here
# ==============================================================================

set.seed(20260416)

library(fixest)
library(dplyr)
library(here)

source(here("scripts", "R", "firstll", "firstll_helpers.R"))
summary_log <- character()


# ==============================================================================
# LOAD DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("LOADING DATA")
message(strrep("=", 70))

gs  <- readRDS(file.path(FIRSTLL_CLEANED, "firstll_gs_est_v2.rds"))
ngs <- readRDS(file.path(FIRSTLL_CLEANED, "firstll_nongs_est_v2.rds"))

gs  <- ensure_scaled(gs)
ngs <- ensure_scaled(ngs)

# Split by tour
gs_atp  <- gs[gs$tour == "ATP", ]
gs_wta  <- gs[gs$tour == "WTA", ]
ngs_atp <- ngs[ngs$tour == "ATP", ]
ngs_wta <- ngs[ngs$tour == "WTA", ]

message("  GS-ATP: ", nrow(gs_atp), " | GS-WTA: ", nrow(gs_wta))
message("  NonGS-ATP: ", nrow(ngs_atp), " | NonGS-WTA: ", nrow(ngs_wta))

# Stack horizons
st_gs_atp  <- stack_horizons_full(gs_atp)
st_gs_wta  <- stack_horizons_full(gs_wta)
st_ngs_atp <- stack_horizons_full(ngs_atp)
st_ngs_wta <- stack_horizons_full(ngs_wta)


# ==============================================================================
# FIX 1: POWER ANALYSIS / MINIMUM DETECTABLE EFFECT
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 1: POWER ANALYSIS")
message(strrep("=", 70))

z_alpha <- 1.96   # two-sided 5%
z_beta  <- 0.84   # 80% power
mde_multiplier <- z_alpha + z_beta  # = 2.80

# --- Panel A: Delta model (match-level performance) --------------------------
delta_res <- tryCatch(
  readRDS(file.path(FIRSTLL_CLEANED, "firstll_delta_model_results.rds")),
  error = function(e) {
    message("  Could not load firstll_delta_model_results.rds: ", e$message)
    NULL
  }
)

# Extract delta model info or use placeholders
delta_known <- data.frame(
  sample   = c("GS-ATP", "GS-WTA", "NonGS-ATP", "NonGS-WTA"),
  delta    = rep(NA_real_, 4),
  se       = rep(NA_real_, 4),
  n_match  = rep(NA_integer_, 4),
  stringsAsFactors = FALSE
)

if (!is.null(delta_res) && !identical(delta_res$status, "placeholder")) {
  sample_map <- list(
    "GS-ATP"    = "gs_atp",
    "GS-WTA"    = "gs_wta",
    "NonGS-ATP" = "nongs_atp",
    "NonGS-WTA" = "nongs_wta"
  )

  for (i in seq_len(nrow(delta_known))) {
    s <- delta_known$sample[i]
    nm <- sample_map[[s]]
    r <- delta_res[[nm]]
    if (!is.null(r) && !is.null(r$pooled)) {
      mod <- r$pooled
      if ("got_ll" %in% names(coef(mod))) {
        ct <- coef(summary(mod))
        delta_known$delta[i]   <- ct["got_ll", "Estimate"]
        delta_known$se[i]      <- ct["got_ll", "Std. Error"]
        delta_known$n_match[i] <- nobs(mod)
      }
    }
  }
  message("  Extracted delta model estimates from F04 results")
} else {
  message("  Delta model results not available; Panel A will show NA")
}

delta_known$mde_logodds <- mde_multiplier * delta_known$se
delta_known$mde_pp      <- delta_known$mde_logodds * 0.25  # approx: logit deriv at p=0.5

message("  Delta model MDE results:")
for (i in seq_len(nrow(delta_known))) {
  message(sprintf("    %s: delta=%s, SE=%s, MDE(log-odds)=%s, MDE(pp)=%s",
                  delta_known$sample[i],
                  ifelse(is.na(delta_known$delta[i]), "NA", fmt(delta_known$delta[i], 3)),
                  ifelse(is.na(delta_known$se[i]), "NA", fmt(delta_known$se[i], 3)),
                  ifelse(is.na(delta_known$mde_logodds[i]), "NA", fmt(delta_known$mde_logodds[i], 3)),
                  ifelse(is.na(delta_known$mde_pp[i]), "NA", fmt(delta_known$mde_pp[i], 3))))
}

# --- Panel B: Elo change from stacked dynamic model -------------------------
message("\n  Computing Elo MDE from stacked dynamic models...")

elo_mde <- list()

estimate_elo_se <- function(sdata, label, cf_term = NULL) {
  rhs <- "got_ll:horizon"
  if (!is.null(cf_term)) rhs <- paste0(rhs, " + ", cf_term, ":horizon")
  rhs <- paste0(rhs, " + ", ZPRE_FIRSTLL)
  fml_str <- paste0("elo_change ~ ", rhs, " | slam_year + horizon")
  fit <- tryCatch(
    feols(as.formula(fml_str), data = sdata, cluster = ~player_id),
    error = function(e) {
      message("    ERROR in ", label, ": ", e$message)
      NULL
    }
  )
  if (is.null(fit)) return(NULL)

  cn <- names(coef(fit))
  ll_idx <- grep("got_ll:horizon|horizon.*:got_ll", cn)
  coefs <- coef(fit)[ll_idx]
  ses   <- sqrt(diag(vcov(fit)))[ll_idx]
  pvals <- 2 * pnorm(-abs(coefs / ses))
  horizon_names <- regmatches(cn[ll_idx], regexpr("\\d+w", cn[ll_idx]))

  result <- data.frame(
    horizon = horizon_names,
    coef    = coefs,
    se      = ses,
    pval    = pvals,
    mde     = mde_multiplier * ses,
    n_obs   = nobs(fit),
    stringsAsFactors = FALSE
  )
  rownames(result) <- NULL
  message("    ", label, ": N=", nobs(fit))
  result
}

elo_mde$gs_atp  <- estimate_elo_se(st_gs_atp,  "GS-ATP")
elo_mde$gs_wta  <- estimate_elo_se(st_gs_wta,  "GS-WTA")
elo_mde$ngs_atp <- estimate_elo_se(st_ngs_atp, "NonGS-ATP", cf_term = "v_hat")
elo_mde$ngs_wta <- estimate_elo_se(st_ngs_wta, "NonGS-WTA", cf_term = "v_hat")

# --- Generate Power Analysis Table -------------------------------------------
message("\n  Generating table_power_analysis.tex...")

get_26w <- function(df) {
  if (is.null(df)) return(list(coef = NA, se = NA, mde = NA))
  row <- df[df$horizon == "26w", ]
  if (nrow(row) == 0) return(list(coef = NA, se = NA, mde = NA))
  list(coef = row$coef[1], se = row$se[1], mde = row$mde[1])
}

elo_26 <- list(
  gs_atp  = get_26w(elo_mde$gs_atp),
  gs_wta  = get_26w(elo_mde$gs_wta),
  ngs_atp = get_26w(elo_mde$ngs_atp),
  ngs_wta = get_26w(elo_mde$ngs_wta)
)

L <- character()
L <- c(L, "\\begin{tabular}{lcccc}")
L <- c(L, "\\toprule")
L <- c(L, " & GS-ATP & GS-WTA & NonGS-ATP & NonGS-WTA \\\\")
L <- c(L, "\\midrule")

# Panel A: Match-level performance
L <- c(L, "\\multicolumn{5}{l}{\\textit{Panel A: Match-level performance ($\\delta$ model)}} \\\\")

format_or_na <- function(x, d) ifelse(is.na(x), "--", fmt(x, d))

L <- c(L, paste0("$\\hat{\\delta}$ (log-odds) & ",
                 paste(sapply(delta_known$delta, format_or_na, d = 3), collapse = " & "), " \\\\"))
L <- c(L, paste0("SE & ",
                 paste(sapply(delta_known$se, format_or_na, d = 3), collapse = " & "), " \\\\"))
L <- c(L, paste0("MDE (log-odds, 80\\% power) & ",
                 paste(sapply(delta_known$mde_logodds, format_or_na, d = 3), collapse = " & "), " \\\\"))
L <- c(L, paste0("MDE (pp win prob) & ",
                 paste(sapply(delta_known$mde_pp, format_or_na, d = 3), collapse = " & "), " \\\\"))
L <- c(L, paste0("$N$ matches & ",
                 paste(ifelse(is.na(delta_known$n_match), "--",
                              formatC(delta_known$n_match, format = "d", big.mark = ",")),
                       collapse = " & "), " \\\\"))

L <- c(L, "\\midrule")

# Panel B: Elo change at 26w
L <- c(L, "\\multicolumn{5}{l}{\\textit{Panel B: Elo change (stacked dynamic model, 26w horizon)}} \\\\")

elo_coefs <- c(elo_26$gs_atp$coef, elo_26$gs_wta$coef,
               elo_26$ngs_atp$coef, elo_26$ngs_wta$coef)
elo_ses   <- c(elo_26$gs_atp$se, elo_26$gs_wta$se,
               elo_26$ngs_atp$se, elo_26$ngs_wta$se)
elo_mdes  <- c(elo_26$gs_atp$mde, elo_26$gs_wta$mde,
               elo_26$ngs_atp$mde, elo_26$ngs_wta$mde)

L <- c(L, paste0("$\\hat{\\beta}_{26w}$ (Elo pts) & ",
                 paste(sapply(elo_coefs, format_or_na, d = 1), collapse = " & "), " \\\\"))
L <- c(L, paste0("SE at 26w & ",
                 paste(sapply(elo_ses, format_or_na, d = 1), collapse = " & "), " \\\\"))
L <- c(L, paste0("MDE at 26w (Elo pts) & ",
                 paste(sapply(elo_mdes, format_or_na, d = 1), collapse = " & "), " \\\\"))

# Panel C: MDE by horizon
L <- c(L, "\\midrule")
L <- c(L, "\\multicolumn{5}{l}{\\textit{Panel C: MDE by horizon (Elo points)}} \\\\")

for (h in HORIZON_LABS) {
  vals <- sapply(c("gs_atp", "gs_wta", "ngs_atp", "ngs_wta"), function(s) {
    if (is.null(elo_mde[[s]])) return(NA)
    row <- elo_mde[[s]][elo_mde[[s]]$horizon == h, ]
    if (nrow(row) == 0) return(NA)
    row$mde[1]
  })
  L <- c(L, paste0("\\quad ", h, " & ",
                   paste(sapply(vals, format_or_na, d = 1), collapse = " & "), " \\\\"))
}

L <- c(L, "\\bottomrule")
L <- c(L, "\\end{tabular}")

writeLines(L, file.path(FIRSTLL_TABLES, "table_power_analysis.tex"))
message("  Wrote: Tables_FirstLL/table_power_analysis.tex")


# ==============================================================================
# FIX 2: ECONOMIC MAGNITUDES -- CONTROL GROUP MEANS
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 2: ECONOMIC MAGNITUDES")
message(strrep("=", 70))

compute_magnitudes <- function(unstacked, label) {
  message("  Computing magnitudes for ", label, "...")

  ctrl_idx  <- unstacked$got_ll == 0
  treat_idx <- unstacked$got_ll == 1
  baseline_pts <- mean(unstacked$pre_rank_pts[ctrl_idx], na.rm = TRUE)
  baseline_elo <- mean(unstacked$pre_elo[ctrl_idx], na.rm = TRUE)

  rows <- list()
  rows[[1]] <- data.frame(
    outcome = "Ranking pts (baseline)", horizon = "--",
    ctrl_mean = baseline_pts,
    treat_mean = mean(unstacked$pre_rank_pts[treat_idx], na.rm = TRUE),
    ctrl_sd = sd(unstacked$pre_rank_pts[ctrl_idx], na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  rows[[2]] <- data.frame(
    outcome = "Elo (baseline)", horizon = "--",
    ctrl_mean = baseline_elo,
    treat_mean = mean(unstacked$pre_elo[treat_idx], na.rm = TRUE),
    ctrl_sd = sd(unstacked$pre_elo[ctrl_idx], na.rm = TRUE),
    stringsAsFactors = FALSE
  )

  k <- 3
  for (ob in outcomes_base) {
    for (h in HORIZONS) {
      col <- paste0(ob, "_", h, "w")
      if (!col %in% names(unstacked)) next
      cm <- mean(unstacked[[col]][ctrl_idx], na.rm = TRUE)
      tm <- mean(unstacked[[col]][treat_idx], na.rm = TRUE)
      cs <- sd(unstacked[[col]][ctrl_idx], na.rm = TRUE)
      rows[[k]] <- data.frame(
        outcome = outcome_labels[ob], horizon = paste0(h, "w"),
        ctrl_mean = cm, treat_mean = tm, ctrl_sd = cs,
        stringsAsFactors = FALSE
      )
      k <- k + 1
    }
  }

  result <- do.call(rbind, rows)
  result$raw_diff  <- result$treat_mean - result$ctrl_mean
  result$pct_ctrl  <- ifelse(abs(result$ctrl_mean) > 1e-6,
                             (result$raw_diff / abs(result$ctrl_mean)) * 100, NA)
  result$effect_sd <- ifelse(result$ctrl_sd > 1e-6,
                             result$raw_diff / result$ctrl_sd, NA)
  result$sample <- label
  result
}

mag_gs_atp  <- compute_magnitudes(gs_atp, "GS-ATP")
mag_gs_wta  <- compute_magnitudes(gs_wta, "GS-WTA")
mag_ngs_atp <- compute_magnitudes(ngs_atp, "NonGS-ATP")
mag_ngs_wta <- compute_magnitudes(ngs_wta, "NonGS-WTA")

# Regression-based effects for key outcomes
get_regression_effects <- function(sdata, label, cf_term = NULL) {
  results <- list()
  for (ob in outcomes_base) {
    if (all(is.na(sdata[[ob]]))) next
    rhs <- "got_ll:horizon"
    if (!is.null(cf_term)) rhs <- paste0(rhs, " + ", cf_term, ":horizon")
    rhs <- paste0(rhs, " + ", ZPRE_FIRSTLL)
    fml_str <- paste0(ob, " ~ ", rhs, " | slam_year + horizon")
    fit <- tryCatch(
      feols(as.formula(fml_str), data = sdata, cluster = ~player_id),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      cn <- names(coef(fit))
      ll_idx <- grep("got_ll:horizon|horizon.*:got_ll", cn)
      coefs <- coef(fit)[ll_idx]
      ses <- sqrt(diag(vcov(fit)))[ll_idx]
      pvals <- 2 * pnorm(-abs(coefs / ses))
      h_labs <- regmatches(cn[ll_idx], regexpr("\\d+w", cn[ll_idx]))
      results[[ob]] <- data.frame(
        horizon = h_labs, coef = coefs, se = ses, pval = pvals,
        stringsAsFactors = FALSE
      )
      rownames(results[[ob]]) <- NULL
    }
  }
  results
}

reg_gs_atp  <- get_regression_effects(st_gs_atp, "GS-ATP")
reg_gs_wta  <- get_regression_effects(st_gs_wta, "GS-WTA")
reg_ngs_atp <- get_regression_effects(st_ngs_atp, "NonGS-ATP", cf_term = "v_hat")
reg_ngs_wta <- get_regression_effects(st_ngs_wta, "NonGS-WTA", cf_term = "v_hat")

# --- Generate Economic Magnitudes Table --------------------------------------
message("\n  Generating table_economic_magnitudes.tex...")

build_magnitude_panel <- function(mag_df, reg_list, label) {
  panel_lines <- character()
  panel_lines <- c(panel_lines,
    paste0("\\multicolumn{5}{l}{\\textit{", label, "}} \\\\"))
  panel_lines <- c(panel_lines, "\\midrule")

  # Baseline rows
  base_pts <- mag_df[mag_df$outcome == "Ranking pts (baseline)", ]
  panel_lines <- c(panel_lines,
    paste0("Ranking pts (baseline) & ",
           fmt(base_pts$ctrl_mean, 0), " & -- & -- & -- \\\\"))

  base_elo <- mag_df[mag_df$outcome == "Elo (baseline)", ]
  panel_lines <- c(panel_lines,
    paste0("Elo (baseline) & ",
           fmt(base_elo$ctrl_mean, 0), " & -- & -- & -- \\\\[0.3em]"))

  # Key outcomes at selected horizons
  key_horizons <- c("4w", "26w", "52w")
  key_outcomes <- c("points_change", "elo_change", "n_main_draws")
  ob_display <- c(
    points_change = "Ranking Pts $\\Delta$",
    elo_change    = "Elo $\\Delta$",
    n_main_draws  = "Main Draws"
  )

  for (ob in key_outcomes) {
    for (h in key_horizons) {
      row <- mag_df[mag_df$outcome == outcome_labels[ob] & mag_df$horizon == h, ]
      if (nrow(row) == 0) next

      reg_est <- NA; reg_se <- NA; reg_pv <- NA
      if (!is.null(reg_list[[ob]])) {
        reg_row <- reg_list[[ob]][reg_list[[ob]]$horizon == h, ]
        if (nrow(reg_row) > 0) {
          reg_est <- reg_row$coef[1]
          reg_se  <- reg_row$se[1]
          reg_pv  <- reg_row$pval[1]
        }
      }

      stars <- add_stars(reg_pv)
      pct_of_ctrl <- ifelse(!is.na(reg_est) & abs(row$ctrl_mean) > 1e-6,
                            (reg_est / abs(row$ctrl_mean)) * 100, NA)
      effect_sd <- ifelse(!is.na(reg_est) & row$ctrl_sd > 1e-6,
                          reg_est / row$ctrl_sd, NA)

      panel_lines <- c(panel_lines,
        paste0("\\quad ", ob_display[ob], " (", h, ") & ",
               fmt(row$ctrl_mean, 1), " & ",
               ifelse(is.na(reg_est), "--", paste0(fmt(reg_est, 1), stars)), " & ",
               ifelse(is.na(pct_of_ctrl), "--", paste0(fmt(pct_of_ctrl, 1), "\\%")), " & ",
               ifelse(is.na(effect_sd), "--", fmt(effect_sd, 3)), " \\\\"))
    }
    panel_lines <- c(panel_lines, "")
  }
  panel_lines
}

M <- character()
M <- c(M, "\\begin{tabular}{lcccc}")
M <- c(M, "\\toprule")
M <- c(M, " & Control Mean & LL Effect & Effect/Mean & Effect/SD \\\\")
M <- c(M, "\\midrule")

M <- c(M, build_magnitude_panel(mag_gs_atp,  reg_gs_atp,  "Panel A: ATP Grand Slam (First-LL)"))
M <- c(M, "\\\\[0.5em]")
M <- c(M, build_magnitude_panel(mag_gs_wta,  reg_gs_wta,  "Panel B: WTA Grand Slam (First-LL)"))
M <- c(M, "\\\\[0.5em]")
M <- c(M, build_magnitude_panel(mag_ngs_atp, reg_ngs_atp, "Panel C: ATP Non-Grand Slam (First-LL)"))
M <- c(M, "\\\\[0.5em]")
M <- c(M, build_magnitude_panel(mag_ngs_wta, reg_ngs_wta, "Panel D: WTA Non-Grand Slam (First-LL)"))

M <- c(M, "\\bottomrule")
M <- c(M, "\\end{tabular}")

writeLines(M, file.path(FIRSTLL_TABLES, "table_economic_magnitudes.tex"))
message("  Wrote: Tables_FirstLL/table_economic_magnitudes.tex")


# ==============================================================================
# FIX 3: CF VALIDATION ON GS LOTTERY SAMPLE
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 3: CONTROL FUNCTION VALIDATION")
message(strrep("=", 70))

# GS uses lottery: treatment is randomized among qualifying losers.
# If CF is well-specified, adding v_hat should not change estimates.
# P_i^{LL} = n_ll_slots / n_losers_at_event (equal for all pool members).

compute_gs_p_ll <- function(df) {
  event_info <- df |>
    group_by(slam_year) |>
    summarise(
      n_ll   = sum(got_ll, na.rm = TRUE),
      n_pool = n(),
      .groups = "drop"
    ) |>
    mutate(p_ll = n_ll / n_pool)

  df <- merge(df, event_info[, c("slam_year", "p_ll", "n_pool")],
              by = "slam_year", all.x = TRUE)
  df
}

gs_atp_cf <- compute_gs_p_ll(gs_atp)
gs_wta_cf <- compute_gs_p_ll(gs_wta)

message("  GS-ATP P(LL) range: [", fmt(min(gs_atp_cf$p_ll), 3), ", ",
        fmt(max(gs_atp_cf$p_ll), 3), "]")
message("  GS-WTA P(LL) range: [", fmt(min(gs_wta_cf$p_ll), 3), ", ",
        fmt(max(gs_wta_cf$p_ll), 3), "]")

# Compute v_hat = D * phi(Phi^{-1}(P))/P - (1-D) * phi(Phi^{-1}(P))/(1-P)
gs_atp_cf$v_hat_gs <- compute_gen_residual(gs_atp_cf$got_ll, gs_atp_cf$p_ll)
gs_wta_cf$v_hat_gs <- compute_gen_residual(gs_wta_cf$got_ll, gs_wta_cf$p_ll)

message("  GS-ATP v_hat range: [", fmt(min(gs_atp_cf$v_hat_gs), 3), ", ",
        fmt(max(gs_atp_cf$v_hat_gs), 3), "]")
message("  GS-WTA v_hat range: [", fmt(min(gs_wta_cf$v_hat_gs), 3), ", ",
        fmt(max(gs_wta_cf$v_hat_gs), 3), "]")

# Stack with v_hat_gs
st_gs_atp_cf <- stack_horizons_full(gs_atp_cf)
st_gs_wta_cf <- stack_horizons_full(gs_wta_cf)

# --- Estimate with and without CF for each outcome ---
cf_validation <- list()

estimate_cf_pair <- function(sdata, label) {
  results <- list()
  key_outcomes <- c("points_change", "elo_change", "n_main_draws")

  for (ob in key_outcomes) {
    if (all(is.na(sdata[[ob]]))) next

    # Without CF (baseline)
    rhs_base <- paste0("got_ll:horizon + ", ZPRE_FIRSTLL)
    fml_base <- paste0(ob, " ~ ", rhs_base, " | slam_year + horizon")
    fit_base <- tryCatch(
      feols(as.formula(fml_base), data = sdata, cluster = ~player_id),
      error = function(e) NULL
    )

    # With CF (v_hat_gs from GS lottery)
    rhs_cf <- paste0("got_ll:horizon + v_hat_gs:horizon + ", ZPRE_FIRSTLL)
    fml_cf <- paste0(ob, " ~ ", rhs_cf, " | slam_year + horizon")
    fit_cf <- tryCatch(
      feols(as.formula(fml_cf), data = sdata, cluster = ~player_id),
      error = function(e) NULL
    )

    if (!is.null(fit_base) && !is.null(fit_cf)) {
      extract_ll_coefs <- function(fit) {
        cn <- names(coef(fit))
        ll_idx <- grep("got_ll:horizon|horizon.*:got_ll", cn)
        ll_idx <- ll_idx[!grepl("v_hat", cn[ll_idx])]
        coefs <- coef(fit)[ll_idx]
        ses   <- sqrt(diag(vcov(fit)))[ll_idx]
        pvals <- 2 * pnorm(-abs(coefs / ses))
        h_labs <- regmatches(cn[ll_idx], regexpr("\\d+w", cn[ll_idx]))
        data.frame(horizon = h_labs, coef = coefs, se = ses, pval = pvals,
                   stringsAsFactors = FALSE, row.names = NULL)
      }

      extract_rho <- function(fit) {
        cn <- names(coef(fit))
        rho_idx <- grep("v_hat_gs:horizon|horizon.*:v_hat_gs", cn)
        if (length(rho_idx) == 0) return(NULL)
        coefs <- coef(fit)[rho_idx]
        ses   <- sqrt(diag(vcov(fit)))[rho_idx]
        pvals <- 2 * pnorm(-abs(coefs / ses))
        h_labs <- regmatches(cn[rho_idx], regexpr("\\d+w", cn[rho_idx]))
        data.frame(horizon = h_labs, coef = coefs, se = ses, pval = pvals,
                   stringsAsFactors = FALSE, row.names = NULL)
      }

      results[[ob]] <- list(
        base = extract_ll_coefs(fit_base),
        cf   = extract_ll_coefs(fit_cf),
        rho  = extract_rho(fit_cf)
      )

      message("    ", label, " ", ob, ": base N=", nobs(fit_base),
              " cf N=", nobs(fit_cf))
    }
  }
  results
}

cf_validation$gs_atp <- estimate_cf_pair(st_gs_atp_cf, "GS-ATP")
cf_validation$gs_wta <- estimate_cf_pair(st_gs_wta_cf, "GS-WTA")

# --- Generate CF Validation Table ---
message("\n  Generating table_cf_validation.tex...")

build_cf_panel <- function(res, label, ob = "points_change") {
  if (is.null(res) || is.null(res[[ob]])) {
    return(paste0("\\multicolumn{6}{l}{\\textit{", label,
                  ": estimation failed}} \\\\"))
  }
  r <- res[[ob]]
  panel_lines <- character()
  panel_lines <- c(panel_lines,
    paste0("\\multicolumn{6}{l}{\\textit{", label, "}} \\\\"))
  panel_lines <- c(panel_lines, "\\midrule")

  # Without CF row
  base_cells <- vapply(HORIZON_LABS, function(h) {
    row <- r$base[r$base$horizon == h, ]
    if (nrow(row) == 0) return("--")
    paste0(fmt(row$coef[1], 1), add_stars(row$pval[1]))
  }, character(1))
  base_se_cells <- vapply(HORIZON_LABS, function(h) {
    row <- r$base[r$base$horizon == h, ]
    if (nrow(row) == 0) return("")
    paste0("(", fmt(row$se[1], 1), ")")
  }, character(1))

  panel_lines <- c(panel_lines,
    paste0("Without CF & ", paste(base_cells, collapse = " & "), " \\\\"))
  panel_lines <- c(panel_lines,
    paste0(" & ", paste(base_se_cells, collapse = " & "), " \\\\"))

  # With CF row
  cf_cells <- vapply(HORIZON_LABS, function(h) {
    row <- r$cf[r$cf$horizon == h, ]
    if (nrow(row) == 0) return("--")
    paste0(fmt(row$coef[1], 1), add_stars(row$pval[1]))
  }, character(1))
  cf_se_cells <- vapply(HORIZON_LABS, function(h) {
    row <- r$cf[r$cf$horizon == h, ]
    if (nrow(row) == 0) return("")
    paste0("(", fmt(row$se[1], 1), ")")
  }, character(1))

  panel_lines <- c(panel_lines,
    paste0("With CF & ", paste(cf_cells, collapse = " & "), " \\\\"))
  panel_lines <- c(panel_lines,
    paste0(" & ", paste(cf_se_cells, collapse = " & "), " \\\\"))

  # Rho row
  if (!is.null(r$rho) && nrow(r$rho) > 0) {
    rho_cells <- vapply(HORIZON_LABS, function(h) {
      row <- r$rho[r$rho$horizon == h, ]
      if (nrow(row) == 0) return("--")
      paste0(fmt(row$coef[1], 1), add_stars(row$pval[1]))
    }, character(1))
    rho_se_cells <- vapply(HORIZON_LABS, function(h) {
      row <- r$rho[r$rho$horizon == h, ]
      if (nrow(row) == 0) return("")
      paste0("(", fmt(row$se[1], 1), ")")
    }, character(1))

    panel_lines <- c(panel_lines,
      paste0("$\\hat{\\rho}_h$ & ", paste(rho_cells, collapse = " & "), " \\\\"))
    panel_lines <- c(panel_lines,
      paste0(" & ", paste(rho_se_cells, collapse = " & "), " \\\\"))
  }

  panel_lines
}

T_lines <- character()
T_lines <- c(T_lines, "\\begin{tabular}{l*{5}{c}}")
T_lines <- c(T_lines, "\\toprule")
T_lines <- c(T_lines, paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"))
T_lines <- c(T_lines, "\\midrule")

# Ranking points as primary outcome
T_lines <- c(T_lines, "\\multicolumn{6}{l}{\\textbf{Outcome: Ranking Points $\\Delta$}} \\\\[0.3em]")
T_lines <- c(T_lines, build_cf_panel(cf_validation$gs_atp, "Panel A: ATP Grand Slam (First-LL)",
                                     "points_change"))
T_lines <- c(T_lines, "\\\\[0.5em]")
T_lines <- c(T_lines, build_cf_panel(cf_validation$gs_wta, "Panel B: WTA Grand Slam (First-LL)",
                                     "points_change"))

T_lines <- c(T_lines, "\\\\[0.5em]")
T_lines <- c(T_lines, "\\multicolumn{6}{l}{\\textbf{Outcome: Elo $\\Delta$}} \\\\[0.3em]")
T_lines <- c(T_lines, build_cf_panel(cf_validation$gs_atp, "Panel C: ATP Grand Slam (First-LL)",
                                     "elo_change"))
T_lines <- c(T_lines, "\\\\[0.5em]")
T_lines <- c(T_lines, build_cf_panel(cf_validation$gs_wta, "Panel D: WTA Grand Slam (First-LL)",
                                     "elo_change"))

T_lines <- c(T_lines, "\\bottomrule")
T_lines <- c(T_lines, "\\end{tabular}")

# NOTE: F12 is the canonical producer of table_cf_validation.tex (it applies
# the corrected uniform-P_i specification). F07 writes its own diagnostic copy
# under a distinct filename so the two do not race depending on run order.
writeLines(T_lines, file.path(FIRSTLL_TABLES, "table_cf_validation_f07.tex"))
message("  Wrote: Tables_FirstLL/table_cf_validation_f07.tex (diagnostic; ",
        "F12 writes the canonical table_cf_validation.tex)")


# ==============================================================================
# SAVE ALL RESULTS
# ==============================================================================
message("\n", strrep("=", 70))
message("SAVING ALL RESULTS")
message(strrep("=", 70))

all_results <- list(
  # FIX 1: Power analysis
  delta_mde = delta_known,
  elo_mde   = elo_mde,

  # FIX 2: Economic magnitudes
  magnitudes = list(
    gs_atp  = mag_gs_atp,
    gs_wta  = mag_gs_wta,
    ngs_atp = mag_ngs_atp,
    ngs_wta = mag_ngs_wta
  ),
  regression_effects = list(
    gs_atp  = reg_gs_atp,
    gs_wta  = reg_gs_wta,
    ngs_atp = reg_ngs_atp,
    ngs_wta = reg_ngs_wta
  ),

  # FIX 3: CF validation
  cf_validation = cf_validation,

  # Metadata
  seed      = 20260416,
  timestamp = Sys.time()
)

saveRDS(all_results, file.path(FIRSTLL_CLEANED, "firstll_power_magnitudes_cf_results.rds"))
message("  Saved: Data/cleaned/firstll/firstll_power_magnitudes_cf_results.rds")

message("\n", strrep("=", 70))
message("ALL FIXES COMPLETE (First-LL)")
message("  Tables_FirstLL/table_power_analysis.tex")
message("  Tables_FirstLL/table_economic_magnitudes.tex")
message("  Tables_FirstLL/table_cf_validation.tex")
message("  Data/cleaned/firstll/firstll_power_magnitudes_cf_results.rds")
message(strrep("=", 70))
