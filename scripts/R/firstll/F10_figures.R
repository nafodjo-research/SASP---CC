# ==============================================================================
# F10_figures.R
# Event study figures and pi_ie distribution plots for the first-LL sample.
#
# 1. Event study figures: coefficient + 95% CI for points_change by horizon,
#    estimated from the stacked dynamic model. Separate plots for ATP and WTA.
#
# 2. pi_ie distribution: histogram of performance-based dose (pi_ie) among
#    treated players, separately for GS and non-GS.
#
# Inputs:
#   Data/cleaned/firstll/firstll_gs_est.rds
#   Data/cleaned/firstll/firstll_nongs_est.rds
#   Data/cleaned/firstll/firstll_performance_dose.rds
#
# Outputs:
#   Figures_FirstLL/fig_event_study_atp.pdf
#   Figures_FirstLL/fig_event_study_wta.pdf
#   Figures_FirstLL/fig_pi_dist_gs.pdf
#   Figures_FirstLL/fig_pi_dist_nongs.pdf
#
# Dependencies: ggplot2, fixest, dplyr, here
# ==============================================================================

set.seed(20260416)

library(ggplot2)
library(fixest)
library(dplyr)
library(here)

source(here("scripts", "R", "firstll", "firstll_helpers.R"))
summary_log <- character()


# ==============================================================================
# LOAD DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("F10: FIGURES (First-LL)")
message(strrep("=", 70))

gs  <- readRDS(file.path(FIRSTLL_CLEANED, "firstll_gs_est.rds"))
ngs <- readRDS(file.path(FIRSTLL_CLEANED, "firstll_nongs_est.rds"))

gs  <- ensure_scaled(gs)
ngs <- ensure_scaled(ngs)

# Split by tour
gs_atp  <- gs[gs$tour == "ATP", ]
gs_wta  <- gs[gs$tour == "WTA", ]


# ==============================================================================
# PART 1: EVENT STUDY FIGURES
# ==============================================================================
message("\n", strrep("-", 70))
message("PART 1: EVENT STUDY FIGURES")
message(strrep("-", 70))

#' Estimate stacked dynamic model and extract got_ll:horizon coefficients.
#'
#' @param data  Unstacked player-event data (one row per player-event)
#' @param outcome  Outcome stem (e.g., "points_change")
#' @return data.frame with columns: horizon_num, coef, se, ci_lo, ci_hi
estimate_event_study <- function(data, outcome = "points_change") {
  # Stack horizons
  stacked <- stack_horizons_full(data, outcomes = outcome)

  # Formula: outcome ~ got_ll:horizon + ZPRE | slam_year + horizon
  fml_str <- paste0(outcome, " ~ got_ll:horizon + ", ZPRE_FIRSTLL,
                    " | slam_year + horizon")
  fml <- as.formula(fml_str)

  fit <- feols(fml, data = stacked[!is.na(stacked[[outcome]]), ],
               cluster = ~player_id)

  # Extract got_ll:horizon coefficients
  cn <- names(coef(fit))
  ll_idx <- grep("^got_ll:horizon", cn)
  if (length(ll_idx) == 0) ll_idx <- grep("horizon.*:got_ll$", cn)

  coefs <- coef(fit)[ll_idx]
  ses   <- sqrt(diag(vcov(fit)))[ll_idx]

  # Extract horizon labels from coefficient names
  # Names like "got_ll:horizon4w" or "horizon4w:got_ll"
  h_labels <- gsub(".*horizon([0-9]+w).*", "\\1", cn[ll_idx])
  h_nums   <- as.numeric(gsub("w", "", h_labels))

  data.frame(
    horizon_num = h_nums,
    coef        = coefs,
    se          = ses,
    ci_lo       = coefs - 1.96 * ses,
    ci_hi       = coefs + 1.96 * ses,
    row.names   = NULL
  )
}

#' Plot event study coefficients.
plot_event_study <- function(es_df, y_label = "Coefficient") {
  ggplot(es_df, aes(x = horizon_num, y = coef)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50",
               linewidth = 0.4) +
    geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                  width = 1.5, linewidth = 0.5, color = col_treat) +
    geom_point(size = 2.5, color = col_treat) +
    scale_x_continuous(
      breaks = HORIZONS,
      labels = paste0(HORIZONS, "w")
    ) +
    labs(x = "Horizon (weeks)", y = y_label) +
    theme_paper()
}

# --- ATP event study ----------------------------------------------------------
message("  Estimating ATP event study...")
es_atp <- estimate_event_study(gs_atp, "points_change")

p_atp <- plot_event_study(es_atp, y_label = "Ranking Points Change")
ggsave(file.path(FIRSTLL_FIGURES, "fig_event_study_atp.pdf"),
       plot = p_atp, width = 6, height = 4, device = cairo_pdf)
message("  Saved: fig_event_study_atp.pdf")

# --- WTA event study ----------------------------------------------------------
message("  Estimating WTA event study...")
es_wta <- estimate_event_study(gs_wta, "points_change")

p_wta <- plot_event_study(es_wta, y_label = "Ranking Points Change")
ggsave(file.path(FIRSTLL_FIGURES, "fig_event_study_wta.pdf"),
       plot = p_wta, width = 6, height = 4, device = cairo_pdf)
message("  Saved: fig_event_study_wta.pdf")


# ==============================================================================
# PART 2: PI_IE DISTRIBUTION FIGURES
# ==============================================================================
message("\n", strrep("-", 70))
message("PART 2: PI_IE DISTRIBUTION FIGURES")
message(strrep("-", 70))

dose_data <- readRDS(file.path(FIRSTLL_CLEANED, "firstll_performance_dose.rds"))
message("  Dose data loaded: ", nrow(dose_data), " rows")

#' Plot histogram of pi_ie for treated players.
plot_pi_dist <- function(dose_df, event_type_val) {
  # Filter: treated players with valid pi_ie in (0, 1)
  sub <- dose_df[dose_df$event_type == event_type_val, ]
  if ("pi_ie" %in% names(sub)) {
    sub <- sub[!is.na(sub$pi_ie) & sub$pi_ie > 0 & sub$pi_ie < 1, ]
  } else {
    message("  WARNING: pi_ie column not found in dose data")
    return(NULL)
  }

  n_obs    <- nrow(sub)
  mean_pi  <- mean(sub$pi_ie)
  med_pi   <- median(sub$pi_ie)

  p <- ggplot(sub, aes(x = pi_ie)) +
    geom_histogram(bins = 30, fill = col_treat, color = "white",
                   alpha = 0.85) +
    annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,
             label = paste0("N = ", n_obs,
                            "\nMean = ", sprintf("%.3f", mean_pi),
                            "\nMedian = ", sprintf("%.3f", med_pi)),
             family = "serif", size = 3.5) +
    labs(x = expression(pi[ie]), y = "Count") +
    theme_paper()

  p
}

# GS pi_ie distribution
p_pi_gs <- plot_pi_dist(dose_data, "GS")
if (!is.null(p_pi_gs)) {
  ggsave(file.path(FIRSTLL_FIGURES, "fig_pi_dist_gs.pdf"),
         plot = p_pi_gs, width = 6, height = 4, device = cairo_pdf)
  message("  Saved: fig_pi_dist_gs.pdf")
}

# NonGS pi_ie distribution
p_pi_ngs <- plot_pi_dist(dose_data, "nonGS")
if (!is.null(p_pi_ngs)) {
  ggsave(file.path(FIRSTLL_FIGURES, "fig_pi_dist_nongs.pdf"),
         plot = p_pi_ngs, width = 6, height = 4, device = cairo_pdf)
  message("  Saved: fig_pi_dist_nongs.pdf")
}


# ==============================================================================
# SUMMARY
# ==============================================================================
message("\n", strrep("=", 70))
message("F10 COMPLETE")
message(strrep("=", 70))
message("  Event study ATP coefs: ",
        paste(sprintf("%.2f", es_atp$coef), collapse = ", "))
message("  Event study WTA coefs: ",
        paste(sprintf("%.2f", es_wta$coef), collapse = ", "))
message("\nDONE.")
