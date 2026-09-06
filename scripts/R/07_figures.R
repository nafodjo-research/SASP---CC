# ============================================================================
# Title:    Publication-Quality Figures for Lucky Loser RDD Paper
# Author:   Data Engineer (Claude)
# Date:     2026-03-21
# Purpose:  Create six figures for the Lucky Loser career trajectories paper.
#           Figures follow project style rules: no embedded titles, serif fonts,
#           minimal gridlines, saved as both PDF and PNG.
# Inputs:   Data/cleaned/estimation_sample_final.rds
# Outputs:  Figures/fig1_first_stage_ll_rate.pdf (.png)
#           Figures/fig2_rdd_rank_change_26w.pdf (.png)
#           Figures/fig3_rdd_elo_change_26w.pdf (.png)
#           Figures/fig4_event_study_placeholder.pdf (.png)
#           Figures/fig5_ll_entries_over_time.pdf (.png)
#           Figures/fig6_sample_by_tourney_level.pdf (.png)
#           Output/fig1_data.rds ... Output/fig6_data.rds
# ============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)
library(RColorBrewer)
library(here)

# -- Paths (relative to project root) ----------------------------------------
data_path   <- here("Data", "cleaned", "estimation_sample_final.rds")
fig_dir     <- here("Figures")
output_dir  <- here("Output")

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# -- Load data ----------------------------------------------------------------
d <- readRDS(data_path)
message("Loaded estimation sample: ", nrow(d), " rows x ", ncol(d), " columns")

# -- Custom theme -------------------------------------------------------------
# Matches project rules: serif font, no titles, minimal gridlines, booktabs feel
theme_paper <- function(base_size = 14) {
  theme_minimal(base_size = base_size, base_family = "serif") %+replace%
    theme(
      plot.title       = element_blank(),
      plot.subtitle    = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.3, color = "grey85"),
      axis.line        = element_line(linewidth = 0.4, color = "grey30"),
      axis.ticks       = element_line(linewidth = 0.3, color = "grey30"),
      axis.ticks.length = unit(2, "pt"),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      legend.key.width = unit(18, "pt"),
      plot.margin      = margin(8, 12, 8, 8, unit = "pt"),
      strip.text       = element_text(face = "bold", size = base_size - 1)
    )
}

# -- Color palette (colorblind-safe) ------------------------------------------
# Two-color scheme for treatment/control
col_treat   <- "#2166AC"
col_control <- "#B2182B"
col_fill    <- c("LL Entry" = col_treat, "No LL Entry" = col_control)

# Figure dimensions
fig_w <- 6.5
fig_h <- 4.5
fig_dpi <- 300

# Helper to save PDF + PNG
save_fig <- function(plot, name, width = fig_w, height = fig_h) {
  pdf_path <- file.path(fig_dir, paste0(name, ".pdf"))
  png_path <- file.path(fig_dir, paste0(name, ".png"))
  ggsave(pdf_path, plot, width = width, height = height,
         device = cairo_pdf)
  ggsave(png_path, plot, width = width, height = height, dpi = fig_dpi)
  message("Saved: ", pdf_path, " and ", png_path)
}

# ============================================================================
# FIGURE 1: First Stage -- LL Rate by Running Variable
# ============================================================================

fig1_data <- d |>
  filter(n_ll_slots > 0, !is.na(R_tilde), abs(R_tilde) <= 6) |>
  group_by(R_tilde) |>
  summarise(
    ll_rate = mean(got_ll, na.rm = TRUE),
    n       = n(),
    .groups = "drop"
  )

# Separate sides for polynomial fits
fig1_raw <- d |>
  filter(n_ll_slots > 0, !is.na(R_tilde), abs(R_tilde) <= 6)

p1 <- ggplot(fig1_data, aes(x = R_tilde, y = ll_rate)) +
  # Vertical cutoff line
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey40",
             linewidth = 0.5) +
  # Local polynomial fits on each side of cutoff
  geom_smooth(
    data = fig1_raw |> filter(R_tilde <= 0),
    aes(x = R_tilde, y = got_ll),
    method = "loess", formula = y ~ x, se = TRUE,
    color = col_treat, fill = col_treat, alpha = 0.15,
    linewidth = 0.8
  ) +
  geom_smooth(
    data = fig1_raw |> filter(R_tilde >= 1),
    aes(x = R_tilde, y = got_ll),
    method = "loess", formula = y ~ x, se = TRUE,
    color = col_treat, fill = col_treat, alpha = 0.15,
    linewidth = 0.8
  ) +
  # Binned means as points (sized by N)
  geom_point(aes(size = n), color = col_treat, shape = 16) +
  scale_size_continuous(range = c(2, 6), guide = "none") +
  scale_x_continuous(
    breaks = -6:6,
    labels = -6:6
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, NA)
  ) +
  labs(
    x = "Ranking Distance from LL Cutoff",
    y = "Lucky Loser Entry Rate",
    title = NULL, subtitle = NULL
  ) +
  theme_paper()

save_fig(p1, "fig1_first_stage_ll_rate")
saveRDS(fig1_data, file.path(output_dir, "fig1_data.rds"))

# ============================================================================
# FIGURE 2: RDD Plot -- Ranking Change at 26 Weeks
# ============================================================================

fig2_data <- d |>
  filter(
    n_ll_slots > 0,
    !is.na(R_tilde),
    !is.na(rank_change_26w),
    abs(R_tilde) <= 6
  ) |>
  group_by(R_tilde) |>
  summarise(
    mean_y = mean(rank_change_26w, na.rm = TRUE),
    se_y   = sd(rank_change_26w, na.rm = TRUE) / sqrt(n()),
    n      = n(),
    .groups = "drop"
  )

fig2_raw <- d |>
  filter(
    n_ll_slots > 0, !is.na(R_tilde), !is.na(rank_change_26w),
    abs(R_tilde) <= 6
  )

p2 <- ggplot(fig2_data, aes(x = R_tilde, y = mean_y)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey50",
             linewidth = 0.4) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey40",
             linewidth = 0.5) +
  # Local polynomial fits on each side
  geom_smooth(
    data = fig2_raw |> filter(R_tilde <= 0),
    aes(x = R_tilde, y = rank_change_26w),
    method = "loess", formula = y ~ x, se = TRUE,
    color = col_treat, fill = col_treat, alpha = 0.15,
    linewidth = 0.8
  ) +
  geom_smooth(
    data = fig2_raw |> filter(R_tilde >= 1),
    aes(x = R_tilde, y = rank_change_26w),
    method = "loess", formula = y ~ x, se = TRUE,
    color = col_control, fill = col_control, alpha = 0.15,
    linewidth = 0.8
  ) +
  # Binned means
  geom_point(aes(size = n), color = "grey20", shape = 16) +
  scale_size_continuous(range = c(2, 6), guide = "none") +
  scale_x_continuous(breaks = -6:6) +
  labs(
    x = "Ranking Distance from LL Cutoff",
    y = "Ranking Change at 26 Weeks (positions)",
    title = NULL, subtitle = NULL
  ) +
  theme_paper()

save_fig(p2, "fig2_rdd_rank_change_26w")
saveRDS(fig2_data, file.path(output_dir, "fig2_data.rds"))

# ============================================================================
# FIGURE 3: RDD Plot -- Elo Change at 26 Weeks
# ============================================================================

fig3_data <- d |>
  filter(
    n_ll_slots > 0,
    !is.na(R_tilde),
    !is.na(elo_change_26w),
    abs(R_tilde) <= 6
  ) |>
  group_by(R_tilde) |>
  summarise(
    mean_y = mean(elo_change_26w, na.rm = TRUE),
    se_y   = sd(elo_change_26w, na.rm = TRUE) / sqrt(n()),
    n      = n(),
    .groups = "drop"
  )

fig3_raw <- d |>
  filter(
    n_ll_slots > 0, !is.na(R_tilde), !is.na(elo_change_26w),
    abs(R_tilde) <= 6
  )

p3 <- ggplot(fig3_data, aes(x = R_tilde, y = mean_y)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey50",
             linewidth = 0.4) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey40",
             linewidth = 0.5) +
  geom_smooth(
    data = fig3_raw |> filter(R_tilde <= 0),
    aes(x = R_tilde, y = elo_change_26w),
    method = "loess", formula = y ~ x, se = TRUE,
    color = col_treat, fill = col_treat, alpha = 0.15,
    linewidth = 0.8
  ) +
  geom_smooth(
    data = fig3_raw |> filter(R_tilde >= 1),
    aes(x = R_tilde, y = elo_change_26w),
    method = "loess", formula = y ~ x, se = TRUE,
    color = col_control, fill = col_control, alpha = 0.15,
    linewidth = 0.8
  ) +
  geom_point(aes(size = n), color = "grey20", shape = 16) +
  scale_size_continuous(range = c(2, 6), guide = "none") +
  scale_x_continuous(breaks = -6:6) +
  labs(
    x = "Ranking Distance from LL Cutoff",
    y = "Elo Rating Change at 26 Weeks",
    title = NULL, subtitle = NULL
  ) +
  theme_paper()

save_fig(p3, "fig3_rdd_elo_change_26w")
saveRDS(fig3_data, file.path(output_dir, "fig3_data.rds"))

# ============================================================================
# FIGURE 4: Event Study -- Treatment Effects Over Time (Placeholder)
# ============================================================================
# Uses descriptive means by horizon as placeholder until RDD coefficients
# are available from the main estimation script.

horizons <- c(-26, -12, 0, 4, 8, 12, 26, 52)
horizon_labels <- c("-26w", "-12w", "t0", "+4w", "+8w", "+12w", "+26w", "+52w")

# Map horizons to column names for rank and elo
rank_cols <- c(
  "-26" = NA, "-12" = NA, "0" = "rank_t0",
  "4" = "rank_t4", "8" = "rank_t8", "12" = "rank_t12",
  "26" = "rank_t26", "52" = "rank_t52"
)

# Compute mean rank by treatment at each horizon (for obs near cutoff)
near_cutoff <- d |>
  filter(n_ll_slots > 0, abs(R_tilde) <= 2)

fig4_rows <- list()
for (h in c(0, 4, 8, 12, 26, 52)) {
  col_name <- paste0("rank_t", h)
  if (col_name %in% names(near_cutoff)) {
    means <- near_cutoff |>
      group_by(got_ll) |>
      summarise(
        mean_rank = mean(.data[[col_name]], na.rm = TRUE),
        n = sum(!is.na(.data[[col_name]])),
        .groups = "drop"
      )
    ll_mean <- means$mean_rank[means$got_ll == 1]
    ctrl_mean <- means$mean_rank[means$got_ll == 0]
    # Naive difference (not causal -- placeholder)
    fig4_rows[[length(fig4_rows) + 1]] <- data.frame(
      horizon = h,
      diff = ll_mean - ctrl_mean,
      # Placeholder CI based on pooled SE
      se = sd(near_cutoff[[col_name]], na.rm = TRUE) /
        sqrt(sum(!is.na(near_cutoff[[col_name]])))
    )
  }
}

fig4_data <- bind_rows(fig4_rows) |>
  mutate(
    ci_lo = diff - 1.96 * se,
    ci_hi = diff + 1.96 * se,
    horizon_label = paste0("+", horizon, "w"),
    horizon_label = ifelse(horizon == 0, "t0", horizon_label)
  )

p4 <- ggplot(fig4_data, aes(x = horizon, y = diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50",
             linewidth = 0.4) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 1.5,
                linewidth = 0.5, color = col_treat) +
  geom_point(size = 3, color = col_treat, shape = 16) +
  geom_line(linewidth = 0.4, color = col_treat, linetype = "dotted") +
  scale_x_continuous(
    breaks = c(0, 4, 8, 12, 26, 52),
    labels = c("t0", "+4w", "+8w", "+12w", "+26w", "+52w")
  ) +
  labs(
    x = "Weeks After Qualifying Loss",
    y = "Ranking Difference (LL - Control)",
    title = NULL, subtitle = NULL
  ) +
  theme_paper() +
  # Annotation that this is a placeholder
  annotate(
    "text", x = 30, y = max(fig4_data$ci_hi) * 0.9,
    label = "Placeholder: raw means, not RDD estimates",
    size = 3, family = "serif", fontface = "italic", color = "grey40"
  )

save_fig(p4, "fig4_event_study_placeholder")
saveRDS(fig4_data, file.path(output_dir, "fig4_data.rds"))

# ============================================================================
# FIGURE 5: LL Entries Over Time
# ============================================================================

# Total LL entries per year (full sample)
ll_total <- d |>
  filter(got_ll == 1) |>
  count(year, name = "total_ll")

# LL entries in the RDD sample (tournaments with qualifying data, n_ll_slots>0)
ll_rdd <- d |>
  filter(got_ll == 1, n_ll_slots > 0) |>
  count(year, name = "rdd_ll")

fig5_data <- ll_total |>
  left_join(ll_rdd, by = "year") |>
  mutate(rdd_ll = replace_na(rdd_ll, 0)) |>
  pivot_longer(
    cols = c(total_ll, rdd_ll),
    names_to = "sample",
    values_to = "count"
  ) |>
  mutate(
    sample = recode(sample,
      "total_ll" = "All LL Entries",
      "rdd_ll"   = "RDD Sample"
    )
  )

p5 <- ggplot(fig5_data, aes(x = year, y = count, color = sample,
                             shape = sample)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("All LL Entries" = col_treat,
                                "RDD Sample" = col_control)) +
  scale_shape_manual(values = c("All LL Entries" = 16,
                                "RDD Sample" = 17)) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 2)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    x = "Year",
    y = "Number of Lucky Loser Entries",
    title = NULL, subtitle = NULL
  ) +
  theme_paper() +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = -4)
  )

save_fig(p5, "fig5_ll_entries_over_time")
saveRDS(fig5_data, file.path(output_dir, "fig5_data.rds"))

# ============================================================================
# FIGURE 6: Sample Composition by Tournament Level
# ============================================================================

# Recode tournament levels for display
tourney_labels <- c(
  "G" = "Grand Slam",
  "M" = "Masters 1000",
  "A" = "ATP 250/500"
)

fig6_data <- d |>
  filter(n_ll_slots > 0) |>
  mutate(
    tourney_label = recode(tourney_level, !!!tourney_labels),
    treatment = ifelse(got_ll == 1, "Lucky Loser", "Control")
  ) |>
  count(tourney_label, treatment) |>
  # Order tournament levels by prestige
  mutate(
    tourney_label = factor(tourney_label,
      levels = c("Grand Slam", "Masters 1000", "ATP 250/500"))
  )

# Use a grouped bar chart for clarity
p6 <- ggplot(fig6_data, aes(x = tourney_label, y = n, fill = treatment)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c("Lucky Loser" = col_treat,
                               "Control" = col_control)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    x = "Tournament Level",
    y = "Number of Qualifying Losers",
    title = NULL, subtitle = NULL
  ) +
  theme_paper() +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.margin = margin(t = -4)
  )

save_fig(p6, "fig6_sample_by_tourney_level")
saveRDS(fig6_data, file.path(output_dir, "fig6_data.rds"))

# ============================================================================
# Summary
# ============================================================================
message("\n--- Figure generation complete ---")
message("Figures saved to: ", fig_dir)
message("Underlying data saved to: ", output_dir)
