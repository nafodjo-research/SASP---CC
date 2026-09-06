# ==============================================================================
# utils.R
# Shared helper functions for the Lucky Losers analysis pipeline.
# Sourced by scripts 17-20.
#
# Contents:
#   add_stars(pv)          -- significance stars for LaTeX
#   fmt(x, digits)         -- formatted number string
#   theme_paper(base_size) -- ggplot2 publication theme (serif, no title)
#   slog(msg)              -- append to summary_log and message()
#   stack_horizons(df, horizons, outcomes) -- reshape to stacked panel
#
# Dependencies: ggplot2, dplyr
# ==============================================================================

# --- Significance stars -------------------------------------------------------
add_stars <- function(pv) {
  ifelse(is.na(pv), "",
    ifelse(pv < 0.01, "$^{***}$",
      ifelse(pv < 0.05, "$^{**}$",
        ifelse(pv < 0.1, "$^{*}$", ""))))
}

# --- Number formatting --------------------------------------------------------
fmt <- function(x, d = 2) sprintf(paste0("%.", d, "f"), x)

# --- Publication ggplot theme -------------------------------------------------
theme_paper <- function(base_size = 14) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "serif") %+replace%
    ggplot2::theme(
      plot.title       = ggplot2::element_blank(),
      plot.subtitle    = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(linewidth = 0.3, color = "grey85"),
      axis.line        = ggplot2::element_line(linewidth = 0.4, color = "grey30"),
      axis.ticks       = ggplot2::element_line(linewidth = 0.3, color = "grey30"),
      axis.ticks.length = ggplot2::unit(2, "pt"),
      legend.position  = "bottom",
      legend.title     = ggplot2::element_blank(),
      legend.key.width = ggplot2::unit(18, "pt"),
      plot.margin      = ggplot2::margin(8, 12, 8, 8, unit = "pt"),
      strip.text       = ggplot2::element_text(face = "bold", size = base_size - 1)
    )
}

# --- Summary logger -----------------------------------------------------------
# NOTE: the calling script must initialise  summary_log <- character()
# in its own environment before sourcing this file.
slog <- function(...) {
  msg <- paste0(...)
  summary_log <<- c(summary_log, msg)
  message(msg)
}

# --- Stack horizons -----------------------------------------------------------
#' Reshape a cross-sectional player-event dataset into a stacked panel
#' with one row per player-event-horizon.
#'
#' @param data  Data frame with columns like points_change_4w, ..._52w
#' @param outcomes_base Character vector of outcome stems
#'        (e.g. "points_change", "n_main_draws")
#' @param horizons Numeric vector of week horizons (default 4,8,12,26,52)
#' @return A data frame with factor column `horizon` and one column per outcome
stack_horizons <- function(data, outcomes_base, horizons = c(4, 8, 12, 26, 52)) {
  stacked <- list()
  for (h in horizons) {
    h_label <- paste0(h, "w")
    row_data <- data |>
      dplyr::transmute(
        player_id, tourney_id, tour, slam_year, got_ll,
        pre_rank_pts, pre_rank_pts_sq, player_age, had_prior_ll,
        pre_elo   = if ("pre_elo"   %in% names(data)) pre_elo   else NA_real_,
        pre_elo_sq = if ("pre_elo_sq" %in% names(data)) pre_elo_sq else NA_real_,
        md_matches_won = if ("md_matches_won" %in% names(data)) md_matches_won else NA_integer_,
        n_prior_gs_ll_won = if ("n_prior_gs_ll_won" %in% names(data)) n_prior_gs_ll_won else NA_integer_,
        rank_among_losers = if ("rank_among_losers" %in% names(data)) rank_among_losers else NA_integer_,
        peer_component = if ("peer_component" %in% names(data)) peer_component else NA_real_,
        year = if ("year" %in% names(data)) year else NA_integer_,
        horizon = h_label,
        horizon_num = h
      )
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
  dplyr::bind_rows(stacked) |>
    dplyr::mutate(horizon = factor(horizon, levels = paste0(horizons, "w")))
}

# --- Colour palette -----------------------------------------------------------
col_treat   <- "#E69F00"
col_control <- "#56B4E9"
