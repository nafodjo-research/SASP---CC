# ==============================================================================
# win_model_helpers.R
# Shared functions for the match-winning logit used in:
#   (1) P_i^{LL} estimation (35d)
#   (2) Main-draw performance dose (36)
#   (3) Post-event delta model (37)
#
# All three uses share the SAME covariate specification.
#
# Dependencies: data.table (for Elo rolling joins)
#               The elo_cache object must exist in the calling environment.
# ==============================================================================

# --- Shared logit formula ----------------------------------------------------
# X_{ijm} (pairwise): pts_diff, elo_diff, surface_elo_diff + squares, h2h, age_diff
# Z_{ie}  (event): surface_clay, surface_grass
# Z^{pre} (focal): focal_pts, focal_elo, focal_surf_elo + squares, focal_age
LOGIT_FORMULA <- won ~ pts_diff + pts_diff_sq +
  elo_diff + elo_diff_sq + surface_elo_diff + surface_elo_diff_sq +
  h2h_win_prop + h2h_count + age_diff +
  surface_clay + surface_grass +
  focal_pts + focal_pts_sq + focal_elo + focal_elo_sq +
  focal_surf_elo + focal_surf_elo_sq + focal_age

# --- Coefficient label map (for LaTeX tables) --------------------------------
COEF_MAP <- c(
  "pts_diff"              = "Ranking points diff / 1000",
  "pts_diff_sq"           = "(Ranking points diff / 1000)$^2$",
  "elo_diff"              = "Elo difference / 100",
  "elo_diff_sq"           = "(Elo difference / 100)$^2$",
  "surface_elo_diff"      = "Surface Elo difference / 100",
  "surface_elo_diff_sq"   = "(Surface Elo difference / 100)$^2$",
  "h2h_win_prop"          = "H2H win proportion (Laplace)",
  "h2h_count"             = "H2H encounter count",
  "age_diff"              = "Age difference (years)",
  "surface_clay"          = "Clay surface",
  "surface_grass"         = "Grass surface",
  "focal_pts"             = "Ranking points / 1000",
  "focal_pts_sq"          = "(Ranking points / 1000)$^2$",
  "focal_elo"             = "Elo / 100",
  "focal_elo_sq"          = "(Elo / 100)$^2$",
  "focal_surf_elo"        = "Surface Elo / 100",
  "focal_surf_elo_sq"     = "(Surface Elo / 100)$^2$",
  "focal_age"             = "Age",
  "(Intercept)"           = "Intercept"
)

COEF_ORDER <- c("pts_diff", "pts_diff_sq",
                "elo_diff", "elo_diff_sq",
                "surface_elo_diff", "surface_elo_diff_sq",
                "h2h_win_prop", "h2h_count", "age_diff",
                "surface_clay", "surface_grass",
                "focal_pts", "focal_pts_sq", "focal_elo", "focal_elo_sq",
                "focal_surf_elo", "focal_surf_elo_sq", "focal_age", "(Intercept)")

PANEL_B_START <- "focal_pts"  # first Z^{pre} variable


# --- Elo lookup functions ----------------------------------------------------

#' Get pre-match overall Elo via rolling join (most recent before match date)
get_pre_match_elo <- function(player_ids, match_dates, elo_dt) {
  lookup <- data.table::data.table(
    player_id = player_ids,
    match_date = as.Date(match_dates),
    dummy = 1L
  )
  data.table::setkey(lookup, player_id, match_date)
  joined <- elo_dt[lookup, on = .(player_id, match_date), roll = TRUE]
  joined$elo
}

#' Get surface-specific Elo from the elo_cache environment
#' NOTE: elo_cache must exist in the calling environment or be passed explicitly
get_surface_elo <- function(player_ids, match_dates, surfaces, tour_prefix,
                            cache = elo_cache) {
  elo_vals <- rep(NA_real_, length(player_ids))
  for (i in seq_along(player_ids)) {
    surf <- tolower(surfaces[i])
    if (is.na(surf) || !surf %in% c("hard", "clay", "grass")) next
    env <- cache[[surf]]
    if (is.null(env)) next
    key <- paste0(tour_prefix, "_", player_ids[i])
    if (!exists(key, envir = env)) next
    elo_df <- get(key, envir = env)
    if (is.null(elo_df) || nrow(elo_df) == 0) next
    md <- as.Date(match_dates[i])
    elo_df$date <- as.Date(elo_df$date)
    prior <- elo_df[elo_df$date <= md, , drop = FALSE]
    if (nrow(prior) > 0) elo_vals[i] <- prior$rating[which.max(prior$date)]
  }
  elo_vals
}

#' Merge overall + surface Elo for winner and loser in a match data frame
#' Requires columns: winner_id, loser_id, tourney_date, surface
merge_elo <- function(df, elo_dt, tour_prefix, cache = elo_cache) {
  df$match_date <- as.Date(df$tourney_date)
  message("    Merging overall Elo...")
  df$winner_elo <- get_pre_match_elo(df$winner_id, df$match_date, elo_dt)
  df$loser_elo <- get_pre_match_elo(df$loser_id, df$match_date, elo_dt)
  df$winner_elo[is.na(df$winner_elo)] <- 1500
  df$loser_elo[is.na(df$loser_elo)] <- 1500

  message("    Merging surface Elo...")
  df$winner_surf_elo <- get_surface_elo(df$winner_id, df$match_date,
                                        df$surface, tour_prefix, cache)
  df$loser_surf_elo <- get_surface_elo(df$loser_id, df$match_date,
                                       df$surface, tour_prefix, cache)
  df$winner_surf_elo[is.na(df$winner_surf_elo)] <-
    df$winner_elo[is.na(df$winner_surf_elo)]
  df$loser_surf_elo[is.na(df$loser_surf_elo)] <-
    df$loser_elo[is.na(df$loser_surf_elo)]
  df
}


# --- H2H computation --------------------------------------------------------

#' Build H2H index from a raw match file (returns data.table keyed by player pair)
build_h2h_index <- function(all_match_file) {
  all_m <- readRDS(all_match_file)
  h2h_dt <- data.table::data.table(
    player_a = all_m$winner_id,
    player_b = all_m$loser_id,
    match_date = as.Date(all_m$tourney_date)
  )
  data.table::setkey(h2h_dt, player_a, player_b)
  h2h_dt[, .(dates = list(match_date)), by = .(player_a, player_b)]
}

#' Build H2H index from TWO match files (e.g., main + qual for a single tour)
build_h2h_index_multi <- function(match_files) {
  all_dts <- lapply(match_files, function(f) {
    m <- readRDS(f)
    data.table::data.table(
      player_a = m$winner_id,
      player_b = m$loser_id,
      match_date = as.Date(m$tourney_date)
    )
  })
  h2h_dt <- data.table::rbindlist(all_dts)
  data.table::setkey(h2h_dt, player_a, player_b)
  h2h_dt[, .(dates = list(match_date)), by = .(player_a, player_b)]
}

#' Compute H2H win proportion (Laplace-smoothed) and count for winner/loser pairs
compute_h2h <- function(winner_ids, loser_ids, match_dates, h2h_idx) {
  data.table::setkey(h2h_idx, player_a, player_b)
  n <- length(winner_ids)
  h2h_wins_w <- integer(n)
  h2h_wins_l <- integer(n)
  for (i in seq_len(n)) {
    if (i %% 5000 == 0) message("    H2H ", i, " / ", n)
    w <- winner_ids[i]; l <- loser_ids[i]; d <- as.Date(match_dates[i])
    row_wl <- h2h_idx[.(w, l)]
    if (!is.na(row_wl$player_a[1])) h2h_wins_w[i] <- sum(row_wl$dates[[1]] < d)
    row_lw <- h2h_idx[.(l, w)]
    if (!is.na(row_lw$player_a[1])) h2h_wins_l[i] <- sum(row_lw$dates[[1]] < d)
  }
  total <- h2h_wins_w + h2h_wins_l
  list(h2h_prop = (h2h_wins_w + 1) / (total + 2), h2h_count = total)
}


# --- Estimation data builder -------------------------------------------------

#' Build logit estimation data from a match data frame (random focal assignment)
#' Requires columns: tourney_id, winner_id, loser_id, winner_rank_points,
#'   loser_rank_points, winner_elo, loser_elo, winner_surf_elo, loser_surf_elo,
#'   winner_age, loser_age, h2h_win_prop, h2h_count, surface, weight
build_est_data <- function(df) {
  n <- nrow(df)
  flip <- rbinom(n, 1, 0.5) == 1L

  result <- data.frame(
    tourney_id = df$tourney_id,
    won = as.integer(!flip),
    weight = df$weight,
    # --- X_{ijm} pairwise ---
    pts_diff = ifelse(flip,
      (as.numeric(df$loser_rank_points) - as.numeric(df$winner_rank_points)) / 1000,
      (as.numeric(df$winner_rank_points) - as.numeric(df$loser_rank_points)) / 1000),
    elo_diff = ifelse(flip,
      (df$loser_elo - df$winner_elo) / 100,
      (df$winner_elo - df$loser_elo) / 100),
    surface_elo_diff = ifelse(flip,
      (df$loser_surf_elo - df$winner_surf_elo) / 100,
      (df$winner_surf_elo - df$loser_surf_elo) / 100),
    h2h_win_prop = ifelse(flip, 1 - df$h2h_win_prop, df$h2h_win_prop),
    h2h_count = ifelse(is.na(df$h2h_count), 0L, df$h2h_count),
    age_diff = ifelse(flip,
      as.numeric(df$loser_age) - as.numeric(df$winner_age),
      as.numeric(df$winner_age) - as.numeric(df$loser_age)),
    # --- Z_{ie} event-level ---
    surface_clay = as.integer(df$surface == "Clay"),
    surface_grass = as.integer(df$surface == "Grass"),
    # --- Z^{pre}_{ie} focal player ---
    focal_pts = ifelse(flip,
      as.numeric(df$loser_rank_points), as.numeric(df$winner_rank_points)) / 1000,
    focal_elo = ifelse(flip, df$loser_elo, df$winner_elo) / 100,
    focal_surf_elo = ifelse(flip, df$loser_surf_elo, df$winner_surf_elo) / 100,
    focal_age = ifelse(flip, as.numeric(df$loser_age), as.numeric(df$winner_age)),
    stringsAsFactors = FALSE
  )

  # Add squared terms and handle NAs
  result$pts_diff_sq <- result$pts_diff^2
  result$elo_diff_sq <- result$elo_diff^2
  result$surface_elo_diff_sq <- result$surface_elo_diff^2
  result$focal_pts_sq <- result$focal_pts^2
  result$focal_elo_sq <- result$focal_elo^2
  result$focal_surf_elo_sq <- result$focal_surf_elo^2

  result$pts_diff[is.na(result$pts_diff)] <- 0
  result$pts_diff_sq[is.na(result$pts_diff_sq)] <- 0
  result$age_diff[is.na(result$age_diff)] <- 0
  result$h2h_win_prop[is.na(result$h2h_win_prop)] <- 0.5
  result$focal_pts[is.na(result$focal_pts)] <- 0
  result$focal_pts_sq[is.na(result$focal_pts_sq)] <- 0
  result$focal_age[is.na(result$focal_age)] <- 25

  result
}


# --- Prediction function (from winner's perspective) -------------------------

#' Predict P(winner beats loser) using a fitted logit model
#' Requires columns: winner_elo, loser_elo, winner_surf_elo, loser_surf_elo,
#'   winner_rank_points, loser_rank_points, winner_age, loser_age,
#'   h2h_win_prop, h2h_count, surface
predict_match_probs <- function(df, model, default_pts = 0, default_age = 25) {
  w_pts <- ifelse(is.na(df$winner_rank_points), default_pts * 1000,
                  as.numeric(df$winner_rank_points))
  l_pts <- ifelse(is.na(df$loser_rank_points), default_pts * 1000,
                  as.numeric(df$loser_rank_points))
  pred_data <- data.frame(
    pts_diff = (w_pts - l_pts) / 1000,
    elo_diff = (df$winner_elo - df$loser_elo) / 100,
    surface_elo_diff = (df$winner_surf_elo - df$loser_surf_elo) / 100,
    h2h_win_prop = ifelse(is.na(df$h2h_win_prop), 0.5, df$h2h_win_prop),
    h2h_count = ifelse(is.na(df$h2h_count), 0L, df$h2h_count),
    age_diff = as.numeric(df$winner_age) - as.numeric(df$loser_age),
    surface_clay = as.integer(df$surface == "Clay"),
    surface_grass = as.integer(df$surface == "Grass"),
    focal_pts = w_pts / 1000,
    focal_elo = df$winner_elo / 100,
    focal_surf_elo = df$winner_surf_elo / 100,
    focal_age = ifelse(is.na(df$winner_age), default_age, as.numeric(df$winner_age)),
    stringsAsFactors = FALSE
  )
  pred_data$pts_diff_sq <- pred_data$pts_diff^2
  pred_data$elo_diff_sq <- pred_data$elo_diff^2
  pred_data$surface_elo_diff_sq <- pred_data$surface_elo_diff^2
  pred_data$focal_pts_sq <- pred_data$focal_pts^2
  pred_data$focal_elo_sq <- pred_data$focal_elo^2
  pred_data$focal_surf_elo_sq <- pred_data$focal_surf_elo^2
  pred_data$age_diff[is.na(pred_data$age_diff)] <- 0

  p <- predict(model, newdata = pred_data, type = "response")
  p[is.na(p)] <- 0.5
  p
}


# --- Weight computation ------------------------------------------------------

#' Add weights to match data: total service points (fallback: round number)
add_match_weights <- function(df) {
  df$total_pts <- df$w_svpt + df$l_svpt
  has_pts <- !is.na(df$total_pts)
  df$weight <- ifelse(has_pts, df$total_pts, df$round_num)
  df$weight <- df$weight / mean(df$weight, na.rm = TRUE)
  df$weight[is.na(df$weight)] <- 1.0
  df
}

#' Parse qualifying round number from round column
parse_round_num <- function(df) {
  df$round_num <- as.integer(gsub("Q|QF", "", df$round))
  df$round_num[df$round == "QF"] <- NA_integer_
  max_by_event <- tapply(df$round_num[!is.na(df$round_num)],
                         df$tourney_id[!is.na(df$round_num)], max)
  for (tid in names(max_by_event)) {
    idx <- df$tourney_id == tid & is.na(df$round_num)
    df$round_num[idx] <- max_by_event[tid] + 1L
  }
  df
}


# --- Table generation helpers ------------------------------------------------

#' Generate a two-column (ATP | WTA) logit coefficient table in LaTeX
generate_logit_table <- function(atp_model, wta_model, panel_title_a = "Pairwise match covariates ($X_{ijm}$)",
                                  panel_title_b = "Pre-event player characteristics ($Z^{pre}_{ie}$)") {
  atp_ct <- coef(summary(atp_model))
  wta_ct <- coef(summary(wta_model))

  lines <- character()
  lines <- c(lines, "\\begin{tabular}{lcc}")
  lines <- c(lines, "\\toprule")
  lines <- c(lines, " & ATP & WTA \\\\")
  lines <- c(lines, "\\midrule")
  lines <- c(lines, paste0("\\multicolumn{3}{l}{\\textit{Panel A: ", panel_title_a, "}} \\\\"))

  for (k in seq_along(COEF_ORDER)) {
    v <- COEF_ORDER[k]
    if (v == PANEL_B_START) {
      lines <- c(lines, "\\midrule")
      lines <- c(lines, paste0("\\multicolumn{3}{l}{\\textit{Panel B: ", panel_title_b, "}} \\\\"))
    }
    lab <- COEF_MAP[v]

    # ATP
    if (v %in% rownames(atp_ct)) {
      a_est <- fmt(atp_ct[v, "Estimate"], 4)
      a_se  <- fmt(atp_ct[v, "Std. Error"], 4)
      a_st  <- add_stars(atp_ct[v, "Pr(>|z|)"])
    } else { a_est <- ""; a_se <- ""; a_st <- "" }

    # WTA
    if (v %in% rownames(wta_ct)) {
      w_est <- fmt(wta_ct[v, "Estimate"], 4)
      w_se  <- fmt(wta_ct[v, "Std. Error"], 4)
      w_st  <- add_stars(wta_ct[v, "Pr(>|z|)"])
    } else { w_est <- ""; w_se <- ""; w_st <- "" }

    lines <- c(lines, paste0(lab, " & ", a_est, a_st, " & ", w_est, w_st, " \\\\"))
    lines <- c(lines, paste0(" & (", a_se, ") & (", w_se, ") \\\\"))
  }

  lines <- c(lines, "\\midrule")
  lines <- c(lines, paste0("Observations & ",
                           format(nobs(atp_model), big.mark = ","), " & ",
                           format(nobs(wta_model), big.mark = ","), " \\\\"))
  lines <- c(lines, paste0("AIC & ",
                           format(round(AIC(atp_model)), big.mark = ","), " & ",
                           format(round(AIC(wta_model)), big.mark = ","), " \\\\"))
  lines <- c(lines, "\\bottomrule")
  lines <- c(lines, "\\end{tabular}")

  paste(lines, collapse = "\n")
}
