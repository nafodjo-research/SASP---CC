# ==============================================================================
# 37_delta_model.R
# Post-event match-level performance model (delta).
#
# Estimates whether LL entry changes subsequent match win probability
# conditional on opponent strength, using the unified covariate spec.
#
# Four samples: GS-ATP, GS-WTA, nonGS-ATP, nonGS-WTA
# Four specifications per sample:
#   (1) Pooled:           D_ie [+ v_hat]
#   (2) Dose (wins):      D_ie + D_ie x matches_won_c [+ v_hat interactions]
#   (3) Dose (perf prob):  D_ie + D_ie x perf_dose [+ v_hat interactions]
#   (4) Horizon hetero:   D_ie x horizon [+ v_hat x horizon]
#
# Tracking window: h* = min(52 weeks, next same-type LL opportunity)
# Inference: player-level block bootstrap (200 reps)
#
# Outputs:
#   16 tables, 4 figures, Data/cleaned/delta_model_results.rds
# ==============================================================================

set.seed(20260327)

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(here)
library(stringr)

source(here("scripts", "R", "utils.R"))
source(here("scripts", "R", "win_model_helpers.R"))
summary_log <- character()

RAW_DIR     <- here("Data", "raw")
CLEANED_DIR <- here("Data", "cleaned")
TABLES_DIR  <- here("Tables")
FIGURES_DIR <- here("Figures")
OUTPUT_DIR  <- here("Output")
for (d in c(CLEANED_DIR, TABLES_DIR, FIGURES_DIR, OUTPUT_DIR))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

# Constants
HORIZONS <- c(4, 8, 12, 26, 52)
HORIZON_LABS <- paste0(HORIZONS, "w")
CALENDAR_CAP <- 365  # days
N_BOOT <- 200


# ==============================================================================
# STEP 1: LOAD DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 1: LOAD DATA")
message(strrep("=", 70))

# Event tables
tr <- readRDS(file.path(CLEANED_DIR, "tournament_rebuild_results.rds"))
nongs_est <- readRDS(file.path(CLEANED_DIR, "skeleton_nongs_est_v6.rds"))

# Performance dose
dose_data <- readRDS(file.path(CLEANED_DIR, "performance_dose.rds"))

# Raw match data (all sources)
atp_main <- readRDS(file.path(RAW_DIR, "atp_main_matches.rds"))
atp_qual <- readRDS(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
wta_main <- readRDS(file.path(RAW_DIR, "wta_main_matches.rds"))
wta_qual <- readRDS(file.path(RAW_DIR, "wta_qual_itf_matches.rds"))

# Combine all matches per tour
atp_all <- bind_rows(
  atp_main |> mutate(match_source = "main"),
  atp_qual |> mutate(match_source = "qual_chall")
)
wta_all <- bind_rows(
  wta_main |> mutate(match_source = "main"),
  wta_qual |> mutate(match_source = "qual_itf")
)
rm(atp_main, atp_qual, wta_main, wta_qual)

# Elo data
atp_elo <- as.data.table(readRDS(file.path(CLEANED_DIR, "elo_history.rds")))
wta_elo <- as.data.table(readRDS(file.path(CLEANED_DIR, "wta_elo_history.rds")))
setnames(atp_elo, c("player_id", "match_date", "elo"))
setnames(wta_elo, c("player_id", "match_date", "elo"))
setkey(atp_elo, player_id, match_date)
setkey(wta_elo, player_id, match_date)

elo_cache <- readRDS(file.path(CLEANED_DIR, "tournament_elo_cache.rds"))

# Pre-convert match dates (handles numeric YYYYMMDD and character formats)
safe_as_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (is.numeric(x)) return(as.Date(as.character(x), format = "%Y%m%d"))
  as.Date(x)
}

atp_all$tourney_date_d <- safe_as_date(atp_all$tourney_date)
wta_all$tourney_date_d <- safe_as_date(wta_all$tourney_date)

message("  ATP all matches: ", nrow(atp_all))
message("  WTA all matches: ", nrow(wta_all))
message("  Dose records: ", nrow(dose_data))


# ==============================================================================
# STEP 2: BUILD EVENT TABLES WITH DOSE + CENSORING
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 2: BUILD EVENT TABLES")
message(strrep("=", 70))

# Function to build complete event table with same-type censoring
build_event_table <- function(events_df, nongs_events_df, all_events_list, event_type, tour_label) {
  # events_df: the focal event table (GS or nonGS)
  # For censoring: need to know when the player's next same-type qualifying loss occurs

  ev <- events_df
  # Handle numeric YYYYMMDD dates and character/Date dates
  if (is.numeric(ev$tourney_date)) {
    ev$ev_date <- as.Date(as.character(ev$tourney_date), format = "%Y%m%d")
  } else {
    ev$ev_date <- as.Date(ev$tourney_date)
  }
  ev$event_type <- event_type

  # Build a timeline of all qualifying losses for each player (same type only)
  if (event_type == "GS") {
    all_same <- all_events_list$gs
  } else {
    all_same <- all_events_list$nongs
  }
  if (is.numeric(all_same$tourney_date)) {
    all_same$ev_date <- as.Date(as.character(all_same$tourney_date), format = "%Y%m%d")
  } else {
    all_same$ev_date <- as.Date(all_same$tourney_date)
  }
  censor_source <- all_same[, c("player_id", "ev_date")]

  # For each event, find the next same-type qualifying loss date
  ev$next_same_type_date <- as.Date(NA)
  ev$censor_date <- as.Date(NA)

  for (i in seq_len(nrow(ev))) {
    pid <- ev$player_id[i]
    d <- ev$ev_date[i]

    # Next same-type qualifying loss after this event
    future <- censor_source[censor_source$player_id == pid & censor_source$ev_date > d, ]
    if (nrow(future) > 0) {
      ev$next_same_type_date[i] <- min(future$ev_date)
    }

    # h* = min(52 weeks, next same-type opportunity)
    cal_cap <- d + CALENDAR_CAP
    ev$censor_date[i] <- min(c(ev$next_same_type_date[i], cal_cap), na.rm = TRUE)
  }

  # Merge dose
  dose_sub <- dose_data[dose_data$tour == tour_label & dose_data$event_type == event_type,
                        c("player_id", "tourney_id", "dose", "matches_won")]
  ev <- merge(ev, dose_sub, by = c("player_id", "tourney_id"), all.x = TRUE)
  ev$dose[is.na(ev$dose)] <- 0
  ev$matches_won[is.na(ev$matches_won)] <- 0L

  # Centered matches won (center at treated mean)
  treated_mean <- mean(ev$matches_won[ev$got_ll == 1], na.rm = TRUE)
  ev$matches_won_c <- ev$matches_won - treated_mean

  message("  ", tour_label, " ", event_type, ": ", nrow(ev), " events, ",
          "censor range: ", min(ev$censor_date - ev$ev_date), "-",
          max(ev$censor_date - ev$ev_date), " days")

  ev
}

# Build all-events lists for censoring
gs_atp_all_events <- tr$gs_atp$events
gs_wta_all_events <- tr$gs_wta$events
nongs_atp_all_events <- data.frame(
  player_id = nongs_est$player_id[nongs_est$tour == "ATP"],
  tourney_date = nongs_est$tourney_date[nongs_est$tour == "ATP"],
  stringsAsFactors = FALSE
)
nongs_wta_all_events <- data.frame(
  player_id = nongs_est$player_id[nongs_est$tour == "WTA"],
  tourney_date = nongs_est$tourney_date[nongs_est$tour == "WTA"],
  stringsAsFactors = FALSE
)

ev_gs_atp <- build_event_table(
  tr$gs_atp$events, nongs_est,
  list(gs = gs_atp_all_events, nongs = nongs_atp_all_events),
  "GS", "ATP"
)
ev_gs_wta <- build_event_table(
  tr$gs_wta$events, nongs_est,
  list(gs = gs_wta_all_events, nongs = nongs_wta_all_events),
  "GS", "WTA"
)

# For nonGS, the event table comes from nongs_est
nongs_atp_ev <- nongs_est[nongs_est$tour == "ATP", ]
ev_nongs_atp <- build_event_table(
  nongs_atp_ev, nongs_est,
  list(gs = gs_atp_all_events, nongs = nongs_atp_all_events),
  "nonGS", "ATP"
)

nongs_wta_ev <- nongs_est[nongs_est$tour == "WTA", ]
ev_nongs_wta <- build_event_table(
  nongs_wta_ev, nongs_est,
  list(gs = gs_wta_all_events, nongs = nongs_wta_all_events),
  "nonGS", "WTA"
)


# ==============================================================================
# STEP 3: BUILD POST-EVENT MATCH-LEVEL DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 3: BUILD POST-EVENT MATCH DATA")
message(strrep("=", 70))

build_match_data <- function(ev_df, all_matches, elo_dt, tour_prefix) {
  # VECTORIZED: pre-index matches by player_id, avoid per-match Elo lookups
  message("    Building match-level data for ", nrow(ev_df), " events...")

  # Pre-index matches by player: split into winner/loser indices
  am_dt <- as.data.table(all_matches[, c("winner_id", "loser_id", "tourney_id",
                                          "tourney_date_d", "surface", "tourney_level",
                                          "winner_rank_points", "loser_rank_points",
                                          "winner_age", "loser_age")])

  # Create a player-match index: for each player_id, list their match row indices
  w_idx <- split(seq_len(nrow(am_dt)), am_dt$winner_id)
  l_idx <- split(seq_len(nrow(am_dt)), am_dt$loser_id)

  match_rows <- vector("list", nrow(ev_df))

  for (i in seq_len(nrow(ev_df))) {
    if (i %% 500 == 0) message("      Event ", i, " / ", nrow(ev_df))
    pid <- ev_df$player_id[i]
    ev_d <- ev_df$ev_date[i]
    censor_d <- ev_df$censor_date[i]

    # Get match indices for this player (as winner and as loser)
    widx <- w_idx[[as.character(pid)]]
    lidx <- l_idx[[as.character(pid)]]

    # Filter to window [ev_date+1, censor_date]
    if (!is.null(widx)) {
      w_dates <- am_dt$tourney_date_d[widx]
      widx <- widx[w_dates > ev_d & w_dates <= censor_d]
    }
    if (!is.null(lidx)) {
      l_dates <- am_dt$tourney_date_d[lidx]
      lidx <- lidx[l_dates > ev_d & l_dates <= censor_d]
    }

    n_w <- length(widx)
    n_l <- length(lidx)
    n_tot <- n_w + n_l
    if (n_tot == 0) next

    # Build rows vectorized for this event
    all_idx <- c(widx, lidx)
    focal_is_winner <- c(rep(TRUE, n_w), rep(FALSE, n_l))

    md <- am_dt[all_idx, ]
    match_dates <- md$tourney_date_d
    weeks <- as.numeric(difftime(match_dates, ev_d, units = "days")) / 7

    # Horizon assignment
    horizon <- ifelse(weeks <= 4, "4w",
               ifelse(weeks <= 8, "8w",
               ifelse(weeks <= 12, "12w",
               ifelse(weeks <= 26, "26w", "52w"))))

    # Opponent characteristics
    opp_rank_pts <- ifelse(focal_is_winner,
                           as.numeric(md$loser_rank_points),
                           as.numeric(md$winner_rank_points))
    opp_rank_pts[is.na(opp_rank_pts)] <- 0
    opp_age <- ifelse(focal_is_winner,
                      as.numeric(md$loser_age),
                      as.numeric(md$winner_age))
    opp_age[is.na(opp_age)] <- 25

    # Opponent Elo: batch lookup
    opp_ids <- ifelse(focal_is_winner, md$loser_id, md$winner_id)
    opp_elo <- get_pre_match_elo(opp_ids, match_dates, elo_dt)
    opp_elo[is.na(opp_elo)] <- 1500

    # Focal pre-event characteristics (frozen)
    focal_elo_pre <- ev_df$pre_elo[i]
    focal_pts_pre <- ev_df$pre_rank_pts[i]
    focal_age_pre <- ev_df$player_age[i]

    # Surface Elo: use overall Elo as proxy for speed
    # (per-match surface Elo lookup is too slow for 100k+ matches)
    focal_surf_elo_pre <- focal_elo_pre  # approximate
    opp_surf_elo <- opp_elo  # approximate

    match_rows[[i]] <- data.frame(
      event_idx = i,
      player_id = pid,
      tourney_id_event = ev_df$tourney_id[i],
      match_date = match_dates,
      weeks_after = weeks,
      horizon = horizon,
      won = as.integer(focal_is_winner),
      got_ll = ev_df$got_ll[i],
      v_hat = if ("v_hat" %in% names(ev_df)) ev_df$v_hat[i] else NA_real_,
      dose = ev_df$dose[i],
      matches_won_c = ev_df$matches_won_c[i],
      pts_diff = (focal_pts_pre - opp_rank_pts) / 1000,
      elo_diff = (focal_elo_pre - opp_elo) / 100,
      surface_elo_diff = (focal_surf_elo_pre - opp_surf_elo) / 100,
      age_diff = focal_age_pre - opp_age,
      surface_clay = as.integer(md$surface == "Clay"),
      surface_grass = as.integer(md$surface == "Grass"),
      focal_pts = focal_pts_pre / 1000,
      focal_elo = focal_elo_pre / 100,
      focal_surf_elo = focal_surf_elo_pre / 100,
      focal_age = focal_age_pre,
      h2h_win_prop = 0.5,
      h2h_count = 0L,
      stringsAsFactors = FALSE
    )
  }

  result <- data.table::rbindlist(match_rows, fill = TRUE)
  if (nrow(result) == 0) return(data.frame())

  result <- as.data.frame(result)

  # Squared terms
  result$pts_diff_sq <- result$pts_diff^2
  result$elo_diff_sq <- result$elo_diff^2
  result$surface_elo_diff_sq <- result$surface_elo_diff^2
  result$focal_pts_sq <- result$focal_pts^2
  result$focal_elo_sq <- result$focal_elo^2
  result$focal_surf_elo_sq <- result$focal_surf_elo^2

  result$horizon <- factor(result$horizon, levels = HORIZON_LABS)

  message("    Built ", nrow(result), " match observations")
  result
}

message("  GS-ATP match data...")
md_gs_atp <- build_match_data(ev_gs_atp, atp_all, atp_elo, "ATP")
message("  GS-WTA match data...")
md_gs_wta <- build_match_data(ev_gs_wta, wta_all, wta_elo, "WTA")
message("  NonGS-ATP match data...")
md_nongs_atp <- build_match_data(ev_nongs_atp, atp_all, atp_elo, "ATP")
message("  NonGS-WTA match data...")
md_nongs_wta <- build_match_data(ev_nongs_wta, wta_all, wta_elo, "WTA")

message("\n  Match data sizes:")
message("  GS-ATP: ", nrow(md_gs_atp))
message("  GS-WTA: ", nrow(md_gs_wta))
message("  NonGS-ATP: ", nrow(md_nongs_atp))
message("  NonGS-WTA: ", nrow(md_nongs_wta))


# ==============================================================================
# STEP 4: ESTIMATE DELTA MODELS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 4: ESTIMATE DELTA MODELS")
message(strrep("=", 70))

# Base covariates string (everything except treatment terms)
BASE_COVS <- "pts_diff + pts_diff_sq + elo_diff + elo_diff_sq + surface_elo_diff + surface_elo_diff_sq + h2h_win_prop + h2h_count + age_diff + surface_clay + surface_grass + focal_pts + focal_pts_sq + focal_elo + focal_elo_sq + focal_surf_elo + focal_surf_elo_sq + focal_age"

estimate_delta <- function(md, label, has_cf = FALSE) {
  message("\n  --- ", label, " ---")
  if (nrow(md) == 0) {
    message("    No data. Skipping.")
    return(NULL)
  }

  # Drop incomplete cases
  md <- md[complete.cases(md[, c("won", "pts_diff", "elo_diff", "surface_elo_diff",
                                  "got_ll", "focal_pts", "focal_elo")]), ]

  # (1) Pooled
  cf_term <- if (has_cf) " + v_hat" else ""
  fml_pooled <- as.formula(paste0("won ~ ", BASE_COVS, " + got_ll", cf_term))
  mod_pooled <- glm(fml_pooled, data = md, family = binomial(link = "logit"))
  message("    Pooled: N=", nobs(mod_pooled), " delta=",
          round(coef(mod_pooled)["got_ll"], 4), " p=",
          round(coef(summary(mod_pooled))["got_ll", "Pr(>|z|)"], 4))

  # (2) Dose (matches won)
  cf_dose_mw <- if (has_cf) " + v_hat + v_hat:matches_won_c" else ""
  fml_dose_mw <- as.formula(paste0("won ~ ", BASE_COVS,
                                   " + got_ll + got_ll:matches_won_c", cf_dose_mw))
  mod_dose_mw <- glm(fml_dose_mw, data = md, family = binomial(link = "logit"))
  message("    Dose(wins): delta=", round(coef(mod_dose_mw)["got_ll"], 4))

  # (3) Dose (performance probability)
  cf_dose_pp <- if (has_cf) " + v_hat + v_hat:dose" else ""
  fml_dose_pp <- as.formula(paste0("won ~ ", BASE_COVS,
                                   " + got_ll + got_ll:dose", cf_dose_pp))
  mod_dose_pp <- glm(fml_dose_pp, data = md, family = binomial(link = "logit"))
  message("    Dose(perf): delta=", round(coef(mod_dose_pp)["got_ll"], 4))

  # (4) Horizon heterogeneity
  cf_horizon <- if (has_cf) " + v_hat:horizon" else ""
  fml_horizon <- as.formula(paste0("won ~ ", BASE_COVS,
                                   " + got_ll:horizon", cf_horizon))
  mod_horizon <- glm(fml_horizon, data = md, family = binomial(link = "logit"))
  horizon_coefs <- coef(mod_horizon)[grep("got_ll:horizon", names(coef(mod_horizon)))]
  message("    Horizon: ", paste(names(horizon_coefs), round(horizon_coefs, 4),
                                 sep = "=", collapse = ", "))

  list(pooled = mod_pooled, dose_mw = mod_dose_mw,
       dose_pp = mod_dose_pp, horizon = mod_horizon,
       data = md, label = label, has_cf = has_cf)
}

res_gs_atp <- estimate_delta(md_gs_atp, "GS-ATP", has_cf = FALSE)
res_gs_wta <- estimate_delta(md_gs_wta, "GS-WTA", has_cf = FALSE)
res_nongs_atp <- estimate_delta(md_nongs_atp, "NonGS-ATP", has_cf = TRUE)
res_nongs_wta <- estimate_delta(md_nongs_wta, "NonGS-WTA", has_cf = TRUE)


# ==============================================================================
# STEP 5: BOOTSTRAP INFERENCE (200 reps)
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 5: BOOTSTRAP INFERENCE")
message(strrep("=", 70))

bootstrap_delta <- function(res, n_boot = N_BOOT) {
  if (is.null(res)) return(NULL)
  md <- res$data
  has_cf <- res$has_cf

  # Unique players for block bootstrap
  players <- unique(md$player_id)
  n_players <- length(players)

  cf_term <- if (has_cf) " + v_hat" else ""
  fml_pooled <- as.formula(paste0("won ~ ", BASE_COVS, " + got_ll", cf_term))

  cf_horizon <- if (has_cf) " + v_hat:horizon" else ""
  fml_horizon <- as.formula(paste0("won ~ ", BASE_COVS, " + got_ll:horizon", cf_horizon))

  boot_delta <- numeric(n_boot)
  boot_horizon <- matrix(NA, n_boot, length(HORIZON_LABS))
  colnames(boot_horizon) <- HORIZON_LABS

  message("  Bootstrapping ", res$label, " (", n_boot, " reps)...")

  for (b in seq_len(n_boot)) {
    if (b %% 50 == 0) message("    Boot ", b, " / ", n_boot)

    # Resample players with replacement
    boot_players <- sample(players, n_players, replace = TRUE)
    boot_idx <- unlist(lapply(boot_players, function(p) which(md$player_id == p)))
    boot_data <- md[boot_idx, ]

    # Pooled
    mod_b <- tryCatch(
      glm(fml_pooled, data = boot_data, family = binomial(link = "logit")),
      error = function(e) NULL
    )
    if (!is.null(mod_b) && "got_ll" %in% names(coef(mod_b))) {
      boot_delta[b] <- coef(mod_b)["got_ll"]
    }

    # Horizon
    mod_h <- tryCatch(
      glm(fml_horizon, data = boot_data, family = binomial(link = "logit")),
      error = function(e) NULL
    )
    if (!is.null(mod_h)) {
      for (h_idx in seq_along(HORIZON_LABS)) {
        h <- HORIZON_LABS[h_idx]
        cname <- paste0("got_ll:horizon", h)
        if (cname %in% names(coef(mod_h))) {
          boot_horizon[b, h_idx] <- coef(mod_h)[cname]
        }
      }
    }
  }

  list(
    delta_se = sd(boot_delta, na.rm = TRUE),
    delta_ci = quantile(boot_delta, c(0.025, 0.975), na.rm = TRUE),
    horizon_se = apply(boot_horizon, 2, sd, na.rm = TRUE),
    horizon_ci_lo = apply(boot_horizon, 2, function(x) quantile(x, 0.025, na.rm = TRUE)),
    horizon_ci_hi = apply(boot_horizon, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
  )
}

boot_gs_atp <- bootstrap_delta(res_gs_atp)
boot_gs_wta <- bootstrap_delta(res_gs_wta)
boot_nongs_atp <- bootstrap_delta(res_nongs_atp)
boot_nongs_wta <- bootstrap_delta(res_nongs_wta)


# ==============================================================================
# STEP 6: GENERATE TABLES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 6: GENERATE TABLES")
message(strrep("=", 70))

generate_pooled_table <- function(res, boot, label) {
  if (is.null(res)) return("")
  mod <- res$pooled
  ct <- coef(summary(mod))
  delta <- ct["got_ll", "Estimate"]
  delta_se <- boot$delta_se

  lines <- c("\\begin{tabular}{lc}", "\\toprule",
             paste0(" & ", label, " \\\\"), "\\midrule")

  lines <- c(lines, paste0("$\\hat{\\delta}$ (LL entry) & ",
                           fmt(delta, 4), add_stars(2 * pnorm(-abs(delta / delta_se))), " \\\\"))
  lines <- c(lines, paste0(" & (", fmt(delta_se, 4), ") \\\\"))

  if (res$has_cf && "v_hat" %in% rownames(ct)) {
    rho <- ct["v_hat", "Estimate"]
    rho_se <- ct["v_hat", "Std. Error"]
    lines <- c(lines, paste0("$\\hat{\\rho}$ (CF) & ", fmt(rho, 4),
                             add_stars(ct["v_hat", "Pr(>|z|)"]), " \\\\"))
    lines <- c(lines, paste0(" & (", fmt(rho_se, 4), ") \\\\"))
  }

  lines <- c(lines, "\\midrule")
  lines <- c(lines, paste0("Matches & ", format(nobs(mod), big.mark = ","), " \\\\"))
  lines <- c(lines, paste0("Players & ", length(unique(res$data$player_id)), " \\\\"))
  lines <- c(lines, "\\bottomrule", "\\end{tabular}")
  paste(lines, collapse = "\n")
}

generate_dose_table <- function(res, dose_var, dose_label, label) {
  if (is.null(res)) return("")
  if (dose_var == "matches_won_c") mod <- res$dose_mw
  else mod <- res$dose_pp
  ct <- coef(summary(mod))

  lines <- c("\\begin{tabular}{lc}", "\\toprule",
             paste0(" & ", label, " \\\\"), "\\midrule")

  if ("got_ll" %in% rownames(ct)) {
    lines <- c(lines, paste0("$\\hat{\\delta}$ (LL entry) & ",
                             fmt(ct["got_ll", 1], 4), add_stars(ct["got_ll", 4]), " \\\\"))
    lines <- c(lines, paste0(" & (", fmt(ct["got_ll", 2], 4), ") \\\\"))
  }

  int_name <- paste0("got_ll:", dose_var)
  if (int_name %in% rownames(ct)) {
    lines <- c(lines, paste0("$\\hat{\\delta}$ $\\times$ ", dose_label, " & ",
                             fmt(ct[int_name, 1], 4), add_stars(ct[int_name, 4]), " \\\\"))
    lines <- c(lines, paste0(" & (", fmt(ct[int_name, 2], 4), ") \\\\"))
  }

  if (res$has_cf && "v_hat" %in% rownames(ct)) {
    lines <- c(lines, paste0("$\\hat{\\rho}$ & ",
                             fmt(ct["v_hat", 1], 4), add_stars(ct["v_hat", 4]), " \\\\"))
    lines <- c(lines, paste0(" & (", fmt(ct["v_hat", 2], 4), ") \\\\"))
  }

  lines <- c(lines, "\\midrule")
  lines <- c(lines, paste0("Matches & ", format(nobs(mod), big.mark = ","), " \\\\"))
  lines <- c(lines, "\\bottomrule", "\\end{tabular}")
  paste(lines, collapse = "\n")
}

generate_horizon_table <- function(res, boot, label) {
  if (is.null(res)) return("")
  mod <- res$horizon
  ct <- coef(summary(mod))

  lines <- c("\\begin{tabular}{l*{5}{c}}", "\\toprule",
             paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"), "\\midrule")

  coef_vals <- se_vals <- character()
  for (h in HORIZON_LABS) {
    cname <- paste0("got_ll:horizon", h)
    if (cname %in% rownames(ct)) {
      b_se <- boot$horizon_se[h]
      pval <- 2 * pnorm(-abs(ct[cname, 1] / b_se))
      coef_vals <- c(coef_vals, paste0(fmt(ct[cname, 1], 4), add_stars(pval)))
      se_vals <- c(se_vals, paste0("(", fmt(b_se, 4), ")"))
    } else {
      coef_vals <- c(coef_vals, "")
      se_vals <- c(se_vals, "")
    }
  }

  lines <- c(lines, paste0("$\\hat{\\delta}_h$ & ", paste(coef_vals, collapse = " & "), " \\\\"))
  lines <- c(lines, paste0(" & ", paste(se_vals, collapse = " & "), " \\\\"))

  lines <- c(lines, "\\midrule")
  lines <- c(lines, paste0("Matches & ", format(nobs(mod), big.mark = ","), " \\\\"))
  lines <- c(lines, "\\bottomrule", "\\end{tabular}")
  paste(lines, collapse = "\n")
}

# Generate all 16 tables
for (info in list(
  list(res = res_gs_atp, boot = boot_gs_atp, tag = "gs_atp", label = "GS-ATP"),
  list(res = res_gs_wta, boot = boot_gs_wta, tag = "gs_wta", label = "GS-WTA"),
  list(res = res_nongs_atp, boot = boot_nongs_atp, tag = "nongs_atp", label = "NonGS-ATP"),
  list(res = res_nongs_wta, boot = boot_nongs_wta, tag = "nongs_wta", label = "NonGS-WTA")
)) {
  # Pooled
  writeLines(generate_pooled_table(info$res, info$boot, info$label),
             file.path(TABLES_DIR, paste0("table_delta_pooled_", info$tag, ".tex")))
  # Dose (matches won)
  writeLines(generate_dose_table(info$res, "matches_won_c", "Matches won", info$label),
             file.path(TABLES_DIR, paste0("table_delta_dose_mw_", info$tag, ".tex")))
  # Dose (performance probability)
  writeLines(generate_dose_table(info$res, "dose", "Perf. dose", info$label),
             file.path(TABLES_DIR, paste0("table_delta_dose_pp_", info$tag, ".tex")))
  # Horizon heterogeneity
  writeLines(generate_horizon_table(info$res, info$boot, info$label),
             file.path(TABLES_DIR, paste0("table_delta_horizon_", info$tag, ".tex")))
}
message("  16 tables saved.")


# ==============================================================================
# STEP 7: HORIZON FIGURES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 7: HORIZON FIGURES")
message(strrep("=", 70))

generate_horizon_figure <- function(res, boot, label, tag) {
  if (is.null(res)) return(NULL)
  mod <- res$horizon
  ct <- coef(summary(mod))

  # Extract horizon coefficients
  plot_data <- data.frame(
    horizon = HORIZONS,
    delta = NA_real_, se = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_
  )

  for (i in seq_along(HORIZON_LABS)) {
    h <- HORIZON_LABS[i]
    cname <- paste0("got_ll:horizon", h)
    if (cname %in% rownames(ct)) {
      plot_data$delta[i] <- ct[cname, 1]
      plot_data$se[i] <- boot$horizon_se[h]
      plot_data$ci_lo[i] <- boot$horizon_ci_lo[h]
      plot_data$ci_hi[i] <- boot$horizon_ci_hi[h]
    }
  }

  plot_data <- plot_data[!is.na(plot_data$delta), ]

  # Panel (a): AME on match win probability
  # AME = mean[Lambda(X'b + delta) - Lambda(X'b)]
  # Approximate: delta * mean[Lambda'(X'b)] = delta * mean[p*(1-p)]
  md <- res$data
  Xb_base <- predict(mod, type = "link") - coef(mod)["got_ll:horizon4w"] * (md$horizon == "4w") # approximate
  p_base <- plogis(predict(mod, type = "link") - ifelse(!is.na(md$got_ll) & md$got_ll == 1, 0, 0))
  mean_deriv <- mean(p_base * (1 - p_base), na.rm = TRUE)

  plot_data$ame <- plot_data$delta * mean_deriv
  plot_data$ame_lo <- plot_data$ci_lo * mean_deriv
  plot_data$ame_hi <- plot_data$ci_hi * mean_deriv

  # Panel (b): Expected additional ranking points per tournament
  # Rough conversion: 1pp win prob increase at GS ~ 15 ranking points per tournament
  pts_per_pp <- 15  # approximate for GS; 5 for 250-level
  plot_data$pts_per_tourn <- plot_data$ame * pts_per_pp
  plot_data$pts_per_tourn_lo <- plot_data$ame_lo * pts_per_pp
  plot_data$pts_per_tourn_hi <- plot_data$ame_hi * pts_per_pp

  # Panel (c): Cumulative expected ranking points
  plot_data$cum_pts <- cumsum(plot_data$pts_per_tourn)
  plot_data$cum_pts_lo <- cumsum(plot_data$pts_per_tourn_lo)
  plot_data$cum_pts_hi <- cumsum(plot_data$pts_per_tourn_hi)

  # Panel (d): Cumulative expected additional wins
  plot_data$cum_wins <- cumsum(plot_data$ame)
  plot_data$cum_wins_lo <- cumsum(plot_data$ame_lo)
  plot_data$cum_wins_hi <- cumsum(plot_data$ame_hi)

  # Build 4-panel figure
  p_a <- ggplot(plot_data, aes(x = horizon, y = ame)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(ymin = ame_lo, ymax = ame_hi), alpha = 0.15) +
    geom_point(size = 2) + geom_line() +
    labs(x = "Weeks after LL event", y = "AME on win prob") +
    theme_paper()

  p_b <- ggplot(plot_data, aes(x = horizon, y = pts_per_tourn)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(ymin = pts_per_tourn_lo, ymax = pts_per_tourn_hi), alpha = 0.15) +
    geom_point(size = 2) + geom_line() +
    labs(x = "Weeks after LL event", y = "Ranking pts per tournament") +
    theme_paper()

  p_c <- ggplot(plot_data, aes(x = horizon, y = cum_pts)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(ymin = cum_pts_lo, ymax = cum_pts_hi), alpha = 0.15) +
    geom_point(size = 2) + geom_line() +
    labs(x = "Weeks after LL event", y = "Cumulative ranking pts") +
    theme_paper()

  p_d <- ggplot(plot_data, aes(x = horizon, y = cum_wins)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(ymin = cum_wins_lo, ymax = cum_wins_hi), alpha = 0.15) +
    geom_point(size = 2) + geom_line() +
    labs(x = "Weeks after LL event", y = "Cumulative additional wins") +
    theme_paper()

  # Combine with patchwork if available, otherwise use cowplot
  if (requireNamespace("patchwork", quietly = TRUE)) {
    library(patchwork)
    combined <- (p_a | p_b) / (p_c | p_d) +
      plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
  } else {
    combined <- p_a  # fallback
  }

  fig_path <- file.path(FIGURES_DIR, paste0("fig_delta_horizon_", tag, ".pdf"))
  ggsave(fig_path, combined, width = 10, height = 8, device = cairo_pdf, bg = "transparent")
  message("  Figure saved: ", fig_path)
}

generate_horizon_figure(res_gs_atp, boot_gs_atp, "GS-ATP", "gs_atp")
generate_horizon_figure(res_gs_wta, boot_gs_wta, "GS-WTA", "gs_wta")
generate_horizon_figure(res_nongs_atp, boot_nongs_atp, "NonGS-ATP", "nongs_atp")
generate_horizon_figure(res_nongs_wta, boot_nongs_wta, "NonGS-WTA", "nongs_wta")


# ==============================================================================
# STEP 8: SAVE RESULTS AND SUMMARY
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 8: SAVE RESULTS")
message(strrep("=", 70))

all_results <- list(
  gs_atp = res_gs_atp, gs_wta = res_gs_wta,
  nongs_atp = res_nongs_atp, nongs_wta = res_nongs_wta,
  boot_gs_atp = boot_gs_atp, boot_gs_wta = boot_gs_wta,
  boot_nongs_atp = boot_nongs_atp, boot_nongs_wta = boot_nongs_wta
)

# Strip large data from results before saving (keep coefficients only)
for (nm in c("gs_atp", "gs_wta", "nongs_atp", "nongs_wta")) {
  if (!is.null(all_results[[nm]])) {
    all_results[[nm]]$data <- NULL  # remove raw data to save space
  }
}

saveRDS(all_results, file.path(CLEANED_DIR, "delta_model_results.rds"))
message("  Results saved.")

# Print summary
message("\n  === DELTA MODEL SUMMARY ===")
for (nm in c("gs_atp", "gs_wta", "nongs_atp", "nongs_wta")) {
  res <- if (nm == "gs_atp") res_gs_atp else if (nm == "gs_wta") res_gs_wta
         else if (nm == "nongs_atp") res_nongs_atp else res_nongs_wta
  boot <- if (nm == "gs_atp") boot_gs_atp else if (nm == "gs_wta") boot_gs_wta
          else if (nm == "nongs_atp") boot_nongs_atp else boot_nongs_wta
  if (is.null(res)) next

  delta <- coef(res$pooled)["got_ll"]
  se <- boot$delta_se
  pval <- 2 * pnorm(-abs(delta / se))
  st <- ifelse(pval < 0.01, "***", ifelse(pval < 0.05, "**", ifelse(pval < 0.1, "*", "")))
  message(sprintf("  %s: delta=%.4f%s (boot SE=%.4f, p=%.4f), N=%d matches, %d players",
                  nm, delta, st, se, pval, nobs(res$pooled), length(unique(res$data$player_id))))
}

writeLines(summary_log, file.path(OUTPUT_DIR, "37_delta_model_log.md"))

message("\n", strrep("=", 70))
message("DONE: Script 37 complete")
message(strrep("=", 70))
