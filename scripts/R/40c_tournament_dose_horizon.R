# ==============================================================================
# 40c_tournament_dose_horizon.R
# Match-level tournament performance: consolidated tables, horizon x dose
# interaction models, delta-method summary impacts, and figures.
#
# Re-builds match-level data using the same vectorized approach as script 37,
# then estimates new specifications (horizon x dose interactions) and produces
# consolidated appendix tables, combined horizon-dose tables, summary impact
# tables, and 4-panel figures.
#
# Inputs:
#   Data/raw/atp_main_matches.rds, Data/raw/atp_qual_chall_matches.rds
#   Data/raw/wta_main_matches.rds, Data/raw/wta_qual_itf_matches.rds
#   Data/cleaned/tournament_rebuild_results.rds (GS events)
#   Data/cleaned/skeleton_nongs_est_v7.rds (nonGS events with v_hat)
#   Data/cleaned/elo_history.rds, Data/cleaned/wta_elo_history.rds
#   Data/cleaned/performance_dose.rds
#   Data/cleaned/tournament_elo_cache.rds
#
# Outputs:
#   Tables/table_tournament_consolidated_{gs,nongs}_{atp,wta}.tex  (4)
#   Tables/table_horizon_dose_gs.tex, table_horizon_dose_nongs.tex (2)
#   Tables/table_summary_impacts_gs.tex, table_summary_impacts_nongs.tex (2)
#   Figures/fig_delta_horizon_dose_{gs,nongs}_{atp,wta}.pdf (4)
#   Data/cleaned/delta_horizon_dose_results.rds
#
# Dependencies: dplyr, tidyr, data.table, ggplot2, patchwork, here
# ==============================================================================

set.seed(20260327)

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(patchwork)
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
CALENDAR_CAP <- 365
N_BOOT <- 200

# Points per pp win probability increase, by event type
PTS_PER_PP_GS    <- 15
PTS_PER_PP_NONGS <- 5


# ==============================================================================
# STEP 1: LOAD DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 1: LOAD DATA")
message(strrep("=", 70))

tr <- readRDS(file.path(CLEANED_DIR, "tournament_rebuild_results.rds"))
nongs_est <- readRDS(file.path(CLEANED_DIR, "skeleton_nongs_est_v7.rds"))
dose_data <- readRDS(file.path(CLEANED_DIR, "performance_dose.rds"))

atp_main <- readRDS(file.path(RAW_DIR, "atp_main_matches.rds"))
atp_qual <- readRDS(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
wta_main <- readRDS(file.path(RAW_DIR, "wta_main_matches.rds"))
wta_qual <- readRDS(file.path(RAW_DIR, "wta_qual_itf_matches.rds"))

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

# Safe date conversion
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

build_event_table <- function(events_df, nongs_events_df, all_events_list,
                               event_type, tour_label) {
  ev <- events_df
  if (is.numeric(ev$tourney_date)) {
    ev$ev_date <- as.Date(as.character(ev$tourney_date), format = "%Y%m%d")
  } else {
    ev$ev_date <- as.Date(ev$tourney_date)
  }
  ev$event_type <- event_type

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

  ev$next_same_type_date <- as.Date(NA)
  ev$censor_date <- as.Date(NA)

  for (i in seq_len(nrow(ev))) {
    pid <- ev$player_id[i]
    d <- ev$ev_date[i]
    future <- censor_source[censor_source$player_id == pid & censor_source$ev_date > d, ]
    if (nrow(future) > 0) {
      ev$next_same_type_date[i] <- min(future$ev_date)
    }
    cal_cap <- d + CALENDAR_CAP
    ev$censor_date[i] <- min(c(ev$next_same_type_date[i], cal_cap), na.rm = TRUE)
  }

  # Merge dose
  dose_sub <- dose_data[dose_data$tour == tour_label & dose_data$event_type == event_type,
                        c("player_id", "tourney_id", "dose", "matches_won")]
  ev <- merge(ev, dose_sub, by = c("player_id", "tourney_id"), all.x = TRUE)
  ev$dose[is.na(ev$dose)] <- 0
  ev$matches_won[is.na(ev$matches_won)] <- 0L

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
  message("    Building match-level data for ", nrow(ev_df), " events...")

  am_dt <- as.data.table(all_matches[, c("winner_id", "loser_id", "tourney_id",
                                          "tourney_date_d", "surface", "tourney_level",
                                          "winner_rank_points", "loser_rank_points",
                                          "winner_age", "loser_age")])
  w_idx <- split(seq_len(nrow(am_dt)), am_dt$winner_id)
  l_idx <- split(seq_len(nrow(am_dt)), am_dt$loser_id)

  match_rows <- vector("list", nrow(ev_df))

  for (i in seq_len(nrow(ev_df))) {
    if (i %% 500 == 0) message("      Event ", i, " / ", nrow(ev_df))
    pid <- ev_df$player_id[i]
    ev_d <- ev_df$ev_date[i]
    censor_d <- ev_df$censor_date[i]

    widx <- w_idx[[as.character(pid)]]
    lidx <- l_idx[[as.character(pid)]]

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

    all_idx <- c(widx, lidx)
    focal_is_winner <- c(rep(TRUE, n_w), rep(FALSE, n_l))

    md <- am_dt[all_idx, ]
    match_dates <- md$tourney_date_d
    weeks <- as.numeric(difftime(match_dates, ev_d, units = "days")) / 7

    horizon <- ifelse(weeks <= 4, "4w",
               ifelse(weeks <= 8, "8w",
               ifelse(weeks <= 12, "12w",
               ifelse(weeks <= 26, "26w", "52w"))))

    opp_rank_pts <- ifelse(focal_is_winner,
                           as.numeric(md$loser_rank_points),
                           as.numeric(md$winner_rank_points))
    opp_rank_pts[is.na(opp_rank_pts)] <- 0
    opp_age <- ifelse(focal_is_winner,
                      as.numeric(md$loser_age),
                      as.numeric(md$winner_age))
    opp_age[is.na(opp_age)] <- 25

    opp_ids <- ifelse(focal_is_winner, md$loser_id, md$winner_id)
    opp_elo <- get_pre_match_elo(opp_ids, match_dates, elo_dt)
    opp_elo[is.na(opp_elo)] <- 1500

    focal_elo_pre <- ev_df$pre_elo[i]
    focal_pts_pre <- ev_df$pre_rank_pts[i]
    focal_age_pre <- ev_df$player_age[i]
    focal_surf_elo_pre <- focal_elo_pre
    opp_surf_elo <- opp_elo

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
# STEP 4: ESTIMATE ALL MODELS (pooled, dose_mw, dose_pp, horizon, horizon_dose)
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 4: ESTIMATE MODELS")
message(strrep("=", 70))

BASE_COVS <- paste("pts_diff + pts_diff_sq + elo_diff + elo_diff_sq +",
                    "surface_elo_diff + surface_elo_diff_sq +",
                    "h2h_win_prop + h2h_count + age_diff +",
                    "surface_clay + surface_grass +",
                    "focal_pts + focal_pts_sq + focal_elo + focal_elo_sq +",
                    "focal_surf_elo + focal_surf_elo_sq + focal_age")

estimate_all_models <- function(md, label, has_cf = FALSE) {
  message("\n  --- ", label, " ---")
  if (nrow(md) == 0) {
    message("    No data. Skipping.")
    return(NULL)
  }

  md <- md[complete.cases(md[, c("won", "pts_diff", "elo_diff", "surface_elo_diff",
                                  "got_ll", "focal_pts", "focal_elo")]), ]

  cf_term <- if (has_cf) " + v_hat" else ""

  # (1) Pooled
  fml_pooled <- as.formula(paste0("won ~ ", BASE_COVS, " + got_ll", cf_term))
  mod_pooled <- glm(fml_pooled, data = md, family = binomial(link = "logit"))
  message("    Pooled: N=", nobs(mod_pooled), " delta=",
          round(coef(mod_pooled)["got_ll"], 4))

  # (2) Dose (matches won, uncentered for consolidated table)
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

  # (4) Horizon only: got_ll:horizon [+ v_hat:horizon]
  cf_horizon <- if (has_cf) " + v_hat:horizon" else ""
  fml_horizon <- as.formula(paste0("won ~ ", BASE_COVS,
                                   " + got_ll:horizon", cf_horizon))
  mod_horizon <- glm(fml_horizon, data = md, family = binomial(link = "logit"))
  horizon_coefs <- coef(mod_horizon)[grep("got_ll:horizon", names(coef(mod_horizon)))]
  message("    Horizon: ", paste(names(horizon_coefs), round(horizon_coefs, 4),
                                 sep = "=", collapse = ", "))

  # (5) Horizon x dose: got_ll:horizon + got_ll:horizon:dose [+ v_hat:horizon + v_hat:horizon:dose]
  cf_horizon_dose <- if (has_cf) " + v_hat:horizon + v_hat:horizon:dose" else ""
  fml_horizon_dose <- as.formula(paste0("won ~ ", BASE_COVS,
                                        " + got_ll:horizon + got_ll:horizon:dose",
                                        cf_horizon_dose))
  mod_horizon_dose <- glm(fml_horizon_dose, data = md, family = binomial(link = "logit"))
  hd_coefs <- coef(mod_horizon_dose)[grep("got_ll:horizon", names(coef(mod_horizon_dose)))]
  message("    Horizon x Dose: ", length(hd_coefs), " terms")

  list(
    pooled = mod_pooled, dose_mw = mod_dose_mw, dose_pp = mod_dose_pp,
    horizon = mod_horizon, horizon_dose = mod_horizon_dose,
    data = md, label = label, has_cf = has_cf
  )
}

res_gs_atp <- estimate_all_models(md_gs_atp, "GS-ATP", has_cf = FALSE)
res_gs_wta <- estimate_all_models(md_gs_wta, "GS-WTA", has_cf = FALSE)
res_nongs_atp <- estimate_all_models(md_nongs_atp, "NonGS-ATP", has_cf = TRUE)
res_nongs_wta <- estimate_all_models(md_nongs_wta, "NonGS-WTA", has_cf = TRUE)


# ==============================================================================
# STEP 5: BOOTSTRAP INFERENCE FOR HORIZON AND HORIZON x DOSE MODELS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 5: BOOTSTRAP INFERENCE")
message(strrep("=", 70))

bootstrap_all <- function(res, n_boot = N_BOOT) {
  if (is.null(res)) return(NULL)
  md <- res$data
  has_cf <- res$has_cf
  players <- unique(md$player_id)
  n_players <- length(players)

  cf_term <- if (has_cf) " + v_hat" else ""
  cf_horizon <- if (has_cf) " + v_hat:horizon" else ""
  cf_horizon_dose <- if (has_cf) " + v_hat:horizon + v_hat:horizon:dose" else ""

  fml_pooled <- as.formula(paste0("won ~ ", BASE_COVS, " + got_ll", cf_term))
  fml_horizon <- as.formula(paste0("won ~ ", BASE_COVS, " + got_ll:horizon", cf_horizon))
  fml_horizon_dose <- as.formula(paste0("won ~ ", BASE_COVS,
                                        " + got_ll:horizon + got_ll:horizon:dose",
                                        cf_horizon_dose))

  boot_delta <- numeric(n_boot)
  boot_horizon <- matrix(NA, n_boot, length(HORIZON_LABS))
  colnames(boot_horizon) <- HORIZON_LABS
  boot_hd_base <- matrix(NA, n_boot, length(HORIZON_LABS))
  colnames(boot_hd_base) <- HORIZON_LABS
  boot_hd_dose <- matrix(NA, n_boot, length(HORIZON_LABS))
  colnames(boot_hd_dose) <- HORIZON_LABS

  message("  Bootstrapping ", res$label, " (", n_boot, " reps)...")

  for (b in seq_len(n_boot)) {
    if (b %% 50 == 0) message("    Boot ", b, " / ", n_boot)
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

    # Horizon only
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

    # Horizon x dose
    mod_hd <- tryCatch(
      glm(fml_horizon_dose, data = boot_data, family = binomial(link = "logit")),
      error = function(e) NULL
    )
    if (!is.null(mod_hd)) {
      for (h_idx in seq_along(HORIZON_LABS)) {
        h <- HORIZON_LABS[h_idx]
        cname_base <- paste0("got_ll:horizon", h)
        cname_dose <- paste0("got_ll:horizon", h, ":dose")
        if (cname_base %in% names(coef(mod_hd))) {
          boot_hd_base[b, h_idx] <- coef(mod_hd)[cname_base]
        }
        if (cname_dose %in% names(coef(mod_hd))) {
          boot_hd_dose[b, h_idx] <- coef(mod_hd)[cname_dose]
        }
      }
    }
  }

  list(
    delta_se = sd(boot_delta, na.rm = TRUE),
    delta_ci = quantile(boot_delta, c(0.025, 0.975), na.rm = TRUE),
    horizon_se = apply(boot_horizon, 2, sd, na.rm = TRUE),
    horizon_ci_lo = apply(boot_horizon, 2, function(x) quantile(x, 0.025, na.rm = TRUE)),
    horizon_ci_hi = apply(boot_horizon, 2, function(x) quantile(x, 0.975, na.rm = TRUE)),
    hd_base_se = apply(boot_hd_base, 2, sd, na.rm = TRUE),
    hd_base_ci_lo = apply(boot_hd_base, 2, function(x) quantile(x, 0.025, na.rm = TRUE)),
    hd_base_ci_hi = apply(boot_hd_base, 2, function(x) quantile(x, 0.975, na.rm = TRUE)),
    hd_dose_se = apply(boot_hd_dose, 2, sd, na.rm = TRUE),
    hd_dose_ci_lo = apply(boot_hd_dose, 2, function(x) quantile(x, 0.025, na.rm = TRUE)),
    hd_dose_ci_hi = apply(boot_hd_dose, 2, function(x) quantile(x, 0.975, na.rm = TRUE)),
    # Store raw bootstrap draws for summary impact CIs
    boot_hd_base = boot_hd_base,
    boot_hd_dose = boot_hd_dose
  )
}

boot_gs_atp <- bootstrap_all(res_gs_atp)
boot_gs_wta <- bootstrap_all(res_gs_wta)
boot_nongs_atp <- bootstrap_all(res_nongs_atp)
boot_nongs_wta <- bootstrap_all(res_nongs_wta)


# ==============================================================================
# STEP 6: PART 1 -- CONSOLIDATED TOURNAMENT PERFORMANCE TABLES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 6: CONSOLIDATED TABLES")
message(strrep("=", 70))

# Extended coefficient maps for treatment and CF terms
TREATMENT_COEF_MAP <- c(
  "got_ll"                 = "$D_{ie}$ (LL entry)",
  "got_ll:matches_won_c"   = "$D_{ie} \\times$ matches won",
  "got_ll:dose"            = "$D_{ie} \\times$ perf.\\ dose",
  "v_hat"                  = "$\\hat{v}_{ie}$",
  "v_hat:matches_won_c"    = "$\\hat{v}_{ie} \\times$ matches won",
  "v_hat:dose"             = "$\\hat{v}_{ie} \\times$ perf.\\ dose"
)

generate_consolidated_table <- function(res, label, tag) {
  if (is.null(res)) return("")
  has_cf <- res$has_cf
  mods <- list(res$pooled, res$dose_mw, res$dose_pp)
  cts <- lapply(mods, function(m) coef(summary(m)))

  lines <- character()
  lines <- c(lines, "\\begin{tabular}{lccc}")
  lines <- c(lines, "\\toprule")
  lines <- c(lines, " & (1) Pooled & (2) Dose: Wins & (3) Dose: Perf \\\\")
  lines <- c(lines, "\\midrule")

  # --- Treatment terms ---
  lines <- c(lines, "\\multicolumn{4}{l}{\\textit{Treatment}} \\\\")

  treat_vars <- c("got_ll", "got_ll:matches_won_c", "got_ll:dose")
  if (has_cf) treat_vars <- c(treat_vars, "v_hat", "v_hat:matches_won_c", "v_hat:dose")

  for (v in treat_vars) {
    # Skip treatment terms not present in any model
    in_any <- any(sapply(cts, function(ct) v %in% rownames(ct)))
    if (!in_any) next
    lab <- TREATMENT_COEF_MAP[v]
    vals <- character(3)
    se_vals <- character(3)
    for (j in 1:3) {
      if (v %in% rownames(cts[[j]])) {
        est <- fmt(cts[[j]][v, "Estimate"], 4)
        se  <- fmt(cts[[j]][v, "Std. Error"], 4)
        st  <- add_stars(cts[[j]][v, "Pr(>|z|)"])
        vals[j] <- paste0(est, st)
        se_vals[j] <- paste0("(", se, ")")
      }
    }
    lines <- c(lines, paste0(lab, " & ", paste(vals, collapse = " & "), " \\\\"))
    lines <- c(lines, paste0(" & ", paste(se_vals, collapse = " & "), " \\\\"))
  }

  # --- Covariates ---
  lines <- c(lines, "\\midrule")
  lines <- c(lines, "\\multicolumn{4}{l}{\\textit{Panel A: Pairwise match covariates ($X_{ijm}$)}} \\\\")

  panel_a_vars <- c("pts_diff", "pts_diff_sq", "elo_diff", "elo_diff_sq",
                     "surface_elo_diff", "surface_elo_diff_sq",
                     "h2h_win_prop", "h2h_count", "age_diff",
                     "surface_clay", "surface_grass")

  for (v in panel_a_vars) {
    # Skip variables not estimated in any model (dropped due to collinearity)
    in_any <- any(sapply(cts, function(ct) v %in% rownames(ct)))
    if (!in_any) next
    lab <- COEF_MAP[v]
    vals <- character(3)
    se_vals <- character(3)
    for (j in 1:3) {
      if (v %in% rownames(cts[[j]])) {
        est <- fmt(cts[[j]][v, "Estimate"], 4)
        se  <- fmt(cts[[j]][v, "Std. Error"], 4)
        st  <- add_stars(cts[[j]][v, "Pr(>|z|)"])
        vals[j] <- paste0(est, st)
        se_vals[j] <- paste0("(", se, ")")
      }
    }
    lines <- c(lines, paste0(lab, " & ", paste(vals, collapse = " & "), " \\\\"))
    lines <- c(lines, paste0(" & ", paste(se_vals, collapse = " & "), " \\\\"))
  }

  # Panel B: Pre-event player characteristics
  lines <- c(lines, "\\midrule")
  lines <- c(lines, "\\multicolumn{4}{l}{\\textit{Panel B: Pre-event player characteristics ($Z^{pre}_{ie}$)}} \\\\")

  panel_b_vars <- c("focal_pts", "focal_pts_sq", "focal_elo", "focal_elo_sq",
                     "focal_surf_elo", "focal_surf_elo_sq", "focal_age", "(Intercept)")

  for (v in panel_b_vars) {
    in_any <- any(sapply(cts, function(ct) v %in% rownames(ct)))
    if (!in_any) next
    lab <- COEF_MAP[v]
    vals <- character(3)
    se_vals <- character(3)
    for (j in 1:3) {
      if (v %in% rownames(cts[[j]])) {
        est <- fmt(cts[[j]][v, "Estimate"], 4)
        se  <- fmt(cts[[j]][v, "Std. Error"], 4)
        st  <- add_stars(cts[[j]][v, "Pr(>|z|)"])
        vals[j] <- paste0(est, st)
        se_vals[j] <- paste0("(", se, ")")
      }
    }
    lines <- c(lines, paste0(lab, " & ", paste(vals, collapse = " & "), " \\\\"))
    lines <- c(lines, paste0(" & ", paste(se_vals, collapse = " & "), " \\\\"))
  }

  # --- Footer ---
  lines <- c(lines, "\\midrule")
  n_matches <- sapply(mods, nobs)
  n_players <- sapply(mods, function(m) {
    d <- m$data
    if (is.null(d)) length(unique(res$data$player_id))
    else length(unique(d$player_id))
  })
  # Use the res$data for player counts since glm$data may be null
  np <- length(unique(res$data$player_id))
  lines <- c(lines, paste0("Matches & ", paste(format(n_matches, big.mark = ","), collapse = " & "), " \\\\"))
  lines <- c(lines, paste0("Players & ", paste(rep(format(np, big.mark = ","), 3), collapse = " & "), " \\\\"))
  lines <- c(lines, "\\bottomrule")
  lines <- c(lines, "\\end{tabular}")

  tex <- paste(lines, collapse = "\n")
  out_path <- file.path(TABLES_DIR, paste0("table_tournament_consolidated_", tag, ".tex"))
  writeLines(tex, out_path)
  message("  Saved: ", out_path)
  tex
}

generate_consolidated_table(res_gs_atp, "GS-ATP", "gs_atp")
generate_consolidated_table(res_gs_wta, "GS-WTA", "gs_wta")
generate_consolidated_table(res_nongs_atp, "NonGS-ATP", "nongs_atp")
generate_consolidated_table(res_nongs_wta, "NonGS-WTA", "nongs_wta")


# ==============================================================================
# STEP 7: PART 2 -- HORIZON x DOSE COMBINED TABLES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 7: HORIZON x DOSE TABLES")
message(strrep("=", 70))

generate_horizon_dose_table <- function(res_atp, boot_atp, res_wta, boot_wta,
                                         event_type_tag) {
  lines <- character()
  lines <- c(lines, "\\begin{tabular}{l cccc}")
  lines <- c(lines, "\\toprule")
  lines <- c(lines, " & \\multicolumn{2}{c}{ATP} & \\multicolumn{2}{c}{WTA} \\\\")
  lines <- c(lines, "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}")
  lines <- c(lines, " & Horizon & Horizon $\\times$ Dose & Horizon & Horizon $\\times$ Dose \\\\")
  lines <- c(lines, "\\midrule")

  # Helper to extract horizon coefficients from a model
  extract_horizon_coefs <- function(res, boot, model_name, coef_prefix, se_source) {
    if (is.null(res)) return(list(est = rep(NA, 5), se = rep(NA, 5)))
    mod <- res[[model_name]]
    ct <- coef(summary(mod))
    est <- se <- numeric(length(HORIZON_LABS))
    for (i in seq_along(HORIZON_LABS)) {
      h <- HORIZON_LABS[i]
      cname <- paste0(coef_prefix, h)
      if (cname %in% rownames(ct)) {
        est[i] <- ct[cname, "Estimate"]
        se[i] <- boot[[se_source]][h]
      } else {
        est[i] <- NA
        se[i] <- NA
      }
    }
    list(est = est, se = se)
  }

  # Horizon-only model
  atp_h <- extract_horizon_coefs(res_atp, boot_atp, "horizon", "got_ll:horizon", "horizon_se")
  wta_h <- extract_horizon_coefs(res_wta, boot_wta, "horizon", "got_ll:horizon", "horizon_se")

  # Horizon x dose model: base (got_ll:horizon) terms
  atp_hd_base <- extract_horizon_coefs(res_atp, boot_atp, "horizon_dose", "got_ll:horizon", "hd_base_se")
  wta_hd_base <- extract_horizon_coefs(res_wta, boot_wta, "horizon_dose", "got_ll:horizon", "hd_base_se")

  # Horizon x dose model: interaction (got_ll:horizon:dose) terms
  atp_hd_dose <- extract_horizon_coefs(res_atp, boot_atp, "horizon_dose", "got_ll:horizon", "hd_dose_se")
  wta_hd_dose <- extract_horizon_coefs(res_wta, boot_wta, "horizon_dose", "got_ll:horizon", "hd_dose_se")

  # For the dose interaction terms, the coefficient names include ":dose"
  extract_dose_interaction <- function(res, boot) {
    if (is.null(res)) return(list(est = rep(NA, 5), se = rep(NA, 5)))
    mod <- res$horizon_dose
    ct <- coef(summary(mod))
    est <- se <- numeric(length(HORIZON_LABS))
    for (i in seq_along(HORIZON_LABS)) {
      h <- HORIZON_LABS[i]
      cname <- paste0("got_ll:horizon", h, ":dose")
      if (cname %in% rownames(ct)) {
        est[i] <- ct[cname, "Estimate"]
        se[i] <- boot$hd_dose_se[h]
      } else {
        est[i] <- NA
        se[i] <- NA
      }
    }
    list(est = est, se = se)
  }

  atp_dose_int <- extract_dose_interaction(res_atp, boot_atp)
  wta_dose_int <- extract_dose_interaction(res_wta, boot_wta)

  # Format helper
  fmt_cell <- function(est, se) {
    if (is.na(est)) return(c("", ""))
    pval <- 2 * pnorm(-abs(est / se))
    c(paste0(fmt(est, 4), add_stars(pval)),
      paste0("(", fmt(se, 4), ")"))
  }

  # Print horizon base coefficients
  lines <- c(lines, "\\multicolumn{5}{l}{\\textit{LL treatment effect by horizon ($\\hat{\\delta}_h$)}} \\\\")
  for (i in seq_along(HORIZON_LABS)) {
    h <- HORIZON_LABS[i]
    c_atp_h <- fmt_cell(atp_h$est[i], atp_h$se[i])
    c_atp_hd <- fmt_cell(atp_hd_base$est[i], atp_hd_base$se[i])
    c_wta_h <- fmt_cell(wta_h$est[i], wta_h$se[i])
    c_wta_hd <- fmt_cell(wta_hd_base$est[i], wta_hd_base$se[i])

    lines <- c(lines, paste0("$\\hat{\\delta}_{", h, "}$ & ",
                             c_atp_h[1], " & ", c_atp_hd[1], " & ",
                             c_wta_h[1], " & ", c_wta_hd[1], " \\\\"))
    lines <- c(lines, paste0(" & ", c_atp_h[2], " & ", c_atp_hd[2], " & ",
                             c_wta_h[2], " & ", c_wta_hd[2], " \\\\"))
  }

  # Print dose interaction coefficients
  lines <- c(lines, "\\midrule")
  lines <- c(lines, "\\multicolumn{5}{l}{\\textit{Dose interaction ($\\hat{\\delta}_h \\times$ dose)}} \\\\")
  for (i in seq_along(HORIZON_LABS)) {
    h <- HORIZON_LABS[i]
    c_atp <- fmt_cell(atp_dose_int$est[i], atp_dose_int$se[i])
    c_wta <- fmt_cell(wta_dose_int$est[i], wta_dose_int$se[i])

    lines <- c(lines, paste0("$\\hat{\\delta}_{", h, "} \\times$ dose & & ",
                             c_atp[1], " & & ", c_wta[1], " \\\\"))
    lines <- c(lines, paste0(" & & ", c_atp[2], " & & ", c_wta[2], " \\\\"))
  }

  # Footer
  lines <- c(lines, "\\midrule")
  lines <- c(lines, "Covariates & Yes & Yes & Yes & Yes \\\\")

  n_atp_h <- if (!is.null(res_atp)) format(nobs(res_atp$horizon), big.mark = ",") else ""
  n_atp_hd <- if (!is.null(res_atp)) format(nobs(res_atp$horizon_dose), big.mark = ",") else ""
  n_wta_h <- if (!is.null(res_wta)) format(nobs(res_wta$horizon), big.mark = ",") else ""
  n_wta_hd <- if (!is.null(res_wta)) format(nobs(res_wta$horizon_dose), big.mark = ",") else ""

  lines <- c(lines, paste0("Matches & ", n_atp_h, " & ", n_atp_hd,
                           " & ", n_wta_h, " & ", n_wta_hd, " \\\\"))
  lines <- c(lines, "\\bottomrule")
  lines <- c(lines, "\\end{tabular}")

  tex <- paste(lines, collapse = "\n")
  out_path <- file.path(TABLES_DIR, paste0("table_horizon_dose_", event_type_tag, ".tex"))
  writeLines(tex, out_path)
  message("  Saved: ", out_path)
  tex
}

generate_horizon_dose_table(res_gs_atp, boot_gs_atp, res_gs_wta, boot_gs_wta, "gs")
generate_horizon_dose_table(res_nongs_atp, boot_nongs_atp, res_nongs_wta, boot_nongs_wta, "nongs")


# ==============================================================================
# STEP 8: PART 3 -- SUMMARY IMPACTS (DELTA METHOD)
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 8: SUMMARY IMPACTS")
message(strrep("=", 70))

compute_summary_impacts <- function(res, boot, pts_per_pp) {
  if (is.null(res)) return(NULL)
  md <- res$data
  mod_h <- res$horizon
  mod_hd <- res$horizon_dose

  # Mean derivative: mean[p(1-p)] from pooled predicted probabilities
  p_hat <- predict(res$pooled, type = "response")
  mean_deriv <- mean(p_hat * (1 - p_hat), na.rm = TRUE)

  # Mean dose among treated
  mean_dose_treated <- mean(md$dose[md$got_ll == 1], na.rm = TRUE)

  ct_h <- coef(summary(mod_h))
  ct_hd <- coef(summary(mod_hd))

  result <- data.frame(
    horizon = HORIZONS,
    horizon_lab = HORIZON_LABS,
    stringsAsFactors = FALSE
  )

  # Horizon-only model: base AME
  for (i in seq_along(HORIZON_LABS)) {
    h <- HORIZON_LABS[i]
    cname <- paste0("got_ll:horizon", h)

    # Base model
    if (cname %in% rownames(ct_h)) {
      delta_h <- ct_h[cname, "Estimate"]
      se_h <- boot$horizon_se[h]
    } else {
      delta_h <- NA; se_h <- NA
    }
    result$delta_base[i] <- delta_h
    result$se_base[i] <- se_h
    result$ame_base[i] <- delta_h * mean_deriv
    result$ame_base_lo[i] <- boot$horizon_ci_lo[h] * mean_deriv
    result$ame_base_hi[i] <- boot$horizon_ci_hi[h] * mean_deriv

    # Horizon x dose model: base + dose * mean_dose
    cname_base <- paste0("got_ll:horizon", h)
    cname_dose <- paste0("got_ll:horizon", h, ":dose")

    if (cname_base %in% rownames(ct_hd)) {
      delta_hd_base <- ct_hd[cname_base, "Estimate"]
    } else { delta_hd_base <- NA }

    if (cname_dose %in% rownames(ct_hd)) {
      delta_hd_dose <- ct_hd[cname_dose, "Estimate"]
    } else { delta_hd_dose <- 0 }

    # Total effect at mean dose: delta_base + delta_dose * mean_dose
    total_at_mean <- delta_hd_base + delta_hd_dose * mean_dose_treated
    result$delta_at_mean[i] <- total_at_mean
    result$ame_at_mean[i] <- total_at_mean * mean_deriv

    # Bootstrap CI for the total effect at mean dose
    if (!is.null(boot$boot_hd_base) && !is.null(boot$boot_hd_dose)) {
      boot_total <- boot$boot_hd_base[, i] + boot$boot_hd_dose[, i] * mean_dose_treated
      boot_ame <- boot_total * mean_deriv
      result$ame_at_mean_lo[i] <- quantile(boot_ame, 0.025, na.rm = TRUE)
      result$ame_at_mean_hi[i] <- quantile(boot_ame, 0.975, na.rm = TRUE)
    } else {
      result$ame_at_mean_lo[i] <- NA
      result$ame_at_mean_hi[i] <- NA
    }
  }

  # Points per tournament and cumulative
  result$pts_base <- result$ame_base * pts_per_pp
  result$pts_at_mean <- result$ame_at_mean * pts_per_pp
  result$cum_pts_base <- cumsum(ifelse(is.na(result$pts_base), 0, result$pts_base))
  result$cum_pts_at_mean <- cumsum(ifelse(is.na(result$pts_at_mean), 0, result$pts_at_mean))
  result$cum_wins_base <- cumsum(ifelse(is.na(result$ame_base), 0, result$ame_base))
  result$cum_wins_at_mean <- cumsum(ifelse(is.na(result$ame_at_mean), 0, result$ame_at_mean))

  # Bootstrap CIs for cumulative quantities
  if (!is.null(boot$boot_hd_base)) {
    n_boot_valid <- nrow(boot$boot_hd_base)
    cum_pts_base_boot <- matrix(NA, n_boot_valid, length(HORIZON_LABS))
    cum_pts_mean_boot <- matrix(NA, n_boot_valid, length(HORIZON_LABS))
    cum_wins_base_boot <- matrix(NA, n_boot_valid, length(HORIZON_LABS))
    cum_wins_mean_boot <- matrix(NA, n_boot_valid, length(HORIZON_LABS))

    for (b in seq_len(n_boot_valid)) {
      h_ame_base <- boot$boot_hd_base[b, ] * mean_deriv
      # For base model, use horizon-only bootstrap; approximate with hd_base at dose=0
      h_total_mean <- (boot$boot_hd_base[b, ] + boot$boot_hd_dose[b, ] * mean_dose_treated) * mean_deriv

      h_ame_base[is.na(h_ame_base)] <- 0
      h_total_mean[is.na(h_total_mean)] <- 0

      cum_pts_base_boot[b, ] <- cumsum(h_ame_base * pts_per_pp)
      cum_pts_mean_boot[b, ] <- cumsum(h_total_mean * pts_per_pp)
      cum_wins_base_boot[b, ] <- cumsum(h_ame_base)
      cum_wins_mean_boot[b, ] <- cumsum(h_total_mean)
    }

    result$cum_pts_base_lo <- apply(cum_pts_base_boot, 2, function(x) quantile(x, 0.025, na.rm = TRUE))
    result$cum_pts_base_hi <- apply(cum_pts_base_boot, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
    result$cum_pts_mean_lo <- apply(cum_pts_mean_boot, 2, function(x) quantile(x, 0.025, na.rm = TRUE))
    result$cum_pts_mean_hi <- apply(cum_pts_mean_boot, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
    result$cum_wins_base_lo <- apply(cum_wins_base_boot, 2, function(x) quantile(x, 0.025, na.rm = TRUE))
    result$cum_wins_base_hi <- apply(cum_wins_base_boot, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
    result$cum_wins_mean_lo <- apply(cum_wins_mean_boot, 2, function(x) quantile(x, 0.025, na.rm = TRUE))
    result$cum_wins_mean_hi <- apply(cum_wins_mean_boot, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
  }

  result$mean_deriv <- mean_deriv
  result$mean_dose_treated <- mean_dose_treated
  result$pts_per_pp <- pts_per_pp
  result
}

impacts_gs_atp <- compute_summary_impacts(res_gs_atp, boot_gs_atp, PTS_PER_PP_GS)
impacts_gs_wta <- compute_summary_impacts(res_gs_wta, boot_gs_wta, PTS_PER_PP_GS)
impacts_nongs_atp <- compute_summary_impacts(res_nongs_atp, boot_nongs_atp, PTS_PER_PP_NONGS)
impacts_nongs_wta <- compute_summary_impacts(res_nongs_wta, boot_nongs_wta, PTS_PER_PP_NONGS)


# --- Generate summary impact tables ---

generate_summary_table <- function(imp_atp, imp_wta, event_tag) {
  lines <- character()
  lines <- c(lines, "\\begin{tabular}{l cccc}")
  lines <- c(lines, "\\toprule")
  lines <- c(lines, " & \\multicolumn{2}{c}{ATP} & \\multicolumn{2}{c}{WTA} \\\\")
  lines <- c(lines, "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}")
  lines <- c(lines, " & Base & At mean dose & Base & At mean dose \\\\")
  lines <- c(lines, "\\midrule")

  # AME on win probability
  lines <- c(lines, "\\multicolumn{5}{l}{\\textit{AME on match win probability}} \\\\")
  for (i in seq_along(HORIZON_LABS)) {
    h <- HORIZON_LABS[i]
    a_base <- if (!is.null(imp_atp) && !is.na(imp_atp$ame_base[i])) {
      sprintf("%.4f [%.4f, %.4f]", imp_atp$ame_base[i], imp_atp$ame_base_lo[i], imp_atp$ame_base_hi[i])
    } else ""
    a_mean <- if (!is.null(imp_atp) && !is.na(imp_atp$ame_at_mean[i])) {
      sprintf("%.4f [%.4f, %.4f]", imp_atp$ame_at_mean[i], imp_atp$ame_at_mean_lo[i], imp_atp$ame_at_mean_hi[i])
    } else ""
    w_base <- if (!is.null(imp_wta) && !is.na(imp_wta$ame_base[i])) {
      sprintf("%.4f [%.4f, %.4f]", imp_wta$ame_base[i], imp_wta$ame_base_lo[i], imp_wta$ame_base_hi[i])
    } else ""
    w_mean <- if (!is.null(imp_wta) && !is.na(imp_wta$ame_at_mean[i])) {
      sprintf("%.4f [%.4f, %.4f]", imp_wta$ame_at_mean[i], imp_wta$ame_at_mean_lo[i], imp_wta$ame_at_mean_hi[i])
    } else ""
    lines <- c(lines, paste0(h, " & ", a_base, " & ", a_mean, " & ", w_base, " & ", w_mean, " \\\\"))
  }

  # Cumulative ranking points
  lines <- c(lines, "\\midrule")
  lines <- c(lines, "\\multicolumn{5}{l}{\\textit{Cumulative expected ranking points}} \\\\")
  for (i in seq_along(HORIZON_LABS)) {
    h <- HORIZON_LABS[i]
    a_base <- if (!is.null(imp_atp)) fmt(imp_atp$cum_pts_base[i], 1) else ""
    a_mean <- if (!is.null(imp_atp)) fmt(imp_atp$cum_pts_at_mean[i], 1) else ""
    w_base <- if (!is.null(imp_wta)) fmt(imp_wta$cum_pts_base[i], 1) else ""
    w_mean <- if (!is.null(imp_wta)) fmt(imp_wta$cum_pts_at_mean[i], 1) else ""
    lines <- c(lines, paste0(h, " & ", a_base, " & ", a_mean, " & ", w_base, " & ", w_mean, " \\\\"))
  }

  # Cumulative expected wins
  lines <- c(lines, "\\midrule")
  lines <- c(lines, "\\multicolumn{5}{l}{\\textit{Cumulative expected additional wins}} \\\\")
  for (i in seq_along(HORIZON_LABS)) {
    h <- HORIZON_LABS[i]
    a_base <- if (!is.null(imp_atp)) fmt(imp_atp$cum_wins_base[i], 4) else ""
    a_mean <- if (!is.null(imp_atp)) fmt(imp_atp$cum_wins_at_mean[i], 4) else ""
    w_base <- if (!is.null(imp_wta)) fmt(imp_wta$cum_wins_base[i], 4) else ""
    w_mean <- if (!is.null(imp_wta)) fmt(imp_wta$cum_wins_at_mean[i], 4) else ""
    lines <- c(lines, paste0(h, " & ", a_base, " & ", a_mean, " & ", w_base, " & ", w_mean, " \\\\"))
  }

  lines <- c(lines, "\\bottomrule")
  lines <- c(lines, "\\end{tabular}")

  tex <- paste(lines, collapse = "\n")
  out_path <- file.path(TABLES_DIR, paste0("table_summary_impacts_", event_tag, ".tex"))
  writeLines(tex, out_path)
  message("  Saved: ", out_path)
  tex
}

generate_summary_table(impacts_gs_atp, impacts_gs_wta, "gs")
generate_summary_table(impacts_nongs_atp, impacts_nongs_wta, "nongs")


# ==============================================================================
# STEP 9: PART 4 -- HORIZON x DOSE FIGURES (4-panel)
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 9: FIGURES")
message(strrep("=", 70))

generate_horizon_dose_figure <- function(res, boot, impacts, label, tag) {
  if (is.null(res) || is.null(impacts)) return(NULL)

  md <- res$data
  mod_hd <- res$horizon_dose
  pts_per_pp <- impacts$pts_per_pp[1]
  mean_deriv <- impacts$mean_deriv[1]
  mean_dose <- impacts$mean_dose_treated[1]

  ct_hd <- coef(summary(mod_hd))

  # Build plot data: two series -- base (dose=0) and at mean dose
  pd <- data.frame(
    horizon = rep(HORIZONS, 2),
    type = rep(c("Base (dose = 0)", paste0("At mean dose (", fmt(mean_dose, 2), ")")),
               each = length(HORIZONS)),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(HORIZON_LABS)) {
    h <- HORIZON_LABS[i]
    cname_base <- paste0("got_ll:horizon", h)
    cname_dose <- paste0("got_ll:horizon", h, ":dose")

    delta_base <- if (cname_base %in% rownames(ct_hd)) ct_hd[cname_base, "Estimate"] else NA
    delta_dose <- if (cname_dose %in% rownames(ct_hd)) ct_hd[cname_dose, "Estimate"] else 0

    # Base series (dose = 0)
    pd$ame[i] <- delta_base * mean_deriv
    pd$ame_lo[i] <- boot$hd_base_ci_lo[h] * mean_deriv
    pd$ame_hi[i] <- boot$hd_base_ci_hi[h] * mean_deriv

    # At mean dose series
    j <- i + length(HORIZONS)
    total <- delta_base + delta_dose * mean_dose
    pd$ame[j] <- total * mean_deriv

    # Bootstrap CI
    if (!is.null(boot$boot_hd_base)) {
      boot_total <- boot$boot_hd_base[, i] + boot$boot_hd_dose[, i] * mean_dose
      boot_ame <- boot_total * mean_deriv
      pd$ame_lo[j] <- quantile(boot_ame, 0.025, na.rm = TRUE)
      pd$ame_hi[j] <- quantile(boot_ame, 0.975, na.rm = TRUE)
    } else {
      pd$ame_lo[j] <- NA; pd$ame_hi[j] <- NA
    }
  }

  # Points per tournament
  pd$pts <- pd$ame * pts_per_pp
  pd$pts_lo <- pd$ame_lo * pts_per_pp
  pd$pts_hi <- pd$ame_hi * pts_per_pp

  # Cumulative (need to split by type)
  types <- unique(pd$type)
  for (tp in types) {
    idx <- pd$type == tp
    pd$cum_pts[idx] <- cumsum(ifelse(is.na(pd$pts[idx]), 0, pd$pts[idx]))
    pd$cum_wins[idx] <- cumsum(ifelse(is.na(pd$ame[idx]), 0, pd$ame[idx]))

    # Bootstrap CIs for cumulative
    if (!is.null(boot$boot_hd_base)) {
      n_b <- nrow(boot$boot_hd_base)
      cum_pts_boot <- cum_wins_boot <- matrix(NA, n_b, length(HORIZONS))
      for (b in seq_len(n_b)) {
        if (tp == types[1]) {
          # Base: dose = 0
          h_ame <- boot$boot_hd_base[b, ] * mean_deriv
        } else {
          h_ame <- (boot$boot_hd_base[b, ] + boot$boot_hd_dose[b, ] * mean_dose) * mean_deriv
        }
        h_ame[is.na(h_ame)] <- 0
        cum_pts_boot[b, ] <- cumsum(h_ame * pts_per_pp)
        cum_wins_boot[b, ] <- cumsum(h_ame)
      }
      pd$cum_pts_lo[idx] <- apply(cum_pts_boot, 2, function(x) quantile(x, 0.025, na.rm = TRUE))
      pd$cum_pts_hi[idx] <- apply(cum_pts_boot, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
      pd$cum_wins_lo[idx] <- apply(cum_wins_boot, 2, function(x) quantile(x, 0.025, na.rm = TRUE))
      pd$cum_wins_hi[idx] <- apply(cum_wins_boot, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
    } else {
      pd$cum_pts_lo[idx] <- NA; pd$cum_pts_hi[idx] <- NA
      pd$cum_wins_lo[idx] <- NA; pd$cum_wins_hi[idx] <- NA
    }
  }

  pd$type <- factor(pd$type, levels = types)

  # Dodge width for side-by-side points
  dodge_w <- 1.5

  # Panel (a): AME on match win probability
  p_a <- ggplot(pd, aes(x = horizon, y = ame, color = type, fill = type)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(ymin = ame_lo, ymax = ame_hi, group = type), alpha = 0.12, color = NA,
                position = position_dodge(width = dodge_w)) +
    geom_point(size = 2, position = position_dodge(width = dodge_w)) +
    geom_line(aes(group = type), position = position_dodge(width = dodge_w)) +
    scale_color_manual(values = c("grey30", col_treat)) +
    scale_fill_manual(values = c("grey30", col_treat)) +
    labs(x = "Weeks after LL event", y = "AME on win probability", tag = "(a)") +
    theme_paper() +
    theme(legend.position = "bottom")

  # Panel (b): Expected additional ranking points per tournament
  p_b <- ggplot(pd, aes(x = horizon, y = pts, color = type, fill = type)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(ymin = pts_lo, ymax = pts_hi, group = type), alpha = 0.12, color = NA,
                position = position_dodge(width = dodge_w)) +
    geom_point(size = 2, position = position_dodge(width = dodge_w)) +
    geom_line(aes(group = type), position = position_dodge(width = dodge_w)) +
    scale_color_manual(values = c("grey30", col_treat)) +
    scale_fill_manual(values = c("grey30", col_treat)) +
    labs(x = "Weeks after LL event", y = "Ranking pts per tournament", tag = "(b)") +
    theme_paper() +
    theme(legend.position = "bottom")

  # Panel (c): Cumulative expected ranking points
  p_c <- ggplot(pd, aes(x = horizon, y = cum_pts, color = type, fill = type)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(ymin = cum_pts_lo, ymax = cum_pts_hi, group = type), alpha = 0.12, color = NA,
                position = position_dodge(width = dodge_w)) +
    geom_point(size = 2, position = position_dodge(width = dodge_w)) +
    geom_line(aes(group = type), position = position_dodge(width = dodge_w)) +
    scale_color_manual(values = c("grey30", col_treat)) +
    scale_fill_manual(values = c("grey30", col_treat)) +
    labs(x = "Weeks after LL event", y = "Cumulative ranking pts", tag = "(c)") +
    theme_paper() +
    theme(legend.position = "bottom")

  # Panel (d): Cumulative expected additional wins
  p_d <- ggplot(pd, aes(x = horizon, y = cum_wins, color = type, fill = type)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(ymin = cum_wins_lo, ymax = cum_wins_hi, group = type), alpha = 0.12, color = NA,
                position = position_dodge(width = dodge_w)) +
    geom_point(size = 2, position = position_dodge(width = dodge_w)) +
    geom_line(aes(group = type), position = position_dodge(width = dodge_w)) +
    scale_color_manual(values = c("grey30", col_treat)) +
    scale_fill_manual(values = c("grey30", col_treat)) +
    labs(x = "Weeks after LL event", y = "Cumulative additional wins", tag = "(d)") +
    theme_paper() +
    theme(legend.position = "bottom")

  # Combine
  combined <- (p_a | p_b) / (p_c | p_d) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

  fig_path <- file.path(FIGURES_DIR, paste0("fig_delta_horizon_dose_", tag, ".pdf"))
  ggsave(fig_path, combined, width = 10, height = 8, device = cairo_pdf, bg = "transparent")
  message("  Figure saved: ", fig_path)
}

generate_horizon_dose_figure(res_gs_atp, boot_gs_atp, impacts_gs_atp, "GS-ATP", "gs_atp")
generate_horizon_dose_figure(res_gs_wta, boot_gs_wta, impacts_gs_wta, "GS-WTA", "gs_wta")
generate_horizon_dose_figure(res_nongs_atp, boot_nongs_atp, impacts_nongs_atp, "NonGS-ATP", "nongs_atp")
generate_horizon_dose_figure(res_nongs_wta, boot_nongs_wta, impacts_nongs_wta, "NonGS-WTA", "nongs_wta")


# ==============================================================================
# STEP 10: SAVE RESULTS AND LOG
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 10: SAVE RESULTS")
message(strrep("=", 70))

all_results <- list(
  gs_atp = res_gs_atp, gs_wta = res_gs_wta,
  nongs_atp = res_nongs_atp, nongs_wta = res_nongs_wta,
  boot_gs_atp = boot_gs_atp, boot_gs_wta = boot_gs_wta,
  boot_nongs_atp = boot_nongs_atp, boot_nongs_wta = boot_nongs_wta,
  impacts_gs_atp = impacts_gs_atp, impacts_gs_wta = impacts_gs_wta,
  impacts_nongs_atp = impacts_nongs_atp, impacts_nongs_wta = impacts_nongs_wta
)

# Strip raw data and large objects to keep file size reasonable
for (nm in c("gs_atp", "gs_wta", "nongs_atp", "nongs_wta")) {
  if (!is.null(all_results[[nm]])) {
    all_results[[nm]]$data <- NULL
    # Strip $data from glm objects (they store a copy of the full dataset)
    for (mod_nm in c("pooled", "dose_mw", "dose_pp", "horizon", "horizon_dose")) {
      if (!is.null(all_results[[nm]][[mod_nm]])) {
        all_results[[nm]][[mod_nm]]$data <- NULL
        all_results[[nm]][[mod_nm]]$model <- NULL
        all_results[[nm]][[mod_nm]]$residuals <- NULL
        all_results[[nm]][[mod_nm]]$fitted.values <- NULL
        all_results[[nm]][[mod_nm]]$linear.predictors <- NULL
        all_results[[nm]][[mod_nm]]$weights <- NULL
        all_results[[nm]][[mod_nm]]$prior.weights <- NULL
        all_results[[nm]][[mod_nm]]$y <- NULL
        all_results[[nm]][[mod_nm]]$effects <- NULL
      }
    }
  }
}
# Strip large bootstrap draw matrices from boot objects (keep summary stats)
for (nm in c("boot_gs_atp", "boot_gs_wta", "boot_nongs_atp", "boot_nongs_wta")) {
  if (!is.null(all_results[[nm]])) {
    all_results[[nm]]$boot_hd_base <- NULL
    all_results[[nm]]$boot_hd_dose <- NULL
  }
}

saveRDS(all_results, file.path(CLEANED_DIR, "delta_horizon_dose_results.rds"))
message("  Results saved to Data/cleaned/delta_horizon_dose_results.rds")

# Print summary
message("\n  === SUMMARY ===")
for (nm in c("gs_atp", "gs_wta", "nongs_atp", "nongs_wta")) {
  res <- switch(nm,
    gs_atp = res_gs_atp, gs_wta = res_gs_wta,
    nongs_atp = res_nongs_atp, nongs_wta = res_nongs_wta
  )
  boot_res <- switch(nm,
    gs_atp = boot_gs_atp, gs_wta = boot_gs_wta,
    nongs_atp = boot_nongs_atp, nongs_wta = boot_nongs_wta
  )
  if (is.null(res)) next

  delta <- coef(res$pooled)["got_ll"]
  se <- boot_res$delta_se
  pval <- 2 * pnorm(-abs(delta / se))
  st <- ifelse(pval < 0.01, "***", ifelse(pval < 0.05, "**", ifelse(pval < 0.1, "*", "")))
  message(sprintf("  %s: pooled delta=%.4f%s (SE=%.4f, p=%.4f), N=%d matches",
                  nm, delta, st, se, pval, nobs(res$pooled)))

  # Horizon x dose model summary
  mod_hd <- res$horizon_dose
  ct_hd <- coef(summary(mod_hd))
  hd_names <- grep("got_ll:horizon.*:dose", rownames(ct_hd), value = TRUE)
  if (length(hd_names) > 0) {
    message("    Horizon x dose interactions: ",
            paste(hd_names, fmt(ct_hd[hd_names, "Estimate"], 4), sep = "=", collapse = ", "))
  }
}

writeLines(summary_log, file.path(OUTPUT_DIR, "40c_tournament_dose_horizon_log.md"))

message("\n", strrep("=", 70))
message("DONE: Script 40c complete")
message("  4 consolidated tables, 2 horizon-dose tables, 2 summary impact tables, 4 figures")
message(strrep("=", 70))
