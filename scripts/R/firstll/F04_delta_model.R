# ==============================================================================
# F04_delta_model.R
# Post-event match-level performance model (delta) for first-LL sample.
#
# Adapts 37_delta_model.R to the first-LL restricted sample:
#   - Filters tournament_rebuild_results events to first-LL players
#     (had_prior_ll == 0) by matching against firstll skeletons
#   - Uses firstll output paths (Tables_FirstLL/, Figures_FirstLL/)
#   - NonGS uses v_hat from first-LL re-estimated control function
#
# Four samples: GS-ATP, GS-WTA, nonGS-ATP, nonGS-WTA
# Four specifications per sample:
#   (1) Pooled:            D_ie [+ v_hat]
#   (2) Dose (wins):       D_ie + D_ie x matches_won_c [+ v_hat interactions]
#   (3) Dose (perf prob):  D_ie + D_ie x dose [+ v_hat interactions]
#   (4) Horizon hetero:    D_ie x horizon [+ v_hat x horizon]
#
# Tracking window: h* = min(52 weeks, next same-type LL opportunity)
# Inference: robust SEs from glm (bootstrap deferred to full pipeline)
#
# Inputs:
#   Data/cleaned/tournament_rebuild_results.rds
#   Data/cleaned/firstll/firstll_gs_est_v2.rds
#   Data/cleaned/firstll/firstll_nongs_est_v2.rds
#   Data/cleaned/firstll/firstll_performance_dose.rds
#   Data/cleaned/elo_history.rds, wta_elo_history.rds
#   Data/raw/atp_main_matches.rds, atp_qual_chall_matches.rds
#   Data/raw/wta_main_matches.rds, wta_qual_itf_matches.rds
#
# Outputs:
#   Tables_FirstLL/table_delta_pooled_{gs_atp,...}.tex  (4 tables)
#   Tables_FirstLL/table_delta_dose_mw_{gs_atp,...}.tex (4 tables)
#   Tables_FirstLL/table_delta_dose_pp_{gs_atp,...}.tex (4 tables)
#   Tables_FirstLL/table_delta_horizon_{gs_atp,...}.tex (4 tables)
#   Figures_FirstLL/fig_delta_horizon_{gs_atp,...}.pdf  (4 figures)
#   Data/cleaned/firstll/firstll_delta_model_results.rds
#
# Dependencies: dplyr, data.table, ggplot2, here
# ==============================================================================

set.seed(20260416)

library(dplyr)
library(data.table)
library(ggplot2)
library(here)
library(sandwich)
library(lmtest)

source(here("scripts", "R", "firstll", "firstll_helpers.R"))
source(here("scripts", "R", "win_model_helpers.R"))
summary_log <- character()

CALENDAR_CAP <- 365  # days

# Cluster-robust coefficient table at the player_id level.
# Falls back to naive summary if clustering fails (e.g., singletons).
clustered_ct <- function(mod, data, cluster_var = "player_id") {
  cl <- data[[cluster_var]]
  if (is.null(cl) || length(unique(cl)) < 2) {
    return(coef(summary(mod)))
  }
  vcov_cl <- tryCatch(
    sandwich::vcovCL(mod, cluster = cl, type = "HC1"),
    error = function(e) NULL
  )
  if (is.null(vcov_cl)) return(coef(summary(mod)))
  tryCatch(
    lmtest::coeftest(mod, vcov. = vcov_cl),
    error = function(e) coef(summary(mod))
  )
}


# ==============================================================================
# STEP 1: LOAD DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 1: LOAD DATA")
message(strrep("=", 70))

# First-LL skeletons (for filtering events to first-LL players)
fl_gs  <- readRDS(file.path(FIRSTLL_CLEANED, "firstll_gs_est_v2.rds"))
fl_ngs <- readRDS(file.path(FIRSTLL_CLEANED, "firstll_nongs_est_v2.rds"))

# Tournament rebuild results (event tables for delta model)
tr <- tryCatch(
  readRDS(file.path(CLEANED_DIR, "tournament_rebuild_results.rds")),
  error = function(e) {
    message("  ERROR: tournament_rebuild_results.rds not found: ", e$message)
    NULL
  }
)

# Performance dose (first-LL restricted)
dose_data <- tryCatch(
  readRDS(file.path(FIRSTLL_CLEANED, "firstll_performance_dose.rds")),
  error = function(e) {
    message("  WARNING: firstll_performance_dose.rds not found, trying main: ", e$message)
    readRDS(file.path(CLEANED_DIR, "performance_dose.rds"))
  }
)

# NonGS skeleton for censoring timeline
nongs_est <- tryCatch(
  readRDS(file.path(CLEANED_DIR, "skeleton_nongs_est_v8.rds")),
  error = function(e) {
    message("  WARNING: skeleton_nongs_est_v8.rds not found: ", e$message)
    NULL
  }
)

# Raw match data
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

# Pre-convert match dates
safe_as_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (is.numeric(x)) return(as.Date(as.character(x), format = "%Y%m%d"))
  as.Date(x)
}

atp_all$tourney_date_d <- safe_as_date(atp_all$tourney_date)
wta_all$tourney_date_d <- safe_as_date(wta_all$tourney_date)

message("  ATP all matches: ", nrow(atp_all))
message("  WTA all matches: ", nrow(wta_all))
message("  First-LL GS: ", nrow(fl_gs), " | First-LL NGS: ", nrow(fl_ngs))

# Build first-LL identifier sets (player_id + tourney_id)
fl_gs_keys  <- paste0(fl_gs$player_id, "_", fl_gs$tourney_id)
fl_ngs_keys <- paste0(fl_ngs$player_id, "_", fl_ngs$tourney_id)


# ==============================================================================
# STEP 2: BUILD EVENT TABLES (filtered to first-LL)
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 2: BUILD FIRST-LL EVENT TABLES")
message(strrep("=", 70))

if (is.null(tr)) {
  message("  SKIPPING: tournament_rebuild_results.rds not available.")
  message("  Delta model requires match-level data from script 37 pipeline.")
  message("  Creating placeholder outputs.")

  # Save empty results
  saveRDS(list(status = "placeholder", reason = "tournament_rebuild_results.rds missing"),
          file.path(FIRSTLL_CLEANED, "firstll_delta_model_results.rds"))

  # Write placeholder tables
  placeholder_tex <- paste(c(
    "\\begin{tabular}{lc}", "\\toprule",
    " & First-LL \\\\", "\\midrule",
    "\\multicolumn{2}{c}{\\textit{Placeholder: requires re-running delta pipeline}} \\\\",
    "\\bottomrule", "\\end{tabular}"
  ), collapse = "\n")
  for (tag in c("gs_atp", "gs_wta", "nongs_atp", "nongs_wta")) {
    for (prefix in c("table_delta_pooled_", "table_delta_dose_mw_",
                     "table_delta_dose_pp_", "table_delta_horizon_")) {
      writeLines(placeholder_tex, file.path(FIRSTLL_TABLES, paste0(prefix, tag, ".tex")))
    }
  }
  message("  16 placeholder tables written to Tables_FirstLL/")
  message("DONE (placeholder mode)")
  quit(save = "no", status = 0)
}

# Function to build event table with censoring, restricted to first-LL
build_event_table_firstll <- function(events_df, fl_key_set, all_events_list,
                                      event_type, tour_label, dose_df) {
  ev <- events_df
  # Handle date formats
  if (is.numeric(ev$tourney_date)) {
    ev$ev_date <- as.Date(as.character(ev$tourney_date), format = "%Y%m%d")
  } else {
    ev$ev_date <- as.Date(ev$tourney_date)
  }
  ev$event_type <- event_type

  # Filter to first-LL events
  ev$key <- paste0(ev$player_id, "_", ev$tourney_id)
  n_before <- nrow(ev)
  ev <- ev[ev$key %in% fl_key_set, ]
  message("  ", tour_label, " ", event_type, ": ", n_before, " -> ",
          nrow(ev), " first-LL events")

  if (nrow(ev) == 0) return(ev)

  # Censoring: build timeline of same-type qualifying losses
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
    future <- censor_source[censor_source$player_id == pid &
                            censor_source$ev_date > d, ]
    if (nrow(future) > 0) {
      ev$next_same_type_date[i] <- min(future$ev_date)
    }
    cal_cap <- d + CALENDAR_CAP
    ev$censor_date[i] <- min(c(ev$next_same_type_date[i], cal_cap), na.rm = TRUE)
  }

  # Merge dose
  dose_sub <- dose_df[dose_df$tour == tour_label & dose_df$event_type == event_type,
                      c("player_id", "tourney_id", "dose", "matches_won")]
  ev <- merge(ev, dose_sub, by = c("player_id", "tourney_id"), all.x = TRUE)
  ev$dose[is.na(ev$dose)] <- 0
  ev$matches_won[is.na(ev$matches_won)] <- 0L

  # Center matches won at treated mean
  treated_mean <- mean(ev$matches_won[ev$got_ll == 1], na.rm = TRUE)
  if (is.nan(treated_mean)) treated_mean <- 0
  ev$matches_won_c <- ev$matches_won - treated_mean

  message("    Censor range: ", min(ev$censor_date - ev$ev_date), "-",
          max(ev$censor_date - ev$ev_date), " days")
  ev
}

# Build all-events lists for censoring
gs_atp_all_events  <- tr$gs_atp$events
gs_wta_all_events  <- tr$gs_wta$events
nongs_atp_all_events <- if (!is.null(nongs_est)) {
  data.frame(player_id = nongs_est$player_id[nongs_est$tour == "ATP"],
             tourney_date = nongs_est$tourney_date[nongs_est$tour == "ATP"],
             stringsAsFactors = FALSE)
} else data.frame(player_id = character(), tourney_date = character())

nongs_wta_all_events <- if (!is.null(nongs_est)) {
  data.frame(player_id = nongs_est$player_id[nongs_est$tour == "WTA"],
             tourney_date = nongs_est$tourney_date[nongs_est$tour == "WTA"],
             stringsAsFactors = FALSE)
} else data.frame(player_id = character(), tourney_date = character())

ev_gs_atp <- build_event_table_firstll(
  tr$gs_atp$events, fl_gs_keys,
  list(gs = gs_atp_all_events, nongs = nongs_atp_all_events),
  "GS", "ATP", dose_data
)
ev_gs_wta <- build_event_table_firstll(
  tr$gs_wta$events, fl_gs_keys,
  list(gs = gs_wta_all_events, nongs = nongs_wta_all_events),
  "GS", "WTA", dose_data
)

# NonGS: events come from the first-LL NGS skeleton
# Merge v_hat from firstll skeleton
ngs_atp_ev <- fl_ngs[fl_ngs$tour == "ATP", ]
ngs_wta_ev <- fl_ngs[fl_ngs$tour == "WTA", ]

# Build pseudo-event tables for nonGS from the skeleton
build_ngs_event <- function(skel_df, all_events_list, tour_label, dose_df) {
  ev <- skel_df[, intersect(names(skel_df),
    c("player_id", "tourney_id", "tourney_date", "got_ll", "v_hat",
      "pre_rank_pts", "pre_elo", "pre_surf_elo", "player_age"))]
  if (is.numeric(ev$tourney_date)) {
    ev$ev_date <- as.Date(as.character(ev$tourney_date), format = "%Y%m%d")
  } else {
    ev$ev_date <- as.Date(ev$tourney_date)
  }
  ev$event_type <- "nonGS"

  # Censoring
  all_same <- all_events_list$nongs
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
    future <- censor_source[censor_source$player_id == pid &
                            censor_source$ev_date > d, ]
    if (nrow(future) > 0) {
      ev$next_same_type_date[i] <- min(future$ev_date)
    }
    cal_cap <- d + CALENDAR_CAP
    ev$censor_date[i] <- min(c(ev$next_same_type_date[i], cal_cap), na.rm = TRUE)
  }

  # Merge dose
  dose_sub <- dose_df[dose_df$tour == tour_label & dose_df$event_type == "nonGS",
                      c("player_id", "tourney_id", "dose", "matches_won")]
  ev <- merge(ev, dose_sub, by = c("player_id", "tourney_id"), all.x = TRUE)
  ev$dose[is.na(ev$dose)] <- 0
  ev$matches_won[is.na(ev$matches_won)] <- 0L

  treated_mean <- mean(ev$matches_won[ev$got_ll == 1], na.rm = TRUE)
  if (is.nan(treated_mean)) treated_mean <- 0
  ev$matches_won_c <- ev$matches_won - treated_mean

  message("  nonGS-", tour_label, ": ", nrow(ev), " first-LL events")
  ev
}

ev_nongs_atp <- build_ngs_event(
  ngs_atp_ev,
  list(gs = gs_atp_all_events, nongs = nongs_atp_all_events),
  "ATP", dose_data
)
ev_nongs_wta <- build_ngs_event(
  ngs_wta_ev,
  list(gs = gs_wta_all_events, nongs = nongs_wta_all_events),
  "WTA", dose_data
)


# ==============================================================================
# STEP 3: BUILD POST-EVENT MATCH-LEVEL DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 3: BUILD POST-EVENT MATCH DATA")
message(strrep("=", 70))

build_match_data <- function(ev_df, all_matches, elo_dt) {
  if (nrow(ev_df) == 0) return(data.frame())
  message("    Building match-level data for ", nrow(ev_df), " events...")

  am_dt <- as.data.table(all_matches[, c("winner_id", "loser_id", "tourney_id",
                                          "tourney_date_d", "surface",
                                          "winner_rank_points", "loser_rank_points",
                                          "winner_age", "loser_age")])

  w_idx <- split(seq_len(nrow(am_dt)), am_dt$winner_id)
  l_idx <- split(seq_len(nrow(am_dt)), am_dt$loser_id)

  match_rows <- vector("list", nrow(ev_df))

  for (i in seq_len(nrow(ev_df))) {
    if (i %% 200 == 0) message("      Event ", i, " / ", nrow(ev_df))
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
    if (n_w + n_l == 0) next

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
    focal_surf_elo_pre <- focal_elo_pre  # approximate

    match_rows[[i]] <- data.frame(
      event_idx        = i,
      player_id        = pid,
      tourney_id_event = ev_df$tourney_id[i],
      match_date       = match_dates,
      weeks_after      = weeks,
      horizon          = horizon,
      won              = as.integer(focal_is_winner),
      got_ll           = ev_df$got_ll[i],
      v_hat            = if ("v_hat" %in% names(ev_df)) ev_df$v_hat[i] else NA_real_,
      dose             = ev_df$dose[i],
      matches_won_c    = ev_df$matches_won_c[i],
      pts_diff         = (focal_pts_pre - opp_rank_pts) / 1000,
      elo_diff         = (focal_elo_pre - opp_elo) / 100,
      surface_elo_diff = (focal_surf_elo_pre - opp_elo) / 100,
      age_diff         = focal_age_pre - opp_age,
      surface_clay     = as.integer(md$surface == "Clay"),
      surface_grass    = as.integer(md$surface == "Grass"),
      focal_pts        = focal_pts_pre / 1000,
      focal_elo        = focal_elo_pre / 100,
      focal_surf_elo   = focal_surf_elo_pre / 100,
      focal_age        = focal_age_pre,
      h2h_win_prop     = 0.5,
      h2h_count        = 0L,
      stringsAsFactors = FALSE
    )
  }

  result <- data.table::rbindlist(match_rows, fill = TRUE)
  if (nrow(result) == 0) return(data.frame())
  result <- as.data.frame(result)

  result$pts_diff_sq        <- result$pts_diff^2
  result$elo_diff_sq        <- result$elo_diff^2
  result$surface_elo_diff_sq <- result$surface_elo_diff^2
  result$focal_pts_sq       <- result$focal_pts^2
  result$focal_elo_sq       <- result$focal_elo^2
  result$focal_surf_elo_sq  <- result$focal_surf_elo^2

  result$horizon <- factor(result$horizon, levels = HORIZON_LABS)

  message("    Built ", nrow(result), " match observations")
  result
}

message("  GS-ATP match data...")
md_gs_atp <- build_match_data(ev_gs_atp, atp_all, atp_elo)
message("  GS-WTA match data...")
md_gs_wta <- build_match_data(ev_gs_wta, wta_all, wta_elo)
message("  NonGS-ATP match data...")
md_nongs_atp <- build_match_data(ev_nongs_atp, atp_all, atp_elo)
message("  NonGS-WTA match data...")
md_nongs_wta <- build_match_data(ev_nongs_wta, wta_all, wta_elo)

message("\n  Match data sizes (first-LL):")
message("  GS-ATP: ", nrow(md_gs_atp))
message("  GS-WTA: ", nrow(md_gs_wta))
message("  NonGS-ATP: ", nrow(md_nongs_atp))
message("  NonGS-WTA: ", nrow(md_nongs_wta))


# ==============================================================================
# STEP 4: ESTIMATE DELTA MODELS (point estimates, robust SEs)
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 4: ESTIMATE DELTA MODELS")
message(strrep("=", 70))

# Base covariates (matches script 37 / win_model_helpers LOGIT_FORMULA)
BASE_COVS <- paste0(
  "pts_diff + pts_diff_sq + elo_diff + elo_diff_sq + ",
  "surface_elo_diff + surface_elo_diff_sq + ",
  "h2h_win_prop + h2h_count + age_diff + ",
  "surface_clay + surface_grass + ",
  "focal_pts + focal_pts_sq + focal_elo + focal_elo_sq + ",
  "focal_surf_elo + focal_surf_elo_sq + focal_age"
)

estimate_delta <- function(md, label, has_cf = FALSE) {
  message("\n  --- ", label, " (First-LL) ---")
  if (nrow(md) == 0) {
    message("    No data. Skipping.")
    return(NULL)
  }

  md <- md[complete.cases(md[, c("won", "pts_diff", "elo_diff",
                                  "surface_elo_diff", "got_ll",
                                  "focal_pts", "focal_elo")]), ]
  if (nrow(md) < 50) {
    message("    Too few complete cases (N=", nrow(md), "). Skipping.")
    return(NULL)
  }

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

res_gs_atp    <- estimate_delta(md_gs_atp, "GS-ATP", has_cf = FALSE)
res_gs_wta    <- estimate_delta(md_gs_wta, "GS-WTA", has_cf = FALSE)
res_nongs_atp <- estimate_delta(md_nongs_atp, "NonGS-ATP", has_cf = TRUE)
res_nongs_wta <- estimate_delta(md_nongs_wta, "NonGS-WTA", has_cf = TRUE)


# ==============================================================================
# STEP 5: GENERATE TABLES (using robust SEs from glm summary)
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 5: GENERATE TABLES")
message(strrep("=", 70))

generate_pooled_table <- function(res, label) {
  if (is.null(res)) return("")
  ct <- clustered_ct(res$pooled, res$data)
  # Column names differ between coeftest and coef(summary): normalize.
  colnames(ct) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")[seq_len(ncol(ct))]
  delta <- ct["got_ll", "Estimate"]
  delta_se <- ct["got_ll", "Std. Error"]
  pval <- ct["got_ll", "Pr(>|z|)"]

  lines <- c("\\begin{tabular}{lc}", "\\toprule",
             paste0(" & ", label, " \\\\"), "\\midrule")
  lines <- c(lines, paste0("$\\hat{\\delta}$ (LL entry) & ",
                           fmt(delta, 4), add_stars(pval), " \\\\"))
  lines <- c(lines, paste0(" & (", fmt(delta_se, 4), ") \\\\"))

  if (res$has_cf && "v_hat" %in% rownames(ct)) {
    rho <- ct["v_hat", "Estimate"]
    rho_se <- ct["v_hat", "Std. Error"]
    lines <- c(lines, paste0("$\\hat{\\rho}$ (CF) & ", fmt(rho, 4),
                             add_stars(ct["v_hat", "Pr(>|z|)"]), " \\\\"))
    lines <- c(lines, paste0(" & (", fmt(rho_se, 4), ") \\\\"))
  }

  lines <- c(lines, "\\midrule")
  lines <- c(lines, paste0("Matches & ", format(nobs(res$pooled), big.mark = ","), " \\\\"))
  lines <- c(lines, paste0("Players & ", length(unique(res$data$player_id)), " \\\\"))
  lines <- c(lines, "\\bottomrule", "\\end{tabular}")
  paste(lines, collapse = "\n")
}

generate_dose_table <- function(res, dose_var, dose_label, label) {
  if (is.null(res)) return("")
  if (dose_var == "matches_won_c") mod <- res$dose_mw
  else mod <- res$dose_pp
  ct <- clustered_ct(mod, res$data)
  colnames(ct) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")[seq_len(ncol(ct))]

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

generate_horizon_table <- function(res, label) {
  if (is.null(res)) return("")
  mod <- res$horizon
  ct <- clustered_ct(mod, res$data)
  colnames(ct) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")[seq_len(ncol(ct))]

  lines <- c("\\begin{tabular}{l*{5}{c}}", "\\toprule",
             paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"), "\\midrule")

  coef_vals <- se_vals <- character()
  for (h in HORIZON_LABS) {
    cname <- paste0("got_ll:horizon", h)
    if (cname %in% rownames(ct)) {
      coef_vals <- c(coef_vals, paste0(fmt(ct[cname, 1], 4), add_stars(ct[cname, 4])))
      se_vals   <- c(se_vals, paste0("(", fmt(ct[cname, 2], 4), ")"))
    } else {
      coef_vals <- c(coef_vals, "")
      se_vals   <- c(se_vals, "")
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
  list(res = res_gs_atp,    tag = "gs_atp",    label = "GS-ATP"),
  list(res = res_gs_wta,    tag = "gs_wta",    label = "GS-WTA"),
  list(res = res_nongs_atp, tag = "nongs_atp", label = "NonGS-ATP"),
  list(res = res_nongs_wta, tag = "nongs_wta", label = "NonGS-WTA")
)) {
  writeLines(generate_pooled_table(info$res, info$label),
             file.path(FIRSTLL_TABLES, paste0("table_delta_pooled_", info$tag, ".tex")))
  writeLines(generate_dose_table(info$res, "matches_won_c", "Matches won", info$label),
             file.path(FIRSTLL_TABLES, paste0("table_delta_dose_mw_", info$tag, ".tex")))
  writeLines(generate_dose_table(info$res, "dose", "Perf.\\ dose", info$label),
             file.path(FIRSTLL_TABLES, paste0("table_delta_dose_pp_", info$tag, ".tex")))
  writeLines(generate_horizon_table(info$res, info$label),
             file.path(FIRSTLL_TABLES, paste0("table_delta_horizon_", info$tag, ".tex")))
}
message("  16 tables saved to Tables_FirstLL/")


# ==============================================================================
# STEP 6: HORIZON FIGURES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 6: HORIZON FIGURES")
message(strrep("=", 70))

generate_horizon_figure <- function(res, label, tag) {
  if (is.null(res)) return(NULL)
  mod <- res$horizon
  ct <- clustered_ct(mod, res$data)
  colnames(ct) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")[seq_len(ncol(ct))]

  plot_data <- data.frame(
    horizon = HORIZONS,
    delta = NA_real_, se = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_
  )

  for (i in seq_along(HORIZON_LABS)) {
    h <- HORIZON_LABS[i]
    cname <- paste0("got_ll:horizon", h)
    if (cname %in% rownames(ct)) {
      plot_data$delta[i] <- ct[cname, 1]
      plot_data$se[i]    <- ct[cname, 2]
      plot_data$ci_lo[i] <- ct[cname, 1] - 1.96 * ct[cname, 2]
      plot_data$ci_hi[i] <- ct[cname, 1] + 1.96 * ct[cname, 2]
    }
  }
  plot_data <- plot_data[!is.na(plot_data$delta), ]
  if (nrow(plot_data) == 0) return(NULL)

  # AME approximation: delta * mean[p*(1-p)]
  md <- res$data
  p_hat <- predict(mod, type = "response")
  mean_deriv <- mean(p_hat * (1 - p_hat), na.rm = TRUE)

  plot_data$ame    <- plot_data$delta * mean_deriv
  plot_data$ame_lo <- plot_data$ci_lo * mean_deriv
  plot_data$ame_hi <- plot_data$ci_hi * mean_deriv

  p <- ggplot(plot_data, aes(x = horizon, y = ame)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(ymin = ame_lo, ymax = ame_hi), alpha = 0.15) +
    geom_point(size = 2) + geom_line() +
    scale_x_continuous(breaks = HORIZONS) +
    labs(x = "Weeks after LL event", y = "AME on win probability") +
    theme_paper()

  fig_path <- file.path(FIRSTLL_FIGURES, paste0("fig_delta_horizon_", tag, ".pdf"))
  ggsave(fig_path, p, width = 6.5, height = 4, device = cairo_pdf, bg = "transparent")
  message("  Figure saved: ", fig_path)
}

generate_horizon_figure(res_gs_atp,    "GS-ATP",    "gs_atp")
generate_horizon_figure(res_gs_wta,    "GS-WTA",    "gs_wta")
generate_horizon_figure(res_nongs_atp, "NonGS-ATP", "nongs_atp")
generate_horizon_figure(res_nongs_wta, "NonGS-WTA", "nongs_wta")


# ==============================================================================
# STEP 7: SAVE RESULTS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 7: SAVE RESULTS")
message(strrep("=", 70))

all_results <- list(
  gs_atp = res_gs_atp, gs_wta = res_gs_wta,
  nongs_atp = res_nongs_atp, nongs_wta = res_nongs_wta
)

# Strip raw data before saving
for (nm in names(all_results)) {
  if (!is.null(all_results[[nm]])) {
    all_results[[nm]]$data <- NULL
  }
}

saveRDS(all_results, file.path(FIRSTLL_CLEANED, "firstll_delta_model_results.rds"))
message("  Results saved: Data/cleaned/firstll/firstll_delta_model_results.rds")

# Summary
message("\n  === FIRST-LL DELTA MODEL SUMMARY ===")
for (nm in c("gs_atp", "gs_wta", "nongs_atp", "nongs_wta")) {
  res <- all_results[[nm]]
  if (is.null(res)) { message("  ", nm, ": SKIPPED"); next }
  mod <- res$pooled
  ct <- coef(summary(mod))
  delta <- ct["got_ll", "Estimate"]
  se <- ct["got_ll", "Std. Error"]
  pval <- ct["got_ll", "Pr(>|z|)"]
  st <- ifelse(pval < 0.01, "***", ifelse(pval < 0.05, "**",
               ifelse(pval < 0.1, "*", "")))
  message(sprintf("  %s: delta=%.4f%s (SE=%.4f, p=%.4f), N=%d matches",
                  nm, delta, st, se, pval, nobs(mod)))
}

message("\n", strrep("=", 70))
message("DONE: F04_delta_model.R complete")
message(strrep("=", 70))
