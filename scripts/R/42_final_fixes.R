# ==============================================================================
# 42_final_fixes.R
# Five targeted fixes:
#   1. ZPRE_GS = ZPRE_NONGS (no horizon interaction in GS dynamic models)
#   2. Uncentered matches_won in delta model
#   3. Cumulative horizon indicators in delta model
#   4. Horizon × dose spec with cumulative indicators + total effects table
#   5. Two histograms of π_ie (GS and non-GS)
# ==============================================================================

set.seed(20260327)

library(dplyr)
library(data.table)
library(fixest)
library(ggplot2)
library(here)

source(here("scripts", "R", "utils.R"))
source(here("scripts", "R", "win_model_helpers.R"))
summary_log <- character()

RAW_DIR     <- here("Data", "raw")
CLEANED_DIR <- here("Data", "cleaned")
TABLES_DIR  <- here("Tables")
FIGURES_DIR <- here("Figures")
OUTPUT_DIR  <- here("Output")


# ==============================================================================
# FIX 5: HISTOGRAMS OF π_ie (quick, no dependencies)
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 5: HISTOGRAMS OF pi_ie")
message(strrep("=", 70))

dose_data <- readRDS(file.path(CLEANED_DIR, "performance_dose.rds"))
treated <- dose_data[dose_data$pi_ie > 0 & dose_data$pi_ie < 1 & !is.na(dose_data$pi_ie), ]

gs_treated <- treated[treated$event_type == "GS", ]
ngs_treated <- treated[treated$event_type == "nonGS", ]
message("  GS treated with valid pi: ", nrow(gs_treated))
message("  NonGS treated with valid pi: ", nrow(ngs_treated))

p_gs <- ggplot(gs_treated, aes(x = pi_ie)) +
  geom_histogram(bins = 30, fill = "#56B4E9", color = "white", alpha = 0.8) +
  labs(x = expression(pi[ie]), y = "Count") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  annotate("text", x = 0.7, y = Inf, vjust = 2,
           label = paste0("N = ", nrow(gs_treated),
                          "\nMean = ", round(mean(gs_treated$pi_ie), 3),
                          "\nMedian = ", round(median(gs_treated$pi_ie), 3)),
           hjust = 0, size = 3.5, family = "serif") +
  theme_paper()

ggsave(file.path(FIGURES_DIR, "fig_pi_dist_gs.pdf"), p_gs,
       width = 6, height = 4, device = cairo_pdf)
message("  Saved fig_pi_dist_gs.pdf")

p_ngs <- ggplot(ngs_treated, aes(x = pi_ie)) +
  geom_histogram(bins = 30, fill = "#E69F00", color = "white", alpha = 0.8) +
  labs(x = expression(pi[ie]), y = "Count") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  annotate("text", x = 0.7, y = Inf, vjust = 2,
           label = paste0("N = ", nrow(ngs_treated),
                          "\nMean = ", round(mean(ngs_treated$pi_ie), 3),
                          "\nMedian = ", round(median(ngs_treated$pi_ie), 3)),
           hjust = 0, size = 3.5, family = "serif") +
  theme_paper()

ggsave(file.path(FIGURES_DIR, "fig_pi_dist_nongs.pdf"), p_ngs,
       width = 6, height = 4, device = cairo_pdf)
message("  Saved fig_pi_dist_nongs.pdf")


# ==============================================================================
# FIX 1: RE-ESTIMATE GS DYNAMIC MODELS WITH ZPRE AS MAIN EFFECTS (NO :horizon)
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 1: RE-ESTIMATE GS DYNAMICS (ZPRE as main effects)")
message(strrep("=", 70))

gs <- readRDS(file.path(CLEANED_DIR, "skeleton_gs_est_v5.rds"))
ngs <- readRDS(file.path(CLEANED_DIR, "skeleton_nongs_est_v8.rds"))

# Ensure scaled variables exist
ensure_scaled <- function(df) {
  df$pre_rank_pts_s <- df$pre_rank_pts / 1000
  df$pre_rank_pts_sq_s <- df$pre_rank_pts_s^2
  if (is.null(df$pre_elo_s) || all(is.na(df$pre_elo_s))) {
    # Check if pre_elo is already scaled (< 100) or raw (> 100)
    if (median(df$pre_elo, na.rm = TRUE) > 100) {
      df$pre_elo_s <- df$pre_elo / 100
    } else {
      df$pre_elo_s <- df$pre_elo
    }
  }
  df$pre_elo_sq_s <- df$pre_elo_s^2
  if (!"pre_surf_elo_s" %in% names(df) || all(is.na(df$pre_surf_elo_s))) {
    if ("pre_surf_elo" %in% names(df)) {
      if (median(df$pre_surf_elo, na.rm = TRUE) > 100) {
        df$pre_surf_elo_s <- df$pre_surf_elo / 100
      } else {
        df$pre_surf_elo_s <- df$pre_surf_elo
      }
    } else {
      df$pre_surf_elo_s <- df$pre_elo_s
    }
  }
  df$pre_surf_elo_sq_s <- df$pre_surf_elo_s^2
  df
}

gs <- ensure_scaled(gs)
ngs <- ensure_scaled(ngs)

# UNIFIED ZPRE: main effects only (same for GS and nonGS)
ZPRE <- paste0(
  "pre_rank_pts_s + pre_rank_pts_sq_s + ",
  "pre_elo_s + pre_elo_sq_s + ",
  "pre_surf_elo_s + pre_surf_elo_sq_s + ",
  "n_prior_gs_ll_won + n_prior_gs_ll_notwon + ",
  "n_prior_nongs_ll_won + n_prior_nongs_ll_notwon + ",
  "player_age"
)

outcomes_base <- c("points_change", "n_main_draws", "n_matches_250plus",
                   "elo_change", "win_pct")
outcome_labels <- c(
  "points_change" = "Ranking Pts $\\Delta$",
  "elo_change" = "Elo $\\Delta$",
  "n_main_draws" = "Main Draws",
  "n_matches_250plus" = "Matches 250+",
  "win_pct" = "Win \\%"
)
HORIZONS <- c(4, 8, 12, 26, 52)
HORIZON_LABS <- paste0(HORIZONS, "w")

# Stack function
stack_horizons <- function(data, outcomes = outcomes_base) {
  stacked <- list()
  for (h in HORIZONS) {
    hl <- paste0(h, "w")
    rd <- data
    rd$horizon <- hl
    rd$horizon_num <- h
    for (ob in outcomes) {
      cn <- paste0(ob, "_", h, "w")
      if (cn %in% names(data)) rd[[ob]] <- data[[cn]]
      else rd[[ob]] <- NA_real_
    }
    stacked[[hl]] <- rd
  }
  result <- do.call(rbind, stacked)
  result$horizon <- factor(result$horizon, levels = HORIZON_LABS)
  result
}

# Merge dose into skeletons
dose_data <- readRDS(file.path(CLEANED_DIR, "performance_dose.rds"))
merge_dose <- function(df, event_type_val) {
  dd <- dose_data[dose_data$event_type == event_type_val,
                  c("player_id", "tourney_id", "dose", "matches_won")]
  df <- merge(df, dd, by = c("player_id", "tourney_id"), all.x = TRUE)
  df$dose[is.na(df$dose)] <- 0
  df$matches_won_dose <- ifelse(is.na(df$matches_won), 0L, df$matches_won)
  df
}

gs <- merge_dose(gs, "GS")
ngs <- merge_dose(ngs, "nonGS")

# Stack
gs_atp <- gs[gs$tour == "ATP", ]
gs_wta <- gs[gs$tour == "WTA", ]
ngs_atp <- ngs[ngs$tour == "ATP", ]
ngs_wta <- ngs[ngs$tour == "WTA", ]

st_gs_atp <- stack_horizons(gs_atp)
st_gs_wta <- stack_horizons(gs_wta)
st_ngs_atp <- stack_horizons(ngs_atp)
st_ngs_wta <- stack_horizons(ngs_wta)

# Estimate dynamic models
run_dynamic <- function(stacked, fe_str, cf_term = NULL, label) {
  message("  ", label)
  results <- list()
  rho_results <- list()

  for (ob in outcomes_base) {
    sdata <- stacked[!is.na(stacked[[ob]]) &
                     !is.na(stacked$pre_rank_pts_s) &
                     !is.na(stacked$player_age), ]
    if (!is.null(cf_term)) sdata <- sdata[!is.na(sdata$v_hat), ]
    if (nrow(sdata) < 50) next

    rhs <- "got_ll:horizon"
    if (!is.null(cf_term)) rhs <- paste0(rhs, " + v_hat:horizon")
    rhs <- paste0(rhs, " + ", ZPRE)
    fml <- as.formula(paste0(ob, " ~ ", rhs, " | ", fe_str))

    mod <- tryCatch(feols(fml, data = sdata, cluster = ~player_id),
                    error = function(e) NULL)
    if (is.null(mod)) next

    ct <- as.data.frame(coeftable(mod)); ct$var <- rownames(ct)
    for (h in HORIZON_LABS) {
      tn <- paste0("got_ll:horizon", h)
      if (tn %in% ct$var) {
        rt <- ct[ct$var == tn, ]
        results[[paste0(ob, "_", h)]] <- data.frame(
          outcome = ob, horizon = h,
          coef = rt$Estimate, se = rt[["Std. Error"]],
          pval = rt[["Pr(>|t|)"]], stringsAsFactors = FALSE)
      }
      rn <- paste0("v_hat:horizon", h)
      if (rn %in% ct$var) {
        rr <- ct[ct$var == rn, ]
        rho_results[[paste0(ob, "_", h)]] <- data.frame(
          outcome = ob, horizon = h,
          rho = rr$Estimate, rho_se = rr[["Std. Error"]],
          rho_pval = rr[["Pr(>|t|)"]], stringsAsFactors = FALSE)
      }
    }
  }
  list(results = do.call(rbind, results), rho = do.call(rbind, rho_results))
}

dyn_gs_atp <- run_dynamic(st_gs_atp, "slam_year + horizon", NULL, "GS-ATP dynamics")
dyn_gs_wta <- run_dynamic(st_gs_wta, "slam_year + horizon", NULL, "GS-WTA dynamics")
dyn_ngs_atp <- run_dynamic(st_ngs_atp, "tourney_id + horizon", "v_hat", "NonGS-ATP dynamics")
dyn_ngs_wta <- run_dynamic(st_ngs_wta, "tourney_id + horizon", "v_hat", "NonGS-WTA dynamics")

# Generate compact dynamic tables
make_dynamic_table <- function(res, rho_res = NULL) {
  oo <- c("points_change", "elo_change", "n_main_draws", "n_matches_250plus", "win_pct")
  L <- c("\\begin{tabular}{l*{5}{c}}", "\\toprule",
         paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"), "\\midrule")
  for (ob in oo) {
    obr <- res[res$outcome == ob, ]
    if (nrow(obr) == 0) next
    cv <- sv <- character()
    for (h in HORIZON_LABS) {
      r <- obr[obr$horizon == h, ]
      if (nrow(r) == 0) { cv <- c(cv, ""); sv <- c(sv, "") }
      else {
        cv <- c(cv, paste0(fmt(r$coef[1], if (ob == "win_pct") 2 else 1), add_stars(r$pval[1])))
        sv <- c(sv, paste0("(", fmt(r$se[1], if (ob == "win_pct") 2 else 1), ")"))
      }
    }
    L <- c(L, paste0(outcome_labels[ob], " & ", paste(cv, collapse = " & "), " \\\\"))
    L <- c(L, paste0(" & ", paste(sv, collapse = " & "), " \\\\[0.3em]"))
  }
  if (!is.null(rho_res) && nrow(rho_res) > 0) {
    L <- c(L, "\\midrule")
    rv <- character()
    for (h in HORIZON_LABS) {
      rr <- rho_res[rho_res$outcome == "points_change" & rho_res$horizon == h, ]
      if (nrow(rr) == 0) rv <- c(rv, "")
      else rv <- c(rv, paste0(fmt(rr$rho[1], 2), add_stars(rr$rho_pval[1])))
    }
    L <- c(L, paste0("$\\hat{\\rho}_h$ (Pts) & ", paste(rv, collapse = " & "), " \\\\"))
  }
  L <- c(L, "\\bottomrule", "\\end{tabular}")
  paste(L, collapse = "\n")
}

writeLines(make_dynamic_table(dyn_gs_atp$results), file.path(TABLES_DIR, "table_dynamic_stacked_atp.tex"))
writeLines(make_dynamic_table(dyn_gs_wta$results), file.path(TABLES_DIR, "table_dynamic_stacked_wta.tex"))
writeLines(make_dynamic_table(dyn_ngs_atp$results, dyn_ngs_atp$rho), file.path(TABLES_DIR, "table_dynamic_stacked_nongs_atp.tex"))
writeLines(make_dynamic_table(dyn_ngs_wta$results, dyn_ngs_wta$rho), file.path(TABLES_DIR, "table_dynamic_stacked_nongs_wta.tex"))
message("  Dynamic tables saved.")


# ==============================================================================
# FIXES 2-4: DELTA MODEL WITH CUMULATIVE HORIZONS + UNCENTERED DOSE
# ==============================================================================
message("\n", strrep("=", 70))
message("FIXES 2-4: DELTA MODEL (cumulative horizons, uncentered dose)")
message(strrep("=", 70))

# Load match-level data from script 37 outputs or rebuild
# Check if delta_model_results.rds has data
delta_res_file <- file.path(CLEANED_DIR, "delta_model_results.rds")

# We need the actual match-level data. Load from the event tables and rebuild.
tr <- readRDS(file.path(CLEANED_DIR, "tournament_rebuild_results.rds"))

# Load pre-built match data if available, otherwise use what 37 built
# The match data from script 37 is in the model objects (stripped).
# We need to rebuild from event tables.

atp_all <- rbind(
  readRDS(file.path(RAW_DIR, "atp_main_matches.rds")) |> mutate(match_source = "main"),
  readRDS(file.path(RAW_DIR, "atp_qual_chall_matches.rds")) |> mutate(match_source = "qual")
)
wta_all <- rbind(
  readRDS(file.path(RAW_DIR, "wta_main_matches.rds")) |> mutate(match_source = "main"),
  readRDS(file.path(RAW_DIR, "wta_qual_itf_matches.rds")) |> mutate(match_source = "qual")
)

atp_elo <- as.data.table(readRDS(file.path(CLEANED_DIR, "elo_history.rds")))
wta_elo <- as.data.table(readRDS(file.path(CLEANED_DIR, "wta_elo_history.rds")))
setnames(atp_elo, c("player_id", "match_date", "elo"))
setnames(wta_elo, c("player_id", "match_date", "elo"))
setkey(atp_elo, player_id, match_date)
setkey(wta_elo, player_id, match_date)

elo_cache <- readRDS(file.path(CLEANED_DIR, "tournament_elo_cache.rds"))

# Pre-convert dates
safe_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (is.numeric(x)) return(as.Date(as.character(x), format = "%Y%m%d"))
  as.Date(x)
}
atp_all$tourney_date_d <- safe_date(atp_all$tourney_date)
wta_all$tourney_date_d <- safe_date(wta_all$tourney_date)

# Build event tables with censoring (reuse from script 37 pattern)
CALENDAR_CAP <- 365

build_events_with_censor <- function(events_df, all_same_events, tour_label) {
  ev <- events_df
  ev$ev_date <- safe_date(ev$tourney_date)

  all_same <- all_same_events
  all_same$ev_date <- safe_date(all_same$tourney_date)
  censor_src <- all_same[, c("player_id", "ev_date")]

  ev$censor_date <- as.Date(NA)
  for (i in seq_len(nrow(ev))) {
    pid <- ev$player_id[i]; d <- ev$ev_date[i]
    future <- censor_src[censor_src$player_id == pid & censor_src$ev_date > d, ]
    next_d <- if (nrow(future) > 0) min(future$ev_date) else as.Date(NA)
    ev$censor_date[i] <- min(c(next_d, d + CALENDAR_CAP), na.rm = TRUE)
  }

  # Merge dose (uncentered matches_won) — only if not already present
  if (!"dose" %in% names(ev)) {
    dd <- dose_data[dose_data$tour == tour_label,
                    c("player_id", "tourney_id", "dose", "matches_won")]
    ev <- merge(ev, dd, by = c("player_id", "tourney_id"), all.x = TRUE)
  }
  ev$dose[is.na(ev$dose)] <- 0
  if (!"matches_won" %in% names(ev)) ev$matches_won <- 0L
  ev$matches_won[is.na(ev$matches_won)] <- 0L
  ev
}

# GS events
ev_gs_atp <- build_events_with_censor(tr$gs_atp$events, tr$gs_atp$events, "ATP")
ev_gs_wta <- build_events_with_censor(tr$gs_wta$events, tr$gs_wta$events, "WTA")

# NonGS events — dose already merged from line 281
# Remove old dose columns to avoid conflicts, re-merge from dose_data
ngs_atp_ev <- ngs[ngs$tour == "ATP", ]
ngs_wta_ev <- ngs[ngs$tour == "WTA", ]
# Drop old dose columns if present
for (col in c("dose", "matches_won_dose", "matches_won")) {
  ngs_atp_ev[[col]] <- NULL
  ngs_wta_ev[[col]] <- NULL
}
ngs_atp_all_ev <- data.frame(player_id = ngs_atp_ev$player_id,
                              tourney_date = ngs_atp_ev$tourney_date, stringsAsFactors = FALSE)
ngs_wta_all_ev <- data.frame(player_id = ngs_wta_ev$player_id,
                              tourney_date = ngs_wta_ev$tourney_date, stringsAsFactors = FALSE)
ev_ngs_atp <- build_events_with_censor(ngs_atp_ev, ngs_atp_all_ev, "ATP")
ev_ngs_wta <- build_events_with_censor(ngs_wta_ev, ngs_wta_all_ev, "WTA")

# Build match data with CUMULATIVE horizon indicators
build_match_data_cumulative <- function(ev_df, all_matches, elo_dt, tour_prefix) {
  message("    Building match data (cumulative horizons) for ", nrow(ev_df), " events...")

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
    if (!is.null(widx)) { w_d <- am_dt$tourney_date_d[widx]; widx <- widx[w_d > ev_d & w_d <= censor_d] }
    if (!is.null(lidx)) { l_d <- am_dt$tourney_date_d[lidx]; lidx <- lidx[l_d > ev_d & l_d <= censor_d] }

    n_w <- length(widx); n_l <- length(lidx); n_tot <- n_w + n_l
    if (n_tot == 0) next

    all_idx <- c(widx, lidx)
    focal_is_winner <- c(rep(TRUE, n_w), rep(FALSE, n_l))
    md <- am_dt[all_idx, ]
    match_dates <- md$tourney_date_d
    weeks <- as.numeric(difftime(match_dates, ev_d, units = "days")) / 7

    # CUMULATIVE horizon indicators
    I_4w  <- as.integer(weeks <= 4)
    I_8w  <- as.integer(weeks <= 8)
    I_12w <- as.integer(weeks <= 12)
    I_26w <- as.integer(weeks <= 26)
    I_52w <- as.integer(weeks <= 52)

    # Opponent Elo
    opp_ids <- ifelse(focal_is_winner, md$loser_id, md$winner_id)
    opp_elo <- get_pre_match_elo(opp_ids, match_dates, elo_dt)
    opp_elo[is.na(opp_elo)] <- 1500
    opp_rank_pts <- ifelse(focal_is_winner,
                           as.numeric(md$loser_rank_points),
                           as.numeric(md$winner_rank_points))
    opp_rank_pts[is.na(opp_rank_pts)] <- 0
    opp_age <- ifelse(focal_is_winner, as.numeric(md$loser_age), as.numeric(md$winner_age))
    opp_age[is.na(opp_age)] <- 25

    focal_elo_pre <- ev_df$pre_elo[i]
    focal_pts_pre <- ev_df$pre_rank_pts[i]
    focal_age_pre <- ev_df$player_age[i]
    focal_surf_elo_pre <- if ("pre_surf_elo" %in% names(ev_df)) ev_df$pre_surf_elo[i] else focal_elo_pre
    if (is.na(focal_surf_elo_pre)) focal_surf_elo_pre <- focal_elo_pre

    # Scale focal pre-event values
    if (focal_elo_pre > 100) focal_elo_pre_100 <- focal_elo_pre / 100 else focal_elo_pre_100 <- focal_elo_pre
    if (focal_surf_elo_pre > 100) focal_surf_elo_pre_100 <- focal_surf_elo_pre / 100 else focal_surf_elo_pre_100 <- focal_surf_elo_pre

    match_rows[[i]] <- data.frame(
      event_idx = i, player_id = pid,
      won = as.integer(focal_is_winner),
      got_ll = ev_df$got_ll[i],
      v_hat = if ("v_hat" %in% names(ev_df)) ev_df$v_hat[i] else NA_real_,
      dose = ev_df$dose[i],
      matches_won = ev_df$matches_won[i],
      weeks_after = weeks,
      # Cumulative indicators
      I_4w = I_4w, I_8w = I_8w, I_12w = I_12w, I_26w = I_26w, I_52w = I_52w,
      # Treatment × cumulative
      D_4w  = ev_df$got_ll[i] * I_4w,
      D_8w  = ev_df$got_ll[i] * I_8w,
      D_12w = ev_df$got_ll[i] * I_12w,
      D_26w = ev_df$got_ll[i] * I_26w,
      D_52w = ev_df$got_ll[i] * I_52w,
      # BASE_COVS
      pts_diff = (focal_pts_pre - opp_rank_pts) / 1000,
      elo_diff = (focal_elo_pre_100 - opp_elo / 100),
      surface_elo_diff = (focal_surf_elo_pre_100 - opp_elo / 100),
      age_diff = focal_age_pre - opp_age,
      surface_clay = as.integer(md$surface == "Clay"),
      surface_grass = as.integer(md$surface == "Grass"),
      focal_pts = focal_pts_pre / 1000,
      focal_elo = focal_elo_pre_100,
      focal_surf_elo = focal_surf_elo_pre_100,
      focal_age = focal_age_pre,
      h2h_win_prop = 0.5, h2h_count = 0L,
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

  # CF × cumulative indicators
  if ("v_hat" %in% names(result)) {
    result$V_4w  <- result$v_hat * result$I_4w
    result$V_8w  <- result$v_hat * result$I_8w
    result$V_12w <- result$v_hat * result$I_12w
    result$V_26w <- result$v_hat * result$I_26w
    result$V_52w <- result$v_hat * result$I_52w
  }

  message("    Built ", nrow(result), " match observations")
  result
}

message("  Building match data...")
md_gs_atp <- build_match_data_cumulative(ev_gs_atp, atp_all, atp_elo, "ATP")
md_gs_wta <- build_match_data_cumulative(ev_gs_wta, wta_all, wta_elo, "WTA")
md_ngs_atp <- build_match_data_cumulative(ev_ngs_atp, atp_all, atp_elo, "ATP")
md_ngs_wta <- build_match_data_cumulative(ev_ngs_wta, wta_all, wta_elo, "WTA")

message("  Match data: GS-ATP=", nrow(md_gs_atp), " GS-WTA=", nrow(md_gs_wta),
        " NonGS-ATP=", nrow(md_ngs_atp), " NonGS-WTA=", nrow(md_ngs_wta))

# BASE_COVS string
BASE <- "pts_diff + pts_diff_sq + elo_diff + elo_diff_sq + surface_elo_diff + surface_elo_diff_sq + h2h_win_prop + h2h_count + age_diff + surface_clay + surface_grass + focal_pts + focal_pts_sq + focal_elo + focal_elo_sq + focal_surf_elo + focal_surf_elo_sq + focal_age"

# Estimate delta models with CUMULATIVE horizon indicators
estimate_all <- function(md, label, has_cf) {
  message("\n  --- ", label, " ---")
  if (nrow(md) == 0) return(NULL)
  md <- md[complete.cases(md[, c("won", "pts_diff", "elo_diff", "got_ll")]), ]

  D_terms <- "D_4w + D_8w + D_12w + D_26w + D_52w"
  V_terms <- if (has_cf) " + V_4w + V_8w + V_12w + V_26w + V_52w" else ""

  # (1) Pooled (single D, no horizon variation)
  cf1 <- if (has_cf) " + v_hat" else ""
  fml1 <- as.formula(paste0("won ~ ", BASE, " + got_ll", cf1))
  mod_pooled <- glm(fml1, data = md, family = binomial)
  message("    Pooled: delta=", round(coef(mod_pooled)["got_ll"], 4))

  # (2) Dose (matches won, UNCENTERED)
  cf2 <- if (has_cf) " + v_hat + v_hat:matches_won" else ""
  fml2 <- as.formula(paste0("won ~ ", BASE, " + got_ll + got_ll:matches_won", cf2))
  mod_dose_mw <- glm(fml2, data = md, family = binomial)
  message("    Dose(wins): delta=", round(coef(mod_dose_mw)["got_ll"], 4))

  # (3) Dose (perf prob, -logit(π))
  cf3 <- if (has_cf) " + v_hat + v_hat:dose" else ""
  fml3 <- as.formula(paste0("won ~ ", BASE, " + got_ll + got_ll:dose", cf3))
  mod_dose_pp <- glm(fml3, data = md, family = binomial)
  message("    Dose(perf): delta=", round(coef(mod_dose_pp)["got_ll"], 4))

  # (4) Cumulative horizon heterogeneity
  fml4 <- as.formula(paste0("won ~ ", BASE, " + ", D_terms, V_terms))
  mod_horizon <- glm(fml4, data = md, family = binomial)
  d_coefs <- coef(mod_horizon)[c("D_4w", "D_8w", "D_12w", "D_26w", "D_52w")]
  message("    Horizon (incremental): ", paste(names(d_coefs), round(d_coefs, 4), sep = "=", collapse = ", "))
  # Total effects
  total_4w <- sum(d_coefs[c("D_4w", "D_8w", "D_12w", "D_26w", "D_52w")], na.rm = TRUE)
  total_8w <- sum(d_coefs[c("D_8w", "D_12w", "D_26w", "D_52w")], na.rm = TRUE)
  total_12w <- sum(d_coefs[c("D_12w", "D_26w", "D_52w")], na.rm = TRUE)
  total_26w <- sum(d_coefs[c("D_26w", "D_52w")], na.rm = TRUE)
  total_52w <- d_coefs["D_52w"]
  message("    Total effects: 4w=", round(total_4w, 4), " 8w=", round(total_8w, 4),
          " 12w=", round(total_12w, 4), " 26w=", round(total_26w, 4), " 52w=", round(total_52w, 4))

  # (5) Cumulative horizon × dose
  dose_D <- paste0("D_4w + D_8w + D_12w + D_26w + D_52w + ",
                   "D_4w:dose + D_8w:dose + D_12w:dose + D_26w:dose + D_52w:dose")
  dose_V <- if (has_cf) {
    paste0(" + V_4w + V_8w + V_12w + V_26w + V_52w + ",
           "V_4w:dose + V_8w:dose + V_12w:dose + V_26w:dose + V_52w:dose")
  } else ""
  fml5 <- as.formula(paste0("won ~ ", BASE, " + ", dose_D, dose_V))
  mod_horizon_dose <- glm(fml5, data = md, family = binomial)

  list(pooled = mod_pooled, dose_mw = mod_dose_mw, dose_pp = mod_dose_pp,
       horizon = mod_horizon, horizon_dose = mod_horizon_dose,
       data = md, label = label, has_cf = has_cf)
}

res_gs_atp <- estimate_all(md_gs_atp, "GS-ATP", FALSE)
res_gs_wta <- estimate_all(md_gs_wta, "GS-WTA", FALSE)
res_ngs_atp <- estimate_all(md_ngs_atp, "NonGS-ATP", TRUE)
res_ngs_wta <- estimate_all(md_ngs_wta, "NonGS-WTA", TRUE)

# Bootstrap
N_BOOT <- 200

bootstrap_cumulative <- function(res, n_boot = N_BOOT) {
  if (is.null(res)) return(NULL)
  md <- res$data; has_cf <- res$has_cf
  players <- unique(md$player_id); np <- length(players)

  D_terms <- "D_4w + D_8w + D_12w + D_26w + D_52w"
  V_terms <- if (has_cf) " + V_4w + V_8w + V_12w + V_26w + V_52w" else ""
  fml <- as.formula(paste0("won ~ ", BASE, " + ", D_terms, V_terms))

  boot_d <- matrix(NA, n_boot, 5)
  colnames(boot_d) <- c("D_4w", "D_8w", "D_12w", "D_26w", "D_52w")

  message("  Bootstrap ", res$label, "...")
  for (b in seq_len(n_boot)) {
    if (b %% 50 == 0) message("    Boot ", b, "/", n_boot)
    bp <- sample(players, np, replace = TRUE)
    bi <- unlist(lapply(bp, function(p) which(md$player_id == p)))
    bd <- md[bi, ]
    m <- tryCatch(glm(fml, data = bd, family = binomial), error = function(e) NULL)
    if (!is.null(m)) {
      for (k in 1:5) {
        cn <- colnames(boot_d)[k]
        if (cn %in% names(coef(m))) boot_d[b, k] <- coef(m)[cn]
      }
    }
  }

  list(
    incremental_se = apply(boot_d, 2, sd, na.rm = TRUE),
    # Total effect SEs via bootstrap
    total_boot = cbind(
      `4w` = rowSums(boot_d, na.rm = TRUE),
      `8w` = rowSums(boot_d[, 2:5, drop = FALSE], na.rm = TRUE),
      `12w` = rowSums(boot_d[, 3:5, drop = FALSE], na.rm = TRUE),
      `26w` = rowSums(boot_d[, 4:5, drop = FALSE], na.rm = TRUE),
      `52w` = boot_d[, 5]
    )
  )
}

boot_gs_atp <- bootstrap_cumulative(res_gs_atp)
boot_gs_wta <- bootstrap_cumulative(res_gs_wta)
boot_ngs_atp <- bootstrap_cumulative(res_ngs_atp)
boot_ngs_wta <- bootstrap_cumulative(res_ngs_wta)

# Generate consolidated table (pooled + dose_mw + dose_pp as 3 columns)
generate_consolidated <- function(res, label) {
  if (is.null(res)) return("")
  mods <- list(res$pooled, res$dose_mw, res$dose_pp)
  mod_names <- c("(1) Pooled", "(2) Dose: Wins", "(3) Dose: Perf")

  L <- c(paste0("\\begin{tabular}{l", paste(rep("c", 3), collapse = ""), "}"),
         "\\toprule",
         paste0(" & ", paste(mod_names, collapse = " & "), " \\\\"), "\\midrule")

  # Treatment rows
  for (j in 1:3) {
    ct <- coef(summary(mods[[j]]))
    if ("got_ll" %in% rownames(ct)) {
      est <- fmt(ct["got_ll", 1], 4); se <- fmt(ct["got_ll", 2], 4)
      st <- add_stars(ct["got_ll", 4])
    } else { est <- ""; se <- ""; st <- "" }
    if (j == 1) {
      L <- c(L, paste0("$D_{ie}$ & ", est, st,
                       " & ", fmt(coef(summary(mods[[2]]))["got_ll", 1], 4), add_stars(coef(summary(mods[[2]]))["got_ll", 4]),
                       " & ", fmt(coef(summary(mods[[3]]))["got_ll", 1], 4), add_stars(coef(summary(mods[[3]]))["got_ll", 4]), " \\\\"))
      L <- c(L, paste0(" & (", fmt(ct["got_ll", 2], 4), ")",
                       " & (", fmt(coef(summary(mods[[2]]))["got_ll", 2], 4), ")",
                       " & (", fmt(coef(summary(mods[[3]]))["got_ll", 2], 4), ") \\\\"))
      break
    }
  }

  # Dose interaction rows
  ct2 <- coef(summary(mods[[2]]))
  mw_name <- grep("got_ll:matches_won", rownames(ct2), value = TRUE)[1]
  if (!is.na(mw_name) && mw_name %in% rownames(ct2)) {
    L <- c(L, paste0("$D_{ie} \\times$ Wins &  & ",
                     fmt(ct2[mw_name, 1], 4), add_stars(ct2[mw_name, 4]), " &  \\\\"))
    L <- c(L, paste0(" &  & (", fmt(ct2[mw_name, 2], 4), ") &  \\\\"))
  }

  ct3 <- coef(summary(mods[[3]]))
  dp_name <- grep("got_ll:dose", rownames(ct3), value = TRUE)[1]
  if (!is.na(dp_name) && dp_name %in% rownames(ct3)) {
    L <- c(L, paste0("$D_{ie} \\times$ Dose &  &  & ",
                     fmt(ct3[dp_name, 1], 4), add_stars(ct3[dp_name, 4]), " \\\\"))
    L <- c(L, paste0(" &  &  & (", fmt(ct3[dp_name, 2], 4), ") \\\\"))
  }

  L <- c(L, "\\midrule",
         paste0("Matches & ", format(nobs(mods[[1]]), big.mark = ","),
                " & ", format(nobs(mods[[2]]), big.mark = ","),
                " & ", format(nobs(mods[[3]]), big.mark = ","), " \\\\"))
  L <- c(L, "\\bottomrule", "\\end{tabular}")
  paste(L, collapse = "\n")
}

writeLines(generate_consolidated(res_gs_atp, "GS-ATP"), file.path(TABLES_DIR, "table_tournament_consolidated_gs_atp.tex"))
writeLines(generate_consolidated(res_gs_wta, "GS-WTA"), file.path(TABLES_DIR, "table_tournament_consolidated_gs_wta.tex"))
writeLines(generate_consolidated(res_ngs_atp, "NonGS-ATP"), file.path(TABLES_DIR, "table_tournament_consolidated_nongs_atp.tex"))
writeLines(generate_consolidated(res_ngs_wta, "NonGS-WTA"), file.path(TABLES_DIR, "table_tournament_consolidated_nongs_wta.tex"))
message("  Consolidated tables saved.")

# Generate horizon table with total effects
generate_horizon_total <- function(res, boot, label) {
  if (is.null(res) || is.null(boot)) return("")
  mod <- res$horizon
  ct <- coef(summary(mod))

  d_names <- c("D_4w", "D_8w", "D_12w", "D_26w", "D_52w")
  d_coefs <- sapply(d_names, function(n) if (n %in% rownames(ct)) ct[n, 1] else NA)
  d_se <- boot$incremental_se

  # Total effects
  totals <- c(
    sum(d_coefs, na.rm = TRUE),
    sum(d_coefs[2:5], na.rm = TRUE),
    sum(d_coefs[3:5], na.rm = TRUE),
    sum(d_coefs[4:5], na.rm = TRUE),
    d_coefs[5]
  )
  total_se <- apply(boot$total_boot, 2, sd, na.rm = TRUE)
  total_p <- 2 * pnorm(-abs(totals / total_se))

  L <- c("\\begin{tabular}{l*{5}{c}}", "\\toprule",
         paste0(" & 4w & 8w & 12w & 26w & 52w \\\\"), "\\midrule")

  # Incremental row
  cv <- sv <- character()
  for (k in 1:5) {
    cv <- c(cv, paste0(fmt(d_coefs[k], 4), add_stars(2 * pnorm(-abs(d_coefs[k] / d_se[k])))))
    sv <- c(sv, paste0("(", fmt(d_se[k], 4), ")"))
  }
  L <- c(L, paste0("$\\delta_h$ (incremental) & ", paste(cv, collapse = " & "), " \\\\"))
  L <- c(L, paste0(" & ", paste(sv, collapse = " & "), " \\\\[0.3em]"))

  # Total row
  tv <- tsv <- character()
  for (k in 1:5) {
    tv <- c(tv, paste0(fmt(totals[k], 4), add_stars(total_p[k])))
    tsv <- c(tsv, paste0("(", fmt(total_se[k], 4), ")"))
  }
  L <- c(L, paste0("Total effect & ", paste(tv, collapse = " & "), " \\\\"))
  L <- c(L, paste0(" & ", paste(tsv, collapse = " & "), " \\\\"))

  L <- c(L, "\\midrule",
         paste0("Matches & \\multicolumn{5}{c}{", format(nobs(mod), big.mark = ","), "} \\\\"))
  L <- c(L, "\\bottomrule", "\\end{tabular}")
  paste(L, collapse = "\n")
}

writeLines(generate_horizon_total(res_gs_atp, boot_gs_atp, "GS-ATP"), file.path(TABLES_DIR, "table_tournament_firstll_horizon_gs_atp.tex"))
writeLines(generate_horizon_total(res_gs_wta, boot_gs_wta, "GS-WTA"), file.path(TABLES_DIR, "table_tournament_firstll_horizon_gs_wta.tex"))
writeLines(generate_horizon_total(res_ngs_atp, boot_ngs_atp, "NonGS-ATP"), file.path(TABLES_DIR, "table_tournament_firstll_horizon_nongs_atp.tex"))
writeLines(generate_horizon_total(res_ngs_wta, boot_ngs_wta, "NonGS-WTA"), file.path(TABLES_DIR, "table_tournament_firstll_horizon_nongs_wta.tex"))
message("  Horizon tables saved.")

# Generate horizon × dose table with total effects at dose=0 and mean dose
generate_horizon_dose_total <- function(res_atp, boot_atp, res_wta, boot_wta, label) {
  # For each sample, extract cumulative D and D:dose coefficients
  build_cols <- function(res, boot) {
    if (is.null(res)) return(list(d = rep(NA, 5), dd = rep(NA, 5), total_0 = rep(NA, 5), total_mean = rep(NA, 5)))
    mod <- res$horizon_dose
    ct <- coef(mod)
    d_names <- c("D_4w", "D_8w", "D_12w", "D_26w", "D_52w")
    dd_names <- paste0(d_names, ":dose")

    d_c <- sapply(d_names, function(n) if (n %in% names(ct)) ct[n] else NA)
    dd_c <- sapply(dd_names, function(n) if (n %in% names(ct)) ct[n] else NA)

    mean_dose <- mean(res$data$dose[res$data$got_ll == 1 & res$data$dose > 0], na.rm = TRUE)

    # Total at dose=0: sum of d_c[k:5]
    total_0 <- c(sum(d_c, na.rm = TRUE), sum(d_c[2:5], na.rm = TRUE),
                 sum(d_c[3:5], na.rm = TRUE), sum(d_c[4:5], na.rm = TRUE), d_c[5])
    # Total at mean dose: sum of (d_c[k:5] + dd_c[k:5]*mean_dose)
    total_mean <- c(
      sum(d_c + dd_c * mean_dose, na.rm = TRUE),
      sum((d_c + dd_c * mean_dose)[2:5], na.rm = TRUE),
      sum((d_c + dd_c * mean_dose)[3:5], na.rm = TRUE),
      sum((d_c + dd_c * mean_dose)[4:5], na.rm = TRUE),
      (d_c + dd_c * mean_dose)[5]
    )

    list(d = d_c, dd = dd_c, total_0 = total_0, total_mean = total_mean, mean_dose = mean_dose)
  }

  atp <- build_cols(res_atp, boot_atp)
  wta <- build_cols(res_wta, boot_wta)

  L <- c("\\begin{tabular}{l*{5}{c}}", "\\toprule",
         " & 4w & 8w & 12w & 26w & 52w \\\\", "\\midrule",
         "\\multicolumn{6}{l}{\\textit{ATP}} \\\\")

  # ATP total at dose=0
  cv0 <- sapply(atp$total_0, function(x) fmt(x, 4))
  L <- c(L, paste0("\\quad Total at dose$=0$ & ", paste(cv0, collapse = " & "), " \\\\"))
  cvm <- sapply(atp$total_mean, function(x) fmt(x, 4))
  L <- c(L, paste0("\\quad Total at mean dose & ", paste(cvm, collapse = " & "), " \\\\[0.3em]"))

  L <- c(L, "\\multicolumn{6}{l}{\\textit{WTA}} \\\\")
  cv0w <- sapply(wta$total_0, function(x) fmt(x, 4))
  L <- c(L, paste0("\\quad Total at dose$=0$ & ", paste(cv0w, collapse = " & "), " \\\\"))
  cvmw <- sapply(wta$total_mean, function(x) fmt(x, 4))
  L <- c(L, paste0("\\quad Total at mean dose & ", paste(cvmw, collapse = " & "), " \\\\"))

  L <- c(L, "\\bottomrule", "\\end{tabular}")
  paste(L, collapse = "\n")
}

writeLines(generate_horizon_dose_total(res_gs_atp, boot_gs_atp, res_gs_wta, boot_gs_wta, "GS"),
           file.path(TABLES_DIR, "table_horizon_dose_gs.tex"))
writeLines(generate_horizon_dose_total(res_ngs_atp, boot_ngs_atp, res_ngs_wta, boot_ngs_wta, "NonGS"),
           file.path(TABLES_DIR, "table_horizon_dose_nongs.tex"))
message("  Horizon × dose tables saved.")

# Save results
saveRDS(list(
  gs_atp = res_gs_atp, gs_wta = res_gs_wta,
  ngs_atp = res_ngs_atp, ngs_wta = res_ngs_wta,
  boot_gs_atp = boot_gs_atp, boot_gs_wta = boot_gs_wta,
  boot_ngs_atp = boot_ngs_atp, boot_ngs_wta = boot_ngs_wta
), file.path(CLEANED_DIR, "delta_model_results_v2.rds"))


# ==============================================================================
# DONE
# ==============================================================================
message("\n", strrep("=", 70))
message("ALL FIXES COMPLETE")
message(strrep("=", 70))
