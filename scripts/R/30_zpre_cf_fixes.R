# ==============================================================================
# 30_zpre_cf_fixes.R
# Six critical fixes to the Lucky Losers analysis pipeline:
#   FIX 1: Expand Z^pre to include Elo (linear + quadratic), full LL history,
#          and player age in all specifications
#   FIX 2: Fix censoring rule -- censor at next final-round qualifying loss
#          at an LL-granting event of the SAME TYPE (GS or non-GS)
#   FIX 3: Control function (CF) approach for ALL non-GS specifications
#   FIX 4: Split summary stats into 4 tables (GS ATP, GS WTA, nonGS ATP, nonGS WTA)
#   FIX 5: Fix LL history variable labels in all tables
#   FIX 6: Re-estimate all dynamics, hetero, dose, balance, immediate tables
#
# Inputs:
#   Data/cleaned/skeleton_all_losers_est.rds
#   Data/cleaned/skeleton_gs_est.rds
#   Data/cleaned/skeleton_nongs_est.rds
#
# Outputs:
#   Data/cleaned/skeleton_gs_est_v2.rds
#   Data/cleaned/skeleton_nongs_est_v2.rds
#   Tables/table_dynamic_stacked_atp.tex
#   Tables/table_dynamic_stacked_wta.tex
#   Tables/table_dynamic_stacked_nongs_atp.tex
#   Tables/table_dynamic_stacked_nongs_wta.tex
#   Tables/table_hetero_stacked_atp.tex
#   Tables/table_hetero_stacked_wta.tex
#   Tables/table_hetero_stacked_nongs_atp.tex
#   Tables/table_hetero_stacked_nongs_wta.tex
#   Tables/table_dose_stacked.tex
#   Tables/table_dose_stacked_nongs.tex
#   Tables/table_sumstats_gs_atp.tex
#   Tables/table_sumstats_gs_wta.tex
#   Tables/table_sumstats_nongs_atp.tex
#   Tables/table_sumstats_nongs_wta.tex
#   Tables/table_verified_stacked.tex
#   Tables/table_firstll_stacked_atp.tex
#   Tables/table_firstll_stacked_wta.tex
#   Figures/fig_event_study_atp.pdf
#   Figures/fig_event_study_wta.pdf
#   Data/cleaned/zpre_cf_fixes_results.rds
#   Output/zpre_cf_fixes_summary.md
#
# Dependencies: dplyr, tidyr, fixest, ggplot2, here
# ==============================================================================

set.seed(20260326)

# --- Packages -----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(fixest)
library(ggplot2)
library(here)

# --- Shared helpers -----------------------------------------------------------
source(here("scripts", "R", "utils.R"))
summary_log <- character()

# --- Paths --------------------------------------------------------------------
CLEANED_DIR <- here("Data", "cleaned")
TABLES_DIR  <- here("Tables")
FIGURES_DIR <- here("Figures")
OUTPUT_DIR  <- here("Output")
for (d in c(CLEANED_DIR, TABLES_DIR, FIGURES_DIR, OUTPUT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# --- Outcome definitions ------------------------------------------------------
outcomes_base  <- c("points_change", "n_main_draws", "n_matches_250plus", "elo_change")
outcome_labels <- c(
  "points_change"     = "Ranking points $\\Delta$",
  "n_main_draws"      = "Main draws entered",
  "n_matches_250plus" = "Matches at 250+",
  "elo_change"        = "Elo $\\Delta$"
)
HORIZONS     <- c(4, 8, 12, 26, 52)
HORIZON_LABS <- paste0(HORIZONS, "w")

# Full Z^pre formula fragment (FIX 1)
ZPRE_FULL <- paste0("pre_rank_pts + pre_rank_pts_sq + pre_elo + pre_elo_sq",
                     " + n_prior_gs_ll_won + n_prior_gs_ll_notwon",
                     " + n_prior_nongs_ll_won + n_prior_nongs_ll_notwon",
                     " + player_age")

# ==============================================================================
# LOAD DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("LOADING DATA")
message(strrep("=", 70))

all_losers_est <- readRDS(file.path(CLEANED_DIR, "skeleton_all_losers_est.rds"))
gs_est_old     <- readRDS(file.path(CLEANED_DIR, "skeleton_gs_est.rds"))
nongs_est_old  <- readRDS(file.path(CLEANED_DIR, "skeleton_nongs_est.rds"))

message("  all_losers_est: N = ", nrow(all_losers_est))
message("  gs_est_old: N = ", nrow(gs_est_old))
message("  nongs_est_old: N = ", nrow(nongs_est_old))

slog("## Data loaded")
slog("- all_losers_est: N = ", nrow(all_losers_est))
slog("- gs_est_old (GS): N = ", nrow(gs_est_old))
slog("- nongs_est_old (non-GS): N = ", nrow(nongs_est_old))
slog("")


# ==============================================================================
# FIX 1: EXPAND Z^pre
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 1: EXPAND Z^pre")
message(strrep("=", 70))

# 1a. Construct "not won" LL history from existing "opp" and "won" counts
# n_prior_gs_ll_opp = total prior GS final-round qual losses at LL-granting events
# n_prior_gs_ll_won = how many of those resulted in an LL spot
# n_prior_gs_ll_notwon = opp - won
all_losers_est <- all_losers_est |>
  mutate(
    n_prior_gs_ll_notwon    = n_prior_gs_ll_opp - n_prior_gs_ll_won,
    n_prior_nongs_ll_notwon = n_prior_nongs_ll_opp - n_prior_nongs_ll_won
  )

# 1b. Ensure pre_elo exists and is not missing; impute with sample median
med_elo <- median(all_losers_est$pre_elo, na.rm = TRUE)
if (is.na(med_elo)) med_elo <- 1500
all_losers_est <- all_losers_est |>
  mutate(
    pre_elo    = coalesce(pre_elo, med_elo),
    pre_elo_sq = pre_elo^2
  )

n_elo_imputed <- sum(is.na(gs_est_old$pre_elo) | gs_est_old$pre_elo == 1500)
slog("## FIX 1: Expanded Z^pre")
slog("- Added n_prior_gs_ll_notwon, n_prior_nongs_ll_notwon")
slog("- Elo imputed for ~", n_elo_imputed, " observations (set to median = ", round(med_elo), ")")
slog("- Full Z^pre: ", ZPRE_FULL)
slog("")


# ==============================================================================
# FIX 2: FIX CENSORING RULE
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 2: FIX CENSORING RULE")
message(strrep("=", 70))

# Old censoring: next qualifying loss at ANY event (any type)
# New censoring: next final-round qualifying loss at an LL-granting event
#                of the SAME TYPE (GS for GS episodes, non-GS for non-GS episodes)
#
# The all_losers_est already contains ALL final-round qualifying losers at
# LL-granting events (n_ll_slots > 0). We just need to find the next one
# of the same type.

all_losers_est <- all_losers_est |>
  mutate(event_type = if_else(tourney_level == "G", "GS", "nonGS"))

# Compute weeks to next same-type event
all_losers_est <- all_losers_est |>
  arrange(player_id, event_date) |>
  group_by(player_id, event_type) |>
  mutate(
    next_same_type_date = lead(event_date),
    weeks_to_next_same_type = as.numeric(difftime(next_same_type_date, event_date,
                                                   units = "weeks"))
  ) |>
  ungroup()

# Compare old vs new censoring
old_censored_52 <- sum(!is.na(all_losers_est$weeks_to_next_ll) &
                         all_losers_est$weeks_to_next_ll < 52, na.rm = TRUE)
new_censored_52 <- sum(!is.na(all_losers_est$weeks_to_next_same_type) &
                         all_losers_est$weeks_to_next_same_type < 52, na.rm = TRUE)

slog("## FIX 2: Censoring rule fixed")
slog("- Old censoring (any next qual loss) at 52w: ", old_censored_52, " observations censored")
slog("- New censoring (same-type LL event) at 52w: ", new_censored_52, " observations censored")
slog("- Difference: ", old_censored_52 - new_censored_52, " fewer censored")
slog("")

message("  Old censored @52w: ", old_censored_52)
message("  New censored @52w: ", new_censored_52)

# Re-apply censoring with the corrected rule
# First, remove old _cens columns
cens_cols <- grep("_cens$", names(all_losers_est), value = TRUE)
for (cc in cens_cols) all_losers_est[[cc]] <- NULL

# Apply new censoring
for (h in c(4, 8, 12, 26, 52)) {
  for (prefix in c("points_change_", "elo_change_")) {
    col <- paste0(prefix, h, "w")
    if (col %in% names(all_losers_est)) {
      all_losers_est[[col]] <- ifelse(
        !is.na(all_losers_est$weeks_to_next_same_type) &
          all_losers_est$weeks_to_next_same_type < h,
        NA_real_,
        all_losers_est[[col]]
      )
    }
  }
  for (prefix in c("n_main_draws_", "n_matches_250plus_")) {
    col <- paste0(prefix, h, "w")
    if (col %in% names(all_losers_est)) {
      all_losers_est[[col]] <- ifelse(
        !is.na(all_losers_est$weeks_to_next_same_type) &
          all_losers_est$weeks_to_next_same_type < h,
        NA_integer_,
        all_losers_est[[col]]
      )
    }
  }
}

# --- Re-split into GS and non-GS estimation samples --------------------------
gs_est  <- all_losers_est |> filter(tourney_level == "G", rank_among_losers <= 4)
nongs_est <- all_losers_est |> filter(tourney_level != "G")

gs_atp <- gs_est |> filter(tour == "ATP")
gs_wta <- gs_est |> filter(tour == "WTA")

# Carry over peer_component from the old non-GS dataset (computed in script 17)
# peer_component was added AFTER the split in the original pipeline, so all_losers_est
# does not contain it.
pc_lookup <- nongs_est_old |>
  select(tourney_id, player_id, peer_component) |>
  filter(!is.na(peer_component))
nongs_est <- nongs_est |>
  left_join(pc_lookup, by = c("tourney_id", "player_id"))
message("  peer_component merged: ", sum(!is.na(nongs_est$peer_component)), " / ", nrow(nongs_est))

message("  GS estimation: N = ", nrow(gs_est),
        " (ATP: ", nrow(gs_atp), ", WTA: ", nrow(gs_wta), ")")
message("  Non-GS estimation: N = ", nrow(nongs_est))

slog("## Updated estimation samples (after FIX 1 + FIX 2)")
slog("- GS ATP: N = ", nrow(gs_atp),
     " (LL: ", sum(gs_atp$got_ll), ", Control: ", sum(gs_atp$got_ll == 0), ")")
slog("- GS WTA: N = ", nrow(gs_wta),
     " (LL: ", sum(gs_wta$got_ll), ", Control: ", sum(gs_wta$got_ll == 0), ")")
slog("- Non-GS ATP: N = ", sum(nongs_est$tour == "ATP"),
     " (LL: ", sum(nongs_est$tour == "ATP" & nongs_est$got_ll == 1), ")")
slog("- Non-GS WTA: N = ", sum(nongs_est$tour == "WTA"),
     " (LL: ", sum(nongs_est$tour == "WTA" & nongs_est$got_ll == 1), ")")
slog("")

# Save updated datasets
saveRDS(gs_est, file.path(CLEANED_DIR, "skeleton_gs_est_v2.rds"))
saveRDS(nongs_est, file.path(CLEANED_DIR, "skeleton_nongs_est_v2.rds"))
saveRDS(all_losers_est, file.path(CLEANED_DIR, "skeleton_all_losers_est_v2.rds"))
message("  Saved v2 estimation datasets.")


# ==============================================================================
# FIX 3: CONTROL FUNCTION APPROACH FOR ALL NON-GS SPECIFICATIONS
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 3: CONTROL FUNCTION (CF) APPROACH FOR NON-GS")
message(strrep("=", 70))

# The generalized residual from a Bernoulli selection model:
#   v_i = D_i * phi(Phi^{-1}(P_i)) / P_i - (1-D_i) * phi(Phi^{-1}(P_i)) / (1-P_i)
# where P_i = peer_component (the LL selection probability from Bernoulli convolution)

compute_gen_residual <- function(D, P) {
  # Clamp P to avoid division by zero / Inf
  P <- pmax(pmin(P, 0.9999), 0.0001)
  probit_P <- qnorm(P)
  phi_val  <- dnorm(probit_P)
  v <- D * phi_val / P - (1 - D) * phi_val / (1 - P)
  v
}

# Compute v_hat for all non-GS observations
nongs_est <- nongs_est |>
  mutate(
    v_hat = compute_gen_residual(got_ll, peer_component)
  )

message("  v_hat computed for ", sum(!is.na(nongs_est$v_hat)), " non-GS observations")
message("  v_hat range: [", round(min(nongs_est$v_hat, na.rm = TRUE), 3),
        ", ", round(max(nongs_est$v_hat, na.rm = TRUE), 3), "]")

nongs_atp <- nongs_est |> filter(tour == "ATP", !is.na(peer_component))
nongs_wta <- nongs_est |> filter(tour == "WTA", !is.na(peer_component))

message("  Non-GS ATP with CF: N = ", nrow(nongs_atp))
message("  Non-GS WTA with CF: N = ", nrow(nongs_wta))

slog("## FIX 3: Control function approach")
slog("- v_hat = D * phi(Phi^{-1}(P))/P - (1-D) * phi(Phi^{-1}(P))/(1-P)")
slog("- Non-GS ATP with v_hat: N = ", nrow(nongs_atp))
slog("- Non-GS WTA with v_hat: N = ", nrow(nongs_wta))
slog("")


# ==============================================================================
# UPDATED stack_horizons FUNCTION (include new Z^pre + v_hat)
# ==============================================================================

stack_horizons_v2 <- function(data, outcomes_base, horizons = c(4, 8, 12, 26, 52)) {
  stacked <- list()
  for (h in horizons) {
    h_label <- paste0(h, "w")
    row_data <- data |>
      dplyr::transmute(
        player_id, tourney_id, tour, slam_year, got_ll,
        pre_rank_pts, pre_rank_pts_sq,
        pre_elo, pre_elo_sq,
        n_prior_gs_ll_won, n_prior_gs_ll_notwon,
        n_prior_nongs_ll_won, n_prior_nongs_ll_notwon,
        player_age,
        had_prior_ll = if ("had_prior_ll" %in% names(data)) had_prior_ll else NA_integer_,
        md_matches_won = if ("md_matches_won" %in% names(data)) md_matches_won else NA_integer_,
        peer_component = if ("peer_component" %in% names(data)) peer_component else NA_real_,
        v_hat = if ("v_hat" %in% names(data)) v_hat else NA_real_,
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


# ==============================================================================
# FIX 6a: RE-ESTIMATE GS STACKED DYNAMIC MODELS (expanded Z^pre)
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 6a: GS STACKED DYNAMICS (expanded Z^pre)")
message(strrep("=", 70))

stacked_atp <- stack_horizons_v2(gs_atp, outcomes_base)
stacked_wta <- stack_horizons_v2(gs_wta, outcomes_base)

message("  Stacked ATP GS: ", nrow(stacked_atp), " rows")
message("  Stacked WTA GS: ", nrow(stacked_wta), " rows")

run_stacked_gs <- function(stacked_data, tour_label) {
  results <- list()
  model_objects <- list()

  for (ob in outcomes_base) {
    if (!ob %in% names(stacked_data)) next

    sdata <- stacked_data |>
      filter(!is.na(.data[[ob]]),
             !is.na(pre_rank_pts), !is.na(player_age),
             !is.na(pre_elo))
    if (nrow(sdata) < 20) next

    fml <- as.formula(paste0(
      ob, " ~ got_ll:horizon + ", ZPRE_FULL,
      " | slam_year + horizon"
    ))

    fit <- tryCatch(
      feols(fml, data = sdata, vcov = ~player_id),
      error = function(e) {
        message("    Error for ", ob, " (", tour_label, "): ", e$message)
        NULL
      }
    )
    if (is.null(fit)) next

    model_objects[[ob]] <- fit
    cf <- coef(fit)
    se_vec <- sqrt(diag(vcov(fit)))

    for (h_lab in HORIZON_LABS) {
      coef_name <- paste0("got_ll:horizon", h_lab)
      if (!coef_name %in% names(cf)) next

      beta <- cf[coef_name]
      se_val <- se_vec[coef_name]
      pv <- 2 * pnorm(-abs(beta / se_val))
      n_h <- sum(!is.na(sdata[[ob]]) & sdata$horizon == h_lab)

      results[[paste0(ob, "_", h_lab)]] <- tibble(
        tour = tour_label, outcome = ob, horizon = h_lab,
        coef = beta, se = se_val, pvalue = pv,
        n_obs_total = nrow(sdata), n_obs_horizon = n_h,
        n_units = n_distinct(paste0(sdata$player_id, "_", sdata$tourney_id))
      )
    }
  }
  list(results = bind_rows(results), models = model_objects)
}

out_atp <- run_stacked_gs(stacked_atp, "ATP")
out_wta <- run_stacked_gs(stacked_wta, "WTA")

stacked_res_atp <- out_atp$results
stacked_res_wta <- out_wta$results

slog("## FIX 6a: GS stacked dynamics (expanded Z^pre)")
for (i in seq_len(nrow(stacked_res_atp))) {
  r <- stacked_res_atp[i, ]
  slog("- ATP GS ", r$outcome, " @ ", r$horizon,
       ": coef = ", fmt(r$coef), ", SE = ", fmt(r$se),
       ", p = ", fmt(r$pvalue, 3))
}
for (i in seq_len(nrow(stacked_res_wta))) {
  r <- stacked_res_wta[i, ]
  slog("- WTA GS ", r$outcome, " @ ", r$horizon,
       ": coef = ", fmt(r$coef), ", SE = ", fmt(r$se),
       ", p = ", fmt(r$pvalue, 3))
}
slog("")


# ==============================================================================
# FIX 6b: NON-GS STACKED DYNAMICS WITH CF (replaces IV)
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 6b: NON-GS STACKED DYNAMICS WITH CF")
message(strrep("=", 70))

stacked_nongs_atp <- stack_horizons_v2(nongs_atp, outcomes_base)
stacked_nongs_wta <- stack_horizons_v2(nongs_wta, outcomes_base)

message("  Stacked non-GS ATP: ", nrow(stacked_nongs_atp), " rows")
message("  Stacked non-GS WTA: ", nrow(stacked_nongs_wta), " rows")

# CF spec: include v_hat:horizon directly as regressors alongside got_ll:horizon
# No IV syntax needed. Cluster at player level.
run_stacked_nongs_cf <- function(stacked_data, tour_label) {
  results <- list()
  model_objects <- list()
  rho_results <- list()

  for (ob in outcomes_base) {
    if (!ob %in% names(stacked_data)) next

    sdata <- stacked_data |>
      filter(!is.na(.data[[ob]]),
             !is.na(pre_rank_pts), !is.na(player_age),
             !is.na(pre_elo), !is.na(v_hat))
    if (nrow(sdata) < 50) next

    # CF spec: got_ll:horizon + v_hat:horizon + Z^pre | slam_year + horizon
    fml <- as.formula(paste0(
      ob, " ~ got_ll:horizon + v_hat:horizon + ", ZPRE_FULL,
      " | slam_year + horizon"
    ))

    fit <- tryCatch(
      feols(fml, data = sdata, vcov = ~player_id),
      error = function(e) {
        message("    CF error for ", ob, " (", tour_label, "): ", e$message)
        NULL
      }
    )
    if (is.null(fit)) next

    model_objects[[ob]] <- fit
    cf <- coef(fit)
    se_vec <- sqrt(diag(vcov(fit)))

    for (h_lab in HORIZON_LABS) {
      # Treatment effect
      coef_name <- paste0("got_ll:horizon", h_lab)
      if (coef_name %in% names(cf)) {
        beta <- cf[coef_name]
        se_val <- se_vec[coef_name]
        pv <- 2 * pnorm(-abs(beta / se_val))
        n_h <- sum(!is.na(sdata[[ob]]) & sdata$horizon == h_lab)

        results[[paste0(ob, "_", h_lab)]] <- tibble(
          tour = tour_label, outcome = ob, horizon = h_lab,
          coef = beta, se = se_val, pvalue = pv,
          n_obs_total = nrow(sdata), n_obs_horizon = n_h,
          n_units = n_distinct(paste0(sdata$player_id, "_", sdata$tourney_id))
        )
      }

      # Endogeneity test: rho on v_hat
      # fixest may name it "horizonXw:v_hat" or "v_hat:horizonXw"
      rho_name <- paste0("v_hat:horizon", h_lab)
      rho_name_alt <- paste0("horizon", h_lab, ":v_hat")
      rho_nm <- if (rho_name %in% names(cf)) rho_name else if (rho_name_alt %in% names(cf)) rho_name_alt else NA_character_
      if (!is.na(rho_nm)) {
        rho <- cf[rho_nm]
        rho_se <- se_vec[rho_nm]
        rho_pv <- 2 * pnorm(-abs(rho / rho_se))
        rho_results[[paste0(ob, "_", h_lab)]] <- tibble(
          tour = tour_label, outcome = ob, horizon = h_lab,
          rho = rho, rho_se = rho_se, rho_pvalue = rho_pv
        )
      }
    }
  }
  list(results = bind_rows(results), models = model_objects, rho = bind_rows(rho_results))
}

out_nongs_atp <- run_stacked_nongs_cf(stacked_nongs_atp, "ATP non-GS")
out_nongs_wta <- run_stacked_nongs_cf(stacked_nongs_wta, "WTA non-GS")

stacked_nongs_res_atp <- out_nongs_atp$results
stacked_nongs_res_wta <- out_nongs_wta$results
rho_nongs_atp <- out_nongs_atp$rho
rho_nongs_wta <- out_nongs_wta$rho

slog("## FIX 6b: Non-GS stacked dynamics with CF")
for (i in seq_len(nrow(stacked_nongs_res_atp))) {
  r <- stacked_nongs_res_atp[i, ]
  slog("- ATP non-GS CF ", r$outcome, " @ ", r$horizon,
       ": coef = ", fmt(r$coef), ", SE = ", fmt(r$se),
       ", p = ", fmt(r$pvalue, 3))
}
for (i in seq_len(nrow(stacked_nongs_res_wta))) {
  r <- stacked_nongs_res_wta[i, ]
  slog("- WTA non-GS CF ", r$outcome, " @ ", r$horizon,
       ": coef = ", fmt(r$coef), ", SE = ", fmt(r$se),
       ", p = ", fmt(r$pvalue, 3))
}
slog("")

# Log endogeneity test results
slog("## Endogeneity test (rho on v_hat):")
for (i in seq_len(nrow(rho_nongs_atp))) {
  r <- rho_nongs_atp[i, ]
  sig <- if (r$rho_pvalue < 0.05) "SIGNIFICANT (endogeneity detected)" else "not significant"
  slog("- ATP non-GS rho ", r$outcome, " @ ", r$horizon,
       ": rho = ", fmt(r$rho, 3), ", p = ", fmt(r$rho_pvalue, 3), " -- ", sig)
}
for (i in seq_len(nrow(rho_nongs_wta))) {
  r <- rho_nongs_wta[i, ]
  sig <- if (r$rho_pvalue < 0.05) "SIGNIFICANT (endogeneity detected)" else "not significant"
  slog("- WTA non-GS rho ", r$outcome, " @ ", r$horizon,
       ": rho = ", fmt(r$rho, 3), ", p = ", fmt(r$rho_pvalue, 3), " -- ", sig)
}
slog("")


# ==============================================================================
# BUILD STACKED TABLES
# ==============================================================================
message("\n", strrep("=", 70))
message("BUILDING STACKED DYNAMIC TABLES")
message(strrep("=", 70))

build_stacked_tex <- function(res_df, tour_label, filename, is_cf = FALSE) {
  n_hor <- length(HORIZON_LABS)

  tex <- c(
    paste0("\\begin{tabular}{l", paste(rep(" c", n_hor), collapse = ""), "}"),
    "\\toprule",
    paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"),
    "\\midrule"
  )

  avail_outcomes <- intersect(unique(res_df$outcome), names(outcome_labels))
  for (ob in names(outcome_labels)) {
    if (!ob %in% avail_outcomes) next
    olab <- outcome_labels[ob]

    cells <- character()
    se_cells <- character()
    for (h in HORIZON_LABS) {
      r <- res_df |> filter(outcome == ob, horizon == h)
      if (nrow(r) == 0) {
        cells <- c(cells, "")
        se_cells <- c(se_cells, "")
      } else {
        cells <- c(cells, paste0(fmt(r$coef), add_stars(r$pvalue)))
        se_cells <- c(se_cells, paste0("(", fmt(r$se), ")"))
      }
    }
    tex <- c(tex,
      paste0(olab, " & ", paste(cells, collapse = " & "), " \\\\"),
      paste0(" & ", paste(se_cells, collapse = " & "), " \\\\"),
      "\\addlinespace"
    )
  }

  if (nrow(res_df) > 0) {
    n_units <- res_df$n_units[1]
    n_obs <- res_df$n_obs_total[1]

    tex <- c(tex, "\\midrule")

    h_n_cells <- character()
    for (h in HORIZON_LABS) {
      r <- res_df |> filter(horizon == h)
      if (nrow(r) > 0) {
        h_n_cells <- c(h_n_cells, as.character(r$n_obs_horizon[1]))
      } else {
        h_n_cells <- c(h_n_cells, "--")
      }
    }
    tex <- c(tex,
      paste0("$N$ (per horizon) & ", paste(h_n_cells, collapse = " & "), " \\\\"),
      paste0("$N$ (stacked total) & \\multicolumn{", n_hor, "}{c}{", n_obs, "} \\\\"),
      paste0("Player-events & \\multicolumn{", n_hor, "}{c}{", n_units, "} \\\\"),
      paste0("Event FE & \\multicolumn{", n_hor, "}{c}{Yes} \\\\"),
      paste0("Horizon FE & \\multicolumn{", n_hor, "}{c}{Yes} \\\\")
    )
    if (is_cf) {
      tex <- c(tex,
        paste0("Control function & \\multicolumn{", n_hor, "}{c}{Yes} \\\\")
      )
    }
  }

  tex <- c(tex, "\\bottomrule", "\\end{tabular}")
  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

build_stacked_tex(stacked_res_atp, "ATP", "table_dynamic_stacked_atp.tex")
build_stacked_tex(stacked_res_wta, "WTA", "table_dynamic_stacked_wta.tex")
build_stacked_tex(stacked_nongs_res_atp, "ATP non-GS", "table_dynamic_stacked_nongs_atp.tex",
                  is_cf = TRUE)
build_stacked_tex(stacked_nongs_res_wta, "WTA non-GS", "table_dynamic_stacked_nongs_wta.tex",
                  is_cf = TRUE)


# ==============================================================================
# FIX 6c: HETEROGENEITY (GS + NON-GS CF)
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 6c: HETEROGENEITY TABLES")
message(strrep("=", 70))

prepare_hetero_data <- function(data) {
  qts <- quantile(data$pre_rank_pts, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  med_age <- median(data$player_age, na.rm = TRUE)
  data |> mutate(
    rank_above_med = as.integer(pre_rank_pts >= median(pre_rank_pts, na.rm = TRUE)),
    age_above_med  = as.integer(player_age >= med_age),
    prior_ll_dum   = as.integer(had_prior_ll == 1)
  )
}

# --- GS heterogeneity --------------------------------------------------------
gs_atp_h <- prepare_hetero_data(gs_atp)
gs_wta_h <- prepare_hetero_data(gs_wta)
stacked_atp_h <- stack_horizons_v2(gs_atp_h, outcomes_base)
stacked_wta_h <- stack_horizons_v2(gs_wta_h, outcomes_base)

# Merge category variables
merge_cats <- function(stacked, orig) {
  cats <- orig |> select(player_id, tourney_id, rank_above_med, age_above_med, prior_ll_dum)
  stacked |> left_join(cats, by = c("player_id", "tourney_id"))
}
stacked_atp_h <- merge_cats(stacked_atp_h, gs_atp_h)
stacked_wta_h <- merge_cats(stacked_wta_h, gs_wta_h)

run_hetero_horizon <- function(sdata, tour_label) {
  results <- list()

  for (ob in outcomes_base) {
    if (!ob %in% names(sdata)) next
    d <- sdata |> filter(!is.na(.data[[ob]]),
                         !is.na(pre_rank_pts), !is.na(player_age), !is.na(pre_elo))
    if (nrow(d) < 30) next

    for (dim_info in list(
      list(var = "rank_above_med", dim = "Ranking",
           base_label = "beta_h (low rank base)", diff_label = "delta_h (high rank)"),
      list(var = "age_above_med", dim = "Age",
           base_label = "beta_h (young base)", diff_label = "delta_h (older)"),
      list(var = "prior_ll_dum", dim = "Prior LL",
           base_label = "beta_h (no prior base)", diff_label = "delta_h (had prior LL)")
    )) {
      fml <- as.formula(paste0(
        ob, " ~ got_ll:horizon + got_ll:horizon:", dim_info$var,
        " + ", ZPRE_FULL, " | slam_year + horizon"
      ))
      fit <- tryCatch(feols(fml, data = d, vcov = ~player_id), error = function(e) NULL)
      if (is.null(fit)) next

      cf <- coef(fit); se <- sqrt(diag(vcov(fit)))
      for (h_lab in HORIZON_LABS) {
        base_nm <- paste0("got_ll:horizon", h_lab)
        diff_nm <- paste0("got_ll:horizon", h_lab, ":", dim_info$var)
        if (base_nm %in% names(cf)) {
          pv <- 2 * pnorm(-abs(cf[base_nm] / se[base_nm]))
          results[[paste0(dim_info$dim, "_base_", ob, "_", h_lab)]] <- tibble(
            tour = tour_label, outcome = ob, horizon = h_lab,
            dimension = dim_info$dim, term = dim_info$base_label,
            coef = cf[base_nm], se = se[base_nm], pvalue = pv
          )
        }
        if (diff_nm %in% names(cf)) {
          pv <- 2 * pnorm(-abs(cf[diff_nm] / se[diff_nm]))
          results[[paste0(dim_info$dim, "_diff_", ob, "_", h_lab)]] <- tibble(
            tour = tour_label, outcome = ob, horizon = h_lab,
            dimension = dim_info$dim, term = dim_info$diff_label,
            coef = cf[diff_nm], se = se[diff_nm], pvalue = pv
          )
        }
      }
    }
  }
  bind_rows(results)
}

hetero_res_atp <- run_hetero_horizon(stacked_atp_h, "ATP")
hetero_res_wta <- run_hetero_horizon(stacked_wta_h, "WTA")

# --- Non-GS heterogeneity with CF -------------------------------------------
nongs_atp_h <- prepare_hetero_data(nongs_atp)
nongs_wta_h <- prepare_hetero_data(nongs_wta)
stacked_nongs_atp_h <- stack_horizons_v2(nongs_atp_h, outcomes_base)
stacked_nongs_wta_h <- stack_horizons_v2(nongs_wta_h, outcomes_base)
stacked_nongs_atp_h <- merge_cats(stacked_nongs_atp_h, nongs_atp_h)
stacked_nongs_wta_h <- merge_cats(stacked_nongs_wta_h, nongs_wta_h)

run_hetero_nongs_cf <- function(sdata, tour_label) {
  results <- list()

  for (ob in outcomes_base) {
    if (!ob %in% names(sdata)) next
    d <- sdata |> filter(!is.na(.data[[ob]]),
                         !is.na(pre_rank_pts), !is.na(player_age),
                         !is.na(pre_elo), !is.na(v_hat))
    if (nrow(d) < 50) next

    for (dim_info in list(
      list(var = "rank_above_med", dim = "Ranking",
           base_label = "beta_h (low rank base)", diff_label = "delta_h (high rank)"),
      list(var = "age_above_med", dim = "Age",
           base_label = "beta_h (young base)", diff_label = "delta_h (older)"),
      list(var = "prior_ll_dum", dim = "Prior LL",
           base_label = "beta_h (no prior base)", diff_label = "delta_h (had prior LL)")
    )) {
      # CF hetero: got_ll:horizon + got_ll:horizon:cat + v_hat:horizon + v_hat:horizon:cat + Z^pre
      fml <- as.formula(paste0(
        ob, " ~ got_ll:horizon + got_ll:horizon:", dim_info$var,
        " + v_hat:horizon + v_hat:horizon:", dim_info$var,
        " + ", ZPRE_FULL, " | slam_year + horizon"
      ))
      fit <- tryCatch(feols(fml, data = d, vcov = ~player_id), error = function(e) NULL)
      if (is.null(fit)) next

      cf_coefs <- coef(fit); se <- sqrt(diag(vcov(fit)))
      for (h_lab in HORIZON_LABS) {
        base_nm <- paste0("got_ll:horizon", h_lab)
        diff_nm <- paste0("got_ll:horizon", h_lab, ":", dim_info$var)
        if (base_nm %in% names(cf_coefs)) {
          pv <- 2 * pnorm(-abs(cf_coefs[base_nm] / se[base_nm]))
          results[[paste0(dim_info$dim, "_base_", ob, "_", h_lab)]] <- tibble(
            tour = tour_label, outcome = ob, horizon = h_lab,
            dimension = dim_info$dim, term = dim_info$base_label,
            coef = cf_coefs[base_nm], se = se[base_nm], pvalue = pv
          )
        }
        if (diff_nm %in% names(cf_coefs)) {
          pv <- 2 * pnorm(-abs(cf_coefs[diff_nm] / se[diff_nm]))
          results[[paste0(dim_info$dim, "_diff_", ob, "_", h_lab)]] <- tibble(
            tour = tour_label, outcome = ob, horizon = h_lab,
            dimension = dim_info$dim, term = dim_info$diff_label,
            coef = cf_coefs[diff_nm], se = se[diff_nm], pvalue = pv
          )
        }
      }
    }
  }
  bind_rows(results)
}

hetero_nongs_atp <- run_hetero_nongs_cf(stacked_nongs_atp_h, "ATP non-GS")
hetero_nongs_wta <- run_hetero_nongs_cf(stacked_nongs_wta_h, "WTA non-GS")

# --- Build hetero tables ------------------------------------------------------
build_hetero_horizon_tex <- function(res_df, tour_label, filename, is_cf = FALSE) {
  n_hor <- length(HORIZON_LABS)
  dims <- c("Ranking", "Age", "Prior LL")
  primary_ob <- "points_change"
  primary_data <- res_df |> filter(outcome == primary_ob)
  if (nrow(primary_data) == 0) return(invisible(NULL))

  tex <- c(
    "\\begin{tabular}{l c c c c}",
    "\\toprule",
    paste0("Horizon & $\\hat{\\beta}_h$ (base) & SE",
           " & $\\hat{\\delta}_h$ (differential) & SE \\\\"),
    "\\midrule"
  )

  for (dm in dims) {
    dim_data <- primary_data |> filter(dimension == dm)
    if (nrow(dim_data) == 0) next
    diff_terms <- dim_data |> filter(grepl("delta", term)) |> pull(term) |> unique()
    diff_label <- if (length(diff_terms) > 0) diff_terms[1] else ""

    tex <- c(tex, paste0("\\multicolumn{5}{l}{\\textit{",
                         dm, " heterogeneity",
                         ifelse(diff_label != "", paste0(" (", gsub("delta_h ", "", diff_label), ")"), ""),
                         "}} \\\\"))

    for (h_lab in HORIZON_LABS) {
      base_row <- dim_data |> filter(horizon == h_lab, grepl("beta", term))
      diff_row <- dim_data |> filter(horizon == h_lab, grepl("delta", term))

      base_cell <- if (nrow(base_row) > 0) paste0(fmt(base_row$coef), add_stars(base_row$pvalue)) else ""
      base_se <- if (nrow(base_row) > 0) paste0("(", fmt(base_row$se), ")") else ""
      diff_cell <- if (nrow(diff_row) > 0) paste0(fmt(diff_row$coef), add_stars(diff_row$pvalue)) else ""
      diff_se <- if (nrow(diff_row) > 0) paste0("(", fmt(diff_row$se), ")") else ""

      tex <- c(tex, paste0(h_lab, " & ", base_cell, " & ", base_se,
                           " & ", diff_cell, " & ", diff_se, " \\\\"))
    }
    tex <- c(tex, "\\addlinespace")
  }

  # All outcomes at 26w for ranking
  tex <- c(tex, "\\midrule",
    "\\multicolumn{5}{l}{\\textit{All outcomes at 26w (ranking heterogeneity)}} \\\\")
  for (ob in names(outcome_labels)) {
    ob_data <- res_df |> filter(outcome == ob, dimension == "Ranking", horizon == "26w")
    base_r <- ob_data |> filter(grepl("beta", term))
    diff_r <- ob_data |> filter(grepl("delta", term))
    base_cell <- if (nrow(base_r) > 0) paste0(fmt(base_r$coef), add_stars(base_r$pvalue)) else ""
    base_se   <- if (nrow(base_r) > 0) paste0("(", fmt(base_r$se), ")") else ""
    diff_cell <- if (nrow(diff_r) > 0) paste0(fmt(diff_r$coef), add_stars(diff_r$pvalue)) else ""
    diff_se   <- if (nrow(diff_r) > 0) paste0("(", fmt(diff_r$se), ")") else ""
    tex <- c(tex, paste0(outcome_labels[ob], " & ", base_cell, " & ", base_se,
                         " & ", diff_cell, " & ", diff_se, " \\\\"))
  }

  method_label <- if (is_cf) "Control function = Yes" else "Event FE = Yes; Horizon FE = Yes"
  tex <- c(tex, "\\midrule",
    paste0("\\multicolumn{5}{l}{", method_label, "} \\\\"),
    "\\bottomrule", "\\end{tabular}")

  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

if (nrow(hetero_res_atp) > 0)
  build_hetero_horizon_tex(hetero_res_atp, "ATP", "table_hetero_stacked_atp.tex")
if (nrow(hetero_res_wta) > 0)
  build_hetero_horizon_tex(hetero_res_wta, "WTA", "table_hetero_stacked_wta.tex")
if (nrow(hetero_nongs_atp) > 0)
  build_hetero_horizon_tex(hetero_nongs_atp, "ATP non-GS", "table_hetero_stacked_nongs_atp.tex",
                           is_cf = TRUE)
if (nrow(hetero_nongs_wta) > 0)
  build_hetero_horizon_tex(hetero_nongs_wta, "WTA non-GS", "table_hetero_stacked_nongs_wta.tex",
                           is_cf = TRUE)


# ==============================================================================
# FIX 6d: DOSE TABLES (GS + NON-GS CF)
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 6d: DOSE TABLES")
message(strrep("=", 70))

# --- GS dose ------------------------------------------------------------------
prepare_dose_data <- function(data) {
  data |> mutate(
    dose_1win  = as.integer(!is.na(md_matches_won) & md_matches_won == 1),
    dose_2plus = as.integer(!is.na(md_matches_won) & md_matches_won >= 2)
  )
}

gs_atp_d <- prepare_dose_data(gs_atp)
gs_wta_d <- prepare_dose_data(gs_wta)
stacked_atp_d <- stack_horizons_v2(gs_atp_d, outcomes_base)
stacked_wta_d <- stack_horizons_v2(gs_wta_d, outcomes_base)

merge_dose <- function(stacked, orig) {
  cats <- orig |> select(player_id, tourney_id, dose_1win, dose_2plus)
  stacked |> left_join(cats, by = c("player_id", "tourney_id"))
}
stacked_atp_d <- merge_dose(stacked_atp_d, gs_atp_d)
stacked_wta_d <- merge_dose(stacked_wta_d, gs_wta_d)

run_dose_horizon <- function(sdata, tour_label) {
  results <- list()
  for (ob in outcomes_base) {
    if (!ob %in% names(sdata)) next
    d <- sdata |> filter(!is.na(.data[[ob]]),
                         !is.na(pre_rank_pts), !is.na(player_age), !is.na(pre_elo))
    if (nrow(d) < 30) next

    fml <- as.formula(paste0(
      ob, " ~ got_ll:horizon + got_ll:horizon:dose_1win + got_ll:horizon:dose_2plus",
      " + ", ZPRE_FULL, " | slam_year + horizon"
    ))
    fit <- tryCatch(feols(fml, data = d, vcov = ~player_id), error = function(e) NULL)
    if (is.null(fit)) next

    cf <- coef(fit); se <- sqrt(diag(vcov(fit)))
    for (h_lab in HORIZON_LABS) {
      for (nm_info in list(
        list(nm = paste0("got_ll:horizon", h_lab), term = "beta_h (0-win base)"),
        list(nm = paste0("got_ll:horizon", h_lab, ":dose_1win"), term = "delta_h (1 win)"),
        list(nm = paste0("got_ll:horizon", h_lab, ":dose_2plus"), term = "delta_h (2+ wins)")
      )) {
        if (nm_info$nm %in% names(cf)) {
          pv <- 2 * pnorm(-abs(cf[nm_info$nm] / se[nm_info$nm]))
          results[[paste0(nm_info$term, "_", ob, "_", h_lab)]] <- tibble(
            tour = tour_label, outcome = ob, horizon = h_lab,
            term = nm_info$term,
            coef = cf[nm_info$nm], se = se[nm_info$nm], pvalue = pv
          )
        }
      }
    }
  }
  bind_rows(results)
}

dose_res_atp <- run_dose_horizon(stacked_atp_d, "ATP")
dose_res_wta <- run_dose_horizon(stacked_wta_d, "WTA")

# --- Non-GS dose with CF -----------------------------------------------------
# For dose with CF, we use centered matches won as continuous dose
nongs_atp_d <- nongs_atp |>
  mutate(
    matches_won_c    = md_matches_won - mean(md_matches_won[got_ll == 1], na.rm = TRUE),
    matches_won_c_sq = matches_won_c^2
  )
nongs_wta_d <- nongs_wta |>
  mutate(
    matches_won_c    = md_matches_won - mean(md_matches_won[got_ll == 1], na.rm = TRUE),
    matches_won_c_sq = matches_won_c^2
  )

stacked_nongs_atp_d <- stack_horizons_v2(nongs_atp_d, outcomes_base)
stacked_nongs_wta_d <- stack_horizons_v2(nongs_wta_d, outcomes_base)

# Merge dose variables
merge_dose_nongs <- function(stacked, orig) {
  cats <- orig |> select(player_id, tourney_id, matches_won_c, matches_won_c_sq,
                          md_matches_won)
  stacked |> left_join(cats, by = c("player_id", "tourney_id"))
}
stacked_nongs_atp_d <- merge_dose_nongs(stacked_nongs_atp_d, nongs_atp_d)
stacked_nongs_wta_d <- merge_dose_nongs(stacked_nongs_wta_d, nongs_wta_d)

# Also prepare discrete dose for non-GS (like GS) for table comparability
merge_discrete_dose <- function(stacked, orig) {
  cats <- orig |> mutate(
    dose_1win  = as.integer(!is.na(md_matches_won) & md_matches_won == 1),
    dose_2plus = as.integer(!is.na(md_matches_won) & md_matches_won >= 2)
  ) |> select(player_id, tourney_id, dose_1win, dose_2plus)
  stacked |> left_join(cats, by = c("player_id", "tourney_id"))
}
stacked_nongs_atp_d <- merge_discrete_dose(stacked_nongs_atp_d, nongs_atp_d)
stacked_nongs_wta_d <- merge_discrete_dose(stacked_nongs_wta_d, nongs_wta_d)

run_dose_nongs_cf <- function(sdata, tour_label) {
  results <- list()
  for (ob in outcomes_base) {
    if (!ob %in% names(sdata)) next
    d <- sdata |> filter(!is.na(.data[[ob]]),
                         !is.na(pre_rank_pts), !is.na(player_age),
                         !is.na(pre_elo), !is.na(v_hat))
    if (nrow(d) < 50) next

    # CF dose: got_ll:horizon + got_ll:horizon:dose + v_hat:horizon + v_hat:horizon:dose + Z^pre
    fml <- as.formula(paste0(
      ob, " ~ got_ll:horizon + got_ll:horizon:dose_1win + got_ll:horizon:dose_2plus",
      " + v_hat:horizon + v_hat:horizon:dose_1win + v_hat:horizon:dose_2plus",
      " + ", ZPRE_FULL, " | slam_year + horizon"
    ))
    fit <- tryCatch(feols(fml, data = d, vcov = ~player_id), error = function(e) NULL)
    if (is.null(fit)) next

    cf <- coef(fit); se <- sqrt(diag(vcov(fit)))
    for (h_lab in HORIZON_LABS) {
      for (nm_info in list(
        list(nm = paste0("got_ll:horizon", h_lab), term = "beta_h (0-win base)"),
        list(nm = paste0("got_ll:horizon", h_lab, ":dose_1win"), term = "delta_h (1 win)"),
        list(nm = paste0("got_ll:horizon", h_lab, ":dose_2plus"), term = "delta_h (2+ wins)")
      )) {
        if (nm_info$nm %in% names(cf)) {
          pv <- 2 * pnorm(-abs(cf[nm_info$nm] / se[nm_info$nm]))
          results[[paste0(nm_info$term, "_", ob, "_", h_lab)]] <- tibble(
            tour = tour_label, outcome = ob, horizon = h_lab,
            term = nm_info$term,
            coef = cf[nm_info$nm], se = se[nm_info$nm], pvalue = pv
          )
        }
      }
    }
  }
  bind_rows(results)
}

dose_nongs_atp <- run_dose_nongs_cf(stacked_nongs_atp_d, "ATP non-GS")
dose_nongs_wta <- run_dose_nongs_cf(stacked_nongs_wta_d, "WTA non-GS")

# --- Build GS dose table -----------------------------------------------------
build_dose_horizon_tex <- function(atp_res, wta_res, filename) {
  primary_ob <- "points_change"
  tex <- c(
    "\\begin{tabular}{l c c c c c c}",
    "\\toprule",
    paste0("Horizon & $\\hat{\\beta}_h$ & SE",
           " & $\\hat{\\delta}_{1\\text{win},h}$ & SE",
           " & $\\hat{\\delta}_{2+,h}$ & SE \\\\"),
    "\\midrule"
  )

  for (panel in list(list(df = atp_res, label = "Panel A: ATP GS"),
                     list(df = wta_res, label = "Panel B: WTA GS"))) {
    pd <- panel$df |> filter(outcome == primary_ob)
    if (nrow(pd) == 0) next
    tex <- c(tex, paste0("\\multicolumn{7}{l}{\\textit{", panel$label, "}} \\\\"))
    for (h_lab in HORIZON_LABS) {
      base_r   <- pd |> filter(horizon == h_lab, grepl("0-win", term))
      d1win_r  <- pd |> filter(horizon == h_lab, grepl("1 win", term))
      d2plus_r <- pd |> filter(horizon == h_lab, grepl("2\\+", term))
      mc <- function(r) if (nrow(r) > 0) paste0(fmt(r$coef), add_stars(r$pvalue)) else ""
      ms <- function(r) if (nrow(r) > 0) paste0("(", fmt(r$se), ")") else ""
      tex <- c(tex, paste0(h_lab, " & ", mc(base_r), " & ", ms(base_r), " & ",
                           mc(d1win_r), " & ", ms(d1win_r), " & ",
                           mc(d2plus_r), " & ", ms(d2plus_r), " \\\\"))
    }
    tex <- c(tex, "\\addlinespace")
  }
  tex <- c(tex, "\\midrule",
    "\\multicolumn{7}{l}{Event FE = Yes; Horizon FE = Yes} \\\\",
    "\\bottomrule", "\\end{tabular}")
  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

build_dose_horizon_tex(dose_res_atp, dose_res_wta, "table_dose_stacked.tex")

# --- Build non-GS dose table -------------------------------------------------
build_dose_nongs_tex <- function(atp_res, wta_res, filename) {
  n_hor <- length(HORIZON_LABS)
  tex <- c(
    paste0("\\begin{tabular}{l", paste(rep(" c", n_hor), collapse = ""), "}"),
    "\\toprule",
    paste0("Dose subgroup & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"),
    "\\midrule"
  )
  for (panel in list(list(df = atp_res, label = "Panel A: ATP non-GS CF"),
                     list(df = wta_res, label = "Panel B: WTA non-GS CF"))) {
    pd <- panel$df |> filter(outcome == "points_change")
    if (nrow(pd) == 0) next
    tex <- c(tex, paste0("\\multicolumn{", n_hor + 1, "}{l}{\\textit{", panel$label, "}} \\\\"))
    for (tm in unique(pd$term)) {
      cells <- character(); se_cells <- character()
      for (h in HORIZON_LABS) {
        r <- pd |> filter(term == tm, horizon == h)
        if (nrow(r) == 0) { cells <- c(cells, ""); se_cells <- c(se_cells, "") }
        else {
          cells <- c(cells, paste0(fmt(r$coef), add_stars(r$pvalue)))
          se_cells <- c(se_cells, paste0("(", fmt(r$se), ")"))
        }
      }
      tex <- c(tex,
        paste0(tm, " & ", paste(cells, collapse = " & "), " \\\\"),
        paste0(" & ", paste(se_cells, collapse = " & "), " \\\\"))
    }
    tex <- c(tex, "\\addlinespace")
  }
  tex <- c(tex, "\\midrule",
    paste0("Control function & \\multicolumn{", n_hor, "}{c}{Yes} \\\\"),
    paste0("Event FE & \\multicolumn{", n_hor, "}{c}{Yes} \\\\"),
    paste0("Horizon FE & \\multicolumn{", n_hor, "}{c}{Yes} \\\\"),
    "\\bottomrule", "\\end{tabular}")
  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

if (nrow(dose_nongs_atp) > 0 || nrow(dose_nongs_wta) > 0) {
  build_dose_nongs_tex(dose_nongs_atp, dose_nongs_wta, "table_dose_stacked_nongs.tex")
}


# ==============================================================================
# FIX 6e: ROBUSTNESS (VERIFIED + FIRST-LL) with expanded Z^pre
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 6e: ROBUSTNESS TABLES")
message(strrep("=", 70))

verified_events <- gs_est |>
  filter(got_ll == 1) |>
  group_by(tourney_id) |>
  summarise(min_ll_rank = min(rank_among_losers, na.rm = TRUE), .groups = "drop") |>
  filter(min_ll_rank > 1) |>
  pull(tourney_id)

message("  Verified lottery events: ", length(verified_events))

run_robustness_stacked <- function(data_subset, tour_label) {
  stacked_sub <- stack_horizons_v2(data_subset, outcomes_base)
  results <- list()
  for (ob in outcomes_base) {
    if (!ob %in% names(stacked_sub)) next
    d <- stacked_sub |> filter(!is.na(.data[[ob]]),
                                !is.na(pre_rank_pts), !is.na(player_age), !is.na(pre_elo))
    sy_counts <- d |> count(slam_year) |> filter(n >= 2)
    d <- d |> filter(slam_year %in% sy_counts$slam_year)
    if (nrow(d) < 20 || sum(d$got_ll == 1) < 3) next

    fml <- as.formula(paste0(ob, " ~ got_ll:horizon + ", ZPRE_FULL, " | slam_year + horizon"))
    fit <- tryCatch(feols(fml, data = d, vcov = ~player_id), error = function(e) NULL)
    if (is.null(fit)) next

    cf <- coef(fit); se_v <- sqrt(diag(vcov(fit)))
    for (h_lab in HORIZON_LABS) {
      cn <- paste0("got_ll:horizon", h_lab)
      if (cn %in% names(cf)) {
        pv <- 2 * pnorm(-abs(cf[cn] / se_v[cn]))
        results[[paste0(ob, "_", h_lab)]] <- tibble(
          tour = tour_label, outcome = ob, horizon = h_lab,
          coef = cf[cn], se = se_v[cn], pvalue = pv,
          n_obs = nrow(d),
          n_units = n_distinct(paste0(d$player_id, "_", d$tourney_id))
        )
      }
    }
  }
  bind_rows(results)
}

ver_atp_data <- gs_atp |> filter(tourney_id %in% verified_events)
ver_wta_data <- gs_wta |> filter(tourney_id %in% verified_events)
ver_res <- bind_rows(
  run_robustness_stacked(ver_atp_data, "ATP"),
  run_robustness_stacked(ver_wta_data, "WTA")
)

slog("## FIX 6e: Robustness (verified lottery)")
slog("- Verified ATP: N = ", nrow(ver_atp_data), " (LL: ", sum(ver_atp_data$got_ll), ")")
slog("- Verified WTA: N = ", nrow(ver_wta_data), " (LL: ", sum(ver_wta_data$got_ll), ")")
for (i in seq_len(nrow(ver_res))) {
  r <- ver_res[i, ]
  if (r$outcome == "points_change") {
    slog("- Verified ", r$tour, " pts @ ", r$horizon,
         ": coef = ", fmt(r$coef), " (", fmt(r$se), "), p = ", fmt(r$pvalue, 3))
  }
}
slog("")

# Build verified table
build_robustness_tex <- function(res_df, filename) {
  n_hor <- length(HORIZON_LABS)
  panels <- split(res_df, res_df$tour)
  tex <- c(
    paste0("\\begin{tabular}{l", paste(rep(" c", n_hor), collapse = ""), "}"),
    "\\toprule",
    paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"),
    "\\midrule"
  )
  panel_idx <- 0
  for (pname in names(panels)) {
    panel_idx <- panel_idx + 1
    pd <- panels[[pname]]
    tex <- c(tex, paste0("\\multicolumn{", n_hor + 1, "}{l}{\\textit{Panel ",
                         LETTERS[panel_idx], ": ", pname, " GS}} \\\\"))
    for (ob in names(outcome_labels)) {
      ob_data <- pd |> filter(outcome == ob)
      if (nrow(ob_data) == 0) next
      cells <- character(); se_cells <- character()
      for (h in HORIZON_LABS) {
        r <- ob_data |> filter(horizon == h)
        if (nrow(r) == 0) { cells <- c(cells, ""); se_cells <- c(se_cells, "") }
        else {
          cells <- c(cells, paste0(fmt(r$coef), add_stars(r$pvalue)))
          se_cells <- c(se_cells, paste0("(", fmt(r$se), ")"))
        }
      }
      tex <- c(tex,
        paste0(outcome_labels[ob], " & ", paste(cells, collapse = " & "), " \\\\"),
        paste0(" & ", paste(se_cells, collapse = " & "), " \\\\"),
        "\\addlinespace")
    }
    if (nrow(pd) > 0) {
      tex <- c(tex,
        paste0("$N$ & \\multicolumn{", n_hor, "}{c}{", pd$n_obs[1], "} \\\\"),
        paste0("Player-events & \\multicolumn{", n_hor, "}{c}{", pd$n_units[1], "} \\\\"),
        "\\addlinespace")
    }
  }
  tex <- c(tex, "\\midrule",
    paste0("Event FE & \\multicolumn{", n_hor, "}{c}{Yes} \\\\"),
    paste0("Horizon FE & \\multicolumn{", n_hor, "}{c}{Yes} \\\\"),
    "\\bottomrule", "\\end{tabular}")
  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

build_robustness_tex(ver_res, "table_verified_stacked.tex")

# First-LL restriction
gs_atp_first <- gs_atp |> filter(n_prior_gs_ll_won == 0)
gs_wta_first <- gs_wta |> filter(n_prior_gs_ll_won == 0)

first_res_atp <- run_robustness_stacked(gs_atp_first, "ATP")
first_res_wta <- run_robustness_stacked(gs_wta_first, "WTA")

slog("## FIX 6e: Robustness (first-LL)")
slog("- First-LL ATP: N = ", nrow(gs_atp_first), " (LL: ", sum(gs_atp_first$got_ll), ")")
slog("- First-LL WTA: N = ", nrow(gs_wta_first), " (LL: ", sum(gs_wta_first$got_ll), ")")
slog("")

build_single_panel_tex <- function(res_df, filename) {
  n_hor <- length(HORIZON_LABS)
  tex <- c(
    paste0("\\begin{tabular}{l", paste(rep(" c", n_hor), collapse = ""), "}"),
    "\\toprule",
    paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"),
    "\\midrule"
  )
  for (ob in names(outcome_labels)) {
    ob_data <- res_df |> filter(outcome == ob)
    if (nrow(ob_data) == 0) next
    cells <- character(); se_cells <- character()
    for (h in HORIZON_LABS) {
      r <- ob_data |> filter(horizon == h)
      if (nrow(r) == 0) { cells <- c(cells, ""); se_cells <- c(se_cells, "") }
      else {
        cells <- c(cells, paste0(fmt(r$coef), add_stars(r$pvalue)))
        se_cells <- c(se_cells, paste0("(", fmt(r$se), ")"))
      }
    }
    tex <- c(tex,
      paste0(outcome_labels[ob], " & ", paste(cells, collapse = " & "), " \\\\"),
      paste0(" & ", paste(se_cells, collapse = " & "), " \\\\"),
      "\\addlinespace")
  }
  if (nrow(res_df) > 0) {
    tex <- c(tex, "\\midrule",
      paste0("$N$ (stacked) & \\multicolumn{", n_hor, "}{c}{", res_df$n_obs[1], "} \\\\"),
      paste0("Player-events & \\multicolumn{", n_hor, "}{c}{", res_df$n_units[1], "} \\\\"),
      paste0("Event FE & \\multicolumn{", n_hor, "}{c}{Yes} \\\\"),
      paste0("Horizon FE & \\multicolumn{", n_hor, "}{c}{Yes} \\\\"))
  }
  tex <- c(tex, "\\bottomrule", "\\end{tabular}")
  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

if (nrow(first_res_atp) > 0) build_single_panel_tex(first_res_atp, "table_firstll_stacked_atp.tex")
if (nrow(first_res_wta) > 0) build_single_panel_tex(first_res_wta, "table_firstll_stacked_wta.tex")


# ==============================================================================
# FIX 6f: EVENT STUDY FIGURES
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 6f: EVENT STUDY FIGURES")
message(strrep("=", 70))

build_es_from_coefs <- function(res_df, tour_label) {
  pts_df <- res_df |> filter(outcome == "points_change")
  if (nrow(pts_df) == 0) return(NULL)
  baseline <- tibble(
    tour = tour_label, outcome = "points_change", horizon = "0w",
    coef = 0, se = 0, pvalue = NA_real_,
    n_obs_total = pts_df$n_obs_total[1],
    n_obs_horizon = pts_df$n_obs_horizon[1],
    n_units = pts_df$n_units[1]
  )
  bind_rows(baseline, pts_df) |>
    mutate(weeks = as.numeric(gsub("w", "", horizon)),
           ci_lo = coef - 1.96 * se, ci_hi = coef + 1.96 * se)
}

es_atp <- build_es_from_coefs(stacked_res_atp, "ATP")
es_wta <- build_es_from_coefs(stacked_res_wta, "WTA")

make_es_figure <- function(es_data, tour_label) {
  if (is.null(es_data) || nrow(es_data) == 0) return(NULL)
  ggplot(es_data, aes(x = weeks, y = coef)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.2, fill = col_treat) +
    geom_line(color = col_treat, linewidth = 0.9) +
    geom_point(color = col_treat, size = 3) +
    scale_x_continuous(breaks = c(0, 4, 8, 12, 26, 52),
                       labels = c("0", "4", "8", "12", "26", "52")) +
    labs(x = "Weeks after qualifying loss",
         y = "Effect on ranking points (stacked estimate)") +
    theme_paper()
}

p_es_atp <- make_es_figure(es_atp, "ATP")
p_es_wta <- make_es_figure(es_wta, "WTA")

if (!is.null(p_es_atp)) {
  ggsave(file.path(FIGURES_DIR, "fig_event_study_atp.pdf"), p_es_atp,
         width = 7, height = 5, device = cairo_pdf)
  message("  Saved: fig_event_study_atp.pdf")
}
if (!is.null(p_es_wta)) {
  ggsave(file.path(FIGURES_DIR, "fig_event_study_wta.pdf"), p_es_wta,
         width = 7, height = 5, device = cairo_pdf)
  message("  Saved: fig_event_study_wta.pdf")
}


# ==============================================================================
# FIX 4: SPLIT SUMMARY STATS INTO 4 TABLES
# ==============================================================================
message("\n", strrep("=", 70))
message("FIX 4: SUMMARY STATS TABLES (4 separate)")
message(strrep("=", 70))

# FIX 5: Corrected variable labels
balance_vars <- c(
  "player_age"              = "Age",
  "pre_rank_pts"            = "Ranking points",
  "pre_elo"                 = "Elo rating",
  "player_ht"               = "Height (cm)",
  "n_prior_gs_ll_won"       = "Prior GS LL spots won",
  "n_prior_gs_ll_notwon"    = "Prior GS LL candidacies (not won)",
  "n_prior_nongs_ll_won"    = "Prior non-GS LL spots won",
  "n_prior_nongs_ll_notwon" = "Prior non-GS LL candidacies (not won)",
  "n_prior_gs_ll_opp"       = "Prior final-round qual. losses at LL-granting GS events",
  "n_prior_nongs_ll_opp"    = "Prior final-round qual. losses at LL-granting non-GS events"
)

build_balance_table <- function(data, filename, table_label) {
  ll_data  <- data |> filter(got_ll == 1)
  ctl_data <- data |> filter(got_ll == 0)

  tex <- c(
    "\\begin{tabular}{l c c c c}",
    "\\toprule",
    " & LL Mean (SD) & Control Mean (SD) & Difference & $p$-value \\\\",
    "\\midrule"
  )

  for (vn in names(balance_vars)) {
    if (!vn %in% names(data)) next
    vlab <- balance_vars[vn]

    ll_vals  <- ll_data[[vn]]
    ctl_vals <- ctl_data[[vn]]

    ll_mean  <- mean(ll_vals, na.rm = TRUE)
    ll_sd    <- sd(ll_vals, na.rm = TRUE)
    ctl_mean <- mean(ctl_vals, na.rm = TRUE)
    ctl_sd   <- sd(ctl_vals, na.rm = TRUE)
    diff_val <- ll_mean - ctl_mean

    # t-test for difference
    pv <- tryCatch({
      t.test(ll_vals, ctl_vals)$p.value
    }, error = function(e) NA_real_)

    ll_cell  <- paste0(fmt(ll_mean), " (", fmt(ll_sd), ")")
    ctl_cell <- paste0(fmt(ctl_mean), " (", fmt(ctl_sd), ")")
    diff_cell <- fmt(diff_val)
    pv_cell  <- if (!is.na(pv)) fmt(pv, 3) else "--"

    tex <- c(tex, paste0(vlab, " & ", ll_cell, " & ", ctl_cell,
                         " & ", diff_cell, " & ", pv_cell, " \\\\"))
  }

  tex <- c(tex,
    "\\midrule",
    paste0("$N$ (LL) & \\multicolumn{4}{c}{", nrow(ll_data), "} \\\\"),
    paste0("$N$ (Control) & \\multicolumn{4}{c}{", nrow(ctl_data), "} \\\\"),
    "\\bottomrule",
    "\\end{tabular}"
  )

  writeLines(tex, file.path(TABLES_DIR, filename))
  message("  Saved: ", filename)
}

build_balance_table(gs_atp, "table_sumstats_gs_atp.tex", "GS ATP")
build_balance_table(gs_wta, "table_sumstats_gs_wta.tex", "GS WTA")
build_balance_table(nongs_atp, "table_sumstats_nongs_atp.tex", "Non-GS ATP")
build_balance_table(nongs_wta, "table_sumstats_nongs_wta.tex", "Non-GS WTA")

slog("## FIX 4: Summary stats tables (4 separate)")
slog("- table_sumstats_gs_atp.tex")
slog("- table_sumstats_gs_wta.tex")
slog("- table_sumstats_nongs_atp.tex")
slog("- table_sumstats_nongs_wta.tex")
slog("")


# ==============================================================================
# SAVE ALL RESULTS
# ==============================================================================
message("\n", strrep("=", 70))
message("SAVING ALL RESULTS")
message(strrep("=", 70))

all_results <- list(
  # GS dynamics
  stacked_gs_atp       = stacked_res_atp,
  stacked_gs_wta       = stacked_res_wta,
  stacked_gs_models_atp = out_atp$models,
  stacked_gs_models_wta = out_wta$models,

  # Non-GS CF dynamics
  stacked_nongs_atp    = stacked_nongs_res_atp,
  stacked_nongs_wta    = stacked_nongs_res_wta,
  nongs_cf_models_atp  = out_nongs_atp$models,
  nongs_cf_models_wta  = out_nongs_wta$models,
  rho_nongs_atp        = rho_nongs_atp,
  rho_nongs_wta        = rho_nongs_wta,

  # Heterogeneity
  hetero_gs_atp        = hetero_res_atp,
  hetero_gs_wta        = hetero_res_wta,
  hetero_nongs_atp     = hetero_nongs_atp,
  hetero_nongs_wta     = hetero_nongs_wta,

  # Dose
  dose_gs_atp          = dose_res_atp,
  dose_gs_wta          = dose_res_wta,
  dose_nongs_atp       = dose_nongs_atp,
  dose_nongs_wta       = dose_nongs_wta,

  # Robustness
  verified_res         = ver_res,
  firstll_atp          = first_res_atp,
  firstll_wta          = first_res_wta,

  # Event study data
  es_atp               = es_atp,
  es_wta               = es_wta
)

saveRDS(all_results, file.path(CLEANED_DIR, "zpre_cf_fixes_results.rds"))
message("  Saved: zpre_cf_fixes_results.rds")

# Also update the main estimation datasets
saveRDS(gs_est, file.path(CLEANED_DIR, "skeleton_gs_est.rds"))
saveRDS(nongs_est, file.path(CLEANED_DIR, "skeleton_nongs_est.rds"))
message("  Updated: skeleton_gs_est.rds, skeleton_nongs_est.rds")


# ==============================================================================
# WRITE SUMMARY
# ==============================================================================
summary_text <- paste(summary_log, collapse = "\n")
writeLines(summary_text, file.path(OUTPUT_DIR, "zpre_cf_fixes_summary.md"))
message("\n  Summary written to: Output/zpre_cf_fixes_summary.md")
message("\n", strrep("=", 70))
message("  30_zpre_cf_fixes.R COMPLETE")
message(strrep("=", 70))
