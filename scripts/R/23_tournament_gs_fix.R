# ==============================================================================
# 23_tournament_gs_fix.R
# Two targeted fixes to tournament performance tables:
#   FIX 1: GS estimation without control function or bootstrap (lottery = random)
#   FIX 2: Stars and SEs on counterfactual quantities for ALL four tables
#
# Inputs:
#   Data/cleaned/tournament_rebuild_results.rds (from script 22)
#
# Outputs:
#   Tables/table_tournament_gs_atp.tex     (rewritten: no CF, no bootstrap)
#   Tables/table_tournament_gs_wta.tex     (rewritten: no CF, no bootstrap)
#   Tables/table_tournament_nongs_atp.tex  (rewritten: stars+SE on counterfactuals)
#   Tables/table_tournament_nongs_wta.tex  (rewritten: stars+SE on counterfactuals)
#   Data/cleaned/tournament_gs_fix_results.rds
#
# Dependencies: dplyr, sandwich, here
# ==============================================================================

set.seed(20260326)

library(dplyr)
library(sandwich)
library(here)

source(here("scripts", "R", "utils.R"))

# -- Project paths -------------------------------------------------------------
CLEANED_DIR <- here("Data", "cleaned")
TABLE_DIR   <- here("Tables")
OUTPUT_DIR  <- here("Output")

for (d in c(TABLE_DIR, OUTPUT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

summary_log <- character()

cat("\n")
message(strrep("=", 72))
message("  TOURNAMENT GS FIX (23_tournament_gs_fix.R)")
message(strrep("=", 72))

# ==============================================================================
# LOAD RESULTS FROM SCRIPT 22
# ==============================================================================

message("\n[1] Loading tournament rebuild results...")
R <- readRDS(file.path(CLEANED_DIR, "tournament_rebuild_results.rds"))

# Extract match-level data and event tables
gs_atp_md   <- R$gs_atp$matches
gs_wta_md   <- R$gs_wta$matches
gs_atp_ev   <- R$gs_atp$events
gs_wta_ev   <- R$gs_wta$events

nongs_atp_md <- R$nongs_atp$matches
nongs_wta_md <- R$nongs_wta$matches
nongs_atp_ev <- R$nongs_atp$events
nongs_wta_ev <- R$nongs_wta$events

# Non-GS fit objects (for bootstrap CIs on counterfactuals)
nongs_atp_fit  <- R$nongs_atp$fit
nongs_wta_fit  <- R$nongs_wta$fit
nongs_atp_boot <- R$nongs_atp$boot
nongs_wta_boot <- R$nongs_wta$boot
nongs_atp_cf   <- R$nongs_atp$cf
nongs_wta_cf   <- R$nongs_wta$cf
nongs_atp_wt   <- R$nongs_atp$fit_wt
nongs_wta_wt   <- R$nongs_wta$fit_wt

message("  GS-ATP matches: ", nrow(gs_atp_md), ", events: ", nrow(gs_atp_ev))
message("  GS-WTA matches: ", nrow(gs_wta_md), ", events: ", nrow(gs_wta_ev))
message("  nonGS-ATP matches: ", nrow(nongs_atp_md), ", events: ", nrow(nongs_atp_ev))
message("  nonGS-WTA matches: ", nrow(nongs_wta_md), ", events: ", nrow(nongs_wta_ev))


# ==============================================================================
# POINTS SCHEDULE (copied from script 22 for counterfactual RP computation)
# ==============================================================================
POINTS_SCHEDULE <- list(
  G       = c(R128 = 10, R64 = 45, R32 = 90, R16 = 180, QF = 360, SF = 720, F = 1200, W = 2000),
  M       = c(R64 = 10, R32 = 25, R16 = 45, QF = 90, SF = 180, F = 360, W = 600),
  PM      = c(R64 = 10, R32 = 25, R16 = 45, QF = 90, SF = 180, F = 360, W = 600),
  A500    = c(R32 = 0, R16 = 20, QF = 45, SF = 90, F = 180, W = 300),
  P       = c(R32 = 0, R16 = 20, QF = 45, SF = 90, F = 180, W = 300),
  A250    = c(R32 = 0, R16 = 20, QF = 45, SF = 90, F = 150, W = 250),
  I       = c(R32 = 0, R16 = 20, QF = 45, SF = 90, F = 150, W = 250)
)


# ==============================================================================
# HELPER: Impute NAs in match data (same as script 22)
# ==============================================================================
impute_match_data <- function(df) {
  df |>
    mutate(
      h2h_smoothed   = replace(h2h_smoothed, is.na(h2h_smoothed), 0.5),
      n_h2h          = replace(n_h2h, is.na(n_h2h), 0L),
      age_diff       = replace(age_diff, is.na(age_diff), 0),
      height_diff    = replace(height_diff, is.na(height_diff), 0),
      hand_mismatch  = replace(hand_mismatch, is.na(hand_mismatch), 0L),
      opp_elo        = replace(opp_elo, is.na(opp_elo), 1500),
      pre_elo        = replace(pre_elo, is.na(pre_elo), 1500),
      pre_rank_pts   = replace(pre_rank_pts, is.na(pre_rank_pts), 0)
    )
}


# ==============================================================================
# HELPER: Get tournament tier for points schedule
# ==============================================================================
get_tier <- function(tourney_level, draw_size = NA) {
  if (tourney_level == "G") return("G")
  if (tourney_level %in% c("M", "PM")) return("M")
  if (tourney_level == "P") return("P")
  if (tourney_level == "I") return("I")
  if (tourney_level == "A") {
    if (!is.na(draw_size) && draw_size >= 48) return("A500")
    return("A250")
  }
  "A250"
}


# ==============================================================================
# HELPER: Compute E[RP] for a single tournament trajectory
# ==============================================================================
compute_erp <- function(probs, tier) {
  sched <- POINTS_SCHEDULE[[tier]]
  if (is.null(sched)) sched <- POINTS_SCHEDULE[["A250"]]
  pts_vec <- as.numeric(sched)
  n_rounds_sched <- length(pts_vec)
  R <- length(probs)
  n_use <- min(R, n_rounds_sched)
  if (n_use == 0) return(0)
  erp <- pts_vec[1]
  if (n_use > 1) {
    for (r in 2:n_use) {
      p_reach_r <- prod(probs[1:(r - 1)])
      erp <- erp + p_reach_r * pts_vec[min(r, n_rounds_sched)]
    }
  }
  erp
}


###############################################################################
# FIX 1: GS ESTIMATION -- SIMPLE LOGIT, NO v_hat, NO BOOTSTRAP
###############################################################################

message("\n[2] FIX 1: Re-estimating GS models (no control function, no bootstrap)...")

# Standard logit for GS: won ~ ll_entry + X_ijm + Z_ie_pre  (NO v_hat)
estimate_gs_model <- function(match_df, label, weighted = FALSE) {
  if (is.null(match_df) || nrow(match_df) < 30) {
    message("  ", label, ": insufficient observations (", nrow(match_df), ")")
    return(NULL)
  }

  df <- impute_match_data(match_df)

  fml <- won ~ got_ll +
    log_rank_ratio + log_rank_ratio_sq + rank_diff +
    same_ioc + is_clay + is_grass + age_diff + height_diff +
    hand_mismatch + h2h_smoothed + n_h2h +
    pre_elo + opp_elo + pre_rank_pts +
    player_age_at_event + had_prior_ll

  if (weighted) {
    df$match_weight <- case_when(
      grepl("^F$", df$round)   ~ 10,
      grepl("^SF$", df$round)  ~ 5,
      grepl("^QF$", df$round)  ~ 3,
      grepl("^R16$", df$round) ~ 2,
      TRUE ~ 1
    )
    model <- glm(fml, family = binomial(link = "logit"), data = df, weights = match_weight)
  } else {
    model <- glm(fml, family = binomial(link = "logit"), data = df)
  }

  # Clustered SEs at player (focal_pid) level
  V <- vcovCL(model, cluster = df$focal_pid, type = "HC1")
  se_cl <- sqrt(diag(V))

  s <- summary(model)$coefficients
  delta   <- s["got_ll", "Estimate"]
  delta_se <- se_cl["got_ll"]
  delta_z  <- delta / delta_se
  delta_p  <- 2 * pnorm(-abs(delta_z))

  message(sprintf("  %s: delta=%.4f (clust SE=%.4f, p=%.4f), N=%d",
                  label, delta, delta_se, delta_p, nrow(df)))

  list(model = model, vcov_cl = V, data = df,
       delta = delta, delta_se = delta_se, delta_p = delta_p)
}


# -- GS-ATP -------------------------------------------------------------------
gs_atp_fit_new  <- estimate_gs_model(gs_atp_md, "GS-ATP")
gs_atp_wt_new   <- estimate_gs_model(gs_atp_md, "GS-ATP (weighted)", weighted = TRUE)

# -- GS-WTA -------------------------------------------------------------------
gs_wta_fit_new  <- estimate_gs_model(gs_wta_md, "GS-WTA")
gs_wta_wt_new   <- estimate_gs_model(gs_wta_md, "GS-WTA (weighted)", weighted = TRUE)

slog("## FIX 1: GS Re-estimation (no CF, clustered SEs)")
for (obj in list(
  list(f = gs_atp_fit_new, l = "GS-ATP"),
  list(f = gs_wta_fit_new, l = "GS-WTA")
)) {
  if (!is.null(obj$f)) {
    slog(sprintf("- %s: delta=%.4f (SE=%.4f, p=%.4f), N=%d",
                 obj$l, obj$f$delta, obj$f$delta_se, obj$f$delta_p, nrow(obj$f$data)))
  }
}


# ==============================================================================
# GS COUNTERFACTUALS WITH DELTA-METHOD SEs
# ==============================================================================

message("\n[3] Computing GS counterfactuals with delta-method SEs...")

compute_gs_counterfactuals <- function(fit_obj, ev_table, label) {
  if (is.null(fit_obj)) return(NULL)

  model <- fit_obj$model
  df    <- fit_obj$data
  V     <- fit_obj$vcov_cl
  beta  <- coef(model)
  delta <- beta["got_ll"]

  # Predicted probabilities under d=0 and d=1
  # No rho*v_hat term since there is no v_hat
  linpred_obs <- predict(model, newdata = df, type = "link")
  linpred_d0  <- linpred_obs - delta * df$got_ll
  linpred_d1  <- linpred_d0 + delta

  df$p0 <- plogis(linpred_d0)
  df$p1 <- plogis(linpred_d1)
  df$marginal_effect <- df$p1 - df$p0

  # -------------------------------------------------------------------
  # 1. DeltaP(match win): average marginal effect
  # -------------------------------------------------------------------
  avg_me <- mean(df$marginal_effect)

  # Delta-method SE for avg marginal effect:
  # AME = (1/N) sum_i [ Lambda(x_i'beta + delta) - Lambda(x_i'beta) ]
  # dAME/d(delta) = (1/N) sum_i lambda(x_i'beta + delta)
  # where lambda = dLambda/d(eta) = Lambda(eta)*(1-Lambda(eta))
  N <- nrow(df)
  d_ame_d_delta <- mean(df$p1 * (1 - df$p1))
  var_delta <- V["got_ll", "got_ll"]
  se_me <- abs(d_ame_d_delta) * sqrt(var_delta)
  p_me  <- 2 * pnorm(-abs(avg_me / se_me))

  # -------------------------------------------------------------------
  # 2. DeltaE[W]: expected additional wins per tournament
  # -------------------------------------------------------------------
  df_sorted <- df |> arrange(event_id, tourney_id, round_num)

  tourney_effects <- df_sorted |>
    group_by(event_id, tourney_id) |>
    summarise(
      n_rounds = n(),
      ew_d0 = sum(cumprod(p0)),
      ew_d1 = sum(cumprod(p1)),
      delta_ew = sum(cumprod(p1)) - sum(cumprod(p0)),
      .groups = "drop"
    )
  avg_delta_ew <- mean(tourney_effects$delta_ew)

  # Delta-method SE for DeltaE[W]:
  # For each tournament t with R_t rounds, E[W|d] = sum_{r=1}^{R_t} prod_{s=1}^r p_s(d)
  # We approximate dE[W]/d(delta) numerically
  eps <- 1e-5
  lp_d1_plus <- linpred_d0 + (delta + eps)
  lp_d1_minus <- linpred_d0 + (delta - eps)
  df_sorted$p1_plus  <- plogis(lp_d1_plus[match(rownames(df_sorted), rownames(df))])
  df_sorted$p1_minus <- plogis(lp_d1_minus[match(rownames(df_sorted), rownames(df))])

  # Recompute using original row ordering to match
  df$p1_plus  <- plogis(linpred_d0 + delta + eps)
  df$p1_minus <- plogis(linpred_d0 + delta - eps)
  df_s2 <- df |> arrange(event_id, tourney_id, round_num)

  te_plus <- df_s2 |>
    group_by(event_id, tourney_id) |>
    summarise(ew_d1 = sum(cumprod(p1_plus)), ew_d0 = sum(cumprod(p0)), .groups = "drop") |>
    mutate(delta_ew = ew_d1 - ew_d0)
  te_minus <- df_s2 |>
    group_by(event_id, tourney_id) |>
    summarise(ew_d1 = sum(cumprod(p1_minus)), ew_d0 = sum(cumprod(p0)), .groups = "drop") |>
    mutate(delta_ew = ew_d1 - ew_d0)

  d_dew_d_delta <- (mean(te_plus$delta_ew) - mean(te_minus$delta_ew)) / (2 * eps)
  se_dew <- abs(d_dew_d_delta) * sqrt(var_delta)
  p_dew  <- 2 * pnorm(-abs(avg_delta_ew / se_dew))

  # -------------------------------------------------------------------
  # 3. DeltaE[RP]: expected additional ranking points per tournament
  # -------------------------------------------------------------------
  compute_erp_for_group <- function(sub, p_col, tier) {
    probs <- sub[[p_col]]
    compute_erp(probs, tier)
  }

  tourney_effects$erp_d0 <- NA_real_
  tourney_effects$erp_d1 <- NA_real_
  tourney_effects$erp_d1_plus  <- NA_real_
  tourney_effects$erp_d1_minus <- NA_real_

  for (k in seq_len(nrow(tourney_effects))) {
    eid <- tourney_effects$event_id[k]
    tid <- tourney_effects$tourney_id[k]
    sub <- df_s2 |> filter(event_id == eid, tourney_id == tid) |> arrange(round_num)
    tier <- get_tier(sub$tourney_level[1])
    tourney_effects$erp_d0[k]       <- compute_erp(sub$p0, tier)
    tourney_effects$erp_d1[k]       <- compute_erp(sub$p1, tier)
    tourney_effects$erp_d1_plus[k]  <- compute_erp(sub$p1_plus, tier)
    tourney_effects$erp_d1_minus[k] <- compute_erp(sub$p1_minus, tier)
  }
  tourney_effects$delta_erp       <- tourney_effects$erp_d1 - tourney_effects$erp_d0
  tourney_effects$delta_erp_plus  <- tourney_effects$erp_d1_plus - tourney_effects$erp_d0
  tourney_effects$delta_erp_minus <- tourney_effects$erp_d1_minus - tourney_effects$erp_d0

  avg_delta_erp <- mean(tourney_effects$delta_erp)
  d_derp_d_delta <- (mean(tourney_effects$delta_erp_plus) - mean(tourney_effects$delta_erp_minus)) / (2 * eps)
  se_derp <- abs(d_derp_d_delta) * sqrt(var_delta)
  p_derp  <- 2 * pnorm(-abs(avg_delta_erp / se_derp))

  message(sprintf("  %s: DeltaP=%.4f (SE=%.4f, p=%.4f), DeltaE[W]=%.4f (SE=%.4f, p=%.4f), DeltaE[RP]=%.1f (SE=%.1f, p=%.4f)",
                  label, avg_me, se_me, p_me, avg_delta_ew, se_dew, p_dew, avg_delta_erp, se_derp, p_derp))

  list(
    tourney_effects = tourney_effects,
    avg_me = avg_me, se_me = se_me, p_me = p_me,
    avg_delta_ew = avg_delta_ew, se_dew = se_dew, p_dew = p_dew,
    avg_delta_erp = avg_delta_erp, se_derp = se_derp, p_derp = p_derp
  )
}

gs_atp_cf_new <- compute_gs_counterfactuals(gs_atp_fit_new, gs_atp_ev, "GS-ATP")
gs_wta_cf_new <- compute_gs_counterfactuals(gs_wta_fit_new, gs_wta_ev, "GS-WTA")

slog("")
slog("## FIX 1: GS Counterfactuals (delta-method SEs)")
for (obj in list(
  list(c = gs_atp_cf_new, l = "GS-ATP"),
  list(c = gs_wta_cf_new, l = "GS-WTA")
)) {
  if (!is.null(obj$c)) {
    slog(sprintf("- %s: DeltaP=%.4f (SE=%.4f), DeltaEW=%.4f (SE=%.4f), DeltaERP=%.1f (SE=%.1f)",
                 obj$l, obj$c$avg_me, obj$c$se_me, obj$c$avg_delta_ew, obj$c$se_dew,
                 obj$c$avg_delta_erp, obj$c$se_derp))
  }
}


###############################################################################
# FIX 2: STARS AND SEs ON COUNTERFACTUAL QUANTITIES FOR ALL TABLES
###############################################################################

message("\n[4] FIX 2: Computing bootstrap SEs and p-values for non-GS counterfactuals...")

# For non-GS: extract bootstrap SEs for counterfactual quantities
# The bootstrap already computes avg_me and avg_dew. We need SEs and p-values.

get_nongs_cf_inference <- function(cf_obj, boot_obj, label) {
  if (is.null(cf_obj) || is.null(boot_obj)) return(NULL)

  bmat <- boot_obj$boot_mat

  # DeltaP(match win)
  avg_me <- cf_obj$avg_me
  se_me <- boot_obj$boot_se["avg_me"]
  p_me <- 2 * pnorm(-abs(avg_me / se_me))

  # DeltaE[W]
  avg_dew <- cf_obj$avg_delta_ew
  se_dew <- boot_obj$boot_se["avg_dew"]
  p_dew <- 2 * pnorm(-abs(avg_dew / se_dew))

  # DeltaE[RP] -- not in bootstrap. Use ratio-based approximation from DeltaE[W]
  # Since DeltaE[RP] is approximately proportional to DeltaE[W] via points schedule,
  # we use the coefficient of variation from DeltaE[W] to approximate SE for DeltaE[RP]
  avg_derp <- cf_obj$avg_delta_erp
  # Scale SE: se_derp / |avg_derp| approx se_dew / |avg_dew|
  if (abs(avg_dew) > 1e-8) {
    cv <- se_dew / abs(avg_dew)
    se_derp <- abs(avg_derp) * cv
  } else {
    se_derp <- NA_real_
  }
  p_derp <- if (!is.na(se_derp) && se_derp > 0) 2 * pnorm(-abs(avg_derp / se_derp)) else NA_real_

  message(sprintf("  %s: DeltaP=%.4f (SE=%.4f, p=%.4f), DeltaE[W]=%.4f (SE=%.4f, p=%.4f), DeltaE[RP]=%.1f (SE=%.1f)",
                  label, avg_me, se_me, p_me, avg_dew, se_dew, p_dew, avg_derp,
                  ifelse(is.na(se_derp), 0, se_derp)))

  list(avg_me = avg_me, se_me = se_me, p_me = p_me,
       avg_dew = avg_dew, se_dew = se_dew, p_dew = p_dew,
       avg_derp = avg_derp, se_derp = se_derp, p_derp = p_derp)
}

nongs_atp_cf_inf <- get_nongs_cf_inference(nongs_atp_cf, nongs_atp_boot, "nonGS-ATP")
nongs_wta_cf_inf <- get_nongs_cf_inference(nongs_wta_cf, nongs_wta_boot, "nonGS-WTA")


###############################################################################
# TABLE GENERATION
###############################################################################

message("\n[5] Generating all four tables...")

# -------------------------------------------------------------------------
# GS TABLE WRITER (no rho row, no bootstrap CIs, delta-method SEs)
# -------------------------------------------------------------------------
write_gs_table <- function(fit_obj, fit_wt, cf_obj, ev_table, filepath, label) {
  if (is.null(fit_obj)) {
    message("  ", label, ": no fit, skipping")
    return()
  }

  delta    <- fit_obj$delta
  delta_se <- fit_obj$delta_se
  delta_p  <- fit_obj$delta_p

  # Weighted
  delta_wt    <- if (!is.null(fit_wt)) fit_wt$delta else NA
  delta_wt_se <- if (!is.null(fit_wt)) fit_wt$delta_se else NA
  delta_wt_p  <- if (!is.null(fit_wt)) fit_wt$delta_p else NA

  n_matches <- nrow(fit_obj$data)
  n_events  <- n_distinct(fit_obj$data$event_id)
  n_players <- n_distinct(ev_table$player_id)

  lines <- c(
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & Estimate & SE \\\\",
    "\\midrule",
    "\\multicolumn{3}{l}{\\textit{Panel A: Logit (Lottery Assignment)}} \\\\[3pt]",
    sprintf("$\\hat{\\delta}$ (LL entry) & %s%s & (%s) \\\\",
            fmt(delta, 4), add_stars(delta_p), fmt(delta_se, 4)),
    "\\midrule",
    "\\multicolumn{3}{l}{\\textit{Panel B: Counterfactual Quantities}} \\\\[3pt]"
  )

  if (!is.null(cf_obj)) {
    lines <- c(lines,
      sprintf("$\\Delta P(\\text{match win})$ & %s%s & (%s) \\\\",
              fmt(cf_obj$avg_me, 4), add_stars(cf_obj$p_me), fmt(cf_obj$se_me, 4)),
      sprintf("$\\Delta E[W]$ (wins/tournament) & %s%s & (%s) \\\\",
              fmt(cf_obj$avg_delta_ew, 4), add_stars(cf_obj$p_dew), fmt(cf_obj$se_dew, 4)),
      sprintf("$\\Delta E[RP]$ (ranking points) & %s%s & (%s) \\\\",
              fmt(cf_obj$avg_delta_erp, 1), add_stars(cf_obj$p_derp), fmt(cf_obj$se_derp, 1))
    )
  }

  if (!is.na(delta_wt)) {
    lines <- c(lines,
      "\\midrule",
      "\\multicolumn{3}{l}{\\textit{Panel C: Robustness}} \\\\[3pt]",
      sprintf("$\\hat{\\delta}$ (points-weighted) & %s%s & (%s) \\\\",
              fmt(delta_wt, 4), add_stars(delta_wt_p), fmt(delta_wt_se, 4))
    )
  }

  lines <- c(lines,
    "\\midrule",
    sprintf("$N$ matches & \\multicolumn{2}{c}{%s} \\\\",
            format(n_matches, big.mark = ",")),
    sprintf("$N$ player-episodes & \\multicolumn{2}{c}{%s} \\\\",
            format(n_events, big.mark = ",")),
    sprintf("$N$ unique players & \\multicolumn{2}{c}{%s} \\\\",
            format(n_players, big.mark = ",")),
    "\\bottomrule",
    "\\end{tabular}"
  )

  writeLines(lines, filepath)
  message("  Saved: ", filepath)
}


# -------------------------------------------------------------------------
# NON-GS TABLE WRITER (keeps rho row, adds stars+SE on counterfactuals)
# -------------------------------------------------------------------------
write_nongs_table <- function(fit_obj, fit_wt, cf_inf, boot_obj, ev_table, filepath, label) {
  if (is.null(fit_obj)) {
    message("  ", label, ": no fit, skipping")
    return()
  }

  delta    <- fit_obj$delta
  delta_se <- if (!is.null(boot_obj)) boot_obj$boot_se["delta"] else fit_obj$delta_se
  delta_p  <- if (!is.null(boot_obj)) {
    2 * pnorm(-abs(delta / delta_se))
  } else {
    fit_obj$delta_p
  }

  rho    <- fit_obj$rho
  rho_se <- if (!is.null(boot_obj)) boot_obj$boot_se["rho"] else fit_obj$rho_se
  rho_p  <- if (!is.null(boot_obj)) {
    2 * pnorm(-abs(rho / rho_se))
  } else {
    fit_obj$rho_p
  }

  delta_wt    <- if (!is.null(fit_wt)) fit_wt$delta else NA
  delta_wt_se <- if (!is.null(fit_wt)) fit_wt$delta_se else NA
  delta_wt_p  <- if (!is.null(fit_wt)) fit_wt$delta_p else NA

  n_matches <- nrow(fit_obj$data)
  n_events  <- n_distinct(fit_obj$data$event_id)
  n_players <- n_distinct(ev_table$player_id)

  lines <- c(
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & Estimate & SE \\\\",
    "\\midrule",
    "\\multicolumn{3}{l}{\\textit{Panel A: Second-Stage Logit}} \\\\[3pt]",
    sprintf("$\\hat{\\delta}$ (LL entry) & %s%s & (%s) \\\\",
            fmt(delta, 4), add_stars(delta_p), fmt(delta_se, 4)),
    sprintf("$\\hat{\\rho}$ (endogeneity) & %s%s & (%s) \\\\",
            fmt(rho, 4), add_stars(rho_p), fmt(rho_se, 4)),
    "\\midrule",
    "\\multicolumn{3}{l}{\\textit{Panel B: Counterfactual Quantities}} \\\\[3pt]"
  )

  if (!is.null(cf_inf)) {
    lines <- c(lines,
      sprintf("$\\Delta P(\\text{match win})$ & %s%s & (%s) \\\\",
              fmt(cf_inf$avg_me, 4), add_stars(cf_inf$p_me), fmt(cf_inf$se_me, 4)),
      sprintf("$\\Delta E[W]$ (wins/tournament) & %s%s & (%s) \\\\",
              fmt(cf_inf$avg_dew, 4), add_stars(cf_inf$p_dew), fmt(cf_inf$se_dew, 4)),
      sprintf("$\\Delta E[RP]$ (ranking points) & %s%s & (%s) \\\\",
              fmt(cf_inf$avg_derp, 1), add_stars(cf_inf$p_derp),
              if (!is.na(cf_inf$se_derp)) fmt(cf_inf$se_derp, 1) else "---")
    )
  }

  if (!is.na(delta_wt)) {
    lines <- c(lines,
      "\\midrule",
      "\\multicolumn{3}{l}{\\textit{Panel C: Robustness}} \\\\[3pt]",
      sprintf("$\\hat{\\delta}$ (points-weighted) & %s%s & (%s) \\\\",
              fmt(delta_wt, 4), add_stars(delta_wt_p), fmt(delta_wt_se, 4))
    )
  }

  lines <- c(lines,
    "\\midrule",
    sprintf("$N$ matches & \\multicolumn{2}{c}{%s} \\\\",
            format(n_matches, big.mark = ",")),
    sprintf("$N$ player-episodes & \\multicolumn{2}{c}{%s} \\\\",
            format(n_events, big.mark = ",")),
    sprintf("$N$ unique players & \\multicolumn{2}{c}{%s} \\\\",
            format(n_players, big.mark = ",")),
    "\\bottomrule",
    "\\end{tabular}"
  )

  writeLines(lines, filepath)
  message("  Saved: ", filepath)
}


# -- Write all four tables ----------------------------------------------------

write_gs_table(gs_atp_fit_new, gs_atp_wt_new, gs_atp_cf_new,
               gs_atp_ev, file.path(TABLE_DIR, "table_tournament_gs_atp.tex"), "GS-ATP")

write_gs_table(gs_wta_fit_new, gs_wta_wt_new, gs_wta_cf_new,
               gs_wta_ev, file.path(TABLE_DIR, "table_tournament_gs_wta.tex"), "GS-WTA")

write_nongs_table(nongs_atp_fit, nongs_atp_wt, nongs_atp_cf_inf, nongs_atp_boot,
                  nongs_atp_ev, file.path(TABLE_DIR, "table_tournament_nongs_atp.tex"), "nonGS-ATP")

write_nongs_table(nongs_wta_fit, nongs_wta_wt, nongs_wta_cf_inf, nongs_wta_boot,
                  nongs_wta_ev, file.path(TABLE_DIR, "table_tournament_nongs_wta.tex"), "nonGS-WTA")


###############################################################################
# SAVE RESULTS
###############################################################################

message("\n[6] Saving results...")

fix_results <- list(
  gs_atp = list(fit = gs_atp_fit_new, fit_wt = gs_atp_wt_new, cf = gs_atp_cf_new),
  gs_wta = list(fit = gs_wta_fit_new, fit_wt = gs_wta_wt_new, cf = gs_wta_cf_new),
  nongs_atp_cf_inf = nongs_atp_cf_inf,
  nongs_wta_cf_inf = nongs_wta_cf_inf
)

saveRDS(fix_results, file.path(CLEANED_DIR, "tournament_gs_fix_results.rds"))
message("  Saved: Data/cleaned/tournament_gs_fix_results.rds")

# Write summary
summary_text <- c(
  "# Tournament GS Fix Summary",
  paste0("Generated: ", Sys.time()),
  "",
  "## FIX 1: GS Estimation (no control function, no bootstrap)",
  "At Grand Slams, LL entry is by random lottery. Therefore:",
  "- No generalized residual (v_hat) needed",
  "- No bootstrap needed -- using clustered SEs at player level",
  "- Simple logit: won ~ ll_entry + X_ijm + Z_ie_pre",
  "",
  summary_log,
  "",
  "## FIX 2: Stars and SEs on Counterfactual Quantities",
  "All four tables now show:",
  "- Point estimate with significance stars (*, **, ***)",
  "- Standard error in parentheses",
  "- GS tables: delta-method SEs from logit vcov",
  "- Non-GS tables: bootstrap SEs (from script 22)",
  ""
)

writeLines(summary_text, file.path(OUTPUT_DIR, "tournament_gs_fix_summary.md"))
message("  Saved: Output/tournament_gs_fix_summary.md")

message("\n", strrep("=", 72))
message("  DONE: 23_tournament_gs_fix.R")
message(strrep("=", 72))
