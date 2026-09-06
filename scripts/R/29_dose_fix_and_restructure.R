# ==============================================================================
# 29_dose_fix_and_restructure.R
# Fix dose model multicollinearity by centering matches_won at treated-group
# mean, restructure table placement (first-LL primary), and document CF method.
#
# Purpose:
#   TASK 1: Center matches_won at treated-group mean before creating
#           interactions to resolve inconsistency between pooled and dose base
#   TASK 2: Create table placement mapping (first-LL as primary)
#   TASK 3: Document non-GS control function methodology
#
# Inputs:
#   Data/cleaned/tournament_match_fix_results.rds  (from script 24)
#   Data/cleaned/tournament_firstll_results.rds    (from script 28)
#   Data/cleaned/tournament_dose_results.rds       (from script 28)
#   Data/cleaned/tournament_elo_cache.rds
#   Data/raw/atp_main_matches.rds, atp_qual_chall_matches.rds
#   Data/raw/wta_main_matches.rds, wta_qual_itf_matches.rds
#
# Outputs:
#   Tables/table_tournament_dose_gs_atp.tex    (overwritten with centered spec)
#   Tables/table_tournament_dose_gs_wta.tex
#   Tables/table_tournament_dose_nongs_atp.tex
#   Tables/table_tournament_dose_nongs_wta.tex
#   Figures/fig_dose_response.pdf              (updated)
#   Output/table_placement.md
#   Output/nongs_cf_methodology.md
#   Output/dose_fix_diagnostics.md
#   Data/cleaned/tournament_dose_results_centered.rds
#
# Dependencies: dplyr, tidyr, readr, stringr, data.table, sandwich, ggplot2, here
# ==============================================================================

set.seed(20260326)

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(data.table)
library(sandwich)
library(here)

source(here("scripts", "R", "utils.R"))

# -- Project paths -------------------------------------------------------------
RAW_DIR     <- here("Data", "raw")
CLEANED_DIR <- here("Data", "cleaned")
TABLE_DIR   <- here("Tables")
FIG_DIR     <- here("Figures")
OUTPUT_DIR  <- here("Output")

for (d in c(CLEANED_DIR, TABLE_DIR, FIG_DIR, OUTPUT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

N_BOOT <- 200L

# -- Initialize summary log ----------------------------------------------------
summary_log <- character()

cat("\n")
message(strrep("=", 72))
message("  DOSE FIX (CENTERING) + RESTRUCTURE (29_dose_fix_and_restructure.R)")
message(strrep("=", 72))


###############################################################################
# PHASE 1: LOAD DATA
###############################################################################

message("\n[1] Loading saved data from scripts 24 and 28...")

R24 <- readRDS(file.path(CLEANED_DIR, "tournament_match_fix_results.rds"))
R28 <- readRDS(file.path(CLEANED_DIR, "tournament_firstll_results.rds"))

# Load Elo cache
elo_list <- readRDS(file.path(CLEANED_DIR, "tournament_elo_cache.rds"))

get_elo_at_date <- function(env, player, date) {
  df <- env[[player]]
  if (is.null(df)) return(1500)
  v <- df[df$date <= date, ]
  if (nrow(v) == 0) return(1500)
  tail(v$rating, 1)
}

# Load raw match data (needed for computing matches_won)
atp_main <- read_rds(file.path(RAW_DIR, "atp_main_matches.rds"))
atp_qual <- read_rds(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
wta_main <- read_rds(file.path(RAW_DIR, "wta_main_matches.rds"))
wta_qual <- read_rds(file.path(RAW_DIR, "wta_qual_itf_matches.rds"))

# Harmonize matches (same as script 28)
harmonize_matches <- function(main_df, qual_df, tour_label) {
  ensure_cols <- function(df) {
    for (col in c("winner_entry", "loser_entry", "best_of", "winner_age",
                  "loser_age", "match_num", "draw_size", "winner_id", "loser_id",
                  "winner_ht", "loser_ht", "winner_hand", "loser_hand",
                  "winner_ioc", "loser_ioc", "winner_rank", "loser_rank",
                  "winner_rank_points", "loser_rank_points")) {
      if (!col %in% names(df)) df[[col]] <- NA
    }
    df
  }
  main_df <- ensure_cols(main_df) |> mutate(match_source = "main")
  qual_df <- ensure_cols(qual_df) |> mutate(match_source = "qual")
  common_cols <- intersect(names(main_df), names(qual_df))
  bind_rows(
    main_df |> select(all_of(common_cols)),
    qual_df |> select(all_of(common_cols))
  ) |>
    mutate(
      tour = tour_label,
      tourney_date = if (inherits(tourney_date, "Date")) tourney_date
                     else as.Date(as.character(tourney_date), format = "%Y%m%d"),
      year = as.integer(format(tourney_date, "%Y")),
      winner_pid = paste0(tour_label, "_", winner_id),
      loser_pid  = paste0(tour_label, "_", loser_id)
    ) |>
    filter(year >= 2000, year <= 2024)
}

atp_all <- harmonize_matches(atp_main, atp_qual, "ATP")
wta_all <- harmonize_matches(wta_main, wta_qual, "WTA")
matches <- bind_rows(atp_all, wta_all) |> arrange(tourney_date, tourney_id, match_num)

message("  Combined matches: ", nrow(matches))


###############################################################################
# PHASE 2: RECONSTRUCT FIRST-LL SAMPLE WITH DOSE
###############################################################################

message("\n[2] Reconstructing first-LL samples with dose info...")

# Extract event-level data from R24
gs_atp_ev    <- R24$gs_atp$events
gs_wta_ev    <- R24$gs_wta$events
nongs_atp_ev <- R24$nongs_atp$events
nongs_wta_ev <- R24$nongs_wta$events

# Same-type match data
gs_atp_md    <- R24$gs_atp$matches
gs_wta_md    <- R24$gs_wta$matches
nongs_atp_md <- R24$nongs_atp$matches
nongs_wta_md <- R24$nongs_wta$matches

# Apply first-LL restriction
apply_firstll_restriction <- function(ev_table, md_table) {
  first_events <- ev_table |> filter(had_prior_ll == 0)
  first_eids <- first_events$event_id
  md_restricted <- md_table |> filter(event_id %in% first_eids)
  list(events = first_events, matches = md_restricted)
}

firstll <- list()
firstll$gs_atp_same    <- apply_firstll_restriction(gs_atp_ev, gs_atp_md)
firstll$gs_wta_same    <- apply_firstll_restriction(gs_wta_ev, gs_wta_md)
firstll$nongs_atp_same <- apply_firstll_restriction(nongs_atp_ev, nongs_atp_md)
firstll$nongs_wta_same <- apply_firstll_restriction(nongs_wta_ev, nongs_wta_md)

for (nm in names(firstll)) {
  slog(sprintf("  firstll/%s: events=%d (LL=%d), matches=%d",
               nm, nrow(firstll[[nm]]$events),
               sum(firstll[[nm]]$events$got_ll),
               nrow(firstll[[nm]]$matches)))
}


###############################################################################
# PHASE 3: COMPUTE matches_won AND CENTER AT TREATED-GROUP MEAN
###############################################################################

message("\n[3] Computing matches_won and centering at treated-group mean...")

compute_matches_won <- function(ev_table, all_matches, tour_label) {
  md <- all_matches |> filter(tour == tour_label, match_source == "main")
  ev_ll <- ev_table |> filter(got_ll == 1)
  if (nrow(ev_ll) == 0) {
    ev_table$matches_won <- 0L
    return(ev_table)
  }
  ll_wins <- ev_ll |>
    rowwise() |>
    mutate(
      matches_won = {
        wins <- md |>
          filter(winner_pid == player_pid, tourney_id == .data$tourney_id) |>
          nrow()
        as.integer(wins)
      }
    ) |>
    ungroup() |>
    select(event_id, matches_won)

  ev_table |>
    left_join(ll_wins, by = "event_id") |>
    mutate(matches_won = replace_na(matches_won, 0L))
}

# Compute matches_won for each sample
for (sample_key in names(firstll)) {
  tour <- ifelse(grepl("atp", sample_key), "ATP", "WTA")
  firstll[[sample_key]]$events <- compute_matches_won(
    firstll[[sample_key]]$events, matches, tour
  )
}

# Now CENTER at the treated-group mean and propagate to match data
centering_info <- list()

for (sample_key in names(firstll)) {
  ev <- firstll[[sample_key]]$events
  md <- firstll[[sample_key]]$matches

  # Compute treated-group mean of matches_won
  treated_mean <- mean(ev$matches_won[ev$got_ll == 1], na.rm = TRUE)
  centering_info[[sample_key]] <- list(
    mean_matches_won = treated_mean,
    n_treated = sum(ev$got_ll == 1),
    n_control = sum(ev$got_ll == 0)
  )

  # Center
  ev <- ev |>
    mutate(
      matches_won_c    = matches_won - treated_mean,
      matches_won_c_sq = matches_won_c^2
    )

  # Propagate to match data
  dose_lookup <- ev |> select(event_id, matches_won, matches_won_c, matches_won_c_sq)
  md <- md |>
    # Remove any old dose columns if present
    select(-any_of(c("matches_won", "matches_won_sq", "matches_won_c",
                     "matches_won_c_sq", "ll_x_mw", "ll_x_mw_sq"))) |>
    left_join(dose_lookup, by = "event_id") |>
    mutate(
      matches_won    = replace_na(matches_won, 0L),
      matches_won_c  = replace_na(matches_won_c, -treated_mean),
      matches_won_c_sq = replace_na(matches_won_c_sq, treated_mean^2),
      ll_x_mw_c      = got_ll * matches_won_c,
      ll_x_mw_c_sq   = got_ll * matches_won_c_sq
    )

  firstll[[sample_key]]$events  <- ev
  firstll[[sample_key]]$matches <- md

  slog(sprintf("  %s: treated_mean_matches_won = %.3f (N_treated=%d, N_control=%d)",
               sample_key, treated_mean,
               centering_info[[sample_key]]$n_treated,
               centering_info[[sample_key]]$n_control))
}

# Also report distribution of matches_won among treated
slog("\n  Distribution of matches_won among treated:")
for (sample_key in names(firstll)) {
  ev <- firstll[[sample_key]]$events
  treated <- ev |> filter(got_ll == 1)
  if (nrow(treated) > 0) {
    tab <- table(treated$matches_won)
    slog(sprintf("    %s: %s", sample_key,
                 paste(names(tab), "=", tab, collapse = ", ")))
  }
}


###############################################################################
# PHASE 4: IMPUTE AND PREPARE MATCH DATA
###############################################################################

message("\n[4] Imputing match data...")

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


###############################################################################
# PHASE 5: ESTIMATE CENTERED DOSE MODELS
###############################################################################

message("\n[5] Estimating centered dose models...")

# -- GS dose (clustered SEs, no CF) -------------------------------------------
estimate_gs_dose_centered <- function(match_df, centering_mean, label = "") {
  if (is.null(match_df) || nrow(match_df) < 30) {
    message("    ", label, ": insufficient observations")
    return(NULL)
  }

  df <- impute_match_data(match_df)

  fml <- won ~ got_ll + ll_x_mw_c + ll_x_mw_c_sq +
    log_rank_ratio + log_rank_ratio_sq + rank_diff +
    same_ioc + is_clay + is_grass + age_diff + height_diff +
    hand_mismatch + h2h_smoothed + n_h2h +
    pre_elo + opp_elo + pre_rank_pts +
    player_age_at_event

  model <- tryCatch(
    glm(fml, family = binomial(link = "logit"), data = df),
    error = function(e) { message("    ", label, " GLM error: ", e$message); NULL }
  )
  if (is.null(model) || !model$converged) {
    message("    ", label, ": model did not converge")
    return(NULL)
  }

  V <- vcovCL(model, cluster = df$focal_pid, type = "HC1")
  se_cl <- sqrt(diag(V))
  beta <- coef(model)

  # The centered coefficients
  dose_names_c <- c("got_ll", "ll_x_mw_c", "ll_x_mw_c_sq")
  coefs_c <- beta[dose_names_c]
  ses_c   <- se_cl[dose_names_c]
  ps_c    <- 2 * pnorm(-abs(coefs_c / ses_c))

  # The base effect (got_ll coef) is now the effect at the MEAN dose
  message(sprintf("    %s: base_at_mean=%.4f (p=%.4f), dose_c=%.4f, dose_c_sq=%.4f, N=%d",
                  label, coefs_c[1], ps_c[1], coefs_c[2], coefs_c[3], nrow(df)))

  # Implied total effects at 0, 1, 2, 3 ACTUAL wins
  # Back-transform from centered: matches_won_c = w - mean
  # Total effect = base + (w - mean)*dose_c + (w - mean)^2 * dose_c_sq
  implied <- data.frame(wins = 0:3)
  mu <- centering_mean
  for (w in 0:3) {
    wc <- w - mu
    total <- coefs_c[1] + wc * coefs_c[2] + wc^2 * coefs_c[3]
    # Gradient for delta method: d(total)/d(base, dose_c, dose_c_sq) = (1, wc, wc^2)
    grad <- c(1, wc, wc^2)
    V_dose <- V[dose_names_c, dose_names_c]
    se_total <- sqrt(as.numeric(t(grad) %*% V_dose %*% grad))
    p_total  <- 2 * pnorm(-abs(total / se_total))
    implied$total[implied$wins == w] <- total
    implied$se[implied$wins == w]    <- se_total
    implied$p[implied$wins == w]     <- p_total
  }

  n_matches <- nrow(df)
  n_events  <- n_distinct(df$event_id)

  list(coefs = unname(coefs_c), ses = unname(ses_c), ps = unname(ps_c),
       implied = implied, n_matches = n_matches, n_events = n_events,
       rhos = rep(NA_real_, 3), rho_ses = rep(NA_real_, 3), rho_ps = rep(NA_real_, 3),
       centering_mean = mu)
}

# -- NonGS dose (CF with v_hat dose interactions, bootstrap) -------------------
estimate_nongs_dose_centered <- function(match_df, ev_table, centering_mean,
                                          n_boot = N_BOOT, label = "") {
  if (is.null(match_df) || nrow(match_df) < 50) {
    message("    ", label, ": insufficient observations")
    return(NULL)
  }

  df <- impute_match_data(match_df)
  mu <- centering_mean

  # Create v_hat dose interactions (centered)
  df <- df |>
    mutate(
      v_x_mw_c    = v_hat * matches_won_c,
      v_x_mw_c_sq = v_hat * matches_won_c_sq
    )

  fml <- won ~ got_ll + ll_x_mw_c + ll_x_mw_c_sq +
    v_hat + v_x_mw_c + v_x_mw_c_sq +
    log_rank_ratio + log_rank_ratio_sq + rank_diff +
    same_ioc + is_clay + is_grass + age_diff + height_diff +
    hand_mismatch + h2h_smoothed + n_h2h +
    pre_elo + opp_elo + pre_rank_pts +
    player_age_at_event

  model <- tryCatch(
    glm(fml, family = binomial(link = "logit"), data = df),
    error = function(e) { message("    ", label, " GLM error: ", e$message); NULL }
  )
  if (is.null(model) || !model$converged) {
    message("    ", label, ": model did not converge")
    return(NULL)
  }

  s <- summary(model)$coefficients
  dose_names_c <- c("got_ll", "ll_x_mw_c", "ll_x_mw_c_sq")
  rho_names_c  <- c("v_hat", "v_x_mw_c", "v_x_mw_c_sq")

  delta_pts <- s[dose_names_c, "Estimate"]
  rho_pts   <- s[rho_names_c, "Estimate"]

  n_matches <- nrow(df)
  n_events  <- n_distinct(df$event_id)

  # Bootstrap
  players <- unique(ev_table$player_id)
  n_pl <- length(players)
  ev_by_player <- split(ev_table, ev_table$player_id)

  boot_deltas <- matrix(NA_real_, nrow = 0, ncol = 3)
  boot_rhos   <- matrix(NA_real_, nrow = 0, ncol = 3)
  t_start <- Sys.time()

  for (b in seq_len(n_boot)) {
    if (b %% 50 == 0) {
      elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
      message(sprintf("      %s: boot %d/%d (%.1f min)", label, b, n_boot, elapsed))
    }
    tryCatch({
      boot_players <- sample(players, n_pl, replace = TRUE)
      boot_ev <- bind_rows(lapply(boot_players, function(p) ev_by_player[[as.character(p)]]))
      if (nrow(boot_ev) < 20) next
      boot_md <- df |> filter(event_id %in% boot_ev$event_id)
      if (nrow(boot_md) < 50) next

      # Recompute v_hat for bootstrap sample
      q <- qnorm(boot_ev$p_ll)
      phi_q <- dnorm(q)
      boot_ev$v_hat <- boot_ev$got_ll * phi_q / boot_ev$p_ll -
        (1 - boot_ev$got_ll) * phi_q / (1 - boot_ev$p_ll)
      vhat_lookup <- boot_ev |> select(event_id, v_hat_boot = v_hat)
      boot_md <- boot_md |>
        left_join(vhat_lookup, by = "event_id") |>
        mutate(v_hat = coalesce(v_hat_boot, v_hat)) |>
        select(-v_hat_boot) |>
        mutate(v_x_mw_c = v_hat * matches_won_c,
               v_x_mw_c_sq = v_hat * matches_won_c_sq)

      fit_b <- glm(fml, family = binomial(link = "logit"), data = boot_md)
      if (!fit_b$converged) next
      b_coefs <- coef(fit_b)
      if (all(dose_names_c %in% names(b_coefs)) && all(rho_names_c %in% names(b_coefs))) {
        boot_deltas <- rbind(boot_deltas, unname(b_coefs[dose_names_c]))
        boot_rhos   <- rbind(boot_rhos,   unname(b_coefs[rho_names_c]))
      }
    }, error = function(e) NULL)
  }

  elapsed_total <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))

  if (nrow(boot_deltas) < 10) {
    delta_ses <- s[dose_names_c, "Std. Error"]
    rho_ses   <- s[rho_names_c, "Std. Error"]
  } else {
    delta_ses <- apply(boot_deltas, 2, sd, na.rm = TRUE)
    rho_ses   <- apply(boot_rhos, 2, sd, na.rm = TRUE)
  }

  delta_ps <- 2 * pnorm(-abs(delta_pts / delta_ses))
  rho_ps   <- 2 * pnorm(-abs(rho_pts / rho_ses))

  # Implied total effects at 0, 1, 2, 3 ACTUAL wins
  implied <- data.frame(wins = 0:3)
  for (w in 0:3) {
    wc <- w - mu
    total <- delta_pts[1] + wc * delta_pts[2] + wc^2 * delta_pts[3]
    if (nrow(boot_deltas) >= 10) {
      boot_totals <- boot_deltas[, 1] + wc * boot_deltas[, 2] + wc^2 * boot_deltas[, 3]
      se_total <- sd(boot_totals, na.rm = TRUE)
    } else {
      se_total <- NA_real_
    }
    p_total <- if (!is.na(se_total) && se_total > 0) 2 * pnorm(-abs(total / se_total)) else NA_real_
    implied$total[implied$wins == w] <- total
    implied$se[implied$wins == w]    <- se_total
    implied$p[implied$wins == w]     <- p_total
  }

  message(sprintf("    %s: base_at_mean=%.4f, dose_c=%.4f, dose_c_sq=%.4f, N=%d [%.1f min]",
                  label, delta_pts[1], delta_pts[2], delta_pts[3], n_matches, elapsed_total))

  list(coefs = unname(delta_pts), ses = unname(delta_ses), ps = unname(delta_ps),
       implied = implied, n_matches = n_matches, n_events = n_events,
       rhos = unname(rho_pts), rho_ses = unname(rho_ses), rho_ps = unname(rho_ps),
       centering_mean = mu)
}


###############################################################################
# PHASE 6: RUN CENTERED DOSE ESTIMATION
###############################################################################

message("\n[6] Running centered dose estimation for all 4 first-LL samples...")

dose_results_centered <- list()

# GS-ATP
message("\n  === Dose (centered): GS-ATP ===")
dose_results_centered$gs_atp <- estimate_gs_dose_centered(
  firstll$gs_atp_same$matches,
  centering_info$gs_atp_same$mean_matches_won,
  label = "dose-c GS-ATP"
)

# GS-WTA
message("\n  === Dose (centered): GS-WTA ===")
dose_results_centered$gs_wta <- estimate_gs_dose_centered(
  firstll$gs_wta_same$matches,
  centering_info$gs_wta_same$mean_matches_won,
  label = "dose-c GS-WTA"
)

# nonGS-ATP
message("\n  === Dose (centered): nonGS-ATP ===")
dose_results_centered$nongs_atp <- estimate_nongs_dose_centered(
  firstll$nongs_atp_same$matches,
  firstll$nongs_atp_same$events,
  centering_info$nongs_atp_same$mean_matches_won,
  label = "dose-c nonGS-ATP"
)

# nonGS-WTA
message("\n  === Dose (centered): nonGS-WTA ===")
dose_results_centered$nongs_wta <- estimate_nongs_dose_centered(
  firstll$nongs_wta_same$matches,
  firstll$nongs_wta_same$events,
  centering_info$nongs_wta_same$mean_matches_won,
  label = "dose-c nonGS-WTA"
)


###############################################################################
# PHASE 7: COMPARE POOLED vs DOSE BASE (DIAGNOSTIC)
###############################################################################

message("\n[7] Comparing pooled first-LL delta vs centered dose base delta...")

# Re-estimate simple pooled model (no dose) for comparison
pooled_comparison <- list()

for (sample_key in c("gs_atp_same", "gs_wta_same", "nongs_atp_same", "nongs_wta_same")) {
  df <- impute_match_data(firstll[[sample_key]]$matches)
  short_key <- gsub("_same$", "", sample_key)
  is_gs <- grepl("^gs_", sample_key)

  if (is_gs) {
    fml_pooled <- won ~ got_ll +
      log_rank_ratio + log_rank_ratio_sq + rank_diff +
      same_ioc + is_clay + is_grass + age_diff + height_diff +
      hand_mismatch + h2h_smoothed + n_h2h +
      pre_elo + opp_elo + pre_rank_pts +
      player_age_at_event

    model <- tryCatch(
      glm(fml_pooled, family = binomial(link = "logit"), data = df),
      error = function(e) NULL
    )
    if (!is.null(model) && model$converged) {
      V <- vcovCL(model, cluster = df$focal_pid, type = "HC1")
      delta <- coef(model)["got_ll"]
      delta_se <- sqrt(V["got_ll", "got_ll"])
      delta_p <- 2 * pnorm(-abs(delta / delta_se))
      pooled_comparison[[short_key]] <- list(delta = delta, se = delta_se, p = delta_p, n = nrow(df))
    }
  } else {
    fml_pooled <- won ~ got_ll + v_hat +
      log_rank_ratio + log_rank_ratio_sq + rank_diff +
      same_ioc + is_clay + is_grass + age_diff + height_diff +
      hand_mismatch + h2h_smoothed + n_h2h +
      pre_elo + opp_elo + pre_rank_pts +
      player_age_at_event

    model <- tryCatch(
      glm(fml_pooled, family = binomial(link = "logit"), data = df),
      error = function(e) NULL
    )
    if (!is.null(model) && model$converged) {
      s <- summary(model)$coefficients
      pooled_comparison[[short_key]] <- list(
        delta = s["got_ll", "Estimate"],
        se = s["got_ll", "Std. Error"],
        p = s["got_ll", "Pr(>|z|)"],
        n = nrow(df)
      )
    }
  }
}

# Print comparison
slog("\n## Pooled vs Centered Dose Base Comparison")
slog(sprintf("%-12s | %-20s | %-20s | %-20s", "Sample", "Pooled delta (p)",
             "Dose base at mean (p)", "Centering mean"))

for (short_key in c("gs_atp", "gs_wta", "nongs_atp", "nongs_wta")) {
  pooled <- pooled_comparison[[short_key]]
  dose_c <- dose_results_centered[[short_key]]
  ci <- centering_info[[paste0(short_key, "_same")]]

  pooled_str <- if (!is.null(pooled)) {
    sprintf("%.4f (p=%.3f)", pooled$delta, pooled$p)
  } else "N/A"

  dose_str <- if (!is.null(dose_c)) {
    sprintf("%.4f (p=%.3f)", dose_c$coefs[1], dose_c$ps[1])
  } else "N/A"

  mean_str <- sprintf("%.3f", ci$mean_matches_won)

  slog(sprintf("%-12s | %-20s | %-20s | %-20s", toupper(gsub("_", "-", short_key)),
               pooled_str, dose_str, mean_str))
}


###############################################################################
# PHASE 8: ALSO LOAD OLD DOSE RESULTS FOR COMPARISON
###############################################################################

message("\n[8] Loading old dose results for comparison...")

old_dose <- tryCatch(
  readRDS(file.path(CLEANED_DIR, "tournament_dose_results.rds")),
  error = function(e) { message("  Could not load old dose results"); NULL }
)

if (!is.null(old_dose)) {
  slog("\n## Old (uncentered) vs New (centered) Dose Base")
  for (short_key in c("gs_atp", "gs_wta", "nongs_atp", "nongs_wta")) {
    old_r <- old_dose$dose[[short_key]]
    new_r <- dose_results_centered[[short_key]]

    old_str <- if (!is.null(old_r)) sprintf("%.4f (p=%.3f)", old_r$coefs[1], old_r$ps[1]) else "N/A"
    new_str <- if (!is.null(new_r)) sprintf("%.4f (p=%.3f)", new_r$coefs[1], new_r$ps[1]) else "N/A"

    slog(sprintf("  %s: old_base=%s -> new_base=%s",
                 toupper(gsub("_", "-", short_key)), old_str, new_str))
  }
}


###############################################################################
# PHASE 9: WRITE CORRECTED DOSE TABLES
###############################################################################

message("\n[9] Writing corrected dose tables...")

write_dose_table_centered <- function(dose_result, filepath, is_gs = TRUE, label = "") {
  if (is.null(dose_result)) {
    message("  ", label, ": no dose result, skipping")
    return()
  }

  mu <- dose_result$centering_mean

  lines <- c(
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & Estimate & SE \\\\",
    "\\midrule",
    sprintf("\\multicolumn{3}{l}{\\textit{Panel A: Dose-Response Coefficients (centered at $\\bar{w}=%.2f$)}} \\\\[3pt]", mu)
  )

  dose_labels <- c(
    "$\\hat{\\delta}$ (LL effect at mean dose)",
    "$\\hat{\\delta}_{\\text{dose}}$ (LL $\\times$ (wins $-$ $\\bar{w}$))",
    "$\\hat{\\delta}_{\\text{dose}^2}$ (LL $\\times$ (wins $-$ $\\bar{w}$)$^2$)"
  )

  for (i in 1:3) {
    lines <- c(lines, sprintf("%s & %s%s & (%s) \\\\",
                              dose_labels[i],
                              fmt(dose_result$coefs[i], 4),
                              add_stars(dose_result$ps[i]),
                              fmt(dose_result$ses[i], 4)))
  }

  if (!is_gs && !all(is.na(dose_result$rhos))) {
    rho_labels <- c("$\\hat{\\rho}$ (base)",
                     "$\\hat{\\rho}_{\\text{dose}}$",
                     "$\\hat{\\rho}_{\\text{dose}^2}$")
    lines <- c(lines, "[4pt]")
    for (i in 1:3) {
      lines <- c(lines, sprintf("%s & %s%s & (%s) \\\\",
                                rho_labels[i],
                                fmt(dose_result$rhos[i], 4),
                                add_stars(dose_result$rho_ps[i]),
                                fmt(dose_result$rho_ses[i], 4)))
    }
  }

  # Implied total effects (back-transformed to actual wins)
  lines <- c(lines, "\\midrule",
    "\\multicolumn{3}{l}{\\textit{Panel B: Implied Total Effect by Wins at LL Event}} \\\\[3pt]")

  implied <- dose_result$implied
  for (j in seq_len(nrow(implied))) {
    w <- implied$wins[j]
    se_str <- if (!is.na(implied$se[j])) paste0("(", fmt(implied$se[j], 4), ")") else "---"
    lines <- c(lines, sprintf("At %d win%s & %s%s & %s \\\\",
                              w, ifelse(w == 1, "", "s"),
                              fmt(implied$total[j], 4),
                              add_stars(implied$p[j]),
                              se_str))
  }

  lines <- c(lines, "\\midrule",
    sprintf("$N$ matches & \\multicolumn{2}{c}{%s} \\\\", format(dose_result$n_matches, big.mark = ",")),
    sprintf("$N$ events & \\multicolumn{2}{c}{%s} \\\\", format(dose_result$n_events, big.mark = ",")),
    sprintf("Centering mean ($\\bar{w}$) & \\multicolumn{2}{c}{%s} \\\\", fmt(mu, 3)),
    "\\bottomrule", "\\end{tabular}")

  writeLines(lines, filepath)
  message("  Saved: ", filepath)
}

write_dose_table_centered(dose_results_centered$gs_atp,
                           file.path(TABLE_DIR, "table_tournament_dose_gs_atp.tex"),
                           is_gs = TRUE, label = "dose GS-ATP")
write_dose_table_centered(dose_results_centered$gs_wta,
                           file.path(TABLE_DIR, "table_tournament_dose_gs_wta.tex"),
                           is_gs = TRUE, label = "dose GS-WTA")
write_dose_table_centered(dose_results_centered$nongs_atp,
                           file.path(TABLE_DIR, "table_tournament_dose_nongs_atp.tex"),
                           is_gs = FALSE, label = "dose nonGS-ATP")
write_dose_table_centered(dose_results_centered$nongs_wta,
                           file.path(TABLE_DIR, "table_tournament_dose_nongs_wta.tex"),
                           is_gs = FALSE, label = "dose nonGS-WTA")


###############################################################################
# PHASE 10: DOSE-RESPONSE FIGURE (UPDATED)
###############################################################################

message("\n[10] Generating updated dose-response figure...")

dose_fig_data <- data.frame()
for (sample_key in c("gs_atp", "gs_wta", "nongs_atp", "nongs_wta")) {
  dr <- dose_results_centered[[sample_key]]
  if (is.null(dr)) next
  impl <- dr$implied
  impl$sample <- toupper(gsub("_", "-", sample_key))
  dose_fig_data <- bind_rows(dose_fig_data, impl)
}

if (nrow(dose_fig_data) > 0) {
  dose_fig_data <- dose_fig_data |>
    mutate(
      sample = factor(sample, levels = c("GS-ATP", "GS-WTA", "NONGS-ATP", "NONGS-WTA")),
      ci_lo = total - 1.96 * se,
      ci_hi = total + 1.96 * se
    )

  p_dose <- ggplot(dose_fig_data, aes(x = wins, y = total, color = sample)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.15, linewidth = 0.5) +
    scale_x_continuous(breaks = 0:3) +
    labs(x = "Main Draw Wins at LL Event", y = "Implied Total Effect (logit)") +
    theme_paper() +
    scale_color_manual(values = c("GS-ATP" = "#E69F00", "GS-WTA" = "#56B4E9",
                                   "NONGS-ATP" = "#009E73", "NONGS-WTA" = "#CC79A7"))

  ggsave(file.path(FIG_DIR, "fig_dose_response.pdf"), p_dose,
         width = 8, height = 5, device = cairo_pdf)
  message("  Saved: Figures/fig_dose_response.pdf")
}


###############################################################################
# PHASE 11: SAVE CENTERED DOSE RESULTS
###############################################################################

message("\n[11] Saving centered dose results...")

dose_save <- list(
  dose_centered = dose_results_centered,
  centering_info = centering_info,
  pooled_comparison = pooled_comparison
)
saveRDS(dose_save, file.path(CLEANED_DIR, "tournament_dose_results_centered.rds"))
message("  Saved: Data/cleaned/tournament_dose_results_centered.rds")


###############################################################################
# TASK 2: TABLE PLACEMENT MAPPING
###############################################################################

message("\n[12] Writing table placement mapping...")

placement_lines <- c(
  "# Table Placement: First-LL as Primary Analysis",
  "",
  "This document maps each table to its location in the paper.",
  "First-LL (type-specific restriction) tables appear in the MAIN TEXT.",
  "Full-sample tables and additional robustness appear in the APPENDIX.",
  "",
  "---",
  "",
  "## MAIN TEXT Tables (First-LL, Type-Specific Restriction)",
  "",
  "### Mechanisms Section (Tournament Performance)",
  "",
  "| Table File | Description | Paper Location |",
  "|------------|-------------|----------------|",
  "| `table_tournament_firstll_gs_atp.tex` | GS-ATP main effects (first-LL) | Mechanisms section, main result |",
  "| `table_tournament_firstll_gs_wta.tex` | GS-WTA main effects (first-LL) | Mechanisms section, main result |",
  "| `table_tournament_dose_gs_atp.tex` | Dose-response, ATP (centered) | Mechanisms section, dose analysis |",
  "| `table_tournament_dose_gs_wta.tex` | Dose-response, WTA (centered) | Mechanisms section, dose analysis |",
  "| `table_tournament_firstll_horizon_gs_atp.tex` | Horizon heterogeneity, ATP | Mechanisms section, persistence |",
  "| `table_tournament_firstll_horizon_gs_wta.tex` | Horizon heterogeneity, WTA | Mechanisms section, persistence |",
  "",
  "---",
  "",
  "## APPENDIX Tables",
  "",
  "### Full-Sample Results (Appendix A: Full Sample)",
  "",
  "| Table File | Description |",
  "|------------|-------------|",
  "| `table_tournament_gs_atp.tex` | GS-ATP full sample (all LL events, not just first) |",
  "| `table_tournament_gs_wta.tex` | GS-WTA full sample |",
  "",
  "### Robustness (Appendix B: Robustness)",
  "",
  "| Table File | Description |",
  "|------------|-------------|",
  "| `table_tournament_firstll_robust_gs_atp.tex` | First-LL robustness, GS-ATP |",
  "| `table_tournament_firstll_robust_gs_wta.tex` | First-LL robustness, GS-WTA |",
  "| `table_tournament_anytype_gs_atp.tex` | Any-type restriction, GS-ATP |",
  "| `table_tournament_anytype_gs_wta.tex` | Any-type restriction, GS-WTA |",
  "| `table_tournament_robustness_gs_atp.tex` | Full-sample robustness, GS-ATP |",
  "| `table_tournament_robustness_gs_wta.tex` | Full-sample robustness, GS-WTA |",
  "",
  "### Any-Type Variant (Appendix C: Alternative Restrictions)",
  "",
  "| Table File | Description |",
  "|------------|-------------|",
  "| `table_tournament_anytype_horizon_gs_atp.tex` | Any-type horizon, GS-ATP |",
  "| `table_tournament_anytype_horizon_gs_wta.tex` | Any-type horizon, GS-WTA |",
  "| `table_tournament_anytype_robust_gs_atp.tex` | Any-type robustness, GS-ATP |",
  "| `table_tournament_anytype_robust_gs_wta.tex` | Any-type robustness, GS-WTA |",
  "",
  "### Non-Grand Slam Results (Appendix D: Non-GS Events)",
  "",
  "| Table File | Description |",
  "|------------|-------------|",
  "| `table_tournament_firstll_nongs_atp.tex` | First-LL, nonGS-ATP |",
  "| `table_tournament_firstll_nongs_wta.tex` | First-LL, nonGS-WTA |",
  "| `table_tournament_dose_nongs_atp.tex` | Dose-response, nonGS-ATP (centered) |",
  "| `table_tournament_dose_nongs_wta.tex` | Dose-response, nonGS-WTA (centered) |",
  "| `table_tournament_firstll_horizon_nongs_atp.tex` | Horizon, nonGS-ATP |",
  "| `table_tournament_firstll_horizon_nongs_wta.tex` | Horizon, nonGS-WTA |",
  "| `table_tournament_nongs_atp.tex` | Full-sample, nonGS-ATP |",
  "| `table_tournament_nongs_wta.tex` | Full-sample, nonGS-WTA |",
  "| `table_tournament_firstll_robust_nongs_atp.tex` | Robustness, nonGS-ATP |",
  "| `table_tournament_firstll_robust_nongs_wta.tex` | Robustness, nonGS-WTA |",
  "| `table_tournament_anytype_nongs_atp.tex` | Any-type, nonGS-ATP |",
  "| `table_tournament_anytype_nongs_wta.tex` | Any-type, nonGS-WTA |",
  "| `table_tournament_anytype_horizon_nongs_atp.tex` | Any-type horizon, nonGS-ATP |",
  "| `table_tournament_anytype_horizon_nongs_wta.tex` | Any-type horizon, nonGS-WTA |",
  "| `table_tournament_anytype_robust_nongs_atp.tex` | Any-type robustness, nonGS-ATP |",
  "| `table_tournament_anytype_robust_nongs_wta.tex` | Any-type robustness, nonGS-WTA |",
  "| `table_tournament_robustness_nongs_atp.tex` | Full-sample robustness, nonGS-ATP |",
  "| `table_tournament_robustness_nongs_wta.tex` | Full-sample robustness, nonGS-WTA |",
  "| `table_tournament_horizon_nongs_atp.tex` | Full-sample horizon, nonGS-ATP |",
  "| `table_tournament_horizon_nongs_wta.tex` | Full-sample horizon, nonGS-WTA |",
  "",
  "---",
  "",
  "## Rationale",
  "",
  "The first-LL restriction is PRIMARY because:",
  "1. It isolates the clean treatment effect: players experiencing their first-ever LL opportunity",
  "2. Avoids contamination from prior LL experience (learning, confidence effects)",
  "3. Cleaner comparison group: no treated players appear multiple times with varying doses",
  "4. The full sample (including repeat LL recipients) is provided as robustness in the appendix",
  "",
  "Grand Slam results are primary because:",
  "1. GS events use a lottery for LL assignment (true randomization)",
  "2. Non-GS events use ranking-based assignment (requires CF/IV correction)",
  "3. GS results are the cleanest causal estimates; non-GS results validate external validity"
)

writeLines(placement_lines, file.path(OUTPUT_DIR, "table_placement.md"))
message("  Saved: Output/table_placement.md")


###############################################################################
# TASK 3: NON-GS CONTROL FUNCTION METHODOLOGY
###############################################################################

message("\n[13] Writing non-GS CF methodology document...")

cf_lines <- c(
  "# Control Function Approach for Non-Grand Slam Tournament Performance Model",
  "",
  "## Overview",
  "",
  "Non-Grand Slam (non-GS) events assign Lucky Loser (LL) spots based on ranking",
  "among qualifying-round losers, not by lottery. This creates potential selection",
  "bias: higher-ranked qualifying losers are more likely to receive LL entry AND",
  "may differ systematically in ability. We address this using a control function",
  "(CF) approach based on peer qualifying match outcomes as an instrument.",
  "",
  "## Stage 1: Treatment Probability via Bernoulli Convolution",
  "",
  "For each qualifying loser i at event e, we compute the probability of receiving",
  "an LL spot, P_i^{LL}, using the Bernoulli convolution method:",
  "",
  "1. Identify all qualifying matches whose outcomes affect LL ordering",
  "2. Each peer match j has probability p_j of the higher-ranked player winning",
  "   (estimated from historical data / Elo ratings)",
  "3. The LL assignment depends on the joint realization of all peer matches",
  "4. P_i^{LL} = sum over all outcome combinations that result in player i",
  "   receiving an LL spot, weighted by the probability of each combination",
  "",
  "This is a Bernoulli convolution because each peer match is an independent",
  "Bernoulli trial, and P_i^{LL} is a function of the sum/ordering of these trials.",
  "",
  "## Stage 2: Generalized Residual (Control Function)",
  "",
  "The generalized residual from the probit first stage is:",
  "",
  "  v_i = D_i * phi(Phi^{-1}(P_i^{LL})) / P_i^{LL}",
  "       - (1 - D_i) * phi(Phi^{-1}(P_i^{LL})) / (1 - P_i^{LL})",
  "",
  "where:",
  "  - D_i = 1 if player i received LL entry, 0 otherwise",
  "  - phi() is the standard normal PDF",
  "  - Phi^{-1}() is the standard normal quantile function (probit link inverse)",
  "  - P_i^{LL} is the treatment probability from Stage 1",
  "",
  "This is the inverse Mills ratio generalized to binary outcomes.",
  "",
  "## Stage 3: Second-Stage Logit",
  "",
  "### Pooled specification (no dose):",
  "",
  "  logit(P(won_{imt} = 1)) = delta * D_i + rho * v_i + X_{imt}' * beta + Z_i' * gamma",
  "",
  "### Dose-response specification (centered):",
  "",
  "  logit(P(won_{imt} = 1)) = delta * D_i",
  "    + delta_dose * D_i * (w_i - w_bar)",
  "    + delta_dose2 * D_i * (w_i - w_bar)^2",
  "    + rho * v_i",
  "    + rho_dose * v_i * (w_i - w_bar)",
  "    + rho_dose2 * v_i * (w_i - w_bar)^2",
  "    + X_{imt}' * beta + Z_i' * gamma",
  "",
  "where w_i = matches won at LL event and w_bar = mean(w_i | D_i = 1).",
  "",
  "The v_hat dose interactions (rho_dose, rho_dose2) are included because the",
  "selection correction may itself vary with dose: players who win more matches",
  "at the LL event may have different unobserved ability profiles.",
  "",
  "### Horizon-heterogeneous specification:",
  "",
  "  logit(P(won_{imt} = 1)) = sum_h delta_h * D_i * 1(t <= h)",
  "    + sum_h rho_h * v_i * 1(t <= h)",
  "    + X_{imt}' * beta + Z_i' * gamma",
  "",
  "where h indexes horizon windows (4w, 8w, 12w, 26w, 52w) and",
  "v_{ih} = v_i * 1(t <= h) allows the selection correction to vary by horizon.",
  "",
  "## Interpretation",
  "",
  "- delta: causal effect of LL entry on match win probability (at mean dose,",
  "  in the centered specification)",
  "- rho: coefficient on the control function; tests for endogeneity of LL entry.",
  "  If rho is statistically significant, naive logit without CF would be biased.",
  "- delta_dose, delta_dose2: how the LL effect varies with tournament performance",
  "  (matches won). Positive delta_dose suggests compound returns to winning.",
  "",
  "## Inference",
  "",
  "Because v_hat is a generated regressor (estimated in Stage 1), standard errors",
  "from the second-stage logit are invalid. We use a player-level block bootstrap:",
  "",
  "1. Resample players with replacement (preserving all events per player)",
  "2. Recompute v_hat for the bootstrap sample (re-derive from P_i^{LL})",
  "3. Re-estimate the second-stage logit",
  "4. Repeat B = 200 times",
  "5. Bootstrap standard errors = SD of bootstrap coefficient distribution",
  "",
  "The bootstrap is at the player level (not event or match level) because:",
  "- Multiple events per player create within-player correlation",
  "- Multiple matches per event create within-event correlation",
  "- Player-level resampling respects both clustering structures",
  "",
  "## Key Assumption: Exclusion Restriction",
  "",
  "The instrument (peer qualifying match outcomes) satisfies the exclusion",
  "restriction if: conditional on player i's own ability (proxied by Elo, ranking,",
  "age, etc.), the outcomes of other qualifying matches at the same event affect",
  "player i's future career only through whether player i receives the LL spot.",
  "",
  "This is plausible because: (a) peer match outcomes are determined by other",
  "players' performance, not by player i; (b) conditional on player i's observable",
  "characteristics, there is no direct channel from peer outcomes to player i's",
  "future match results."
)

writeLines(cf_lines, file.path(OUTPUT_DIR, "nongs_cf_methodology.md"))
message("  Saved: Output/nongs_cf_methodology.md")


###############################################################################
# PHASE 14: WRITE DIAGNOSTICS SUMMARY
###############################################################################

message("\n[14] Writing diagnostics summary...")

diag_lines <- c(
  "# Dose Model Fix Diagnostics",
  "",
  paste("Generated:", Sys.time()),
  "",
  "## Problem",
  "",
  "The uncentered dose model had base delta (effect at matches_won=0) that",
  "was far from the pooled delta because:",
  "- All control observations have matches_won = 0",
  "- The interaction terms (ll x matches_won, ll x matches_won^2) are",
  "  perfectly collinear with got_ll for control observations",
  "- This inflates standard errors and produces unstable base estimates",
  "",
  "## Fix",
  "",
  "Center matches_won at its treated-group mean before creating interactions:",
  "- matches_won_c = matches_won - mean(matches_won | treated)",
  "- Now the base coefficient on got_ll = effect at the AVERAGE dose",
  "- This should be close to the pooled delta (which averages over all doses)",
  "",
  "## Results",
  ""
)

for (short_key in c("gs_atp", "gs_wta", "nongs_atp", "nongs_wta")) {
  pooled <- pooled_comparison[[short_key]]
  dose_c <- dose_results_centered[[short_key]]
  old_r <- if (!is.null(old_dose)) old_dose$dose[[short_key]] else NULL
  ci <- centering_info[[paste0(short_key, "_same")]]

  diag_lines <- c(diag_lines, paste0("### ", toupper(gsub("_", "-", short_key))), "")
  diag_lines <- c(diag_lines, sprintf("- Centering mean: %.3f", ci$mean_matches_won))
  diag_lines <- c(diag_lines, sprintf("- N treated: %d, N control: %d", ci$n_treated, ci$n_control))

  if (!is.null(pooled)) {
    diag_lines <- c(diag_lines,
      sprintf("- Pooled delta (no dose): %.4f (SE=%.4f, p=%.3f, N=%d)",
              pooled$delta, pooled$se, pooled$p, pooled$n))
  }
  if (!is.null(old_r)) {
    diag_lines <- c(diag_lines,
      sprintf("- OLD dose base (uncentered, at wins=0): %.4f (SE=%.4f, p=%.3f)",
              old_r$coefs[1], old_r$ses[1], old_r$ps[1]))
  }
  if (!is.null(dose_c)) {
    diag_lines <- c(diag_lines,
      sprintf("- NEW dose base (centered, at mean dose): %.4f (SE=%.4f, p=%.3f)",
              dose_c$coefs[1], dose_c$ses[1], dose_c$ps[1]))
    diag_lines <- c(diag_lines, "- Implied effects:")
    for (j in seq_len(nrow(dose_c$implied))) {
      w <- dose_c$implied$wins[j]
      diag_lines <- c(diag_lines,
        sprintf("  - At %d wins: %.4f (SE=%.4f, p=%.3f)",
                w, dose_c$implied$total[j],
                dose_c$implied$se[j], dose_c$implied$p[j]))
    }
  }
  diag_lines <- c(diag_lines, "")
}

# Check: is inconsistency resolved?
diag_lines <- c(diag_lines, "## Inconsistency Check", "")
for (short_key in c("gs_atp", "gs_wta", "nongs_atp", "nongs_wta")) {
  pooled <- pooled_comparison[[short_key]]
  dose_c <- dose_results_centered[[short_key]]
  if (!is.null(pooled) && !is.null(dose_c)) {
    diff_abs <- abs(pooled$delta - dose_c$coefs[1])
    status <- if (diff_abs < 0.15) "RESOLVED" else "STILL DIVERGENT"
    diag_lines <- c(diag_lines,
      sprintf("- %s: |pooled - dose_base| = %.4f -> %s",
              toupper(gsub("_", "-", short_key)), diff_abs, status))
  }
}

writeLines(diag_lines, file.path(OUTPUT_DIR, "dose_fix_diagnostics.md"))
message("  Saved: Output/dose_fix_diagnostics.md")


###############################################################################
# DONE
###############################################################################

message("\n", strrep("=", 72))
message("  SCRIPT COMPLETE: 29_dose_fix_and_restructure.R")
message(strrep("=", 72))
message("\nOutputs:")
message("  Tables: 4 dose tables (overwritten with centered spec)")
message("  Figure: fig_dose_response.pdf (updated)")
message("  Data:   tournament_dose_results_centered.rds")
message("  Docs:   table_placement.md, nongs_cf_methodology.md, dose_fix_diagnostics.md")

cat("\n--- Summary Log ---\n")
cat(paste(summary_log, collapse = "\n"), "\n")
