# ==============================================================================
# 12b_tournament_estimation.R
# Fast estimation-only script for the Sequential Bernoulli Tournament Model.
# Loads pre-built match data and event table, runs CF-IV estimation, bootstrap,
# heterogeneity, counterfactual simulation, and produces all outputs.
#
# Inputs:
#   Data/cleaned/tournament_model_match_data.rds  (850K rows, from 12_tournament_performance.R)
#   Data/cleaned/tournament_model_event_table.rds  (event table, from 12_tournament_performance.R)
#
# Outputs:
#   Data/cleaned/tournament_model_results.rds
#   Tables/table_tournament_first_stage.tex
#   Tables/table_tournament_match_effects.tex
#   Tables/table_tournament_effects.tex
#   Tables/table_tournament_heterogeneity.tex
#   Figures/fig_dynamic_effects.pdf
#   Figures/fig_tournament_effects.pdf
#   Output/tournament_model_summary.md
#
# Dependencies: dplyr, tidyr, ggplot2, here, patchwork, data.table
# ==============================================================================

set.seed(20260321)

library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(patchwork)
library(data.table)

t_global_start <- Sys.time()

# -- Project paths -----------------------------------------------------------
CLEANED_DIR <- here("Data", "cleaned")
TABLE_DIR   <- here("Tables")
FIG_DIR     <- here("Figures")
OUTPUT_DIR  <- here("Output")

for (d in c(TABLE_DIR, FIG_DIR, OUTPUT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# -- Parameters --------------------------------------------------------------
T_HORIZON        <- 10L
CALENDAR_CAP     <- 365L
TRUNCATION_MODE  <- "at_next_event"
YEARS            <- 2000:2024
N_BOOT           <- 50L

# -- Custom theme (consistent with 07_figures.R) -----------------------------
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

col_treat   <- "#E69F00"
col_control <- "#56B4E9"

stars_fn <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}


###############################################################################
# SECTION 1: LOAD PRE-BUILT DATA
###############################################################################

message("================================================================")
message("  Lucky Loser CF-IV: Estimation-Only (Fast)")
message("================================================================")

message("\n[1/8] Loading pre-built match data and event table...")

match_df <- readRDS(file.path(CLEANED_DIR, "tournament_model_match_data.rds"))
event_table <- readRDS(file.path(CLEANED_DIR, "tournament_model_event_table.rds"))

# Convert to data.frame if data.table
if (inherits(match_df, "data.table")) match_df <- as.data.frame(match_df)
if (inherits(event_table, "data.table")) event_table <- as.data.frame(event_table)

message("  Match data: ", format(nrow(match_df), big.mark = ","), " rows x ", ncol(match_df), " cols")
message("  Event table: ", format(nrow(event_table), big.mark = ","), " events")
message("  Treated: ", sum(event_table$ll_entry), "  Control: ", sum(1 - event_table$ll_entry))

# Detect whether Bernoulli IV was used (check if instrument != p_ll_opportunity)
has_bernoulli_iv <- "selection_prob" %in% names(event_table)


###############################################################################
# SECTION 2: FIRST STAGE — Probit for LL Entry
###############################################################################

message("\n[2/8] First stage: Probit for LL entry...")

first_stage <- function(ev_table, verbose = TRUE) {
  model <- glm(ll_entry ~ instrument,
               family = binomial(link = "probit"),
               data = ev_table)

  # Generalized residuals (control function)
  lp <- predict(model, type = "link")
  phi <- dnorm(lp)
  Phi <- pnorm(lp)

  vhat <- ev_table$ll_entry * (phi / Phi) -
    (1 - ev_table$ll_entry) * (phi / (1 - Phi))

  ev_table$vhat <- vhat

  if (verbose) {
    s <- summary(model)
    message("  N events: ", nrow(ev_table))
    message("  Instrument coef: ", round(coef(model)["instrument"], 4))
    message("  Instrument p-value: ",
            round(s$coefficients["instrument", "Pr(>|z|)"], 6))
    message("  Pseudo-R2: ",
            round(1 - model$deviance / model$null.deviance, 4))
  }

  list(model = model, event_table = ev_table)
}

s1 <- first_stage(event_table)


###############################################################################
# SECTION 3: SECOND STAGE — Match-Level Logit + Control Function
###############################################################################

message("\n[3/8] Second stage: Match-level logit with control function...")

# Helper to prepare match data (merge vhat, impute NAs)
prepare_match_data <- function(match_data, ev_table) {
  df <- match_data |>
    left_join(ev_table |> select(event_id, vhat), by = "event_id")

  df <- df |>
    filter(!is.na(elo_diff), !is.na(vhat), !is.na(win), !is.na(ll_entry))

  df$h2h_smoothed[is.na(df$h2h_smoothed)] <- 0.5
  df$age_diff[is.na(df$age_diff)] <- 0
  med_wsf <- median(df$weeks_since_focal, na.rm = TRUE)
  df$weeks_since_focal[is.na(df$weeks_since_focal)] <- med_wsf
  df$weeks_since_opp[is.na(df$weeks_since_opp)] <- med_wsf
  df$surf_elo_diff[is.na(df$surf_elo_diff)] <- df$elo_diff[is.na(df$surf_elo_diff)]

  df$tourney_level <- factor(df$tourney_level)
  df$surface <- factor(df$surface)

  df
}

# --- Pooled CF-IV ---
second_stage_pooled <- function(df, verbose = TRUE) {
  model <- glm(win ~ elo_diff + surf_elo_diff + h2h_smoothed +
                 age_diff + bo5 +
                 tourney_level + surface +
                 weeks_since_focal +
                 ll_entry + vhat,
               family = binomial(link = "logit"),
               data = df)

  if (verbose) {
    s <- summary(model)$coefficients
    message("  N matches: ", nrow(df))
    message("  delta (ll_entry): ", round(coef(model)["ll_entry"], 4))
    message("  Odds ratio: ", round(exp(coef(model)["ll_entry"]), 4))
    if ("ll_entry" %in% rownames(s))
      message("  p-value: ", round(s["ll_entry", "Pr(>|z|)"], 4))
    if ("vhat" %in% rownames(s)) {
      message("  rho (vhat): ", round(coef(model)["vhat"], 4))
      message("  vhat p-value: ", round(s["vhat", "Pr(>|z|)"], 4))
    }
  }

  list(model = model, data = df)
}

# --- Dynamic (horizon-interacted) ---
second_stage_dynamic <- function(df, verbose = TRUE) {
  df$horizon <- factor(df$t_after_event)

  model <- glm(win ~ elo_diff + surf_elo_diff + h2h_smoothed +
                 age_diff + bo5 +
                 tourney_level + surface +
                 weeks_since_focal +
                 ll_entry:horizon + vhat:horizon,
               family = binomial(link = "logit"),
               data = df)

  if (verbose) {
    coef_tbl <- summary(model)$coefficients
    ll_rows <- grep("ll_entry:horizon", rownames(coef_tbl))
    message("  N matches: ", nrow(df))
    if (length(ll_rows) > 0) {
      message("  LL effect by horizon:")
      for (r in ll_rows) {
        message("    ", rownames(coef_tbl)[r], ": ",
                round(coef_tbl[r, 1], 4), " (SE=", round(coef_tbl[r, 2], 4),
                ", p=", round(coef_tbl[r, 4], 4), ")")
      }
    }
  }

  list(model = model, data = df)
}

# Prepare data once
df_prepared <- prepare_match_data(match_df, s1$event_table)
message("  Prepared data: ", format(nrow(df_prepared), big.mark = ","), " matches after filtering")

s2_pooled  <- second_stage_pooled(df_prepared)
s2_dynamic <- second_stage_dynamic(df_prepared)


###############################################################################
# SECTION 4: ROBUSTNESS — Naive Logit + First-LL-Only + Full-Window Proxy
###############################################################################

message("\n[4/8] Robustness checks...")

# --- Naive logit (no control function) ---
naive_model <- glm(win ~ elo_diff + surf_elo_diff + h2h_smoothed +
                     age_diff + bo5 +
                     tourney_level + surface +
                     weeks_since_focal +
                     ll_entry,
                   family = binomial(link = "logit"),
                   data = df_prepared)

s2_naive <- list(model = naive_model, data = df_prepared)
message("  Naive logit delta: ", round(coef(naive_model)["ll_entry"], 4),
        " (p=", round(summary(naive_model)$coefficients["ll_entry", "Pr(>|z|)"], 4), ")")

# --- First-LL-only ---
first_ll <- s1$event_table |>
  filter(ll_entry == 1) |>
  group_by(player) |>
  arrange(tourney_date) |>
  slice(1) |>
  ungroup()

first_ctrl <- s1$event_table |>
  filter(ll_entry == 0) |>
  group_by(player) |>
  arrange(tourney_date) |>
  slice(1) |>
  ungroup()

event_first <- bind_rows(first_ll, first_ctrl)
message("  First-LL sample: ", nrow(first_ll), " treated, ", nrow(first_ctrl), " control")

# Re-run first stage on restricted sample
s1_first <- first_stage(event_first, verbose = FALSE)
df_first <- prepare_match_data(
  match_df |> filter(event_id %in% event_first$event_id),
  s1_first$event_table
)

s2_first <- NULL
if (nrow(df_first) > 50) {
  s2_first <- second_stage_pooled(df_first, verbose = FALSE)
  message("  First-LL delta: ", round(coef(s2_first$model)["ll_entry"], 4))
}

# --- Full-window proxy: use all match data (no re-build needed) ---
# The saved match_df was built with "at_next_event" truncation.
# For a "full window" robustness, we simply re-use the same data
# (the structural difference would require rebuilding match data,
#  which we skip for speed). We note this in the summary.
s2_full_window <- NULL
message("  Full-window robustness: skipped (requires data rebuild)")


###############################################################################
# SECTION 5: COUNTERFACTUAL TOURNAMENT SIMULATION
###############################################################################

message("\n[5/8] Computing counterfactual tournament effects...")

compute_tournament_effects <- function(model, data) {
  beta <- coef(model)
  delta <- beta["ll_entry"]

  # Vectorized counterfactual predictions
  linpred_observed <- predict(model, newdata = data, type = "link")
  linpred_control <- linpred_observed - delta * data$ll_entry
  linpred_treated <- linpred_control + delta

  data$p0 <- plogis(linpred_control)
  data$p1 <- plogis(linpred_treated)
  data$marginal_effect <- data$p1 - data$p0

  # Use data.table for fast grouped operations
  cf_dt <- as.data.table(data)
  setorder(cf_dt, event_id, tourney_id, round_num)

  # Tournament-level effects
  tourney_df <- cf_dt[, {
    R <- .N
    cp0 <- cumprod(p0)
    cp1 <- cumprod(p1)
    ew_c <- sum(cp0)
    ew_t <- sum(cp1)
    tw_c <- cp0[R]
    tw_t <- cp1[R]

    list(
      player        = player[1],
      ll_entry      = ll_entry[1],
      t_after_event = t_after_event[1],
      n_rounds      = R,
      avg_match_me  = mean(p1 - p0),
      ew_control    = ew_c,
      ew_treated    = ew_t,
      delta_ew      = ew_t - ew_c,
      tw_control    = tw_c,
      tw_treated    = tw_t,
      delta_tw      = tw_t - tw_c,
      ratio_tw      = tw_t / max(tw_c, 1e-12),
      log_ratio_tw  = sum(log(p1)) - sum(log(p0))
    )
  }, by = .(event_id, tourney_id)]

  # Round-reaching effects
  round_df <- cf_dt[, {
    R <- .N
    reach_c <- cumprod(p0)
    reach_t <- cumprod(p1)
    list(
      round         = seq_len(R),
      reach_control = reach_c,
      reach_treated = reach_t,
      delta_reach   = reach_t - reach_c
    )
  }, by = .(event_id, tourney_id)]

  tourney_df <- as.data.frame(tourney_df)
  round_df   <- as.data.frame(round_df)

  message("  Match-level ATE (avg marginal effect): ",
          round(mean(tourney_df$avg_match_me), 4))
  message("  Tournament-level ATE on expected wins: ",
          round(mean(tourney_df$delta_ew), 4))
  message("  Tournament-level ATE on P(win tournament): ",
          round(mean(tourney_df$delta_tw), 6))

  list(tourney_effects = tourney_df,
       round_effects   = round_df,
       match_cf        = data)
}

te_results <- compute_tournament_effects(s2_pooled$model, s2_pooled$data)


###############################################################################
# SECTION 6: HETEROGENEITY ANALYSIS
###############################################################################

message("\n[6/8] Heterogeneity analysis...")

analyse_heterogeneity <- function(tourney_effects, match_data) {
  tourney_meta <- match_data |>
    distinct(event_id, tourney_id, qual_level, qual_surface,
             pre_elo_focal, bo5, tour) |>
    group_by(event_id, tourney_id) |>
    slice(1) |>
    ungroup()

  te <- tourney_effects |>
    left_join(tourney_meta, by = c("event_id", "tourney_id"))

  by_level <- te |>
    group_by(qual_level) |>
    summarise(n = n(), mean_dEW = mean(delta_ew), mean_dTW = mean(delta_tw),
              mean_match = mean(avg_match_me), .groups = "drop")
  message("  By tournament level:")
  for (j in seq_len(nrow(by_level))) {
    message("    ", by_level$qual_level[j], ": n=", by_level$n[j],
            " dEW=", round(by_level$mean_dEW[j], 4))
  }

  by_surface <- te |>
    group_by(qual_surface) |>
    summarise(n = n(), mean_dEW = mean(delta_ew), mean_dTW = mean(delta_tw),
              mean_match = mean(avg_match_me), .groups = "drop")

  te$elo_quartile <- cut(te$pre_elo_focal,
                          breaks = quantile(te$pre_elo_focal,
                                            probs = c(0, 0.25, 0.5, 0.75, 1),
                                            na.rm = TRUE),
                          labels = c("Q1 (weakest)", "Q2", "Q3", "Q4 (strongest)"),
                          include.lowest = TRUE)

  by_elo <- te |>
    filter(!is.na(elo_quartile)) |>
    group_by(elo_quartile) |>
    summarise(n = n(), mean_elo = mean(pre_elo_focal, na.rm = TRUE),
              mean_dEW = mean(delta_ew), mean_dTW = mean(delta_tw),
              mean_match = mean(avg_match_me), .groups = "drop")

  by_horizon <- te |>
    group_by(t_after_event) |>
    summarise(n = n(), mean_dEW = mean(delta_ew), mean_dTW = mean(delta_tw),
              mean_match = mean(avg_match_me), .groups = "drop")

  by_tour <- te |>
    group_by(tour) |>
    summarise(n = n(), mean_dEW = mean(delta_ew), mean_dTW = mean(delta_tw),
              mean_match = mean(avg_match_me), .groups = "drop")

  list(by_level = by_level, by_surface = by_surface,
       by_elo = by_elo, by_horizon = by_horizon,
       by_tour = by_tour, full = te)
}

het <- analyse_heterogeneity(te_results$tourney_effects, match_df)


###############################################################################
# SECTION 7: BOOTSTRAP INFERENCE
###############################################################################

message("\n[7/8] Bootstrap inference (", N_BOOT, " replications)...")

bootstrap_both_stages <- function(ev_table, match_data, n_boot = 50L,
                                   model_type = "pooled") {
  players <- unique(ev_table$player)
  n_players <- length(players)

  # Pre-split data by player for fast lookup
  ev_split <- split(ev_table, ev_table$player)
  match_split <- split(match_data, match_data$player)

  all_coefs <- vector("list", n_boot)
  n_success <- 0L

  message("  Bootstrapping ", n_boot, " replications (player-level blocks)...")

  for (b in seq_len(n_boot)) {
    if (b %% 10 == 0) message("    Replication ", b, " / ", n_boot)

    boot_players <- sample(players, n_players, replace = TRUE)

    # Build bootstrap samples using pre-split data
    boot_ev_list <- vector("list", n_players)
    boot_match_list <- vector("list", n_players)

    for (j in seq_len(n_players)) {
      p <- boot_players[j]
      ev_sub <- ev_split[[p]]
      if (is.null(ev_sub) || nrow(ev_sub) == 0L) next
      ev_sub$event_id <- ev_sub$event_id + j * 1e6
      boot_ev_list[[j]] <- ev_sub

      m_sub <- match_split[[p]]
      if (is.null(m_sub) || nrow(m_sub) == 0L) next
      m_sub$event_id <- m_sub$event_id + j * 1e6
      boot_match_list[[j]] <- m_sub
    }

    boot_events <- do.call(rbind, boot_ev_list[!sapply(boot_ev_list, is.null)])
    boot_matches <- do.call(rbind, boot_match_list[!sapply(boot_match_list, is.null)])

    if (is.null(boot_events) || nrow(boot_events) < 20) next
    if (is.null(boot_matches) || nrow(boot_matches) < 50) next

    tryCatch({
      s1_b <- first_stage(boot_events, verbose = FALSE)
      df_b <- prepare_match_data(boot_matches, s1_b$event_table)

      if (nrow(df_b) < 50) next

      if (model_type == "pooled") {
        s2_b <- second_stage_pooled(df_b, verbose = FALSE)
      } else {
        s2_b <- second_stage_dynamic(df_b, verbose = FALSE)
      }

      n_success <- n_success + 1L
      all_coefs[[n_success]] <- coef(s2_b$model)
    }, error = function(e) {})
  }

  message("  Successful replications: ", n_success, " of ", n_boot)

  if (n_success < 10) {
    message("  WARNING: Too few successful replications for reliable inference.")
    return(NULL)
  }

  all_coefs <- all_coefs[seq_len(n_success)]
  all_names <- unique(unlist(lapply(all_coefs, names)))
  boot_matrix <- matrix(NA, nrow = n_success, ncol = length(all_names),
                         dimnames = list(NULL, all_names))
  for (i in seq_len(n_success)) {
    boot_matrix[i, names(all_coefs[[i]])] <- all_coefs[[i]]
  }

  boot_se <- apply(boot_matrix, 2, sd, na.rm = TRUE)
  message("  Bootstrap SE for ll_entry: ",
          if ("ll_entry" %in% all_names) round(boot_se["ll_entry"], 4) else "N/A")

  boot_matrix
}

boot_pooled  <- bootstrap_both_stages(s1$event_table, match_df, N_BOOT, "pooled")
boot_dynamic <- bootstrap_both_stages(s1$event_table, match_df, N_BOOT, "dynamic")


###############################################################################
# SECTION 8: OUTPUTS — TABLES, FIGURES, SUMMARY
###############################################################################

message("\n[8/8] Generating outputs...")

# ---- Helper: extract coefficient info for tables ----------------------------

extract_coef_row <- function(model_obj, boot_mat = NULL, label = "") {
  if (is.null(model_obj)) return(NULL)
  m <- model_obj$model
  d <- model_obj$data
  s <- summary(m)$coefficients

  delta <- s["ll_entry", "Estimate"]
  delta_se_analytic <- s["ll_entry", "Std. Error"]
  delta_p_analytic <- s["ll_entry", "Pr(>|z|)"]

  if (!is.null(boot_mat) && "ll_entry" %in% colnames(boot_mat)) {
    delta_se <- sd(boot_mat[, "ll_entry"], na.rm = TRUE)
    delta_z <- delta / delta_se
    delta_p <- 2 * pnorm(-abs(delta_z))
  } else {
    delta_se <- delta_se_analytic
    delta_p <- delta_p_analytic
  }

  rho <- if ("vhat" %in% rownames(s)) s["vhat", "Estimate"] else NA
  rho_p <- if ("vhat" %in% rownames(s)) s["vhat", "Pr(>|z|)"] else NA

  data.frame(
    Specification = label,
    N_matches = nrow(d),
    N_events = length(unique(d$event_id)),
    delta = delta,
    delta_SE = delta_se,
    OR = exp(delta),
    p_value = delta_p,
    rho = rho,
    rho_p = rho_p,
    stringsAsFactors = FALSE
  )
}

# ---- Table: First Stage (Probit) -------------------------------------------

write_first_stage_table <- function(model, filepath) {
  s <- summary(model)$coefficients
  intercept <- s["(Intercept)", ]
  instrument_row <- s["instrument", ]
  n_obs <- model$df.null + 1
  pseudo_r2 <- 1 - model$deviance / model$null.deviance
  iv_label <- if (has_bernoulli_iv) "Selection Probability" else "P(LL opportunity)"

  lines <- c(
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & Coefficient & Std. Error \\\\",
    "\\midrule",
    sprintf("%s & %.4f%s & (%.4f) \\\\",
            iv_label, instrument_row[1], stars_fn(instrument_row[4]), instrument_row[2]),
    sprintf("Intercept & %.4f%s & (%.4f) \\\\",
            intercept[1], stars_fn(intercept[4]), intercept[2]),
    "\\midrule",
    sprintf("Observations & \\multicolumn{2}{c}{%s} \\\\",
            format(n_obs, big.mark = ",")),
    sprintf("Pseudo $R^2$ & \\multicolumn{2}{c}{%.4f} \\\\", pseudo_r2),
    "\\bottomrule",
    "\\end{tabular}"
  )

  writeLines(lines, filepath)
  message("  Saved: ", filepath)
}

write_first_stage_table(s1$model, file.path(TABLE_DIR, "table_tournament_first_stage.tex"))


# ---- Table: Match Effects ---------------------------------------------------

write_match_effects_table <- function(pooled, first_ll, full_window, naive,
                                       boot_mat, filepath) {
  rows <- list(
    extract_coef_row(pooled, boot_mat, "CF-IV (truncated)"),
    extract_coef_row(first_ll, NULL, "CF-IV (first LL only)"),
    extract_coef_row(full_window, NULL, "CF-IV (full window)"),
    extract_coef_row(naive, NULL, "Naive logit (no CF)")
  )
  rows <- rows[!sapply(rows, is.null)]
  tbl <- do.call(rbind, rows)

  lines <- c(
    "\\begin{tabular}{lccccccc}",
    "\\toprule",
    "Specification & $N_{\\text{matches}}$ & $N_{\\text{events}}$ & $\\hat{\\delta}$ & SE & OR & $\\hat{\\rho}$ & Endog.~$p$ \\\\",
    "\\midrule"
  )

  for (j in seq_len(nrow(tbl))) {
    r <- tbl[j, ]
    rho_str <- if (is.na(r$rho)) "---" else sprintf("%.4f", r$rho)
    rho_p_str <- if (is.na(r$rho_p)) "---" else sprintf("%.4f", r$rho_p)
    lines <- c(lines, sprintf(
      "%s & %s & %s & %.4f%s & (%.4f) & %.3f & %s & %s \\\\",
      r$Specification,
      format(r$N_matches, big.mark = ","),
      format(r$N_events, big.mark = ","),
      r$delta, stars_fn(r$p_value), r$delta_SE, r$OR,
      rho_str, rho_p_str
    ))
  }

  lines <- c(lines, "\\bottomrule", "\\end{tabular}")
  writeLines(lines, filepath)
  message("  Saved: ", filepath)
}

write_match_effects_table(s2_pooled, s2_first, s2_full_window, s2_naive,
                           boot_pooled,
                           file.path(TABLE_DIR, "table_tournament_match_effects.tex"))


# ---- Table: Tournament Effects ----------------------------------------------

write_tournament_effects_table <- function(te_results, filepath) {
  te <- te_results$tourney_effects
  lines <- c(
    "\\begin{tabular}{lccc}",
    "\\toprule",
    " & Match $\\Delta P(\\text{win})$ & $\\Delta E[W]$ & $\\Delta P(\\text{win tourney})$ \\\\",
    "\\midrule",
    sprintf("Mean & %.4f & %.4f & %.6f \\\\",
            mean(te$avg_match_me), mean(te$delta_ew), mean(te$delta_tw)),
    sprintf("Median & %.4f & %.4f & %.6f \\\\",
            median(te$avg_match_me), median(te$delta_ew), median(te$delta_tw)),
    sprintf("SD & %.4f & %.4f & %.6f \\\\",
            sd(te$avg_match_me), sd(te$delta_ew), sd(te$delta_tw)),
    sprintf("N (player-tournaments) & \\multicolumn{3}{c}{%s} \\\\",
            format(nrow(te), big.mark = ",")),
    "\\bottomrule",
    "\\end{tabular}"
  )

  writeLines(lines, filepath)
  message("  Saved: ", filepath)
}

write_tournament_effects_table(te_results,
                                file.path(TABLE_DIR, "table_tournament_effects.tex"))


# ---- Table: Heterogeneity --------------------------------------------------

write_heterogeneity_table <- function(het, filepath) {
  lines <- c(
    "\\begin{tabular}{llcccc}",
    "\\toprule",
    "Dimension & Category & $N$ & $\\Delta P(\\text{match})$ & $\\Delta E[W]$ & $\\Delta P(\\text{tourney})$ \\\\",
    "\\midrule"
  )

  for (j in seq_len(nrow(het$by_level))) {
    r <- het$by_level[j, ]
    prefix <- if (j == 1) "Tournament level" else ""
    lines <- c(lines, sprintf(
      "%s & %s & %s & %.4f & %.4f & %.6f \\\\",
      prefix, r$qual_level, format(r$n, big.mark = ","),
      r$mean_match, r$mean_dEW, r$mean_dTW
    ))
  }
  lines <- c(lines, "\\midrule")

  for (j in seq_len(nrow(het$by_surface))) {
    r <- het$by_surface[j, ]
    prefix <- if (j == 1) "Surface" else ""
    lines <- c(lines, sprintf(
      "%s & %s & %s & %.4f & %.4f & %.6f \\\\",
      prefix, r$qual_surface, format(r$n, big.mark = ","),
      r$mean_match, r$mean_dEW, r$mean_dTW
    ))
  }
  lines <- c(lines, "\\midrule")

  for (j in seq_len(nrow(het$by_elo))) {
    r <- het$by_elo[j, ]
    prefix <- if (j == 1) "Player strength" else ""
    lines <- c(lines, sprintf(
      "%s & %s & %s & %.4f & %.4f & %.6f \\\\",
      prefix, r$elo_quartile, format(r$n, big.mark = ","),
      r$mean_match, r$mean_dEW, r$mean_dTW
    ))
  }

  lines <- c(lines, "\\bottomrule", "\\end{tabular}")
  writeLines(lines, filepath)
  message("  Saved: ", filepath)
}

write_heterogeneity_table(het, file.path(TABLE_DIR, "table_tournament_heterogeneity.tex"))


# ---- Figure: Dynamic Effects ------------------------------------------------

make_dynamic_effects_plot <- function(dynamic_model, boot_matrix = NULL) {
  coef_tbl <- summary(dynamic_model$model)$coefficients
  ll_rows <- grep("ll_entry:horizon", rownames(coef_tbl))

  if (length(ll_rows) == 0) {
    message("  WARNING: No horizon interaction coefficients found.")
    return(NULL)
  }

  horizons <- as.integer(gsub(".*horizon", "", rownames(coef_tbl)[ll_rows]))
  deltas <- coef_tbl[ll_rows, "Estimate"]

  if (!is.null(boot_matrix)) {
    ll_cols <- grep("ll_entry:horizon", colnames(boot_matrix))
    if (length(ll_cols) > 0 && length(ll_cols) == length(ll_rows)) {
      se <- apply(boot_matrix[, ll_cols, drop = FALSE], 2, sd, na.rm = TRUE)
    } else {
      se <- coef_tbl[ll_rows, "Std. Error"]
    }
  } else {
    se <- coef_tbl[ll_rows, "Std. Error"]
  }

  n_by_h <- dynamic_model$data |>
    group_by(t_after_event) |>
    summarise(n = n(), .groups = "drop")

  plot_df <- data.frame(
    horizon = horizons,
    delta = deltas,
    se = se,
    lower = deltas - 1.96 * se,
    upper = deltas + 1.96 * se
  ) |>
    left_join(n_by_h, by = c("horizon" = "t_after_event"))

  p <- ggplot(plot_df, aes(x = horizon, y = delta)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50",
               linewidth = 0.5) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  width = 0.2, color = col_treat, linewidth = 0.6) +
    geom_point(color = col_treat, size = 3) +
    geom_line(color = col_treat, linewidth = 0.8) +
    geom_text(aes(label = paste0("n=", format(n, big.mark = ",")),
                  y = lower - 0.02 * diff(range(c(lower, upper)))),
              size = 2.5, family = "serif", color = "grey40") +
    scale_x_continuous(breaks = horizons) +
    labs(x = "Tournaments After LL Entry (t)",
         y = expression(hat(delta)(t) ~ " (log-odds)")) +
    theme_paper()

  p
}

p_dynamic <- make_dynamic_effects_plot(s2_dynamic, boot_dynamic)
if (!is.null(p_dynamic)) {
  ggsave(file.path(FIG_DIR, "fig_dynamic_effects.pdf"), p_dynamic,
         width = 8, height = 5, device = cairo_pdf)
  message("  Saved: Figures/fig_dynamic_effects.pdf")
}


# ---- Figure: Tournament Effects (4-panel) -----------------------------------

make_tournament_effects_plot <- function(te_results, het) {
  te <- te_results$tourney_effects
  rd <- te_results$round_effects

  p_a <- ggplot(te, aes(x = avg_match_me)) +
    geom_histogram(bins = 40, fill = col_treat, color = "white", alpha = 0.85) +
    geom_vline(xintercept = mean(te$avg_match_me), linetype = "dashed",
               color = "red3", linewidth = 0.7) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
    labs(x = expression(Delta * P(win)), y = "Frequency", tag = "(a)") +
    theme_paper(base_size = 11)

  p_b <- ggplot(te, aes(x = delta_ew)) +
    geom_histogram(bins = 40, fill = col_control, color = "white", alpha = 0.85) +
    geom_vline(xintercept = mean(te$delta_ew), linetype = "dashed",
               color = "red3", linewidth = 0.7) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
    labs(x = "Additional Expected Wins", y = "Frequency", tag = "(b)") +
    theme_paper(base_size = 11)

  round_avg <- rd |>
    group_by(round) |>
    summarise(mean_delta = mean(delta_reach),
              se_delta = sd(delta_reach) / sqrt(n()), .groups = "drop")

  p_c <- ggplot(round_avg, aes(x = round, y = mean_delta)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = mean_delta - 1.96 * se_delta,
                      ymax = mean_delta + 1.96 * se_delta),
                  width = 0.15, color = "darkgreen", linewidth = 0.6) +
    geom_point(color = "darkgreen", size = 2.5) +
    geom_line(color = "darkgreen", linewidth = 0.7) +
    scale_x_continuous(breaks = round_avg$round) +
    labs(x = "Round", y = expression(Delta * P(reach)), tag = "(c)") +
    theme_paper(base_size = 11)

  hz <- het$by_horizon
  p_d <- ggplot(hz, aes(x = t_after_event, y = mean_dEW)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(color = "purple4", size = 2.5) +
    geom_line(color = "purple4", linewidth = 0.7) +
    scale_x_continuous(breaks = hz$t_after_event) +
    labs(x = "Tournaments After LL Entry (t)",
         y = expression(Delta * E * "[W]"), tag = "(d)") +
    theme_paper(base_size = 11)

  (p_a | p_b) / (p_c | p_d)
}

p_tournament <- make_tournament_effects_plot(te_results, het)
ggsave(file.path(FIG_DIR, "fig_tournament_effects.pdf"), p_tournament,
       width = 10, height = 8, device = cairo_pdf)
message("  Saved: Figures/fig_tournament_effects.pdf")


# ---- Save full results object -----------------------------------------------

results_obj <- list(
  first_stage            = s1,
  pooled                 = s2_pooled,
  dynamic                = s2_dynamic,
  naive                  = s2_naive,
  robustness_first_ll    = s2_first,
  robustness_full_window = s2_full_window,
  boot_pooled            = boot_pooled,
  boot_dynamic           = boot_dynamic,
  tournament_effects     = te_results,
  heterogeneity          = het,
  event_table            = event_table,
  match_data             = match_df,
  params = list(
    T_horizon = T_HORIZON,
    calendar_cap = CALENDAR_CAP,
    truncation_mode = TRUNCATION_MODE,
    years = YEARS,
    n_boot = N_BOOT
  )
)

saveRDS(results_obj, file.path(CLEANED_DIR, "tournament_model_results.rds"))
message("  Saved: Data/cleaned/tournament_model_results.rds")


# ---- Summary markdown -------------------------------------------------------

write_summary <- function(results, filepath) {
  pooled_model <- results$pooled$model
  pooled_s <- summary(pooled_model)$coefficients

  delta <- pooled_s["ll_entry", "Estimate"]
  delta_se <- pooled_s["ll_entry", "Std. Error"]
  delta_p <- pooled_s["ll_entry", "Pr(>|z|)"]

  boot_se <- if (!is.null(results$boot_pooled) && "ll_entry" %in% colnames(results$boot_pooled)) {
    sd(results$boot_pooled[, "ll_entry"], na.rm = TRUE)
  } else {
    delta_se
  }

  te <- results$tournament_effects$tourney_effects

  rho <- if ("vhat" %in% rownames(pooled_s)) pooled_s["vhat", "Estimate"] else NA
  rho_p <- if ("vhat" %in% rownames(pooled_s)) pooled_s["vhat", "Pr(>|z|)"] else NA

  lines <- c(
    "# Tournament Performance Model: Results Summary",
    "",
    paste0("Generated: ", Sys.time()),
    paste0("Script: 12b_tournament_estimation.R (estimation-only, fast)"),
    "",
    "## Parameters",
    paste0("- Years: ", min(results$params$years), "-", max(results$params$years)),
    paste0("- T_horizon: ", results$params$T_horizon, " tournaments"),
    paste0("- Calendar cap: ", results$params$calendar_cap, " days"),
    paste0("- Truncation: ", results$params$truncation_mode),
    paste0("- Bootstrap replications: ", results$params$n_boot),
    "",
    "## Sample",
    paste0("- Events (qualifying losses): ", nrow(results$event_table)),
    paste0("  - Treated (LL entry): ", sum(results$event_table$ll_entry)),
    paste0("  - Control: ", sum(1 - results$event_table$ll_entry)),
    paste0("- Match-level observations: ", format(nrow(results$match_data), big.mark = ",")),
    "",
    "## Main Results (Pooled CF-IV)",
    paste0("- delta (ll_entry, log-odds): ", round(delta, 4)),
    paste0("- Analytic SE: ", round(delta_se, 4)),
    paste0("- Bootstrap SE: ", round(boot_se, 4)),
    paste0("- Odds ratio: ", round(exp(delta), 4)),
    paste0("- p-value (analytic): ", round(delta_p, 4)),
    "",
    "## Endogeneity Test",
    paste0("- rho (vhat): ", if (!is.na(rho)) round(rho, 4) else "N/A"),
    paste0("- rho p-value: ", if (!is.na(rho_p)) round(rho_p, 4) else "N/A"),
    paste0("- Interpretation: ",
           if (!is.na(rho_p) && rho_p < 0.05) "Evidence of endogeneity; CF correction needed"
           else "No strong evidence of endogeneity; naive and CF results may be similar"),
    "",
    "## Tournament-Level Treatment Effects",
    paste0("- Match-level ATE (avg marginal effect on P(win)): ",
           round(mean(te$avg_match_me), 4)),
    paste0("- Tournament-level ATE on expected wins (E[dW]): ",
           round(mean(te$delta_ew), 4)),
    paste0("- Tournament-level ATE on P(win tournament): ",
           round(mean(te$delta_tw), 6)),
    "",
    "## Interpretation",
    paste0("A delta of ", round(delta, 3), " in log-odds means:"),
    paste0("- At P(win)=0.50: shift to ~", round(plogis(qlogis(0.50) + delta), 3)),
    paste0("- At P(win)=0.30: shift to ~", round(plogis(qlogis(0.30) + delta), 3)),
    "- These effects compound across tournament rounds",
    "",
    "## Robustness",
    paste0("- First-LL only: delta = ",
           if (!is.null(results$robustness_first_ll))
             round(coef(results$robustness_first_ll$model)["ll_entry"], 4)
           else "N/A"),
    paste0("- Full window: skipped (requires data rebuild)"),
    paste0("- Naive logit: delta = ",
           round(coef(results$naive$model)["ll_entry"], 4)),
    "",
    "## Output Files",
    "- Data/cleaned/tournament_model_results.rds",
    "- Tables/table_tournament_first_stage.tex",
    "- Tables/table_tournament_match_effects.tex",
    "- Tables/table_tournament_effects.tex",
    "- Tables/table_tournament_heterogeneity.tex",
    "- Figures/fig_dynamic_effects.pdf",
    "- Figures/fig_tournament_effects.pdf"
  )

  writeLines(lines, filepath)
  message("  Saved: ", filepath)
}

write_summary(results_obj, file.path(OUTPUT_DIR, "tournament_model_summary.md"))


# ---- Done -------------------------------------------------------------------

elapsed_total <- as.numeric(difftime(Sys.time(), t_global_start, units = "mins"))

message("\n================================================================")
message("  Tournament performance estimation complete.")
message(sprintf("  Total runtime: %.1f minutes", elapsed_total))
message("  Key result: delta = ",
        round(coef(s2_pooled$model)["ll_entry"], 4),
        " (OR = ", round(exp(coef(s2_pooled$model)["ll_entry"]), 4), ")")
message("  See Output/tournament_model_summary.md for full details.")
message("================================================================")
