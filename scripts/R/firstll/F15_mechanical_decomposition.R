# ==============================================================================
# F15_mechanical_decomposition.R  (REWRITTEN FOR R4)
# Saturated-interaction test of the first-break claim.
#
# WHAT CHANGED AND WHY
# --------------------
# The R3 version of this script compared the pooled coefficient to a
# treated-share-weighted average of stratum-specific coefficients:
#
#     beta^pool  vs  w_0*beta^(n=0) + w_1*beta^(n=1) + w_{>=2}*beta^(n>=2)
#
# and invoked the law of iterated expectations. The R4 methods referee
# correctly rejected that framing. Under a pooled regression with event fixed
# effects and covariate coefficients Gamma shared across strata, beta^pool is
# NOT the treated-share-weighted average of stratum ATTs. It is a
# variance-weighted average whose implicit weights depend on the within-stratum
# variance of D conditional on the covariates (Angrist 1998; Sloczynski 2022
# ReStat for the binary-treatment case; Goodman-Bacon 2021 for the staggered
# analogue). When strata differ in covariate composition and in residual
# variance, the two weighting schemes diverge in population, not just in
# finite samples. The ~75-point "gap" the old script reported was therefore a
# specification artifact, not a finding.
#
# THE REPLACEMENT
# ---------------
# Estimate ONE saturated model on the pooled sample:
#
#   Y_ieh = alpha_e + alpha_h + sum_h beta_h (D x 1[h])
#                             + sum_h gamma1_h (D x 1[h] x 1[n=1])
#                             + sum_h gamma2_h (D x 1[h] x 1[n>=2])
#                             + Gamma Z + eps
#
# Now beta_h IS the ATT for the first-time (n=0) stratum at horizon h,
# beta_h + gamma1_h is the ATT for the second-LL stratum, and
# beta_h + gamma2_h is the ATT for the third-plus stratum. Because they all
# come from a single regression on the pooled data, they integrate to the
# pooled estimand by construction and no decomposition identity is required.
#
# The first-break claim is then a hypothesis test, not an arithmetic exercise:
#   H0: gamma1_h = 0        (second LL has the same effect as the first)
#   H0: gamma2_h = 0        (third-plus has the same effect as the first)
#   H0: gamma1_h = gamma2_h = 0 for all h   (joint: no experience gradient)
#
# Inputs:
#   Data/cleaned/skeleton_gs_est_v5.rds
#   Data/cleaned/skeleton_nongs_est_v8.rds
#
# Outputs:
#   Tables_FirstLL/table_saturated_experience.tex
#   Data/cleaned/firstll/saturated_experience.rds
#   Output_FirstLL/F15_saturated_summary.md
#
# Dependencies: dplyr, fixest, here
# ==============================================================================

set.seed(20260416)

library(dplyr)
library(fixest)
library(here)

source(here("scripts", "R", "firstll", "firstll_helpers.R"))
summary_log <- character()


# ==============================================================================
# STEP 1: LOAD AND BUILD EXPERIENCE STRATA
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 1: LOAD DATA")
message(strrep("=", 70))

gs_full  <- ensure_scaled(readRDS(file.path(CLEANED_DIR, "skeleton_gs_est_v5.rds")))
ngs_full <- ensure_scaled(readRDS(file.path(CLEANED_DIR, "skeleton_nongs_est_v8.rds")))

gs_full$n_prior_ll  <- gs_full$n_prior_gs_ll_won + gs_full$n_prior_nongs_ll_won
ngs_full$n_prior_ll <- ngs_full$n_prior_gs_ll_won + ngs_full$n_prior_nongs_ll_won

for (nm in c("gs_full", "ngs_full")) {
  d <- get(nm)
  d$exp1  <- as.integer(d$n_prior_ll == 1)
  d$exp2p <- as.integer(d$n_prior_ll >= 2)
  assign(nm, d)
}

dose_full <- readRDS(file.path(CLEANED_DIR, "performance_dose.rds"))
gs_full  <- merge_dose(gs_full,  dose_full, "GS")
ngs_full <- merge_dose(ngs_full, dose_full, "nonGS")

message("  GS full: ", nrow(gs_full), " | NonGS full: ", nrow(ngs_full))
for (nm in c("GS", "NonGS")) {
  d <- if (nm == "GS") gs_full else ngs_full
  slog(sprintf("%s strata (treated counts): n=0: %d, n=1: %d, n>=2: %d",
               nm,
               sum(d$got_ll == 1 & d$n_prior_ll == 0),
               sum(d$got_ll == 1 & d$n_prior_ll == 1),
               sum(d$got_ll == 1 & d$n_prior_ll >= 2)))
}


# ==============================================================================
# STEP 2: SATURATED ESTIMATION
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 2: SATURATED INTERACTION MODEL")
message(strrep("=", 70))

run_saturated <- function(data, fe_str, cf_term, tour_label, design_label,
                          outcome) {
  sub <- data[data$tour == tour_label, ]
  st  <- stack_horizons_full(sub)
  st$.y <- st[[outcome]]
  keep <- !is.na(st$.y) & !is.na(st$pre_rank_pts_s) & !is.na(st$player_age)
  if (!is.null(cf_term)) keep <- keep & !is.na(st$v_hat)
  st <- st[keep, ]
  if (nrow(st) < 100) return(NULL)

  # SATURATION ON BOTH SIDES.
  #
  # The treatment side carries base horizon effects (the n=0 stratum) plus
  # experience shifters. That alone is NOT enough. Without stratum main
  # effects interacted with horizon, the model forces the UNTREATED outcome
  # path to have the same horizon profile for LL-naive candidates and for
  # chronic repeat candidates, up to a constant shift from the prior-LL counts
  # in ZPRE_FULL. That restriction is false in this setting: players who reach
  # n >= 2 are, by construction, players who have spent years losing in final
  # qualifying, and their untreated ranking trajectory differs from a
  # first-timer's. Any such difference has nowhere to go but gamma_2h, which
  # is exactly why an earlier version of this script produced a gamma_2h that
  # grew monotonically in the horizon (-63.9 at 4w to -182.3 at 52w) and
  # implied ATTs larger in absolute value than the physically feasible dose.
  # That is the signature of a differential control-group path, not of a
  # treatment shifter.
  #
  # Adding horizon:exp1 and horizon:exp2p lets each stratum have its own
  # untreated horizon profile, so gamma_jh is identified off the
  # treated-versus-control contrast WITHIN stratum rather than off differences
  # between strata in the untreated path.
  rhs <- paste0("got_ll:horizon + got_ll:horizon:exp1 + got_ll:horizon:exp2p",
                " + horizon:exp1 + horizon:exp2p")
  if (!is.null(cf_term)) rhs <- paste0(rhs, " + v_hat:horizon")
  rhs <- paste0(rhs, " + ", ZPRE_FULL)
  fml <- as.formula(paste0(".y ~ ", rhs, " | ", fe_str))

  mod <- tryCatch(feols(fml, data = st, cluster = ~player_id),
                  error = function(e) { message("    ", conditionMessage(e)); NULL })
  if (is.null(mod)) return(NULL)

  ct <- as.data.frame(coeftable(mod)); ct$var <- rownames(ct)
  cf_all <- coef(mod); V <- vcov(mod)

  rows <- list()
  for (h in HORIZON_LABS) {
    b_name  <- paste0("got_ll:horizon", h)
    g1_name <- paste0("got_ll:horizon", h, ":exp1")
    g2_name <- paste0("got_ll:horizon", h, ":exp2p")
    # fixest can order interaction terms differently; resolve by matching.
    if (!(b_name %in% names(cf_all))) {
      alt <- grep(paste0("horizon", h, "$"), names(cf_all), value = TRUE)
      alt <- alt[grepl("got_ll", alt) & !grepl("exp", alt)]
      if (length(alt) == 1) b_name <- alt else next
    }
    g1_name <- grep(paste0("horizon", h, ".*exp1|exp1.*horizon", h),
                    names(cf_all), value = TRUE)
    g1_name <- g1_name[grepl("got_ll", g1_name)]
    g2_name <- grep(paste0("horizon", h, ".*exp2p|exp2p.*horizon", h),
                    names(cf_all), value = TRUE)
    g2_name <- g2_name[grepl("got_ll", g2_name)]

    b  <- unname(cf_all[b_name])
    se_b <- sqrt(V[b_name, b_name])
    p_b  <- 2 * pnorm(-abs(b / se_b))

    g1 <- if (length(g1_name) == 1) unname(cf_all[g1_name]) else NA_real_
    g2 <- if (length(g2_name) == 1) unname(cf_all[g2_name]) else NA_real_
    se_g1 <- if (length(g1_name) == 1) sqrt(V[g1_name, g1_name]) else NA_real_
    se_g2 <- if (length(g2_name) == 1) sqrt(V[g2_name, g2_name]) else NA_real_
    p_g1  <- if (!is.na(g1)) 2 * pnorm(-abs(g1 / se_g1)) else NA_real_
    p_g2  <- if (!is.na(g2)) 2 * pnorm(-abs(g2 / se_g2)) else NA_real_

    # Implied stratum ATTs with delta-method SEs.
    att1 <- if (!is.na(g1)) b + g1 else NA_real_
    att2 <- if (!is.na(g2)) b + g2 else NA_real_
    se_att1 <- if (!is.na(g1))
      sqrt(V[b_name, b_name] + V[g1_name, g1_name] + 2 * V[b_name, g1_name])
      else NA_real_
    se_att2 <- if (!is.na(g2))
      sqrt(V[b_name, b_name] + V[g2_name, g2_name] + 2 * V[b_name, g2_name])
      else NA_real_

    rows[[h]] <- data.frame(
      design = design_label, tour = tour_label, outcome = outcome, horizon = h,
      beta_n0 = b, se_n0 = se_b, p_n0 = p_b,
      gamma1 = g1, se_gamma1 = se_g1, p_gamma1 = p_g1,
      gamma2 = g2, se_gamma2 = se_g2, p_gamma2 = p_g2,
      att_n1 = att1, se_att_n1 = se_att1,
      att_n2p = att2, se_att_n2p = se_att2,
      ratio_n0_n1 = if (!is.na(att1) && abs(att1) > 1e-8) b / att1 else NA_real_,
      stringsAsFactors = FALSE)
  }
  res <- do.call(rbind, rows)

  # Joint test: are ALL experience shifters zero?
  int_vars <- grep("got_ll.*exp", names(cf_all), value = TRUE)
  wald_stat <- NA_real_; wald_p <- NA_real_
  if (length(int_vars) > 0) {
    wt <- tryCatch(fixest::wald(mod, keep = "got_ll.*exp", print = FALSE),
                   error = function(e) NULL)
    if (!is.null(wt)) { wald_stat <- wt$stat; wald_p <- wt$p }
  }
  attr(res, "wald") <- c(stat = wald_stat, p = wald_p, df = length(int_vars))
  res
}

all_rows <- list(); wald_rows <- list()
for (oc in c("points_change", "n_main_draws")) {
  for (t in c("ATP", "WTA")) {
    r <- run_saturated(gs_full, "slam_year + horizon", NULL, t, "GS", oc)
    if (!is.null(r)) {
      all_rows[[paste("GS", t, oc)]] <- r
      w <- attr(r, "wald")
      wald_rows[[paste("GS", t, oc)]] <- data.frame(
        design = "GS", tour = t, outcome = oc,
        wald_stat = w["stat"], wald_p = w["p"], wald_df = w["df"],
        stringsAsFactors = FALSE)
    }
    r <- run_saturated(ngs_full, "tourney_id + horizon", "v_hat", t, "NonGS", oc)
    if (!is.null(r)) {
      all_rows[[paste("NonGS", t, oc)]] <- r
      w <- attr(r, "wald")
      wald_rows[[paste("NonGS", t, oc)]] <- data.frame(
        design = "NonGS", tour = t, outcome = oc,
        wald_stat = w["stat"], wald_p = w["p"], wald_df = w["df"],
        stringsAsFactors = FALSE)
    }
  }
}
sat <- do.call(rbind, all_rows); rownames(sat) <- NULL
wald_df <- do.call(rbind, wald_rows); rownames(wald_df) <- NULL


# ==============================================================================
# STEP 3: REPORT
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 3: RESULTS")
message(strrep("=", 70))

slog("\nSaturated model: first-LL effect and experience shifters")
slog("  beta_n0   = ATT for first-time (n=0) recipients")
slog("  gamma1    = shift for second LL (n=1); H0: gamma1 = 0")
slog("  gamma2    = shift for third-plus (n>=2); H0: gamma2 = 0")
sub <- sat[sat$outcome == "points_change", ]
for (i in seq_len(nrow(sub))) {
  r <- sub[i, ]
  slog(sprintf("  %-6s %-4s %-4s: b_n0=%7.1f (p=%.3f) | g1=%8.1f (p=%.3f) | g2=%8.1f (p=%.3f)",
               r$design, r$tour, r$horizon, r$beta_n0, r$p_n0,
               r$gamma1, r$p_gamma1, r$gamma2, r$p_gamma2))
}

slog("\nJoint Wald tests (H0: all experience shifters = 0):")
for (i in seq_len(nrow(wald_df))) {
  r <- wald_df[i, ]
  verdict <- if (!is.na(r$wald_p) && r$wald_p < 0.05)
    "REJECT: effects differ by LL experience" else "fail to reject"
  slog(sprintf("  %-6s %-4s %-18s: F=%6.2f, df=%d, p=%.4f -> %s",
               r$design, r$tour, r$outcome, r$wald_stat, r$wald_df,
               r$wald_p, verdict))
}


# ==============================================================================
# STEP 4: TABLE
# ==============================================================================
make_sat_table <- function(d, focal_outcome = "points_change") {
  dd <- d[d$outcome == focal_outcome, ]
  L <- c("\\begin{tabular}{l*{5}{c}}", "\\toprule",
         paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"),
         "\\midrule")
  first <- TRUE
  for (des in c("GS", "NonGS")) {
    for (t in c("ATP", "WTA")) {
      sub <- dd[dd$design == des & dd$tour == t, ]
      if (nrow(sub) == 0) next
      sub <- sub[match(HORIZON_LABS, sub$horizon), ]
      if (!first) L <- c(L, "\\\\[0.3em]")
      first <- FALSE
      L <- c(L, paste0("\\multicolumn{6}{l}{\\textit{Panel: ", des, "-", t,
                       "}} \\\\", " \\midrule"))
      f1 <- function(x, d2 = 1) vapply(x, function(z)
        if (is.na(z)) "" else fmt(z, d2), character(1))
      st <- function(v, p) vapply(seq_along(v), function(i)
        if (is.na(v[i])) "" else paste0(fmt(v[i], 1), add_stars(p[i])),
        character(1))
      L <- c(L,
        paste0("$\\hat{\\beta}_h$ (first LL, $n=0$) & ",
               paste(st(sub$beta_n0, sub$p_n0), collapse = " & "), " \\\\"),
        paste0(" & (", paste(f1(sub$se_n0), collapse = ") & ("), ") \\\\"),
        paste0("$\\hat{\\gamma}_{1h}$ (shift, $n=1$) & ",
               paste(st(sub$gamma1, sub$p_gamma1), collapse = " & "), " \\\\"),
        paste0(" & (", paste(f1(sub$se_gamma1), collapse = ") & ("), ") \\\\"),
        paste0("$\\hat{\\gamma}_{2h}$ (shift, $n\\geq 2$) & ",
               paste(st(sub$gamma2, sub$p_gamma2), collapse = " & "), " \\\\"),
        paste0(" & (", paste(f1(sub$se_gamma2), collapse = ") & ("), ") \\\\"),
        paste0("Implied ATT, $n=1$ & ",
               paste(f1(sub$att_n1), collapse = " & "), " \\\\"),
        paste0("Implied ATT, $n\\geq 2$ & ",
               paste(f1(sub$att_n2p), collapse = " & "), " \\\\"))
    }
  }
  L <- c(L, "\\bottomrule", "\\end{tabular}")
  paste(L, collapse = "\n")
}

writeLines(make_sat_table(sat),
           file.path(FIRSTLL_TABLES, "table_saturated_experience.tex"))
message("  Wrote table_saturated_experience.tex")

saveRDS(list(saturated = sat, wald = wald_df),
        file.path(FIRSTLL_CLEANED, "saturated_experience.rds"))
writeLines(summary_log, file.path(FIRSTLL_OUTPUT, "F15_saturated_summary.md"))
message("\nSummary saved to Output_FirstLL/F15_saturated_summary.md")
message("DONE.")
