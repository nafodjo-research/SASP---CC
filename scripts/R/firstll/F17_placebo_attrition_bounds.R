# ==============================================================================
# F17_placebo_attrition_bounds.R
# Pre-treatment placebo tests, attrition diagnostics, and Lee (2009) bounds.
#
# Three R4 methods-referee requests, all addressing the same underlying
# question: is the estimated treatment effect an artifact of selection rather
# than a causal response?
#
#   PART A. PRE-TREATMENT PLACEBO. The event-study figures show visually
#           parallel pre-trends, but no formal test was reported. Here we
#           build outcomes at h in {-12, -8, -4} weeks (the same range-join
#           construction used for post-treatment outcomes in
#           03_merge_rankings.R, with a negative offset) and regress them on
#           eventual LL status using the headline specification. Under random
#           assignment these coefficients should be indistinguishable from
#           zero. This is the strongest single defence against the residual
#           contamination concern, because ranking-rule assignment selects on
#           pre-treatment rank and would therefore show up here.
#
#   PART B. ATTRITION. Observation rates fall with the horizon as players drop
#           out of the rankings. If treated players survive at a higher rate
#           (because LL entry generates matches that keep them ranked), then
#           outcomes at long horizons are conditioned on a treatment-affected
#           selection event.
#
#   PART C. LEE (2009) BOUNDS. Where attrition is differential, we trim the
#           over-represented group from each tail of its outcome distribution
#           to restore equal observation rates, giving sharp bounds on the
#           treatment effect for the always-observed subpopulation.
#
# Inputs:
#   Data/cleaned/firstll/firstll_gs_est_v2.rds
#   Data/cleaned/firstll/ll_lottery_classification.rds
#   Data/raw/atp_rankings.rds, wta_rankings.rds
#
# Outputs:
#   Tables_FirstLL/table_placebo_pretreatment.tex
#   Tables_FirstLL/table_attrition.tex
#   Tables_FirstLL/table_lee_bounds.tex
#   Data/cleaned/firstll/placebo_attrition_bounds.rds
#   Output_FirstLL/F17_placebo_attrition_summary.md
#
# Dependencies: dplyr, fixest, here
# ==============================================================================

set.seed(20260416)

library(dplyr)
library(fixest)
library(here)

source(here("scripts", "R", "firstll", "firstll_helpers.R"))
summary_log <- character()

PRE_HORIZONS <- c(-12L, -8L, -4L)


# ==============================================================================
# LOAD
# ==============================================================================
gs <- ensure_scaled(readRDS(file.path(FIRSTLL_CLEANED, "firstll_gs_est_v2.rds")))
classif <- readRDS(file.path(FIRSTLL_CLEANED, "ll_lottery_classification.rds"))

gs$event_key      <- paste(gs$tour, gs$tourney_id, gs$year, sep = "|")
classif$event_key <- paste(classif$tour, classif$tourney_id, classif$year, sep = "|")
verified_events   <- unique(classif$event_key[classif$classification == "lottery_verified"])

atp_rank <- readRDS(file.path(RAW_DIR, "atp_rankings.rds"))
wta_rank <- readRDS(file.path(RAW_DIR, "wta_rankings.rds"))

to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  as.Date(as.character(x), format = "%Y%m%d")
}
atp_rank$rank_date <- to_date(atp_rank$ranking_date)
wta_rank$rank_date <- to_date(wta_rank$ranking_date)

message("Loaded rankings: ATP ", nrow(atp_rank), " rows; WTA ", nrow(wta_rank), " rows")


# ==============================================================================
# PART A: PRE-TREATMENT PLACEBO
# ==============================================================================
message("\n", strrep("=", 70))
message("PART A: PRE-TREATMENT PLACEBO OUTCOMES")
message(strrep("=", 70))

#' Attach ranking points at event_date + offset_weeks*7 (offset may be negative).
#' Mirrors the range-join in 03_merge_rankings.R: nearest ranking within 10 days.
attach_points_at <- function(dat, rank_df, offset_weeks, colname) {
  tgt <- dat |>
    filter(!is.na(event_date)) |>
    transmute(player_id, tourney_id,
              target_date = as.Date(event_date) + offset_weeks * 7)

  rs <- rank_df |>
    transmute(player_id = player, rank_date, points)

  matched <- tgt |>
    inner_join(rs, by = "player_id", relationship = "many-to-many") |>
    filter(abs(as.numeric(rank_date - target_date)) <= 10) |>
    mutate(dd = abs(as.numeric(rank_date - target_date))) |>
    group_by(player_id, tourney_id) |>
    slice_min(dd, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(player_id, tourney_id, !!colname := points)

  left_join(dat, matched, by = c("player_id", "tourney_id"))
}

gs_pre <- gs
for (h in PRE_HORIZONS) {
  cn <- paste0("points_tm", abs(h))
  message("  Building ", cn, " ...")
  atp_part <- attach_points_at(gs_pre[gs_pre$tour == "ATP", ], atp_rank, h, cn)
  wta_part <- attach_points_at(gs_pre[gs_pre$tour == "WTA", ], wta_rank, h, cn)
  gs_pre <- bind_rows(atp_part, wta_part)
}

# Placebo outcome: change from t(-k) to t(-4), i.e. purely pre-treatment drift.
# Using the most recent pre-period as the anchor mirrors the post-treatment
# construction (change relative to the event-date stock).
for (h in PRE_HORIZONS) {
  cn <- paste0("points_tm", abs(h))
  gs_pre[[paste0("placebo_change_", abs(h), "w")]] <-
    gs_pre$points_t0 - gs_pre[[cn]]
}

slog("Pre-treatment placebo coverage:")
for (h in PRE_HORIZONS) {
  cn <- paste0("points_tm", abs(h))
  slog(sprintf("  t-%dw: %d of %d observations matched (%.1f%%)",
               abs(h), sum(!is.na(gs_pre[[cn]])), nrow(gs_pre),
               100 * mean(!is.na(gs_pre[[cn]]))))
}

run_placebo <- function(d, tour_label, sample_label) {
  sub <- d[d$tour == tour_label, ]
  out <- list()
  for (h in PRE_HORIZONS) {
    oc <- paste0("placebo_change_", abs(h), "w")
    sd2 <- sub[!is.na(sub[[oc]]) & !is.na(sub$pre_rank_pts_s) &
               !is.na(sub$player_age), ]
    if (nrow(sd2) < 25) next
    sd2$.y <- sd2[[oc]]
    fml <- as.formula(paste0(".y ~ got_ll + ", ZPRE_FIRSTLL, " | slam_year"))
    m <- tryCatch(feols(fml, data = sd2, cluster = ~player_id),
                  error = function(e) NULL)
    if (is.null(m)) next
    ct <- as.data.frame(coeftable(m))
    if (!("got_ll" %in% rownames(ct))) next
    out[[paste0(abs(h), "w")]] <- data.frame(
      sample = sample_label, tour = tour_label,
      horizon = paste0("-", abs(h), "w"),
      coef = ct["got_ll", "Estimate"], se = ct["got_ll", "Std. Error"],
      pval = ct["got_ll", "Pr(>|t|)"], n = nobs(m),
      stringsAsFactors = FALSE)
  }
  do.call(rbind, out)
}

placebo_rows <- list()
for (t in c("ATP", "WTA")) {
  placebo_rows[[paste0("pooled_", t)]] <- run_placebo(gs_pre, t, "Pooled")
  placebo_rows[[paste0("ver_", t)]] <-
    run_placebo(gs_pre[gs_pre$event_key %in% verified_events, ], t, "Verified")
}
placebo_df <- do.call(rbind, placebo_rows)

slog("\nPre-treatment placebo estimates (should be indistinguishable from zero):")
n_reject <- 0
for (i in seq_len(nrow(placebo_df))) {
  r <- placebo_df[i, ]
  star <- ifelse(r$pval < 0.05, "  <-- REJECTS AT 5%", "")
  if (r$pval < 0.05) n_reject <- n_reject + 1
  slog(sprintf("  %-9s %-4s %-5s: %8.2f (%7.2f) p=%.3f  N=%d%s",
               r$sample, r$tour, r$horizon, r$coef, r$se, r$pval, r$n, star))
}
slog(sprintf("\n  %d of %d placebo tests reject at 5%% (expected under the null: %.1f)",
             n_reject, nrow(placebo_df), 0.05 * nrow(placebo_df)))

L <- c("\\begin{tabular}{lll*{3}{c}}", "\\toprule",
       "Sample & Tour & & $-12$w & $-8$w & $-4$w \\\\", "\\midrule")
for (s in unique(placebo_df$sample)) {
  for (t in c("ATP", "WTA")) {
    sub <- placebo_df[placebo_df$sample == s & placebo_df$tour == t, ]
    if (nrow(sub) == 0) next
    cv <- sv <- character()
    for (h in c("-12w", "-8w", "-4w")) {
      r <- sub[sub$horizon == h, ]
      if (nrow(r) == 0) { cv <- c(cv, ""); sv <- c(sv, "") }
      else {
        cv <- c(cv, paste0(fmt(r$coef[1], 1), add_stars(r$pval[1])))
        sv <- c(sv, paste0("(", fmt(r$se[1], 1), ")"))
      }
    }
    L <- c(L, paste0(s, " & ", t, " & $\\hat{\\beta}$ & ",
                     paste(cv, collapse = " & "), " \\\\"))
    L <- c(L, paste0(" & & & ", paste(sv, collapse = " & "), " \\\\[0.3em]"))
  }
}
L <- c(L, "\\bottomrule", "\\end{tabular}")
writeLines(paste(L, collapse = "\n"),
           file.path(FIRSTLL_TABLES, "table_placebo_pretreatment.tex"))
message("  Wrote table_placebo_pretreatment.tex")


# ==============================================================================
# PART B: ATTRITION
# ==============================================================================
message("\n", strrep("=", 70))
message("PART B: ATTRITION DIAGNOSTICS")
message(strrep("=", 70))

attr_rows <- list()
for (t in c("ATP", "WTA")) {
  sub <- gs[gs$tour == t, ]
  for (h in HORIZON_LABS) {
    oc <- paste0("points_change_", h)
    tr <- sub[sub$got_ll == 1, ]; co <- sub[sub$got_ll == 0, ]
    rate_t <- mean(!is.na(tr[[oc]])); rate_c <- mean(!is.na(co[[oc]]))
    # Two-proportion test for the differential.
    tt <- tryCatch(
      prop.test(c(sum(!is.na(tr[[oc]])), sum(!is.na(co[[oc]]))),
                c(nrow(tr), nrow(co)))$p.value,
      error = function(e) NA_real_, warning = function(w) NA_real_)
    attr_rows[[paste(t, h)]] <- data.frame(
      tour = t, horizon = h,
      rate_treated = rate_t, rate_control = rate_c,
      diff = rate_t - rate_c, pval = tt,
      n_treated = nrow(tr), n_control = nrow(co),
      stringsAsFactors = FALSE)
  }
}
attr_df <- do.call(rbind, attr_rows)

slog("\nObservation rates by treatment status:")
for (i in seq_len(nrow(attr_df))) {
  r <- attr_df[i, ]
  flag <- if (!is.na(r$pval) && r$pval < 0.10) "  <-- DIFFERENTIAL" else ""
  slog(sprintf("  %s %-4s: treated %.1f%%, control %.1f%%, diff %+.1fpp (p=%.3f)%s",
               r$tour, r$horizon, 100 * r$rate_treated, 100 * r$rate_control,
               100 * r$diff, r$pval, flag))
}

L <- c("\\begin{tabular}{ll*{5}{c}}", "\\toprule",
       paste0("Tour & & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"),
       "\\midrule")
for (t in c("ATP", "WTA")) {
  sub <- attr_df[attr_df$tour == t, ]
  tv <- cv <- dv <- character()
  for (h in HORIZON_LABS) {
    r <- sub[sub$horizon == h, ]
    tv <- c(tv, fmt(100 * r$rate_treated[1], 1))
    cv <- c(cv, fmt(100 * r$rate_control[1], 1))
    dv <- c(dv, paste0(fmt(100 * r$diff[1], 1), add_stars(r$pval[1])))
  }
  L <- c(L,
    paste0(t, " & Treated (\\%) & ", paste(tv, collapse = " & "), " \\\\"),
    paste0(" & Control (\\%) & ", paste(cv, collapse = " & "), " \\\\"),
    paste0(" & Difference & ", paste(dv, collapse = " & "), " \\\\[0.3em]"))
}
L <- c(L, "\\bottomrule", "\\end{tabular}")
writeLines(paste(L, collapse = "\n"), file.path(FIRSTLL_TABLES, "table_attrition.tex"))
message("  Wrote table_attrition.tex")


# ==============================================================================
# PART C: LEE (2009) BOUNDS
# ==============================================================================
message("\n", strrep("=", 70))
message("PART C: LEE BOUNDS")
message(strrep("=", 70))

#' Sharp Lee (2009) bounds under monotone selection.
#' Trims the over-observed group from each tail so that both groups have equal
#' observation rates, then reports the resulting min/max mean differences.
lee_bounds <- function(y_t, y_c, rate_t, rate_c) {
  y_t <- y_t[!is.na(y_t)]; y_c <- y_c[!is.na(y_c)]
  if (length(y_t) < 5 || length(y_c) < 5) return(NULL)

  if (rate_t > rate_c) {
    # Treated over-observed: trim treated.
    p <- (rate_t - rate_c) / rate_t
    k <- floor(p * length(y_t))
    if (k < 1) return(list(lower = mean(y_t) - mean(y_c),
                           upper = mean(y_t) - mean(y_c), trim_frac = 0))
    ys <- sort(y_t)
    lower <- mean(ys[seq_len(length(ys) - k)]) - mean(y_c)          # drop top k
    upper <- mean(ys[(k + 1):length(ys)]) - mean(y_c)               # drop bottom k
  } else if (rate_c > rate_t) {
    p <- (rate_c - rate_t) / rate_c
    k <- floor(p * length(y_c))
    if (k < 1) return(list(lower = mean(y_t) - mean(y_c),
                           upper = mean(y_t) - mean(y_c), trim_frac = 0))
    ys <- sort(y_c)
    lower <- mean(y_t) - mean(ys[(k + 1):length(ys)])               # drop bottom k
    upper <- mean(y_t) - mean(ys[seq_len(length(ys) - k)])          # drop top k
  } else {
    d <- mean(y_t) - mean(y_c)
    return(list(lower = d, upper = d, trim_frac = 0))
  }
  list(lower = lower, upper = upper,
       trim_frac = abs(rate_t - rate_c) / max(rate_t, rate_c))
}

lee_rows <- list()
for (t in c("ATP", "WTA")) {
  sub <- gs[gs$tour == t, ]
  for (h in HORIZON_LABS) {
    oc <- paste0("points_change_", h)
    tr <- sub[sub$got_ll == 1, ]; co <- sub[sub$got_ll == 0, ]
    rate_t <- mean(!is.na(tr[[oc]])); rate_c <- mean(!is.na(co[[oc]]))
    lb <- lee_bounds(tr[[oc]], co[[oc]], rate_t, rate_c)
    if (is.null(lb)) next
    naive <- mean(tr[[oc]], na.rm = TRUE) - mean(co[[oc]], na.rm = TRUE)
    lee_rows[[paste(t, h)]] <- data.frame(
      tour = t, horizon = h, naive_diff = naive,
      lower = lb$lower, upper = lb$upper, trim_frac = lb$trim_frac,
      stringsAsFactors = FALSE)
  }
}
lee_df <- do.call(rbind, lee_rows)

slog("\nLee (2009) bounds on the raw mean difference:")
for (i in seq_len(nrow(lee_df))) {
  r <- lee_df[i, ]
  cross <- if (r$lower <= 0 && r$upper >= 0) "  <-- bounds include zero" else ""
  slog(sprintf("  %s %-4s: naive=%7.1f  bounds=[%7.1f, %7.1f]  trim=%.1f%%%s",
               r$tour, r$horizon, r$naive_diff, r$lower, r$upper,
               100 * r$trim_frac, cross))
}

L <- c("\\begin{tabular}{ll*{5}{c}}", "\\toprule",
       paste0("Tour & & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"),
       "\\midrule")
for (t in c("ATP", "WTA")) {
  sub <- lee_df[lee_df$tour == t, ]
  nv <- lv <- uv <- character()
  for (h in HORIZON_LABS) {
    r <- sub[sub$horizon == h, ]
    if (nrow(r) == 0) { nv <- c(nv, ""); lv <- c(lv, ""); uv <- c(uv, "") }
    else {
      nv <- c(nv, fmt(r$naive_diff[1], 1))
      lv <- c(lv, fmt(r$lower[1], 1))
      uv <- c(uv, fmt(r$upper[1], 1))
    }
  }
  L <- c(L,
    paste0(t, " & Unadjusted & ", paste(nv, collapse = " & "), " \\\\"),
    paste0(" & Lee lower & ", paste(lv, collapse = " & "), " \\\\"),
    paste0(" & Lee upper & ", paste(uv, collapse = " & "), " \\\\[0.3em]"))
}
L <- c(L, "\\bottomrule", "\\end{tabular}")
writeLines(paste(L, collapse = "\n"), file.path(FIRSTLL_TABLES, "table_lee_bounds.tex"))
message("  Wrote table_lee_bounds.tex")


# ==============================================================================
# SAVE
# ==============================================================================
saveRDS(list(placebo = placebo_df, attrition = attr_df, lee = lee_df),
        file.path(FIRSTLL_CLEANED, "placebo_attrition_bounds.rds"))
writeLines(summary_log, file.path(FIRSTLL_OUTPUT, "F17_placebo_attrition_summary.md"))
message("\nSummary saved to Output_FirstLL/F17_placebo_attrition_summary.md")
message("DONE.")
