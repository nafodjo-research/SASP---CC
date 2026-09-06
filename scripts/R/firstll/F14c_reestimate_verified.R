# ==============================================================================
# F14c_reestimate_verified.R
# Re-estimate the headline stacked dynamic model on the F14-classified
# verified-lottery subsample and regenerate the verified-lottery table.
#
# METHOD
# ======
# The F14 classification identifies, for each treated GS LL entry, whether
# the assignment is provably lottery-based (rank_among_losers > 1) or
# ambiguous (rank_among_losers == 1, could be lottery or ranking). We
# define an event as verified-lottery if AT LEAST ONE treated observation
# at that event is F14-classified as lottery_verified. This is the natural
# extension of F13's stricter rule (which required ALL treated at the event
# to have rank_among_losers > 1). The looser rule captures events where
# lottery mechanism verifiably operated even when a co-treated LL happens
# to have rank_among_losers == 1 (which could happen under lottery too).
#
# Once verified events are identified, all pool members (treated and
# control) at those events enter the estimation sample.
#
# INPUTS
# ======
#   Data/cleaned/firstll/firstll_gs_est_v2.rds
#   Data/cleaned/firstll/ll_lottery_classification.rds  (from F14)
#
# OUTPUTS
# =======
#   Tables_FirstLL/table_verified_firstll.tex  (overwritten with new N)
#   Data/cleaned/firstll/verified_firstll_results_f14.rds
#   Output_FirstLL/F14c_reestimate_summary.md
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
# STEP 1: LOAD
# ==============================================================================
gs <- readRDS(file.path(FIRSTLL_CLEANED, "firstll_gs_est_v2.rds"))
gs <- ensure_scaled(gs)

classif <- readRDS(file.path(FIRSTLL_CLEANED, "ll_lottery_classification.rds"))

message("Loaded ", nrow(gs), " GS first-LL rows and ",
        nrow(classif), " classification rows.")

# Build the event key at (tour, tourney_id, year) level — the paper's
# identifying unit.
gs$event_key <- paste(gs$tour, gs$tourney_id, gs$year, sep = "|")
classif$event_key <- paste(classif$tour, classif$tourney_id, classif$year,
                           sep = "|")


# ==============================================================================
# STEP 2: BUILD VERIFIED EVENT SET
# ==============================================================================
# An event is verified-lottery if at least one treated observation at that
# event is F14-classified as lottery_verified.
verified_events <- unique(
  classif$event_key[classif$classification == "lottery_verified"]
)

slog(sprintf("F14 verified event set: %d events", length(verified_events)))
for (t in c("ATP", "WTA")) {
  n_t_events <- length(unique(
    classif$event_key[classif$classification == "lottery_verified" &
                      classif$tour == t]
  ))
  slog(sprintf("  %s: %d events", t, n_t_events))
}

gs_ver <- gs[gs$event_key %in% verified_events, ]

slog("\nF14 verified sample sizes (all pool members at verified events):")
for (t in c("ATP", "WTA")) {
  sub <- gs_ver[gs_ver$tour == t, ]
  n_ev <- length(unique(sub$event_key))
  slog(sprintf("  %s: N=%d (T=%d, C=%d) across %d events",
               t, nrow(sub), sum(sub$got_ll == 1),
               sum(sub$got_ll == 0), n_ev))
}


# ==============================================================================
# STEP 3: ESTIMATE STACKED DYNAMIC MODEL
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 3: RE-ESTIMATE STACKED DYNAMIC MODEL")
message(strrep("=", 70))

run_verified <- function(data, tour_label) {
  sub <- data[data$tour == tour_label, ]
  if (nrow(sub) < 30) return(NULL)

  st <- stack_horizons_full(sub)
  results <- list()

  for (ob in c("points_change", "elo_change", "n_main_draws",
               "n_matches_250plus")) {
    sdata <- st[!is.na(st[[ob]]) & !is.na(st$pre_rank_pts_s) &
                !is.na(st$player_age), ]
    if (nrow(sdata) < 30) next

    fml <- as.formula(paste0(ob, " ~ got_ll:horizon + ", ZPRE_FIRSTLL,
                             " | slam_year + horizon"))
    mod <- tryCatch(feols(fml, data = sdata, cluster = ~player_id),
                    error = function(e) NULL)
    if (is.null(mod)) next

    ct <- as.data.frame(coeftable(mod)); ct$var <- rownames(ct)
    for (h in HORIZON_LABS) {
      tn <- paste0("got_ll:horizon", h)
      if (tn %in% ct$var) {
        r <- ct[ct$var == tn, ]
        results[[paste0(ob, "_", h)]] <- data.frame(
          tour = tour_label, outcome = ob, horizon = h,
          coef = r$Estimate, se = r[["Std. Error"]],
          pval = r[["Pr(>|t|)"]], stringsAsFactors = FALSE)
      }
    }
  }
  do.call(rbind, results)
}

ver_atp <- run_verified(gs_ver, "ATP")
ver_wta <- run_verified(gs_ver, "WTA")
ver_all <- rbind(ver_atp, ver_wta)

slog("\nF14 verified lottery estimates (player-clustered SEs):")
for (i in seq_len(nrow(ver_all))) {
  r <- ver_all[i, ]
  star <- ifelse(r$pval < 0.01, "***",
            ifelse(r$pval < 0.05, "**",
              ifelse(r$pval < 0.1, "*", "")))
  slog(sprintf("  %s %s %s: %.2f (%.2f) p=%.3f %s",
               r$tour, r$outcome, r$horizon, r$coef, r$se, r$pval, star))
}


# ==============================================================================
# STEP 4: OVERWRITE THE PAPER'S TABLE
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 4: REGENERATE table_verified_firstll.tex")
message(strrep("=", 70))

make_verified_table <- function(res, gs_v) {
  oos <- c("points_change", "elo_change", "n_main_draws", "n_matches_250plus")

  L <- c("\\begin{tabular}{l*{5}{c}}", "\\toprule",
         paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"),
         "\\midrule")

  for (t in c("ATP", "WTA")) {
    n_events <- length(unique(gs_v$event_key[gs_v$tour == t]))
    n_obs <- sum(gs_v$tour == t)
    n_t   <- sum(gs_v$tour == t & gs_v$got_ll == 1)
    n_c   <- sum(gs_v$tour == t & gs_v$got_ll == 0)

    L <- c(L, paste0("\\multicolumn{6}{l}{\\textit{", t,
                     "}: $N = ", n_obs, "$ (", n_t, " treated, ", n_c,
                     " control), ", n_events, " events} \\\\"))

    for (ob in oos) {
      sub <- res[res$tour == t & res$outcome == ob, ]
      if (nrow(sub) == 0) next
      d <- if (ob == "n_main_draws" || ob == "n_matches_250plus") 2 else 1

      cv <- sv <- character()
      for (h in HORIZON_LABS) {
        r <- sub[sub$horizon == h, ]
        if (nrow(r) == 0) { cv <- c(cv, ""); sv <- c(sv, "") }
        else {
          cv <- c(cv, paste0(fmt(r$coef, d), add_stars(r$pval)))
          sv <- c(sv, paste0("(", fmt(r$se, d), ")"))
        }
      }
      L <- c(L, paste0("  ", outcome_labels[ob], " & ",
                       paste(cv, collapse = " & "), " \\\\"))
      L <- c(L, paste0("  & ", paste(sv, collapse = " & "), " \\\\[0.3em]"))
    }
    L <- c(L, "\\midrule")
  }
  L <- c(L, "\\bottomrule", "\\end{tabular}")
  paste(L, collapse = "\n")
}

writeLines(make_verified_table(ver_all, gs_ver),
           file.path(FIRSTLL_TABLES, "table_verified_firstll.tex"))
message("  Saved: Tables_FirstLL/table_verified_firstll.tex")

saveRDS(ver_all, file.path(FIRSTLL_CLEANED,
                           "verified_firstll_results_f14.rds"))


# ==============================================================================
# STEP 5: SUMMARY
# ==============================================================================
slog("\n", strrep("=", 70))
slog("F14c SUMMARY — headline point estimates for prose")
slog(strrep("=", 70))

# Report ranking-point coefficients across horizons for both tours.
for (t in c("ATP", "WTA")) {
  slog(sprintf("\n%s ranking-point effects on the F14 verified subsample:", t))
  sub <- ver_all[ver_all$tour == t & ver_all$outcome == "points_change", ]
  for (h in HORIZON_LABS) {
    r <- sub[sub$horizon == h, ]
    if (nrow(r) > 0) {
      star <- ifelse(r$pval < 0.01, "***",
                ifelse(r$pval < 0.05, "**",
                  ifelse(r$pval < 0.1, "*", "")))
      slog(sprintf("  %s: %.1f (%.1f) p=%.3f %s",
                   h, r$coef, r$se, r$pval, star))
    }
  }
  slog(sprintf("\n%s Elo-change effects on the F14 verified subsample:", t))
  sub <- ver_all[ver_all$tour == t & ver_all$outcome == "elo_change", ]
  for (h in HORIZON_LABS) {
    r <- sub[sub$horizon == h, ]
    if (nrow(r) > 0) {
      star <- ifelse(r$pval < 0.01, "***",
                ifelse(r$pval < 0.05, "**",
                  ifelse(r$pval < 0.1, "*", "")))
      slog(sprintf("  %s: %.1f (%.1f) p=%.3f %s",
                   h, r$coef, r$se, r$pval, star))
    }
  }
}

writeLines(summary_log,
           file.path(FIRSTLL_OUTPUT, "F14c_reestimate_summary.md"))
message("\nSummary saved to Output_FirstLL/F14c_reestimate_summary.md")
message("DONE.")
