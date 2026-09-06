# ==============================================================================
# F13_verified_lottery.R
# Build a first-LL verified lottery subsample table (strategist-critic fix)
#
# "Verified lottery" = events where the selected LL was provably NOT the
# highest-ranked qualifying loser — confirming lottery-based assignment.
# For each such event, check who the highest-ranked qualifying loser was;
# if it wasn't the LL recipient, the lottery mechanism is confirmed.
#
# First-LL verified = intersection of (verified lottery events) and
# (had_prior_ll == 0 players in the top-4 pool).
#
# Inputs:
#   Data/cleaned/firstll/firstll_gs_est_v2.rds
#
# Outputs:
#   Tables_FirstLL/table_verified_firstll.tex
#   Output_FirstLL/F13_verified_summary.md
# ==============================================================================

set.seed(20260416)

library(dplyr)
library(fixest)
library(here)

source(here("scripts", "R", "firstll", "firstll_helpers.R"))
summary_log <- character()

gs <- readRDS(file.path(FIRSTLL_CLEANED, "firstll_gs_est_v2.rds"))
gs <- ensure_scaled(gs)

# Identify verified lottery events:
# An event is verified-lottery if the LL recipient is NOT the highest-ranked
# qualifying loser (rank_among_losers > 1 among treated)
verified_events <- gs |>
  filter(got_ll == 1) |>
  group_by(tourney_id) |>
  summarise(min_rank_treated = min(rank_among_losers, na.rm = TRUE),
            .groups = "drop") |>
  filter(min_rank_treated > 1) |>
  pull(tourney_id)

slog("Verified lottery events (first-LL): ", length(verified_events))

gs_verified <- gs[gs$tourney_id %in% verified_events, ]

slog("First-LL verified lottery sample sizes:")
for (t in c("ATP", "WTA")) {
  sub <- gs_verified[gs_verified$tour == t, ]
  n_events <- length(unique(sub$tourney_id))
  slog(sprintf("  %s: N=%d (T=%d, C=%d) across %d events",
               t, nrow(sub), sum(sub$got_ll == 1), sum(sub$got_ll == 0), n_events))
}


# ==============================================================================
# ESTIMATE STACKED DYNAMIC MODEL ON FIRST-LL VERIFIED SUBSAMPLE
# ==============================================================================

run_verified <- function(data, tour_label) {
  sub <- data[data$tour == tour_label, ]
  if (nrow(sub) < 30) return(NULL)

  st <- stack_horizons_full(sub)
  results <- list()

  for (ob in c("points_change", "elo_change", "n_main_draws", "n_matches_250plus")) {
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

ver_atp <- run_verified(gs_verified, "ATP")
ver_wta <- run_verified(gs_verified, "WTA")
ver_all <- rbind(ver_atp, ver_wta)

slog("\nFirst-LL verified lottery results:")
for (i in seq_len(nrow(ver_all))) {
  r <- ver_all[i, ]
  star <- ifelse(r$pval < 0.01, "***",
            ifelse(r$pval < 0.05, "**",
              ifelse(r$pval < 0.1, "*", "")))
  slog(sprintf("  %s %s %s: %.2f (%.2f) p=%.3f %s",
               r$tour, r$outcome, r$horizon, r$coef, r$se, r$pval, star))
}


# ==============================================================================
# GENERATE TABLE
# ==============================================================================

make_verified_table <- function(res) {
  oos <- c("points_change", "elo_change", "n_main_draws", "n_matches_250plus")

  L <- c("\\begin{tabular}{l*{5}{c}}", "\\toprule",
         paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"), "\\midrule")

  for (t in c("ATP", "WTA")) {
    n_events <- length(unique(gs_verified$tourney_id[gs_verified$tour == t]))
    n_obs <- sum(gs_verified$tour == t)
    n_t <- sum(gs_verified$tour == t & gs_verified$got_ll == 1)
    n_c <- sum(gs_verified$tour == t & gs_verified$got_ll == 0)

    L <- c(L, paste0("\\multicolumn{6}{l}{\\textit{", t, "}: $N = ",
                     n_obs, "$ (", n_t, " treated, ", n_c, " control), ",
                     n_events, " events} \\\\"))

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
      L <- c(L, paste0("  ", outcome_labels[ob], " & ", paste(cv, collapse = " & "), " \\\\"))
      L <- c(L, paste0("  & ", paste(sv, collapse = " & "), " \\\\[0.3em]"))
    }
    L <- c(L, "\\midrule")
  }
  L <- c(L, "\\bottomrule", "\\end{tabular}")
  paste(L, collapse = "\n")
}

writeLines(make_verified_table(ver_all),
           file.path(FIRSTLL_TABLES, "table_verified_firstll.tex"))
message("  Table saved: Tables_FirstLL/table_verified_firstll.tex")

saveRDS(ver_all, file.path(FIRSTLL_CLEANED, "verified_firstll_results.rds"))
writeLines(summary_log, file.path(FIRSTLL_OUTPUT, "F13_verified_summary.md"))
message("DONE.")
