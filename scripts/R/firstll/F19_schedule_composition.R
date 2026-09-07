# ==============================================================================
# F19_schedule_composition.R
# Does the WTA Elo gain reflect improved play or an easier schedule?
#
# THE QUESTION
# ------------
# Elo rises for WTA lucky losers at several horizons, and the effect is
# stronger on the provably randomized subsample. Two readings compete:
#
#   (a) ABILITY. Main-draw experience against strong opponents improves play,
#       and Elo picks that up.
#   (b) SCHEDULE COMPOSITION. Elo rewards wins against any opponent. If the
#       ranking gain routes treated players into lower-tier events with
#       weaker fields, they win more matches against weaker opponents and
#       Elo rises without any change in underlying ability.
#
# These have a testable implication that separates them. Under (b), treated
# players should face measurably WEAKER opponents after the focal event.
# Under (a), opponent strength should be similar or higher (since better
# ranking admits them to stronger fields) while win rates rise.
#
# This script computes, for every post-event match in the 26 weeks following
# the focal event, the opponent's pre-match Elo, and regresses the player's
# mean opponent Elo on treatment using the headline specification. It also
# reports the treatment effect on matches played and on win rate, so the
# three quantities can be read together.
#
# Inputs:
#   Data/cleaned/firstll/firstll_gs_est_v2.rds
#   Data/cleaned/firstll/ll_lottery_classification.rds
#   Data/cleaned/elo_history.rds, wta_elo_history.rds
#   Data/raw/{atp,wta}_main_matches.rds, {atp_qual_chall,wta_qual_itf}_matches.rds
#
# Outputs:
#   Tables_FirstLL/table_schedule_composition.tex
#   Data/cleaned/firstll/schedule_composition.rds
#   Output_FirstLL/F19_schedule_summary.md
#
# Dependencies: dplyr, data.table, fixest, here
# ==============================================================================

set.seed(20260416)

library(dplyr)
library(data.table)
library(fixest)
library(here)

source(here("scripts", "R", "firstll", "firstll_helpers.R"))
source(here("scripts", "R", "win_model_helpers.R"))
summary_log <- character()

WINDOW_DAYS <- 182L   # 26 weeks


# ==============================================================================
# LOAD
# ==============================================================================
gs <- ensure_scaled(readRDS(file.path(FIRSTLL_CLEANED, "firstll_gs_est_v2.rds")))
classif <- readRDS(file.path(FIRSTLL_CLEANED, "ll_lottery_classification.rds"))
gs$event_key      <- paste(gs$tour, gs$tourney_id, gs$year, sep = "|")
classif$event_key <- paste(classif$tour, classif$tourney_id, classif$year, sep = "|")
verified_events   <- unique(classif$event_key[classif$classification == "lottery_verified"])

atp_all <- bind_rows(
  readRDS(file.path(RAW_DIR, "atp_main_matches.rds")),
  readRDS(file.path(RAW_DIR, "atp_qual_chall_matches.rds")))
wta_all <- bind_rows(
  readRDS(file.path(RAW_DIR, "wta_main_matches.rds")),
  readRDS(file.path(RAW_DIR, "wta_qual_itf_matches.rds")))

safe_as_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (is.numeric(x)) return(as.Date(as.character(x), format = "%Y%m%d"))
  as.Date(x)
}
atp_all$mdate <- safe_as_date(atp_all$tourney_date)
wta_all$mdate <- safe_as_date(wta_all$tourney_date)

atp_elo <- as.data.table(readRDS(file.path(CLEANED_DIR, "elo_history.rds")))
wta_elo <- as.data.table(readRDS(file.path(CLEANED_DIR, "wta_elo_history.rds")))
setnames(atp_elo, c("player_id", "match_date", "elo"))
setnames(wta_elo, c("player_id", "match_date", "elo"))
setkey(atp_elo, player_id, match_date)
setkey(wta_elo, player_id, match_date)

message("Loaded. ATP matches: ", nrow(atp_all), " | WTA matches: ", nrow(wta_all))


# ==============================================================================
# BUILD POST-EVENT SCHEDULE MEASURES
# ==============================================================================
message("\n", strrep("=", 70))
message("BUILDING POST-EVENT OPPONENT MEASURES")
message(strrep("=", 70))

build_schedule <- function(ev, all_m, elo_dt, tour_label) {
  ev <- ev[ev$tour == tour_label, ]
  if (nrow(ev) == 0) return(NULL)
  am <- as.data.table(all_m[, c("winner_id", "loser_id", "mdate",
                                "tourney_level")])
  w_idx <- split(seq_len(nrow(am)), am$winner_id)
  l_idx <- split(seq_len(nrow(am)), am$loser_id)

  out <- vector("list", nrow(ev))
  for (i in seq_len(nrow(ev))) {
    if (i %% 40 == 0) message("    ", i, " / ", nrow(ev))
    pid <- ev$player_id[i]
    d0  <- as.Date(ev$event_date[i])
    d1  <- d0 + WINDOW_DAYS

    widx <- w_idx[[as.character(pid)]]
    lidx <- l_idx[[as.character(pid)]]
    if (!is.null(widx)) widx <- widx[am$mdate[widx] > d0 & am$mdate[widx] <= d1]
    if (!is.null(lidx)) lidx <- lidx[am$mdate[lidx] > d0 & am$mdate[lidx] <= d1]
    nw <- length(widx); nl <- length(lidx)
    if (nw + nl == 0) {
      out[[i]] <- data.frame(player_id = pid, tourney_id = ev$tourney_id[i],
                             n_matches = 0L, mean_opp_elo = NA_real_,
                             win_rate = NA_real_, stringsAsFactors = FALSE)
      next
    }
    all_i  <- c(widx, lidx)
    is_win <- c(rep(TRUE, nw), rep(FALSE, nl))
    mdt    <- am$mdate[all_i]
    opp    <- ifelse(is_win, am$loser_id[all_i], am$winner_id[all_i])
    oelo   <- get_pre_match_elo(opp, mdt, elo_dt)
    oelo[is.na(oelo)] <- 1500

    out[[i]] <- data.frame(
      player_id = pid, tourney_id = ev$tourney_id[i],
      n_matches = nw + nl,
      mean_opp_elo = mean(oelo, na.rm = TRUE),
      win_rate = mean(is_win),
      stringsAsFactors = FALSE)
  }
  bind_rows(out)
}

sched <- bind_rows(
  build_schedule(gs, atp_all, atp_elo, "ATP") |> mutate(tour = "ATP"),
  build_schedule(gs, wta_all, wta_elo, "WTA") |> mutate(tour = "WTA"))

gs_s <- left_join(gs, sched, by = c("player_id", "tourney_id", "tour"))
message("  Merged. Non-missing opponent Elo: ",
        sum(!is.na(gs_s$mean_opp_elo)), " of ", nrow(gs_s))


# ==============================================================================
# ESTIMATE
# ==============================================================================
message("\n", strrep("=", 70))
message("ESTIMATING SCHEDULE-COMPOSITION EFFECTS")
message(strrep("=", 70))

est_one <- function(d, outcome, label, tour_label) {
  sub <- d[d$tour == tour_label & !is.na(d[[outcome]]) &
           !is.na(d$pre_rank_pts_s) & !is.na(d$player_age), ]
  if (nrow(sub) < 25) return(NULL)
  sub$.y <- sub[[outcome]]
  m <- tryCatch(feols(as.formula(paste0(".y ~ got_ll + ", ZPRE_FIRSTLL,
                                        " | slam_year")),
                      data = sub, cluster = ~player_id),
                error = function(e) NULL)
  if (is.null(m)) return(NULL)
  ct <- as.data.frame(coeftable(m))
  if (!("got_ll" %in% rownames(ct))) return(NULL)
  data.frame(sample = label, tour = tour_label, outcome = outcome,
             coef = ct["got_ll", "Estimate"], se = ct["got_ll", "Std. Error"],
             pval = ct["got_ll", "Pr(>|t|)"], n = nobs(m),
             mean_control = mean(sub$.y[sub$got_ll == 0], na.rm = TRUE),
             stringsAsFactors = FALSE)
}

rows <- list()
for (t in c("ATP", "WTA")) {
  for (oc in c("mean_opp_elo", "n_matches", "win_rate")) {
    rows[[paste("pooled", t, oc)]] <- est_one(gs_s, oc, "Pooled", t)
    rows[[paste("ver", t, oc)]] <-
      est_one(gs_s[gs_s$event_key %in% verified_events, ], oc, "Verified", t)
  }
}
res <- do.call(rbind, rows); rownames(res) <- NULL

slog("Post-event schedule composition (26-week window):")
slog("  mean_opp_elo = average pre-match Elo of opponents faced")
slog("  Under the schedule-composition account, treated players should face")
slog("  WEAKER opponents (negative coefficient on mean_opp_elo).")
for (i in seq_len(nrow(res))) {
  r <- res[i, ]
  star <- ifelse(r$pval < 0.01, "***", ifelse(r$pval < 0.05, "**",
           ifelse(r$pval < 0.1, "*", "")))
  slog(sprintf("  %-9s %-4s %-14s: %8.3f (%7.3f) p=%.3f %-3s  [control mean %8.2f, N=%d]",
               r$sample, r$tour, r$outcome, r$coef, r$se, r$pval, star,
               r$mean_control, r$n))
}

slog("\nInterpretation:")
for (t in c("ATP", "WTA")) {
  oe <- res[res$sample == "Pooled" & res$tour == t & res$outcome == "mean_opp_elo", ]
  if (nrow(oe) == 0) next
  if (oe$pval[1] < 0.10 && oe$coef[1] < 0) {
    slog(sprintf("  %s: opponents are significantly WEAKER (%.1f Elo, p=%.3f).",
                 t, oe$coef[1], oe$pval[1]))
    slog("     Schedule composition can account for any Elo gain here.")
  } else if (oe$pval[1] < 0.10 && oe$coef[1] > 0) {
    slog(sprintf("  %s: opponents are significantly STRONGER (%+.1f Elo, p=%.3f).",
                 t, oe$coef[1], oe$pval[1]))
    slog("     Schedule composition runs against, not toward, an Elo gain.")
  } else {
    slog(sprintf("  %s: opponent strength is unchanged (%+.1f Elo, p=%.3f).",
                 t, oe$coef[1], oe$pval[1]))
    slog("     Schedule composition does not account for an Elo gain here.")
  }
}


# ==============================================================================
# TABLE
# ==============================================================================
oc_lab <- function(o) switch(o,
  "mean_opp_elo" = "Mean opponent Elo",
  "n_matches"    = "Matches played",
  "win_rate"     = "Win rate", o)

L <- c("\\begin{tabular}{ll*{3}{c}}", "\\toprule",
       "Sample & Tour & Mean opp.\\ Elo & Matches played & Win rate \\\\",
       "\\midrule")
for (s in c("Pooled", "Verified")) {
  for (t in c("ATP", "WTA")) {
    sub <- res[res$sample == s & res$tour == t, ]
    if (nrow(sub) == 0) next
    cv <- sv <- character()
    for (oc in c("mean_opp_elo", "n_matches", "win_rate")) {
      r <- sub[sub$outcome == oc, ]
      if (nrow(r) == 0) { cv <- c(cv, ""); sv <- c(sv, "") }
      else {
        dg <- if (oc == "win_rate") 3 else 2
        cv <- c(cv, paste0(fmt(r$coef[1], dg), add_stars(r$pval[1])))
        sv <- c(sv, paste0("(", fmt(r$se[1], dg), ")"))
      }
    }
    L <- c(L, paste0(s, " & ", t, " & ", paste(cv, collapse = " & "), " \\\\"))
    L <- c(L, paste0(" & & ", paste(sv, collapse = " & "), " \\\\[0.3em]"))
  }
}
L <- c(L, "\\bottomrule", "\\end{tabular}")
writeLines(paste(L, collapse = "\n"),
           file.path(FIRSTLL_TABLES, "table_schedule_composition.tex"))
message("  Wrote table_schedule_composition.tex")

saveRDS(res, file.path(FIRSTLL_CLEANED, "schedule_composition.rds"))
writeLines(summary_log, file.path(FIRSTLL_OUTPUT, "F19_schedule_summary.md"))
message("\nSummary saved to Output_FirstLL/F19_schedule_summary.md")
message("DONE.")
