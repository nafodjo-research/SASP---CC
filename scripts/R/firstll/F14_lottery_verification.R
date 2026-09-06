# ==============================================================================
# F14_lottery_verification.R
# Classify each GS lucky-loser assignment as lottery-based or ranking-based.
#
# BACKGROUND
# ==========
# Grand Slam LL selection follows a two-track rule (Wimbledon 2005 onward):
#   (a) Withdrawal BEFORE qualifying completes -> lottery among the top four
#       ranked final-round qualifying losers.
#   (b) Withdrawal AFTER qualifying completes  -> the highest-ranked
#       final-round qualifying loser receives the slot deterministically.
#
# The paper's identification uses (a). Contamination from (b) biases
# ranking-point estimates upward. The paper's "verified lottery" subsample
# restricts to events where the assignment was provably (a). This script
# extends and audits that classification.
#
# METHOD
# ======
# The Sackmann feed does not record withdrawal timing, so we cannot classify
# every entry from data alone. But rank_among_losers gives a partial
# classification for free:
#
#   * rank_among_losers > 1: the LL was NOT the highest-ranked qualifying
#     loser. Under rule (b), the top-ranked loser would have been picked
#     instead. So a lucky loser with rank_among_losers > 1 is LOTTERY-VERIFIED.
#
#   * rank_among_losers == 1: could be either rule. AMBIGUOUS. External
#     verification (Wayback Machine, tournament press releases, Wikipedia
#     match reports) is required to date the withdrawal.
#
# The output CSV is a working file: rows classified as "lottery_verified"
# require no further work; rows classified as "ambiguous" are the target
# set for Stages B (web scrape) and C (manual curation).
#
# INPUTS
# ======
#   Data/cleaned/firstll/firstll_gs_est_v2.rds  (94 treated + 108 control)
#   Data/cleaned/firstll/verified_firstll_results.rds  (F13 output, if present)
#
# OUTPUTS
# =======
#   Data/cleaned/firstll/ll_lottery_classification.csv (working file)
#   Data/cleaned/firstll/ll_lottery_classification.rds (mirror for R)
#   Output_FirstLL/F14_verification_summary.md
#
# NEXT STEPS
# ==========
# F14b_wayback_verification.R (to be written): consumes rows where
#     classification == "ambiguous", queries the Wayback Machine and
#     tournament sites for withdrawal announcements, updates classification.
# F14c_manual_review.R: reads the CSV back after human curation and
#     regenerates the summary + a re-estimation of the headline model on
#     the verified subset.
#
# Dependencies: dplyr, here
# ==============================================================================

set.seed(20260416)

library(dplyr)
library(here)

source(here("scripts", "R", "firstll", "firstll_helpers.R"))
summary_log <- character()


# ==============================================================================
# STEP 1: LOAD
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 1: LOAD FIRST-LL GRAND SLAM DATA")
message(strrep("=", 70))

gs <- readRDS(file.path(FIRSTLL_CLEANED, "firstll_gs_est_v2.rds"))

# Load F13's verified-lottery classification if available
f13_path <- file.path(FIRSTLL_CLEANED, "verified_firstll_results.rds")
f13 <- if (file.exists(f13_path)) readRDS(f13_path) else NULL
slog("F13 verified-lottery results file: ",
     if (is.null(f13)) "not found (proceeding without F13 cross-check)"
     else "loaded")

message("  Loaded ", nrow(gs), " GS first-LL rows (",
        sum(gs$got_ll == 1), " treated, ",
        sum(gs$got_ll == 0), " control)")


# ==============================================================================
# STEP 2: DETERMINISTIC CLASSIFICATION FROM RANK_AMONG_LOSERS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 2: DETERMINISTIC CLASSIFICATION")
message(strrep("=", 70))

# We classify only the treated observations. Controls are not "assigned" via
# either rule — they are the eligible-but-not-selected pool for whoever WAS
# selected. Their inclusion in the identifying sample rides on the treated
# player's classification: an event where the treated LL is lottery-verified
# has all its controls in the verified-lottery subsample.

treated <- gs[gs$got_ll == 1, ]

treated$classification <- ifelse(
  treated$rank_among_losers > 1,
  "lottery_verified",  # provably lottery: ranking rule would have picked #1
  "ambiguous"          # could be either lottery or ranking
)

# Reason string for audit trail
treated$reason <- ifelse(
  treated$classification == "lottery_verified",
  sprintf("rank_among_losers = %d > 1; ranking rule would pick #1",
          treated$rank_among_losers),
  "rank_among_losers = 1; withdrawal-timing evidence required"
)

# Confidence: "high" for lottery-verified (deterministic from data),
# "unknown" for ambiguous (pending external verification)
treated$confidence <- ifelse(
  treated$classification == "lottery_verified",
  "high",
  "unknown"
)

# Fields to be populated by external verification
treated$source <- ""             # e.g., "wayback:usopen.org/2015-08-27"
treated$withdrawal_date <- NA_character_  # ISO date of withdrawal announcement
treated$withdrawn_player <- NA_character_ # who the LL replaced (optional)
treated$verifier_notes <- ""     # human-readable notes

slog("Deterministic classification:")
tab <- table(treated$classification)
for (nm in names(tab)) slog(sprintf("  %-20s %d", nm, tab[nm]))

# Break down by tour
slog("\nBy tour:")
for (t in c("ATP", "WTA")) {
  sub <- treated[treated$tour == t, ]
  tab <- table(sub$classification)
  slog(sprintf("  %s: lottery_verified=%d, ambiguous=%d, total=%d",
               t, sum(tab["lottery_verified"], na.rm = TRUE),
               sum(tab["ambiguous"], na.rm = TRUE), nrow(sub)))
}


# ==============================================================================
# STEP 3: CROSS-CHECK AGAINST F13 (if available)
# ==============================================================================
if (!is.null(f13) && is.data.frame(f13$verified_events)) {
  message("\n", strrep("=", 70))
  message("STEP 3: CROSS-CHECK vs F13 verified_firstll_results")
  message(strrep("=", 70))

  # F13 verified_events lists the (tourney_id, year, tour) combinations
  # judged verified-lottery by F13. Reconcile with our classification.
  f13_key <- paste(f13$verified_events$tourney_id,
                   f13$verified_events$year,
                   f13$verified_events$tour, sep = "_")
  treated$f13_key <- paste(treated$tourney_id, treated$year,
                           treated$tour, sep = "_")
  treated$f13_flag <- treated$f13_key %in% f13_key

  # A row we classify "lottery_verified" that F13 does not flag as
  # verified is a discrepancy worth logging.
  disc <- treated[treated$classification == "lottery_verified" & !treated$f13_flag, ]
  slog(sprintf("\nF13 flags %d event-tour keys as verified-lottery.",
               length(unique(f13_key))))
  slog(sprintf("Rows we classify lottery_verified but F13 does not flag: %d",
               nrow(disc)))
  slog(sprintf("Rows F13 flags but we don't (rank_among_losers == 1): %d",
               sum(treated$f13_flag & treated$classification == "ambiguous")))

  treated$f13_key <- NULL
}


# ==============================================================================
# STEP 4: WRITE WORKING FILE
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 4: WRITE CSV + RDS")
message(strrep("=", 70))

# Keep the columns most useful for downstream curation. Preserve player_id
# so we can rejoin cleanly; carry player_name so a curator can eyeball rows.
out_cols <- c(
  "tour", "year", "slam_name", "tourney_id", "tourney_date", "event_date",
  "player_id", "player_name", "player_rank",
  "rank_among_losers", "n_ll_slots", "n_losers_at_event",
  "classification", "confidence", "source",
  "withdrawal_date", "withdrawn_player", "verifier_notes", "reason"
)
if ("f13_flag" %in% names(treated)) out_cols <- c(out_cols, "f13_flag")

out <- treated[, intersect(out_cols, names(treated))]

# Order: worst first so a curator sees ambiguous rows at the top
out <- out[order(out$classification == "lottery_verified",
                 out$tour, out$year, out$slam_name), ]

csv_path <- file.path(FIRSTLL_CLEANED, "ll_lottery_classification.csv")
rds_path <- file.path(FIRSTLL_CLEANED, "ll_lottery_classification.rds")
write.csv(out, csv_path, row.names = FALSE, na = "")
saveRDS(out, rds_path)

message("  Wrote ", nrow(out), " rows to:")
message("    ", csv_path)
message("    ", rds_path)


# ==============================================================================
# STEP 5: SUMMARY REPORT
# ==============================================================================
slog("\n", strrep("=", 70))
slog("F14 CLASSIFICATION SUMMARY")
slog(strrep("=", 70))

n_lot <- sum(out$classification == "lottery_verified")
n_amb <- sum(out$classification == "ambiguous")
slog(sprintf("Total treated GS LL entries: %d", nrow(out)))
slog(sprintf("  Lottery-verified (auto):   %d (%.1f%%)",
             n_lot, 100 * n_lot / nrow(out)))
slog(sprintf("  Ambiguous (needs review):  %d (%.1f%%)",
             n_amb, 100 * n_amb / nrow(out)))

# Sample-size implications for a headline re-estimation on the verified set
slog("\nSample sizes after restricting to lottery-verified events:")
slog("  (Both treated and their event-mates enter the identifying sample.)")

verified_events <- unique(paste(out$tour[out$classification == "lottery_verified"],
                                out$tourney_id[out$classification == "lottery_verified"],
                                out$year[out$classification == "lottery_verified"],
                                sep = "|"))
gs$event_key <- paste(gs$tour, gs$tourney_id, gs$year, sep = "|")
verified_full <- gs[gs$event_key %in% verified_events, ]
gs$event_key <- NULL

for (t in c("ATP", "WTA")) {
  sub <- verified_full[verified_full$tour == t, ]
  slog(sprintf("  %s: N=%d (T=%d, C=%d) across %d events",
               t, nrow(sub), sum(sub$got_ll == 1),
               sum(sub$got_ll == 0),
               length(unique(sub$tourney_id))))
}

slog("\nAmbiguous entries by tour (curation targets for F14b):")
for (t in c("ATP", "WTA")) {
  sub <- out[out$tour == t & out$classification == "ambiguous", ]
  slog(sprintf("  %s: %d entries needing withdrawal-timing verification",
               t, nrow(sub)))
}

slog("\nNext steps for the ambiguous set:")
slog("  1. F14b_wayback_verification.R (to be written): scripted Wayback")
slog("     Machine + tournament press-release queries for each ambiguous row.")
slog("  2. Manual curation for entries F14b cannot resolve.")
slog("  3. F14c_manual_review.R: reads the CSV back after curation and")
slog("     re-estimates the headline stacked model on the verified subsample.")

writeLines(summary_log,
           file.path(FIRSTLL_OUTPUT, "F14_verification_summary.md"))
message("\nSummary saved to Output_FirstLL/F14_verification_summary.md")
message("DONE.")
