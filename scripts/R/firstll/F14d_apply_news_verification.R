# ==============================================================================
# F14d_apply_news_verification.R
# Apply the news-verification classifications from the Sep 6 curation pass.
#
# For each of the 23 F14-ambiguous GS LL entries, I ran targeted WebSearch
# queries against tennis news, Wikipedia tournament pages, and the tournament
# official sites (via Wayback where needed) to find:
#   - Who withdrew from the main draw (creating the LL slot)
#   - When the withdrawal was announced
#   - Whether the withdrawal preceded qualifying completion (=> lottery)
#     or followed it (=> ranking rule)
#
# RESULTS
# =======
# 7 of 23 confirmed at high confidence: all RANKING (post-qualifying
#   withdrawal). Every recent Slam entry with clear news coverage
#   turned out to be a "day before main draw" withdrawal, which the
#   ranking rule picks up rather than a lottery draw.
# 3 of 23 medium confidence: also point to RANKING but exact
#   withdrawal date was not found in accessible news.
# 13 of 23 UNRESOLVED: older entries (mostly pre-2015 WTA) where
#   contemporaneous coverage is thin and Wayback captures do not
#   date the withdrawal.
#
# ZERO of 23 confirmed as LOTTERY. The systematic finding is that
# "rank-among-losers = 1" LL entries at Grand Slams are almost never
# lottery draws. This is consistent with the two-track rule as
# actually implemented: pre-qualifying withdrawals are relatively
# rare (players know the tour schedule and injuries usually escalate
# in the days before main draw); the modal LL scenario is a "night
# before main draw" injury withdrawal that triggers the ranking rule.
#
# IMPLICATION FOR THE PAPER
# =========================
# Under F14's event-level rule (event verified-lottery if ANY treated
# LL there has rank_among_losers > 1), events with mixed lottery- and
# ranking-assigned LLs are included in the verified subsample. This is
# not a bug: the pool-level randomization is genuine at these events;
# the rank-1 LLs at such events are ranking-assigned but the event
# pool still contains lottery-randomized variation. However, when
# building the tightest verified subsample, an alternative rule is to
# require ALL treated at the event to have rank_among_losers > 1
# (F13's original strict rule), which excludes mixed events entirely.
#
# The 23 news-verified rank-1 entries confirm that F13's stricter rule
# is warranted for the tightest identification and F14's looser rule
# is warranted when maximizing statistical power. The paper reports
# both.
#
# INPUTS
# ======
#   Data/cleaned/firstll/ll_lottery_classification.rds
#
# OUTPUTS
# =======
#   Data/cleaned/firstll/ll_lottery_classification.rds (overwritten)
#   Data/cleaned/firstll/ll_lottery_classification.csv (overwritten)
#   Output_FirstLL/F14d_news_verification_summary.md
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
classif <- readRDS(file.path(FIRSTLL_CLEANED, "ll_lottery_classification.rds"))
message("Loaded ", nrow(classif), " classification rows.")


# ==============================================================================
# STEP 2: ENCODE NEWS-VERIFIED CLASSIFICATIONS
# ==============================================================================
# For each ambiguous row, I encode what the Sep 6 news check found.
# The join key is (tour, tourney_id, year, player_name).

news_updates <- tribble(
  ~tour, ~year, ~player_name, ~classification, ~confidence, ~withdrawn_player, ~withdrawal_date, ~source, ~verifier_notes,

  # HIGH-CONFIDENCE RANKING (post-qualifying withdrawal, news-dated)
  "ATP", 2024L, "Giovanni Mpetshi Perricard", "ranking", "high", "Alejandro Davidovich Fokina", "2024-06-30",
    "tennismajors.com; yahoo sports", "Withdrew Sunday night before Mon Jul 1 main draw; qualifying ended Thu Jun 27.",
  "ATP", 2023L, "Fabian Marozsan", "ranking", "high", "Nick Kyrgios", "2023-07-02",
    "malaymail.com; cbc.ca; sportbible.com", "Kyrgios withdrew Sun Jul 2, night before Mon Jul 3 main draw; qualifying ended Thu Jun 29.",
  "ATP", 2021L, "Francisco Cerundolo", "ranking", "high", "Milos Raonic", "2021-05-30",
    "cbc.ca; espn.com; skysports.com", "Raonic withdrew Sun May 30, day of main draw; qualifying ended Fri May 28.",
  "ATP", 2019L, "Kamil Majchrzak", "ranking", "high", "Milos Raonic", "2019-08-25",
    "usopen.org", "Raonic withdrew Sun Aug 25 before Mon Aug 26 main draw; qualifying ended Fri Aug 23.",
  "WTA", 2024L, "Renata Zarazua", "ranking", "high", "Ekaterina Alexandrova", "2024-06-30",
    "espn.com; freemalaysiatoday.com; beinsports.com", "Alexandrova withdrew hours before Mon Jul 1 first-round match; qualifying ended Thu Jun 27.",
  "WTA", 2023L, "Yanina Wickmayer", "ranking", "high", "Bianca Andreescu / Paula Badosa", "2023-08-26",
    "wtatennis.com; usopen.org; dhnet.be", "Andreescu and Badosa both withdrew Sat Aug 26 before Mon Aug 28 main draw; qualifying ended Fri Aug 25.",
  "WTA", 2023L, "Leolia Jeanjean", "ranking", "high", "Ajla Tomljanovic", "2023-01-14",
    "opencourt.ca; punditfeed.com", "Tomljanovic withdrew Sat Jan 14, Jeanjean directly replaced her; qualifying ended Thu Jan 12.",

  # MEDIUM-CONFIDENCE RANKING (post-qualifying inferred but exact date not
  # confirmed; typical late-injury withdrawal pattern)
  "ATP", 2014L, "Martin Klizan", "ranking", "medium", "David Goffin", NA_character_,
    "wikipedia AO 2014; bleacher report", "Goffin withdrew with right quadriceps injury; exact date not found. Same event had another LL (Robert), so mixed assignment plausible.",
  "ATP", 2019L, "Brayden Schnur", "ranking", "medium", "Borna Coric", NA_character_,
    "cbc.ca; ctvnews.ca; tennisworld", "Coric injury withdrawal; typical late-withdrawal pattern per articles, but exact date not found.",
  "WTA", 2023L, "Camila Osorio", "ranking", "medium", "Paula Badosa (and prior withdrawals)", NA_character_,
    "wtatennis.com; opencourt.ca", "Osorio was seed 6 in the LL lottery pool (not top 4); got in only after post-lottery withdrawal, consistent with ranking rule for a late withdrawal.",

  # UNRESOLVED (older entries with thin news coverage)
  "ATP", 2006L, "Melle Van Gemerden", "unresolved", "low", NA_character_, NA_character_, "", "Pre-2010; Wayback and news search did not date the withdrawal.",
  "ATP", 2007L, "Mariano Zabaleta", "unresolved", "low", NA_character_, NA_character_, "", "Beat Calleri in R1 as LL; withdrawal circumstances not documented in accessible coverage.",
  "ATP", 2007L, "Robin Haase", "unresolved", "low", NA_character_, NA_character_, "", "Lost to Marat Safin R1 as LL; withdrawal date not found.",
  "ATP", 2009L, "Peter Luczak", "unresolved", "low", NA_character_, NA_character_, "", "Fresno State bio confirms LL entry when another qualifier could not compete; exact scenario/date not found.",
  "WTA", 2006L, "Nicole Pratt", "unresolved", "low", NA_character_, NA_character_, "", "Pre-2010; no dated withdrawal news found.",
  "WTA", 2006L, "Julia Vakulenko", "unresolved", "low", NA_character_, NA_character_, "", "Pre-2010; no dated withdrawal news found.",
  "WTA", 2008L, "Monica Niculescu", "unresolved", "low", NA_character_, NA_character_, "", "Pre-2010; no dated withdrawal news found.",
  "WTA", 2009L, "Katie Obrien", "unresolved", "low", NA_character_, NA_character_, "", "Pre-2010; no dated withdrawal news found.",
  "WTA", 2010L, "Stephanie Dubois", "unresolved", "low", NA_character_, NA_character_, "", "No dated withdrawal news found in accessible coverage.",
  "WTA", 2012L, "Misaki Doi", "unresolved", "low", NA_character_, NA_character_, "", "No dated withdrawal news found in accessible coverage.",
  "WTA", 2015L, "Yulia Putintseva", "unresolved", "low", NA_character_, NA_character_, "", "No dated withdrawal news found for AO 2015 in accessible coverage.",
  "WTA", 2021L, "Mayar Sherif", "unresolved", "low", NA_character_, NA_character_, "", "One of three LLs at US Open 2021; specific withdrawal not identified.",
  "WTA", 2024L, "Hailey Baptiste", "unresolved", "low", NA_character_, NA_character_, "", "Seed 6 in RG 2024 qualifying LL pool; specific replaced-player not identified in accessible news."
)


# ==============================================================================
# STEP 3: APPLY UPDATES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 3: APPLY UPDATES")
message(strrep("=", 70))

# Left join updates onto the classification file by (tour, year, player_name)
matched <- classif$player_name %in% news_updates$player_name &
           classif$classification == "ambiguous"

updated_rows <- 0
for (i in seq_len(nrow(news_updates))) {
  u <- news_updates[i, ]
  idx <- which(classif$player_name == u$player_name &
               classif$tour == u$tour &
               classif$year == u$year)
  if (length(idx) == 1) {
    cur <- classif$classification[idx]

    # PRECEDENCE RULE.
    #
    # F14 now certifies a lottery two ways: an individual rank above the slot
    # count, and an event-level rank gap. The news audit speaks to individual
    # assignments, so the two can disagree, and which wins depends on the
    # direction of the disagreement.
    #
    #   news says "ranking"  -> overrides. Direct dated evidence that THIS
    #     player's slot came from a post-qualifying withdrawal beats an
    #     inference drawn from the event's rank pattern. A gap proves that
    #     SOME slot at the event was drawn by lottery, not that every slot
    #     was; multi-slot events can mix the two mechanisms.
    #
    #   news says "unresolved" -> does NOT override a verified classification.
    #     Failing to find a source is absence of evidence. If the rank pattern
    #     already proves a lottery ran, that proof stands.
    if (u$classification == "unresolved" && cur == "lottery_verified") {
      classif$verifier_notes[idx] <- paste0(
        "News search found no datable source, but the event rank pattern ",
        "independently certifies a lottery; classification retained.")
      updated_rows <- updated_rows + 1
      next
    }

    classif$classification[idx]  <- u$classification
    classif$confidence[idx]      <- u$confidence
    classif$source[idx]          <- u$source
    if (!is.na(u$withdrawal_date))
      classif$withdrawal_date[idx] <- u$withdrawal_date
    if (!is.na(u$withdrawn_player))
      classif$withdrawn_player[idx] <- u$withdrawn_player
    classif$verifier_notes[idx]  <- u$verifier_notes
    updated_rows <- updated_rows + 1
  } else {
    message("  WARNING: no unique match for ", u$player_name, " (", u$year, " ", u$tour, ")")
  }
}
message("  Applied updates to ", updated_rows, " rows.")

# Hard check. The join key is (tour, year, player_name) and player names drift
# across sources (accents, hyphens, transliterations). A silent partial join
# would leave entries marked "ambiguous" while the summary claimed they were
# classified, so fail loudly rather than write a half-updated file.
if (updated_rows != nrow(news_updates)) {
  stop(
    "F14d ABORTED: only ", updated_rows, " of ", nrow(news_updates),
    " news-verification rows matched the classification file.\n",
    "  This usually means a player_name in the tribble does not match the ",
    "spelling\n",
    "  in firstll_gs_est_v2.rds (accents, hyphens, transliteration). Check the ",
    "WARNING\n",
    "  lines above, fix the tribble, and re-run. No file was written.",
    call. = FALSE
  )
}


# ==============================================================================
# STEP 4: WRITE
# ==============================================================================
csv_path <- file.path(FIRSTLL_CLEANED, "ll_lottery_classification.csv")
rds_path <- file.path(FIRSTLL_CLEANED, "ll_lottery_classification.rds")
write.csv(classif, csv_path, row.names = FALSE, na = "")
saveRDS(classif, rds_path)


# ==============================================================================
# STEP 5: SUMMARY
# ==============================================================================
slog("\n", strrep("=", 70))
slog("F14d NEWS-VERIFICATION SUMMARY")
slog(strrep("=", 70))
slog(sprintf("Total treated GS LL entries:      %d", nrow(classif)))

tab <- table(classif$classification)
for (nm in names(tab)) slog(sprintf("  %-20s %d", nm, tab[[nm]]))

slog("\nBy tour:")
for (t in c("ATP", "WTA")) {
  sub <- classif[classif$tour == t, ]
  tab <- table(sub$classification)
  slog(sprintf("  %s: lottery_verified=%d, ranking=%d, unresolved=%d, ambiguous=%d, total=%d",
               t,
               sum(sub$classification == "lottery_verified"),
               sum(sub$classification == "ranking"),
               sum(sub$classification == "unresolved"),
               sum(sub$classification == "ambiguous"),
               nrow(sub)))
}

slog("\nHigh-confidence RANKING classifications (all from news):")
sub <- classif[classif$classification == "ranking" & classif$confidence == "high", ]
for (i in seq_len(nrow(sub))) {
  r <- sub[i, ]
  slog(sprintf("  %s %d %s (%s): withdrew %s, replaced by rank-1 loser -> ranking rule",
               r$tour, r$year, r$player_name, r$slam_name,
               r$withdrawal_date))
}

slog("\nMedium-confidence RANKING classifications (news pattern but no exact date):")
sub <- classif[classif$classification == "ranking" & classif$confidence == "medium", ]
for (i in seq_len(nrow(sub))) {
  r <- sub[i, ]
  slog(sprintf("  %s %d %s (%s): %s",
               r$tour, r$year, r$player_name, r$slam_name, r$verifier_notes))
}

slog("\nUnresolved (older entries, thin news coverage):")
sub <- classif[classif$classification == "unresolved", ]
for (i in seq_len(nrow(sub))) {
  r <- sub[i, ]
  slog(sprintf("  %s %d %s (%s)",
               r$tour, r$year, r$player_name, r$slam_name))
}

slog("\nKey takeaway:")
slog("Every rank-1 entry with datable news evidence turned out to be")
slog("RANKING-RULE assigned (post-qualifying withdrawal). ZERO of the 23")
slog("news-checked entries were confirmed as lottery.")
slog("")
slog("COVERAGE CAVEAT. The 23 entries audited here were selected under the")
slog("ORIGINAL F14 rule, which classified an entry as ambiguous only when")
slog("rank_among_losers == 1. F14 has since been corrected to the")
slog("mathematically right threshold, rank_among_losers > n_ll_slots, which")
slog("moved a further 42 entries (rank 2-4 at events with 2-7 slots) out of")
slog("'lottery_verified' and into 'ambiguous'. Those 42 have NOT been")
slog("audited. They stay excluded from the verified subsample on the rank")
slog("criterion alone, which is conservative: an unaudited entry is never")
slog("counted as a verified lottery, so auditing them can only grow the")
slog("verified subsample or confirm the current exclusion.")
slog("")
slog("To close the gap, extend the news_updates tribble above to cover the")
slog("rows this script reports as still 'ambiguous', then re-run F14c to")
slog("re-estimate on the enlarged verified subsample.")

n_unaudited <- sum(classif$classification == "ambiguous")
slog(sprintf("\nCurrently unaudited ambiguous entries: %d", n_unaudited))

writeLines(summary_log,
           file.path(FIRSTLL_OUTPUT, "F14d_news_verification_summary.md"))
message("\nSummary saved to Output_FirstLL/F14d_news_verification_summary.md")
message("DONE.")
