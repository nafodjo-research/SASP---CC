
======================================================================
F14d NEWS-VERIFICATION SUMMARY
======================================================================
Total treated GS LL entries:      94
  ambiguous            21
  lottery_verified     56
  ranking              10
  unresolved           7

By tour:
  ATP: lottery_verified=35, ranking=6, unresolved=2, ambiguous=18, total=61
  WTA: lottery_verified=21, ranking=4, unresolved=5, ambiguous=3, total=33

High-confidence RANKING classifications (all from news):
  ATP 2024 Giovanni Mpetshi Perricard (Wimbledon): withdrew 2024-06-30, replaced by rank-1 loser -> ranking rule
  WTA 2023 Leolia Jeanjean (Australian Open): withdrew 2023-01-14, replaced by rank-1 loser -> ranking rule
  WTA 2024 Renata Zarazua (Wimbledon): withdrew 2024-06-30, replaced by rank-1 loser -> ranking rule
  ATP 2019 Kamil Majchrzak (US Open): withdrew 2019-08-25, replaced by rank-1 loser -> ranking rule
  ATP 2021 Francisco Cerundolo (Roland Garros): withdrew 2021-05-30, replaced by rank-1 loser -> ranking rule
  ATP 2023 Fabian Marozsan (Wimbledon): withdrew 2023-07-02, replaced by rank-1 loser -> ranking rule
  WTA 2023 Yanina Wickmayer (US Open): withdrew 2023-08-26, replaced by rank-1 loser -> ranking rule

Medium-confidence RANKING classifications (news pattern but no exact date):
  ATP 2014 Martin Klizan (Australian Open): Goffin withdrew with right quadriceps injury; exact date not found. Same event had another LL (Robert), so mixed assignment plausible.
  ATP 2019 Brayden Schnur (Wimbledon): Coric injury withdrawal; typical late-withdrawal pattern per articles, but exact date not found.
  WTA 2023 Camila Osorio (Roland Garros): Osorio was seed 6 in the LL lottery pool (not top 4); got in only after post-lottery withdrawal, consistent with ranking rule for a late withdrawal.

Unresolved (older entries, thin news coverage):
  ATP 2007 Mariano Zabaleta (Roland Garros)
  ATP 2009 Peter Luczak (US Open)
  WTA 2006 Nicole Pratt (US Open)
  WTA 2006 Julia Vakulenko (Wimbledon)
  WTA 2008 Monica Niculescu (Roland Garros)
  WTA 2012 Misaki Doi (Wimbledon)
  WTA 2015 Yulia Putintseva (Australian Open)

Key takeaway:
Every rank-1 entry with datable news evidence turned out to be
RANKING-RULE assigned (post-qualifying withdrawal). ZERO of the 23
news-checked entries were confirmed as lottery.

COVERAGE CAVEAT. The 23 entries audited here were selected under the
ORIGINAL F14 rule, which classified an entry as ambiguous only when
rank_among_losers == 1. F14 has since been corrected to the
mathematically right threshold, rank_among_losers > n_ll_slots, which
moved a further 42 entries (rank 2-4 at events with 2-7 slots) out of
'lottery_verified' and into 'ambiguous'. Those 42 have NOT been
audited. They stay excluded from the verified subsample on the rank
criterion alone, which is conservative: an unaudited entry is never
counted as a verified lottery, so auditing them can only grow the
verified subsample or confirm the current exclusion.

To close the gap, extend the news_updates tribble above to cover the
rows this script reports as still 'ambiguous', then re-run F14c to
re-estimate on the enlarged verified subsample.

Currently unaudited ambiguous entries: 21
