
======================================================================
F14d NEWS-VERIFICATION SUMMARY
======================================================================
Total treated GS LL entries:      94
  lottery_verified     71
  ranking              10
  unresolved           13

By tour:
  ATP: lottery_verified=51, ranking=6, unresolved=4, ambiguous=0, total=61
  WTA: lottery_verified=20, ranking=4, unresolved=9, ambiguous=0, total=33

High-confidence RANKING classifications (all from news):
  ATP 2019 Kamil Majchrzak (US Open): withdrew 2019-08-25, replaced by rank-1 loser -> ranking rule
  ATP 2021 Francisco Cerundolo (Roland Garros): withdrew 2021-05-30, replaced by rank-1 loser -> ranking rule
  ATP 2023 Fabian Marozsan (Wimbledon): withdrew 2023-07-02, replaced by rank-1 loser -> ranking rule
  ATP 2024 Giovanni Mpetshi Perricard (Wimbledon): withdrew 2024-06-30, replaced by rank-1 loser -> ranking rule
  WTA 2023 Leolia Jeanjean (Australian Open): withdrew 2023-01-14, replaced by rank-1 loser -> ranking rule
  WTA 2023 Yanina Wickmayer (US Open): withdrew 2023-08-26, replaced by rank-1 loser -> ranking rule
  WTA 2024 Renata Zarazua (Wimbledon): withdrew 2024-06-30, replaced by rank-1 loser -> ranking rule

Medium-confidence RANKING classifications (news pattern but no exact date):
  ATP 2014 Martin Klizan (Australian Open): Goffin withdrew with right quadriceps injury; exact date not found. Same event had another LL (Robert), so mixed assignment plausible.
  ATP 2019 Brayden Schnur (Wimbledon): Coric injury withdrawal; typical late-withdrawal pattern per articles, but exact date not found.
  WTA 2023 Camila Osorio (Roland Garros): Osorio was seed 6 in the LL lottery pool (not top 4); got in only after post-lottery withdrawal, consistent with ranking rule for a late withdrawal.

Unresolved (older entries, thin news coverage):
  ATP 2006 Melle Van Gemerden (Roland Garros)
  ATP 2007 Mariano Zabaleta (Roland Garros)
  ATP 2007 Robin Haase (US Open)
  ATP 2009 Peter Luczak (US Open)
  WTA 2006 Nicole Pratt (US Open)
  WTA 2006 Julia Vakulenko (Wimbledon)
  WTA 2008 Monica Niculescu (Roland Garros)
  WTA 2009 Katie Obrien (Roland Garros)
  WTA 2010 Stephanie Dubois (Wimbledon)
  WTA 2012 Misaki Doi (Wimbledon)
  WTA 2015 Yulia Putintseva (Australian Open)
  WTA 2021 Mayar Sherif (US Open)
  WTA 2024 Hailey Baptiste (Roland Garros)

Key takeaway:
Every rank-1 ambiguous entry with datable news evidence turned out
to be RANKING-RULE assigned (post-qualifying withdrawal). ZERO of
the 23 news-checked entries were confirmed as lottery. The F14
verified-lottery subsample uses an event-level rule that includes
some events where the rank-1 LL was ranking-assigned but a co-treated
LL (rank > 1) was lottery-assigned; the pool-level randomization
is genuine at these events even though one of the treated LLs was
not randomized. F13's stricter rule (ALL treated must have
rank_among_losers > 1) excludes such mixed events.
