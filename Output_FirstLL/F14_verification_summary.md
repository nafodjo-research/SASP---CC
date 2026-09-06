F13 verified-lottery results file: loaded
Deterministic classification:
  ambiguous            23
  lottery_verified     71

By tour:
  ATP: lottery_verified=51, ambiguous=10, total=61
  WTA: lottery_verified=20, ambiguous=13, total=33

======================================================================
F14 CLASSIFICATION SUMMARY
======================================================================
Total treated GS LL entries: 94
  Lottery-verified (auto):   71 (75.5%)
  Ambiguous (needs review):  23 (24.5%)

Sample sizes after restricting to lottery-verified events:
  (Both treated and their event-mates enter the identifying sample.)
  ATP: N=86 (T=55, C=31) across 35 events
  WTA: N=47 (T=27, C=20) across 17 events

Ambiguous entries by tour (curation targets for F14b):
  ATP: 10 entries needing withdrawal-timing verification
  WTA: 13 entries needing withdrawal-timing verification

Next steps for the ambiguous set:
  1. F14b_wayback_verification.R (to be written): scripted Wayback
     Machine + tournament press-release queries for each ambiguous row.
  2. Manual curation for entries F14b cannot resolve.
  3. F14c_manual_review.R: reads the CSV back after curation and
     re-estimates the headline stacked model on the verified subsample.
