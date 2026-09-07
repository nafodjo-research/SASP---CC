F13 verified-lottery results file: loaded
Deterministic classification:
  ambiguous            65
  lottery_verified     29

By tour:
  ATP: lottery_verified=18, ambiguous=43, total=61
  WTA: lottery_verified=11, ambiguous=22, total=33

======================================================================
F14 CLASSIFICATION SUMMARY
======================================================================
Total treated GS LL entries: 94
  Lottery-verified (auto):   29 (30.9%)
  Ambiguous (needs review):  65 (69.1%)

Sample sizes after restricting to lottery-verified events:
  (Both treated and their event-mates enter the identifying sample.)
  ATP: N=49 (T=24, C=25) across 18 events
  WTA: N=31 (T=15, C=16) across 10 events

Ambiguous entries by tour (curation targets for F14b):
  ATP: 43 entries needing withdrawal-timing verification
  WTA: 22 entries needing withdrawal-timing verification

Next steps for the ambiguous set:
  1. F14b_wayback_verification.R (to be written): scripted Wayback
     Machine + tournament press-release queries for each ambiguous row.
  2. Manual curation for entries F14b cannot resolve.
  3. F14c_manual_review.R: reads the CSV back after curation and
     re-estimates the headline stacked model on the verified subsample.
