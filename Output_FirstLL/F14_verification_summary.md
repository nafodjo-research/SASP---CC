F13 verified-lottery results file: loaded
Event-level rank-gap test: 44 of 64 events show a gap
Deterministic classification:
  ambiguous            33
  lottery_verified     61

By tour:
  ATP: lottery_verified=38, ambiguous=23, total=61
  WTA: lottery_verified=23, ambiguous=10, total=33

======================================================================
F14 CLASSIFICATION SUMMARY
======================================================================
Total treated GS LL entries: 94
  Lottery-verified (auto):   61 (64.9%)
  Ambiguous (needs review):  33 (35.1%)

Sample sizes after restricting to lottery-verified events:
  (Both treated and their event-mates enter the identifying sample.)
  ATP: N=70 (T=38, C=32) across 29 events
  WTA: N=41 (T=23, C=18) across 15 events

Ambiguous entries by tour (curation targets for F14b):
  ATP: 23 entries needing withdrawal-timing verification
  WTA: 10 entries needing withdrawal-timing verification

Next steps for the ambiguous set:
  1. F14b_wayback_verification.R (to be written): scripted Wayback
     Machine + tournament press-release queries for each ambiguous row.
  2. Manual curation for entries F14b cannot resolve.
  3. F14c_manual_review.R: reads the CSV back after curation and
     re-estimates the headline stacked model on the verified subsample.
