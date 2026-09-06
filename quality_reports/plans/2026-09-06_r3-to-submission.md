# Plan — R3 → Submission Polish

**Date:** 2026-09-06
**Status:** APPROVED (user confirmed 1→2→3→4)
**Paper:** Paper_FirstLL/ — "The First Break: Career Effects of Initial Access Shocks"
**Baseline:** R3 = 82/100, weighted aggregate 83.6/100 (per firstll_editorial_synthesis_R3.md)
**Target:** ≥90 (PR gate) → prep for submission to JHR / AEJ:Applied / JLE / Labour Economics

---

## Stage 1 — Code re-runs

**1.1 F04 delta-model bootstrap (Option B)**
- Add player-level block bootstrap function to F04_delta_model.R
- N_BOOT = 1000 (MC SE ~0.007 vs current 0.015 at 200)
- Regenerate δ̂-hats for 4 samples (ATP GS, WTA GS, ATP non-GS, WTA non-GS)
- Update appendix.tex line 30 to accurately describe the inference (1,000 reps, player blocks)
- Update the appendix Table tab:delta_all with bootstrap SEs

**1.2 F11 BH extension**
- Currently: BH q-values applied to ranking-points outcome only
- Change: extend to Elo, main draws, matches 250+, points — the full primary-outcome set
- Regenerate table_bh_qvalues.tex

**Compile check**: rebuild main.tex, zero broken refs, no overfulls.

## Stage 2 — Mechanical decomposition (F15 new)

Write F15_mechanical_decomposition.R:
- Compute stratum weights w_0, w_1, w_{≥2} = shares of pooled sample by n_prior_ll
- Compute β^{n=0}, β^{n=1}, β^{n≥2} from stratum-specific regressions (already estimated in F09)
- Weighted average: w_0·β^{n=0} + w_1·β^{n=1} + w_{≥2}·β^{n≥2}
- Compare to β^{pool} at each of h ∈ {4,8,12,26,52} weeks
- Output: table_mechanical_decomposition.tex — 3 rows × 5 columns
- Text: one paragraph added to robustness.tex explaining calculation

## Stage 3 — Fast text fixes

- Wildcards one-row table (from F11 output; ~5 lines of tex)
- Fisher N=120→103 explanation (one sentence in appendix.tex near Fisher block)
- "Three contributions" → "four" (intro P5 — verify count is actually four)
- Abstract: Fisher p=0.164 at 26w — sharpen existing disclosure
- Verified lottery subsample: move from "upon request" to appendix table (F13 output exists in Output_FirstLL)

## Stage 4 — Style rewrite (per style_cleanup_prompt.md)

Apply-directly with diff summary (user preference). Order by section:

| Order | Section | Focus |
|---|---|---|
| 1 | Abstract | 1.5, 1.7, 7.1 |
| 2 | Introduction | 1.1, 1.2, 1.3, 1.5, 2.10 |
| 3 | Conclusion | 2.7 length, 1.5 self-glorification |
| 4 | Results | 1.1 numbers-with-interpretation, 2.2 no methodology, 2.6 section refs |
| 5 | Empirical strategy | 2.2, 1.6 jargon |
| 6 | Data | 1.4 em-dashes, 1.6 jargon |
| 7 | Mechanisms | 1.5, 2.2 |
| 8 | Robustness | 1.1, 1.5 |
| 9 | Literature, Background | Light pass |
| 10 | Appendix | Light pass |

Grep passes performed globally first:
- Pass 1: editorial adjectives/verbs
- Pass 2: editorial hedges
- Pass 3: em-dashes in prose
- Pass 4: `\paragraph{}` markers
- Pass 5: gendered pronouns
- Pass 6: `§` symbols
- Pass 7: enumeration in intro

## Stage 5 — Verify

- Compile main.tex clean (zero undefined refs, zero overfulls)
- Write diff summary
- Weighted aggregate rescore

## Parallel — Q3 pipeline (F14 lottery verification)

Written after Stage 1 concludes. Not blocking Stages 2-5.
- Scope: 236 GS LL entries
- Sources: Wayback Machine + tournament sites + Wikipedia + ATP/WTA press releases (Sackmann only carries tournament-week dates, cannot verify withdrawal timing)
- Deliverable: Data/cleaned/ll_lottery_classification.csv + verification appendix table
- Est. effort: 30-50 hours (Wayback batch scraping + manual verification)

---

## Rejected during scoping

- **F04 bootstrap 200 → 2000 reps**: MC SE improvement from 0.007 → 0.005 not worth 2× runtime. Using 1000.
- **Sackmann-only Stage A classification for F14**: Sackmann dates are tournament-week only. Cannot infer withdrawal timing without external sources.
- **Adding a "not shown but available" verified-lottery** verification: user (and referees) explicitly rejected this.
