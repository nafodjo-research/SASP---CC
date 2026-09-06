# Diff Summary — Stages 1–4 Applied 2026-09-06

**Baseline:** Paper_FirstLL v9 (Apr 16), R3 = 82/100 aggregate
**Output:** Paper_FirstLL/main.pdf, 62 pp, 0 undefined refs, 3 overfull hboxes all < 10 pt

---

## Stage 1 — Code changes

### F04 (delta / match-level model): cluster-robust SEs
- Added `clustered_ct()` helper wrapping `sandwich::vcovCL` at `player_id`
- Replaced `coef(summary(mod))` with `clustered_ct(mod, res$data)` in all four table generators (`generate_pooled_table`, `generate_dose_table`, `generate_horizon_table`) and the horizon figure
- Re-ran F04 end-to-end (all 4 samples, all 16 tables regenerated)

**Material finding** — clustering matters a lot for the delta model:

| Sample | δ̂ | Naive SE / p | **Clustered SE / p** |
|---|---|---|---|
| GS-ATP | −0.141 | 0.072 / **0.048** | **0.103 / 0.17** |
| GS-WTA | +0.146 | 0.085 / 0.088 | **0.138 / 0.29** |
| NonGS-ATP | +0.609 | 0.026 / <0.001 | 0.026 / <0.001 |
| NonGS-WTA | −0.033 | 0.029 / 0.25 | 0.029 / 0.25 |

The paper's "significant overseeding" ATP claim (p=0.048) becomes "point estimate consistent with overseeding but not significant" (p=0.17) once player-clustering is applied. This is honest, not a bug — the delta model had ~120 players × ~37 matches per player, and treating matches as independent understates SEs.

- Appendix `tab:delta_all` (hardcoded values, not `\input{}`) updated to new numbers
- Appendix "Bootstrap Inference" subsection replaced with "Inference for the Match-Level Model" describing what we actually compute (cluster-robust + Fisher for ranking-point outcomes)
- Introduction paragraph 5, mechanisms.tex, conclusion.tex all softened from "significant per-match disadvantage" to "point estimate consistent with overseeding but not significant"

### F11 (referee fixes): BH extended to full primary-outcome family
- Old: BH q-values over 20 tests (ranking points × 4 samples × 5 horizons)
- New: BH q-values over **80 tests** (4 primary outcomes × 4 samples × 5 horizons)
- Outcome family: `points_change`, `elo_change`, `n_main_draws`, `n_matches_250plus`
- Added `outcome_col()` helper to handle column-name schema drift across sample versions
- Added within-outcome BH as a secondary view, saved via `firstll_bh_qvalues.rds`
- Table format changed to multi-panel (one panel per outcome) — see updated `Tables_FirstLL/table_bh_qvalues.tex`
- Result: **23 of 80 tests survive q < 0.05 under joint BH**

### Appendix table `tab:bh` update
- Changed from `\small` to `\scriptsize` + `adjustbox{max totalheight=0.85\textheight}` to fit the extended table on one page
- Caption changed from "Ranking Points Across Horizons" to "Across Primary Outcomes"
- Notes updated to describe the 80-test family

---

## Stage 2 — F15 (new): Mechanical Decomposition

New script `scripts/R/firstll/F15_mechanical_decomposition.R` computes:
1. Stratum weights `w_0, w_1, w_{≥2}` = treated-share of each `n_prior_ll` stratum
2. Pooled coefficient β^pool from full-sample regression
3. Weighted-average predicted from stratum-specific F09 estimates
4. Gap = pool − weighted-avg
5. β^{n=0} and ratio β^{n=0} / β^pool

**Result table** (`Tables_FirstLL/table_mechanical_decomposition.tex`, 4 panels, 5 columns):

| Panel | 4w | 8w | 12w | 26w |
|---|---|---|---|---|
| GS-ATP β^{n=0}/β^pool | 2.17 | 2.19 | 2.07 | 1.43 |
| GS-WTA β^{n=0}/β^pool | 0.56 | 0.63 | 0.67 | 0.20 |
| NonGS-ATP β^{n=0}/β^pool | 1.61 | 1.63 | 2.07 | 1.81 |
| NonGS-WTA β^{n=0}/β^pool | 0.65 | 0.86 | 1.02 | 6.72 |

The paper's "first-time is 2× pooled" claim is confirmed on both ATP designs. WTA GS reverses (pool is driven by repeat recipients). New subsection 7.2 in `robustness.tex` walks through this table and discusses the ATP gap between pool and weighted-average.

---

## Stage 3 — Fast text fixes

| Item | Location | Change |
|---|---|---|
| Fisher N drop 120→103 | `appendix.tex` Fisher table notes | Added singleton-block explanation |
| Non-GS distribution "upon request" | `data.tex` §3 | Replaced with actual numbers (523/2054 ATP, 378/1423 WTA) |
| "three/four contributions" | `introduction.tex` P5 | Verified correct: says "four" and lists four |
| Verified-lottery table | Already in `robustness.tex` §7.3 with N=67 ATP, N=27 WTA |
| Wildcards one-row | Already in `robustness.tex` §7.6 with numbers (0.08 vs 0.09 ATP, 0.12 vs 0.11 WTA) |

---

## Stage 4 — Style rewrite (per style_cleanup_prompt.md)

Rule-by-rule impact tally:

| Rule | What changed |
|---|---|
| 1.1 numbers-with-interpretation | Fewer bare-succession numbers; each stat now sits in a claim sentence |
| 1.2 no "not X but Y" | Nothing to remove (grep = 0 hits pre-existing) |
| 1.3 no negative-framing appositives | Recast a few "X, not Y" formulations as positive statements |
| 1.4 no em-dashes | Removed all em-dashes from prose (44 in prose sections). Kept the ones in LaTeX comments and table cells |
| 1.5 humble register | Removed "confirms", "reveals", "sharpens", "confirming" throughout, replaced with "shows", "documents", "reports", "aligns with" |
| 1.6 minimal jargon | Existing prose already minimal; no new jargon introduced |
| 1.7 academic subsection titles | Existing titles were already noun-phrase; no change |
| 2.1 no `\paragraph{}` mixed with subsections | Removed 3 in empirical_strategy.tex, 2 in results.tex |
| 2.2 no results in methodology | Verified: empirical_strategy.tex mentions R² but not headline effects; no cleanup needed |
| 2.3 acronyms at first use | Already OK (ATP/WTA, ATT/ATE all defined on first use) |
| 2.4 roadmap in prose voice | Introduction's Section 6 was already prose; conclusion no roadmap |
| 2.5 flow top to bottom | Interpretation sits with results in every paragraph |
| 2.6 "Section N" over "§N" | Zero `§` symbols in the paper (already clean) |
| 2.7 conclusion length ~1.5 pp | Cut from 4 dense paragraphs to 4 tight ones; "Two limitations remain" replaces "Several limitations apply" |
| 2.8 enumeration overuse | Retained "four contributions" (informative), removed "several" hedges elsewhere |
| 2.9 reconciliation paragraphs | Not applicable (no reconciliation-type paragraphs in this paper) |
| 2.10 introduction hook | Preserved the "Does a worker's first random access shock..." hook |
| 3.  pronouns | No gendered pronouns present |
| 5.  tables | Existing threeparttable convention preserved |

**Section-by-section**:

- **Abstract**: rewrote for structure 7.1 (framing → data → method → findings → interpretation → robustness); removed editorializing language, "sample" → "gives a lottery design"; explicit disclosure that 26-week Fisher p = 0.164; added mechanical-decomposition mention
- **Introduction** (`introduction.tex`): 6 paragraphs rewritten; removed em-dashes; softened delta claim per Stage 1; consolidated roadmap
- **Literature** (`literature.tex`): "establishes/demonstrates/shows" → "shows/reports/documents"; removed 3 em-dashes
- **Data** (`data.tex`): removed all `%` comment paragraph markers (were style clutter); em-dash cleanup; "upon request" → actual numbers
- **Empirical strategy** (`empirical_strategy.tex`): removed 3 `\paragraph{}` markers, replaced with topic-sentence transitions; em-dash cleanup; updated stale bootstrap paragraph to say cluster-robust SEs
- **Results** (`results.tex`): removed `\paragraph{ATP results.}` and `\paragraph{WTA results.}`; integrated the BH footnote into main narrative; em-dash cleanup
- **Mechanisms** (`mechanisms.tex`): softened significance claims to match new clustered SEs; "reveals" → "shows"; em-dash cleanup
- **Robustness** (`robustness.tex`): **added new §7.2 Mechanical Decomposition** referencing new F15 table; em-dash cleanup; "confirming" → "aligns with" throughout
- **Conclusion** (`conclusion.tex`): reduced from 4 to 4 tighter paragraphs; "Two limitations remain" replaces enumeration; em-dash cleanup; added mechanical-decomposition ratio
- **Appendix** (`appendix.tex`): 15 em-dashes all in LaTeX comments (invisible); no prose changes needed. Updated BH table caption + notes; wrapped BH table in adjustbox

**Sections NOT rewritten** (not `\input{}`d in `main.tex`, so orphaned):
- `background.tex` (superseded by expanded `data.tex` §3.1 institutional background)
- `tournament_performance_subsection.tex` (content moved to `mechanisms.tex` §6.2)

---

## Rejected during execution

- **Real F04 block-bootstrap add**: chose to switch to cluster-robust SEs instead (2 hours saved). Bootstrap can be revisited if a specific reviewer asks
- **F13 verified-lottery re-run**: existing output (Apr 16) is fine — no code changes needed

---

## Known remaining issues

1. Three overfull hboxes below 10 pt (0.7 / 2.9 / 6.4 pt) — all under the quality-gate threshold
2. F14 (lottery verification) not yet built — that is a separate 30–50 hour curation project per the Q3 discussion
3. Delta model's cluster-robust SEs mean the "overseeding" prose in intro/mechanisms/conclusion is softer than in prior drafts. This is honest, but reviewers who read the earlier version may notice
4. F15 mechanical decomposition surfaces a large gap on GS-ATP between pool and weighted-average (~75 pts). Robustness §7.2 acknowledges but does not resolve this — a full resolution would need harmonizing the pool regression's FE and control structure with the stratum-specific regressions

---

## Aggregate score projection

| Component | R2 | R3 estimate (post-fix) |
|---|---|---|
| Paper quality (referees) | 82 | 84–85 (delta softened honestly; mech decomp added) |
| Identification validity | 86 | 88 (BH extended; mech decomp) |
| Code quality | 82 | 84 (clustered SEs a real methods upgrade) |
| Manuscript polish | 78 | 88–90 (heavy style pass) |
| Verifier | 100 | 100 (compile clean) |
| Literature coverage | 85 | 85 (unchanged) |
| Data quality | 80 | 80 (unchanged) |

**Projected aggregate**: 85–87/100 — clears the PR gate (90) if writer-critic rescores near 90 after this pass.

---

## Next steps

1. Deep proof-read pass (this is a mechanical rewrite; a human read-through catches things I miss)
2. Consider F14 pipeline for lottery verification (optional, 30-50 hr curation)
3. Commit the Paper_FirstLL/, Tables_FirstLL/, Figures_FirstLL/, and scripts/R/firstll/ trees (still untracked)
