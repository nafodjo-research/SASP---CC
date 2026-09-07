# Editorial Synthesis: Round 4 Full Review

**Date:** 2026-09-06
**Paper:** The First Break: Career Effects of Initial Access Shocks in Tournament Labor Markets
**Scope:** Full pipeline — peer review (2 referees) + all critics (strategist, coder, writer, verifier)

---

## Score Trajectory

| Round | Domain | Methods | Referee Avg | Recommendation |
|---|---|---|---|---|
| R1 | 68 | 72 | 70 | Major Revisions |
| R2 | 76 | 77 | 76.5 | Major Revisions |
| R3 | 82 | 82 | 82 | Minor Revisions |
| **R4** | **76** | **72** | **74** | **Major Revisions** |

The R4 referee scores dropped ~8 points from R3. This is **not** because the paper got worse — every referee explicitly notes the R4 upgrades (cluster-robust SEs, extended BH, mechanical decomposition, F14 verified sample, news verification) are real improvements. It is because R4 referees went deeper, and new methodological objections surfaced that were not visible at R3 depth.

## R4 Component Scores

| Critic | Score | Verdict |
|---|---|---|
| Domain referee | 76 | Major Revisions |
| Methods referee | 72 | Major Revisions |
| Strategist critic | 86 | Commit-ready (below PR) |
| Coder critic | 76 | Commit-ready (below PR) |
| Writer critic | 87 | Commit-ready (below PR) |
| Verifier | PASS (100) | 9/9 checks pass |

## Weighted Aggregate

Using the weights from `quality.md`:

| Component | Weight | Score | Contribution |
|---|---|---|---|
| Paper quality (peer refs avg) | 25% | 74 | 18.5 |
| Identification validity (strategist) | 25% | 86 | 21.5 |
| Code quality (coder) | 15% | 76 | 11.4 |
| Manuscript polish (writer) | 10% | 87 | 8.7 |
| Replication readiness (verifier) | 5% | 100 | 5.0 |
| Literature coverage (domain dim) | 10% | 72 | 7.2 |
| Data quality (default) | 10% | 80 | 8.0 |
| **Total** | 100% | | **80.3** |

## Gate Status

| Gate | Threshold | R3 | R4 | Status |
|---|---|---|---|---|
| Commit | ≥80 | 83.6 | **80.3** | PASS (marginal) |
| PR | ≥90 | 83.6 | 80.3 | Fail (gap: 9.7) |
| Submission | ≥95 | 83.6 | 80.3 | Fail (gap: 14.7) |

The paper clears the commit gate. It does not clear the PR gate.

---

## Convergent Concerns (Multiple Reviewers Agree)

### 1. Elevate the verified-lottery subsample to primary
**Reviewers:** methods (Major #2), strategist (Blocking #1), coder (implicitly via F14 threshold bug)

The news verification (F14d) found 0 of 10 datable rank-1 entries were lottery draws — all were post-qualifying ranking-rule assignments. The pooled sample carries ~19% (ATP) to ~28% (WTA) rank-based observations, potentially all upward-biasing.

**Fix:** Promote the verified subsample (N=86 ATP, N=47 WTA) to primary in the abstract, intro, and Section 5. Report the pooled first-time sample (N=120, N=82) as robustness. The 26w ATP effect (76.6, p=0.050) and 8w WTA effect (77.5, p=0.005) survive.

### 2. Fix the mechanical decomposition framing (§7.2)
**Reviewers:** methods (Major #1), strategist (Blocking #3)

The "β^{pool} vs w_0·β^{n=0} + w_1·β^{n=1} + w_{≥2}·β^{n≥2}" equation is NOT a valid law-of-iterated-expectations identity under pooled TWFE with heterogeneous strata (Sloczynski 2020 ReStat; Goodman-Bacon 2021 JEcs). The 75-point ATP gap is a specification artifact from variance-weighted vs treated-share weights, not a substantive finding.

**Fix:** Replace the decomposition with a saturated interaction spec:
`Y = α_e + α_h + β·D + γ_1·D·1[n=1] + γ_2·D·1[n≥2] + ΓZ + ε`
Report β + γ_1 + γ_2 as the stratum-specific ATTs, which integrate correctly to the pooled estimand. Present the "twice the pool" claim from this specification.

### 3. Rewrite the first-break interpretation
**Reviewers:** domain (Major #1)

The Wald test rejection of first-LL = second-LL equality is equally consistent with (a) genuine behavioral first-break pivotality and (b) mechanical diminishing returns (later strata already carry ranking capital). The paper wants (a) but the evidence supports (b).

**Fix:** Either (i) condition on comparable pre-treatment ranking capital across strata to identify (a), or (ii) reframe the paper as "diminishing marginal returns to opportunity in a threshold-based tournament market."

### 4. Reframe the $100K headline
**Reviewers:** domain (Major #3)

The $100K figure conflates the mechanical immediate-event prize (conditional on advancement) with the causal 26-week cascade. Currently reads as if the whole $100K is the cascade.

**Fix:** Report the decomposition explicitly: $X mechanical immediate prize, $Y causal cascade. Only $Y is the object of interest.

### 5. Differentiate from Pallais (2014)
**Reviewers:** domain (Major #2, "single most consequential fix")

Pallais studies a randomized single opportunity in a reputational feedback labor market (oDesk ratings), finds persistent access effects. That is the closest analog and the paper does not differentiate.

**Fix:** Rewrite the contribution paragraph with Pallais as the anchor. Articulate 3–4 specific advances: dynamic tracking, ability decomposition, cascade fade-out, marginal returns to $n$th shock.

### 6. Add tournament-theory literature
**Reviewers:** domain (Major #5)

Missing: Lazear & Rosen (1981 JPE), Rosen (1986 AER), Prendergast (1999 JEL), Ehrenberg & Bognanno (1990 JPE), Kahn (2000 JEP), Genakos & Pagliero (2012 JPE), Malmendier & Nagel (2011 QJE), Rosen (1981 AER superstars).

**Fix:** Add a "tournament labor markets" paragraph to the literature section positioning the paper within tournament theory.

---

## Methodological Additions (Methods Referee)

Beyond the convergent issues:

- **Wild-cluster bootstrap** for verified-lottery subsamples (N=47 WTA has small cluster count) — Cameron-Gelbach-Miller 2008, Roodman et al. 2019
- **Formal pre-treatment placebo table** at h ∈ {−12, −8, −4} weeks (visual event studies are not enough given the ATP within-event F-test p=0.046)
- **Differential-attrition analysis at 52w** — Lee (2009) bounds (75% treated vs 63% control observation rate at 52w on ATP GS)
- **Alternative CF specifications** for ATP non-GS given the CF validation failure — semi-parametric probit, series expansion in P^{LL}, or IV/2SLS interpretation
- **Romano-Wolf step-down** to complement BH for correlated horizons
- **Fisher permutations** 1,000 → 5,000+ (cheap; tightens borderline p-values)
- **Oster (2019) sensitivity bounds** for headline results
- **Report exact cluster counts** for every clustered SE

## Code Fixes (Coder Critic)

Five blocking items to reach PR-ready code:

1. **F04 lines 764–771** — replace naive `coef(summary(mod))` with `clustered_ct(mod, res$data)` in the summary log; log currently contradicts tables
2. **F14 line 105** — change `rank_among_losers > 1` to `rank_among_losers > n_ll_slots` (mathematically correct classification threshold)
3. **F04 placeholder mode** — 16 placeholder tables silently ship if `tournament_rebuild_results.rds` is missing; add `stop()`
4. **F14d line 161** — add `stopifnot(updated_rows == nrow(news_updates))` to catch silent name-join failures
5. **`table_cf_validation.tex` collision** — F11 and F12 both write this filename; rename one

## Writer Fixes (Writer Critic)

Three tiny prose fixes to reach 90/100:

1. **`mechanisms.tex` line 11** — remove "however" mid-sentence
2. **`empirical_strategy.tex` line 74** — replace "sharpens the parameter estimates" with neutral technical phrasing
3. **`robustness.tex` line 69** — replace "delivers a sharper 8-week ranking-point effect" with plain reporting

Plus: orphan `background.tex` and `tournament_performance_subsection.tex` still contain em-dashes and `\paragraph{}` markers — delete or align.

---

## Editorial Decision: MAJOR REVISIONS (both referees)

Both peer referees independently recommend Major Revisions. This is a step back from R3's Minor Revisions consensus, and it should be read carefully:

- The paper has not regressed. R3 referees saw a solid, well-executed paper and let some deeper concerns slide because the surface was clean. R4 referees, faced with a still-cleaner surface, went deeper. The methodological objections that surfaced (mechanical decomposition identity, sample-choice-under-news-check, ATP CF failure) are the kind of issues that would surface at journal-level review anyway.
- The 74-point referee average is honest quality: this is a good paper with real work still to do. It is not a bad paper.
- The strategist critic (86, close to PR) and writer critic (87, close to PR) are both aligned with the referees' sense of "close, but".

The clearest path to a submission-ready draft:

**Phase 1 — Structural reframes** (12–20 hours writing):
- Elevate verified-lottery subsample to primary in abstract/intro/§5 (convergent issue #1)
- Rewrite §7.2 mechanical decomposition using the saturated interaction specification (convergent issue #2)
- Rewrite intro contribution paragraph with Pallais as the anchor (convergent issue #5)
- Add tournament-theory paragraph to literature (convergent issue #6)
- Decompose $100K into mechanical vs cascade (convergent issue #4)
- Adjudicate the first-break vs diminishing-returns framing (convergent issue #3)

**Phase 2 — Methodological additions** (10–15 hours R + writing):
- Wild-cluster bootstrap for verified subsamples
- Formal pre-treatment placebo table
- Lee bounds at 52w
- Alternative CF specification for ATP non-GS
- Romano-Wolf step-down alongside BH
- Bump Fisher permutations to 5,000+
- Cluster counts documented everywhere

**Phase 3 — Code fixes** (2–4 hours):
- F04 summary log fix, F14 rank rule fix, F04 placeholder, F14d stopifnot, table_cf_validation.tex rename

**Phase 4 — Writer polish** (30 min):
- Three prose fixes for "however", "sharpens", "sharper"

**Est. total effort to reach ≥90/100 aggregate**: 25–40 hours across all fronts.

### Path to Submission-Grade (≥95)

After Phase 1–4, one more revision cycle addressing the remaining methods-referee minor comments (SE type in CF table, exclusion restriction defense for CF, prize-money re-scaling if primary sample changes) and domain-referee minor comments (age imbalance in years, 6-month censoring robustness, alternative heterogeneity interpretation) would move the referees to Minor Revisions and put the aggregate over 95.

## Score Trajectory Summary

```
R1: 70 ── R2: 76.5 ── R3: 82 ── R4: 80.3 (aggregate)
                                   |
                                   └── referee avg: 74 (Major Revisions)
                                                       depth-driven, not quality-driven regression
```

**Verdict:** Commit ready. R4 has surfaced the last layer of methodological work — structural sample reframe, saturated interaction spec, small-sample inference upgrades. Address the 6 convergent issues and the paper is submission-ready at AEJ:Applied or JHR.
