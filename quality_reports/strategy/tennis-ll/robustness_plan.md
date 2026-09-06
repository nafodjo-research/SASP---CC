# Robustness Plan: Tennis Lucky Losers

**Date:** 2026-03-20
**Corresponds to:** `strategy_memo.md` Section 6 and `pseudo_code.md`

---

## Overview

Robustness checks are organized into five categories: (A) specification sensitivity, (B) sample restrictions, (C) alternative estimators, (D) heterogeneity, and (E) mechanism tests. Each check is classified by priority (MUST, SHOULD, MAY) and maps to a specific referee concern.

---

## A. Specification Sensitivity

### A1. Bandwidth Sensitivity (MUST)

**Concern addressed:** Results may be sensitive to the choice of bandwidth around the cutoff.

**Implementation:**
- Estimate main results at 50%, 75%, 100%, 125%, 150%, and 200% of the MSE-optimal bandwidth
- For the local randomization framework, vary the window width from $w=1$ to $w=5$ (integer steps)
- Report all estimates in a single figure (point estimates + 95% CIs)
- Results should be qualitatively stable across bandwidths

**Package:** `rdrobust` (bandwidth grid) and `rdlocrand` (window grid)

### A2. Polynomial Order Sensitivity (MUST)

**Concern addressed:** Local polynomial estimates may depend on the polynomial order.

**Implementation:**
- Report local linear ($p=1$) and local quadratic ($p=2$) estimates side by side
- Do NOT use global polynomials of any order (Gelman and Imbens 2019)
- For the local randomization framework, this is not applicable (uses difference in means)

### A3. Kernel Sensitivity (SHOULD)

**Concern addressed:** Kernel choice may affect estimates.

**Implementation:**
- Primary: triangular kernel (MSE-optimal)
- Robustness: uniform kernel (equivalent to unweighted local regression within bandwidth)
- Robustness: Epanechnikov kernel

### A4. Covariate Adjustment (SHOULD)

**Concern addressed:** Adding covariates should not change results if the RDD is valid, but should improve precision.

**Implementation:**
- Report main results without covariates (uncontrolled)
- Report with covariates: age, career stage, tournament tier, surface, year
- Point estimates should be similar; standard errors may shrink

### A5. Running Variable Normalization (SHOULD)

**Concern addressed:** The choice to normalize $\tilde{R} = R - c_t$ vs. using the raw rank could matter.

**Implementation:**
- Primary: normalized running variable ($\tilde{R}$, cutoff at 0)
- Robustness: raw rank among losers, with tournament-specific cutoffs
- Robustness: normalized by pool size: $\tilde{R}_{norm} = (R - c_t) / N_t$

---

## B. Sample Restrictions

### B1. Donut Hole RDD (MUST)

**Concern addressed:** Observations exactly at the cutoff may be special (SUTVA violation, measurement error at the boundary).

**Implementation:**
- Exclude observations with $\tilde{R} \in \{-1, 0, 1\}$ (the two players immediately flanking the cutoff)
- Also try excluding only $\tilde{R} = 0$ (the marginal treated player)
- Results should survive the donut hole; if they weaken substantially, the effect is driven by the boundary players (consistent with SUTVA violation)

### B2. Single-Slot Tournaments Only (MUST)

**Concern addressed:** When multiple LL slots open, the comparison is less clean because multiple players cross the threshold.

**Implementation:**
- Restrict to tournaments where $c_t = 1$ (exactly one LL slot)
- This provides the cleanest comparison: one player just above and one just below a single cutoff
- Report sample size reduction and whether results hold

### B3. Time Period Restrictions (SHOULD)

**Concern addressed:** Data quality and institutional rules change over time.

**Implementation:**
- Full sample: 2007-2024
- Restricted: 2011-2024 (qualifying match statistics available)
- Restricted: 2009-2023 (post-points-restructuring, pre-2024 changes)
- Restricted: 2007-2019 (overlap with Maity et al. for validation)
- Early vs. late period split: 2007-2015 vs. 2016-2024

### B4. Tournament Level Restrictions (SHOULD)

**Concern addressed:** Effects may differ by tournament tier; pooling may mask heterogeneity.

**Implementation:**
- Full sample: all ATP tour-level events (G, M, A)
- Restricted: exclude Grand Slams (since they use a different LL mechanism)
- Restricted: Masters 1000 only
- Restricted: ATP 500/250 only
- Results by tier inform the treatment dose mechanism

### B5. Excluding Walkovers and Retirements (SHOULD)

**Concern addressed:** Final qualifying round losses via walkover or retirement are different from match losses.

**Implementation:**
- Primary: include all final-round losers
- Robustness: exclude walkovers (score = "W/O") and retirements (score contains "RET")
- These players may have been injured, which confounds their future outcomes

### B6. Player Frequency Trimming (MAY)

**Concern addressed:** Some players appear as final-round qualifying losers many times; they may drive results.

**Implementation:**
- Identify players appearing in the sample more than 10 times
- Re-estimate excluding the top 5% of repeat players
- Alternatively, weight observations inversely by player frequency

---

## C. Alternative Estimators

### C1. Three-Framework Comparison (MUST)

**Concern addressed:** With a discrete running variable, no single estimator is universally valid.

**Implementation:**
- Local randomization (`rdlocrand`): primary
- Continuity-based (`rdrobust`): robustness
- Honest CIs (`RDHonest`): robustness
- All three reported in the main results table

### C2. Fuzzy RDD (SHOULD)

**Concern addressed:** If some players decline the LL spot, the RDD is fuzzy rather than sharp.

**Implementation:**
- Report the first-stage concordance rate
- If concordance < 100%, estimate fuzzy RDD using `rdrobust` with `fuzzy = is_ll`
- The running variable predicts LL eligibility (as-if sharp), but actual entry may differ
- Compare sharp and fuzzy estimates; they should be close if non-compliance is rare

### C3. Oster (2019) Bounds for Selection on Unobservables (SHOULD)

**Concern addressed:** Could unobserved heterogeneity at the cutoff explain the results?

**Implementation:**
- Compute the Oster delta (ratio of selection on unobservables to observables required to explain away the result)
- If delta > 1, results are robust to proportional selection
- Package: `psacalc` (Stata) or manual computation in R

### C4. Permutation Inference (SHOULD)

**Concern addressed:** Asymptotic inference may be unreliable with discrete running variables and small effective samples.

**Implementation:**
- Randomization inference (Fisher exact test) within the local randomization framework
- Permute treatment assignment within tournament blocks (preserving the number of treated and controls per tournament)
- Report exact p-values alongside asymptotic ones

### C5. Lee (2009) Bounds for Sample Selection (MAY)

**Concern addressed:** If LL entry affects whether we observe subsequent outcomes (e.g., players who do not receive LL entry may drop out of the rankings), differential attrition biases the estimates.

**Implementation:**
- Check whether the RDD predicts missingness of follow-up outcomes
- If differential attrition exists, compute Lee bounds (trimming the group with lower attrition)
- Package: `leeinger` or manual implementation

---

## D. Heterogeneity Analysis

### D1. By Tournament Tier (MUST)

**Motivation:** Treatment dose varies by tier -- Grand Slams offer the most ranking points and prize money; ATP 250s offer the least.

**Implementation:** Estimate main RDD separately for Grand Slams, Masters 1000, and ATP 500/250. If effects increase with tier, this supports the treatment dose mechanism.

### D2. By Career Stage (MUST)

**Motivation:** Young players building their careers may benefit more from marginal opportunities than established veterans.

**Implementation:** Split sample at median career stage (years on tour). Alternatively, split at age 23 (common cutpoint for "developing" vs. "established" players).

### D3. By Proximity to Financial Break-Even (SHOULD)

**Motivation:** Schoettl et al. (2025) document that ranking ~150 is the break-even point. Players near this threshold have the most to gain from marginal ranking improvements.

**Implementation:** Split sample at ranking 100-200 vs. below 100 or above 200 at the time of the qualifying event.

### D4. By Surface (SHOULD)

**Motivation:** Players may have surface-specific skills; LL entry on a preferred surface may yield better performance.

**Implementation:** Estimate separately for hard, clay, and grass court events.

### D5. By Era (SHOULD)

**Motivation:** The competitive landscape and points structure have changed over time.

**Implementation:** Split at 2015 (approximate midpoint) or at the 2009 points restructuring.

### D6. By Player Nationality / Region (MAY)

**Motivation:** Players from countries with strong tennis federations may have better support systems to capitalize on an LL opportunity.

**Implementation:** Split by broad region (Europe, Americas, Asia/Oceania, Africa) or by federation support tier.

### D7. By Gender (ATP vs. WTA) (SHOULD)

**Motivation:** If WTA data is used, comparing effects across tours provides an external validity check.

**Implementation:** Estimate separately for ATP and WTA. Report pooled and separate results.

---

## E. Mechanism Tests

### E1. Immediate Points Gained (MUST)

**Motivation:** The direct mechanism: LL entry allows earning ranking points at the tournament. The first question is whether LLs actually earn non-trivial points.

**Implementation:** Estimate the RDD on ranking points earned at the specific tournament. Also report the distribution of LL match results (R1 loss, R2, R3, etc.).

### E2. Tournament Access Channel (MUST)

**Motivation:** The cumulative advantage mechanism: ranking points from LL entry improve the ranking, which improves future tournament access (direct entry rather than qualifying).

**Implementation:**
- Estimate the RDD on the number of subsequent main draw entries obtained via direct acceptance (not qualifying, not LL, not WC) in the next 3, 6, and 12 months
- If LL entry causally increases direct entry to future tournaments, this is evidence for the cumulative advantage channel

### E3. Opponent Quality Channel (SHOULD)

**Motivation:** LL entry exposes the player to higher-quality opponents in the main draw. Does this "learning by competing" matter?

**Implementation:** Estimate the RDD on the average ranking of opponents faced in the next 3 months. If opponents get stronger, this is consistent with improved tournament access. Also test whether LL recipients' win rate against higher-ranked opponents improves.

### E4. Decomposition: Direct Points vs. Cumulative Advantage (SHOULD)

**Motivation:** Is the medium-term effect simply the mechanical ranking point gain, or does it amplify through cumulative advantage?

**Implementation:**
- Compute the "direct effect": ranking points earned at the LL tournament, mechanically translated into ranking improvement using the points table
- Compare to the actual ranking change at t+26w and t+52w
- If the actual change exceeds the mechanical direct effect, the excess is attributable to cumulative advantage (additional tournaments accessed, additional points earned at those tournaments)

### E5. Prize Money Channel (MAY)

**Motivation:** LL entry provides immediate prize money. For financially precarious players, this may affect ability to continue on tour.

**Implementation:** If prize money data is available, estimate the RDD on prize money earned at the tournament and over the following 6-12 months.

---

## Reporting Summary

| Category | Check | Priority | Where Reported |
|----------|-------|----------|---------------|
| A1 | Bandwidth sensitivity | MUST | Main paper, figure |
| A2 | Polynomial order | MUST | Main paper, table column |
| A3 | Kernel sensitivity | SHOULD | Appendix table |
| A4 | Covariate adjustment | SHOULD | Main paper, table column |
| A5 | RV normalization | SHOULD | Appendix table |
| B1 | Donut hole | MUST | Main paper, table row |
| B2 | Single-slot tournaments | MUST | Main paper, table row |
| B3 | Time period restrictions | SHOULD | Appendix table |
| B4 | Tournament level restrictions | SHOULD | Heterogeneity table |
| B5 | Exclude walkovers/retirements | SHOULD | Appendix table |
| B6 | Player frequency trimming | MAY | Appendix table |
| C1 | Three-framework comparison | MUST | Main paper, table |
| C2 | Fuzzy RDD | SHOULD | Appendix or main (if needed) |
| C3 | Oster bounds | SHOULD | Main paper, discussed in text |
| C4 | Permutation inference | SHOULD | Main paper (p-values) |
| C5 | Lee bounds | MAY | Appendix (if attrition exists) |
| D1 | By tournament tier | MUST | Main paper, heterogeneity table |
| D2 | By career stage | MUST | Main paper, heterogeneity table |
| D3 | By financial break-even proximity | SHOULD | Main paper or appendix |
| D4 | By surface | SHOULD | Appendix |
| D5 | By era | SHOULD | Appendix |
| D6 | By nationality/region | MAY | Appendix |
| D7 | By gender (ATP vs WTA) | SHOULD | Main paper if WTA included |
| E1 | Immediate points gained | MUST | Main paper, results section |
| E2 | Tournament access channel | MUST | Main paper, mechanism section |
| E3 | Opponent quality channel | SHOULD | Mechanism section or appendix |
| E4 | Decomposition | SHOULD | Discussion section |
| E5 | Prize money channel | MAY | If data available |

**Total MUST checks:** 10
**Total SHOULD checks:** 17
**Total MAY checks:** 5
