# Falsification and Placebo Tests: Tennis Lucky Losers

**Date:** 2026-03-20
**Corresponds to:** `strategy_memo.md` and `robustness_plan.md`

---

## Overview

Falsification tests ask: "If the design is valid, what should we NOT find?" Each test below specifies the null hypothesis, the expected result under a valid design, and what a failure would imply.

---

## F1. Placebo Cutoffs (MUST)

**Logic:** If the effect is truly at the LL cutoff, there should be no discontinuity at other (non-cutoff) values of the running variable.

**Implementation:**
- Estimate the RDD at placebo cutoffs: $\tilde{R} = -3, -2, -1, +1, +2, +3$ (i.e., pretend the cutoff is shifted left or right)
- For each placebo cutoff, compute the point estimate and CI
- Report in a single figure alongside the true cutoff estimate

**Expected result:** No significant discontinuity at any placebo cutoff. Only the true cutoff should show an effect.

**What failure implies:** If effects appear at non-cutoff values, the "discontinuity" may be driven by a smooth underlying relationship (e.g., higher-ranked players have better outcomes everywhere, and the RDD is picking up slope rather than a jump).

---

## F2. Lagged Outcomes as Balance Checks (MUST)

**Logic:** Pre-determined outcomes (measured before the LL event) should not show a discontinuity at the cutoff. If they do, the running variable is correlated with pre-existing differences.

**Implementation:** Run the RDD with each of the following as the outcome variable:
- Ranking 12 weeks before the event: $\text{rank}_{t-12w}$
- Ranking 26 weeks before the event: $\text{rank}_{t-26w}$
- Ranking points 12 weeks before: $\text{points}_{t-12w}$
- Main draw match wins in the 6 months before the event
- Main draw entries in the 6 months before the event
- Qualifying attempts in the 12 months before the event
- Win rate in qualifying matches in the 12 months before

**Expected result:** No significant discontinuity for any lagged outcome. Point estimates near zero.

**What failure implies:** If pre-event ranking trends differ at the cutoff, the groups are not comparable and the RDD is invalid. This would suggest sorting or selection near the cutoff.

---

## F3. Covariate Balance at the Cutoff (MUST)

**Logic:** Pre-determined player characteristics should be smooth at the cutoff.

**Implementation:** Run the RDD (or local randomization test) with each covariate as the outcome:
- Age
- Height
- Handedness (left vs. right)
- Career stage (years on tour)
- Nationality (coded as region)
- Surface preference (share of prior matches on the tournament surface)

**Expected result:** No significant discontinuity. Report the number of covariates significant at the 5% level; under the null, at most 1 out of 6 should be significant by chance.

**What failure implies:** Systematic differences at the cutoff would undermine the as-if random interpretation. This would be the most damaging falsification failure.

---

## F4. Density / Manipulation Test (MUST)

**Logic:** If players cannot manipulate their ranking position relative to the LL cutoff, the density of the running variable should be smooth at the cutoff.

**Implementation:**
- Cattaneo, Jansson, and Ma (2020) test via `rddensity` package
- Also report the McCrary (2008) test for comparison with older literature
- Caveat: with a discrete running variable, density tests have reduced power and standard tests may not apply directly. Report the mass at each integer value of $\tilde{R}$ in a histogram.

**Expected result:** No significant bunching at or just below the cutoff. The distribution of $\tilde{R}$ should be approximately symmetric around zero.

**What failure implies:** Bunching just below the cutoff (more players at $\tilde{R} = 0$ than expected) would suggest manipulation. However, this is highly implausible here because: (a) the player's ranking is determined by 52 weeks of prior results, (b) the cutoff depends on unpredictable withdrawals. If the test rejects, investigate whether the finding is mechanical (e.g., tournaments with many LL slots pile up observations near zero).

---

## F5. Placebo Outcomes (MUST)

**Logic:** The RDD should show no effect on outcomes that CANNOT be affected by LL entry.

**Implementation:**
- **Height:** Player height cannot be changed by LL entry. Estimate the RDD on height.
- **Handedness:** Cannot change. Estimate on a left-hand indicator.
- **Birth year / age at first professional match:** Cannot be affected.
- **Nationality:** Cannot be affected.

**Expected result:** Precisely estimated zeros for all placebo outcomes.

**What failure implies:** A "significant" effect on height or handedness would indicate a broken design (systematic differences at the cutoff, not treatment effects).

---

## F6. Grand Slam Lottery Randomization Verification (MUST)

**Logic:** Within the Grand Slam top-4 pool, LL assignment should be independent of rank position (1st, 2nd, 3rd, 4th in the pool).

**Implementation:**
- Tabulate: among top-4 pool members, what share became LLs by rank position?
  - Rank 1 (best): share = ?
  - Rank 2: share = ?
  - Rank 3: share = ?
  - Rank 4: share = ?
- Chi-squared test: H0 = LL probability is equal across all 4 positions
- Fisher exact test (for small cells)
- Also test whether observable covariates (age, ranking level, prior Grand Slam experience) differ between LL recipients and non-recipients within the pool

**Expected result:** LL probability is approximately equal across positions 1-4. No covariate differences.

**What failure implies:** If the highest-ranked pool member is significantly more likely to become the LL, the "lottery" may actually involve ranking-based priority (the institutional rule may be misunderstood or may have changed). This would invalidate the lottery design and convert it into a mini-RDD within the pool.

---

## F7. Withdrawal Count Independence (SHOULD)

**Logic:** The number of LL slots (driven by main draw withdrawals) should be uncorrelated with the characteristics of qualifying losers.

**Implementation:**
- Regress $c_t$ (withdrawal count / LL slots at tournament $t$) on:
  - Average ranking of final-round qualifying losers at tournament $t$
  - Average age of qualifying losers
  - Surface
  - Tournament tier
  - Year
- Test joint significance of qualifying loser characteristics (conditional on tournament tier, surface, year)

**Expected result:** Qualifying loser characteristics do not predict the number of LL slots, conditional on tournament-level controls.

**What failure implies:** If tournaments with stronger qualifying fields also have more withdrawals, the treatment intensity (number of LL slots) is correlated with player quality. This does not invalidate the within-tournament RDD (which conditions on $c_t$) but would complicate the interpretation of pooled estimates.

---

## F8. Testing for Anticipation Effects (SHOULD)

**Logic:** If players anticipate becoming LLs (e.g., they know a main draw player is likely to withdraw), they might alter their qualifying match behavior. This would violate the "no anticipation" assumption.

**Implementation:**
- Among final-round qualifying losers who eventually become LLs, test whether their qualifying match performance differs from non-LL losers:
  - Sets won in the final qualifying round loss
  - Games won per set
  - Match duration
  - Whether the loss was close (e.g., deciding set) vs. one-sided
- RDD: estimate the discontinuity in closeness of the qualifying loss at the LL cutoff

**Expected result:** No difference in qualifying match performance at the cutoff. The quality of the loss should be similar for LL recipients and non-recipients.

**What failure implies:** If LL recipients had systematically closer losses (suggesting they were "trying harder" because they knew they might become LLs), the as-if random interpretation is weakened. However, this is unlikely because LL status depends on post-qualifying withdrawals.

---

## F9. Checking for Composition Effects over Time (SHOULD)

**Logic:** If the composition of the qualifying loser pool changes over the sample period (e.g., due to rule changes or data quality improvements), time trends could confound the estimates.

**Implementation:**
- Plot the average ranking, age, and sample size of qualifying losers over time
- Test whether the LL effect varies smoothly over time (estimate year-by-year or rolling-window effects)
- Check for structural breaks around known rule changes (2009 points restructuring, 2006 GS lottery introduction)

**Expected result:** Stable composition and stable effects over time. No structural breaks at rule change dates.

**What failure implies:** If the qualifying loser pool becomes systematically different over time, include year fixed effects (already planned) and show robustness to period restrictions.

---

## F10. Regression to the Mean Test (SHOULD)

**Logic:** Players who lose in the final qualifying round may have been performing above their long-run average (they qualified deep into the draw). Subsequent ranking declines could be regression to the mean rather than a treatment effect.

**Implementation:**
- Test whether the ranking trajectory of ALL final-round qualifying losers (both LL and non-LL) shows mean reversion
- If both groups show ranking declines after the event, the LL effect is the DIFFERENCE in trajectories, which nets out common mean reversion
- Specifically: estimate the RDD on $\Delta \text{ranking}_{t+26w}$. If mean reversion is symmetric, it cancels in the difference at the cutoff.

**Expected result:** Both groups may show some mean reversion, but the RDD compares WITHIN the group of qualifying losers, so common mean reversion is differenced out. The RDD estimate should not be driven by differential mean reversion.

**What failure implies:** If higher-ranked qualifying losers (closer to the LL cutoff) have stronger mean reversion than lower-ranked losers (further from cutoff), the RDD would pick up differential mean reversion rather than a treatment effect. Test by plotting mean reversion as a function of $\tilde{R}$ outside the bandwidth -- it should be smooth.

---

## F11. Excluding Repeat LL Recipients (MAY)

**Logic:** Some players may become LLs multiple times. If repeat LLs are systematically different (e.g., they are "professional qualifiers" who frequently reach the final round), they could drive results.

**Implementation:**
- Identify players who received LL entry more than once in the sample
- Re-estimate excluding all observations from repeat LL recipients
- Alternatively, use only the first LL event per player

**Expected result:** Results should be similar. If they change substantially, the effect is concentrated among repeat qualifiers (a specific subgroup).

---

## F12. Testing the "Loser's Curse" / SUTVA (SHOULD)

**Logic:** If SUTVA is violated, the player just below the cutoff (the "unlucky non-LL") may perform WORSE than players further from the cutoff due to the psychological or competitive harm of being the one who just missed.

**Implementation:**
- Compare outcomes for $\tilde{R} = 1$ (just missed) vs. $\tilde{R} = 2, 3, 4, ...$ (further from cutoff)
- If $\tilde{R} = 1$ has systematically worse outcomes than $\tilde{R} = 2, 3$, this suggests SUTVA violation
- Estimate the "control group gradient": how do control outcomes vary with distance from the cutoff?

**Expected result:** Outcomes should be approximately flat (or slowly declining) in $\tilde{R}$ among controls. No sharp dip at $\tilde{R} = 1$.

**What failure implies:** A dip at $\tilde{R} = 1$ would suggest the estimated RDD effect combines a positive treatment effect with a negative SUTVA spillover. The RDD estimate would be an upper bound on the true treatment effect. This is important to acknowledge and bound.

---

## Summary Table

| Test | ID | Priority | Expected Result | What Failure Implies |
|------|----|----------|----------------|---------------------|
| Placebo cutoffs | F1 | MUST | No effect at non-cutoff values | Smooth relationship, not a jump |
| Lagged outcomes | F2 | MUST | No pre-event discontinuity | Non-comparable groups |
| Covariate balance | F3 | MUST | Smooth covariates at cutoff | Sorting / selection |
| Density test | F4 | MUST | No bunching at cutoff | Manipulation (implausible) |
| Placebo outcomes | F5 | MUST | Zero effect on height, hand, etc. | Broken design |
| GS lottery randomization | F6 | MUST | Equal LL probability by rank in pool | Rule misunderstood |
| Withdrawal independence | F7 | SHOULD | No correlation with loser quality | Endogenous treatment intensity |
| Anticipation effects | F8 | SHOULD | No qualifying match differences | Strategic behavior |
| Composition over time | F9 | SHOULD | Stable pool and stable effects | Time trends confound |
| Regression to mean | F10 | SHOULD | Common mean reversion cancels | Differential mean reversion |
| Repeat LLs | F11 | MAY | Similar results excluding repeats | Effect driven by subgroup |
| SUTVA / Loser's curse | F12 | SHOULD | No dip for just-missed controls | RDD overestimates |

**Total MUST tests:** 6
**Total SHOULD tests:** 5
**Total MAY tests:** 1
