# Methods Referee Report
**Date:** 2026-09-06
**Paper:** The First Break: Career Effects of Initial Access Shocks in Tournament Labor Markets
**Design:** Lottery-based ITT (Grand Slam, primary) + Control Function (non-GS, corroborative)
**Calibrated to:** Journal of Human Resources / AEJ:Applied
**Recommendation:** Major Revisions
**Overall Score:** 72/100

## Summary
The paper uses the Grand Slam lucky-loser lottery as random assignment within the top-4 qualifying-loser pool, restricts to first-time recipients (N=120 ATP, N=82 WTA), and extends the analysis to non-GS events via an analytically derived control function that uses exact enumeration over 2^{Ne−1} outcomes of peer qualifying matches. The core empirical narrative (positive short-term access effects, null Elo, fade-out within a year) is directionally credible and honestly hedged, and Round 4 has meaningfully improved the paper (player-clustered SEs, expanded BH, verified-lottery subsample, mechanical decomposition, news verification). But three methodological issues still block a Minor Revisions verdict: (i) the "mechanical decomposition" of §7.2 is not a valid identity as presented; (ii) the primary sample includes 23 rank-1 observations for which the news check found zero lottery confirmations, arguing that the verified-lottery subsample should be primary and the pooled sample should be robustness; (iii) the ATP CF fails its own validation on the randomized benchmark by a wide margin, materially weakening the non-GS ATP corroboration.

## Dimension Scores
| Dimension | Weight | Score | Notes |
|-----------|--------|-------|-------|
| Identification | 35% | 68 | Lottery is real, but lottery/rank contamination is unresolved for ~28% of ATP and ~43% of WTA GS observations; the news check strengthens the case for elevating the verified sample. |
| Estimation | 25% | 72 | Stacked model reasonable; analytical control function derivation is clean; mechanical decomposition equation is not a valid LIE identity under pooled TWFE with heterogeneous strata. |
| Inference | 20% | 72 | Player-clustered SEs now appropriate; Fisher inference is a plus; but no wild-cluster bootstrap for the small verified-lottery samples, only 1,000 permutations, and the 80-test BH family assumes exchangeability across highly correlated horizons. |
| Robustness | 15% | 78 | Verified subsample, balance, CF validation, wildcards all reported; missing formal pre-trend placebo, no attrition analysis, no Oster/Lee bounds. |
| Replication | 5% | 85 | GitHub URL provided; formal replication package not described in the paper. |
| **Weighted** | 100% | **72.1** | |

Computation: 0.35(68) + 0.25(72) + 0.20(72) + 0.15(78) + 0.05(85) = 23.80 + 18.00 + 14.40 + 11.70 + 4.25 = 72.15.

## Sanity Check Results
- **Sign:** Plausible. Positive ranking-point effects for LL entrants, null Elo on ATP, fade-out by 52w — all consistent with the access-not-ability mechanism.
- **Magnitude:** Plausible. Immediate-event effect of 36.4 ranking points (roughly one first-round loss plus a half-second-round loss) with 72–83 point effects at 4–12w implies ~40 additional points from downstream access, which is credible given the rolling window.
- **Dynamics:** Coherent. ATP peaks at 12w, fades by 52w; WTA concentrates at 8w. Event-study figures show visually parallel pre-trends, but no formal pre-period placebo regression is reported.
- **Consistency:** Mostly stable. Verified-lottery ATP 26w (+76.6, p=0.050) is close to headline (+72.6). But headline 4w and 8w effects (72.5, 72.1) drop to (33.2, 39.1, both n.s.) in the verified subsample; the paper interprets this as consistent-in-sign, but the halving of the point estimate at the shortest horizons is not trivial. Fisher inference at 26w fails (p=0.164) while clustered inference succeeds (p=0.047) — a fragility the paper acknowledges.

## Major Comments

**1. The mechanical decomposition (§7.2, Table 12) is not a valid identity.**
The paper writes β^{pool} vs. w_0 β^{(n=0)} + w_1 β^{(n=1)} + w_{≥2} β^{(n≥2)} and invokes the law of iterated expectations. Under a pooled TWFE regression with event fixed effects and shared covariate coefficients Γ, β^{pool} is not the treated-share-weighted average of stratum-specific ATTs. As Sloczynski (2020, ReStat) shows for TWFE with binary treatment and heterogeneous effects, and as Goodman-Bacon (2021, JEcs) shows for staggered designs, the implicit pooled weights depend on the within-stratum variance of D and on the covariate composition, and can differ arbitrarily from the treated-share weights. The "75-point gap" between pool (31.9) and weighted average (−43.5) at 4w on ATP GS is very plausibly a specification artifact (pooled event FE absorb between-stratum variation asymmetrically; n=1 stratum has small cells producing large-magnitude negative point estimates that pull the weighted average down mechanically). **Fix:** (a) drop the LIE framing; (b) instead estimate a saturated interaction specification Y = α_e + α_h + β·D + γ_1·D·1[n=1] + γ_2·D·1[n≥2] + ΓZ + ε on the pooled sample, and report β + γ_1 + γ_2 as the stratum-specific ATTs (which will now integrate correctly to the pooled sample estimand); (c) present the "twice the pool" claim from this specification, not from a false decomposition. This is a rewriting exercise, not a re-estimation exercise.

**2. The verified-lottery subsample should be the primary sample, not a robustness check.**
The news verification of the 23 rank-1 first-time GS entries is a striking finding: 10 of 10 datable cases were ranking-rule assignments after post-qualifying withdrawals; 0 were lottery draws. Assuming the 13 unresolved rank-1 cases are similar (a defensible working hypothesis given the mechanism and the datable subset), the pooled sample contains ~19% (23/120) ATP and ~28% (23/82) WTA rank-based assignments — potentially all upward-biasing. The paper's stance ("the verified-lottery subsample recovers the headline") accepts a slightly wobblier headline than necessary. **Fix:** promote the verified subsample (N=86 ATP, N=47 WTA) to primary in the abstract, intro, and Section 5. Report the full first-time sample (N=120, N=82) as a robustness check that pools verified lotteries with ambiguous rank-1 entries whose lottery status the news check cannot confirm. This will strengthen — not weaken — the paper's identification story, at the cost of some precision. The 26w ATP effect (76.6, p=0.050) and 8w WTA effect (77.5, p=0.005) survive.

**3. The ATP CF fails its own randomized-benchmark validation, and this should carry more weight.**
Table A.13 (CF validation) shows that applying the CF to the randomized ATP GS sample flips ranking-point estimates from +72.5 to −106.2 (p<0.10), Elo from −4.4 to −89.3 (p<0.01), and main draws from −0.2 to −2.2 (p<0.10). This is not a small perturbation; it is a categorical failure of the CF specification on the sample where the CF should be inert. The paper's explanation ("uniform-within-top-4 mischaracterizes selection") is a description, not a diagnosis. Two implications: (i) the non-GS ATP CF estimates cannot be treated as clean corroboration — they identify a different estimand under a specification that we know fails on the closest-adjacent randomized benchmark; (ii) any headline claims that pool GS and non-GS "aligned" evidence should be revised to acknowledge that the non-GS ATP CF has a specification problem the paper cannot diagnose. **Fix:** (a) re-estimate the non-GS ATP results with alternative CF specifications — heteroskedastic probit, semi-parametric with polynomial series in P^{LL}, or an IV using P^{LL} directly (Heckman two-step with LATE interpretation); (b) if any of these produce more sensible CF validation on GS-ATP, adopt that specification; (c) if none do, demote the non-GS ATP results to "extends the design to a larger but non-randomized sample where the exact identification is less clean," and re-frame the abstract accordingly.

**4. The GS n=1 stratum estimates are physically implausible and should be handled better.**
Table 8 (marginal impact by LL experience) reports GS-ATP second-LL ranking-point coefficients of −220.9 at 4w, −215.3 at 8w, −195.6 at 12w with SEs of 45–49, all significant at p<0.01. Given that a player who does not receive LL entry at a GS earns 0 ranking points at that event and the maximum plausible LL-event dose is on the order of 90–180 points (round-by-round), a negative treatment effect of magnitude −220 requires the counterfactual outcome for controls to be extraordinarily positive at that horizon — implausible on face given that controls are LL-naive players ranked 100–250. The paper flags this ("sometimes larger in absolute value than the physically possible treatment dose") but then uses these numbers in the mechanical decomposition. **Fix:** either (a) drop the GS n=1 cells from the decomposition and report the non-GS decomposition (which uses N≈700 cells and is well-identified) as the sole quantitative claim, or (b) use Bayesian shrinkage / empirical Bayes to shrink the small GS n=1 estimates toward zero before computing the weighted average. As currently reported, the "gap" is arithmetically dominated by two or three noisy negative point estimates.

**5. Small-sample inference: wild-cluster bootstrap is needed for the verified-lottery subsamples and the marginal-impact GS strata.**
The paper reports Fisher randomization inference (1,000 permutations within Slam×Year blocks) as the small-sample-appropriate check for the headline, which is good. But (i) the verified subsample (N=86 ATP, N=47 WTA) is where the paper's second-line claims live, and asymptotic clustered SEs at N=47 with an unknown but small number of player clusters likely over-reject; (ii) the GS n=1 and n≥2 strata are also small. **Fix:** apply wild-cluster bootstrap (Cameron, Gelbach, Miller 2008; Roodman et al. 2019, Stata Journal) with the Webb 6-point weight distribution to all verified-lottery estimates and to the marginal-impact GS strata. Report the WCB p-values alongside the clustered p-values in Tables 11 and 8. Also: 1,000 permutations for Fisher inference gives a p-value standard error of ~0.005 at p=0.03, which is borderline. Bumping to 5,000–10,000 permutations is cheap and would tighten the reported Fisher p-values without changing the story.

**6. Pre-treatment placebo is asserted from figures, not tested.**
The event-study figures (Fig. 3, 4) plot cumulative points from t=−12 weeks to t=+52 weeks and are visually parallel pre-event. But no formal placebo regression is reported: regressing pre-event outcome changes (e.g., ranking-point change from t=−8 to t=−4) on the eventual LL treatment status. Given the within-event balance rejection on ATP (F-test p=0.046, driven by age p=0.040), a formal pre-period placebo strengthens the claim substantially. **Fix:** add a table of placebo pre-period regressions at h ∈ {−12, −8, −4} weeks for ranking points and Elo, using the same specification as the headline. If the placebo coefficients are indistinguishable from zero, this is the strongest single defense against the pooled-sample contamination concern.

**7. Differential attrition at 52 weeks is not discussed.**
From Table A.1 summary statistics for ATP GS: at 52w, 46 of 61 treated players (75%) are observed vs. 37 of 59 controls (63%). The 12-percentage-point differential at 52w plausibly reflects LL-treated players continuing to play more matches (and therefore having observed ranking outcomes), while controls exit the sample at higher rates. This is not just missing data — it selects on the treatment effect itself. **Fix:** report Lee (2009) bounds for the 52w estimates on both tours, or at minimum estimate a Heckman selection model with pre-event characteristics driving selection. The 52w ATP result already fades to insignificance, so this may not change the story — but the paper's claim of "fadeout by 52w" is currently underidentified against "differential exit."

**8. BH family definition — treat correlated horizons as one effective test.**
The 80-test BH family (4 outcomes × 4 samples × 5 horizons) treats horizons within an outcome as independent tests, but they share substantial variance (a player's 4w, 8w, and 12w outcomes are heavily autocorrelated). Under strong positive dependence, BH is valid but conservative; under BH-Yekutieli (2001), q-values would rise further. **Fix:** report Romano-Wolf step-down p-values (Romano and Wolf 2005 Econometrica; Romano and Wolf 2016 for Stata implementation) as a complementary MHT correction that accounts for dependence. Also report a "one primary claim per sample-outcome" family of 16 tests (4 outcomes × 4 samples) where the horizon is treated as a nuisance dimension — this is more consistent with how the paper argues its identification, and would likely leave 6–8 tests surviving at q<0.05.

## Minor Comments

1. **N-per-cluster undisclosed.** State the number of unique player clusters for each sample. This determines whether asymptotic clustered SEs are OK. Rule of thumb: below 40 clusters, wild bootstrap; below 20, be honest that inference is uncertain.

2. **Number of horizons in the stacked estimand.** The stacked model runs 5 horizons on ~120 obs → ~600 stacked observations. The paper says "approximately 600" — report the exact number and the exact number of unique episodes per horizon (some 26w and 52w episodes are censored; this affects the stacked structure).

3. **Weighting in the mechanical decomposition.** The weights w_n are described as "treated-share." Are they the share of treated observations in the pooled sample that come from stratum n, or the share of pool-eligible observations? The interpretation of the ratio differs.

4. **CF validation Table (A.13) shows blanks for ρ̂_h.** Populate these — the ρ̂_h values are the key diagnostic and their absence is a table-quality issue.

5. **Immediate effects table shows the treatment effect is 39.9pp higher win rate.** This is at the focal event but conditions on control means being 0 (control players do not play). The paper says this correctly ("mechanically zero"), but the resulting β̂ = 0.39 is essentially the LL win rate itself, not a treatment effect against a counterfactual. Consider dropping the immediate-effects "Event FE" columns and presenting only the LL means as descriptive.

6. **Elo Δ point estimate discrepancies between Tables 3 (regression) and A.9 (Fisher).** ATP Elo 4w is −4.4 in Table 3 but −1.2 in Table A.9 (Fisher panel); ATP 12w is −4.8 vs −5.6. Small differences, but they suggest the Fisher analysis uses a different sample or specification. Explain.

7. **Prize-money magnitudes should be re-scaled if headline moves to verified sample.** The $101K figure is computed on the pooled headline. If the primary sample changes to the verified subsample, recompute.

8. **The exclusion restriction for the non-GS CF** needs more defense. Peer match outcomes may proxy for on-day event conditions (surface pace, weather, referee tightness) that affect player i through channels other than LL slot allocation. The paper argues residual correlation is "a priori weak" but does not test it.

## Technical Suggestions

- **Re-estimate the ATP CF with a semi-parametric first stage.** Use a series expansion in P^{LL} rather than the analytical normal-CDF form. If validation still fails, use a nonparametric matching estimator on non-GS instead of the CF.

- **Add an IV/2SLS specification for non-GS using P^{LL} as instrument.** This gives a LATE interpretation for the marginal LL entrant and does not require the normal-error CF parametric assumption.

- **Report Oster (2019) δ or Cinelli-Hazlett (2020) sensitivity bounds.** Given the CF specification concerns and residual balance issues, a sensitivity analysis showing how much unobserved selection would be needed to overturn the headline is high-value.

- **Show event-study coefficients at pre-event horizons (h = −4, −8, −12 weeks).** A formal pre-period placebo table.

- **Report a Lee bounds analysis for 52w outcomes.** Given differential attrition.

- **Increase Fisher permutations to ≥5,000.** For stable p-values in the 0.02–0.05 range.

- **Add Romano-Wolf step-down p-values.** Complements the BH correction, accounts for dependence across horizons.

- **Add a formal test of the "first-break" claim within the interaction specification.** Report H_0: γ_1 = 0 and H_0: γ_2 = 0 with joint F-test. This is what the "mechanical decomposition" is trying to do and should replace it.

## Questions for the Authors

1. What is the exact number of unique player clusters in the ATP GS (N=120) and WTA GS (N=82) samples? And in the verified-lottery subsamples (N=86, N=47)?

2. In the mechanical decomposition, are the weights w_n the treated-share in the pooled sample or the pool-eligible share? Please state the formal object β^{pool} identifies under pooled TWFE with heterogeneous effects.

3. Given that the news check found 10 of 10 datable rank-1 entries were ranking-rule assignments and 0 were lottery, what is the justification for keeping rank-1 entries in the primary sample rather than making the verified-lottery sample primary?

4. Can the ATP CF failure on the randomized benchmark be diagnosed more precisely? Is the issue the normal-error assumption in the generalized residual, the assumption of uniform selection within top-4, or something about the qualifying-match logit itself?

5. What is the exact stacked-observation count for each sample, and how are censored 52w observations handled?

6. Do the pre-event placebo regressions (regressing pre-event ranking changes on eventual LL treatment) yield null coefficients?

7. Is the CF re-estimated on the first-LL sample or on the full sample? Table A.3 states re-estimated on the restricted set — please confirm and report the exact match-count difference.

## Verdict

**Top methodological concern: the framing of the "mechanical decomposition" as a law-of-iterated-expectations identity, combined with the choice to keep the (largely rank-assigned) pooled sample as the headline rather than the verified-lottery subsample.** Both are pre-decisions that can be corrected in one revision cycle without new data. The lottery identification is sound where it applies; the paper's job is to be transparent about where it does not apply, and to make the verified-lottery sample carry the primary weight of the headline. Fix these two issues, add wild-cluster bootstrap for the small samples, and add a formal pre-period placebo, and this paper is ready for JHR/AEJ:Applied at Minor Revisions. As it stands, Round 4 substantially improved the paper (the player-clustered SEs and news verification are important upgrades), but three interlocking framing issues keep the overall score in the Major Revisions range.

---

**Score: 72/100 — Major Revisions**
**One-line verdict:** Lottery identification is credible in the verified subsample, but the paper's headline still rests on a pooled sample the news check suggests is largely rank-assigned, and the "mechanical decomposition" is not a valid identity as written — fix these two framing issues (elevate verified sample; replace decomposition with a saturated interaction specification) and this becomes a Minor Revision.
