# Strategy Review R4: The First Break

**Date:** 2026-09-06
**Reviewer:** strategist-critic
**Files reviewed:**
- `Paper_FirstLL\sections\empirical_strategy.tex`
- `Paper_FirstLL\sections\robustness.tex`
- `Paper_FirstLL\sections\appendix.tex`
- `quality_reports\firstll_editorial_synthesis_R3.md`

## Phase 1: Claim Identification

- **Designs:** (1) Randomized lottery within top-4 GS pool + within-event FE (primary GS), (2) Control-function / peer-selection probability (non-GS), (3) event study for dynamics.
- **Estimand:** ATT at horizons h ∈ {4, 8, 12, 26, 52}; ATT = ATE within experimental population under lottery.
- **Treatment:** First-ever LL slot at focal event (D_ie = 1).
- **Control:** LL-naive top-4 (GS) or logit-eligible (non-GS) final-round qualifying losers not selected.
- **Outcomes:** Ranking points, Elo, main-draw entries, matches at 250+, prize money.

## Phase 2: Core Design Validity

### Design A — GS Lottery (primary)

**Assessment: SOUND WITH RESIDUAL CONTAMINATION**

The identifying assumption E[Y(0)|D=1,e] = E[Y(0)|D=0,e] holds only for pre-qualifying-withdrawal events; post-qualifying withdrawals use the ranking rule. The paper is honest about this and the verified-lottery subsample plus news verification are the right remedies. Balance tests are largely acceptable (WTA within-event F p = 0.516; ATP p = 0.046 with age as the driver — plausibly chance under seven covariates × two tours).

**Sanity check on dynamics:** ATP verified subsample: 33.2 → 51.2 → 76.6 → 12.3 at 4/12/26/52 weeks — coherent inverted-U consistent with ranking-point half-life. WTA: 60.8 → 77.5 (strong at 8w) — magnitude plausible relative to ~150 pt round-of-128 win. Signs and magnitudes pass basic economic-intuition tests. No suspicious pre-trend (event studies referenced but not re-inspected in this pass).

### Design B — Non-GS Control Function

**Assessment: SOUND (as corroboration only)**

The paper correctly concedes in §7.6 that ATP CF fails the falsification test (ρ_4 = 90.6, p < 0.01) and treats the CF as corroboration rather than free-standing identification. This is the right framing. WTA CF passes. Combinatorial enumeration of the selection probability is well-executed and the conditional-independence justification (line 74) is defensible.

### Phase 2 issue: Verified subsample rule vs. news verification

The core question: **does the event-level rule "event verified if any treated LL has rank > 1" produce a clean lottery sample?**

Not entirely. The rule identifies events where at least one slot was lottery-assigned (implying at least one pre-qualifying withdrawal). But in multi-slot verified events, the remaining slots may have been filled by ranking rule if additional withdrawals came post-qualifying. Concretely:
- 35 verified ATP events, 55 treated → ~1.57 treated per event.
- If each event has exactly one rank>1 LL, ~20/55 ≈ 36% of the treated in the "verified" subsample are rank-1 observations.
- The news check on 23 rank-1 entries found 10/23 confirmed ranking, 0/23 confirmed lottery, 13/23 unresolved. Under the paper's own extrapolation ("most unresolved are also ranking assignments"), rank-1 treated observations in verified events remain contaminated.

The 26-week ATP verified estimate (76.6) matching the headline (72.6) is offered as evidence that contamination doesn't matter. But contamination is signed **upward** (ranking rule promotes higher-skill players). Under upward bias in the headline, the truly-cleaner sample should give a **lower** point estimate. Near-equality is not consistent with the paper's stated bias direction unless contamination is quantitatively immaterial. A cleaner robustness check would drop rank-1 treated observations from the verified events and re-estimate. This is not done.

## Phase 3: Inference

- **Cluster-robust SEs at player level:** correctly implemented across the dynamic model, dose, heterogeneity, marginal impact, verified subsample, delta model, and match-level tournament performance. Consistent throughout.
- **Delta cluster-robust update:** honest handling. p 0.048 → 0.17 correctly triggered softening of the overseeding claim to "consistent with… though not significant." Good.
- **Fisher randomization:** Effective N drop from 120 to 103 (~14% attrition from singleton-block exclusion). Explained in table note (per R3 fix) but a comparable disclosure for the 26w Fisher p (the R3 abstract concern) and the direction of any bias from singleton exclusion would strengthen inference.
- **BH correction:** applied to 4 outcomes × 4 samples × 5 horizons = 80 tests. Fine.
- **CF validation table:** rho_4 = 90.6, p < 0.01. The note doesn't say SEs are player-clustered; if they aren't, the ATP-CF-fails claim might be less sharp (though 90.6 is such a large statistic that clustering is unlikely to save it). Disclose SE type.
- **Post-cluster-robust re-check on other claims:** the paper needs a brief statement confirming that dose interactions, heterogeneity by age/rank, and marginal-impact Wald tests (F = 3.87 / 3.05) all use player-clustered SEs and that no *other* previously-significant claim moved out of significance under the upgrade. The delta softening is a shot across the bow that invites this audit.

## Phase 4: Polish & Completeness

- **Mechanical decomposition (§7.2):** ratios 2.17, 2.19, 2.07 on ATP support the "~2×" claim numerically. But the ~75-pt gap between β^pool and the treated-share-weighted average of stratum coefficients is under-explained. The paper attributes it to "small n=1 and n=2+ cells produce imprecise negative estimates." The deeper reason is that β^pool is a *variance-weighted* (Frisch-Waugh) average of stratum effects with weights determined by within-stratum variation in D | Z, while the decomposition uses treated-share weights; when strata have different Z distributions and different residual variances, these weights genuinely differ even in population. This distinction should be made explicit rather than framed as noise, because a reader will ask whether the 75-pt gap indicates a specification problem. It doesn't, but the paper leaves the wrong impression.
- **WTA ratios < 1:** honestly acknowledged as an asymmetry between tours.
- **Wildcards:** now shown with numbers (0.08 vs 0.09 ATP, 0.12 vs 0.11 WTA) — good.
- **Overseeding claim after delta softening (§7 non-verified):** paper still uses the Elo −44.2 (p = 0.098) at 52w in the verified subsample to reinforce the overseeding channel. Given delta ATP p = 0.17 and 52w Elo p ≈ 0.10, the combined evidence is suggestive rather than conclusive — the mechanism paragraphs elsewhere should reflect this hedge consistently.

## Deduction Table

| # | Issue | Deduction |
|---|---|---|
| 1 | Verified subsample contains rank-1 treated observations that the news check indicates are typically ranking-assigned; ~36% of treated in verified sample are potentially contaminated; stricter subsample (drop rank-1 treated) not reported | −5 |
| 2 | Near-equality of verified (76.6) vs headline (72.6) ATP 26w estimates offered as corroboration, but under the paper's own upward-bias direction, verified should be lower; interpretation glosses this tension | −2 |
| 3 | Mechanical decomposition explanation of 75-pt gap conflates finite-sample noise with the variance-weighted-vs-treated-share-weighted distinction; would confuse a careful referee | −2 |
| 4 | No documented post-cluster-robust audit of previously-significant heterogeneity/dose/marginal-impact claims after the delta softening (0.048 → 0.17) exposed SE-sensitivity | −2 |
| 5 | CF validation table (tab:cf_validation) note does not state SE type; ATP failure claim rests on that inference | −1 |
| 6 | Fisher N 120→103 attrition disclosed only in table note; direction of any singleton-block bias not discussed | −1 |
| 7 | SUTVA / cross-treated interaction (e.g., LL winner facing eligible-but-untreated player later) not explicitly discussed; probably minor in tennis but should be stated | −1 |
| **Total** | | **−14** |

## Score: 86 / 100

## Verdict: **Commit-ready** (just below PR threshold of 90)

The paper has genuinely progressed from R3 (82) and the identification is defensible. Score sits in Commit range because the verified-subsample contamination question and the mechanical-decomposition framing are the kind of items a careful top-field referee will push back on. None require rebuilding the design; each is 1–2 hours of analysis + writing.

## Blocking Issues (must fix before submission)

1. **Report a stricter verified subsample** that drops rank-1 treated observations from verified events (or, equivalently, restrict to events with ≥2 rank>1 treated). Present alongside the current F14 estimates. If the point estimates fall meaningfully, the paper needs to acknowledge upward contamination bias in the headline; if they hold, it strengthens the causal claim.
2. **Reconcile the "verified ≈ headline" pattern** with the stated upward direction of contamination bias. Either (a) argue why contamination is small (e.g., how many rank-1 treated are in verified events), or (b) treat the verified estimate as a lower bound.
3. **Rewrite the §7.2 explanation of the 75-pt gap** to distinguish (i) LIE equivalence conditions (identical X distribution, no interaction) from (ii) the actual mechanical distinction between variance-weighted and treated-share-weighted averages. This turns an apparent red flag into a clean methodological footnote.
4. **Document a one-sentence audit** confirming that dose interactions, heterogeneity by age/rank, marginal-impact Wald tests, and all headline β_h estimates are estimated with player-clustered SEs and that no previously-reported significance level changed under the cluster-robust upgrade (aside from the disclosed delta). The delta softening invites this question and answering it preemptively is cheap.

## Non-Blocking Improvements

1. State SE type in the CF validation table note.
2. Add a sentence on singleton-block direction of Fisher bias, if any.
3. Explicitly state SUTVA/no-interference assumption for tennis: the fact that some treated and control players may face each other in future matches, and why this induces only bounded contamination.
4. In §7.3, quantify how many rank-1 treated appear in the verified subsample (this is knowable from the data and directly answers the residual-contamination question).
5. Consider a two-panel event-study plot with (a) headline and (b) verified subsample side by side; visual similarity is more persuasive than a paragraph.

## Positive Findings

1. The R4 upgrades (cluster-robust delta, mechanical decomposition, expanded F14, news check) directly address R3 referee concerns and the paper handled the delta p-value shift honestly rather than defensively — this is the right scientific behavior.
2. The design layering (lottery → verified subsample → news verification → CF as corroboration) is a genuinely thoughtful triangulation. The ATP CF failure is disclosed rather than buried.
3. Balance, wildcard robustness, power calculations, BH correction, and Fisher inference are all present and correctly framed — the paper does not rely on a single specification.

**Final score:** 86 / 100
**Verdict:** Commit-ready
