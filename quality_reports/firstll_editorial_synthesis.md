# Editorial Synthesis: First-LL Paper Peer Review

**Date:** 2026-04-16
**Paper:** The First Break: Career Effects of Initial Access Shocks in Tournament Labor Markets

---

## Scores

| | Domain Referee | Methods Referee | Average |
|---|---|---|---|
| **Score** | **68** | **72** | **70** |
| **Recommendation** | Major Revisions | Major Revisions | |

## Dimension Breakdown

| Dimension | Domain | Methods |
|---|---|---|
| Contribution/Identification | 62 | 72 |
| Literature/Estimation | 72 | 75 |
| Substance/Inference | 70 | 65 |
| External validity/Robustness | 60 | 78 |
| Journal fit/Replication | 72 | 70 |

---

## Convergent Concerns (Both Referees Flag)

These are the highest-priority items — both referees independently identified them:

### 1. Conclusion reports FULL-sample results, not first-LL results
- **Domain:** "The conclusion reports results from the FULL sample, not the first-LL sample. This is a serious inconsistency."
- **Methods:** "The conclusion's first paragraph reports N=248 ATP... contradicting the paper's title and stated contribution."
- **Fix:** Straightforward — rewrite conclusion with first-LL numbers. LOW effort, HIGH impact.

### 2. CF validation table is problematic
- **Domain:** Not flagged (did not examine table closely).
- **Methods:** "The CF validation table is a RED FLAG. Rho is -950 (significant) on the randomized sample. This is the opposite of validation."
- **Fix:** Diagnose why Pi estimated on non-GS qualifying matches produces nonsensical rho on GS sample. Likely: Pi = c_e/N_e (uniform) is misspecified for the lottery context. May need to recompute Pi specifically for GS events using the correct lottery probability. MEDIUM effort.

### 3. Fisher p-values diverge from clustered SE p-values
- **Methods:** "ATP ranking points at 26w: Fisher p = 0.164 vs. clustered p = 0.047. This undermines the headline claim."
- **Domain:** Not flagged but relevant.
- **Fix:** Report Fisher as primary inference for GS. If the 26w result doesn't survive Fisher, reframe the finding around 4-12w (which do survive: Fisher p = 0.000, 0.002, 0.028). MEDIUM effort.

### 4. Balance is marginal for first-LL
- **Domain:** Not flagged as major.
- **Methods:** "Individual-level: ATP age p=0.034, WTA ranking points p=0.007. Within-pool balance tests needed."
- **Fix:** Add within-event balance tests (regress covariates on D with event FE). MEDIUM effort.

### 5. 200 bootstrap reps insufficient
- **Domain:** "200 bootstrap replications is low. Standard practice is 500-1000."
- **Methods:** Same concern.
- **Fix:** Increase to 500. LOW effort (just change N_BOOT and re-run F04).

### 6. Non-GS sample sizes inconsistent between sections
- **Domain:** "Section 4.5 states N=2,708 ATP but Section 3.4 reports N=2,054."
- **Methods:** Same observation.
- **Fix:** Audit all sample size references. The empirical strategy section still has old-paper numbers. LOW effort.

---

## Domain-Only Concerns

### 7. "First break" may be mechanical (diminishing marginal returns to ranking points)
- The first LL occurs at the lowest baseline ranking, so the marginal value of additional points is mechanically largest. The marginal impact analysis (Section 7.1) does not control for baseline ranking across experience groups.
- **Fix:** Add baseline ranking as a control in the marginal impact regressions, or stratify within narrow ranking bands. MEDIUM effort.

### 8. 52-week fadeout undermines initial conditions framing
- Effects disappear by 52 weeks. Oyer (2006) and Oreopoulos (2012) find decade-long persistence.
- **Fix:** Either (a) measure longer-run outcomes (career length, peak ranking, prize money), or (b) reframe as "temporary access shocks in transparent markets" rather than "initial conditions." Option (b) is honest and defensible.

### 9. Missing earnings/prize money analysis
- Prize money data is publicly available. Even back-of-envelope calculations would ground the results.
- **Fix:** Compute prize money differential. MEDIUM effort.

### 10. WTA results are internally contradictory
- Elo gains (stacked model) but null match win probability (delta model). If Elo improves but win probability doesn't, what does that mean?
- **Fix:** Discuss explicitly. May reflect Elo's sensitivity to the schedule of opponents played rather than actual improvement.

### 11. Wildcards not discussed
- If control players receive wildcards, this contaminates the counterfactual.
- **Fix:** Add 1 paragraph discussing wildcards. LOW effort.

---

## Methods-Only Concerns

### 12. Event study figures lack pre-treatment periods
- Figures only show post-treatment horizons (4-52w). No visual evidence of pre-trends.
- **Fix:** Regenerate figures with -12w, -8w, -4w pre-treatment periods. MEDIUM effort (need pre-treatment outcome data in the skeleton).

### 13. Non-GS main draws at 4w is significantly NEGATIVE (-1.4)
- Treated players enter fewer main draws in the first 4 weeks. Counterintuitive.
- **Fix:** Discuss. Likely mechanical: the LL event itself occupies week 1-2, and the focal event's main draw appearance is counted in the immediate effects model, not the dynamic model. LOW effort to explain.

### 14. Multiple testing correction absent
- 100 hypothesis tests across 5 outcomes × 5 horizons × 4 samples. No BH/FWER correction.
- **Fix:** Add Romano-Wolf or BH q-values for the primary outcome (ranking points) across horizons. MEDIUM effort.

### 15. Estimand not clearly defined
- Is beta_h ATT or ITT? What are the weights across events in the stacked model?
- **Fix:** Add 1 paragraph to empirical strategy clarifying the estimand. LOW effort.

### 16. Marginal impact table has implausibly large n=1 estimates
- ATP second-LL ranking points at 4w: -284.2 (SE 32.4). Implausibly large.
- **Fix:** Investigate. May be a coding issue in F09 (the n=1 GS sample is tiny: N=48). Report with appropriate caveats about small-sample instability.

---

## Priority Ranking for Revision

| Priority | Issue | Effort | Both Referees? |
|---|---|---|---|
| **CRITICAL** | Conclusion uses wrong sample | Low | Yes |
| **CRITICAL** | CF validation table broken | Medium | Methods only but fatal if unresolved |
| **CRITICAL** | Fisher vs clustered-SE divergence at 26w | Medium | Methods primary |
| **HIGH** | Pre-treatment periods in event study | Medium | Methods only |
| **HIGH** | Within-event balance tests | Medium | Methods only |
| **HIGH** | Multiple testing correction | Medium | Methods only |
| **HIGH** | Marginal impact n=1 implausible estimates | Medium | Methods only |
| **MEDIUM** | Mechanical first-break argument | Medium | Domain only |
| **MEDIUM** | Reframe away from "initial conditions" (52w fade) | Low | Domain only |
| **MEDIUM** | Prize money analysis | Medium | Domain only |
| **MEDIUM** | Bootstrap reps 200→500 | Low | Both |
| **MEDIUM** | Sample size inconsistencies in text | Low | Both |
| **LOW** | Wildcards discussion | Low | Domain only |
| **LOW** | Negative 4w main draws explanation | Low | Methods only |
| **LOW** | Estimand clarification paragraph | Low | Methods only |
| **LOW** | WTA contradiction discussion | Low | Domain only |

---

## Editorial Decision: MAJOR REVISIONS

The paper presents a novel and well-executed analysis with a credible identification strategy. The marginal impact robustness design is a genuine methodological contribution. However, several issues prevent acceptance:

1. The CF validation failure is the most technically concerning issue — it must be diagnosed and resolved.
2. The Fisher inference at 26 weeks challenges the headline result. The honest finding may be significant at 4-12 weeks (which Fisher confirms) but not at 26 weeks.
3. The conclusion using wrong-sample numbers is an easy but important fix.
4. The framing needs recalibration: this is a paper about temporary access shocks in transparent markets, not about initial conditions in the Oyer/Oreopoulos sense.

All issues are addressable in one revision cycle. The path to acceptance requires: fix the CF validation, adopt Fisher as primary inference, add pre-treatment event study, reframe the contribution, and rewrite the conclusion.
