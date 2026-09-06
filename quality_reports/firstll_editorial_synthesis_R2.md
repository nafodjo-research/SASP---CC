# Editorial Synthesis: Round 2 Peer Review

**Date:** 2026-04-16
**Paper:** The First Break: Career Effects of Initial Access Shocks in Tournament Labor Markets

---

## Score Progression

| Round | Domain | Methods | Average |
|---|---|---|---|
| **R1** | 68 | 72 | 70 |
| **R2** | **76** | **77** | **76.5** |
| **Change** | +8 | +5 | +6.5 |

Both referees: **Major Revisions** — but both note the paper is now closer to Minor.

## R2 Dimension Scores

| Dimension | Domain | Methods |
|---|---|---|
| Contribution/Identification | 76 | 82 |
| Literature/Estimation | 80 | 72 |
| Substance/Inference | 72 | 76 |
| External/Robustness | 74 | 78 |
| Journal/Replication | 78 | 80 |

## R1 → R2 Resolution Assessment

| R1 Concern | R2 Status | Both Referees? |
|---|---|---|
| Conclusion used full-sample numbers | **Resolved** | Both confirm |
| Fisher inference divergence at 26w | **Addressed** (reported, discussed) | Both confirm |
| Pre-treatment event study | **Partially** (figures have h=0 anchor, no leads table) | Methods flags |
| Within-event balance | **Resolved** | Both confirm |
| Estimand clarity | **Resolved** | Methods confirms |
| Negative 4w main draws | **Resolved** | Methods confirms |
| Multiple testing (BH) | **Addressed** (primary only, not all outcomes) | Methods wants broader |
| 200 bootstrap reps | **NOT addressed** | Methods still flags |
| Sample size inconsistencies | **Resolved** | Both confirm |
| Marginal impact n=1 implausible | **NOT addressed** | Methods flags harder |
| CF validation | **WORSE** (ATP now shows +CF ≠ baseline; ρ row blank) | Methods flags as #1 issue |
| Fadeout framing | **Resolved** (reframed as market feature) | Domain confirms |
| Prize money | **NOT addressed** (listed as limitation only) | Domain flags as biggest remaining issue |
| WTA contradiction | **Partially** (discussed, not resolved) | Both flag |
| Wildcards | **Addressed but thin** (no table, "verify" asserted) | Both flag |

## Critical Unresolved Issues (Both Referees Converge)

### 1. CF Validation Table is Broken (Methods: #1 priority)
- **Problem**: Table shows "+CF" ATP ranking point coefficients of -106 to -128 vs. baseline of +72 to +82. The $\hat{\rho}_h$ row is empty. On a randomized sample with correctly specified uniform P_i, ρ should be ≈ 0 and "+CF" should match baseline.
- **Methods**: "This is not a minor check. If CF fails its own validation on the randomized benchmark, non-GS CF estimates are on thin ice."
- **Fix**: The bug is in F11 — the regex for extracting ρ rows looks for `"v_hat_gs:horizon"` but the variable was renamed. Need to re-run with correct extraction AND investigate why ATP instability actually exists (likely: uniform P_i isn't right when events have wildly different $c_e/N_e$ ratios, e.g., small Slam qualifying draws vs. large ones).

### 2. Marginal Impact n=1 Table is Numerically Impossible (Methods: #2 priority)
- **Problem**: ATP second-LL ranking points at 4w: -284.2 ± 32.4. Given the maximum physical dose at a Grand Slam is +180 points, a negative effect of 300+ cannot be correct.
- **Methods**: "This either (a) reflects severe conditioning-on-future-outcomes through prior-LL controls, (b) reflects drastically different pre-treatment pool when n=1, or (c) is a bug. *This number cannot be published as-is.*"
- **Fix**: The F09 script includes `n_prior_gs_ll_won` and `n_prior_nongs_ll_won` in ZPRE_FULL — but these are collinear with the `n_prior_ll` strata (zero within n=0 strata, constant within n=1 strata). Re-estimate F09 with prior-LL controls DROPPED within each stratum.

### 3. Prize Money Missing (Domain: #1 priority)
- **Problem**: Every outcome is non-monetary (ranking points, Elo, matches). For a labor economics journal, this is binding.
- **Domain**: "Listing this as 'future work' is the weakest move the paper makes."
- **Fix**: Back-of-envelope using publicly available prize money tables. Grand Slam first-round loser prize is ~$70K USD; this × treatment effect on main draws = dollar impact.

## Other Items

### 4. Fisher N drops from 120 to 103 (ATP)
- **Methods**: Permutation drops singleton blocks. Needs explicit rule statement and a check that singleton-block dropping isn't what kills 26w significance.

### 5. WTA individual imbalance vs joint F
- **Methods**: Three individual p < 0.05 but joint F = 0.063. Report exact F and df; consider entropy balancing.

### 6. Verified lottery subsample "available on request"
- **Both**: Not acceptable. Put in appendix.

### 7. Bootstrap at 200 reps
- **Methods**: Still unresolved from R1. Monte Carlo SE of p-value near 0.05 is ±0.015.

### 8. Own the mechanical part of first-break claim
- **Domain**: Calibrate a "purely mechanical" benchmark (constant per-match points, differential stock) and show the observed first-vs-second gap exceeds it.

## Abstract Inconsistencies (Minor but Visible)

- Abstract says effects "peak at 26 weeks" — data shows peak at 12 weeks (+82.8 vs +72.6 at 26w).
- Abstract leads with 26w effect (clustered p=0.047) but Fisher shows p=0.164 at 26w.
- WTA "concentrate at 8 weeks" overstates the single significant horizon.

---

## Editorial Decision: MAJOR REVISIONS (leaning toward Minor)

The paper has made substantial, honest progress. Both referees recognize genuine improvements (conclusion fixed, marginal impact design added, Fisher inference added, within-event balance added, prose tightened).

However, three items stand out as blocking:

1. **CF validation table is broken** (Methods: "single biggest red flag"). Must be diagnosed and either fixed or honestly reframed as a failed test that limits the CF's applicability.
2. **Marginal impact n=1 table has numerically impossible values**. Must be re-estimated without collinear prior-LL controls.
3. **Prize money analysis** (Domain: "binding constraint for a labor economics journal"). Must be delivered, not promised.

If these three are addressed, the paper clears 85/100 and moves to Minor Revisions at JHR/JLE/Labour Economics. The path to acceptance is clear and feasible in one more revision cycle.

### Recommended Additional (Smaller) Fixes
- Increase bootstrap to 2,000 reps
- Report verified-lottery subsample table in appendix
- Add pre-treatment lead coefficients (numerical, not just visual)
- Abstract consistency (peak at 12w not 26w; hedge 26w with Fisher)
- WTA balance joint F diagnostic (Hotelling T²)
- Calibration benchmark for the first-break claim
- BH correction across all outcomes, not just ranking points
