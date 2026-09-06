# Editorial Synthesis: Round 3 Peer Review

**Date:** 2026-04-16
**Paper:** The First Break: Career Effects of Initial Access Shocks in Tournament Labor Markets

---

## Score Progression Across Rounds

| Round | Domain | Methods | Average | Recommendation |
|---|---|---|---|---|
| R1 | 68 | 72 | 70 | Major Revisions |
| R2 | 76 | 77 | 76.5 | Major Revisions |
| **R3** | **82** | **82** | **82** | **Minor Revisions** |

**Both referees converge at 82/100 and independently recommend Minor Revisions.**

## R3 Dimension Scores

| Dimension | Domain | Methods |
|---|---|---|
| Contribution/Identification | 82 | 85 |
| Literature/Estimation | 85 | 82 |
| Substance/Inference | 80 | 75 |
| External/Robustness | 78 | 82 |
| Journal/Replication | 85 | 85 |

## R2 → R3 Resolution Assessment

### Resolved (both referees confirm)

1. **CF validation table** — rho now populated. Methods: "honest failure disclosure is an improvement." Paper explicitly reports ATP CF fails while WTA CF passes, and correctly frames this as evidence that the lottery (not the CF) is the primary identification strategy.

2. **Marginal impact n=1** — re-estimated without collinear controls. Methods: "Re-estimation improved this from R2 value (-284) ... The paper now openly says 'sometimes larger in absolute value than the physically possible treatment dose.' This is the right disclosure."

3. **Prize money analysis** — new section and appendix table. Domain: "The new prize money translation is the single most important addition — it converts a paper about ranking points into a paper about labor market dollars."

### Partially resolved

4. **WTA contradiction** — Methods satisfied with new discussion. Domain wants a concrete test (condition Elo on opponent tier).

5. **Mechanical first-break framing** — Domain notes the 2x language is now in the intro but calls for a formal decomposition showing $\beta^{pool}$ = weighted average of $\beta^{n=0}$, $\beta^{n=1}$, $\beta^{n\geq 2}$.

6. **Overseeding finding elevated** — Domain wants richer treatment (round-by-round, mechanism).

### Still outstanding (minor)

7. **Wildcards "show don't tell"** — Domain flags this unresolved. Needs a one-row table or explicit numbers.

8. **Bootstrap 200 reps** — Methods persists. Standard practice is 2,000.

9. **Verified lottery "upon request"** — Both referees: put in appendix.

10. **BH correction limited to ranking points** — Methods wants it extended to all primary outcomes.

11. **Fisher N drop 120→103** — Methods: needs one-sentence explanation of singleton-block exclusion.

12. **Abstract: 26w clustered vs Fisher discrepancy not disclosed** — Domain: move to abstract.

13. **"Three contributions" says four** — counting inconsistency.

## Editorial Decision: MINOR REVISIONS

Both referees independently reach the same score (82) and same recommendation (Minor Revisions), with the methods referee explicitly noting "leaning toward Accept." The paper has crossed the acceptance threshold for strong field journals (JHR, AEJ:Applied).

## Path to Acceptance

**Fast fixes (low effort, high impact)**:
- Wildcards: produce a one-row table with treated/control wildcard rates (1 R script, 1 sentence)
- Fisher N explanation: 1 sentence in appendix
- Contribution count: change "three" → "four"
- Abstract: add Fisher p=0.164 at 26w disclosure
- Verified lottery subsample: move from "upon request" to appendix table

**Medium effort fixes**:
- Bootstrap 200 → 2,000: re-run F04 delta model (~20 min compute)
- BH correction extended to Elo, main draws, matches 250+ (modify F11)
- Mechanical decomposition of first-break claim: compute $w_0\beta^{n=0} + w_1\beta^{n=1} + w_{\geq 2}\beta^{n\geq 2}$ vs $\beta^{pool}$ (one table)
- Overseeding by round: round-level delta estimation

**Harder but not required for Minor Revisions**:
- Domain: richer external validity discussion
- Methods: Newey/Klein-Spady semi-parametric CF
- McCrary-style density test for P^LL

## Score Trajectory Summary

```
R1: 70/100 ──→ R2: 76.5/100 ──→ R3: 82/100
             +6.5                 +5.5
     Major              Major              Minor
```

The paper has moved from "needs substantial work" to "acceptable pending polish" in three revision cycles. All three R2 blocking issues (CF, marginal impact, prize money) are resolved. The remaining items are polish, not substance.

**Verdict: Ready for journal submission after one final polish pass.**
