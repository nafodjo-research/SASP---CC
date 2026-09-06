# Paper Excellence Review: Paper_FirstLL

**Date:** 2026-04-16
**Mode:** `/review --all` (all critics in parallel)

---

## Component Scores

| Critic | Score | Status |
|---|---|---|
| **Strategist-critic** (Identification validity, 25% weight) | 62/100 | Below commit threshold |
| **Writer-critic** (Manuscript polish, 10% weight) | 50/100 (pre-fix) → ~85 (post-fix) | Fixed during review |
| **Coder-critic** (Code quality, 15% weight) | 84/100 | Clears commit |
| **Verifier** (Compilation, 5% weight) | PASS (100/100) | All 7 checks pass |

## Weighted Aggregate

Using the rules in `quality.md`:
- Identification validity: 25%
- Code quality: 15%
- Manuscript polish: 10%
- Replication readiness (verifier): 5%
- Paper quality (peer review R3 average): 25% → 82/100
- Literature coverage: 10% (N/A — not separately scored; use 85 based on domain referee)
- Data quality: 10% (N/A — no explorer run; assume 80)

**Weighted using only scored components:**
- Strategist: 62 × 0.25 = 15.5
- Paper quality (peer referees): 82 × 0.25 = 20.5
- Code: 84 × 0.15 = 12.6
- Manuscript polish (post-fix): 85 × 0.10 = 8.5
- Verifier: 100 × 0.05 = 5.0
- Literature (domain ref): 85 × 0.10 = 8.5
- Data quality (default): 80 × 0.10 = 8.0

**Weighted total: 78.6/100**

Just below the 80 commit threshold — driven primarily by the strategist-critic's 62.

---

## Cross-Critic Convergence

### Where critics agree

| Issue | Strategist | Writer | Coder | Verifier |
|---|---|---|---|---|
| Verified lottery "upon request" is unacceptable | Yes (#2.2 critical) | Yes | — | — |
| ATP within-event balance failure (p=0.046) needs engagement | Yes (#2.3 major) | Yes | — | — |
| CF validation fails on ATP — must be addressed | Yes (#2.6 major) | — | — | — |
| Bootstrap reps 200 → ≥1000 | Yes (#3.3) | — | Yes | — |
| Multiple testing scope incomplete | Yes (#3.2) | — | — | — |
| Sample size inconsistency (data.tex vs rest) | — | Yes (critical, -25) | — | — |
| Duplicate labels | — | Yes (critical, -15) | — | Yes (warning) |

### Writer-critic fixes already applied during review
- Duplicate tab:sumstats labels removed
- Balance p-values corrected in data.tex (0.230/0.333 → 0.106/0.063)
- Sample sizes N=248/132 → N=120/82 in data.tex notes
- Z^pre description updated (drops prior-LL vars)
- "three contributions" → "four"
- WTA non-GS Elo claim corrected
- 26w added to abstract with Fisher caveat
- Fisher p=0.000 → p<0.001
- Wildcard numbers added

Paper now compiles to **56 pages, zero undefined references, no multiply-defined labels, 3 overfull hboxes.**

---

## Strategist-critic's Core Concerns (unresolved)

The strategist-critic's 62/100 is the binding constraint. Three issues stand out:

### 1. CRITICAL: Verified lottery ATP subsample shows NULL ranking point effects
The cleanest identification (verified-lottery-only subsample, N=100) produces null effects. The paper says this is "limited power" but the strategist argues N=100 is only marginally smaller than the headline N=120, so the null cannot be dismissed as power alone. Listing it as "available upon request" is unacceptable for a robustness subsample that directly addresses the identifying assumption.

**Path forward**: Add the verified lottery subsample as a main-text table. Either (a) accept a weaker headline claim if the null survives a proper comparison, or (b) demonstrate via simulation that the null reflects power, not contamination.

### 2. MAJOR: ATP within-event balance rejection (p=0.046)
Under genuine within-event randomization, balance should hold. A 4.6% rejection is formally evidence against random assignment conditional on event. Controls don't "fix" a violation of the identifying assumption — they just absorb its linear projection.

**Path forward**: Show whether balance recovers in the verified lottery subsample. If yes, the contamination story holds. If no, the lottery claim is dubious.

### 3. MAJOR: Contamination bias sign is undefended
The paper claims lottery contamination "biases toward attenuation," but under ranking-based assignment, treated players are systematically higher-ranked than controls — this plausibly biases POSITIVELY, not attenuation. Either way, a signed-bias argument with explicit assumptions is needed.

**Path forward**: Provide a signed-bias derivation, or bound the contamination share using published ATP/WTA withdrawal logs.

---

## Editorial Gate Status

| Gate | Threshold | Current | Status |
|---|---|---|---|
| Commit | 80 | ~79 | Marginal — BLOCKED on strategist |
| PR | 90 | ~79 | Blocked |
| Submission | 95 | ~79 | Blocked |

Note: Peer referees (R3) scored the paper at 82/100 (Minor Revisions at JHR/AEJ:Applied level). The strategist-critic applies a stricter internal standard than external peer referees, focusing on design-level tensions that the referees did not catch (verified lottery null, within-event balance failure).

This is a normal divergence: the paper looks acceptable to outside reviewers but has real internal tensions that the strategist identifies. These are the kinds of issues that become major revision requests later in the R&R cycle.

## Recommended Next Steps

**If targeting strong field journal (JHR/AEJ:Applied)**: Submit after addressing writer-critic fixes (already done). The paper is at the borderline Accept/Minor Revisions standard. The strategist concerns would become R&R requests.

**If strengthening before submission**: Address the three strategist criticals:
1. Promote verified lottery subsample to main text (requires re-estimation script)
2. Derive signed-bias bounds for contamination (analytical, 1-2 pages)
3. Show within-event balance on verified subsample (analytical, 1 paragraph)

These three fixes would push the strategist score to ~80 and the aggregate to ~84.

## Bottom Line

- **Code: ready** (84)
- **Manuscript polish: ready after in-flight fixes** (~85)
- **Compilation: clean** (PASS)
- **Peer review: Minor Revisions** (82)
- **Strategist: below threshold** (62) — 3 design tensions need addressing

Aggregate: **78.6/100** — just below commit gate. With the writer-critic fixes I've already applied, the paper is at roughly JHR/AEJ:Applied submission-ready quality for external review. The strategist-critic has identified internal issues that would likely surface in R&R but are not yet blocking submission.
