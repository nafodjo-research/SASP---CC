# Identification Strategy Memo: Lucky Losers in Professional Tennis

**Date:** 2026-03-20
**Research Question:** What is the causal impact of Lucky Loser (LL) entry into a professional tennis main draw on match performance, ranking trajectory, tournament access, and career outcomes?
**Target Journals:** AEJ:Applied (primary), JLE (secondary), RESTAT, JSE
**Phase:** Strategy (medium severity)

---

## 1. The Ideal Experiment

The ideal experiment randomly assigns main draw entry to a pool of equally skilled players who just failed to qualify. Half receive the opportunity to compete; half do not. We then track ranking, earnings, and career persistence over 1-3 years.

**How far is the data from the ideal?**

Remarkably close in two distinct ways:

1. **Grand Slam Lottery (2006-present):** The top-4 ranked final-round qualifying losers are placed in a random draw for LL slots. This IS the ideal experiment -- literal randomization among near-identical players -- but with small samples (~304 pool-observations over 19 years).

2. **ATP Ranking-Based Cutoff (2007-present):** At non-Grand-Slam events, LL slots go to the highest-ranked final-round qualifying loser. Among the pool of 4-16 players who all lost in the same round of qualifying, ranking creates a sharp cutoff determining who receives the opportunity. This is a regression discontinuity design with a larger sample (~6,000 final-round losers) but a discrete running variable.

The dual-design structure is a major strength: the lottery provides clean identification to validate the RDD, while the RDD provides statistical power.

---

## 2. Primary Strategy: RDD at the Qualifying Loser Ranking Cutoff (ATP Events)

### 2.1 Design

**Type:** Sharp Regression Discontinuity Design (RDD)

**Estimand:** Local Average Treatment Effect at the cutoff (LATE). Specifically, the causal effect of receiving a main draw LL entry on outcomes for players whose ranking among final-round qualifying losers is at the margin of the LL cutoff. This is a LATE for marginal qualifying losers -- those who are borderline LL recipients.

**Treatment definition:** Binary indicator $D_i = 1$ if player $i$ enters the main draw as a Lucky Loser at tournament $t$. Operationally: player appears in the main draw match file with `winner_entry = "LL"` or `loser_entry = "LL"` for that tournament.

**Running variable:** $R_{it}$ = player $i$'s ranking position among all final-round qualifying losers at tournament $t$, where 1 = highest-ranked loser (best position). This is constructed by:
1. Identifying all players who lost in the final qualifying round at tournament $t$
2. Sorting by ATP ranking (ascending: rank 1 is best)
3. Assigning position 1, 2, ..., $N_t$ where $N_t$ is the number of final-round losers

**Cutoff:** $c_t$ = number of LL slots at tournament $t$ (= number of main draw withdrawals filled by LLs). Treatment is assigned when $R_{it} \leq c_t$. The cutoff varies by tournament.

**Why sharp:** At non-Grand-Slam ATP events, LL selection follows strict ranking order. The highest-ranked final-round loser gets the first LL spot, the second-highest gets the second, and so on. Conditional on $c_t$ withdrawals, ranking perfectly determines LL status. This is sharp: $D_i = \mathbf{1}[R_{it} \leq c_t]$.

**Normalization:** Since the cutoff varies by tournament, normalize the running variable as:

$$\tilde{R}_{it} = R_{it} - c_t$$

so that the cutoff is at zero for all tournaments. Players with $\tilde{R}_{it} \leq 0$ are treated; those with $\tilde{R}_{it} > 0$ are controls.

### 2.2 Control Group

The control group consists of final-round qualifying losers at the same tournament whose ranking position is just above the LL cutoff (i.e., $\tilde{R}_{it} = 1, 2, 3, ...$). These players:
- Lost in the same round of qualifying
- Were competing for the same tournament entry
- Had similar (but slightly lower) ATP rankings
- Were NOT offered main draw entry

**Why them:** They are the closest counterfactual to LL recipients. They share the same qualifying draw, same surface, same week, similar skill level. The only difference is a ranking position that placed them on the wrong side of the withdrawal-driven cutoff.

### 2.3 Key Assumptions

**Assumption 1: Continuity of potential outcomes at the cutoff.** $E[Y(0) | R = r]$ and $E[Y(1) | R = r]$ are continuous in $r$ at $r = c_t$. This means that absent LL entry, players just above and below the cutoff would have similar expected outcomes. Plausible because ranking among qualifying losers is a noisy measure of ability, and the small ranking differences near the cutoff (e.g., ranked 150 vs. 155 in ATP rankings) reflect minimal skill differences.

**Assumption 2: No manipulation of the running variable.** Players cannot precisely sort their ranking position to be on one side of the cutoff. This is strongly credible because: (a) the cutoff depends on the number of main draw withdrawals, which is determined by OTHER players' injuries/decisions and is unknown to qualifying losers at the time of their qualifying match; (b) the ranking is determined by past results accumulated over 52 weeks, which cannot be manipulated in the moment; (c) a player cannot "choose" to be the 2nd-highest-ranked loser rather than the 3rd-highest.

**Assumption 3: No other treatment at the cutoff.** There is no other benefit or penalty that discontinuously changes at the LL ranking cutoff. This appears to hold: the only consequence of having a higher ranking among qualifying losers is LL eligibility.

**Assumption 4 (for discrete RV): Local randomization.** Given the discrete nature of the running variable, an alternative framework assumes that within a narrow window, treatment assignment is as-if random. This is supported by the fact that the cutoff ($c_t$) is determined by withdrawals exogenous to qualifying loser characteristics.

### 2.4 Handling the Discrete Running Variable

The running variable $\tilde{R}_{it}$ takes integer values with a small number of mass points per tournament (4-16 final-round losers). This is a well-known challenge for standard RDD inference.

**Primary approach: Local randomization framework (Cattaneo, Idrobo, and Titiunik 2020, 2024).** Rather than fitting local polynomials, this framework treats the running variable window as a local experiment. Within a window $[c_t - w, c_t + w]$, treatment is assumed as-if randomly assigned. This is especially natural here because: the cutoff depends on exogenous withdrawals, and ranking differences of 1-2 positions among qualifying losers reflect near-identical ability.

Implementation: Use the `rdlocrand` package in R/Stata (Cattaneo, Frandsen, and Titiunik 2015). The key output is a p-value from a randomization inference test (Fisher exact test or related permutation test) rather than a conventional t-statistic.

**Robustness approach 1: Continuity-based RDD with honest CIs (Kolesar and Rothe 2018).** Use the `RDHonest` package in R to compute confidence intervals that have guaranteed coverage under bounded curvature of the conditional expectation function. This addresses the Lee and Card (2008) concern that conventional RDD SEs overstate precision when the running variable is discrete.

**Robustness approach 2: Pooled RDD with clustering.** Pool all tournament-level observations, use `rdrobust` with bias-corrected CIs (Calonico, Cattaneo, and Titiunik 2014), and cluster standard errors at the tournament level. Report these alongside the local randomization and honest CI results to show stability across frameworks.

### 2.5 The Endogenous Cutoff Problem

The number of LL slots ($c_t$) at each tournament is determined by how many main draw players withdraw. This creates two concerns:

**Concern A: Selection into the analysis sample.** Tournaments with zero withdrawals contribute no treatment variation and are excluded. The analysis is conditional on at least one withdrawal occurring. This is analogous to lottery designs conditioning on a lottery being held. The estimated effect is the LATE for players at tournaments where at least one withdrawal occurs (the vast majority of tournaments).

**Concern B: Could withdrawal patterns be correlated with qualifying loser quality?** If strong main draw players withdraw from tournaments that also attract strong qualifying losers, the pool characteristics vary with $c_t$. **Defense:** Main draw withdrawal decisions (typically injury, illness, schedule) are made BEFORE the qualifying draw is finalized and are orthogonal to qualifying loser rankings. Moreover, we condition on tournament fixed effects (or tournament-level characteristics) to absorb any tournament-level correlation.

**Treatment:** Include tournament-level controls (draw size, surface, tournament tier, year) and show robustness to excluding tournaments with unusually many or few withdrawals.

### 2.6 Outcomes (Ordered by Time Horizon)

**Immediate (at the tournament):**
- $Y_1$: Main draw match win indicator (won at least one main draw match)
- $Y_2$: Rounds advanced in main draw (coded numerically: R128=1, ..., F=7)
- $Y_3$: Ranking points earned at the tournament
- $Y_4$: Sets won / games won (continuous performance measures)

**Short-term (4-12 weeks post-tournament):**
- $Y_5$: Ranking change at $t+4$, $t+8$, $t+12$ weeks (ranking at follow-up minus ranking at event)
- $Y_6$: Points change at $t+4$, $t+8$, $t+12$ weeks
- $Y_7$: Number of subsequent main draw entries (direct acceptance, not via qualifying) in 4/8/12 weeks

**Medium-term (13-52 weeks post-tournament):**
- $Y_8$: Ranking change at $t+26$, $t+52$ weeks
- $Y_9$: Total main draw entries in next 6/12 months
- $Y_{10}$: Total match wins (main draw) in next 6/12 months
- $Y_{11}$: Imputed prize money earned in next 6/12 months

**Long-term (1-3 years post-tournament, exploratory):**
- $Y_{12}$: Ranking at $t+104$ weeks (2 years)
- $Y_{13}$: Career survival indicator (still active on tour 1/2/3 years later)
- $Y_{14}$: Cumulative main draw entries over 1/2/3 years

**Competitiveness outcomes (novel contribution):**
- $C_{it}$: Competitiveness index $\mathbb{E}_j[P_t(i \to j \mid X_{it}, X_{jt}, X_{ijt})]$ — the expected probability of beating a representative opponent, conditional on individual, opponent, and pairwise characteristics. Constructed from a first-stage match-outcome model (logit) estimated on all non-LL main draw matches. This separates ability gains from mechanical ranking point accumulation.
- $Elo_{it}$: Surface-specific Elo rating computed from match history using Sackmann's algorithm. Used as a robustness check for the competitiveness index (avoids generated regressor concerns).

**NOTE on generated regressor:** The competitiveness index is a predicted probability from a first-stage model. Standard errors in the RDD must account for first-stage estimation error via bootstrapping the two-stage procedure or Murphy-Topel correction. The match-outcome model should be estimated EXCLUDING LL main draw matches to avoid circularity.

**Mechanism-diagnostic outcomes:**
- $Y_{15}$: Quality of opponents faced in next 3 months (average opponent ranking)
- $Y_{16}$: Tournament tier of subsequent entries (Grand Slams, Masters, 500/250)

### 2.7 Testable Implications

1. **Balance test:** Pre-determined covariates (age, height, handedness, nationality, career stage, ranking entering qualifying, surface preference, recent form) should be smooth at the cutoff. Run the RDD on each covariate as the "outcome."

2. **Density test:** McCrary (2008) / Cattaneo, Jansson, and Ma (2020) test for bunching of the running variable at the cutoff. We do NOT expect bunching because players cannot manipulate their ranking position among qualifying losers relative to an unknown withdrawal count. Note: density tests have reduced power with discrete running variables -- report with appropriate caveats.

3. **Pre-trends / lagged outcomes:** The player's ranking trajectory in the 12-26 weeks BEFORE the qualifying event should show no discontinuity at the cutoff. This is a powerful placebo check.

4. **First stage verification:** Confirm that ranking position among qualifying losers perfectly predicts LL status (sharp RDD). Report the concordance rate. Any non-compliance (player ranked high enough but did NOT become LL) should be documented and the fuzzy RDD alternative considered.

### 2.8 Threats to Identification

**Threat 1: SUTVA violation / strategic spillovers.** The player just below the cutoff does not merely "fail to receive treatment" -- they lose a specific opportunity that was given to someone else. If the cutoff player's future outcomes are depressed by the psychological effect of being the "unlucky" non-LL, SUTVA is violated and the estimated effect is inflated (combines the gain to the treated and the loss to the control). **Mitigation:** (a) Compare controls to players further from the cutoff -- if the closest control has unusually bad outcomes, this suggests SUTVA violation. (b) Use the Grand Slam lottery as a check: there, all 4 pool members had equal probability, so the SUTVA concern is weaker (the psychological "near miss" effect is diffused across the pool). (c) Acknowledge this as a bound: the RDD estimate may be an upper bound on the treatment effect if SUTVA is violated in this direction.

**Threat 2: Endogenous number of LL slots.** Addressed above (Section 2.5). Main defense: withdrawals are driven by main draw player decisions orthogonal to qualifying loser characteristics.

**Threat 3: Measurement error in the running variable.** The ranking used for LL ordering may differ from the ranking in the data if: (a) the "live" ranking differs from the published weekly ranking; (b) ranking data is missing and imputed. **Mitigation:** Cross-validate with multiple ranking sources. Restrict sample to post-2011 where qualifying match data is most complete. Report first-stage concordance.

**Threat 4: Measurement error in the treatment variable.** The `entry` field may have some misclassification. Known direction: likely attenuates toward zero (some LLs coded as direct entrants). **Mitigation:** Cross-validate with Maity et al. (2025). Conduct sensitivity analysis restricting to years with highest data quality.

**Threat 5: Player declining the LL spot.** A player ranked high enough may decline (injury, fatigue). This converts the sharp RDD into a fuzzy RDD. **Mitigation:** (a) Test the first-stage concordance rate; if >95%, treat as sharp. (b) Report fuzzy RDD estimates as robustness. (c) In the local randomization framework, this is non-compliance handled via IV.

**Threat 6: Multiple LL slots.** When multiple withdrawals occur, multiple LLs enter. The "next player in line" faces a different counterfactual depending on how many slots open. **Mitigation:** Normalize the running variable relative to the realized cutoff. Alternatively, estimate separately for tournaments with exactly 1 LL slot (cleanest comparison) and show results are similar when including multiple-slot tournaments.

---

## 3. Secondary Strategy: Grand Slam Lottery Design

### 3.1 Design

**Type:** Natural experiment / lottery design (equivalent to an RCT with non-compliance)

**Estimand:**
- **ITT:** Effect of being in the top-4 eligible pool on outcomes (intent-to-treat among all pool members, regardless of whether they actually received LL entry). This is well-defined and avoids selection from declining.
- **LATE (via IV):** Effect of actual LL entry, instrumenting with pool membership. Instrument = in the top-4 pool; treatment = actually entered the main draw as LL. Relevance: pool membership strongly predicts LL entry. Exclusion: being in the top-4 pool affects outcomes only through the possibility of LL entry (no direct effect of being designated "eligible" per se).

**Treatment definition:** $D_i = 1$ if player $i$ was selected in the random draw and entered the Grand Slam main draw as LL.

**Pool definition:** At each Grand Slam (2006-present), the pool of eligible LL candidates among the 16 final-round qualifying losers (Q3 losers). **IMPORTANT NUANCE:** The pool size and selection mechanism depend on withdrawal TIMING:
- Withdrawal AFTER qualifying completes → highest-ranked loser gets the spot (NO lottery, ranking-based)
- 1 withdrawal BEFORE qualifying completes → top-2 ranked losers enter random draw for 1 spot
- 2+ withdrawals BEFORE qualifying completes → top-3-4 ranked losers enter random draw

This means the clean lottery design applies ONLY when withdrawals occur before qualifying is complete. The analyst must determine withdrawal timing for each Grand Slam LL entry, which is NOT directly available in the Sackmann data and requires supplementary research (news reports, draw sheets). The effective lottery sample is SMALLER than the naive estimate of ~304 pool observations.

**Control groups (two):**
1. **Within-pool controls:** Top-4 ranked Q3 losers who were NOT selected in the random draw. These are the purest controls -- same eligibility, same tournament, similar ranking.
2. **Outside-pool controls:** Q3 losers ranked 5th-16th who were not eligible for the lottery. Comparing pool members vs. non-pool members provides an additional test: is there a discontinuity at the 4th/5th ranking boundary? This is a supplementary RDD within the Grand Slam setting.

### 3.2 Key Assumptions

**Assumption 1: Random assignment within the top-4 pool.** The Grand Slam rulebook states selection is by random draw. This must be verified by: (a) citing the ITF/Grand Slam rulebook, (b) testing whether within the pool, LL probability is independent of ranking position (1st vs. 2nd vs. 3rd vs. 4th).

**Assumption 2: Exclusion restriction (for LATE).** Being in the top-4 pool affects outcomes only through LL entry. Plausible because the pool designation is not publicly announced in a way that would affect future tournament organizers' decisions. However, the pool designation may affect the player's own psychology (knowing they are "on standby"). This is a minor concern but should be acknowledged.

**Assumption 3: Non-compliance is one-sided.** Only pool members can receive LL entry (no never-takers among the non-pool); some pool members may not receive entry (either not drawn or decline). No one outside the pool can be treated. This supports the IV exclusion.

### 3.3 Power Concerns

- ~304 pool observations over 19 years (4 Grand Slams x 4 pool members x 19 years)
- ~152 treated (central estimate), ~152 within-pool controls
- Standardized MDE at 80% power, alpha = 0.05: approximately 0.23 SD (for the within-pool comparison with N = 300)
- For ranking changes (SD ~ 50-100 positions), MDE is ~12-23 ranking positions
- This is underpowered for small effects but can detect economically meaningful changes
- **Mitigation:** Include WTA Grand Slams to approximately double the sample. Use multiple outcome horizons to increase the chance of detecting effects where they are largest.

### 3.4 What the Lottery Design Uniquely Provides

1. **Validation of the RDD:** If both designs produce effects in the same direction and similar magnitude, this powerfully supports the causal interpretation.
2. **Cleaner SUTVA:** The random draw diffuses the "near miss" psychology -- all 4 pool members had equal probability, so the control condition is less psychologically loaded.
3. **External validity check:** Grand Slams are the highest-tier events, so the treatment dose (ranking points, prize money, prestige) is largest. If effects are found here, the RDD (which pools all tournament levels) may be attenuated by lower-dose events.

---

## 4. Comparison of the Two Designs

| Dimension | ATP RDD | Grand Slam Lottery |
|-----------|---------|-------------------|
| **Internal validity** | Strong but requires RDD assumptions; discrete RV is a challenge | Near-ideal (literal randomization) |
| **Statistical power** | Good (~6,000 observations, effective ~2,000-3,000) | Limited (~300 observations) |
| **SUTVA concern** | Moderate (player just below cutoff directly harmed) | Weaker (random draw among 4 equals) |
| **Running variable** | Discrete, few mass points per tournament | N/A (lottery, no running variable) |
| **Cutoff manipulation** | No -- cutoff depends on exogenous withdrawals | N/A |
| **External validity** | Broad (all ATP tournament tiers) | Narrow (Grand Slams only -- highest tier) |
| **Treatment dose** | Varies by tournament level (10-180 ranking points for R1) | High (Grand Slam R1 = 10 points, but prestige and prize money are largest) |
| **Sample period** | 2007-2024 | 2006-2024 |
| **Estimand** | LATE at the ranking cutoff | ITT (within pool) or LATE (IV with pool as instrument) |

**Recommended emphasis:** Lead with the ATP RDD (power, breadth), use the Grand Slam lottery as a validation exercise. In the paper, present the lottery results first to establish the causal narrative with the cleanest design, then show the RDD replicates and extends with greater precision and generalizability.

---

## 5. Estimation Approach

### 5.1 Primary Estimator: Local Randomization RDD

**Package:** `rdlocrand` (R) or `rdlocrand` (Stata) -- Cattaneo, Frandsen, and Titiunik (2015)

**Specification:**

```
rdrandinf Y Rtilde, cutoff(0) wl(-w) wr(w) statistic(diffmeans)
```

Where:
- $Y$ = outcome variable
- $\tilde{R}$ = normalized running variable (rank among losers minus cutoff)
- Window $[-w, w]$ selected by the minimum window procedure in `rdwinselect`
- Test statistic: difference in means (Fisher exact / permutation p-value)

**Fixed effects:** Tournament-year fixed effects absorbed by restricting comparison to within-tournament variation (each tournament is its own mini-experiment).

**Clustering:** Standard errors clustered at the player level (players may appear as qualifying losers at multiple tournaments). For the local randomization framework, use permutation inference which is robust to arbitrary dependence within the permutation block (tournament).

### 5.2 Robustness Estimators

**Estimator 2: Continuity-based RDD with rdrobust**

```r
library(rdrobust)
rdrobust(Y, Rtilde, c = 0, kernel = "triangular", bwselect = "mserd",
         cluster = player_id, covs = X, all = TRUE)
```

- Local linear regression (polynomial order p = 1)
- MSE-optimal bandwidth (Calonico, Cattaneo, Titiunik 2014)
- Bias-corrected robust CIs
- Triangle kernel
- Covariates: age, career stage, surface, tournament tier, year

**Estimator 3: Honest CIs for discrete running variable**

```r
library(RDHonest)
RDHonest(Y ~ Rtilde, cutoff = 0, M = Mbound, kern = "triangular",
         se.method = "EHW")
```

Where $M$ is the bound on the second derivative (curvature) of the conditional expectation function, estimated from the data following Kolesar and Rothe (2018).

**Estimator 4: Grand Slam lottery -- simple difference in means**

```r
# Within the top-4 pool at each Grand Slam
lm(Y ~ D + factor(tourney_id), data = gs_pool, subset = (rank_among_losers <= 4))
# Cluster SEs at player level
```

For the IV version:
```r
library(ivreg)
ivreg(Y ~ D | in_top4_pool, data = gs_all_q3_losers)
```

### 5.3 Functional Form

- Primary: nonparametric (local linear within bandwidth)
- Robustness: local quadratic ($p = 2$) to check sensitivity to polynomial order
- For the lottery: linear with tournament fixed effects
- No global polynomials (known to produce unreliable RDD estimates; Gelman and Imbens 2019)

### 5.4 Fixed Effects Structure

- **Tournament-year FE:** Absorb tournament-specific shocks (draw strength, withdrawal patterns, surface, location). Each tournament-year is a separate "mini-experiment."
- **Year FE:** Control for secular trends in rankings, points structures, tour evolution.
- **Surface FE:** Hard, Clay, Grass (if not absorbed by tournament FE).
- Do NOT include player FE in the cross-sectional RDD. Player FE are relevant for the panel outcome analysis (ranking trajectory).

For the ranking trajectory outcomes ($Y_5$ through $Y_{14}$):
- Use player-level panel with player FE
- Event study specification around the LL event
- Week FE for secular trends

### 5.5 Sample Restrictions

1. ATP tour-level events only (exclude Challengers, ITF, Davis Cup): `tourney_level` in {G, M, A}
2. Years 2007-2024 (post-reliable qualifying data)
3. Grand Slam lottery: restricted to 2006-2024 and `tourney_level = "G"`
4. Exclude tournaments with zero LL entries (no treatment variation)
5. Exclude walkovers in the final qualifying round (the "loss" is not a standard match)
6. Exclude events where qualifying data is missing or incomplete

### 5.6 Clustering

- **Primary:** Cluster at the player level (players appear multiple times across tournaments)
- **Robustness:** Cluster at the tournament-year level (observations within a tournament are not independent)
- **Local randomization:** Permutation inference is cluster-free (permute within tournament blocks)

### 5.7 Multiple Hypothesis Testing

With multiple outcome variables across multiple time horizons, correct for multiple testing:
- **Primary outcomes (pre-registered):** Ranking change at $t+26$ weeks (medium-term); total main draw entries in next 12 months
- **Secondary outcomes:** All others, reported with Benjamini-Hochberg FDR corrections
- **Exploratory outcomes:** Long-term (2+ years) reported without formal correction but with appropriate caveats
- Use the Westfall-Young step-down procedure or Romano-Wolf correction for the family of ranking change outcomes ($t+4$, $t+8$, ..., $t+52$)

---

## 6. Anticipated Referee Objections

### Objection 1: "The running variable is discrete with few mass points. Standard RDD is invalid."

**Response:** We present three complementary inferential frameworks:
(a) Local randomization (Cattaneo et al. 2015, 2020, 2024) -- our primary framework, specifically designed for discrete running variables where treatment is as-if random within a window
(b) Honest CIs (Kolesar and Rothe 2018) -- guaranteed coverage under bounded curvature
(c) Standard rdrobust (Calonico et al. 2014) -- reported for comparability with the literature
All three produce consistent results, and the Grand Slam lottery provides a completely independent validation that does not rely on any continuity or polynomial assumption.

### Objection 2: "SUTVA is violated -- the player who just misses LL entry is directly harmed by NOT receiving the slot that went to the player above them."

**Response:** We acknowledge this concern and interpret our RDD estimate as an upper bound on the pure treatment effect (it combines the gain to the LL recipient and any psychological or competitive harm to the near-miss). We present three pieces of evidence: (a) the Grand Slam lottery estimate, where SUTVA concerns are attenuated because all pool members had equal probability; (b) a test comparing the "just-missed" control to controls further from the cutoff -- if SUTVA violation is large, the just-missed player should have worse outcomes than more distant controls; (c) a donut-hole RDD excluding the observation immediately below the cutoff. If all three analyses tell a consistent story, the qualitative finding is robust even if the exact magnitude reflects some SUTVA inflation.

### Objection 3: "The number of LL slots is endogenous. You don't observe the cutoff ex ante."

**Response:** The cutoff is determined by main draw player withdrawals, which are driven by injuries and scheduling decisions made independently of qualifying loser characteristics. We show that (a) tournament-level withdrawal counts are uncorrelated with observable characteristics of qualifying losers (balance test), (b) results are robust to restricting the sample to tournaments with exactly one LL slot (cleanest cutoff), and (c) including tournament-year fixed effects absorbs any tournament-level confounders that might correlate with both withdrawal rates and outcomes. Furthermore, the Grand Slam lottery is immune to this concern: the pool is defined by the top-4 rule regardless of how many slots open.

---

## 7. Paper Narrative and Framing

### Opening Hook
Professional tennis is one of the most unequal labor markets in the world (Gini ~ 0.91). A player ranked 150 is at the financial break-even point (Schoettl et al. 2025). At this margin, a single tournament opportunity -- worth as few as 10 ranking points and a few thousand dollars in prize money -- can trigger a feedback loop through the ranking system. We study whether these marginal opportunities cause persistent career effects.

### Conceptual Framework
Tournament theory (Lazear and Rosen 1981; Rosen 1981) + cumulative advantage / Matthew effect (Merton 1968). The ranking system creates a natural amplification mechanism: ranking points determine tournament access, tournament access determines ranking point opportunities. A one-time exogenous shock (LL entry) propagates through this feedback loop.

### Positioning
- **Closest empirical precedent:** Wang et al. (2019) on near-miss NIH grants (same RDD-at-opportunity-threshold idea, but in science rather than sports)
- **Closest sports precedent:** Zhou et al. (2023) on NBA draft round boundaries; Engist et al. (2021) on UEFA seeding
- **Closest education precedent:** Zimmerman (2014), Goodman et al. (2017) on college admission RDD
- **Differentiation from Maity et al. (2025):** Economics vs. psychology framing; RDD vs. comparison; economic outcomes vs. match wins; mechanism analysis vs. near-miss narrative

### Expected Results
Based on the institutional structure and the literature:
- **Immediate:** LL entrants win some main draw matches (Maity et al. suggest non-trivial win rates), gaining ranking points they would not otherwise have earned
- **Short-term:** Ranking improvement of 5-20 positions at $t+12$ weeks for LL recipients (the direct points effect)
- **Medium-term:** The ranking boost translates to improved tournament access (direct entry rather than qualifying), creating additional ranking point opportunities. Effect at $t+52$ weeks may be larger or smaller than at $t+12$ depending on whether cumulative advantage amplifies or the effect fades
- **Heterogeneity:** Effects should be larger at higher-tier events (more points at stake) and for players near the break-even ranking (~150) where marginal access matters most

---

## 8. Summary of Recommended Strategy

| Element | Specification |
|---------|--------------|
| **Primary design** | Sharp RDD at qualifying loser ranking cutoff (ATP events, 2007-2024) |
| **Primary inference** | Local randomization framework (`rdlocrand`) |
| **Primary estimand** | LATE at the cutoff |
| **Validation design** | Grand Slam lottery (2006-2024), ITT within top-4 pool |
| **Primary outcome** | Competitiveness index $C_{it}$ at t+4/8/12/26/52w; Elo as robustness |
| **Secondary outcomes** | Ranking change at t+26w; main draw entries in next 12mo |
| **Robustness estimators** | rdrobust, RDHonest, fuzzy RDD |
| **Clustering** | Player level (primary); tournament-year (robustness) |
| **Multiple testing** | Romano-Wolf / Westfall-Young for outcome family |
| **Key covariates** | Age, career stage, surface, tournament tier, year |
| **Sample** | ATP tour-level (G, M, A), 2007-2024; GS lottery 2006-2024 |
