# Research Specification: Lucky Losers and Career Trajectories

**Date:** 2026-03-20
**Status:** DRAFT
**Author:** Hugo Sant'Anna (interview) + Claude (formalization)

---

## 1. Research Question

**Primary:** What is the causal impact of lucky loser (LL) entry — and the associated bonus ranking points — on a player's short- and medium-term competitive potential?

**Operationalized:** Does LL entry shift the expected win probability $\mathbb{E}_j[P_t(i \to j \mid X_{it}, X_{jt}, X_{ijt})]$ — a competitiveness index measuring the expected probability that player $i$ beats a representative opponent, conditional on individual characteristics, recent form, and pairwise variables — in the weeks and months following the opportunity?

**Secondary questions:**
1. Does the effect persist beyond the mechanical points boost, or does it fade within a few months?
2. Is the effect heterogeneous by career stage (early-career Challenger-level players vs. established tour players)?
3. Does the LL effect operate through increased tournament access at higher tiers (e.g., ATP 250+ entry after Challenger-level baseline)?

---

## 2. Core Economic Story

This is a paper about **the returns to opportunity at the margin** in a tournament-based labor market. A lucky loser receives an unexpected chance to compete — and earn ranking points — in a draw they would otherwise have missed. The question is whether this marginal opportunity has effects beyond the direct points earned: does it shift the player's trajectory, or does ability alone determine long-term outcomes?

**Broader lesson for labor economists:** If a single unexpected break in a transparent, meritocratic setting (professional tennis) can measurably shift career trajectories, what does this imply for labor markets where opportunity allocation is less transparent and more path-dependent?

**Expected effect:** Temporary boost (fades within ~6 months). Strongest for early-career players competing primarily at Challenger level who receive LL entry into ATP 250+ events. Mechanism: the bonus points and experience shift them into a higher tier of tournament access temporarily, with potential knock-on effects on confidence and competitiveness.

---

## 3. Identification Strategy

### Primary Design: Regression Discontinuity (ATP/WTA events)

At non-Grand Slam events, LL spots are awarded to the **highest-ranked final-round qualifying losers**. This creates a sharp cutoff: among players who lost in the last qualifying round, those ranked just above vs. just below the LL threshold receive very different treatment (main draw entry vs. going home).

- **Running variable:** Ranking position among final-round qualifying losers (discrete)
- **Cutoff:** The ranking threshold that determines LL entry (typically 1-2 spots per tournament)
- **Treatment:** Main draw entry + opportunity to earn ranking points
- **Control:** Final-round qualifying losers who did not receive LL entry

**Key assumptions to defend:**
- No manipulation of ranking at the cutoff (players cannot precisely control their ranking relative to other qualifying losers)
- Continuity of potential outcomes at the cutoff
- Discrete running variable requires appropriate inference methods (Kolesár and Rothe, 2018)

### Secondary Design: Grand Slam Lottery

At Grand Slams, LL spots are assigned **randomly** among final-round qualifying losers. This provides a clean randomized experiment, though with smaller sample size (~300 observations over the full sample period).

- **Treatment:** Random selection as LL at a Grand Slam
- **Control:** Final-round qualifying losers not selected
- **Advantage:** True randomization, no selection concerns
- **Limitation:** Small sample, limited to 4 tournaments/year

### Complementarity

The RDD provides power; the lottery provides internal validity. Consistent estimates across both designs would be strongly convincing.

---

## 4. Outcome Variables

### Primary Outcome: Competitiveness Index

$$C_{it} = \mathbb{E}_j[P_t(i \to j \mid X_{it}, X_{jt}, X_{ijt})]$$

The expected win probability of player $i$ against a representative opponent pool at time $t$, conditional on:
- $X_{it}$: player $i$'s characteristics (ranking, recent form, surface proficiency, age)
- $X_{jt}$: opponent characteristics (same variables)
- $X_{ijt}$: pairwise variables (head-to-head record, nationality match, etc.)

**Construction:** Estimate a match-outcome model (logit/probit) on the full match dataset, then compute predicted probabilities for each player against a standardized opponent pool at each point in time. The LL effect is then measured as the shift in this index post-treatment.

### Secondary Outcomes

1. **Match victories** ($n_{i,e,s,t}$): Number of matches won in subsequent tournaments (0, 3, 6, 12 months post-LL)
2. **Ranking progression**: Change in ATP/WTA ranking at 3, 6, 12 months post-LL entry
3. **Tournament access**: Binary indicator for direct main draw entry at same-tier or higher-tier events in subsequent months
4. **Tournament tier progression**: Average tier of tournaments entered (Challenger, ATP 250, 500, 1000, Grand Slam) in subsequent months

---

## 5. Data

### Source
Jeff Sackmann's GitHub repositories:
- `tennis_atp`: ATP main draw matches (1968–2024), qualifying/challenger matches (1978–2024)
- `tennis_wta`: WTA main draw matches (1968–2024), qualifying/ITF matches (1968–2024)

### Sample Period
**2000–2024** (extending from the original 2007–2017 window; exact start year TBD based on data quality for qualifying rounds and LL flags)

### Estimation Sample
1. **Treatment group:** All players who received a lucky loser (LL) spot at any professional event in the sample period
2. **Control group:** All final-round qualifying losers at the same tournaments who did not receive LL entry

### Key Variables Already Constructed (from AnnotatedCode.R)
- Match-level data (winner/loser, scores, stats) for ATP and WTA
- Tiebreak counts and winner/loser tiebreak wins
- Cumulative match statistics by tournament and player (e.g., aces)
- Tour indicator (ATP/WTA)

### Variables to Construct
- LL entry flag (from `entry` field in Sackmann data)
- Running variable: ranking position among final-round qualifying losers per tournament
- Pre-treatment covariates: age, ranking, weeks since last match, recent win%, surface-specific performance
- Competitiveness index (from estimated match-outcome model)
- Tournament tier classification
- Career stage indicators (e.g., years on tour, baseline tournament tier)

---

## 6. Covariates (from Proposal)

### Player Pre-Tournament Form
- Age, ranking, and seed at time of tournament
- Weeks since last match (match readiness)
- Recent match performance (last 2-3 tournaments)
- Chamberlain device expansion for player fixed effects

### Tournament Characteristics
- Tournament tier (Grand Slam, ATP 1000, ATP 500, ATP 250, Challenger)
- Event size and entry difficulty
- Surface

### Player's Prior History at Tournament
- Number of previous participations
- Historical win percentage at event
- Historical match performance at event

### Player's Surface-Specific Experience
- Win percentage on surface (excluding current event)
- Average match statistics on surface

### Tournament Peer Effects
- First-round opponent characteristics
- Expected strength of potential subsequent opponents

### Pairwise Competitive History
- Historical win percentage against opponent groups
- Head-to-head records
- Same nationality indicator

---

## 7. Empirical Approach (Ordered)

1. **Data construction:** Build estimation sample with LL flags, running variable, covariates
2. **Descriptive analysis:** LL frequency by tour, tier, year; balance tables at RDD cutoff
3. **Match-outcome model:** Estimate $P(i \text{ beats } j \mid X)$ on full match data; compute competitiveness index
4. **RDD estimation:** Local polynomial regression at ranking cutoff (Calonico et al., 2014; robust bias-corrected inference)
5. **Lottery estimation:** Simple difference-in-means for Grand Slam LL lottery
6. **Robustness:** Bandwidth sensitivity, placebo cutoffs, McCrary/density test, donut-hole RDD
7. **Heterogeneity:** By career stage (early-career vs. veteran), tour (ATP vs. WTA), tournament tier, surface
8. **Mechanisms (suggestive):** Decompose into direct points effect, tournament access channel, competitiveness shift net of points

---

## 8. Primary Audience

**Labor economists** who study opportunity, mobility, and career dynamics. Tennis is the *setting*, not the audience. The paper should:
- Lead with the economic question (do marginal opportunities shift trajectories?)
- Frame tennis as a "laboratory" with clean measurement and transparent institutions
- Minimize institutional detail in the main text; relegate tennis-specific mechanics to appendix/background section
- Emphasize external validity implications for other labor markets

### Target Journals (in order)
1. **AEJ: Applied Economics** — clean applied micro with novel natural experiment
2. **Journal of Labor Economics** — if mechanisms (human capital, tournament access) are well-identified
3. **RESTAT** — if competitiveness index methodology is a contribution
4. **Journal of Sports Economics** — field fallback

---

## 9. Expected Contribution

1. **First causal estimate** of unexpected opportunity effects in a transparent tournament labor market (no prior paper uses RDD or lottery on lucky losers with formal identification)
2. **Novel outcome variable** — competitiveness index $\mathbb{E}_j[P_t(i \to j \mid X)]$ that separates ability effects from mechanical points accumulation
3. **Dual identification strategy** — RDD + lottery on the same population provides unusually strong internal validity
4. **External validity narrative** — connects to labor economics literature on initial conditions, opportunity access, and cumulative advantage

---

## 10. Key Risks

| Risk | Severity | Mitigation |
|------|----------|------------|
| LL effect is zero (null result) | Medium | Still publishable: tells us opportunity doesn't matter in meritocratic settings |
| Discrete running variable weakens RDD | High | Use Kolesár and Rothe (2018) methods; supplement with GS lottery |
| Small Grand Slam lottery sample | Medium | Treat as validation, not primary; combine with RDD for joint inference |
| Competitiveness index model misspecification | Medium | Use multiple specifications (logit, random forest); show robustness |
| Scooping by Maity et al. (2025) | Low-Medium | They use correlational methods; our RDD/lottery design is clearly differentiated |
| Manipulation at RDD cutoff | Low | Rankings are determined months in advance; manipulation requires losing on purpose |

---

## 11. Scope Boundaries

### In scope (MUST)
- RDD at ATP/WTA LL cutoff (primary identification)
- Grand Slam lottery (secondary identification)
- Competitiveness index as primary outcome
- Ranking and match win outcomes
- Heterogeneity by career stage
- 2000–2024 sample (or longest feasible window)

### In scope (SHOULD)
- Tournament access / tier progression outcomes
- ATP and WTA analysis (pooled and separate)
- Mechanism decomposition (points vs. access vs. competitiveness)

### Out of scope (explicitly excluded)
- Psychological momentum (not measurable from match data)
- Network/sponsorship effects (no data)
- Structural model of tournament entry decisions
- Welfare analysis
