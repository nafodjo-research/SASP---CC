# Domain Profile

## Field

**Primary:** Labor Economics (opportunity shocks, career trajectories, economic mobility)
**Adjacent subfields:** Sports Economics, Personnel Economics, Applied Microeconometrics
**Setting:** Professional tennis (ATP/WTA tour) as a laboratory for studying labor market dynamics

---

## Target Journals (ranked by tier)

| Tier | Journals |
|------|----------|
| Top-5 | AER, JPE, QJE, REStud |
| Top field | AEJ:Applied, JLE, RESTAT |
| Strong field | JHR, JPubE, Labour Economics, Economic Journal |
| Specialty | Journal of Sports Economics, International Journal of Sport Finance |

---

## Common Data Sources

| Dataset | Type | Access | Notes |
|---------|------|--------|-------|
| Jeff Sackmann ATP GitHub | Match-level panel | Public | Main draw + qualifying/challenger matches 1968–2024; includes entry type (LL flag), scores, rankings, stats |
| Jeff Sackmann WTA GitHub | Match-level panel | Public | Same structure as ATP; qualifying/ITF matches 1968–2024 |
| ATP/WTA official rankings | Weekly panel | Public (scraped) | Official ranking points and positions; useful for ranking trajectory outcomes |

---

## Common Identification Strategies

| Strategy | Typical Application | Key Assumption to Defend |
|----------|-------------------|------------------------|
| Sharp RDD at LL ranking cutoff | Among final-round qualifying losers, highest-ranked get LL spots (ATP/WTA events) | No manipulation of ranking at the cutoff; continuity of potential outcomes |
| Randomized lottery | Grand Slam LL assignment is by lottery among qualifying losers | True randomization (institutional feature); balance check |
| Combined RDD + lottery | RDD for power, lottery for validation | Consistent estimates across both designs reinforce internal validity |

---

## Field Conventions

- Binary outcomes (win/loss) → logit with marginal effects; LPM as robustness
- RDD → local polynomial with robust bias-corrected inference (Calonico et al., 2014)
- Discrete running variable → Kolesár and Rothe (2018) inference methods
- Cluster standard errors at the player level (repeated observations per player)
- Show event study / dynamic treatment effects for persistence analysis
- Report ITT (intent to treat) as primary; discuss relationship to LATE
- Always discuss external validity: tennis → broader labor markets

---

## Notation Conventions

| Symbol | Meaning | Anti-pattern |
|--------|---------|-------------|
| $C_{it}$ | Competitiveness index for player $i$ at time $t$ | Don't use $C$ without subscripts |
| $P_t(i \to j \mid X)$ | Win probability of $i$ vs $j$ conditional on observables | Don't conflate with unconditional win rate |
| $n_{i,e,s,t}$ | Matches won by player $i$ in event $e$, surface $s$, tournament $t$ | Don't use $Y$ generically without defining |
| $R_{it}$ | ATP/WTA ranking of player $i$ at time $t$ | Don't use "rank" for both ranking and running variable |
| $D_i$ | LL treatment indicator (1 = received LL entry) | Don't confuse with entry into qualifying |
| $X_{it}$ | Player $i$'s characteristics at time $t$ | Subscript $t$ required for time-varying |
| $X_{ijt}$ | Pairwise variables for $i$ vs $j$ at time $t$ | Always include both player subscripts |

---

## Seminal References

| Paper | Why It Matters |
|-------|---------------|
| Oyer (2006) | Initial labor market conditions → persistent career effects (Wall Street analysts) |
| Oreopoulos et al. (2012) | Graduating in recession → long-term earnings penalty |
| Chetty et al. (2014) | Geography of opportunity; neighborhood effects on mobility |
| Lee and Lemieux (2010) | RDD practitioner guide — our primary identification follows this |
| Calonico, Cattaneo, Titiunik (2014) | Robust RDD inference — the estimator we use |
| Kolesár and Rothe (2018) | RDD with discrete running variable — critical for our setting |
| Gauriot and Page (2019) | Luck vs. skill in tennis (RESTAT) — closest published precedent |
| Maity et al. (2025) | Correlational analysis of lucky losers — scooping risk, but no causal ID |

---

## Field-Specific Referee Concerns

- "Is the running variable truly discrete or continuous?" — must address with appropriate methods
- "Can players manipulate their ranking to get LL spots?" — address with McCrary test and institutional argument
- "How is the competitiveness index constructed? Is it just a predicted probability?" — need robustness to model specification
- "External validity: why should we care about tennis players?" — frame as laboratory for opportunity shocks in labor markets
- "How does this differ from Maity et al. (2025)?" — emphasize causal identification vs. correlation
- "What about wildcards?" — must discuss how LL mechanism differs from wildcard entry
- "Heterogeneity: is this just mechanical (more points → better ranking)?" — need to separate ability effect from points channel
- "Sample size for Grand Slam lottery: is it powered?" — be transparent about limitations; position as validation

---

## Quality Tolerance Thresholds

| Quantity | Tolerance | Rationale |
|----------|-----------|-----------|
| Point estimates | 1e-4 | Probability-scale outcomes |
| Standard errors | 1e-4 | Robust/clustered SEs |
| RDD bandwidth | Report optimal + 0.5x, 1.5x, 2x | Standard sensitivity grid |
| Balance test p-values | > 0.05 across covariates | Standard covariate balance |
| McCrary test p-value | > 0.10 | No evidence of manipulation |
