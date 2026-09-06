# Frontier Map: Lucky Losers in Professional Tennis

**Date:** 2026-03-20
**Last revised:** 2026-03-20 (Round 2 -- addressing librarian-critic feedback)

---

## What Has Been Done

### 1. Lucky Losers in Tennis -- Direct Evidence

- **Maity et al. (2025)** is the only published paper that directly studies lucky losers in ATP tennis. Published in Scientific Reports (not an economics journal), it examines whether near-miss setbacks improve future performance through a psychological lens. Key limitations: (a) the identification is comparison-based rather than formal RDD, (b) outcomes are match wins rather than economic outcomes (ranking points, earnings, tournament access), (c) framing is psychology/near-miss rather than economics of opportunity, (d) no formal welfare or policy analysis.

### 2. Tennis Career Trajectories

- **Punta et al. (2024)** documents that early career wins at prestigious tournaments predict long-term career success, but uses correlational methods without causal identification.
- **Schoettl et al. (2025)** establishes that players around ranking 150 are at the financial break-even point, providing institutional context for why marginal opportunities matter.
- **Klaassen and Magnus (2001, 2003, 2009)** established the econometric foundations for tennis match-level analysis.

### 3. Tennis Tournament Incentives and Player Behavior

- **Ehrenberg and Bognanno (1990)** established that prize spreads affect effort in golf; tennis analogs exist but are less well-studied.
- **Silverman and Seidel (2016)** document ~1% performance increase when prize differentials double in ATP events.
- **Sunde (2009)** tests heterogeneity and tournament incentive effects using ATP data, finding that underdogs reduce effort against much stronger opponents -- directly relevant to lucky loser matchup dynamics.
- **Gonzalez-Diaz, Gossner, and Rogers (2012)** identify persistent heterogeneity in "critical ability" (performance under pressure) using US Open point-level data, relevant to the high-pressure context lucky losers face.
- **Dagaev and Sonin (2018)** study incentive compatibility in multi-stage tournaments theoretically.

### 4. Luck vs. Skill in Sports Careers

- **Gauriot and Page (2019)** provide causal evidence from professional soccer that lucky successes are systematically overrewarded -- evaluators conflate luck with skill. Published in RESTAT. The mechanism of overrewarding quasi-random outcomes is directly relevant to whether the tennis system appropriately values lucky loser entry.

### 5. Career Opportunity Shocks (Non-Tennis)

- **Wang, Jones, and Wang (2019)** -- RDD at NIH funding thresholds: near-miss scientists outperform narrow winners (+19.4% citations) but have higher attrition.
- **Oyer (2006)** -- IV showing initial job placement for economics PhDs has causal, persistent career effects.
- **Oreopoulos, von Wachter, and Heisz (2012)** -- Graduating in recession causes 9% initial earnings loss persisting ~10 years.
- **Zhou, Hu, and Shi (2023)** -- RDD at NBA draft round boundary shows contract structure causally affects career length and earnings.

### 6. RDD at Opportunity-Access Thresholds (Education)

- **Goodman, Hurwitz, and Smith (2017)** -- RDD at Georgia's 4-year college admission threshold: access significantly increases degree completion.
- **Zimmerman (2014)** -- RDD at Florida college admission cutoff: marginal admission yields 22% earnings gains 8--14 years later.
- **Hoekstra (2009)** -- RDD at flagship university admission cutoff: attending causes ~20% higher earnings for white men.

These education RDD papers establish the template for studying how access to opportunity at a threshold creates persistent career effects -- the same conceptual structure as the lucky loser study.

### 7. RDD in Sports

- **Engist, Merkus, and Schafmeister (2021)** -- RDD at UEFA seeding cutoffs: no causal effect of favorable seeding on advancement.
- **Keefer (2016)** -- RDD at NFL draft round boundaries: large compensation discontinuities and sunk-cost effects.
- **Lee (2008)** -- Foundational RDD paper using close elections (methodological template).

---

## What Has NOT Been Done (The Gaps)

### Gap 1: No Formal Causal Identification of Lucky Loser Effects on Economic Outcomes

No paper uses RDD or other rigorous causal methods to estimate the effect of lucky loser entry on ranking points gained, ranking progression, future tournament access, or prize money earnings. Maity et al. (2025) comes closest but uses comparison rather than formal RDD, and focuses on match wins rather than economic outcomes.

### Gap 2: No Economics Paper on Tennis Opportunity Shocks

The existing tennis lucky loser paper (Maity et al. 2025) is framed as a psychology/management science paper about setbacks and motivation, published in Scientific Reports. No economics journal paper examines the economic channel: how a quasi-random opportunity to compete in a main draw causally affects the accumulation of ranking points, which determines future tournament access, which determines earnings potential.

### Gap 3: No Analysis of the Cumulative Advantage Mechanism in Tennis

Despite tennis having one of the highest Gini coefficients in professional sports (~0.91), no paper formally tests whether the Matthew effect / cumulative advantage operates through the ranking system. The ranking system creates a natural feedback loop: ranking points determine tournament access, tournament access determines ranking point opportunities, and so on. Lucky loser entries provide an exogenous shock to this feedback loop.

### Gap 4: No Welfare Analysis of Tournament Access Rules

No paper evaluates the welfare implications of the lucky loser rule, wildcards, or other mechanisms for allocating scarce main draw slots. Given the extreme financial precarity at the qualifying margin (break-even at ~ATP 150), the allocation of main draw access has real welfare consequences.

### Gap 5: No Short-Run vs. Medium-Run Decomposition

Existing work does not decompose the effect of a lucky loser opportunity into: (a) the immediate match effect (does facing a better opponent help or hurt?), (b) the short-run ranking effect (how many additional points are gained?), and (c) the medium-run career effect (does the ranking boost improve subsequent tournament access and create cumulative advantage?).

---

## Where This Paper Fits

The proposed paper fills Gaps 1--3 and partially fills Gap 5. It would be:

1. **The first economics paper** using formal causal identification (RDD at the qualifying cutoff) to estimate the effect of lucky loser entry on career outcomes.

2. **The first paper** to document the cumulative advantage mechanism in tennis, showing how a one-time opportunity shock propagates through the ranking system.

3. **A bridge paper** connecting the sports economics literature (tournament theory, career dynamics) with the labor economics literature on initial conditions and path dependence, and the education economics literature on opportunity access at thresholds.

### Contribution Matrix

| Dimension | Maity et al. (2025) | This Paper (Proposed) |
|-----------|--------------------|-----------------------|
| Setting | ATP tennis | ATP/WTA tennis |
| Mechanism | Psychological (near-miss) | Economic (opportunity + cumulative advantage) |
| Identification | Comparison | RDD at qualifying cutoff |
| Outcomes | Match wins | Ranking points, ranking, tournament access, earnings |
| Time horizon | Short-run performance | Short + medium-run career trajectory |
| Framework | Psychology | Tournament theory + cumulative advantage |
| Target audience | General science | Economics |
| Journal type | Multidisciplinary (Sci. Reports) | Economics (AEJ:Applied, JLE, JSE, JHR, RESTAT) |

---

## Risk Assessment

### Scooping Risk: MODERATE

Maity et al. (2025) is the primary risk. However, the differentiation is substantial:
- Different discipline (economics vs. psychology/management science)
- Different method (RDD vs. comparison)
- Different outcomes (economic vs. match performance)
- Different theoretical framework

A referee may ask: "How is this different from Maity et al.?" The answer must clearly articulate the economics contribution: causal identification via RDD, economic outcomes (not just wins), and the cumulative advantage mechanism.

### Methodological Risk: MODERATE

The RDD at the qualifying cutoff requires careful attention to several issues:

**Standard RDD concerns:**
- Whether the running variable is truly continuous or discrete (qualifying round rankings may be discrete)
- Whether players can manipulate their position near the cutoff (unlikely, since LL status depends on another player's withdrawal)
- Whether the sample size around the cutoff is sufficient for RDD

**Discrete running variable concern (CRITICAL):**
The qualifying running variable (e.g., qualifying rank or position among last-qualifying-round losers) is likely discrete with a limited number of mass points per tournament. This is a well-known challenge for RDD inference:

- **Kolesar and Rothe (2018, AER)** show that the common practice of clustering standard errors by the running variable does not adequately guard against model misspecification when the running variable is discrete, and produces CIs with poor coverage. They propose CIs with guaranteed coverage under curvature or smoothness bounds, implemented in the RDHonest package. **This paper must be cited and the method should be considered.**

- **Lee and Card (2008, JoE)** propose modeling specification errors as random when the running variable is discrete, showing that conventional SEs overstate precision. Their approach provides an alternative inference framework.

- **Cattaneo, Idrobo, and Titiunik (2024, Extensions)** discuss the local randomization framework as an alternative to the continuity-based RDD framework when the running variable is discrete. In the local randomization framework, treatment is assumed to be as-if randomly assigned within a narrow window, which may be a natural fit for the lucky loser setting where the outcome depends on which player withdraws.

**Recommendation:** The paper should present results under both the continuity-based and local randomization frameworks, and use the Kolesar-Rothe honest CIs as a robustness check. The McCrary (2008) / Cattaneo, Jansson, and Ma (2020) density tests should be reported for the manipulation check, with the caveat that density tests have reduced power with discrete running variables.

**Note on manipulation tests:** The standard McCrary (2008) density test and its modern replacement by Cattaneo, Jansson, and Ma (2020, JASA) should both be reported. The Cattaneo et al. test (rddensity package) is preferred for its better finite-sample properties.

### Data Risk: LOW

ATP match-level data with qualifying round information is publicly available via Jeff Sackmann's Tennis Abstract (GitHub). The main constraint is that qualifying round data is primarily available from 2007 onward, limiting the analysis window.
