# Positioning: Lucky Losers in Professional Tennis

**Date:** 2026-03-20
**Last revised:** 2026-03-20 (Round 2 -- addressing librarian-critic feedback)

---

## Suggested Contribution Statement

This paper provides the first causal estimates of the effect of quasi-random opportunity shocks on career outcomes in a professional tournament setting. We exploit the lucky loser mechanism in ATP/WTA tennis -- where players who lose in qualifying rounds gain main draw entry when another player withdraws -- as a natural experiment that generates exogenous variation in access to high-stakes competition. Using a regression discontinuity design at the qualifying cutoff, we estimate the causal impact of main draw entry on ranking point accumulation, ranking trajectory, subsequent tournament access, and prize money earnings over short and medium horizons. We document a cumulative advantage mechanism: the initial ranking point shock improves future tournament access, which generates further ranking gains, consistent with path dependence in career dynamics. Our findings contribute to the economics of opportunity, tournament theory, and the growing literature on how initial conditions shape professional careers.

---

## Differentiation from Closest Competitors

### vs. Maity et al. (2025) -- "Early Career Setback and Future Achievement in Professional Sports"

This is the paper that most directly overlaps. Key points of differentiation:

| Dimension | Maity et al. | Our Paper |
|-----------|-------------|-----------|
| Discipline | Computational social science / psychology | Applied economics |
| Question | Does setback improve motivation/performance? | Does opportunity shock cause cumulative career advantage? |
| Identification | Descriptive comparison | Formal RDD at qualifying threshold |
| Running variable | Not formally defined | Qualifying round ranking/position |
| Outcomes | Match wins | Ranking points, ranking level, tournament access, earnings |
| Mechanism tested | Psychological resilience/near-miss | Economic: opportunity -> points -> access -> more points |
| Theory | Psychology of setbacks | Tournament theory + cumulative advantage |
| Policy relevance | None discussed | Tournament design, allocation of main draw access |
| Sample | ATP only | ATP + WTA |
| Target journal | Scientific Reports | Economics journal |

**Key talking point for referees:** "Our paper complements Maity et al. (2025) by shifting from whether setbacks improve motivation to whether opportunity shocks create cumulative economic advantage through the ranking system. We use formal causal identification (RDD) rather than descriptive comparison, and we study economic outcomes -- ranking points, tournament access, and earnings -- rather than match wins alone."

### vs. Wang, Jones, and Wang (2019) -- "Early-Career Setback and Future Career Impact"

**Relationship:** Methodological predecessor in a different domain (academia). Our paper extends their near-miss framework to a setting where: (a) the feedback loop from initial conditions to future opportunities is more transparent (ranking points directly determine tournament access), (b) outcomes are observed at much higher frequency (weekly rankings, match-by-match), and (c) the mechanism is economic rather than motivational.

### vs. Oyer (2006) and Oreopoulos et al. (2012) -- Initial Conditions Literature

**Relationship:** Our paper adds to this literature by providing a cleaner natural experiment. In the tennis setting, the opportunity shock is sharper (binary: LL or not) and the feedback mechanism is fully observable (ranking points -> tournament access -> more points), unlike labor markets where the channels are partially opaque.

### vs. Engist et al. (2021) -- RDD in Sports Tournaments

**Relationship:** Methodological sibling. We use a similar approach (RDD at a tournament cutoff) but study a fundamentally different outcome (individual career trajectory rather than single-tournament advancement). Our paper also has a richer dynamic story about cumulative advantage that goes beyond single-tournament effects.

### vs. Goodman et al. (2017), Zimmerman (2014), Hoekstra (2009) -- Education RDD Papers

**Relationship:** These papers establish the conceptual template: RDD at an opportunity-access threshold (college admission) reveals persistent effects on career outcomes (degree completion, earnings). Our paper applies this same conceptual logic to a different setting (tournament access) where the mechanism is more transparent and outcomes are observed at higher frequency. The tennis setting complements the education evidence by showing that opportunity-access effects operate even in a highly competitive, skill-based labor market.

### vs. Gauriot and Page (2019) -- Luck vs. Skill in Sports

**Relationship:** Gauriot and Page show that evaluators systematically overreward lucky outcomes in professional sports (soccer). Our paper examines a related but distinct question: not whether lucky loser entry is overrewarded by evaluators, but whether the quasi-random opportunity itself has a causal effect on career trajectories through the mechanical ranking system. The two papers are complementary: theirs about perception of luck, ours about the structural consequences of luck.

---

## Target Journal Strategy

### Primary Target: AEJ: Applied Economics

**Fit:** Clean applied micro paper with credible identification in a novel setting. The sports economics context is accessible to a broad applied audience, and the cumulative advantage story speaks to labor economics more broadly.

**Positioning:** "This paper provides new evidence on how initial conditions shape career trajectories, exploiting a transparent natural experiment in professional tennis. Our RDD design yields clean causal estimates of opportunity effects, and the institutional setting allows us to trace the full mechanism from initial shock to cumulative career advantage."

**Potential referee concern:** "Is this too narrow for AEJ:Applied? Is tennis a niche setting?" Respond by emphasizing the external validity: the cumulative advantage mechanism (initial opportunity -> credential -> more opportunity) operates in many labor markets; tennis provides unusually clean identification.

### Secondary Target: Journal of Labor Economics (JLE)

**Fit:** Career dynamics, path dependence, and the economics of opportunity are core JLE themes. The cumulative advantage mechanism is a labor economics story.

**Positioning:** Emphasize the human capital and career dynamics contribution over the sports setting.

### Tertiary Target: Review of Economics and Statistics (RESTAT)

**Fit:** Novel data, careful measurement, and methodological precision. RESTAT values getting the econometrics exactly right.

**Positioning:** Emphasize the measurement innovation (tracking the full mechanism through observable ranking data) and the RDD methodology.

### Field Target: Journal of Sports Economics (JSE)

**Fit:** Perfect topical fit. The JSE already publishes RDD papers in sports settings (Engist et al. 2021, Keefer 2016). This would be a natural home if the paper is more narrowly focused on the tennis-specific results.

**Positioning:** Emphasize tennis-specific contributions: first causal evidence on LL effects, tournament design implications, inequality in professional tennis.

### Alternative: Journal of Human Resources (JHR)

**Fit:** JHR publishes empirical papers on career outcomes and human capital accumulation. The paper's focus on how access to opportunity affects career trajectories aligns with JHR's scope.

**Positioning:** Emphasize the human capital and career opportunity channel; connect to education and labor market access literature.

---

## Suggested Paper Structure

1. **Introduction** -- Frame as economics of opportunity, not sports. Lead with: "Small initial advantages can compound into large career differences. We estimate this mechanism using a natural experiment in professional tennis."

2. **Institutional Background** -- ATP/WTA ranking system, qualifying rounds, lucky loser mechanism. Establish that LL status is quasi-random conditional on being in the final qualifying round.

3. **Theoretical Framework** -- Brief model combining tournament theory (Lazear-Rosen) with cumulative advantage (Merton/Matthew effect). Predictions: (a) LL entry generates ranking points, (b) ranking points improve future tournament access, (c) this creates a multiplier effect on career outcomes.

4. **Data and Descriptive Statistics** -- Sackmann ATP data, qualifying round information, ranking histories. Document that LL and non-LL players in the last qualifying round are comparable on observables.

5. **Empirical Strategy** -- RDD at the qualifying cutoff. Discussion of: running variable definition, bandwidth selection (rdrobust), validity checks (no sorting, covariate balance, density tests using both McCrary 2008 and Cattaneo, Jansson, and Ma 2020). Discussion of discrete running variable concerns (Kolesar and Rothe 2018; Lee and Card 2008) and presentation under both continuity-based and local randomization frameworks.

6. **Results** -- (a) First stage: LL entry on main draw participation, (b) Immediate effect on ranking points, (c) Short-run effect on ranking level, (d) Medium-run effect on tournament access and earnings.

7. **Mechanism: Cumulative Advantage** -- Decompose the medium-run effect into: direct point gain vs. indirect gain through improved tournament access. Test whether the effect amplifies over time.

8. **Robustness** -- Alternative bandwidths, polynomial orders, placebo cutoffs, donut RDD, fuzzy RDD specification, Kolesar-Rothe honest CIs for discrete running variable.

9. **Discussion and Conclusion** -- External validity to other career settings, implications for tournament design, connection to inequality in professional sports.

---

## Key References to Cite in Each Section (Must-Cite List)

| Section | Must-Cite Papers |
|---------|-----------------|
| Introduction | Wang et al. (2019), Oyer (2006), Maity et al. (2025), Rosen (1981), Merton (1968) |
| Institutional background | Schoettl et al. (2025), Punta et al. (2024) |
| Theory | Lazear and Rosen (1981), Frank and Cook (1995), Merton (1968), Pluchino et al. (2018) |
| Literature review | Goodman et al. (2017), Zimmerman (2014), Hoekstra (2009), Gauriot and Page (2019), Sunde (2009), Gonzalez-Diaz et al. (2012) |
| Empirical strategy | Hahn et al. (2001), Lee and Lemieux (2010), Calonico et al. (2014), Cattaneo et al. (2020, 2024), McCrary (2008), Cattaneo, Jansson, and Ma (2020), Imbens and Kalyanaraman (2012), Kolesar and Rothe (2018), Lee and Card (2008) |
| Results | Compare to Wang et al. (2019), Engist et al. (2021), Zhou et al. (2023) |
| Mechanism | Oreopoulos et al. (2012), Oyer (2006), Merton (1968) |
| Robustness | Cattaneo et al. (2024, Extensions), Kolesar and Rothe (2018), Lee and Card (2008) |
| Discussion | Ehrenberg and Bognanno (1990), Massey and Thaler (2013), Gauriot and Page (2019) |
