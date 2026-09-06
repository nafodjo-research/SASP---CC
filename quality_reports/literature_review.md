# Literature Review: Lucky Losers in Professional Tennis

**Research Question:** What is the causal impact of Lucky Loser (LL) entry and associated additional points on subsequent match performance, winning probabilities, and short/medium-term career outcomes?

**Date compiled:** 2026-03-20
**Last revised:** 2026-03-20 (Round 2 -- fully reconciled BibTeX-annotation mismatch)

**Full materials located in:** `quality_reports/literature/lucky-losers/`

---

## Executive Summary

This literature review covers 49 papers across six categories, with 49 BibTeX entries fully reconciled against 49 annotations. The key finding is that while one paper (Maity et al. 2025) directly studies lucky losers in ATP tennis, it does so from a psychological/computational social science perspective rather than an economics perspective, and uses descriptive comparison rather than formal causal identification. No economics paper uses RDD or other rigorous causal methods to estimate the effect of lucky loser entry on economic career outcomes (ranking points, tournament access, earnings). This represents a clear gap suitable for a contribution to AEJ:Applied, JLE, JHR, RESTAT, or JSE.

The bibliography includes:
- (a) Four seminal RDD methods papers (Hahn et al. 2001, McCrary 2008, Cattaneo et al. 2020 JASA, Imbens and Kalyanaraman 2012)
- (b) Four tennis/sports economics papers on luck, performance, and incentives (Gauriot and Page 2019, Sunde 2009, Gonzalez-Diaz et al. 2012, plus a note on the wildcard literature)
- (c) Three education RDD papers strengthening the opportunity-access-at-threshold narrative (Goodman et al. 2017, Zimmerman 2014, Hoekstra 2009)
- (d) Two discrete running variable methods papers critical for the qualifying-rank running variable (Kolesar and Rothe 2018, Lee and Card 2008)
- (e) Full annotations for all BibTeX entries including general references (Angrist and Pischke 2009, Imbens 2004, Oyer 2008, Keefer 2017, Connolly and Rendleman 2008, Groothuis et al. 2007, Pan 2021)

---

## Paper Counts by Category

| Category | Count |
|----------|-------|
| 1. Directly related (same question/context) | 9 (+ wildcard note) |
| 2. Same method, different context | 11 |
| 3. Same context, different method | 7 |
| 4. Theoretical foundations | 10 |
| 5. Methods papers | 10 |
| 6. General references | 2 |
| **Total** | **49** |

---

## 1. Directly Related Papers

**Maity, Wang, Dehmamy, Medvec, Uzzi, and Wang (2025)** study lucky losers in ATP tennis as part of a broader investigation of early-career setbacks in professional sports (Scientific Reports). Using ATP data from 2007--2019 (54,084 matches, 2,688 players, 825 tournaments), they compare future performance of lucky losers to players who just missed advancing. They find non-winners can outperform winning counterparts at the margins. **Proximity: 5.** This is the primary scooping risk, but differentiation is substantial: different discipline, no formal RDD, psychological rather than economic framing, match wins rather than ranking/earnings outcomes.

**Punta, Ferrara, and Ferrara (2024)** document that the prestige of the tournament where a player achieves a first main-draw win predicts long-term career success (EPJ Data Science). Correlational, not causal. **Proximity: 3.**

**Gauriot and Page (2019)** provide causal evidence from professional soccer that lucky successes are systematically overrewarded by evaluators, violating the informativeness principle (RESTAT). Relevant to whether quasi-random outcomes are overrewarded in sports careers. **Proximity: 3.**

**Gonzalez-Diaz, Gossner, and Rogers (2012)** identify persistent heterogeneity in "critical ability" (performance under pressure) in professional tennis using US Open point-level data (JEBO). Relevant to whether lucky losers face differential performance pressure. **Proximity: 3.**

**Sunde (2009)** tests tournament incentive effects using ATP data, finding underdogs reduce effort against stronger opponents (Applied Economics). Directly relevant to lucky loser matchup dynamics. **Proximity: 3.**

**Schoettl et al. (2025)** establish that players around ATP ranking 150 are at the financial break-even point, with only one-third of top juniors achieving career financial viability (SSRN). Essential institutional context. **Proximity: 3.**

**Dagaev and Sonin (2018)** formally prove that tournament systems can be incentive-incompatible, with teams benefiting from losing (Journal of Sports Economics). Related concept but different mechanism. **Proximity: 2.**

---

## 2. Methodological Precedents (Same Method, Different Context)

**Wang, Jones, and Wang (2019)** use RDD at the NIH R01 funding threshold to show that near-miss scientists who persist outperform narrow winners (16.1% vs. 13.3% hit rate; +19.4% citations), while experiencing >10% higher attrition (Nature Communications). This is the intellectual template for the tennis study. **Proximity: 4.**

**Engist, Merkus, and Schafmeister (2021)** use RDD at UEFA seeding cutoffs and find no causal effect of favorable seeding on tournament advancement (Journal of Sports Economics). Direct methodological precedent for RDD in sports tournaments. **Proximity: 4.**

**Goodman, Hurwitz, and Smith (2017)** use RDD at Georgia's 4-year college admission threshold to show access substantially increases degree completion (JLE). Template for opportunity-access-at-threshold design. **Proximity: 3.**

**Zimmerman (2014)** uses RDD at a Florida college admission cutoff to show marginal admission yields 22% earnings gains 8--14 years later (JLE). Canonical RDD-at-access-threshold with long-run career effects. **Proximity: 3.**

**Hoekstra (2009)** uses RDD at flagship university admission cutoff showing ~20% earnings premium for white men (RESTAT). **Proximity: 3.**

**Zhou, Hu, and Shi (2023)** use RDD at the NBA first/second round draft boundary to show contract structure causally affects career length and earnings (Applied Economics). **Proximity: 3.**

**Keefer (2016)** uses RDD at NFL draft round boundaries finding large compensation discontinuities and sunk-cost effects on playing time (Journal of Sports Economics). **Proximity: 3.**

**Lee (2008)** demonstrates RDD using close elections to estimate incumbency advantages of 40--45 percentage points (Journal of Econometrics). Foundational RDD paper. **Proximity: 3.**

**Oyer (2006)** uses graduation-year conditions as IV for initial placement, showing persistent career effects for economics PhDs (Journal of Economic Perspectives). **Proximity: 3.**

**Oreopoulos, von Wachter, and Heisz (2012)** estimate 9% initial earnings loss from recession graduation persisting ~10 years (AEJ: Applied). **Proximity: 2.**

---

## 3. Same Context, Different Method

**Klaassen and Magnus (2001, 2003, 2009)** established the econometric foundations for tennis match analysis: dynamic binary panel data models, match forecasting using rankings, and efficiency analysis of service strategy (JASA, EJOR, Journal of Econometrics). **Proximity: 2.**

**Del Corral and Prieto-Rodriguez (2010)** test ranking differences as predictors of Grand Slam outcomes (International Journal of Forecasting). **Proximity: 2.**

**Guryan, Kroft, and Notowidigdo (2009)** find no peer effects from random groupings in PGA Tour events (AEJ: Applied). Relevant null result for whether facing better opponents matters. **Proximity: 2.**

**Connolly and Rendleman (2008)** decompose PGA Tour performance into skill and luck, finding luck plays a larger role than commonly acknowledged (JASA). **Proximity: 2.**

**Pan (2021)** examines dynamic ranking point incentives and effort allocation in ATP tennis (Berkeley undergraduate thesis). **Proximity: 2.**

---

## 4. Theoretical Foundations

**Lazear and Rosen (1981)** introduce tournament theory: effort increases with prize spreads in rank-order tournaments (JPE). Foundational. **Proximity: 2.**

**Rosen (1981)** explains superstar economics: small talent differences produce large earnings differences with joint consumption (AER). Explains tennis's extreme inequality. **Proximity: 2.**

**Merton (1968)** introduces the Matthew Effect / cumulative advantage: initial recognition advantages compound over time through preferential access to resources and opportunities (Science). This is the theoretical foundation for the paper's key mechanism -- that a one-time lucky loser opportunity can trigger a cumulative advantage feedback loop through the ranking system. **Proximity: 2.**

**Frank and Cook (1995)** document winner-take-all dynamics across labor markets. Tennis has a Gini coefficient of ~0.91, with the top 100 earning 80% of total prize money. **Proximity: 2.**

**Ehrenberg and Bognanno (1990)** provide the first empirical test of tournament theory using PGA golf data (JPE). **Proximity: 2.**

**Massey and Thaler (2013)** document overvaluation of early NFL draft picks due to behavioral biases (Management Science). **Proximity: 2.**

**Pluchino, Biondo, and Rapisarda (2018)** model talent vs. luck, showing 78% of simulated tournament winners lack highest merit. **Proximity: 2.**

**Groothuis, Hill, and Perri (2007)** examine early entry decisions in the NBA draft through option value and human capital theory (JSE). **Proximity: 2.**

**Oyer (2008)** shows stock market conditions at MBA graduation cause persistent sorting into investment banking and higher lifetime income (Journal of Finance). **Proximity: 2.**

**Keefer (2017)** confirms the sunk-cost fallacy in NFL playing time decisions (JSE). **Proximity: 2.**

---

## 5. Methods Papers

**Hahn, Todd, and van der Klaauw (2001)** establish the foundational identification result for RDD: treatment effects are nonparametrically identified under continuity assumptions (Econometrica). **Proximity: 3.**

**McCrary (2008)** introduces the standard density discontinuity test for manipulation of the running variable in RDD (Journal of Econometrics). The most critical validity check. **Proximity: 3.**

**Cattaneo, Jansson, and Ma (2020)** introduce the modern local polynomial density estimator and manipulation test implemented in rddensity, replacing McCrary as the state-of-the-art (JASA). **Proximity: 3.**

**Imbens and Kalyanaraman (2012)** derive the optimal bandwidth for the RDD estimator and propose the data-driven "IK" bandwidth selector (Review of Economic Studies). **Proximity: 3.**

**Calonico, Cattaneo, and Titiunik (2014)** introduce robust bias-corrected confidence intervals for RDD via rdrobust (Econometrica). Essential. **Proximity: 3.**

**Lee and Lemieux (2010)** provide the comprehensive RDD survey and user guide (JEL). Essential. **Proximity: 3.**

**Cattaneo, Idrobo, and Titiunik (2020, 2024)** provide practical RDD implementation guides including fuzzy RDD, discrete running variables, and local randomization framework (Cambridge Elements). Essential. **Proximity: 3.**

**Kolesar and Rothe (2018)** show that standard inference fails with discrete running variables and propose honest CIs with guaranteed coverage (AER). Directly relevant -- the qualifying running variable is likely discrete. **Proximity: 3.**

**Lee and Card (2008)** address inference with specification error when the running variable is discrete (Journal of Econometrics). Complementary to Kolesar and Rothe. **Proximity: 3.**

**Sun and Abraham (2021)** and **Callaway and Sant'Anna (2021)** address biased TWFE event studies under staggered treatment (Journal of Econometrics). Relevant if using event-study design. **Proximity: 2.**

---

## Key Data Sources

| Dataset | Access | Coverage | Notes |
|---------|--------|----------|-------|
| Jeff Sackmann Tennis Abstract (GitHub) | Public | ATP matches, rankings, 1968--present | Primary data source; qualifying data from ~2007 |
| Sackmann Match Charting Project (GitHub) | Public | Point-by-point data, selected matches | Supplementary for mechanism analysis |
| ATP Tour official website | Public | Rankings, results, prize money | Official source for validation |
| WTA Tour official data | Public | Women's matches, rankings | For extending to WTA |

---

## Frontier Summary

| What's been done | What's missing |
|-----------------|---------------|
| Lucky losers studied as psychological near-miss (Maity et al. 2025) | Formal causal identification (RDD) of LL effects |
| Career trajectories correlated with early wins (Punta et al. 2024) | Economic outcomes (ranking points, earnings, tournament access) |
| RDD used in other sports (NBA draft, NFL draft, UEFA seeding) | RDD applied to tennis qualifying cutoff |
| RDD at admission thresholds in education (Goodman et al., Zimmerman, Hoekstra) | Opportunity-access RDD in a competitive tournament labor market |
| Tournament theory tested in golf/tennis (Ehrenberg, Silverman, Sunde) | Cumulative advantage mechanism traced through ranking system |
| Financial break-even documented at ~ATP 150 (Schoettl et al. 2025) | Welfare analysis of tournament access rules |
| Luck overrewarded in sports careers (Gauriot and Page 2019) | Structural consequences of luck through ranking mechanics |
| Discrete RDD methods developed (Kolesar-Rothe, Lee-Card) | Applied to tennis qualifying setting |

---

## Output Files

- **Annotated Bibliography:** `C:/Users/maste/Documents/TennisLL/clo-author/quality_reports/literature/lucky-losers/annotated_bibliography.md`
- **Frontier Map:** `C:/Users/maste/Documents/TennisLL/clo-author/quality_reports/literature/lucky-losers/frontier_map.md`
- **Positioning:** `C:/Users/maste/Documents/TennisLL/clo-author/quality_reports/literature/lucky-losers/positioning.md`
- **BibTeX:** `C:/Users/maste/Documents/TennisLL/clo-author/Bibliography_base.bib`
