# Data Discovery Report: Lucky Losers in Professional Tennis

**Date:** 2026-03-20
**Agent:** Explorer
**Phase:** Discovery
**Research Question:** What is the causal impact of Lucky Loser (LL) entry on a player's short- and medium-term competitive potential?
**Identification Strategies:** (1) RDD at ATP ranking cutoff among final-round qualifying losers; (2) Grand Slam lottery design among top-ranked final-round qualifying losers.

---

## Executive Summary

Thirteen data sources were evaluated. The Jeff Sackmann ATP and WTA GitHub repositories (Grade A) are the clear primary data source, providing match-level data with direct LL identification via the `entry` field, qualifying round results, and weekly rankings -- all publicly available under CC BY-NC-SA 4.0. The data supports both identification strategies. Key findings:

1. **The LL treatment variable exists and is directly coded** in the `winner_entry`/`loser_entry` fields.
2. **The running variable must be constructed** by ranking final-round qualifying losers by ATP ranking within each tournament.
3. **The Grand Slam lottery rule is more nuanced than initially documented.** The pool size for the random draw depends on WHEN the withdrawal occurs relative to qualifying completion: (a) if after qualifying is complete, the single highest-ranked final-round loser gets the spot (NO lottery); (b) if one withdrawal occurs before qualifying completes, the top-2 ranked losers enter a random draw; (c) for two withdrawals before completion, top-3 go into the draw for 2 slots; (d) the "top-4 random draw" applies when multiple withdrawals occur. This complicates the clean lottery design.
4. **Qualifying data completeness pre-2010 is uncertain.** Match statistics are only available from 2011 onward, though match results (wins/losses with player identities) extend earlier. The entry field reliability requires empirical validation.
5. **Estimated sample size:** ~1,500-2,100 LL entries across ATP events (2007-2024), with ~6,000-6,500 total final-round qualifying losers forming the comparison group.

---

## Ranked Data Sources

### 1. Jeff Sackmann / Tennis Abstract -- ATP

**Feasibility Grade: A**

**Repository:** https://github.com/JeffSackmann/tennis_atp

**Coverage:**
- Main draw matches: 1968--present (~233,000+ matches)
- Qualifying/Challenger matches: 1978--present (files: `atp_matches_qual_chall_YYYY.csv`)
- Match statistics: 1991--present for tour-level main draw; 2011--present for tour-level qualifying
- Rankings: mostly complete 1985--present (intermittent 1973--1984; 1982 missing)
- Ranking files by decade: `atp_rankings_70s.csv` through `atp_rankings_current.csv`
- Player biographical data: `atp_players.csv`

**Key Variables:**
- `winner_entry` / `loser_entry`: Entry type codes including **LL** (lucky loser), Q (qualifier), WC (wild card), PR (protected ranking), SE (special entry). This is the critical treatment variable.
- `winner_rank` / `loser_rank`: ATP ranking at tournament date
- `winner_rank_points` / `loser_rank_points`: Ranking points at tournament date
- `tourney_level`: G (Grand Slam), M (Masters 1000), A (tour-level 500/250), C (Challenger), S (Satellites/ITFs), F (Finals), D (Davis Cup)
- `round`: Q1, Q2, Q3 for qualifying; R128 through F for main draw
- Match statistics: aces, double faults, serve points, 1st/2nd serve metrics, break points
- `surface`, `draw_size`, `best_of`, `minutes`
- Player demographics: age, height, hand, nationality

**Format:** Repeated cross-section at match level. Restructured into player-level panel via weekly rankings merge.

**Access:** Public, free, CC BY-NC-SA 4.0 license. Non-commercial use required (academic research qualifies). Git clone or direct CSV download.

**Known Issues:**
- Qualifying match statistics only from 2011 onward (match results available earlier)
- The `entry` field may not be perfectly populated for all years -- requires empirical validation
- Rankings data has gaps in the 1970s--1980s
- Some matches deleted by maintainer when they failed sanity checks
- Qualifying and Challenger matches combined in a single file per year -- must filter by `tourney_level`
- Maintainer uses "when-in-doubt-exclude" approach, meaning some qualifying matches may be omitted

**Fit Assessment:**
- Treatment identification: **EXCELLENT** -- `entry = "LL"` directly identifies lucky losers
- Outcome measurement: **GOOD** -- ranking points, match wins, ranking trajectory all observable; prize money NOT directly available but imputable
- Running variable: **REQUIRES CONSTRUCTION** -- rank final-round qualifying losers by ATP ranking within each tournament
- Population: **CORRECT** -- professional tennis players at the qualifying/main draw margin
- Variation: **SUFFICIENT** -- estimated ~80-120 LL entries per year across all ATP events
- Time period: **ADEQUATE** -- qualifying data quality likely strongest from ~2007 onward

**Who Used It:** Klaassen and Magnus (2001, 2003, 2009); Sunde (2009); Gonzalez-Diaz et al. (2012); widely used in tennis analytics and sports economics literature.

---

### 2. Jeff Sackmann / Tennis Abstract -- WTA

**Feasibility Grade: A**

**Repository:** https://github.com/JeffSackmann/tennis_wta

**Coverage:** Same structure as ATP. Main draw 1968--present; qualifying/ITF 1968--present.

**Key Variables:** Identical column structure to ATP repository.

**Access:** Public, free, CC BY-NC-SA 4.0.

**Known Issues:**
- The `Tours` column documenting tournament types is "not anywhere near complete" per the README
- ITF, tour-level qualifying, and some ITF-level qualifying are mixed in the qualifying files
- WTA tournament tier structure differs (Premier Mandatory, Premier 5, Premier, International)
- LL identification may be less reliable for WTA than ATP due to less complete data curation

**Fit Assessment:**
- Extends sample to women's tour, potentially doubling statistical power
- WTA follows the same Grand Slam lottery rules and ranking-based selection at regular events
- Important for external validity (gender dimension)
- WTA qualifying draw sizes may differ from ATP -- must verify empirically

---

### 3. ATP Weekly Rankings (from Sackmann Repository)

**Feasibility Grade: A**

**Source:** Included in the tennis_atp repository (ranking files by decade).

**Coverage:** Weekly rankings from 1985--present. Columns: ranking_date, rank, player_id, ranking_points.

**Fit Assessment:**
- **ESSENTIAL** for measuring ranking trajectory outcomes
- Allows construction of: ranking at time of LL entry, ranking at t+4/8/12/26/52 weeks
- Points data allows direct measurement of ranking points gained from the tournament
- Weekly frequency is ideal for event study designs

---

### 4. Maity et al. (2025) Replication Data

**Feasibility Grade: B+**

**Source:** OSF (https://osf.io/pe7cg/) -- availability to be confirmed by visiting URL directly. Also see: https://www.dashunwang.com/academic-articles/early-career-setback-and-future-achievement-in-professional-sports

**Coverage:** 194,840 ATP matches spanning 1915--2019. LL analysis focused on 2007--2019: 2,688 players, 825 tournaments, 54,084 matches.

**Key Variables:** Lucky loser identification, match outcomes, tournament information, player rankings.

**Access:** Public on OSF (pending confirmation of availability).

**Known Issues:**
- Ends at 2019 (5+ years of additional data available)
- Psychology/computational framing -- different variable construction than economics research
- LL identification methodology needs verification against Sackmann data

**Fit Assessment:**
- **VALIDATION DATASET** -- cross-check LL identification for 2007--2019 overlap
- Provides benchmark LL counts for measurement error assessment
- Not sufficient as primary data due to truncated period and limited variable set

---

### 5. Tennis-Data.co.uk (Betting Odds)

**Feasibility Grade: B**

**Source:** http://www.tennis-data.co.uk/alldata.php

**Coverage:** ATP and WTA match results with betting odds from multiple bookmakers (Pinnacle, Bet365, etc.), approximately 2000--present.

**Key Variables:** Pre-match betting odds (implied win probabilities), match results, tournament and round info.

**Access:** Public, free download as Excel/CSV files.

**Known Issues:**
- Likely does NOT include qualifying round matches (main draw only)
- No entry type field -- must merge with Sackmann data
- Coverage of qualifying rounds and LL identification uncertain

**Fit Assessment:**
- **SUPPLEMENTARY** -- betting odds provide market-based expected performance measure
- Can test whether the market correctly prices LL players
- Must be merged with Sackmann data using player names + tournament + date

---

### 6. Tennis Abstract Elo Ratings

**Feasibility Grade: B**

**Source:** https://tennisabstract.com/reports/atp_elo_ratings.html (display only); algorithm computable from match data using code at https://github.com/JeffSackmann/tennis_viz

**Coverage:** Overall + surface-specific Elo ratings. Current ratings displayed on tennisabstract.com. Historical Elo can be computed from the match-level data.

**Fit Assessment:**
- **HIGHLY RELEVANT** for the competitiveness index outcome variable
- Elo ratings are a natural operationalization of E_j[P_t(i->j|X)] -- expected win probability against representative opponents
- Must be computed from scratch using match data rather than downloaded
- The algorithm accounts for opponent strength, which is exactly what the competitiveness index requires
- Surface-specific Elo provides heterogeneity analysis by surface

---

### 7. OnCourt Database

**Feasibility Grade: B-**

**Source:** https://www.oncourt.info/

**Coverage:** 1.5+ million matches since 1990. Includes qualifying draws, LL filters.

**Access:** Paid software -- 48.95 EUR/year or 88.95 EUR lifetime. 30-day free trial.

**Known Issues:** Proprietary software interface; data exportability is the main concern.

**Fit Assessment:** Potential validation source for LL identification. The software's explicit LL filter confirms that entry type data is well-maintained in at least one comprehensive database.

---

### 8. TennisMyLife (TML) Database

**Feasibility Grade: B-**

**Source:** https://github.com/Tennismylife/TML-Database (archived); https://stats.tennismylife.org/ (live)

**Coverage:** ATP tournament matches from ~1977 onward.

**Known Issues:** GitHub no longer actively maintained; provenance less rigorous; unclear licensing.

**Fit Assessment:** Potential cross-validation for entry field completeness. Claims to have integrated missing data from Sackmann's CSVs.

---

### 9. ATP Official Rulebook (Institutional Documentation)

**Feasibility Grade: A (as documentation, not data)**

**Sources:**
- ATP Rulebook Chapter 7: https://www.atptour.com/-/media/files/rulebook/2025/2025-rulebook-chapter-7_the-competition_20may.pdf
- Grand Slam Rulebook 2025: https://www.itftennis.com/media/5986/grand-slam-rulebook-2025-f.pdf

**Key Information:**
- ATP events: LL selection is by strict PIF ATP Rankings order among final-round qualifying losers
- Grand Slams: LL selection depends on TIMING of withdrawal (see critical finding below)
- Points tables by tournament level and round

**Fit Assessment:** **ESSENTIAL** for institutional background and identification strategy design.

---

### 10. Prize Money Data

**Feasibility Grade: B**

**Sources:** https://www.perfect-tennis.com/prize-money/ ; ATP Media Guides

**Fit Assessment:** Allows construction of prize money outcome. Requires manual compilation into lookup table: tournament_level x year x round -> prize_money_USD. Timeline: 2-4 hours.

---

### 11. Sackmann Point-by-Point Data

**Feasibility Grade: C+**

**Repository:** https://github.com/JeffSackmann/tennis_pointbypoint

**Fit Assessment:** NOT REQUIRED for primary question. Could measure whether LL players perform differently under pressure. Coverage of qualifying matches at point level is likely very limited.

---

### 12. Commercial APIs (Sportradar, API-Tennis)

**Feasibility Grade: D**

Not recommended. Public Sackmann data covers the same ground. Cost is prohibitive ($500-1,000+/month).

---

### 13. ATP/WTA Official Websites (Web Scraping)

**Feasibility Grade: C-**

Not recommended. Sackmann data already derives from these sources. Scraping introduces reproducibility and legal concerns.

---

## Critical Institutional Finding: Grand Slam LL Selection Is More Nuanced

Web research revealed that the Grand Slam LL selection rule is **timing-dependent**, which complicates the clean lottery design:

| Withdrawal Timing | Pool Size for Random Draw | Rule |
|-------------------|--------------------------|------|
| After qualifying completes | 1 (no lottery) | Highest-ranked final-round loser gets the spot directly |
| 1 withdrawal before qualifying completes | 2 | Top-2 ranked losers enter random draw for 1 spot |
| 2 withdrawals before qualifying completes | 3 | Top-3 ranked losers enter random draw for 2 spots |
| Multiple withdrawals (general case) | Up to 4 | Top-4 (or fewer) ranked losers enter random draw |

**Implication for identification:** The "top-4 random draw" is NOT the universal rule. When withdrawals occur after qualifying is complete, there is NO lottery -- it reverts to ranking-based selection, just like ATP events. The lottery design is only valid for withdrawals that occur BEFORE qualifying completes.

**What this means for the study:**
1. The lottery sample is SMALLER than the naive estimate of 304 player-tournament observations
2. Researchers must determine WHEN each withdrawal occurred (before or after qualifying completion) to correctly classify which LL entries were lottery-based vs. ranking-based
3. This information is NOT directly available in the Sackmann data -- it may require supplementary research (tournament draw sheets, news reports)
4. The ranking-based ATP RDD becomes even more important as the primary design

**Source:** [Lucky loser - Wikipedia](https://en.wikipedia.org/wiki/Lucky_loser); [Tennis365 - What is a Lucky Loser](https://www.tennis365.com/tennis-features/what-lucky-loser-tennis-rules-how-they-are-chosen-tournament-winners); [Australian Open - How draws are made](https://ausopen.com/articles/news/how-grand-slam-tournament-draws-are-made)

---

## Variable Availability Matrix

| Research Need | Sackmann ATP | Sackmann WTA | Rankings | Betting Odds | Elo (computed) |
|--------------|:---:|:---:|:---:|:---:|:---:|
| LL treatment indicator | YES | YES | -- | -- | -- |
| Final-round qualifying losers | YES | YES | -- | -- | -- |
| Player ranking (running variable) | YES | YES | YES | -- | -- |
| Ranking trajectory (outcome) | -- | -- | YES | -- | -- |
| Ranking points trajectory | -- | -- | YES | -- | -- |
| Match wins/losses | YES | YES | -- | -- | -- |
| Tournament rounds advanced | YES | YES | -- | -- | -- |
| Match statistics (serve, return) | PARTIAL (2011+) | PARTIAL | -- | -- | -- |
| Competitiveness index (Elo) | -- | -- | -- | -- | YES |
| Expected win probability (market) | -- | -- | -- | YES | -- |
| Prize money | IMPUTABLE | IMPUTABLE | -- | -- | -- |
| Player demographics | YES | YES | -- | -- | -- |
| Tournament characteristics | YES | YES | -- | -- | -- |
| Grand Slam lottery eligibility | CONSTRUCTIBLE | CONSTRUCTIBLE | YES | -- | -- |
| Withdrawal timing (GS) | NO | NO | -- | -- | -- |

---

## Gap Analysis

### Gap 1: Qualifying Data Completeness Pre-2010

**Status:** UNCERTAIN -- requires empirical validation

Match statistics for qualifying are only available from 2011 onward. Match results (who won, who lost, with player IDs and rankings) are available earlier from the qual_chall files, but completeness is uncertain. The maintainer's "when-in-doubt-exclude" approach may mean some qualifying rounds are missing entirely for pre-2010 tournaments.

**Mitigation:** (a) Empirically count qualifying tournaments and final-round matches per year to assess coverage; (b) Start the primary sample at 2007 or 2011 depending on completeness; (c) Use pre-2010 data as a robustness check if available.

### Gap 2: LL Entry Field Reliability

**Status:** REQUIRES VALIDATION

The `entry` field is the sole basis for LL treatment identification. If LL entries are sometimes coded as blank (direct acceptance), treated players are misclassified as controls, creating attenuation bias (conservative). The early "Alt" (Alternate) code in older data may also capture some LL entries.

**Mitigation:** (a) Count LL entries per year and compare to expected frequencies; (b) Cross-validate against Maity et al. (2025) for 2007-2019; (c) Check whether "Alt" entries in early data should be reclassified as LL.

### Gap 3: Full Set of Final-Round Qualifying Losers

**Status:** LIKELY CONSTRUCTIBLE but needs verification

The qualifying files should contain all final-round matches, from which losers can be extracted. However, the number of qualifying rounds varies by tournament level (Q2 for ATP 250s, Q3 for Grand Slams/some Masters), requiring programmatic identification of the final round per tournament.

**Mitigation:** For each tournament, compute max(round) among {Q1, Q2, Q3} to identify the final qualifying round. Verify expected counts: 4 losers at ATP 250, 16 at Grand Slams.

### Gap 4: Grand Slam Lottery vs. Ranking-Based LL Selection

**Status:** MORE COMPLEX THAN INITIALLY UNDERSTOOD

The Grand Slam rule depends on withdrawal timing. Withdrawals after qualifying completion are ranking-based (no lottery). Only pre-completion withdrawals trigger a random draw among 2-4 top-ranked losers. The timing of withdrawal is NOT recorded in the Sackmann data.

**Mitigation:** (a) For each Grand Slam LL, research whether the withdrawal occurred before or after qualifying using news reports or tournament draw sheets; (b) As a conservative approach, treat ALL Grand Slam LL entries as potentially ranking-based and use only the ATP RDD; (c) Use the Grand Slam design only when withdrawal timing can be verified.

### Gap 5: Withdrawal Endogeneity

**Status:** MODERATE CONCERN

The number of LL slots per tournament depends on how many main draw players withdrew, which is driven by injuries/illness of MAIN DRAW players. This is plausibly exogenous to qualifying loser characteristics. However, systematic patterns (e.g., more withdrawals at certain tournaments or times of year) could introduce selection.

**Mitigation:** (a) Document withdrawal patterns by tournament and year; (b) Condition on "at least one withdrawal occurred" (standard in lottery designs); (c) Show that qualifying loser characteristics do not predict the number of LL slots.

### Gap 6: Elo Ratings Not Pre-Computed

**Status:** SOLVABLE

Elo ratings are not directly downloadable but can be computed from the match data using Sackmann's algorithm. This is the natural operationalization of the competitiveness index.

**Mitigation:** Implement Elo calculation from match data. Reference code is available at https://github.com/JeffSackmann/tennis_viz. Standard Elo update formula with K-factor calibrated for tennis.

---

## Measurement Validity: Primary Outcome Construction

### Competitiveness Index as Generated Regressor

The primary outcome $C_{it} = \mathbb{E}_j[P_t(i \to j \mid X)]$ is a predicted probability from a first-stage match-outcome model. This creates two concerns:

**Concern 1: Generated regressor bias.** Standard errors in the second-stage RDD will understate uncertainty because they ignore estimation error in the first stage. **Mitigation:** (a) Bootstrap the entire two-stage procedure (estimate match model → compute index → run RDD) to get correct inference. (b) Murphy and Topel (1985) two-step variance correction as an alternative.

**Concern 2: Circularity.** The match-outcome model is estimated on all matches, including LL main draw appearances. If LL entry improves performance, these matches are influenced by treatment, biasing the index. **Mitigation:** (a) Estimate the match model EXCLUDING all main draw matches by LL entrants. The index then captures "how good the player would be expected to perform" based on non-LL matches only. (b) As robustness, estimate the model on pre-LL-entry matches only for each player.

**Concern 3: Model specification sensitivity.** The competitiveness index depends on the functional form of the match-outcome model. **Mitigation:** (a) Report results using multiple specifications: logit, random forest, Elo-based. (b) Use Elo as the primary robustness measure — it avoids the generated regressor problem entirely and is standard in the tennis literature.

### Elo as Alternative/Robustness Outcome

Elo ratings provide a model-free competitiveness measure that updates after every match using a known formula. Advantages:
- No generated regressor problem (Elo is computed sequentially, not estimated)
- Standard in tennis analytics (Sackmann, tennisabstract.com)
- Surface-specific variants available (hard, clay, grass)
- Can be computed for every player in the sample from match data

**Decision:** Competitiveness index is the primary outcome (novel contribution); Elo is the primary robustness check. If both tell the same story, the result is robust to outcome measurement.

---

## Running Variable Feasibility: Mass Points Near Cutoff

### The Discrete Running Variable Challenge

The running variable — ranking position among final-round qualifying losers — is discrete with VERY FEW mass points per tournament:

| Tournament Tier | Qualifying Draw Size | Final-Round Losers | Mass Points |
|----------------|---------------------|-------------------|-------------|
| ATP 250 (28 draw) | 12-16 qualifying spots, Q2 final round | ~4-6 | 4-6 |
| ATP 250 (32 draw) | 16 qualifying spots, Q2 final round | ~6-8 | 6-8 |
| ATP 500 | 8-12 qualifying spots | ~4-6 | 4-6 |
| ATP Masters 1000 | 24-32 qualifying spots, Q3 final round | ~12-16 | 12-16 |
| Grand Slams | 128 draw, 32 qualifying spots, Q3 | 16 | 16 |

**Implication:** At ATP 250 events, the RDD is essentially a comparison of the player ranked 1st among losers (who gets the LL spot) vs. the player ranked 2nd-4th. This is a VERY coarse running variable.

**Why this still works:**
1. **Pooling across tournaments.** While each tournament has few mass points, pooling 60+ tournaments/year × 18 years gives thousands of observations at each normalized distance from the cutoff ($\tilde{R} = -1, 0, 1, 2, ...$).
2. **The local randomization framework is designed for this.** Cattaneo, Idrobo, and Titiunik (2020, 2024) explicitly develop methods for RDD with few mass points. The test is a permutation/Fisher test within a window, not asymptotic inference.
3. **Multiple LL slots per tournament.** When 2+ withdrawals occur, more players are treated, creating more variation near the cutoff.

**Empirical verification needed:** Before committing to the RDD, we MUST:
1. Tabulate the distribution of $\tilde{R}_{it}$ (normalized running variable) across all pooled tournaments
2. Verify that there are sufficient observations at $\tilde{R} = \{-2, -1, 0, 1, 2\}$ for the local randomization test
3. Run `rdpower` (Cattaneo, Titiunik, and Vazquez-Bare 2019) to compute formal power at the pooled level

**Added to data construction tasks:** Step 5b — tabulate running variable distribution and verify mass point density near cutoff.

---

## Sample Selection and Attrition

### Selection Into Qualifying

Players in qualifying are not a random sample of tennis professionals. They are typically ranked ~80-300 (ATP) and strategically choose which tournaments to enter. If LL opportunities are more common at certain tournament types, and players self-select into those, the analysis sample is not representative of all marginal professionals.

**Why this is not a first-order concern for the RDD:** The RDD compares players within the SAME tournament and qualifying round. Self-selection into the tournament is absorbed — both treatment and control groups chose the same event. The identifying variation is ranking position AMONG those who chose the same tournament, which is determined by accumulated results over 52 weeks.

### Sport-Exit Attrition

If LL entry (and the associated points/confidence boost) prevents a player from exiting professional tennis, then the treatment group has more complete outcome trajectories than controls. Players who don't get the LL spot and subsequently retire have missing outcome data, creating survival bias.

**Magnitude assessment:** Professional tennis players at the qualifying level are typically invested in multi-year careers. Exit within 6-12 months of a single LL opportunity is rare for this population. However, the concern is more relevant for long-term outcomes (1-3 years).

**Mitigation:**
1. Report attrition rates by treatment status at each follow-up horizon
2. Use Lee (2009) bounds to bound the treatment effect under worst-case attrition
3. Define "exit from sport" as an outcome itself — does LL entry affect career survival?
4. For the primary 3-6 month window, attrition should be minimal

---

## WTA Data Quality Assessment (Revised)

**Feasibility Grade: B+ (downgraded from A)**

The WTA data has the same structure as ATP but with known reliability concerns:
- The `Tours` column documenting tournament types is "not anywhere near complete" (README)
- LL identification may be less reliable than ATP
- ITF, tour-level qualifying, and some ITF-level qualifying are mixed in qualifying files

**Recommendation:** Treat WTA as a CONDITIONAL extension. Start with ATP-only analysis. Before pooling, validate WTA LL coding by: (a) counting LL entries per year and checking plausibility, (b) verifying that ranking-based LL selection holds at WTA events (same institutional rules), (c) comparing entry field completeness between ATP and WTA. Only pool if WTA validation passes.

---

## Power and Sample Size Estimates

### ATP Ranking-Based RDD (Primary Design)

- ~60-65 ATP tour-level events per year with qualifying
- ~6 final-round losers per event on average (varies: 4 at ATP 250, 16 at Grand Slams)
- ~360 final-round losers per year
- Sample period: 2007-2024 (18 years)
- Total final-round losers: ~6,000-6,500
- Total LL entries: ~1,500-2,100
- Effective sample near cutoff (30-50% of total): ~2,000-3,000
- **Power assessment:** For standardized effect of 0.1-0.2 SD, power ~0.60-0.85 at alpha=0.05. **Adequate.**

### Grand Slam Lottery Design (Secondary -- Smaller Than Initially Estimated)

- 4 Grand Slams per year, 2006-2024 (19 years) = 76 tournament observations
- Not all withdrawals trigger a lottery (only pre-completion withdrawals)
- True lottery observations likely FEWER than the initially estimated 304
- **Power assessment:** Underpowered for small effects. Useful only as validation/supplementary evidence.

### Recommendation

The ATP RDD is the **primary identification strategy** (larger sample, adequate power). The Grand Slam lottery should be a **secondary/validation strategy** only if withdrawal timing can be verified, and should be presented with appropriate caveats about the timing-dependent rule.

---

## Recommended Data Strategy

### Primary Dataset
**Sackmann ATP + Rankings** (Sources 1 + 3): Match-level data with LL identification, merged with weekly rankings panel.

### Secondary Dataset
**Sackmann WTA + Rankings** (Source 2): Extends to women's tour for external validity and doubled sample.

### Validation
**Maity et al. (2025) Replication Data** (Source 4): Cross-check LL identification for 2007-2019.

### Supplementary
- **Tennis-Data.co.uk** (Source 5): Betting odds as control/additional outcome
- **Elo Ratings** (Source 6): Compute from match data for competitiveness index
- **Prize Money** (Source 10): Construct earnings outcome
- **ATP/Grand Slam Rulebooks** (Source 9): Institutional documentation

### Not Recommended
- Commercial APIs (Source 12): Too expensive, no marginal benefit
- Point-by-point data (Source 11): Not needed for primary question
- Web scraping (Source 13): Sackmann already provides this

---

## Institutional Consistency Over Time

The LL selection rules may have changed over the 2000-2024 sample period. The data assessment cites the 2025 ATP Rulebook, but earlier editions should be checked. Key questions:
- Was the ranking-based LL selection rule consistent across all years?
- Did the number of qualifying spots or draw sizes change?
- Were there rule changes around 2007-2009 that coincide with the start of reliable qualifying data?

**Mitigation:** (a) Check archived ATP Rulebooks (if available) for pre-2015 LL selection rules. (b) Empirically test whether the ranking-LL concordance rate varies across years. (c) If rule changes are found, restrict the sample or include structural break indicators.

---

## Prior Work and Existing Code

The researcher has existing R code (`AnnotatedCode.R` in the parent directory `C:\Users\maste\Documents\TennisLL\`) that constructs:
- ATP + WTA data pipeline from Sackmann's GitHub CSVs (1968-2024)
- Tiebreak extraction (count and winner/loser tiebreak wins)
- Cumulative match statistics by tournament and player (e.g., aces) using `data.table`
- Tour indicator variable (ATP/WTA)

This code provides a starting point for the data construction pipeline. It does NOT yet construct: LL flags, running variable, qualifying round identification, Elo ratings, or the competitiveness index.

---

## Critical Data Construction Tasks (Ordered)

1. **Clone Sackmann ATP/WTA repositories** (10 min)
2. **Audit LL entry field** by year -- count LL entries, flag temporal patterns (1-2 hours)
3. **Audit qualifying round completeness** by year and tournament level (2-3 hours)
4. **Construct final-round qualifying losers sample** per tournament (2-3 hours)
5. **Build the running variable** -- rank losers by ATP ranking within each tournament (2-3 hours)
5b. **Tabulate running variable distribution** -- verify mass point density near cutoff for RDD feasibility (1-2 hours)
6. **Cross-validate LL counts** against Maity et al. for 2007-2019 (2-3 hours)
7. **Verify ranking-LL concordance** at ATP events (3-4 hours)
8. **Merge with rankings panel** for outcome trajectories (2-3 hours)
9. **Compute Elo ratings** from match history (4-6 hours)
10. **Investigate Grand Slam withdrawal timing** for lottery subsample (4-6 hours)
11. **Formal power calculations** using `rdpower` package (2-3 hours)
12. **Validate WTA LL coding** before pooling with ATP (2-3 hours)
13. **Check institutional consistency** of LL rules across sample period (2-3 hours)
14. **Assess sport-exit attrition** rates by treatment status at each horizon (1-2 hours)

**Total estimated time:** 4-5 days for full data assembly and validation.

---

## Sources

- [Jeff Sackmann tennis_atp GitHub](https://github.com/JeffSackmann/tennis_atp)
- [Jeff Sackmann tennis_wta GitHub](https://github.com/JeffSackmann/tennis_wta)
- [Jeff Sackmann tennis_viz GitHub](https://github.com/JeffSackmann/tennis_viz)
- [Lucky loser - Wikipedia](https://en.wikipedia.org/wiki/Lucky_loser)
- [Tennis365 - What is a Lucky Loser](https://www.tennis365.com/tennis-features/what-lucky-loser-tennis-rules-how-they-are-chosen-tournament-winners)
- [Australian Open - How Grand Slam draws are made](https://ausopen.com/articles/news/how-grand-slam-tournament-draws-are-made)
- [ATP Rulebook Chapter 7 (2025)](https://www.atptour.com/-/media/files/rulebook/2025/2025-rulebook-chapter-7_the-competition_20may.pdf)
- [Grand Slam Rulebook 2025](https://www.itftennis.com/media/5986/grand-slam-rulebook-2025-f.pdf)
- [Let's Go Tennis - Lucky Loser Rules 2025](https://letsgotennis.com/tennis-tips/lucky-loser-tennis/)
- [Maity et al. - Early Career Setback (Dashun Wang page)](https://www.dashunwang.com/academic-articles/early-career-setback-and-future-achievement-in-professional-sports)
- [Tennis Abstract - ATP Elo Ratings](https://tennisabstract.com/reports/atp_elo_ratings.html)
- [Tennis-Data.co.uk](http://www.tennis-data.co.uk/alldata.php)
