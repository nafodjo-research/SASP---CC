# Data Sources Assessment: Lucky Losers in Professional Tennis

**Date:** 2026-03-20 (Revised: Round 3)
**Research Question:** Causal impact of Lucky Loser (LL) entry on subsequent match performance, ranking progression, tournament access, and career outcomes.
**Identification Strategies:** (1) RDD at the ranking cutoff among final-round qualifying losers at ATP events (primary); (2) Grand Slam lottery design among top-ranked final-round qualifying losers (secondary, with caveats).

---

## CRITICAL UPDATE (Round 3): Grand Slam LL Selection Is Timing-Dependent

Web verification of the Grand Slam LL selection rule revealed a critical nuance not captured in Rounds 1-2. The "top-4 random draw" is NOT the universal Grand Slam rule. The pool size and selection mechanism depend on WHEN the main draw withdrawal occurs relative to the completion of qualifying:

| Withdrawal Timing | Pool Size | Selection Method |
|-------------------|-----------|-----------------|
| After qualifying completes | 1 | Highest-ranked final-round loser gets the spot directly (NO lottery) |
| 1 withdrawal before qualifying completes | 2 | Top-2 ranked losers enter random draw for 1 spot |
| 2 withdrawals before qualifying completes | 3 | Top-3 ranked losers enter random draw for 2 spots |
| Multiple withdrawals (general case) | Up to 4 | Top-4 ranked losers enter random draw |

**Sources:** [Lucky loser - Wikipedia](https://en.wikipedia.org/wiki/Lucky_loser); [Tennis365](https://www.tennis365.com/tennis-features/what-lucky-loser-tennis-rules-how-they-are-chosen-tournament-winners); [Australian Open](https://ausopen.com/articles/news/how-grand-slam-tournament-draws-are-made)

**Implications for identification:**
1. The lottery sample is SMALLER than the Round 2 estimate of ~304 player-tournament observations in the eligible pool, because post-completion withdrawals are ranking-based.
2. Withdrawal timing is NOT recorded in the Sackmann data. Determining which GS LL entries were lottery-based vs. ranking-based requires supplementary research (news reports, tournament draw sheets).
3. This strengthens the case for the ATP ranking-based RDD as the primary design.
4. The Grand Slam lottery design should be presented with full transparency about the timing-dependent rule.

---

## Issue 1 Resolution: Grand Slam Lottery Pool Reconstructability

### Can the full LL-eligible pool be reconstructed from the Sackmann data?

**Yes, with high confidence for the FULL pool; with uncertainty about which entries used the lottery mechanism.** The Sackmann qualifying match files (`atp_matches_qual_chall_YYYY.csv`) contain match-level results for qualifying rounds at tour-level events. The round column uses values Q1, Q2, and Q3 (for Grand Slams with three qualifying rounds). The reconstruction logic is:

1. **Identify final-round qualifying losers:** Filter the qualifying file for Grand Slam tournaments (`tourney_level = "G"`) and the final qualifying round (`round = "Q3"`). The losers of these matches are all final-round qualifying losers. At each Grand Slam, 128 players enter qualifying, play 3 rounds, and 16 qualify. This means exactly 16 players lose in Q3 at each Grand Slam.

2. **Determine who became an LL:** Cross-reference with the main draw file (`atp_matches_YYYY.csv`) for the same tournament. Any player appearing with `winner_entry = "LL"` or `loser_entry = "LL"` was selected as a lucky loser. This identifies the treated players.

3. **Identify the eligible pool:** Per Grand Slam rules (since 2006), the LL pool consists of the top-ranked players among the 16 final-round losers. The pool size varies from 1 to 4 depending on withdrawal timing (see Critical Update above). Using the `loser_rank` field from the qualifying match or the weekly rankings file closest to the tournament date, rank the 16 Q3 losers.

4. **Construct treatment and control:** Among the pool members, those who appear as LLs in the main draw are treated; those who do not are controls. For pre-completion withdrawals, assignment within the pool is by random draw. For post-completion withdrawals, assignment is by ranking order.

### Verification steps required

- **Step A:** Confirm that Q3 round data is populated for Grand Slam qualifying from 2006 onward. Grand Slam qualifying has 128 players and 3 rounds, so Q3 should be present. Must be verified empirically.

- **Step B:** Count the number of Q3 losers per Grand Slam per year. Expected count is 16. Deviations (walkovers, retirements) should be documented.

- **Step C:** Cross-validate LL counts against the main draw. For each Grand Slam, count `entry = "LL"` appearances in the main draw. Typical counts are 0-4 LLs per Grand Slam.

- **Step D:** Verify that the `loser_rank` field in qualifying files is populated. If not, merge with weekly rankings using `player_id` and the ranking date closest to (but before) the tournament start date.

- **Step E (NEW):** For each Grand Slam LL entry, determine whether the withdrawal occurred before or after qualifying completion. This requires consulting news reports, tournament draw sheets, or official announcement timing. This step is ESSENTIAL for correctly classifying lottery vs. ranking-based LL entries.

### What CANNOT be verified from the data alone

- **The randomization itself:** The data records who became an LL, not the randomization draw. We rely on the institutional rule that selection is by random draw for pre-completion withdrawals.

- **Player availability:** A player in the eligible pool may have declined the LL spot. This is analogous to non-compliance in an RCT and can be handled via ITT or IV.

- **Withdrawal timing:** The data does not record when the withdrawal occurred relative to qualifying completion. This is the key limitation for the Grand Slam lottery design.

### Bottom line on the lottery design

The lottery pool is partially reconstructable. The design is feasible but has THREE limitations (updated from two): (a) the sample is smaller than initially estimated because not all GS LL entries are lottery-based, (b) non-compliance (declining the LL spot) cannot be directly observed, and (c) determining which entries are lottery-based requires supplementary research. The lottery design should be presented as a **clean but low-powered and partially verifiable** identification strategy.

---

## Issue 4 Resolution: Power and Sample Size Estimates

### Grand Slam Lottery Design (REVISED DOWNWARD)

**Pool construction:**
- 4 Grand Slams per year
- 16 final-round qualifying losers per Grand Slam
- Pool size varies from 1 to 4 depending on withdrawal timing (see Critical Update)
- Sample period: 2006--2024 (19 years)
- Upper bound on lottery observations: ~4 x 4 x 19 = ~304 (if all withdrawals were pre-completion)
- Realistic estimate: Perhaps 50-70% of withdrawals occur before qualifying completes, reducing the true lottery sample

**Revised power assessment:** The effective lottery sample is likely ~150-200 player-tournament observations in the top-4 pool, with ~60-100 treated. This is **severely underpowered** for detecting modest effects and should be treated as supplementary evidence only.

### ATP Ranking-Based RDD (UNCHANGED)

**Pool construction:**
- ~60-65 ATP tour-level events per year with qualifying
- ~360 final-round qualifying losers per year
- Sample period: 2007--2024 (18 years)
- Total final-round losers: ~6,000-6,500
- Total LL entries: ~1,500-2,100
- Effective sample near cutoff: ~2,000-3,000

**Power assessment for RDD:** For a standardized effect of 0.1-0.2 SD, power at alpha=0.05 is approximately 0.60-0.85. **Adequate for the primary analysis.**

### Recommendation

The ATP RDD should be the **primary identification strategy** (larger sample, adequate power). The Grand Slam lottery should be a **supplementary/validation strategy** only, presented with full transparency about the timing-dependent rule.

---

## Ranked Data Sources

### 1. Jeff Sackmann / Tennis Abstract -- ATP (Primary Candidate)

**Feasibility Grade: A**

**Repository:** https://github.com/JeffSackmann/tennis_atp

**Coverage:**
- Main draw matches: 1968--present (~233,000+ matches)
- Qualifying/Challenger matches: 1978--present (files: `atp_matches_qual_chall_YYYY.csv`)
- Match statistics available: 1991--present for tour-level, 2011--present for tour-level qualifying
- Rankings: mostly complete 1985--present (intermittent 1973--1984, missing 1982)
- Ranking files by decade: `atp_rankings_70s.csv` through `atp_rankings_current.csv`
- Player biographical data: `atp_players.csv`

**Key Variables:**
- `winner_entry` / `loser_entry`: Entry type codes including **LL** (lucky loser), Q (qualifier), WC (wild card), PR (protected ranking), SE (special entry). This is the critical treatment variable.
- `winner_rank` / `loser_rank`: ATP ranking at tournament date
- `winner_rank_points` / `loser_rank_points`: Ranking points at tournament date
- `tourney_level`: G (Grand Slam), M (Masters 1000), A (tour-level), C (Challenger), S (Satellites/ITFs), F (finals), D (Davis Cup)
- `round`: Round designation -- Q1, Q2, Q3 for qualifying rounds; R128, R64, R32, R16, QF, SF, F for main draw
- `score`: Match score (sets, games)
- `surface`: Playing surface
- `winner_age` / `loser_age`, `winner_ht` / `loser_ht`, `winner_hand` / `loser_hand`, `winner_ioc` / `loser_ioc`
- Match statistics: aces, double faults, serve points, 1st serve in/won, 2nd serve won, serve games, break points saved/faced (for both winner and loser)

**Format:** Repeated cross-section at match level. Can be restructured into player-level panel by linking player IDs across matches and merging with weekly ranking files.

**Access:** Public, free, CC BY-NC-SA 4.0 license. Non-commercial use required -- fine for academic research.

**Known Issues:**
- Qualifying round match statistics only available from 2011 onward (match results available earlier)
- The `entry` field for LL may not be perfectly populated for all years; needs validation (see Issue 3 below)
- Rankings data has gaps in the 1970s-1980s
- Some matches deleted due to failed sanity checks (loser won 60% of points, match time under 20 minutes)
- Qualifying and Challenger matches are combined in a single file per year; need to filter by `tourney_level`
- The repository maintainer takes a "when-in-doubt-exclude" approach, which may result in some qualifying matches being omitted

**Fit Assessment:**
- Treatment identification: **EXCELLENT** -- `winner_entry = "LL"` or `loser_entry = "LL"` directly identifies lucky losers in main draw matches
- Outcome measurement: **GOOD** -- ranking points, match wins, and ranking trajectory observable through rankings files; prize money NOT directly available but can be imputed from tournament level and round
- Running variable: **REQUIRES CONSTRUCTION** -- see Issue 2 / data_dictionary.md for detailed construction method
- Population: **CORRECT** -- professional tennis players at the qualifying/main draw margin
- Variation: **SUFFICIENT** -- estimated ~80-120 LL entries per year across all tournaments
- Time period: **ADEQUATE** -- qualifying data from ~2007 onward; Grand Slam lottery design from 2006 onward

**Who Used It:**
- Maity et al. (2025) used ATP data from atptour.com but the same underlying information
- Punta/Zappala et al. (2024) used ATP data for career trajectory analysis
- Klaassen and Magnus (2001, 2003, 2009) used ATP match data
- Sunde (2009), Silverman and Seidel (2016) used ATP tournament data

---

### 2. Jeff Sackmann / Tennis Abstract -- WTA (Primary Candidate, Complement)

**Feasibility Grade: A**

**Repository:** https://github.com/JeffSackmann/tennis_wta

**Coverage:**
- Qualifying/ITF matches: 1968--2024 (files: `wta_matches_qual_itf_YYYY.csv`)
- Main draw matches: 1968--present
- Rankings: similar coverage to ATP

**Key Variables:** Same column structure as ATP repository.

**Access:** Public, free, CC BY-NC-SA 4.0.

**Known Issues:**
- The `Tours` column documenting tournament types is "not anywhere near complete" per the README
- ITF, tour-level qualifying, and some ITF-level qualifying are mixed in the qualifying files
- WTA tournament structure differs somewhat from ATP (different tier system: P, PM, I, etc.)
- Lucky loser identification may be less reliable for WTA than ATP due to less complete data curation

**Fit Assessment:**
- Extends sample to women's tour, potentially doubling statistical power
- Important for external validity (ATP + WTA results)
- Same entry type coding (LL, Q, WC) should apply
- WTA LL selection mechanism follows the same rules as ATP (ranking-based for regular events, lottery at Grand Slams)
- WTA qualifying draw sizes may differ from ATP -- must verify

---

### 3. Maity et al. (2025) Replication Data

**Feasibility Grade: B+**

**Repository:** https://osf.io/pe7cg/

**Coverage:**
- 194,840 ATP matches spanning 1915--2019
- Lucky loser analysis focused on 2007--2019: 2,688 players, 825 tournaments, 54,084 matches
- Source: atptour.com (official ATP website scrape)

**Key Variables:**
- Lucky loser identification (their treatment variable)
- Match wins/losses
- Tournament information
- Player rankings

**Access:** Public, available on OSF (Open Science Framework).

**Known Issues:**
- Stops at 2019 (5+ years of additional data available)
- Focused on match wins, not economic outcomes (ranking points, earnings)
- Their LL identification methodology needs verification against Sackmann data
- Psychology/computational framing means variables may not be optimally structured for economics research

**Fit Assessment:**
- Useful as a **validation dataset** -- cross-check your LL identification against theirs for the overlapping 2007--2019 period
- Maity et al. report specific LL counts that can serve as a benchmark for measurement error assessment
- Not sufficient as primary data due to truncated time period and limited variable set

---

### 4. ATP Weekly Rankings (from Sackmann Repository)

**Feasibility Grade: A**

**Source:** Included in the tennis_atp repository (ranking files by decade)

**Coverage:**
- Weekly rankings from 1985--present (with some gaps in 1970s--1984)
- Columns: ranking_date, ranking, player_id, ranking_points (where available)

**Key Variables:**
- Weekly ranking position
- Ranking points (available for most of the modern era)
- Player ID for merging with match data

**Format:** Repeated cross-section at player-week level. Naturally forms a panel.

**Fit Assessment:**
- **ESSENTIAL** for measuring ranking trajectory outcomes
- Allows construction of: ranking at time of LL entry, ranking 4/8/12/26/52 weeks after entry, ranking slope before/after
- Points data allows direct measurement of ranking points gained from the tournament
- Weekly frequency is ideal for event study designs

---

### 5. Tennis Abstract Elo Ratings (Computable)

**Feasibility Grade: B+**

**Source:** Algorithm described at https://www.tennisabstract.com/blog/2019/12/03/an-introduction-to-tennis-elo/ ; reference code at https://github.com/JeffSackmann/tennis_viz ; current ratings displayed at https://tennisabstract.com/reports/atp_elo_ratings.html

**Coverage:** Computable for any player with match history in the Sackmann data. Overall + surface-specific Elo available.

**Key Variables:** Elo rating (overall), surface-specific Elo, Elo rating changes per match.

**Access:** Must be computed from match data. Algorithm is public. Not directly downloadable as a pre-computed panel.

**Fit Assessment:**
- **HIGHLY RELEVANT** for the competitiveness index E_j[P_t(i->j|X)]
- Elo ratings naturally operationalize expected win probability against representative opponents
- The Sackmann algorithm includes surface-specific variants (grass, clay, hard court Elo)
- Computation requires processing full match history but is straightforward
- Provides a richer outcome measure than ranking position alone

---

### 6. Tennis-Data.co.uk (Betting Odds Data)

**Feasibility Grade: B**

**Source:** http://www.tennis-data.co.uk/alldata.php

**Coverage:**
- ATP and WTA match results with betting odds
- Approximately 2000--present
- Odds from multiple bookmakers (Pinnacle, Bet365, etc.)

**Key Variables:**
- Pre-match betting odds (implied win probabilities)
- Match results
- Tournament and round information

**Access:** Public, free download as Excel/CSV files.

**Known Issues:**
- Likely does NOT include qualifying round matches (betting data typically covers main draw only)
- No entry type field (LL, Q, WC) -- would need to merge with Sackmann data
- Coverage of qualifying rounds and lucky loser identification uncertain

**Fit Assessment:**
- **SUPPLEMENTARY** -- betting odds provide a market-based measure of expected performance
- Can test whether the market correctly prices LL players
- Useful as a control variable (pre-match expected win probability)
- Must be merged with primary Sackmann data using player names + tournament + date

---

### 7. OnCourt Database

**Feasibility Grade: B-**

**Source:** https://www.oncourt.info/

**Coverage:** 1.5+ million matches since 1990. Qualifying draws, LL filters available.

**Access:** Paid software -- 48.95 EUR/year or 88.95 EUR lifetime. 30-day free trial.

**Fit Assessment:** Potential validation source. The software's explicit LL filter confirms entry type data is well-maintained. Main concern is data exportability.

---

### 8. TennisMyLife (TML) Database

**Feasibility Grade: B-**

**Source:** https://github.com/Tennismylife/TML-Database (archived); https://stats.tennismylife.org/ (live)

**Fit Assessment:** Potential cross-validation for entry field completeness. Less established in academic use.

---

### 9. Sackmann Point-by-Point Data

**Feasibility Grade: C+**

**Repository:** https://github.com/JeffSackmann/tennis_pointbypoint

**Fit Assessment:** NOT REQUIRED for the primary research question. Could measure pressure performance. Coverage of qualifying matches at point level is very limited.

---

### 10. Prize Money Data (Perfect Tennis, ATP Media Guides)

**Feasibility Grade: B**

**Sources:** https://www.perfect-tennis.com/prize-money/ ; ATP Media Guides (annual PDFs)

**Fit Assessment:** Allows construction of prize money outcome. Requires manual compilation: tournament_level x year x round -> prize_money_USD. Timeline: 2-4 hours.

---

### 11. ATP Official Rulebook (Institutional Documentation)

**Feasibility Grade: A (as documentation, not data)**

**Sources:**
- ATP Chapter 7: https://www.atptour.com/-/media/files/rulebook/2025/2025-rulebook-chapter-7_the-competition_20may.pdf
- Grand Slam Rulebook: https://www.itftennis.com/media/5986/grand-slam-rulebook-2025-f.pdf

**Fit Assessment:** ESSENTIAL for institutional background. Documents the two distinct assignment mechanisms and the timing-dependent Grand Slam rule.

---

### 12. Commercial APIs (Sportradar, API-Tennis, Enetpulse)

**Feasibility Grade: D**

Not recommended. Public Sackmann data covers the same ground. Cost is prohibitive.

---

### 13. ATP/WTA Official Websites (Web Scraping)

**Feasibility Grade: C-**

Not recommended. Sackmann data already derives from these sources. However, official tournament draw sheets may be needed to verify Grand Slam withdrawal timing for the lottery design.

---

## Issue 3 Resolution: Measurement Error in the Entry Field

### Nature of the concern

The `winner_entry` and `loser_entry` fields in the Sackmann data are the sole basis for identifying LL treatment status in the main draw. If this field has systematic missingness, the treatment variable is mismeasured.

### What is known

- The Sackmann data dictionary lists LL as a valid entry code alongside WC, Q, PR, SE, and ITF.
- The repository maintainer uses a "when-in-doubt-exclude" approach to data quality.
- The `examples.py` file in the repository demonstrates filtering by `loser_entry == 'LL'` for Grand Slam matches, confirming the field is well-established.
- Qualifying match statistics are only available from 2011 onward, but match results are available earlier.
- The entry field is populated at the tournament-match level; if a match is in the data, the entry field is typically present.

### Required empirical validation

1. **Count LL entries per year** in main draw files from 2000-2024. Expected: ~80-120 per year for post-2007 years.
2. **Count missingness of the entry field.** Blank entry means "direct acceptance" (NOT missing data). Check whether some LL entries are miscoded as blank.
3. **Cross-validate against Maity et al.** for 2007-2019. Discrepancies > 10% would indicate systematic missingness.
4. **Check for temporal patterns.** If LL coding improves over time, earlier years have systematic undercounting.

### Expected direction of measurement error

If LL entries are miscoded as blank, this is one-directional: treated players misclassified as controls. This biases treatment effects toward zero (attenuation). The bias is conservative -- positive findings remain credible. If missingness > 15% in early years, restrict sample to post-2010/2011 as robustness check.

---

## Issue 5 Resolution: External Validity

### Across tournament levels

- **Grand Slams:** Qualifying players typically ranked ~100-250
- **Masters 1000:** Large draws, well-documented qualifying, ranking-based LL selection
- **ATP 500/250:** Smaller qualifying draws, lower stakes, bulk of RDD sample
- **Heterogeneity by level is itself interesting:** Does the size of the opportunity shock matter?

### Rule changes affecting the sample

- **2006:** Grand Slams switch to timing-dependent lottery/ranking for LL selection
- **2009:** ATP ranking points restructured (Masters 1000 created, points scale changed)
- **2019:** Davis Cup format changed
- **2024:** Further ranking points restructuring. Consider ending sample at 2023.

### ATP vs. WTA

- WTA follows same Grand Slam and regular-event LL rules
- WTA data quality reportedly lower, especially for qualifying
- Including WTA approximately doubles sample and adds gender dimension

---

## Summary: Recommended Data Strategy

### Primary Dataset
**Sackmann ATP + Rankings** (Sources 1 + 4): Match-level data with LL identification via `winner_entry`/`loser_entry` fields, merged with weekly rankings panel.

### Secondary Dataset
**Sackmann WTA + Rankings** (Source 2): Extends to women's tour for external validity.

### Competitiveness Index
**Elo Ratings** (Source 5): Compute from match data for the primary outcome variable E_j[P_t(i->j|X)].

### Validation
**Maity et al. (2025) Replication Data** (Source 3): Cross-check LL identification for 2007-2019.

### Supplementary
- **Tennis-Data.co.uk** (Source 6): Betting odds
- **Prize Money Data** (Source 10): Earnings outcome
- **ATP/GS Rulebooks** (Source 11): Institutional documentation
- **TennisMyLife** (Source 8): Cross-validation

### Not Recommended
- Commercial APIs (Source 12): Too expensive
- Point-by-point data (Source 9): Not needed
- Web scraping (Source 13): Already covered by Sackmann

---

## Critical Data Construction Tasks (Ordered)

1. **Clone Sackmann ATP/WTA repositories** (10 min)
2. **Audit LL entry field** by year -- count LL entries, flag temporal patterns (1-2 hours)
3. **Audit qualifying round completeness** by year and tournament level (2-3 hours)
4. **Construct final-round qualifying losers sample** per tournament (2-3 hours)
5. **Build the running variable** -- rank losers by ATP ranking within each tournament (2-3 hours)
6. **Cross-validate LL counts** against Maity et al. for 2007-2019 (2-3 hours)
7. **Verify ranking-LL concordance** at ATP events (3-4 hours)
8. **Merge with rankings panel** for outcome trajectories (2-3 hours)
9. **Compute Elo ratings** from match history (4-6 hours)
10. **Investigate Grand Slam withdrawal timing** for lottery subsample (4-6 hours)
11. **Formal power calculations** using `rdpower` package (2-3 hours)

**Total estimated time:** 3-4 days for full data assembly and validation.

---

## Key Institutional Details for Identification

**ATP Events (non-Grand Slam):** LL selection is by strict PIF ATP Rankings order among final-round qualifying losers. The highest-ranked loser gets the first LL spot. This creates a **sharp RDD** at the ranking cutoff among final-round losers. The running variable is the player's position in the ranking-ordered list of final-round losers.

**Grand Slams (since 2006):** LL selection is TIMING-DEPENDENT:
- If withdrawal occurs AFTER qualifying completes: highest-ranked final-round loser gets the spot directly (ranking-based, NOT a lottery)
- If withdrawal occurs BEFORE qualifying completes with 1 vacancy: top-2 ranked final-round losers enter random draw
- If withdrawal occurs BEFORE qualifying completes with 2 vacancies: top-3 ranked losers enter random draw for 2 spots
- General case with multiple pre-completion withdrawals: up to top-4 enter random draw

This was adopted in 2006 after the Gimelstob incident at 2005 Wimbledon, where a high-ranked qualifying player had perverse incentives to lose the final qualifying round strategically.

**Recommendation:** Present the ATP ranking-based RDD as the **primary identification strategy** (larger sample, adequate power, clean sharp design). The Grand Slam lottery should be **supplementary/validation** only, with full transparency about the timing-dependent rule and the subset of entries where the lottery mechanism applies.
