# Access Instructions: Tennis Lucky Loser Data Sources

**Date:** 2026-03-20 (Revised: Round 3)

---

## 1. Jeff Sackmann / Tennis Abstract -- ATP (PRIMARY)

**URL:** https://github.com/JeffSackmann/tennis_atp

**Access Method:** Git clone or direct CSV download from GitHub

**Steps:**
1. Clone repository: `git clone https://github.com/JeffSackmann/tennis_atp.git`
2. Key files to use:
   - `atp_matches_YYYY.csv` (main draw, 1968--present)
   - `atp_matches_qual_chall_YYYY.csv` (qualifying + challengers, 1978--present)
   - `atp_rankings_XXs.csv` (rankings by decade)
   - `atp_rankings_current.csv` (current year rankings)
   - `atp_players.csv` (player biographical data)
   - `matches_data_dictionary.txt` (column definitions)
   - `examples/examples.py` (demonstrates entry field filtering including LL)
3. All files are CSV format, directly loadable in R/Python/Stata

**License:** CC BY-NC-SA 4.0 (Creative Commons Attribution-NonCommercial-ShareAlike)
- Attribution required (cite Jeff Sackmann / Tennis Abstract)
- Non-commercial use only (academic research qualifies)
- Share-alike: derivative works must use same license

**Timeline:** Immediate (public repository, no application needed)

**Size:** Approximately 200-300 MB total for all files

**Update Frequency:** Regularly updated (often within days of tournament completion)

**Citation:** "Tennis databases, files, and algorithms by Jeff Sackmann / Tennis Abstract, licensed under CC BY-NC-SA 4.0."

---

## 2. Jeff Sackmann / Tennis Abstract -- WTA (PRIMARY)

**URL:** https://github.com/JeffSackmann/tennis_wta

**Access Method:** Git clone or direct CSV download

**Steps:**
1. Clone repository: `git clone https://github.com/JeffSackmann/tennis_wta.git`
2. Key files:
   - `wta_matches_YYYY.csv` (main draw)
   - `wta_matches_qual_itf_YYYY.csv` (qualifying + ITF)
   - `wta_rankings_XXs.csv` (rankings by decade)
   - `wta_players.csv` (player data)
3. Same CSV format as ATP repository

**License:** Same CC BY-NC-SA 4.0

**Timeline:** Immediate

**Note:** Column structure mirrors ATP repository. Refer to ATP `matches_data_dictionary.txt` for column definitions. WTA qualifying files mix ITF-level matches with tour-level qualifying; filter by `tourney_level` carefully.

---

## 3. Elo Rating Computation Code

**URL:** https://github.com/JeffSackmann/tennis_viz

**Access Method:** Git clone

**Steps:**
1. Clone repository: `git clone https://github.com/JeffSackmann/tennis_viz.git`
2. Reference the Elo computation code (primarily Python)
3. Algorithm description: https://www.tennisabstract.com/blog/2019/12/03/an-introduction-to-tennis-elo/
4. Adapt for your study: compute Elo from full match history, record snapshots at each qualifying event date

**License:** Check repository for terms (likely same CC BY-NC-SA 4.0 as other Sackmann repos)

**Timeline:** Immediate for code; 4-6 hours to implement and compute Elo for full sample

---

## 4. Maity et al. (2025) Replication Data

**URL:** https://osf.io/pe7cg/
**Alternative:** Check https://www.dashunwang.com/academic-articles/early-career-setback-and-future-achievement-in-professional-sports for links

**Access Method:** Direct download from Open Science Framework

**Steps:**
1. Navigate to https://osf.io/pe7cg/
2. Download available code and data files
3. Review their LL identification methodology
4. Use for validation/cross-checking against Sackmann data (2007--2019 overlap)

**License:** Check OSF repository for specific terms (academic replication data is typically freely available)

**Timeline:** Immediate

**Note:** This is supplementary/validation data. Their dataset covers 2007--2019 and was sourced from atptour.com. Use to verify your LL identification pipeline produces consistent counts.

---

## 5. Tennis-Data.co.uk (Betting Odds)

**URL:** http://www.tennis-data.co.uk/alldata.php

**Access Method:** Direct download of Excel/CSV files

**Steps:**
1. Navigate to http://www.tennis-data.co.uk/alldata.php
2. Download yearly files for ATP and WTA
3. Review field descriptions at http://www.tennis-data.co.uk/notes.txt
4. Merge with Sackmann data using player names + tournament + date

**License:** Free for personal/research use (check site terms)

**Timeline:** Immediate

**Known Limitation:** Main draw matches only (no qualifying). Must merge with Sackmann data to identify LL players.

---

## 6. OnCourt Database

**URL:** https://www.oncourt.info/

**Access Method:** Software purchase + installation

**Steps:**
1. Download free 30-day trial from oncourt.info
2. Explore data using built-in filters (lucky loser filter available)
3. Evaluate whether raw data can be exported to CSV/Excel
4. If needed for validation, purchase license (48.95 EUR/year or 88.95 EUR lifetime)

**Timeline:** 30-day trial immediate; purchase decision after evaluation

**Recommendation:** Try the free trial to validate LL identification and assess data quality against Sackmann, but do not plan to use as primary data source due to export limitations.

---

## 7. ATP Official Rulebook

**URL:** https://www.atptour.com/-/media/files/rulebook/2025/

**Access Method:** Direct PDF download

**Key Documents:**
- Chapter 7 (Competition): Lucky Loser selection rules, withdrawal procedures, alternate list ordering
  - 2025 version: `2025-rulebook-chapter-7_the-competition_20may.pdf`
  - Also check August and December revisions for any mid-year changes
- Chapter 9 (Rankings): Points table by tournament level and round
  - `2025-rulebook-chapter-9_pif-atp-rankings_23dec.pdf`

**Grand Slam Rulebook:** https://www.itftennis.com/media/5986/grand-slam-rulebook-2025-f.pdf
- Contains the timing-dependent LL selection rule for Grand Slams (in effect since 2006)
- CRITICAL: The rule varies based on when the withdrawal occurs relative to qualifying completion

**Historical Rulebooks:** Search for archived versions at:
- https://www.itftennis.com/ (hosts ATP rulebooks for multiple years)
- https://www.atptour.com/-/media/files/rulebook/ (current year structure, try substituting year)

**Timeline:** Immediate (public PDFs)

**Note:** Essential for institutional background. Download and archive rulebooks for at least 2006, 2009, 2019, and 2024 to document rule changes during the sample period.

---

## 8. Prize Money Data

**Sources:**
- Perfect Tennis: https://www.perfect-tennis.com/prize-money/
- ATP Media Guides: https://www.atptour.com/en/media/rankings-and-stats

**Access Method:** Manual compilation from web pages and PDF media guides

**Steps:**
1. For current/recent years: use Perfect Tennis website (organized by tournament)
2. For historical years: download ATP Media Guides (annual PDFs with prize money tables)
3. Compile into a lookup table: tournament_level x year x round -> prize_money_USD
4. Merge with match data

**Timeline:** 2-4 hours of manual compilation for a usable lookup table

**Known Issue:** Currency conversion needed for non-USD tournaments in earlier years.

---

## 9. ATP Ranking Points Table

**Source:** Check ITF website for annual points tables:
- 2025: https://www.itftennis.com/ (search for ATP points table)
- Historical: Try substituting year in known URLs

**Steps:**
1. Download points tables for each year in your sample period
2. Document changes in points structure (major changes in 2009 and 2024)
3. Build a lookup table: tournament_level x year x round -> ranking_points
4. Use to calculate expected points gained from LL entry

**Timeline:** 1-2 hours

---

## Measurement Validation Protocol

### Step 1: Entry Field Completeness Audit (Priority: HIGH)

**Objective:** Quantify the reliability of the `entry` field in Sackmann main draw data.

**Method:**
1. Load all main draw files for 2000-2024
2. For each year, count:
   - Total matches
   - Matches where `winner_entry` is non-blank (any value)
   - Matches where `winner_entry = "LL"`
   - Matches where `loser_entry = "LL"`
   - Distinct player-tournament pairs with entry = "LL"
3. Report year-by-year LL counts
4. Flag any years with suspiciously low LL counts (< 30 for post-2007 years would be suspicious)

**Expected output:** Year-by-year LL count table

**Timeline:** 1-2 hours of R/Python scripting

### Step 2: Cross-Validation with Maity et al. (Priority: HIGH)

**Objective:** Compare LL identification between Sackmann data and the independently-collected Maity et al. dataset.

**Method:**
1. Download Maity et al. data from OSF
2. Extract their LL identification for 2007-2019
3. Merge on player name + tournament + year
4. Compare year-by-year LL counts
5. Identify any player-tournament observations that are LL in one source but not the other

**Expected output:** Concordance rate and list of discrepancies

**Timeline:** 2-3 hours

### Step 3: Qualifying Round Completeness Audit (Priority: HIGH)

**Objective:** Verify that final-round qualifying losers can be identified for the sample period.

**Method:**
1. Load qualifying files for 2000-2024
2. For each year and tournament level (G, M, A), count:
   - Number of tournaments with qualifying data
   - Number of final-round matches per tournament
   - Number of distinct final-round losers per tournament
3. For Grand Slams specifically, verify 16 Q3 losers per tournament (expected)
4. Flag tournaments with missing or incomplete qualifying data

**Expected output:** Qualifying data coverage table by year and tournament level

**Timeline:** 2-3 hours

### Step 4: Running Variable Verification (Priority: MEDIUM)

**Objective:** Verify that ranking among final-round losers correctly predicts LL assignment at ATP (non-Grand Slam) events.

**Method:**
1. For each ATP event (non-Grand Slam) with at least one LL entry:
   - Rank the final-round qualifying losers by ATP ranking
   - Check whether the LL(s) are the highest-ranked losers
2. Compute the concordance rate
3. Investigate discrepancies

**Expected output:** Concordance rate and investigation of discrepancies

**Timeline:** 3-4 hours

### Step 5: Grand Slam Lottery Pool Reconstruction (Priority: MEDIUM)

**Objective:** Reconstruct the eligible pool for Grand Slams from 2006 onward and classify lottery vs. ranking-based entries.

**Method:**
1. For each Grand Slam from 2006-2024:
   - Identify all Q3 losers (expected: 16 per Grand Slam)
   - Rank by ATP ranking to identify the top-4 pool
   - Determine how many became LLs
2. Research withdrawal timing for each GS LL entry:
   - Search news reports, tournament draw release dates, official announcements
   - Classify each LL entry as: lottery (pre-completion withdrawal), ranking-based (post-completion), or unverified
3. For verified lottery entries, test randomness: among eligible pool members, is LL probability independent of rank position?

**Expected output:** Pool reconstruction with lottery/ranking classification for all Grand Slams 2006-2024

**Timeline:** 4-6 hours (pool reconstruction) + 4-8 hours (withdrawal timing research)

### Step 6: Formal Power Calculations (Priority: MEDIUM)

**Objective:** Compute minimum detectable effects for both the RDD and lottery designs.

**Method:**
1. After constructing the sample, use the `rdpower` package in R for the RDD design
2. For the Grand Slam lottery subsample, use standard two-sample power calculations
3. Report minimum detectable effect sizes at 80% power, alpha = 0.05

**Expected output:** Power tables for both designs

**Timeline:** 2-3 hours (after sample construction)

---

## Recommended Download and Validation Sequence

| Order | Source | Purpose | Time Needed |
|-------|--------|---------|-------------|
| 1 | Sackmann ATP (git clone) | Primary match + ranking data | 10 minutes |
| 2 | Sackmann WTA (git clone) | Secondary match + ranking data | 10 minutes |
| 3 | Sackmann tennis_viz (git clone) | Elo computation reference code | 5 minutes |
| 4 | Entry field audit (Step 1) | Validate LL identification | 1-2 hours |
| 5 | Maity et al. OSF data | Validation dataset | 15 minutes |
| 6 | Cross-validation (Step 2) | Compare LL counts | 2-3 hours |
| 7 | Qualifying completeness (Step 3) | Verify comparison group | 2-3 hours |
| 8 | ATP/GS Rulebook PDFs | Institutional rules | 15 minutes |
| 9 | Running variable check (Step 4) | Verify ranking-based assignment | 3-4 hours |
| 10 | GS pool reconstruction (Step 5a) | Reconstruct eligible pools | 4-6 hours |
| 11 | GS withdrawal timing (Step 5b) | Classify lottery vs. ranking | 4-8 hours |
| 12 | Elo computation | Competitiveness index | 4-6 hours |
| 13 | Power calculations (Step 6) | Assess statistical feasibility | 2-3 hours |
| 14 | Tennis-Data.co.uk | Betting odds (supplementary) | 20 minutes |
| 15 | Points tables (PDFs) | Ranking points lookup | 30 minutes |
| 16 | Prize money compilation | Earnings outcome | 2-4 hours |
| 17 | OnCourt trial (optional) | Validation / cross-check | 1 hour |

**Total estimated time:** 3-5 days for full data assembly and validation.

**Critical path:** Steps 1-7 must be completed before any analysis can begin. Steps 1-4 are the minimum viable dataset. Steps 5-6 are required for the identification strategy to be credible. Step 12 (Elo) is required for the primary outcome variable.

---

## Data Storage Recommendations

```
Data/
  raw/
    sackmann_atp/          # git clone of tennis_atp
    sackmann_wta/          # git clone of tennis_wta
    sackmann_viz/          # git clone of tennis_viz (Elo code)
    maity_replication/     # OSF download
    tennis_data_uk/        # betting odds CSVs
    atp_rulebooks/         # PDF documentation (multiple years)
    gs_rulebooks/          # Grand Slam rulebooks (multiple years)
    points_tables/         # PDF points tables (multiple years)
  cleaned/
    atp_main_draw.csv      # filtered main draw matches
    atp_qualifying.csv     # filtered qualifying matches (not challengers)
    atp_rankings_panel.csv # combined rankings across decades
    atp_elo_panel.csv      # computed Elo ratings at player-week level
    atp_ll_events.csv      # all LL entries with constructed variables
    atp_final_qual_losers.csv  # all final-round qualifying losers
    atp_gs_lottery_pool.csv    # Grand Slam pools with lottery/ranking classification
    wta_main_draw.csv
    wta_qualifying.csv
    wta_rankings_panel.csv
    wta_elo_panel.csv
    wta_ll_events.csv
    wta_final_qual_losers.csv
    points_lookup.csv      # tournament_level x year x round -> points
    prize_money_lookup.csv # tournament x year x round -> prize_money
  validation/
    ll_count_comparison.csv    # Sackmann vs Maity LL counts by year
    entry_field_audit.csv      # Year-by-year entry field completeness
    ranking_concordance.csv    # Running variable verification results
    gs_lottery_classification.csv  # GS LL entries classified as lottery/ranking/unverified
    gs_lottery_randomness.csv  # Grand Slam lottery pool analysis
    power_calculations.md      # Power analysis results
  documentation/
    data_construction_log.md
    variable_definitions.md
    sample_restrictions.md
    rule_changes_timeline.md   # Timeline of LL rule changes
    gs_withdrawal_timing.md    # Evidence for withdrawal timing at each GS
```

**Note:** Raw Sackmann repositories should be gitignored (large, frequently updated). Store only the processed/cleaned datasets in the project repository, or use git submodules.
