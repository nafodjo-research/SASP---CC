# Data Dictionary: Primary Datasets for Tennis Lucky Loser Study

**Date:** 2026-03-20 (Revised: Round 3)
**Primary Source:** Jeff Sackmann / Tennis Abstract (tennis_atp + tennis_wta GitHub repositories)

---

## 1. Match-Level Data (Main Draw)

**File:** `atp_matches_YYYY.csv` (1968--present)

| Variable | Type | Description | Role in Study |
|----------|------|-------------|---------------|
| `tourney_id` | String | Unique tournament ID (YYYY + code) | Merge key; fixed effects |
| `tourney_name` | String | Tournament name | Descriptive |
| `tourney_date` | Integer | Tournament start date (YYYYMMDD) | Time variable |
| `surface` | String | Hard, Clay, Grass, Carpet | Control / heterogeneity |
| `draw_size` | Integer | Number of players in draw | Tournament characteristic |
| `tourney_level` | String | G/M/A/C/S/F/D (see below) | Tournament tier; heterogeneity |
| `match_num` | Integer | Match ID within tournament | Unique match identifier |
| `winner_id` | Integer | Winner player ID | Player identifier |
| `winner_name` | String | Winner name | Descriptive |
| `winner_hand` | String | R/L/U | Control |
| `winner_ht` | Integer | Height in cm | Control |
| `winner_ioc` | String | 3-char country code | Control / fixed effect |
| `winner_age` | Float | Age at tournament date | Control |
| **`winner_entry`** | **String** | **Entry type: LL, Q, WC, PR, SE, ITF** | **TREATMENT VARIABLE** |
| `winner_seed` | Integer | Tournament seed number | Control |
| `winner_rank` | Integer | ATP ranking at tournament date | Running variable / control |
| `winner_rank_points` | Integer | Ranking points at tournament date | Control / outcome |
| `loser_id` | Integer | Loser player ID | Player identifier |
| `loser_name` | String | Loser name | Descriptive |
| `loser_hand` | String | R/L/U | Control |
| `loser_ht` | Integer | Height in cm | Control |
| `loser_ioc` | String | 3-char country code | Control / fixed effect |
| `loser_age` | Float | Age at tournament date | Control |
| **`loser_entry`** | **String** | **Entry type: LL, Q, WC, PR, SE, ITF** | **TREATMENT VARIABLE** |
| `loser_seed` | Integer | Tournament seed number | Control |
| `loser_rank` | Integer | ATP ranking at tournament date | Running variable / control |
| `loser_rank_points` | Integer | Ranking points at tournament date | Control / outcome |
| `round` | String | Round (R128, R64, R32, R16, QF, SF, F, RR) | Outcome (how far LL advanced) |
| `best_of` | Integer | 3 or 5 sets | Match format control |
| `minutes` | Integer | Match duration | Descriptive |
| `score` | String | Match score | Outcome detail |
| `w_ace` | Integer | Winner aces | Match statistic |
| `w_df` | Integer | Winner double faults | Match statistic |
| `w_svpt` | Integer | Winner serve points | Match statistic |
| `w_1stIn` | Integer | Winner 1st serves in | Match statistic |
| `w_1stWon` | Integer | Winner 1st serve points won | Match statistic |
| `w_2ndWon` | Integer | Winner 2nd serve points won | Match statistic |
| `w_SvGms` | Integer | Winner serve games | Match statistic |
| `w_bpSaved` | Integer | Winner break points saved | Match statistic |
| `w_bpFaced` | Integer | Winner break points faced | Match statistic |
| `l_ace` through `l_bpFaced` | Integer | Same stats for loser | Match statistics |

### Tournament Level Codes (Men's ATP)

| Code | Level | Points (Winner) | Qualifying Draw | Qualifying Rounds | Final-Round Losers |
|------|-------|-----------------|-----------------|-------------------|-------------------|
| G | Grand Slam | 2000 | 128 players | 3 (Q1, Q2, Q3) | 16 |
| M | Masters 1000 (96-draw) | 1000 | 48 players | 3 (Q1, Q2, Q3) | ~12 |
| M | Masters 1000 (56/48-draw) | 1000 | 24-28 players | 2-3 rounds | ~4-8 |
| A | ATP 500 | 500 | Varies | 1-2 rounds | ~4 |
| A | ATP 250 | 250 | 16 players | 2 (Q1, Q2) | 4 |
| C | Challenger | Varies (50-175) | Sometimes | Varies | Varies |
| S | Satellites/ITFs | Varies | Yes | Varies | Varies |
| F | Tour Finals | 1500 | No qualifying | N/A | N/A |
| D | Davis Cup | N/A | N/A | N/A | N/A |

### Entry Type Codes

| Code | Meaning | Relevance |
|------|---------|-----------|
| **LL** | **Lucky Loser** | **Treatment group** |
| Q | Qualifier | Comparison group (won qualifying) |
| WC | Wild Card | Alternative entry mechanism |
| PR | Protected Ranking | Injured player returning |
| SE | Special Exempt | Special entry |
| ITF | ITF entry | Lower-level entry |
| Alt | Alternate | Sometimes used instead of LL in early data |
| (blank) | Direct acceptance | Ranked high enough for direct entry |

---

## 2. Qualifying/Challenger Match Data

**File:** `atp_matches_qual_chall_YYYY.csv` (1978--present)

Same column structure as main draw files. Key differences:
- Contains qualifying round matches: Q1 (first round), Q2 (second round), Q3 (third round, Grand Slams only)
- Also contains Challenger-level tournament matches (must filter by `tourney_level`)
- Match statistics available from 2011 onward for tour-level qualifying
- **Critical for constructing the comparison group:** identify all players who lost in the final qualifying round

### Filtering Logic for Qualifying Matches

To extract tour-level qualifying matches:
- Filter where `tourney_level` in {G, M, A} (exclude C for Challengers)
- The final qualifying round varies by tournament level:
  - Grand Slams (`tourney_level = "G"`): final round is Q3
  - Masters 1000 with 48-player qualifying: final round is Q3
  - ATP 250 with 16-player qualifying: final round is Q2
  - Other tournaments: must be determined from the draw size or by identifying the maximum Q-round value per tournament

### Identifying the Final Qualifying Round Programmatically

Because the number of qualifying rounds varies by tournament, the safest approach is:

```
For each tournament in qualifying files where tourney_level in {G, M, A}:
  max_round = max(round) among {Q1, Q2, Q3}
  final_round_losers = all losers in matches where round = max_round
```

This handles tournaments with 2 qualifying rounds (max = Q2) and 3 qualifying rounds (max = Q3) automatically.

---

## 3. Rankings Data

**Files:** `atp_rankings_XXs.csv` (by decade) + `atp_rankings_current.csv`

| Variable | Type | Description | Role in Study |
|----------|------|-------------|---------------|
| `ranking_date` | Integer | Date of ranking (YYYYMMDD) | Time variable |
| `rank` | Integer | ATP ranking position | **PRIMARY OUTCOME** |
| `player` | Integer | Player ID (matches `winner_id`/`loser_id`) | Merge key |
| `points` | Integer | Ranking points (where available) | **PRIMARY OUTCOME** |

**Frequency:** Weekly (updated every Monday)

**Usage:**
- Merge with match data on `player` = `winner_id` or `loser_id`
- Construct pre/post LL entry ranking trajectories
- Measure ranking change at t+4, t+8, t+12, t+26, t+52 weeks
- Compute ranking points gained from the specific tournament

---

## 4. Player Biographical Data

**File:** `atp_players.csv`

| Variable | Type | Description | Role in Study |
|----------|------|-------------|---------------|
| `player_id` | Integer | Unique player ID | Merge key |
| `name_first` | String | First name | Descriptive |
| `name_last` | String | Last name | Descriptive |
| `hand` | String | R/L/U | Control |
| `dob` | Integer | Date of birth (YYYYMMDD) | Age calculation |
| `ioc` | String | Country code | Control / fixed effect |
| `height` | Integer | Height in cm | Control (only ~7% populated for historical players) |

---

## 5. Constructed Variables (To Be Built)

### Running Variable Construction (Issue 2 Resolution)

The running variable for the RDD is the player's position in the ranking-ordered list of final-round qualifying losers at a given tournament. Construction proceeds as follows:

#### Step 1: Identify final-round qualifying losers

For each tournament with qualifying (filter: `tourney_level` in {G, M, A} in the qualifying files):

```
final_qual_round = max(round value in {Q1, Q2, Q3}) for that tournament
final_round_losers = players who appear as losers in matches where round = final_qual_round
```

#### Step 2: Obtain each loser's ranking

**Option A (preferred):** Use the `loser_rank` field from the qualifying match record.

**Option B (fallback):** If `loser_rank` is missing, merge with the weekly rankings file. The relevant ranking is the one published on the Monday before or of the week containing the tournament's qualifying start date:

```
ranking_week = most recent ranking_date <= (tourney_date - 7)
```

**Complication:** The ranking used for LL ordering is the "live" ranking at the time of the withdrawal, which may differ from the published weekly ranking. In practice, most LL decisions happen between the end of qualifying and the start of the main draw, so the most recent published ranking is a close approximation.

#### Step 3: Rank the losers within each tournament

Sort final-round qualifying losers by their ATP ranking (ascending -- rank 1 is best). Assign a position variable:

```
rank_among_losers = 1 (highest-ranked loser), 2, 3, ..., N (lowest-ranked)
```

#### Step 4: Determine how many LL slots opened

```
ll_slots_at_tournament = count of distinct players with entry = "LL" in main draw matches for that tournament
```

#### Step 5: Define treatment status

**For ATP events (non-Grand Slam):**
- Players with `rank_among_losers <= ll_slots_at_tournament` should have received LL entry
- This is a **sharp RDD** -- ranking perfectly determines treatment

**For Grand Slams (since 2006) -- TIMING-DEPENDENT:**
- If withdrawal was after qualifying completion: highest-ranked loser gets the spot (ranking-based, same as ATP)
- If withdrawal was before qualifying completion: top-2 to top-4 ranked losers enter random draw (lottery)
- The timing of withdrawal is NOT recorded in the Sackmann data and requires supplementary research
- Conservative approach: classify all GS LL entries as potentially ranking-based unless timing is verified

#### Addressing the withdrawal count problem

The number of LL slots is endogenous to the withdrawal process, but withdrawals are driven by injuries/illness of MAIN DRAW players, plausibly exogenous to qualifying loser characteristics.

#### Addressing varying qualifying draw sizes

**Approach A (preferred):** Normalize: `rank_among_losers / total_losers_at_tournament`, creating a [0,1] variable.

**Approach B:** Estimate separately by tournament level.

### Full list of constructed variables

| Variable | Construction Method | Role |
|----------|-------------------|------|
| `is_ll` | = 1 if player appears in main draw with `entry = "LL"` | Treatment indicator |
| `is_final_qual_loser` | = 1 if player lost in the final qualifying round (max Q-round for that tournament) | Sample restriction |
| `rank_among_losers` | Position when sorting final-round losers by ATP ranking (ascending, 1 = best) | **Running variable for RDD** |
| `total_losers_at_tournament` | Count of final-round qualifying losers at that tournament | Normalizing denominator |
| `normalized_rank` | `rank_among_losers / total_losers_at_tournament` | Normalized running variable |
| `ll_slots_at_tournament` | Count of distinct LL entries in main draw for that tournament | Cutoff determinant |
| `ll_eligible_gs` | = 1 if `is_final_qual_loser` AND `rank_among_losers <= 4` AND `tourney_level = "G"` AND year >= 2006 | Grand Slam eligible pool indicator |
| `gs_lottery_verified` | = 1 if withdrawal timing is verified as pre-completion (lottery); 0 if post-completion (ranking-based); NA if unverified | Grand Slam lottery subsample flag |
| `withdrawal_count` | = `ll_slots_at_tournament` | Contextual variable |
| `ranking_at_event` | Player's ATP ranking at the time of the qualifying event | Running variable input; control |
| `ranking_points_at_event` | Player's ranking points at event | Control |
| `elo_at_event` | Player's Elo rating computed from match history up to the qualifying event date | **PRIMARY OUTCOME / Control** |
| `surface_elo_at_event` | Surface-specific Elo rating (hard, clay, grass) at event date | Heterogeneity / control |
| `elo_change_Xw` | Elo rating at t+X weeks minus Elo at tournament date | **PRIMARY OUTCOME (competitiveness index)** |
| `ranking_change_Xw` | Ranking at t+X weeks minus ranking at tournament date | **Primary outcome** |
| `points_change_Xw` | Ranking points at t+X weeks minus points at tournament date | **Primary outcome** |
| `main_draw_entries_next_Y` | Count of main draw entries (non-qualifying) in next Y months | **Secondary outcome** |
| `matches_won_next_Y` | Count of main draw match wins in next Y months | **Secondary outcome** |
| `rounds_advanced` | Maximum round reached in the tournament (coded numerically: R128=1, R64=2, ..., F=7) | **Immediate outcome** |
| `prize_money_earned` | Imputed from tournament level + round reached + year | **Secondary outcome** |
| `closeness_of_loss` | Games or sets margin in the final qualifying round loss | Alternative running variable / control |
| `surface` | Surface of the tournament (Hard, Clay, Grass) | Heterogeneity |
| `age_at_event` | Player age at tournament date | Control |
| `career_stage` | Career year = (tournament_year - year of first tour-level match) | Control / heterogeneity |

---

## 6. Elo Rating Construction

**Algorithm:** Standard Elo with parameters calibrated for tennis. Reference: Sackmann's "An Introduction to Tennis Elo" (https://www.tennisabstract.com/blog/2019/12/03/an-introduction-to-tennis-elo/).

**Update formula:**
```
Elo_new = Elo_old + K * (actual_result - expected_result)
expected_result = 1 / (1 + 10^((opponent_elo - player_elo) / 400))
```

**Key parameters:**
- K-factor: Typically ~32 for new players, ~16 for established players (Sackmann uses match-count-dependent K)
- Starting Elo: ~1500 (standard)
- Surface-specific Elo: Separate ratings for hard, clay, grass computed using only matches on that surface

**Why Elo is the right competitiveness index:**
- E_j[P_t(i->j|X)] = expected win probability against representative opponents = exactly what Elo measures
- Elo accounts for opponent quality (unlike win/loss record or ranking)
- Surface-specific Elo captures context-dependent ability
- Change in Elo after LL entry directly measures change in competitive potential

**Construction steps:**
1. Load ALL match data (main draw + qualifying + challenger) chronologically
2. Initialize all players at starting Elo
3. Process matches in date order, updating both players' Elo after each match
4. Record Elo snapshot at each player-tournament date
5. Compute Elo change at +4/+8/+12/+26/+52 weeks

---

## 7. ATP Ranking Points Table (2025, for reference)

| Tournament Level | W | F | SF | QF | R16 | R32 | R64 | R128 | Q |
|-----------------|-----|------|-----|-----|------|------|------|------|-----|
| Grand Slam | 2000 | 1200 | 720 | 360 | 180 | 90 | 45 | 10 | 25 |
| Masters 1000 (96) | 1000 | 600 | 360 | 180 | 90 | 45 | 10 | -- | 25 |
| Masters 1000 (56/48) | 1000 | 600 | 360 | 180 | 90 | 45 | -- | -- | 16 |
| ATP 500 | 500 | 300 | 180 | 90 | -- | -- | -- | -- | -- |
| ATP 250 | 250 | 150 | 90 | 45 | 20 | -- | -- | -- | -- |

**Note on LL points:** Lucky losers receive main draw points based on the round they reach. They do NOT receive qualifying bonus points (Q column). This is economically relevant: an LL who loses in R1 at a Grand Slam gets 10 points, whereas a qualifier who loses in R1 gets 10 + 25 = 35 points. The "treatment dose" of LL entry (in ranking points) is the main draw round points only.

**Points restructuring timeline:**
- Pre-2009: Different points scale (conversion factors needed)
- 2009-2023: Standard scale shown above (with minor annual adjustments)
- 2024: Restructured points table; verify current values from ATP rulebook

---

## 8. WTA Equivalent Variables

The WTA repository (`tennis_wta`) uses an identical column structure. Key differences:

| ATP Variable | WTA Equivalent | Notes |
|-------------|----------------|-------|
| `tourney_level = G` | Same | Grand Slams are joint |
| `tourney_level = M` | `tourney_level = PM` | Premier Mandatory |
| `tourney_level = A` | `tourney_level = P/I` | Premier / International |
| Qualifying file | `wta_matches_qual_itf_YYYY.csv` | Includes ITF matches (filter needed) |
| Rankings file | `wta_rankings_XXs.csv` | Same structure |

---

## 9. Merge Strategy

```
Step 1: Load qualifying match files for each year
        -> Filter to tourney_level in {G, M, A}
        -> Identify the final qualifying round per tournament (max of Q1/Q2/Q3)
        -> Extract all final-round losers with their player_id, tourney_id, loser_rank

Step 2: Load main draw match files for each year
        -> Identify LL entries: all matches where winner_entry = "LL" or loser_entry = "LL"
        -> For each tournament, count distinct LL player_ids = ll_slots_at_tournament

Step 3: Link qualifying losers to main draw LL entries
        -> Match by player_id + tourney_id
        -> Players in Step 1 who appear as LL in Step 2: is_ll = 1
        -> Players in Step 1 who do NOT appear in main draw: is_ll = 0

Step 4: Construct the running variable
        -> For each tournament, rank the final-round losers by loser_rank (ascending)
        -> Assign rank_among_losers = 1 (best ranked), 2, ..., N

Step 5: Merge with rankings panel
        -> For each player-tournament, find the ranking_date closest to (and before) tourney_date
        -> Extract rank and points at event time
        -> Also extract rank and points at t+4, t+8, t+12, t+26, t+52 weeks

Step 6: Compute Elo ratings
        -> Process full match history chronologically
        -> Record Elo snapshot at each player-tournament date
        -> Compute surface-specific Elo as well
        -> Extract Elo at event time and at t+4/+8/+12/+26/+52 weeks

Step 7: Construct treatment/control groups and outcome variables
        -> For ATP RDD: treatment = is_ll, running variable = rank_among_losers
        -> For Grand Slam lottery: restrict to tourney_level = "G", year >= 2006,
           rank_among_losers <= 4; treatment = is_ll within this pool
           Flag gs_lottery_verified for entries where withdrawal timing is confirmed

Step 8: Merge supplementary data
        -> Prize money: by tournament + year + round
        -> Betting odds: by player + tournament + date (Tennis-Data.co.uk)

Step 9: Validation checks
        -> Compare LL counts with Maity et al. (2025) for 2007-2019
        -> Verify that rank_among_losers <= ll_slots correlates with is_ll (~perfect for ATP events)
        -> Flag tournaments where the ranking-ordering does not match LL assignment
        -> For Grand Slams, investigate withdrawal timing for lottery classification
```
