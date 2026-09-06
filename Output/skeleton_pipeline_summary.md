# Skeleton Pipeline Summary
Generated: 2026-03-25 08:42:28.185969

## Step 1: Sample Construction
- ATP GS (2006-2024): N = 248 (LL: 132, Control: 116)
- WTA GS (2006-2024): N = 132 (LL: 54, Control: 78)
- ATP non-GS: N = 2666 (LL: 860)
- WTA non-GS: N = 2545 (LL: 755)
- Unique ATP GS events: 62
- Unique WTA GS events: 33
- Unique ATP GS players: 174
- Unique WTA GS players: 112

## Step 3: Censoring
- Observations censored at 52w horizon: 3103 / 6658

## Final GS Estimation Sample
- ATP GS: N = 248 (LL: 132, Control: 116)
- WTA GS: N = 132 (LL: 54, Control: 78)
- Unique ATP events: 62
- Unique WTA events: 33
- Unique ATP players: 174
- Unique WTA players: 112

## Step 6: Immediate Effects
- ATP md_any_win: FE coef = 0.32, p = 0.000, N = 248
- ATP md_matches_played: FE coef = 1.48, p = 0.000, N = 248
- ATP md_points: FE coef = 29.66, p = 0.000, N = 248
- WTA md_any_win: FE coef = 0.38, p = 0.000, N = 132
- WTA md_matches_played: FE coef = 1.41, p = 0.000, N = 132
- WTA md_points: FE coef = 24.86, p = 0.000, N = 132

## Step 7: Post-Episode Dynamics (GS)
- ATP n_main_draws_4w: FE coef = 0.04, p = 0.717
- ATP n_matches_250plus_4w: FE coef = 0.08, p = 0.683
- ATP points_change_4w: FE coef = 25.79, p = 0.001
- ATP elo_change_4w: FE coef = -1.04, p = 0.784
- ATP n_main_draws_8w: FE coef = 0.29, p = 0.072
- ATP n_matches_250plus_8w: FE coef = 0.38, p = 0.228
- ATP points_change_8w: FE coef = 27.99, p = 0.029
- ATP elo_change_8w: FE coef = -3.14, p = 0.618
- ATP n_main_draws_12w: FE coef = 0.41, p = 0.035
- ATP n_matches_250plus_12w: FE coef = 0.52, p = 0.157
- ATP points_change_12w: FE coef = 33.98, p = 0.023
- ATP elo_change_12w: FE coef = -7.20, p = 0.344
- ATP n_main_draws_26w: FE coef = 0.93, p = 0.024
- ATP n_matches_250plus_26w: FE coef = 1.45, p = 0.068
- ATP points_change_26w: FE coef = 48.50, p = 0.093
- ATP elo_change_26w: FE coef = 2.40, p = 0.832
- ATP n_main_draws_52w: FE coef = 1.19, p = 0.190
- ATP n_matches_250plus_52w: FE coef = 1.83, p = 0.281
- ATP points_change_52w: FE coef = 6.36, p = 0.888
- ATP elo_change_52w: FE coef = -16.51, p = 0.247

## Step 8: Heterogeneity
- ATP subgroups estimated: 6
- WTA subgroups estimated: 6

## Step 9: First-LL-Only
- ATP first-LL: N = 174 (LL: 91, Control: 83)
- WTA first-LL: N = 112 (LL: 48, Control: 64)

## Step 10: Verified Lottery
- Verified events: 34
- ATP verified: N = 100 (LL: 38)
- WTA verified: N = 44 (LL: 14)

## Step 11: Balance
- ATP joint F-test: F = 1.47, p = 0.191
- WTA joint F-test: F = 1.86, p = 0.093

## Step 12: Non-GS IV
- ATP First stage coef: 0.8137
- ATP First stage F: 2079.2
- ATP N: 2672

- IV ATP points_change_26w: coef = 26.19, SE = 10.25, p = 0.011, N = 2545
- IV ATP n_main_draws_26w: coef = 0.29, SE = 0.18, p = 0.105, N = 2672
- IV ATP n_matches_250plus_26w: coef = 0.70, SE = 0.34, p = 0.041, N = 2672
- IV ATP elo_change_26w: coef = 6.71, SE = 4.23, p = 0.113, N = 2175
- IV WTA points_change_26w: coef = 33.27, SE = 14.81, p = 0.025, N = 2376
- IV WTA n_main_draws_26w: coef = 0.14, SE = 0.19, p = 0.450, N = 2545
- IV WTA n_matches_250plus_26w: coef = 0.29, SE = 0.39, p = 0.466, N = 2545
- IV WTA elo_change_26w: coef = -4.50, SE = 4.89, p = 0.357, N = 2039
## Step 14b: Non-GS Mirror Tables
- ATP non-GS immediate: 3 outcomes
- WTA non-GS immediate: 3 outcomes
- ATP non-GS dynamic: 20 outcomes
- WTA non-GS dynamic: 20 outcomes
- ATP non-GS heterogeneity: 24 subgroup-outcomes
- WTA non-GS heterogeneity: 24 subgroup-outcomes
- ATP first-LL non-GS: N = 861
- WTA first-LL non-GS: N = 752

## Step 16: Consistency Checks
- GS dist ATP = est N: TRUE
- GS dist WTA = est N: TRUE
- Sumstats ATP = est N: TRUE (248 vs 248)
- Sumstats WTA = est N: TRUE (132 vs 132)

