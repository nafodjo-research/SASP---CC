## Step 1: Data loaded
- Non-GS estimation sample: N = 5253
- ATP qualifying matches: 223097
- WTA qualifying matches: 593885

## Step 2: Final qualifying round matches identified
- Events with qualifying data: 945
- Final qualifying round matches: 5187
- Events without qualifying data: 0

## Step 3: Win probabilities computed
- Valid predictions: 5126 / 5187
- Mean P(winner wins): 0.521

## Step 4: P_i^{LL} computed
- Total observations: 5208
- Exact enumeration: 5069
- Monte Carlo: 115
- Not found in qualifying data: 24
- Trivial (single match): 0

## Step 6: Merged into estimation data
- Replaced: 5229 observations
- Kept old: 24 observations

## Step 7: Diagnostic comparison
- N compared: 5229
- Correlation: 0.776
- Mean absolute difference: 0.2342
- RMSD: 0.3368
- Max absolute difference: 1
- Direction: 306 higher, 4109 lower, 814 ~same

## Step 8: Generalized residuals recomputed
- v_hat range: [-3.958, 3.958]

## Step 10: Non-GS stacked dynamics with corrected CF
- ATP non-GS CF points_change @ 4w: coef = 14.54, SE = 6.65, p = 0.029
- ATP non-GS CF points_change @ 8w: coef = 14.42, SE = 7.02, p = 0.040
- ATP non-GS CF points_change @ 12w: coef = 7.76, SE = 7.93, p = 0.328
- ATP non-GS CF points_change @ 26w: coef = 24.20, SE = 10.69, p = 0.024
- ATP non-GS CF points_change @ 52w: coef = -6.74, SE = 19.67, p = 0.732
- ATP non-GS CF n_main_draws @ 4w: coef = -1.36, SE = 0.14, p = 0.000
- ATP non-GS CF n_main_draws @ 8w: coef = -0.87, SE = 0.13, p = 0.000
- ATP non-GS CF n_main_draws @ 12w: coef = -0.30, SE = 0.13, p = 0.019
- ATP non-GS CF n_main_draws @ 26w: coef = 1.62, SE = 0.19, p = 0.000
- ATP non-GS CF n_main_draws @ 52w: coef = 4.70, SE = 0.46, p = 0.000
- ATP non-GS CF n_matches_250plus @ 4w: coef = -2.57, SE = 0.27, p = 0.000
- ATP non-GS CF n_matches_250plus @ 8w: coef = -1.68, SE = 0.25, p = 0.000
- ATP non-GS CF n_matches_250plus @ 12w: coef = -0.72, SE = 0.25, p = 0.004
- ATP non-GS CF n_matches_250plus @ 26w: coef = 2.75, SE = 0.38, p = 0.000
- ATP non-GS CF n_matches_250plus @ 52w: coef = 8.07, SE = 0.87, p = 0.000
- ATP non-GS CF elo_change @ 4w: coef = 0.13, SE = 2.42, p = 0.959
- ATP non-GS CF elo_change @ 8w: coef = 0.69, SE = 2.71, p = 0.799
- ATP non-GS CF elo_change @ 12w: coef = -1.09, SE = 2.90, p = 0.706
- ATP non-GS CF elo_change @ 26w: coef = -0.04, SE = 3.88, p = 0.991
- ATP non-GS CF elo_change @ 52w: coef = -5.74, SE = 5.69, p = 0.313
- WTA non-GS CF points_change @ 4w: coef = 30.71, SE = 9.61, p = 0.001
- WTA non-GS CF points_change @ 8w: coef = 24.41, SE = 10.31, p = 0.018
- WTA non-GS CF points_change @ 12w: coef = 25.19, SE = 11.47, p = 0.028
- WTA non-GS CF points_change @ 26w: coef = 6.42, SE = 15.92, p = 0.687
- WTA non-GS CF points_change @ 52w: coef = 13.29, SE = 32.61, p = 0.684
- WTA non-GS CF n_main_draws @ 4w: coef = -0.98, SE = 0.13, p = 0.000
- WTA non-GS CF n_main_draws @ 8w: coef = -0.53, SE = 0.12, p = 0.000
- WTA non-GS CF n_main_draws @ 12w: coef = -0.12, SE = 0.12, p = 0.333
- WTA non-GS CF n_main_draws @ 26w: coef = 1.26, SE = 0.17, p = 0.000
- WTA non-GS CF n_main_draws @ 52w: coef = 3.35, SE = 0.43, p = 0.000
- WTA non-GS CF n_matches_250plus @ 4w: coef = -1.87, SE = 0.27, p = 0.000
- WTA non-GS CF n_matches_250plus @ 8w: coef = -1.13, SE = 0.25, p = 0.000
- WTA non-GS CF n_matches_250plus @ 12w: coef = -0.42, SE = 0.25, p = 0.096
- WTA non-GS CF n_matches_250plus @ 26w: coef = 1.97, SE = 0.37, p = 0.000
- WTA non-GS CF n_matches_250plus @ 52w: coef = 6.24, SE = 0.85, p = 0.000
- WTA non-GS CF elo_change @ 4w: coef = 6.66, SE = 2.60, p = 0.011
- WTA non-GS CF elo_change @ 8w: coef = 5.53, SE = 2.82, p = 0.050
- WTA non-GS CF elo_change @ 12w: coef = 9.06, SE = 3.07, p = 0.003
- WTA non-GS CF elo_change @ 26w: coef = 3.16, SE = 4.50, p = 0.482
- WTA non-GS CF elo_change @ 52w: coef = 3.13, SE = 6.11, p = 0.609

## Endogeneity test (rho on v_hat) -- CORRECTED P_i^{LL}:
- ATP rho points_change @ 4w: rho = 4.332, p = 0.131 -- not significant
- ATP rho points_change @ 8w: rho = 2.692, p = 0.378 -- not significant
- ATP rho points_change @ 12w: rho = 2.689, p = 0.347 -- not significant
- ATP rho points_change @ 26w: rho = 0.096, p = 0.983 -- not significant
- ATP rho points_change @ 52w: rho = 12.540, p = 0.186 -- not significant
- ATP rho n_main_draws @ 4w: rho = 0.482, p = 0.000 -- SIGNIFICANT
- ATP rho n_main_draws @ 8w: rho = 0.343, p = 0.000 -- SIGNIFICANT
- ATP rho n_main_draws @ 12w: rho = 0.179, p = 0.001 -- SIGNIFICANT
- ATP rho n_main_draws @ 26w: rho = -0.450, p = 0.000 -- SIGNIFICANT
- ATP rho n_main_draws @ 52w: rho = -1.309, p = 0.000 -- SIGNIFICANT
- ATP rho n_matches_250plus @ 4w: rho = 0.896, p = 0.000 -- SIGNIFICANT
- ATP rho n_matches_250plus @ 8w: rho = 0.642, p = 0.000 -- SIGNIFICANT
- ATP rho n_matches_250plus @ 12w: rho = 0.373, p = 0.000 -- SIGNIFICANT
- ATP rho n_matches_250plus @ 26w: rho = -0.691, p = 0.000 -- SIGNIFICANT
- ATP rho n_matches_250plus @ 52w: rho = -2.194, p = 0.000 -- SIGNIFICANT
- ATP rho elo_change @ 4w: rho = 0.668, p = 0.487 -- not significant
- ATP rho elo_change @ 8w: rho = -0.656, p = 0.576 -- not significant
- ATP rho elo_change @ 12w: rho = 0.365, p = 0.773 -- not significant
- ATP rho elo_change @ 26w: rho = -0.207, p = 0.915 -- not significant
- ATP rho elo_change @ 52w: rho = 0.881, p = 0.767 -- not significant
- WTA rho points_change @ 4w: rho = -3.830, p = 0.262 -- not significant
- WTA rho points_change @ 8w: rho = -0.710, p = 0.839 -- not significant
- WTA rho points_change @ 12w: rho = -2.130, p = 0.583 -- not significant
- WTA rho points_change @ 26w: rho = -7.041, p = 0.239 -- not significant
- WTA rho points_change @ 52w: rho = -12.633, p = 0.252 -- not significant
- WTA rho n_main_draws @ 4w: rho = 0.352, p = 0.000 -- SIGNIFICANT
- WTA rho n_main_draws @ 8w: rho = 0.227, p = 0.000 -- SIGNIFICANT
- WTA rho n_main_draws @ 12w: rho = 0.095, p = 0.036 -- SIGNIFICANT
- WTA rho n_main_draws @ 26w: rho = -0.445, p = 0.000 -- SIGNIFICANT
- WTA rho n_main_draws @ 52w: rho = -1.217, p = 0.000 -- SIGNIFICANT
- WTA rho n_matches_250plus @ 4w: rho = 0.706, p = 0.000 -- SIGNIFICANT
- WTA rho n_matches_250plus @ 8w: rho = 0.477, p = 0.000 -- SIGNIFICANT
- WTA rho n_matches_250plus @ 12w: rho = 0.238, p = 0.010 -- SIGNIFICANT
- WTA rho n_matches_250plus @ 26w: rho = -0.725, p = 0.000 -- SIGNIFICANT
- WTA rho n_matches_250plus @ 52w: rho = -2.319, p = 0.000 -- SIGNIFICANT
- WTA rho elo_change @ 4w: rho = -1.686, p = 0.060 -- not significant
- WTA rho elo_change @ 8w: rho = -0.489, p = 0.623 -- not significant
- WTA rho elo_change @ 12w: rho = -1.388, p = 0.232 -- not significant
- WTA rho elo_change @ 26w: rho = -1.844, p = 0.232 -- not significant
- WTA rho elo_change @ 52w: rho = -1.575, p = 0.563 -- not significant

