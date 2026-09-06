# Equation Fixes Summary (19_equation_fixes.R)
Generated: 2026-03-25 11:17:09.733721

## FIX 1: Horizon Fixed Effects Test

- Mean absolute % change in coefficients: 77.8%
- Max absolute % change: 429.1%
- Horizon FE adoption decision: YES -- adopt horizon FE
- ATP points_change: R2 without HFE = 0.2030, R2 with HFE = 0.2034, improvement = 0.0004
- ATP n_main_draws: R2 without HFE = 0.3506, R2 with HFE = 0.5257, improvement = 0.1752
- ATP n_matches_250plus: R2 without HFE = 0.3248, R2 with HFE = 0.4570, improvement = 0.1322
- ATP elo_change: R2 without HFE = 0.1668, R2 with HFE = 0.1672, improvement = 0.0004
- WTA points_change: R2 without HFE = 0.1975, R2 with HFE = 0.2132, improvement = 0.0157
- WTA n_main_draws: R2 without HFE = 0.3473, R2 with HFE = 0.6059, improvement = 0.2586
- WTA n_matches_250plus: R2 without HFE = 0.3671, R2 with HFE = 0.5406, improvement = 0.1736
- WTA elo_change: R2 without HFE = 0.1695, R2 with HFE = 0.1710, improvement = 0.0015

>>> Using event_id + horizon FE for all subsequent specs

## FIX 2: Heterogeneity with D + D x category dummies

- ATP points_change | Ranking | D (Q1 base): coef = 29.36 (25.66), p = 0.253
- ATP points_change | Ranking | D x rank_q2: coef = -8.40 (36.49), p = 0.818
- ATP points_change | Ranking | D x rank_q3: coef = -6.73 (38.70), p = 0.862
- ATP points_change | Ranking | D x rank_q4: coef = -16.17 (45.56), p = 0.723
- ATP points_change | Age | D (young base): coef = 36.41 (24.47), p = 0.137
- ATP points_change | Age | D x Above-median age: coef = -30.15 (30.74), p = 0.327
- ATP points_change | Prior LL | D (no prior base): coef = 42.96 (21.83), p = 0.049
- ATP points_change | Prior LL | D x Had prior LL: coef = -44.34 (24.73), p = 0.073
- ATP points_change | Dose | D (0 wins base): coef = 0.77 (17.07), p = 0.964
- ATP points_change | Dose | D x 1 win: coef = 49.33 (27.45), p = 0.072
- ATP points_change | Dose | D x 2+ wins: coef = 91.20 (34.38), p = 0.008
- WTA points_change | Ranking | D (Q1 base): coef = 72.10 (48.10), p = 0.134
- WTA points_change | Ranking | D x rank_q2: coef = 42.91 (63.13), p = 0.497
- WTA points_change | Ranking | D x rank_q3: coef = -29.67 (60.70), p = 0.625
- WTA points_change | Ranking | D x rank_q4: coef = -197.36 (77.52), p = 0.011
- WTA points_change | Age | D (young base): coef = 1.27 (33.00), p = 0.969
- WTA points_change | Age | D x Above-median age: coef = 41.00 (44.01), p = 0.351
- WTA points_change | Prior LL | D (no prior base): coef = 47.08 (29.57), p = 0.111
- WTA points_change | Prior LL | D x Had prior LL: coef = -63.84 (49.52), p = 0.197
- WTA points_change | Dose | D (0 wins base): coef = 1.70 (30.50), p = 0.956
- WTA points_change | Dose | D x 1 win: coef = 35.73 (36.57), p = 0.329
- WTA points_change | Dose | D x 2+ wins: coef = 219.42 (134.23), p = 0.102

## FIX 3: Verified Lottery Subsample (Stacked)

- ATP verified: N_stacked = 500, N_units = 100
- WTA verified: N_stacked = 220, N_units = 44
- Verified ATP points_change @ 4w: coef = -19.30, SE = 19.99, p = 0.334
- Verified ATP points_change @ 8w: coef = -7.72, SE = 20.33, p = 0.704
- Verified ATP points_change @ 12w: coef = -7.11, SE = 19.69, p = 0.718
- Verified ATP points_change @ 26w: coef = -5.18, SE = 44.61, p = 0.908
- Verified ATP points_change @ 52w: coef = -47.17, SE = 56.01, p = 0.400
- Verified ATP n_main_draws @ 4w: coef = -0.24, SE = 0.41, p = 0.555
- Verified ATP n_main_draws @ 8w: coef = -0.25, SE = 0.41, p = 0.540
- Verified ATP n_main_draws @ 12w: coef = -0.14, SE = 0.42, p = 0.746
- Verified ATP n_main_draws @ 26w: coef = -0.28, SE = 0.59, p = 0.633
- Verified ATP n_main_draws @ 52w: coef = -0.98, SE = 1.29, p = 0.447
- Verified ATP n_matches_250plus @ 4w: coef = -0.80, SE = 0.67, p = 0.238
- Verified ATP n_matches_250plus @ 8w: coef = -0.88, SE = 0.73, p = 0.229
- Verified ATP n_matches_250plus @ 12w: coef = -0.70, SE = 0.77, p = 0.361
- Verified ATP n_matches_250plus @ 26w: coef = -0.64, SE = 1.14, p = 0.575
- Verified ATP n_matches_250plus @ 52w: coef = -1.91, SE = 2.41, p = 0.428
- Verified ATP elo_change @ 4w: coef = -17.81, SE = 7.13, p = 0.012
- Verified ATP elo_change @ 8w: coef = -17.64, SE = 9.19, p = 0.055
- Verified ATP elo_change @ 12w: coef = -19.68, SE = 8.85, p = 0.026
- Verified ATP elo_change @ 26w: coef = -18.50, SE = 13.84, p = 0.181
- Verified ATP elo_change @ 52w: coef = -34.57, SE = 15.76, p = 0.028
- Verified WTA points_change @ 4w: coef = 85.70, SE = 31.17, p = 0.006
- Verified WTA points_change @ 8w: coef = 98.98, SE = 30.29, p = 0.001
- Verified WTA points_change @ 12w: coef = 96.28, SE = 34.55, p = 0.005
- Verified WTA points_change @ 26w: coef = 163.25, SE = 57.58, p = 0.005
- Verified WTA points_change @ 52w: coef = 304.88, SE = 118.98, p = 0.010
- Verified WTA n_main_draws @ 4w: coef = 0.35, SE = 0.58, p = 0.543
- Verified WTA n_main_draws @ 8w: coef = 0.57, SE = 0.59, p = 0.330
- Verified WTA n_main_draws @ 12w: coef = 1.05, SE = 0.60, p = 0.080
- Verified WTA n_main_draws @ 26w: coef = 2.15, SE = 0.76, p = 0.004
- Verified WTA n_main_draws @ 52w: coef = 4.89, SE = 2.06, p = 0.017
- Verified WTA n_matches_250plus @ 4w: coef = 0.44, SE = 1.16, p = 0.702
- Verified WTA n_matches_250plus @ 8w: coef = 1.14, SE = 0.99, p = 0.250
- Verified WTA n_matches_250plus @ 12w: coef = 1.73, SE = 1.01, p = 0.088
- Verified WTA n_matches_250plus @ 26w: coef = 4.25, SE = 1.35, p = 0.002
- Verified WTA n_matches_250plus @ 52w: coef = 7.07, SE = 3.98, p = 0.076
- Verified WTA elo_change @ 4w: coef = 16.11, SE = 8.63, p = 0.062
- Verified WTA elo_change @ 8w: coef = 25.93, SE = 10.15, p = 0.011
- Verified WTA elo_change @ 12w: coef = 42.81, SE = 13.01, p = 0.001
- Verified WTA elo_change @ 26w: coef = 40.73, SE = 14.83, p = 0.006
- Verified WTA elo_change @ 52w: coef = 47.26, SE = 23.99, p = 0.049

## FIX 4: First-LL-Only Stacked

- ATP first-LL only: N = 193 (LL: 103)
- WTA first-LL only: N = 118 (LL: 46)
- First-LL ATP points_change @ 4w: coef = 29.51, SE = 17.03, p = 0.083
- First-LL ATP points_change @ 8w: coef = 34.84, SE = 18.48, p = 0.059
- First-LL ATP points_change @ 12w: coef = 37.09, SE = 19.01, p = 0.051
- First-LL ATP points_change @ 26w: coef = 32.16, SE = 30.05, p = 0.285
- First-LL ATP points_change @ 52w: coef = 4.10, SE = 40.30, p = 0.919
- First-LL ATP n_main_draws @ 4w: coef = -0.24, SE = 0.34, p = 0.493
- First-LL ATP n_main_draws @ 8w: coef = -0.03, SE = 0.33, p = 0.938
- First-LL ATP n_main_draws @ 12w: coef = 0.09, SE = 0.33, p = 0.781
- First-LL ATP n_main_draws @ 26w: coef = 0.50, SE = 0.44, p = 0.248
- First-LL ATP n_main_draws @ 52w: coef = 0.74, SE = 0.91, p = 0.419
- First-LL ATP n_matches_250plus @ 4w: coef = -0.45, SE = 0.68, p = 0.509
- First-LL ATP n_matches_250plus @ 8w: coef = -0.16, SE = 0.67, p = 0.813
- First-LL ATP n_matches_250plus @ 12w: coef = 0.09, SE = 0.64, p = 0.891
- First-LL ATP n_matches_250plus @ 26w: coef = 0.79, SE = 0.82, p = 0.333
- First-LL ATP n_matches_250plus @ 52w: coef = 1.39, SE = 1.71, p = 0.417
- First-LL ATP elo_change @ 4w: coef = -3.73, SE = 5.50, p = 0.497
- First-LL ATP elo_change @ 8w: coef = -3.81, SE = 6.60, p = 0.563
- First-LL ATP elo_change @ 12w: coef = -7.53, SE = 7.05, p = 0.285
- First-LL ATP elo_change @ 26w: coef = -1.21, SE = 9.15, p = 0.894
- First-LL ATP elo_change @ 52w: coef = -10.53, SE = 12.66, p = 0.405
- First-LL WTA points_change @ 4w: coef = 64.09, SE = 25.11, p = 0.011
- First-LL WTA points_change @ 8w: coef = 72.77, SE = 25.00, p = 0.004
- First-LL WTA points_change @ 12w: coef = 34.51, SE = 25.00, p = 0.167
- First-LL WTA points_change @ 26w: coef = 12.50, SE = 37.82, p = 0.741
- First-LL WTA points_change @ 52w: coef = 41.50, SE = 66.23, p = 0.531
- First-LL WTA n_main_draws @ 4w: coef = 0.12, SE = 0.36, p = 0.749
- First-LL WTA n_main_draws @ 8w: coef = 0.46, SE = 0.35, p = 0.188
- First-LL WTA n_main_draws @ 12w: coef = 0.45, SE = 0.37, p = 0.220
- First-LL WTA n_main_draws @ 26w: coef = 0.95, SE = 0.50, p = 0.056
- First-LL WTA n_main_draws @ 52w: coef = 1.39, SE = 1.14, p = 0.221
- First-LL WTA n_matches_250plus @ 4w: coef = -0.11, SE = 0.67, p = 0.866
- First-LL WTA n_matches_250plus @ 8w: coef = 0.84, SE = 0.56, p = 0.137
- First-LL WTA n_matches_250plus @ 12w: coef = 0.87, SE = 0.57, p = 0.127
- First-LL WTA n_matches_250plus @ 26w: coef = 1.91, SE = 0.86, p = 0.026
- First-LL WTA n_matches_250plus @ 52w: coef = 2.22, SE = 1.94, p = 0.253
- First-LL WTA elo_change @ 4w: coef = 11.27, SE = 5.98, p = 0.059
- First-LL WTA elo_change @ 8w: coef = 11.79, SE = 6.55, p = 0.072
- First-LL WTA elo_change @ 12w: coef = 12.37, SE = 7.99, p = 0.122
- First-LL WTA elo_change @ 26w: coef = 16.74, SE = 10.22, p = 0.102
- First-LL WTA elo_change @ 52w: coef = 27.52, SE = 14.52, p = 0.058

## FIX 5: Non-GS Equivalents

- ATP non-GS points_change@26w | Ranking | High ranking pts: coef = 27.25, SE = 15.21, p = 0.073
- ATP non-GS points_change@26w | Ranking | Low ranking pts: coef = 20.20, SE = 12.11, p = 0.095
- ATP non-GS points_change@26w | Age | Older: coef = 36.67, SE = 14.79, p = 0.013
- ATP non-GS points_change@26w | Age | Younger: coef = 16.96, SE = 14.82, p = 0.252
- ATP non-GS points_change@26w | Prior LL | Had prior LL: coef = 16.63, SE = 15.44, p = 0.282
- ATP non-GS points_change@26w | Prior LL | No prior LL: coef = 34.59, SE = 14.24, p = 0.015
- WTA non-GS points_change@26w | Ranking | High ranking pts: coef = 65.83, SE = 22.09, p = 0.003
- WTA non-GS points_change@26w | Ranking | Low ranking pts: coef = 5.52, SE = 15.81, p = 0.727
- WTA non-GS points_change@26w | Age | Older: coef = 35.47, SE = 18.71, p = 0.058
- WTA non-GS points_change@26w | Age | Younger: coef = 24.48, SE = 22.57, p = 0.278
- WTA non-GS points_change@26w | Prior LL | Had prior LL: coef = 41.42, SE = 24.11, p = 0.086
- WTA non-GS points_change@26w | Prior LL | No prior LL: coef = 27.48, SE = 18.11, p = 0.129

