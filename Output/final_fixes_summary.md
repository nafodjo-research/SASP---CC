# Final Fixes Summary (20_final_fixes.R)
Generated: 2026-03-25 12:28:20.642091

## Key changes from script 18/19:
- FIX 1: Controls are now horizon-INVARIANT (Gamma, not gamma_h)
- FIX 2: Hetero tables have horizon-specific rows (beta_h, delta_h)
- FIX 3: Dose tables have horizon-specific rows
- FIX 4: Robustness tables (verified, first-LL) use corrected spec
- FIX 5: Event study figures plot regression coefficients with 95% CI
- FIX 6: Non-GS mirror tables use corrected stacked IV spec
- FIX 7: Shared helpers extracted to scripts/R/utils.R
- FIX 8: All computed objects saved via saveRDS()

## FIX 1: Stacked dynamic models with horizon-INVARIANT controls

- ATP stacked points_change @ 4w: coef = 22.92, SE = 13.69, p = 0.094, N_obs = 1200, N_units = 248
- ATP stacked points_change @ 8w: coef = 22.10, SE = 14.31, p = 0.122, N_obs = 1200, N_units = 248
- ATP stacked points_change @ 12w: coef = 28.58, SE = 15.39, p = 0.063, N_obs = 1200, N_units = 248
- ATP stacked points_change @ 26w: coef = 29.76, SE = 26.19, p = 0.256, N_obs = 1200, N_units = 248
- ATP stacked points_change @ 52w: coef = 2.08, SE = 36.38, p = 0.954, N_obs = 1200, N_units = 248
- ATP stacked n_main_draws @ 4w: coef = -0.04, SE = 0.27, p = 0.879, N_obs = 1240, N_units = 248
- ATP stacked n_main_draws @ 8w: coef = 0.21, SE = 0.25, p = 0.418, N_obs = 1240, N_units = 248
- ATP stacked n_main_draws @ 12w: coef = 0.34, SE = 0.25, p = 0.177, N_obs = 1240, N_units = 248
- ATP stacked n_main_draws @ 26w: coef = 0.74, SE = 0.36, p = 0.041, N_obs = 1240, N_units = 248
- ATP stacked n_main_draws @ 52w: coef = 0.73, SE = 0.80, p = 0.361, N_obs = 1240, N_units = 248
- ATP stacked n_matches_250plus @ 4w: coef = -0.15, SE = 0.53, p = 0.773, N_obs = 1240, N_units = 248
- ATP stacked n_matches_250plus @ 8w: coef = 0.19, SE = 0.50, p = 0.702, N_obs = 1240, N_units = 248
- ATP stacked n_matches_250plus @ 12w: coef = 0.42, SE = 0.48, p = 0.379, N_obs = 1240, N_units = 248
- ATP stacked n_matches_250plus @ 26w: coef = 1.08, SE = 0.68, p = 0.111, N_obs = 1240, N_units = 248
- ATP stacked n_matches_250plus @ 52w: coef = 1.28, SE = 1.51, p = 0.397, N_obs = 1240, N_units = 248
- ATP stacked elo_change @ 4w: coef = -0.32, SE = 4.85, p = 0.947, N_obs = 1127, N_units = 248
- ATP stacked elo_change @ 8w: coef = -0.01, SE = 5.66, p = 0.998, N_obs = 1127, N_units = 248
- ATP stacked elo_change @ 12w: coef = -5.34, SE = 6.04, p = 0.376, N_obs = 1127, N_units = 248
- ATP stacked elo_change @ 26w: coef = -1.64, SE = 8.23, p = 0.842, N_obs = 1127, N_units = 248
- ATP stacked elo_change @ 52w: coef = -10.20, SE = 11.11, p = 0.359, N_obs = 1127, N_units = 248
- WTA stacked points_change @ 4w: coef = 46.18, SE = 22.21, p = 0.038, N_obs = 640, N_units = 132
- WTA stacked points_change @ 8w: coef = 58.07, SE = 23.89, p = 0.015, N_obs = 640, N_units = 132
- WTA stacked points_change @ 12w: coef = 17.65, SE = 25.36, p = 0.486, N_obs = 640, N_units = 132
- WTA stacked points_change @ 26w: coef = -12.23, SE = 39.52, p = 0.757, N_obs = 640, N_units = 132
- WTA stacked points_change @ 52w: coef = -0.14, SE = 64.88, p = 0.998, N_obs = 640, N_units = 132
- WTA stacked n_main_draws @ 4w: coef = -0.09, SE = 0.30, p = 0.762, N_obs = 660, N_units = 132
- WTA stacked n_main_draws @ 8w: coef = 0.31, SE = 0.32, p = 0.343, N_obs = 660, N_units = 132
- WTA stacked n_main_draws @ 12w: coef = 0.27, SE = 0.34, p = 0.424, N_obs = 660, N_units = 132
- WTA stacked n_main_draws @ 26w: coef = 0.69, SE = 0.46, p = 0.128, N_obs = 660, N_units = 132
- WTA stacked n_main_draws @ 52w: coef = 1.13, SE = 1.02, p = 0.269, N_obs = 660, N_units = 132
- WTA stacked n_matches_250plus @ 4w: coef = -0.51, SE = 0.55, p = 0.353, N_obs = 660, N_units = 132
- WTA stacked n_matches_250plus @ 8w: coef = 0.45, SE = 0.52, p = 0.388, N_obs = 660, N_units = 132
- WTA stacked n_matches_250plus @ 12w: coef = 0.40, SE = 0.54, p = 0.455, N_obs = 660, N_units = 132
- WTA stacked n_matches_250plus @ 26w: coef = 1.26, SE = 0.82, p = 0.121, N_obs = 660, N_units = 132
- WTA stacked n_matches_250plus @ 52w: coef = 1.56, SE = 1.75, p = 0.373, N_obs = 660, N_units = 132
- WTA stacked elo_change @ 4w: coef = 9.85, SE = 5.28, p = 0.062, N_obs = 577, N_units = 131
- WTA stacked elo_change @ 8w: coef = 6.74, SE = 6.23, p = 0.280, N_obs = 577, N_units = 131
- WTA stacked elo_change @ 12w: coef = 7.49, SE = 7.77, p = 0.335, N_obs = 577, N_units = 131
- WTA stacked elo_change @ 26w: coef = 13.41, SE = 9.08, p = 0.140, N_obs = 577, N_units = 131
- WTA stacked elo_change @ 52w: coef = 19.38, SE = 14.05, p = 0.168, N_obs = 577, N_units = 131

## FIX 1 (Non-GS IV corrected):
- ATP non-GS IV points_change @ 4w: coef = 32.42, SE = 10.73, p = 0.003, N_horizon = 2666, N_stacked = 12828
- ATP non-GS IV points_change @ 8w: coef = 31.65, SE = 11.10, p = 0.004, N_horizon = 2638, N_stacked = 12828
- ATP non-GS IV points_change @ 12w: coef = 25.36, SE = 11.72, p = 0.031, N_horizon = 2609, N_stacked = 12828
- ATP non-GS IV points_change @ 26w: coef = 33.53, SE = 14.07, p = 0.017, N_horizon = 2545, N_stacked = 12828
- ATP non-GS IV points_change @ 52w: coef = 7.10, SE = 19.87, p = 0.721, N_horizon = 2370, N_stacked = 12828
- ATP non-GS IV n_main_draws @ 4w: coef = -1.23, SE = 0.20, p = 0.000, N_horizon = 2672, N_stacked = 13360
- ATP non-GS IV n_main_draws @ 8w: coef = -0.68, SE = 0.19, p = 0.000, N_horizon = 2672, N_stacked = 13360
- ATP non-GS IV n_main_draws @ 12w: coef = -0.07, SE = 0.18, p = 0.721, N_horizon = 2672, N_stacked = 13360
- ATP non-GS IV n_main_draws @ 26w: coef = 1.64, SE = 0.23, p = 0.000, N_horizon = 2672, N_stacked = 13360
- ATP non-GS IV n_main_draws @ 52w: coef = 4.34, SE = 0.43, p = 0.000, N_horizon = 2672, N_stacked = 13360
- ATP non-GS IV n_matches_250plus @ 4w: coef = -2.10, SE = 0.37, p = 0.000, N_horizon = 2672, N_stacked = 13360
- ATP non-GS IV n_matches_250plus @ 8w: coef = -1.10, SE = 0.36, p = 0.002, N_horizon = 2672, N_stacked = 13360
- ATP non-GS IV n_matches_250plus @ 12w: coef = -0.11, SE = 0.36, p = 0.761, N_horizon = 2672, N_stacked = 13360
- ATP non-GS IV n_matches_250plus @ 26w: coef = 3.05, SE = 0.45, p = 0.000, N_horizon = 2672, N_stacked = 13360
- ATP non-GS IV n_matches_250plus @ 52w: coef = 7.77, SE = 0.83, p = 0.000, N_horizon = 2672, N_stacked = 13360
- ATP non-GS IV elo_change @ 4w: coef = -0.12, SE = 3.46, p = 0.973, N_horizon = 2461, N_stacked = 11322
- ATP non-GS IV elo_change @ 8w: coef = 0.97, SE = 3.75, p = 0.796, N_horizon = 2319, N_stacked = 11322
- ATP non-GS IV elo_change @ 12w: coef = -2.02, SE = 3.84, p = 0.598, N_horizon = 2327, N_stacked = 11322
- ATP non-GS IV elo_change @ 26w: coef = -0.72, SE = 4.78, p = 0.880, N_horizon = 2175, N_stacked = 11322
- ATP non-GS IV elo_change @ 52w: coef = -8.16, SE = 5.76, p = 0.157, N_horizon = 2040, N_stacked = 11322
- WTA non-GS IV points_change @ 4w: coef = 33.99, SE = 16.69, p = 0.042, N_horizon = 2526, N_stacked = 12046
- WTA non-GS IV points_change @ 8w: coef = 29.11, SE = 17.44, p = 0.095, N_horizon = 2484, N_stacked = 12046
- WTA non-GS IV points_change @ 12w: coef = 29.27, SE = 18.35, p = 0.111, N_horizon = 2446, N_stacked = 12046
- WTA non-GS IV points_change @ 26w: coef = 16.89, SE = 20.40, p = 0.408, N_horizon = 2376, N_stacked = 12046
- WTA non-GS IV points_change @ 52w: coef = -0.94, SE = 27.48, p = 0.973, N_horizon = 2214, N_stacked = 12046
- WTA non-GS IV n_main_draws @ 4w: coef = -1.00, SE = 0.22, p = 0.000, N_horizon = 2543, N_stacked = 12715
- WTA non-GS IV n_main_draws @ 8w: coef = -0.52, SE = 0.20, p = 0.009, N_horizon = 2543, N_stacked = 12715
- WTA non-GS IV n_main_draws @ 12w: coef = -0.05, SE = 0.20, p = 0.789, N_horizon = 2543, N_stacked = 12715
- WTA non-GS IV n_main_draws @ 26w: coef = 1.26, SE = 0.24, p = 0.000, N_horizon = 2543, N_stacked = 12715
- WTA non-GS IV n_main_draws @ 52w: coef = 2.65, SE = 0.45, p = 0.000, N_horizon = 2543, N_stacked = 12715
- WTA non-GS IV n_matches_250plus @ 4w: coef = -1.88, SE = 0.43, p = 0.000, N_horizon = 2543, N_stacked = 12715
- WTA non-GS IV n_matches_250plus @ 8w: coef = -1.09, SE = 0.40, p = 0.007, N_horizon = 2543, N_stacked = 12715
- WTA non-GS IV n_matches_250plus @ 12w: coef = -0.25, SE = 0.40, p = 0.527, N_horizon = 2543, N_stacked = 12715
- WTA non-GS IV n_matches_250plus @ 26w: coef = 2.02, SE = 0.51, p = 0.000, N_horizon = 2543, N_stacked = 12715
- WTA non-GS IV n_matches_250plus @ 52w: coef = 4.84, SE = 0.85, p = 0.000, N_horizon = 2543, N_stacked = 12715
- WTA non-GS IV elo_change @ 4w: coef = 6.59, SE = 4.00, p = 0.100, N_horizon = 2420, N_stacked = 10745
- WTA non-GS IV elo_change @ 8w: coef = 5.40, SE = 4.25, p = 0.203, N_horizon = 2235, N_stacked = 10745
- WTA non-GS IV elo_change @ 12w: coef = 8.74, SE = 4.51, p = 0.052, N_horizon = 2158, N_stacked = 10745
- WTA non-GS IV elo_change @ 26w: coef = 0.66, SE = 5.47, p = 0.904, N_horizon = 2039, N_stacked = 10745
- WTA non-GS IV elo_change @ 52w: coef = -6.86, SE = 6.30, p = 0.276, N_horizon = 1893, N_stacked = 10745

## FIX 2: Heterogeneity with horizon-specific rows

- ATP points_change @ 4w | Ranking | beta_h (low rank base): coef = 23.31 (17.10), p = 0.173
- ATP points_change @ 4w | Ranking | delta_h (high rank): coef = -0.28 (24.10), p = 0.991
- ATP points_change @ 8w | Ranking | beta_h (low rank base): coef = 20.28 (18.08), p = 0.262
- ATP points_change @ 8w | Ranking | delta_h (high rank): coef = 4.05 (25.25), p = 0.873
- ATP points_change @ 12w | Ranking | beta_h (low rank base): coef = 30.71 (18.09), p = 0.090
- ATP points_change @ 12w | Ranking | delta_h (high rank): coef = -3.78 (27.68), p = 0.892
- ATP points_change @ 26w | Ranking | beta_h (low rank base): coef = 22.28 (26.95), p = 0.408
- ATP points_change @ 26w | Ranking | delta_h (high rank): coef = 14.83 (37.71), p = 0.694
- ATP points_change @ 52w | Ranking | beta_h (low rank base): coef = 20.82 (37.91), p = 0.583
- ATP points_change @ 52w | Ranking | delta_h (high rank): coef = -38.67 (50.73), p = 0.446
- ATP points_change @ 4w | Age | beta_h (young base): coef = 6.62 (18.85), p = 0.726
- ATP points_change @ 4w | Age | delta_h (older): coef = 30.47 (26.10), p = 0.243
- ATP points_change @ 8w | Age | beta_h (young base): coef = 12.92 (20.79), p = 0.534
- ATP points_change @ 8w | Age | delta_h (older): coef = 16.55 (27.64), p = 0.549
- ATP points_change @ 12w | Age | beta_h (young base): coef = 28.11 (23.43), p = 0.230
- ATP points_change @ 12w | Age | delta_h (older): coef = 0.90 (31.08), p = 0.977
- ATP points_change @ 26w | Age | beta_h (young base): coef = 56.84 (35.26), p = 0.107
- ATP points_change @ 26w | Age | delta_h (older): coef = -54.06 (40.84), p = 0.186
- ATP points_change @ 52w | Age | beta_h (young base): coef = 86.00 (49.13), p = 0.080
- ATP points_change @ 52w | Age | delta_h (older): coef = -162.08 (51.56), p = 0.002
- ATP points_change @ 4w | Prior LL | beta_h (no prior base): coef = 25.05 (16.91), p = 0.139
- ATP points_change @ 4w | Prior LL | delta_h (had prior LL): coef = -8.13 (21.50), p = 0.705
- ATP points_change @ 8w | Prior LL | beta_h (no prior base): coef = 31.71 (18.11), p = 0.080
- ATP points_change @ 8w | Prior LL | delta_h (had prior LL): coef = -22.01 (21.46), p = 0.305
- ATP points_change @ 12w | Prior LL | beta_h (no prior base): coef = 50.33 (20.17), p = 0.013
- ATP points_change @ 12w | Prior LL | delta_h (had prior LL): coef = -44.38 (23.70), p = 0.061
- ATP points_change @ 26w | Prior LL | beta_h (no prior base): coef = 65.92 (33.79), p = 0.051
- ATP points_change @ 26w | Prior LL | delta_h (had prior LL): coef = -71.50 (34.84), p = 0.040
- ATP points_change @ 52w | Prior LL | beta_h (no prior base): coef = 42.55 (46.02), p = 0.355
- ATP points_change @ 52w | Prior LL | delta_h (had prior LL): coef = -82.76 (48.30), p = 0.087
- WTA points_change @ 4w | Ranking | beta_h (low rank base): coef = 69.89 (25.39), p = 0.006
- WTA points_change @ 4w | Ranking | delta_h (high rank): coef = -52.29 (44.15), p = 0.236
- WTA points_change @ 8w | Ranking | beta_h (low rank base): coef = 81.64 (25.64), p = 0.001
- WTA points_change @ 8w | Ranking | delta_h (high rank): coef = -52.07 (46.32), p = 0.261
- WTA points_change @ 12w | Ranking | beta_h (low rank base): coef = 53.94 (29.03), p = 0.063
- WTA points_change @ 12w | Ranking | delta_h (high rank): coef = -71.15 (49.36), p = 0.149
- WTA points_change @ 26w | Ranking | beta_h (low rank base): coef = 67.39 (43.05), p = 0.117
- WTA points_change @ 26w | Ranking | delta_h (high rank): coef = -137.94 (63.31), p = 0.029
- WTA points_change @ 52w | Ranking | beta_h (low rank base): coef = 185.56 (75.81), p = 0.014
- WTA points_change @ 52w | Ranking | delta_h (high rank): coef = -310.63 (101.18), p = 0.002
- WTA points_change @ 4w | Age | beta_h (young base): coef = -14.65 (23.17), p = 0.527
- WTA points_change @ 4w | Age | delta_h (older): coef = 111.93 (42.34), p = 0.008
- WTA points_change @ 8w | Age | beta_h (young base): coef = -2.24 (26.30), p = 0.932
- WTA points_change @ 8w | Age | delta_h (older): coef = 111.03 (41.27), p = 0.007
- WTA points_change @ 12w | Age | beta_h (young base): coef = -21.59 (29.25), p = 0.460
- WTA points_change @ 12w | Age | delta_h (older): coef = 73.10 (41.89), p = 0.081
- WTA points_change @ 26w | Age | beta_h (young base): coef = -12.85 (49.09), p = 0.794
- WTA points_change @ 26w | Age | delta_h (older): coef = 3.56 (59.62), p = 0.952
- WTA points_change @ 52w | Age | beta_h (young base): coef = 76.84 (92.88), p = 0.408
- WTA points_change @ 52w | Age | delta_h (older): coef = -127.23 (105.49), p = 0.228
- WTA points_change @ 4w | Prior LL | beta_h (no prior base): coef = 41.72 (21.80), p = 0.056
- WTA points_change @ 4w | Prior LL | delta_h (had prior LL): coef = 11.08 (47.80), p = 0.817
- WTA points_change @ 8w | Prior LL | beta_h (no prior base): coef = 56.46 (23.26), p = 0.015
- WTA points_change @ 8w | Prior LL | delta_h (had prior LL): coef = 3.78 (48.38), p = 0.938
- WTA points_change @ 12w | Prior LL | beta_h (no prior base): coef = 36.52 (24.86), p = 0.142
- WTA points_change @ 12w | Prior LL | delta_h (had prior LL): coef = -48.87 (50.92), p = 0.337
- WTA points_change @ 26w | Prior LL | beta_h (no prior base): coef = 30.41 (43.67), p = 0.486
- WTA points_change @ 26w | Prior LL | delta_h (had prior LL): coef = -114.22 (62.71), p = 0.069
- WTA points_change @ 52w | Prior LL | beta_h (no prior base): coef = 78.75 (80.82), p = 0.330
- WTA points_change @ 52w | Prior LL | delta_h (had prior LL): coef = -204.79 (102.59), p = 0.046

## FIX 3: Dose table with horizon-specific rows

- ATP dose points_change @ 4w | beta_h (0-win base): coef = 3.92 (13.98), p = 0.779
- ATP dose points_change @ 4w | delta_h (1 win): coef = 51.54 (21.29), p = 0.015
- ATP dose points_change @ 4w | delta_h (2+ wins): coef = 66.76 (33.31), p = 0.045
- ATP dose points_change @ 8w | beta_h (0-win base): coef = 1.72 (15.08), p = 0.909
- ATP dose points_change @ 8w | delta_h (1 win): coef = 56.86 (22.46), p = 0.011
- ATP dose points_change @ 8w | delta_h (2+ wins): coef = 71.25 (34.16), p = 0.037
- ATP dose points_change @ 12w | beta_h (0-win base): coef = 12.98 (16.32), p = 0.426
- ATP dose points_change @ 12w | delta_h (1 win): coef = 41.00 (28.57), p = 0.151
- ATP dose points_change @ 12w | delta_h (2+ wins): coef = 62.65 (32.78), p = 0.056
- ATP dose points_change @ 26w | beta_h (0-win base): coef = 6.23 (27.99), p = 0.824
- ATP dose points_change @ 26w | delta_h (1 win): coef = 37.51 (36.67), p = 0.306
- ATP dose points_change @ 26w | delta_h (2+ wins): coef = 145.71 (58.50), p = 0.013
- ATP dose points_change @ 52w | beta_h (0-win base): coef = -23.51 (37.71), p = 0.533
- ATP dose points_change @ 52w | delta_h (1 win): coef = 60.97 (57.36), p = 0.288
- ATP dose points_change @ 52w | delta_h (2+ wins): coef = 113.36 (73.57), p = 0.123
- WTA dose points_change @ 4w | beta_h (0-win base): coef = 30.07 (27.26), p = 0.270
- WTA dose points_change @ 4w | delta_h (1 win): coef = 39.28 (30.69), p = 0.201
- WTA dose points_change @ 4w | delta_h (2+ wins): coef = 72.88 (118.12), p = 0.537
- WTA dose points_change @ 8w | beta_h (0-win base): coef = 33.78 (28.62), p = 0.238
- WTA dose points_change @ 8w | delta_h (1 win): coef = 52.13 (31.73), p = 0.100
- WTA dose points_change @ 8w | delta_h (2+ wins): coef = 171.87 (79.75), p = 0.031
- WTA dose points_change @ 12w | beta_h (0-win base): coef = -2.98 (30.41), p = 0.922
- WTA dose points_change @ 12w | delta_h (1 win): coef = 43.73 (38.65), p = 0.258
- WTA dose points_change @ 12w | delta_h (2+ wins): coef = 152.99 (81.20), p = 0.060
- WTA dose points_change @ 26w | beta_h (0-win base): coef = -37.80 (44.19), p = 0.392
- WTA dose points_change @ 26w | delta_h (1 win): coef = 44.16 (56.73), p = 0.436
- WTA dose points_change @ 26w | delta_h (2+ wins): coef = 265.71 (177.90), p = 0.135
- WTA dose points_change @ 52w | beta_h (0-win base): coef = -17.34 (69.70), p = 0.804
- WTA dose points_change @ 52w | delta_h (1 win): coef = -5.19 (100.22), p = 0.959
- WTA dose points_change @ 52w | delta_h (2+ wins): coef = 436.12 (499.89), p = 0.383

## FIX 4: Robustness tables with corrected control spec

- Verified ATP: N = 100 (LL: 38)
- Verified WTA: N = 44 (LL: 14)
- Verified ATP pts @ 4w: coef = -19.30 (19.99), p = 0.334
- Verified ATP pts @ 8w: coef = -7.72 (20.33), p = 0.704
- Verified ATP pts @ 12w: coef = -7.11 (19.69), p = 0.718
- Verified ATP pts @ 26w: coef = -5.18 (44.61), p = 0.908
- Verified ATP pts @ 52w: coef = -47.17 (56.01), p = 0.400
- Verified WTA pts @ 4w: coef = 85.70 (31.17), p = 0.006
- Verified WTA pts @ 8w: coef = 98.98 (30.29), p = 0.001
- Verified WTA pts @ 12w: coef = 96.28 (34.55), p = 0.005
- Verified WTA pts @ 26w: coef = 163.25 (57.58), p = 0.005
- Verified WTA pts @ 52w: coef = 304.88 (118.98), p = 0.010

- First-LL ATP: N = 193 (LL: 103)
- First-LL WTA: N = 118 (LL: 46)
- First-LL ATP pts @ 4w: coef = 29.51 (17.03), p = 0.083
- First-LL ATP pts @ 8w: coef = 34.84 (18.48), p = 0.059
- First-LL ATP pts @ 12w: coef = 37.09 (19.01), p = 0.051
- First-LL ATP pts @ 26w: coef = 32.16 (30.05), p = 0.285
- First-LL ATP pts @ 52w: coef = 4.10 (40.30), p = 0.919
- First-LL WTA pts @ 4w: coef = 64.09 (25.11), p = 0.011
- First-LL WTA pts @ 8w: coef = 72.77 (25.00), p = 0.004
- First-LL WTA pts @ 12w: coef = 34.51 (25.00), p = 0.167
- First-LL WTA pts @ 26w: coef = 12.50 (37.82), p = 0.741
- First-LL WTA pts @ 52w: coef = 41.50 (66.23), p = 0.531

## FIX 5: Event study figures from corrected stacked coefficients

- ATP event study beta_4w = 22.92 [-3.92, 49.76]
- ATP event study beta_8w = 22.10 [-5.94, 50.15]
- ATP event study beta_12w = 28.58 [-1.59, 58.75]
- ATP event study beta_26w = 29.76 [-21.58, 81.09]
- ATP event study beta_52w = 2.08 [-69.22, 73.38]
- WTA event study beta_4w = 46.18 [2.65, 89.71]
- WTA event study beta_8w = 58.07 [11.25, 104.90]
- WTA event study beta_12w = 17.65 [-32.06, 67.37]
- WTA event study beta_26w = -12.23 [-89.69, 65.23]
- WTA event study beta_52w = -0.14 [-127.30, 127.03]

## FIX 6: Non-GS mirror tables (hetero, dose) with corrected spec

- ATP non-GS pts@26w | Ranking | High ranking pts: coef = 40.01, SE = 27.23, p = 0.142
- ATP non-GS pts@26w | Ranking | Low ranking pts: coef = 4.64, SE = 18.49, p = 0.802
- ATP non-GS pts@26w | Age | Older: coef = 65.05, SE = 22.60, p = 0.004
- ATP non-GS pts@26w | Age | Younger: coef = 8.16, SE = 22.54, p = 0.717
- ATP non-GS pts@26w | Prior LL | Had prior LL: coef = 18.46, SE = 23.25, p = 0.427
- ATP non-GS pts@26w | Prior LL | No prior LL: coef = 64.55, SE = 21.45, p = 0.003
- WTA non-GS pts@26w | Ranking | High ranking pts: coef = 67.73, SE = 37.09, p = 0.068
- WTA non-GS pts@26w | Ranking | Low ranking pts: coef = 3.31, SE = 20.90, p = 0.874
- WTA non-GS pts@26w | Age | Older: coef = -1.67, SE = 27.53, p = 0.952
- WTA non-GS pts@26w | Age | Younger: coef = 52.13, SE = 33.41, p = 0.119
- WTA non-GS pts@26w | Prior LL | Had prior LL: coef = 36.70, SE = 34.32, p = 0.285
- WTA non-GS pts@26w | Prior LL | No prior LL: coef = 9.98, SE = 22.86, p = 0.663

