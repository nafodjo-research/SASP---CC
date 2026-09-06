# Horizon Fixed Effects Test
Generated: 2026-03-25 11:16:59.039808

## Specification Comparison
- Spec A (no HFE): Y ~ got_ll:factor(horizon) + pre_rank_pts + pre_rank_pts_sq + player_age | event_id
- Spec B (with HFE): Y ~ got_ll:factor(horizon) + pre_rank_pts + pre_rank_pts_sq + player_age | event_id + horizon

## Coefficient Comparison

- ATP points_change @ 4w: no_HFE = 18.76 (15.84), HFE = 22.92 (13.69), diff = 4.16 (22.2%)
- ATP points_change @ 8w: no_HFE = 23.12 (15.91), HFE = 22.10 (14.31), diff = -1.02 (-4.4%)
- ATP points_change @ 12w: no_HFE = 24.16 (16.47), HFE = 28.58 (15.39), diff = 4.42 (18.3%)
- ATP points_change @ 26w: no_HFE = 29.12 (21.17), HFE = 29.76 (26.19), diff = 0.64 (2.2%)
- ATP points_change @ 52w: no_HFE = 11.06 (27.03), HFE = 2.08 (36.38), diff = -8.98 (-81.2%)
- ATP n_main_draws @ 4w: no_HFE = -2.65 (0.28), HFE = -0.04 (0.27), diff = 2.61 (98.5%)
- ATP n_main_draws @ 8w: no_HFE = -1.62 (0.28), HFE = 0.21 (0.25), diff = 1.83 (112.7%)
- ATP n_main_draws @ 12w: no_HFE = -1.04 (0.28), HFE = 0.34 (0.25), diff = 1.38 (132.9%)
- ATP n_main_draws @ 26w: no_HFE = 1.26 (0.33), HFE = 0.74 (0.36), diff = -0.52 (-41.0%)
- ATP n_main_draws @ 52w: no_HFE = 6.03 (0.62), HFE = 0.73 (0.80), diff = -5.30 (-87.9%)
- ATP n_matches_250plus @ 4w: no_HFE = -4.22 (0.56), HFE = -0.15 (0.53), diff = 4.07 (96.4%)
- ATP n_matches_250plus @ 8w: no_HFE = -2.64 (0.54), HFE = 0.19 (0.50), diff = 2.83 (107.2%)
- ATP n_matches_250plus @ 12w: no_HFE = -1.70 (0.52), HFE = 0.42 (0.48), diff = 2.13 (124.9%)
- ATP n_matches_250plus @ 26w: no_HFE = 1.87 (0.63), HFE = 1.08 (0.68), diff = -0.79 (-42.2%)
- ATP n_matches_250plus @ 52w: no_HFE = 9.52 (1.18), HFE = 1.28 (1.51), diff = -8.24 (-86.5%)
- ATP elo_change @ 4w: no_HFE = -2.62 (5.25), HFE = -0.32 (4.85), diff = 2.29 (87.7%)
- ATP elo_change @ 8w: no_HFE = -0.40 (5.42), HFE = -0.01 (5.66), diff = 0.38 (96.3%)
- ATP elo_change @ 12w: no_HFE = -3.69 (5.71), HFE = -5.34 (6.04), diff = -1.66 (-45.0%)
- ATP elo_change @ 26w: no_HFE = -1.63 (7.18), HFE = -1.64 (8.23), diff = -0.00 (-0.3%)
- ATP elo_change @ 52w: no_HFE = -8.89 (8.65), HFE = -10.20 (11.11), diff = -1.31 (-14.7%)
- WTA points_change @ 4w: no_HFE = 8.73 (24.99), HFE = 46.18 (22.21), diff = 37.45 (429.1%)
- WTA points_change @ 8w: no_HFE = 28.43 (25.43), HFE = 58.07 (23.89), diff = 29.64 (104.3%)
- WTA points_change @ 12w: no_HFE = 14.65 (26.47), HFE = 17.65 (25.36), diff = 3.00 (20.5%)
- WTA points_change @ 26w: no_HFE = 27.83 (33.94), HFE = -12.23 (39.52), diff = -40.06 (-144.0%)
- WTA points_change @ 52w: no_HFE = 34.85 (56.72), HFE = -0.14 (64.88), diff = -34.99 (-100.4%)
- WTA n_main_draws @ 4w: no_HFE = -2.82 (0.34), HFE = -0.09 (0.30), diff = 2.73 (96.7%)
- WTA n_main_draws @ 8w: no_HFE = -1.73 (0.35), HFE = 0.31 (0.32), diff = 2.04 (117.8%)
- WTA n_main_draws @ 12w: no_HFE = -1.06 (0.34), HFE = 0.27 (0.34), diff = 1.33 (125.4%)
- WTA n_main_draws @ 26w: no_HFE = 1.11 (0.44), HFE = 0.69 (0.46), diff = -0.41 (-37.3%)
- WTA n_main_draws @ 52w: no_HFE = 6.81 (0.88), HFE = 1.13 (1.02), diff = -5.68 (-83.4%)
- WTA n_matches_250plus @ 4w: no_HFE = -3.89 (0.59), HFE = -0.51 (0.55), diff = 3.38 (86.8%)
- WTA n_matches_250plus @ 8w: no_HFE = -2.17 (0.56), HFE = 0.45 (0.52), diff = 2.62 (120.9%)
- WTA n_matches_250plus @ 12w: no_HFE = -1.33 (0.55), HFE = 0.40 (0.54), diff = 1.74 (130.1%)
- WTA n_matches_250plus @ 26w: no_HFE = 1.89 (0.76), HFE = 1.26 (0.82), diff = -0.62 (-33.0%)
- WTA n_matches_250plus @ 52w: no_HFE = 8.67 (1.49), HFE = 1.56 (1.75), diff = -7.11 (-82.1%)
- WTA elo_change @ 4w: no_HFE = 8.08 (5.76), HFE = 9.85 (5.28), diff = 1.77 (21.8%)
- WTA elo_change @ 8w: no_HFE = 8.17 (6.16), HFE = 6.74 (6.23), diff = -1.44 (-17.6%)
- WTA elo_change @ 12w: no_HFE = 10.17 (7.16), HFE = 7.49 (7.77), diff = -2.67 (-26.3%)
- WTA elo_change @ 26w: no_HFE = 14.71 (7.95), HFE = 13.41 (9.08), diff = -1.29 (-8.8%)
- WTA elo_change @ 52w: no_HFE = 15.84 (12.17), HFE = 19.38 (14.05), diff = 3.54 (22.3%)

## R-squared Comparison

- ATP points_change: no_HFE R2 = 0.2030, HFE R2 = 0.2034, improvement = 0.0004
- ATP n_main_draws: no_HFE R2 = 0.3506, HFE R2 = 0.5257, improvement = 0.1752
- ATP n_matches_250plus: no_HFE R2 = 0.3248, HFE R2 = 0.4570, improvement = 0.1322
- ATP elo_change: no_HFE R2 = 0.1668, HFE R2 = 0.1672, improvement = 0.0004
- WTA points_change: no_HFE R2 = 0.1975, HFE R2 = 0.2132, improvement = 0.0157
- WTA n_main_draws: no_HFE R2 = 0.3473, HFE R2 = 0.6059, improvement = 0.2586
- WTA n_matches_250plus: no_HFE R2 = 0.3671, HFE R2 = 0.5406, improvement = 0.1736
- WTA elo_change: no_HFE R2 = 0.1695, HFE R2 = 0.1710, improvement = 0.0015

## Decision
- Mean |%change| in treatment coefficients: 77.8%
- Threshold for adoption: 5%
- Decision: ADOPT horizon FE -- coefficients change meaningfully
