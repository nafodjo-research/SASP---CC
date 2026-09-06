# Final Table Fixes Summary (21_final_table_fixes.R)
Generated: 2026-03-25 17:07:31.288821

## Key changes:
- FIX 1: Investigated verified subsample contradictions (Cook's distance, trimmed re-estimation)
- FIX 2: Event study figures now show RAW MEAN trajectories (treated vs control)
- FIX 3: Non-GS distribution table splits ATP 250 and ATP 500
- FIX 4: Non-GS immediate effects table now includes md_points (ranking points at event)
- FIX 5: Verified all GS tables have non-GS mirror equivalents
- FIX 6: Created first-LL non-GS tables (table_firstll_stacked_nongs_atp/wta.tex)

## FIX 1: Verified Subsample Investigation

- Verified lottery events: 34
- Verified ATP: N = 100 (LL: 38, Control: 62)
- Verified WTA: N = 44 (LL: 14, Control: 30)

### ATP Verified Elo Analysis
- ATP Elo: top Cook's D = 0.0477
- Influential obs (above 4/(n-p)): 30 / 458
- ATP Elo (trimmed) @ 4w: coef = -18.06, SE = 6.97, p = 0.010
- ATP Elo (trimmed) @ 8w: coef = -17.87, SE = 8.83, p = 0.043
- ATP Elo (trimmed) @ 12w: coef = -20.36, SE = 8.52, p = 0.017
- ATP Elo (trimmed) @ 26w: coef = -18.17, SE = 12.55, p = 0.148
- ATP Elo (trimmed) @ 52w: coef = -38.28, SE = 15.30, p = 0.012

### WTA Verified Elo Analysis
- WTA verified treated: N = 14
- WTA treated Elo @26w: mean = 43.4, sd = 45.5
- WTA Elo (trimmed) @ 4w: coef = 17.83, SE = 8.77, p = 0.042
- WTA Elo (trimmed) @ 8w: coef = 28.01, SE = 10.36, p = 0.007
- WTA Elo (trimmed) @ 12w: coef = 44.92, SE = 13.35, p = 0.001
- WTA Elo (trimmed) @ 26w: coef = 42.01, SE = 14.62, p = 0.004
- WTA Elo (trimmed) @ 52w: coef = 39.48, SE = 22.78, p = 0.083

### Regenerating table_verified_stacked.tex with N_units and N_obs

- Verified ATP points_change @ 4w: coef = -19.30 (19.99), p = 0.334
- Verified ATP points_change @ 8w: coef = -7.72 (20.33), p = 0.704
- Verified ATP points_change @ 12w: coef = -7.11 (19.69), p = 0.718
- Verified ATP points_change @ 26w: coef = -5.18 (44.61), p = 0.908
- Verified ATP points_change @ 52w: coef = -47.17 (56.01), p = 0.400
- Verified ATP n_main_draws @ 4w: coef = -0.24 (0.41), p = 0.555
- Verified ATP n_main_draws @ 8w: coef = -0.25 (0.41), p = 0.540
- Verified ATP n_main_draws @ 12w: coef = -0.14 (0.42), p = 0.746
- Verified ATP n_main_draws @ 26w: coef = -0.28 (0.59), p = 0.633
- Verified ATP n_main_draws @ 52w: coef = -0.98 (1.29), p = 0.447
- Verified ATP n_matches_250plus @ 4w: coef = -0.80 (0.67), p = 0.238
- Verified ATP n_matches_250plus @ 8w: coef = -0.88 (0.73), p = 0.229
- Verified ATP n_matches_250plus @ 12w: coef = -0.70 (0.77), p = 0.361
- Verified ATP n_matches_250plus @ 26w: coef = -0.64 (1.14), p = 0.575
- Verified ATP n_matches_250plus @ 52w: coef = -1.91 (2.41), p = 0.428
- Verified ATP elo_change @ 4w: coef = -17.81 (7.13), p = 0.012
- Verified ATP elo_change @ 8w: coef = -17.64 (9.19), p = 0.055
- Verified ATP elo_change @ 12w: coef = -19.68 (8.85), p = 0.026
- Verified ATP elo_change @ 26w: coef = -18.50 (13.84), p = 0.181
- Verified ATP elo_change @ 52w: coef = -34.57 (15.76), p = 0.028
- Verified WTA points_change @ 4w: coef = 85.70 (31.17), p = 0.006
- Verified WTA points_change @ 8w: coef = 98.98 (30.29), p = 0.001
- Verified WTA points_change @ 12w: coef = 96.28 (34.55), p = 0.005
- Verified WTA points_change @ 26w: coef = 163.25 (57.58), p = 0.005
- Verified WTA points_change @ 52w: coef = 304.88 (118.98), p = 0.010
- Verified WTA n_main_draws @ 4w: coef = 0.35 (0.58), p = 0.543
- Verified WTA n_main_draws @ 8w: coef = 0.57 (0.59), p = 0.330
- Verified WTA n_main_draws @ 12w: coef = 1.05 (0.60), p = 0.080
- Verified WTA n_main_draws @ 26w: coef = 2.15 (0.76), p = 0.004
- Verified WTA n_main_draws @ 52w: coef = 4.89 (2.06), p = 0.017
- Verified WTA n_matches_250plus @ 4w: coef = 0.44 (1.16), p = 0.702
- Verified WTA n_matches_250plus @ 8w: coef = 1.14 (0.99), p = 0.250
- Verified WTA n_matches_250plus @ 12w: coef = 1.73 (1.01), p = 0.088
- Verified WTA n_matches_250plus @ 26w: coef = 4.25 (1.35), p = 0.002
- Verified WTA n_matches_250plus @ 52w: coef = 7.07 (3.98), p = 0.076
- Verified WTA elo_change @ 4w: coef = 16.11 (8.63), p = 0.062
- Verified WTA elo_change @ 8w: coef = 25.93 (10.15), p = 0.011
- Verified WTA elo_change @ 12w: coef = 42.81 (13.01), p = 0.001
- Verified WTA elo_change @ 26w: coef = 40.73 (14.83), p = 0.006
- Verified WTA elo_change @ 52w: coef = 47.26 (23.99), p = 0.049

## FIX 2: Event study figures with raw mean trajectories

- ATP Lucky Loser @0w: mean = 0 [0, 0] (N=132)
- ATP Lucky Loser @4w: mean = 16.7 [5.5, 27.9] (N=132)
- ATP Lucky Loser @12w: mean = 22.1 [0.8, 43.5] (N=127)
- ATP Lucky Loser @26w: mean = 28.1 [-7.5, 63.6] (N=126)
- ATP Lucky Loser @52w: mean = 11.8 [-38.7, 62.4] (N=117)
- ATP Control @0w: mean = 0 [0, 0] (N=116)
- ATP Control @4w: mean = -4.2 [-13.3, 5] (N=116)
- ATP Control @12w: mean = -4.4 [-24.1, 15.2] (N=116)
- ATP Control @26w: mean = 2.3 [-37.6, 42.2] (N=113)
- ATP Control @52w: mean = 13.8 [-42.1, 69.6] (N=106)
- WTA Lucky Loser @0w: mean = 0 [0, 0] (N=54)
- WTA Lucky Loser @4w: mean = 40.7 [22, 59.3] (N=54)
- WTA Lucky Loser @12w: mean = 46.6 [6.4, 86.7] (N=54)
- WTA Lucky Loser @26w: mean = 58.7 [-3, 120.5] (N=53)
- WTA Lucky Loser @52w: mean = 65 [-48.5, 178.5] (N=46)
- WTA Control @0w: mean = 0 [0, 0] (N=78)
- WTA Control @4w: mean = 12.2 [0.7, 23.7] (N=78)
- WTA Control @12w: mean = 46.7 [16.7, 76.6] (N=78)
- WTA Control @26w: mean = 88.7 [37.3, 140.2] (N=75)
- WTA Control @52w: mean = 85.3 [14.7, 155.8] (N=70)

## FIX 3: Non-GS distribution table with ATP 250/500 split

- ATP distribution:
  Masters 1000: LL=188, Ctrl=674, Total=862
  ATP 500: LL=203, Ctrl=391, Total=594
  ATP 250: LL=471, Ctrl=745, Total=1216
- WTA distribution:
  WTA 1000: LL=58, Ctrl=237, Total=295
  WTA 500: LL=332, Ctrl=808, Total=1140
  WTA 250: LL=365, Ctrl=745, Total=1110

## FIX 4: Non-GS immediate effects with ranking points

- Non-GS md_points: mean (LL) = 21.5, max = 650
- Non-GS md_points: mean (Ctrl) = 0
- ATP non-GS md_any_win: LL mean = 0.36, Ctrl mean = 0.00, IV = 0.36 (0.02)
- ATP non-GS md_matches_played: LL mean = 1.54, Ctrl mean = 0.00, IV = 1.53 (0.04)
- ATP non-GS md_points: LL mean = 27.89, Ctrl mean = 0.00, IV = 26.14 (1.41)
- WTA non-GS md_any_win: LL mean = 0.37, Ctrl mean = 0.00, IV = 0.38 (0.02)
- WTA non-GS md_matches_played: LL mean = 1.50, Ctrl mean = 0.00, IV = 1.52 (0.04)
- WTA non-GS md_points: LL mean = 14.19, Ctrl mean = 0.00, IV = 13.63 (0.73)

## FIX 5: Non-GS table structure verification

- table_immediate.tex (exists) -> table_immediate_nongs.tex (exists)
- table_dynamic_stacked_atp.tex (exists) -> table_dynamic_stacked_nongs_atp.tex (exists)
- table_dynamic_stacked_wta.tex (exists) -> table_dynamic_stacked_nongs_wta.tex (exists)
- table_hetero_stacked_atp.tex (exists) -> table_hetero_stacked_nongs_atp.tex (exists)
- table_hetero_stacked_wta.tex (exists) -> table_hetero_stacked_nongs_wta.tex (exists)
- table_dose_stacked.tex (exists) -> table_dose_stacked_nongs.tex (exists)
- table_firstll_stacked_atp.tex (exists) -> table_firstll_stacked_nongs_atp.tex (exists)
- table_firstll_stacked_wta.tex (exists) -> table_firstll_stacked_nongs_wta.tex (exists)

## FIX 6: First-LL non-GS tables (robustness equivalent of verified)

- Non-GS first-LL ATP: N = 1424 (LL: 397)
- Non-GS first-LL WTA: N = 1423 (LL: 378)
- First-LL non-GS ATP points_change @ 4w: coef = 47.72 (16.84), p = 0.005
- First-LL non-GS ATP points_change @ 8w: coef = 45.98 (17.33), p = 0.008
- First-LL non-GS ATP points_change @ 12w: coef = 39.17 (18.00), p = 0.030
- First-LL non-GS ATP points_change @ 26w: coef = 64.55 (21.45), p = 0.003
- First-LL non-GS ATP points_change @ 52w: coef = 54.53 (26.61), p = 0.040
- First-LL non-GS ATP n_main_draws @ 4w: coef = -1.08 (0.30), p = 0.000
- First-LL non-GS ATP n_main_draws @ 8w: coef = -0.60 (0.29), p = 0.037
- First-LL non-GS ATP n_main_draws @ 12w: coef = -0.03 (0.28), p = 0.923
- First-LL non-GS ATP n_main_draws @ 26w: coef = 1.71 (0.34), p = 0.000
- First-LL non-GS ATP n_main_draws @ 52w: coef = 4.64 (0.57), p = 0.000
- First-LL non-GS ATP n_matches_250plus @ 4w: coef = -1.88 (0.59), p = 0.001
- First-LL non-GS ATP n_matches_250plus @ 8w: coef = -1.00 (0.58), p = 0.083
- First-LL non-GS ATP n_matches_250plus @ 12w: coef = 0.02 (0.57), p = 0.972
- First-LL non-GS ATP n_matches_250plus @ 26w: coef = 3.28 (0.70), p = 0.000
- First-LL non-GS ATP n_matches_250plus @ 52w: coef = 8.54 (1.13), p = 0.000
- First-LL non-GS ATP elo_change @ 4w: coef = 6.37 (5.75), p = 0.268
- First-LL non-GS ATP elo_change @ 8w: coef = 8.16 (6.10), p = 0.181
- First-LL non-GS ATP elo_change @ 12w: coef = 4.18 (6.44), p = 0.516
- First-LL non-GS ATP elo_change @ 26w: coef = 9.18 (7.62), p = 0.228
- First-LL non-GS ATP elo_change @ 52w: coef = 3.46 (8.41), p = 0.680
- First-LL non-GS WTA points_change @ 4w: coef = 14.88 (19.51), p = 0.446
- First-LL non-GS WTA points_change @ 8w: coef = 18.30 (19.18), p = 0.340
- First-LL non-GS WTA points_change @ 12w: coef = 19.34 (20.12), p = 0.336
- First-LL non-GS WTA points_change @ 26w: coef = 9.98 (22.86), p = 0.663
- First-LL non-GS WTA points_change @ 52w: coef = 23.32 (34.85), p = 0.504
- First-LL non-GS WTA n_main_draws @ 4w: coef = -1.53 (0.27), p = 0.000
- First-LL non-GS WTA n_main_draws @ 8w: coef = -1.15 (0.25), p = 0.000
- First-LL non-GS WTA n_main_draws @ 12w: coef = -0.78 (0.24), p = 0.001
- First-LL non-GS WTA n_main_draws @ 26w: coef = 0.41 (0.27), p = 0.124
- First-LL non-GS WTA n_main_draws @ 52w: coef = 1.88 (0.54), p = 0.000
- First-LL non-GS WTA n_matches_250plus @ 4w: coef = -2.95 (0.54), p = 0.000
- First-LL non-GS WTA n_matches_250plus @ 8w: coef = -2.18 (0.49), p = 0.000
- First-LL non-GS WTA n_matches_250plus @ 12w: coef = -1.45 (0.47), p = 0.002
- First-LL non-GS WTA n_matches_250plus @ 26w: coef = 0.80 (0.53), p = 0.133
- First-LL non-GS WTA n_matches_250plus @ 52w: coef = 4.05 (1.01), p = 0.000
- First-LL non-GS WTA elo_change @ 4w: coef = 2.29 (5.75), p = 0.690
- First-LL non-GS WTA elo_change @ 8w: coef = 2.60 (5.91), p = 0.660
- First-LL non-GS WTA elo_change @ 12w: coef = 4.74 (6.11), p = 0.438
- First-LL non-GS WTA elo_change @ 26w: coef = -5.70 (7.24), p = 0.431
- First-LL non-GS WTA elo_change @ 52w: coef = -7.98 (8.61), p = 0.354

