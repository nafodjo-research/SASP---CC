## Data loaded
- all_losers_est: N = 6658
- gs_est_old (GS): N = 380
- nongs_est_old (non-GS): N = 5229

## FIX 1: Expanded Z^pre
- Added n_prior_gs_ll_notwon, n_prior_nongs_ll_notwon
- Elo imputed for ~0 observations (set to median = 1782)
- Full Z^pre: pre_rank_pts + pre_rank_pts_sq + pre_elo + pre_elo_sq + n_prior_gs_ll_won + n_prior_gs_ll_notwon + n_prior_nongs_ll_won + n_prior_nongs_ll_notwon + player_age

## FIX 2: Censoring rule fixed
- Old censoring (any next qual loss) at 52w: 3103 observations censored
- New censoring (same-type LL event) at 52w: 2341 observations censored
- Difference: 762 fewer censored

## Updated estimation samples (after FIX 1 + FIX 2)
- GS ATP: N = 248 (LL: 132, Control: 116)
- GS WTA: N = 132 (LL: 54, Control: 78)
- Non-GS ATP: N = 2708 (LL: 874)
- Non-GS WTA: N = 2545 (LL: 755)

## FIX 3: Control function approach
- v_hat = D * phi(Phi^{-1}(P))/P - (1-D) * phi(Phi^{-1}(P))/(1-P)
- Non-GS ATP with v_hat: N = 2708
- Non-GS WTA with v_hat: N = 2545

## FIX 6a: GS stacked dynamics (expanded Z^pre)
- ATP GS points_change @ 4w: coef = 33.74, SE = 14.47, p = 0.020
- ATP GS points_change @ 8w: coef = 34.36, SE = 14.24, p = 0.016
- ATP GS points_change @ 12w: coef = 41.78, SE = 14.69, p = 0.004
- ATP GS points_change @ 26w: coef = 54.60, SE = 26.21, p = 0.037
- ATP GS points_change @ 52w: coef = 8.23, SE = 42.85, p = 0.848
- ATP GS n_main_draws @ 4w: coef = 0.23, SE = 0.28, p = 0.414
- ATP GS n_main_draws @ 8w: coef = 0.52, SE = 0.25, p = 0.038
- ATP GS n_main_draws @ 12w: coef = 0.68, SE = 0.25, p = 0.007
- ATP GS n_main_draws @ 26w: coef = 1.21, SE = 0.35, p = 0.001
- ATP GS n_main_draws @ 52w: coef = 1.39, SE = 0.92, p = 0.131
- ATP GS n_matches_250plus @ 4w: coef = 0.23, SE = 0.55, p = 0.677
- ATP GS n_matches_250plus @ 8w: coef = 0.71, SE = 0.47, p = 0.130
- ATP GS n_matches_250plus @ 12w: coef = 0.94, SE = 0.47, p = 0.046
- ATP GS n_matches_250plus @ 26w: coef = 1.94, SE = 0.68, p = 0.004
- ATP GS n_matches_250plus @ 52w: coef = 2.30, SE = 1.77, p = 0.195
- ATP GS elo_change @ 4w: coef = -2.01, SE = 5.15, p = 0.696
- ATP GS elo_change @ 8w: coef = -0.12, SE = 6.00, p = 0.984
- ATP GS elo_change @ 12w: coef = -4.11, SE = 6.22, p = 0.508
- ATP GS elo_change @ 26w: coef = 3.23, SE = 8.93, p = 0.717
- ATP GS elo_change @ 52w: coef = -14.42, SE = 12.89, p = 0.263
- WTA GS points_change @ 4w: coef = 66.59, SE = 21.38, p = 0.002
- WTA GS points_change @ 8w: coef = 80.43, SE = 22.23, p = 0.000
- WTA GS points_change @ 12w: coef = 33.86, SE = 21.97, p = 0.123
- WTA GS points_change @ 26w: coef = 6.49, SE = 36.31, p = 0.858
- WTA GS points_change @ 52w: coef = 10.00, SE = 67.17, p = 0.882
- WTA GS n_main_draws @ 4w: coef = 0.07, SE = 0.31, p = 0.819
- WTA GS n_main_draws @ 8w: coef = 0.50, SE = 0.32, p = 0.121
- WTA GS n_main_draws @ 12w: coef = 0.51, SE = 0.34, p = 0.128
- WTA GS n_main_draws @ 26w: coef = 0.81, SE = 0.46, p = 0.081
- WTA GS n_main_draws @ 52w: coef = 1.17, SE = 1.08, p = 0.276
- WTA GS n_matches_250plus @ 4w: coef = -0.11, SE = 0.53, p = 0.837
- WTA GS n_matches_250plus @ 8w: coef = 0.94, SE = 0.47, p = 0.047
- WTA GS n_matches_250plus @ 12w: coef = 0.93, SE = 0.49, p = 0.058
- WTA GS n_matches_250plus @ 26w: coef = 1.67, SE = 0.79, p = 0.033
- WTA GS n_matches_250plus @ 52w: coef = 1.71, SE = 1.86, p = 0.358
- WTA GS elo_change @ 4w: coef = 10.17, SE = 5.56, p = 0.067
- WTA GS elo_change @ 8w: coef = 7.08, SE = 6.53, p = 0.278
- WTA GS elo_change @ 12w: coef = 6.60, SE = 7.90, p = 0.404
- WTA GS elo_change @ 26w: coef = 16.67, SE = 9.61, p = 0.083
- WTA GS elo_change @ 52w: coef = 13.49, SE = 16.25, p = 0.406

## FIX 6b: Non-GS stacked dynamics with CF
- ATP non-GS CF points_change @ 4w: coef = 16.82, SE = 6.84, p = 0.014
- ATP non-GS CF points_change @ 8w: coef = 14.60, SE = 7.01, p = 0.037
- ATP non-GS CF points_change @ 12w: coef = 9.27, SE = 7.95, p = 0.243
- ATP non-GS CF points_change @ 26w: coef = 29.49, SE = 10.39, p = 0.005
- ATP non-GS CF points_change @ 52w: coef = 1.46, SE = 18.18, p = 0.936
- ATP non-GS CF n_main_draws @ 4w: coef = -1.06, SE = 0.13, p = 0.000
- ATP non-GS CF n_main_draws @ 8w: coef = -0.64, SE = 0.13, p = 0.000
- ATP non-GS CF n_main_draws @ 12w: coef = -0.10, SE = 0.12, p = 0.400
- ATP non-GS CF n_main_draws @ 26w: coef = 1.52, SE = 0.18, p = 0.000
- ATP non-GS CF n_main_draws @ 52w: coef = 4.16, SE = 0.44, p = 0.000
- ATP non-GS CF n_matches_250plus @ 4w: coef = -2.00, SE = 0.26, p = 0.000
- ATP non-GS CF n_matches_250plus @ 8w: coef = -1.23, SE = 0.24, p = 0.000
- ATP non-GS CF n_matches_250plus @ 12w: coef = -0.34, SE = 0.24, p = 0.154
- ATP non-GS CF n_matches_250plus @ 26w: coef = 2.70, SE = 0.36, p = 0.000
- ATP non-GS CF n_matches_250plus @ 52w: coef = 7.33, SE = 0.82, p = 0.000
- ATP non-GS CF elo_change @ 4w: coef = 0.01, SE = 2.46, p = 0.998
- ATP non-GS CF elo_change @ 8w: coef = -0.35, SE = 2.73, p = 0.898
- ATP non-GS CF elo_change @ 12w: coef = -1.45, SE = 2.94, p = 0.622
- ATP non-GS CF elo_change @ 26w: coef = 1.68, SE = 4.17, p = 0.686
- ATP non-GS CF elo_change @ 52w: coef = -4.34, SE = 5.72, p = 0.448
- WTA non-GS CF points_change @ 4w: coef = 18.38, SE = 10.17, p = 0.071
- WTA non-GS CF points_change @ 8w: coef = 13.73, SE = 10.86, p = 0.206
- WTA non-GS CF points_change @ 12w: coef = 15.80, SE = 12.08, p = 0.191
- WTA non-GS CF points_change @ 26w: coef = -2.93, SE = 15.27, p = 0.848
- WTA non-GS CF points_change @ 52w: coef = 2.04, SE = 29.76, p = 0.945
- WTA non-GS CF n_main_draws @ 4w: coef = -0.74, SE = 0.13, p = 0.000
- WTA non-GS CF n_main_draws @ 8w: coef = -0.36, SE = 0.12, p = 0.002
- WTA non-GS CF n_main_draws @ 12w: coef = -0.01, SE = 0.12, p = 0.948
- WTA non-GS CF n_main_draws @ 26w: coef = 1.07, SE = 0.17, p = 0.000
- WTA non-GS CF n_main_draws @ 52w: coef = 2.63, SE = 0.41, p = 0.000
- WTA non-GS CF n_matches_250plus @ 4w: coef = -1.46, SE = 0.26, p = 0.000
- WTA non-GS CF n_matches_250plus @ 8w: coef = -0.86, SE = 0.24, p = 0.000
- WTA non-GS CF n_matches_250plus @ 12w: coef = -0.25, SE = 0.24, p = 0.285
- WTA non-GS CF n_matches_250plus @ 26w: coef = 1.66, SE = 0.35, p = 0.000
- WTA non-GS CF n_matches_250plus @ 52w: coef = 4.77, SE = 0.80, p = 0.000
- WTA non-GS CF elo_change @ 4w: coef = 4.37, SE = 2.47, p = 0.077
- WTA non-GS CF elo_change @ 8w: coef = 3.66, SE = 2.68, p = 0.172
- WTA non-GS CF elo_change @ 12w: coef = 7.42, SE = 2.94, p = 0.012
- WTA non-GS CF elo_change @ 26w: coef = 0.98, SE = 4.21, p = 0.817
- WTA non-GS CF elo_change @ 52w: coef = -0.64, SE = 5.79, p = 0.912

## Endogeneity test (rho on v_hat):
- ATP non-GS rho points_change @ 4w: rho = 3.417, p = 0.173 -- not significant
- ATP non-GS rho points_change @ 8w: rho = 4.004, p = 0.144 -- not significant
- ATP non-GS rho points_change @ 12w: rho = 2.483, p = 0.422 -- not significant
- ATP non-GS rho points_change @ 26w: rho = -4.700, p = 0.272 -- not significant
- ATP non-GS rho points_change @ 52w: rho = 5.876, p = 0.458 -- not significant
- ATP non-GS rho n_main_draws @ 4w: rho = 0.237, p = 0.000 -- SIGNIFICANT (endogeneity detected)
- ATP non-GS rho n_main_draws @ 8w: rho = 0.156, p = 0.004 -- SIGNIFICANT (endogeneity detected)
- ATP non-GS rho n_main_draws @ 12w: rho = 0.023, p = 0.687 -- not significant
- ATP non-GS rho n_main_draws @ 26w: rho = -0.334, p = 0.000 -- SIGNIFICANT (endogeneity detected)
- ATP non-GS rho n_main_draws @ 52w: rho = -0.805, p = 0.000 -- SIGNIFICANT (endogeneity detected)
- ATP non-GS rho n_matches_250plus @ 4w: rho = 0.467, p = 0.000 -- SIGNIFICANT (endogeneity detected)
- ATP non-GS rho n_matches_250plus @ 8w: rho = 0.316, p = 0.003 -- SIGNIFICANT (endogeneity detected)
- ATP non-GS rho n_matches_250plus @ 12w: rho = 0.091, p = 0.381 -- not significant
- ATP non-GS rho n_matches_250plus @ 26w: rho = -0.588, p = 0.000 -- SIGNIFICANT (endogeneity detected)
- ATP non-GS rho n_matches_250plus @ 52w: rho = -1.486, p = 0.000 -- SIGNIFICANT (endogeneity detected)
- ATP non-GS rho elo_change @ 4w: rho = 0.928, p = 0.339 -- not significant
- ATP non-GS rho elo_change @ 8w: rho = 0.492, p = 0.671 -- not significant
- ATP non-GS rho elo_change @ 12w: rho = 0.846, p = 0.519 -- not significant
- ATP non-GS rho elo_change @ 26w: rho = -2.130, p = 0.359 -- not significant
- ATP non-GS rho elo_change @ 52w: rho = -0.676, p = 0.827 -- not significant
- WTA non-GS rho points_change @ 4w: rho = 4.470, p = 0.230 -- not significant
- WTA non-GS rho points_change @ 8w: rho = 6.319, p = 0.116 -- not significant
- WTA non-GS rho points_change @ 12w: rho = 3.120, p = 0.465 -- not significant
- WTA non-GS rho points_change @ 26w: rho = -2.668, p = 0.696 -- not significant
- WTA non-GS rho points_change @ 52w: rho = -7.930, p = 0.522 -- not significant
- WTA non-GS rho n_main_draws @ 4w: rho = 0.161, p = 0.001 -- SIGNIFICANT (endogeneity detected)
- WTA non-GS rho n_main_draws @ 8w: rho = 0.091, p = 0.027 -- SIGNIFICANT (endogeneity detected)
- WTA non-GS rho n_main_draws @ 12w: rho = -0.001, p = 0.973 -- not significant
- WTA non-GS rho n_main_draws @ 26w: rho = -0.300, p = 0.000 -- SIGNIFICANT (endogeneity detected)
- WTA non-GS rho n_main_draws @ 52w: rho = -0.669, p = 0.001 -- SIGNIFICANT (endogeneity detected)
- WTA non-GS rho n_matches_250plus @ 4w: rho = 0.382, p = 0.000 -- SIGNIFICANT (endogeneity detected)
- WTA non-GS rho n_matches_250plus @ 8w: rho = 0.259, p = 0.002 -- SIGNIFICANT (endogeneity detected)
- WTA non-GS rho n_matches_250plus @ 12w: rho = 0.081, p = 0.320 -- not significant
- WTA non-GS rho n_matches_250plus @ 26w: rho = -0.507, p = 0.000 -- SIGNIFICANT (endogeneity detected)
- WTA non-GS rho n_matches_250plus @ 52w: rho = -1.196, p = 0.002 -- SIGNIFICANT (endogeneity detected)
- WTA non-GS rho elo_change @ 4w: rho = -0.398, p = 0.660 -- not significant
- WTA non-GS rho elo_change @ 8w: rho = 0.560, p = 0.566 -- not significant
- WTA non-GS rho elo_change @ 12w: rho = -0.759, p = 0.518 -- not significant
- WTA non-GS rho elo_change @ 26w: rho = -0.653, p = 0.690 -- not significant
- WTA non-GS rho elo_change @ 52w: rho = 1.418, p = 0.564 -- not significant

## FIX 6e: Robustness (verified lottery)
- Verified ATP: N = 100 (LL: 38)
- Verified WTA: N = 44 (LL: 14)
- Verified ATP pts @ 4w: coef = 0.68 (21.81), p = 0.975
- Verified ATP pts @ 8w: coef = -4.56 (21.54), p = 0.832
- Verified ATP pts @ 12w: coef = 9.52 (21.48), p = 0.658
- Verified ATP pts @ 26w: coef = 43.62 (46.92), p = 0.352
- Verified ATP pts @ 52w: coef = -11.35 (67.57), p = 0.867
- Verified WTA pts @ 4w: coef = 76.57 (42.02), p = 0.068
- Verified WTA pts @ 8w: coef = 91.48 (32.04), p = 0.004
- Verified WTA pts @ 12w: coef = 76.71 (37.48), p = 0.041
- Verified WTA pts @ 26w: coef = 164.29 (49.31), p = 0.001
- Verified WTA pts @ 52w: coef = 326.77 (119.83), p = 0.006

## FIX 6e: Robustness (first-LL)
- First-LL ATP: N = 193 (LL: 103)
- First-LL WTA: N = 118 (LL: 46)

## FIX 4: Summary stats tables (4 separate)
- table_sumstats_gs_atp.tex
- table_sumstats_gs_wta.tex
- table_sumstats_nongs_atp.tex
- table_sumstats_nongs_wta.tex

