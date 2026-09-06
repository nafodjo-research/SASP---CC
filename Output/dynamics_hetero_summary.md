# Dynamics & Heterogeneity Re-estimation Summary
Generated: 2026-03-28 14:03:01.633428

## Models estimated

### Stacked Dynamic (4 samples x 4 outcomes = 16 models)
- GS-ATP, GS-WTA, NonGS-ATP, NonGS-WTA
- Outcomes: points_change, elo_change, n_main_draws, n_matches_250plus
- Horizons: 4w, 8w, 12w, 26w, 52w

### Heterogeneity (4 samples x 4 outcomes x 3 dimensions = 48 models)
- Dimensions: rank_above_med, age_above_med, had_prior_ll

### Dose (4 samples x 4 outcomes x 2 dose types = 32 models)
- Dose types: matches_won, performance probability (-log pi)

### First-LL Robustness (4 samples x 4 outcomes = 16 models)

## Z^{pre} specification
- GS: pre_rank_pts_s, pre_rank_pts_sq_s, pre_elo_s, pre_elo_sq_s,
  pre_surf_elo_s, pre_surf_elo_sq_s, n_prior_gs_ll_won, n_prior_gs_ll_notwon,
  n_prior_nongs_ll_won, n_prior_nongs_ll_notwon, player_age
  ALL interacted with horizon
- NonGS: Same variables as main effects (not interacted)

## Scaling
- pre_rank_pts: /1000
- pre_elo: /100 (GS already on this scale; NonGS divided)
- pre_surf_elo: /100

## Key log messages
GS skeleton: 380 rows, 82 cols
NonGS skeleton: 5253 rows, 85 cols
  GS surface Elo NAs before fallback: 0 / 380
  NonGS surface Elo NAs before fallback: 0 / 5253
  Saved skeleton_gs_est_v4.rds and skeleton_nongs_est_v7.rds
  GS pre_elo_s range: [15.78, 21.46]
  GS pre_surf_elo_s range: [13.77, 20.80]
  NonGS pre_elo_s range: [13.65, 22.42]
  NonGS pre_surf_elo_s range: [13.01, 22.28]
  GS-ATP: 248 obs
  GS-WTA: 132 obs
  NonGS-ATP: 2708 obs
  NonGS-WTA: 2545 obs
  Stacked GS-ATP: 1240 rows
  Stacked GS-WTA: 660 rows
  Stacked NonGS-ATP: 13540 rows
  Stacked NonGS-WTA: 12725 rows
  Saved table_dynamic_stacked_atp.tex
  Saved table_dynamic_stacked_wta.tex
  Saved table_dynamic_stacked_nongs_atp.tex
  Saved table_dynamic_stacked_nongs_wta.tex
  Saved 4 full dynamic tables (appendix)
  Dose data: 5537 rows
  Dose merged -- GS-ATP: 64 treated with dose > 0
  Dose merged -- NonGS-ATP: 1114 treated with dose > 0
  Saved table_hetero_stacked_atp.tex
  Saved table_hetero_stacked_wta.tex
  Saved table_hetero_stacked_nongs_atp.tex
  Saved table_hetero_stacked_nongs_wta.tex
  Saved table_dose_stacked.tex (GS-ATP)
  Saved table_perf_prob_stacked.tex (GS-WTA)
  Saved table_dose_stacked_nongs.tex (NonGS-ATP)
  Saved table_perf_prob_stacked_nongs.tex (NonGS-WTA)
  First-LL GS-ATP: 46 / 248
  First-LL GS-WTA: 42 / 132
  First-LL NonGS-ATP: 672 / 2708
  First-LL NonGS-WTA: 635 / 2545
  Saved table_firstll_stacked_atp.tex
  Saved table_firstll_stacked_wta.tex
  Saved table_firstll_stacked_nongs_atp.tex
  Saved table_firstll_stacked_nongs_wta.tex
