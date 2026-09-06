# 41_fixes.R Summary
Generated: 2026-03-28 21:58:29.713776

## Fixes applied

### FIX 1: Cross-tab tables (LL eligibility x wins)
- table_ll_careers_gs.tex
- table_ll_careers_nongs.tex

### FIX 2: Add winning percentage outcome
- Computed win_pct_4w/8w/12w/26w/52w from raw match data
- Updated skeletons: v5 (GS), v8 (NonGS)
- Regenerated dynamic stacked tables with 5th outcome row

### FIX 3: Change dose from -log(pi) to -logit(pi)
- dose = log((1-pi)/pi) instead of -log(pi)
- Regenerated all dose tables

### FIX 4: Reformat heterogeneity tables
- New format: side-by-side base + interaction under each horizon
- 2 columns per horizon (10 total) instead of 1 column per horizon

## Key log messages
GS skeleton: 380 rows (248 ATP, 132 WTA)
NonGS skeleton: 5253 rows (2708 ATP, 2545 WTA)
  GS cross-tab -- ATP players: 174, WTA players: 112
  Saved table_ll_careers_gs.tex
  NonGS cross-tab -- ATP players: 862, WTA players: 752
  Saved table_ll_careers_nongs.tex
  ATP matches: 335153, WTA matches: 690137
  GS  win_pct_4w: 349 / 380 non-NA
  NonGS win_pct_4w: 4729 / 5253 non-NA
  GS  win_pct_8w: 372 / 380 non-NA
  NonGS win_pct_8w: 4939 / 5253 non-NA
  GS  win_pct_12w: 374 / 380 non-NA
  NonGS win_pct_12w: 5051 / 5253 non-NA
  GS  win_pct_26w: 377 / 380 non-NA
  NonGS win_pct_26w: 5141 / 5253 non-NA
  GS  win_pct_52w: 380 / 380 non-NA
  NonGS win_pct_52w: 5186 / 5253 non-NA
  Saved skeleton_gs_est_v5.rds and skeleton_nongs_est_v8.rds
  Dose data: 5537 rows
  Old dose range: [0.000, 6.461]
  New dose range: [-5.948, 6.460]
  Saved updated performance_dose.rds
  Stacked GS-ATP: 1240 rows
  Stacked NonGS-ATP: 13540 rows
  Saved table_dynamic_stacked_atp.tex
  Saved table_dynamic_stacked_wta.tex
  Saved table_dynamic_stacked_nongs_atp.tex
  Saved table_dynamic_stacked_nongs_wta.tex
  Saved table_dose_stacked.tex (GS-ATP)
  Saved table_perf_prob_stacked.tex (GS-WTA)
  Saved table_dose_stacked_nongs.tex (NonGS-ATP)
  Saved table_perf_prob_stacked_nongs.tex (NonGS-WTA)
  Saved table_hetero_stacked_atp.tex (new format)
  Saved table_hetero_stacked_wta.tex (new format)
  Saved table_hetero_stacked_nongs_atp.tex (new format)
  Saved table_hetero_stacked_nongs_wta.tex (new format)
