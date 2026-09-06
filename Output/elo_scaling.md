# Elo Scaling Documentation
Generated: 2026-03-27 13:33:45.847906

## What was done

All Elo variables used as COVARIATES (not outcomes) were divided by 100:

### Estimation samples (skeleton_*_est_v3.rds)
- `pre_elo` = `pre_elo_raw / 100`
- `pre_elo_sq` = `(pre_elo_raw / 100)^2`

### Tournament match data (in-memory for table re-estimation)
- `pre_elo` = `pre_elo_raw / 100` (focal player Elo)
- `opp_elo` = `opp_elo_raw / 100` (opponent Elo)

## What was NOT scaled

- `elo_change_4w`, `elo_change_8w`, `elo_change_12w`, `elo_change_26w`, `elo_change_52w`
  These are OUTCOME variables measuring Elo change in raw points.
  Raw Elo change in points (e.g., +30 points) is directly interpretable.

- `elo_t0`, `elo_t4`, ... `elo_t52` (level outcomes) are also not scaled.

## Why

Raw Elo values are ~1500-2100. This means:
- Coefficients on `pre_elo` were O(10^-3)
- Coefficients on `pre_elo_sq` were O(10^-6)

After scaling by 100:
- `pre_elo` ranges from ~14-21
- `pre_elo_sq` ranges from ~190-450
- Coefficients are now O(10^-1) and O(10^-3) respectively

## Pre-scaling ranges

- GS pre_elo: [1577.8, 2146.1]
- Non-GS pre_elo: [1364.7, 2242.4]

## Post-scaling ranges

- GS pre_elo: [15.78, 21.46]
- Non-GS pre_elo: [13.65, 22.42]

## Tables updated

- `table_sumstats_gs_atp.tex` (Elo row label: 'Elo rating / 100')
- `table_sumstats_gs_wta.tex`
- `table_sumstats_nongs_atp.tex`
- `table_sumstats_nongs_wta.tex`
- `table_tournament_firstll_gs_atp.tex` (re-estimated with scaled Elo)
- `table_tournament_firstll_gs_wta.tex` (re-estimated with scaled Elo)

## Note on downstream scripts

Any script that loads `skeleton_*_est_v3.rds` gets the scaled Elo.
Scripts using the `ZPRE_FULL` formula string (`pre_elo + pre_elo_sq`) will
automatically pick up the scaled values from the v3 datasets.

The stacked dynamics models (scripts 30, 18) that use `elo_change` as an
outcome should continue to use unscaled Elo change values. The v3 datasets
preserve the original `elo_change_*w` columns.
