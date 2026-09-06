# Selection Model Summary

Generated: 2026-03-22 08:52:38.313094

## Sample Construction

- All final-round qualifiers (winners + losers): 12968
- Winners (qualified for main draw): 6484
- Losers (potential LL candidates): 6484
- Players with valid win probabilities: 12976
- Selection probabilities computed: 4000

## Selection Probability Distribution

- Mean: 0.0921
- SD: 0.1474

## First Stage

- Coefficient on selection probability (simple): 1.9328
- F-statistic (simple): 2205

## Main Results (2SLS)

- rank_change_4w: coef = -14.96 (SE = 3), p = 0, N = 3994
- rank_change_8w: coef = -17.12 (SE = 3.2), p = 0, N = 3965
- rank_change_12w: coef = -18.31 (SE = 3.5), p = 0, N = 3920
- rank_change_26w: coef = -25.23 (SE = 4.87), p = 0, N = 3840
- rank_change_52w: coef = -45.71 (SE = 8), p = 0, N = 3601
- elo_change_4w: coef = -1.23 (SE = 1.63), p = 0.451, N = 3725
- elo_change_8w: coef = 2.49 (SE = 2.38), p = 0.296, N = 3569
- elo_change_12w: coef = -1.11 (SE = 2.73), p = 0.685, N = 3525
- elo_change_26w: coef = 2.99 (SE = 3.87), p = 0.439, N = 3229
- elo_change_52w: coef = -8.1 (SE = 4.95), p = 0.102, N = 3079

## Control Function Results

- rank_change_4w: coef = -12.34 (SE = 2.76), p = 0, Hausman p = 0.023
- rank_change_8w: coef = -13.8 (SE = 3.01), p = 0, Hausman p = 0.108
- rank_change_12w: coef = -14.62 (SE = 3.31), p = 0, Hausman p = 0.117
- rank_change_26w: coef = -20.96 (SE = 4.74), p = 0, Hausman p = 0.091
- rank_change_52w: coef = -38.79 (SE = 8.01), p = 0, Hausman p = 0.03
- elo_change_4w: coef = -0.92 (SE = 1.62), p = 0.572, Hausman p = 0.737
- elo_change_8w: coef = 3.02 (SE = 2.34), p = 0.197, Hausman p = 0.187
- elo_change_12w: coef = -0.11 (SE = 2.71), p = 0.968, Hausman p = 0.832
- elo_change_26w: coef = 4.42 (SE = 3.8), p = 0.245, Hausman p = 0.463
- elo_change_52w: coef = -6.85 (SE = 4.95), p = 0.166, Hausman p = 0.153

## Mechanism Tests
- Mechanisms estimated: 8

- competitiveness_v2: coef = 0.0662 (SE = 0.0026), p = 0
- elo_change_4w: coef = -1.2304 (SE = 1.6337), p = 0.451
- elo_change_8w: coef = 2.4906 (SE = 2.3809), p = 0.296
- elo_change_12w: coef = -1.1076 (SE = 2.7343), p = 0.685
- elo_change_26w: coef = 2.9935 (SE = 3.8709), p = 0.439
- elo_change_52w: coef = -8.098 (SE = 4.9495), p = 0.102
- dose_rank_change_26w: coef = -8.1444 (SE = 3.3455), p = 0.015
- dose_rank_change_52w: coef = -14.5456 (SE = 6.5382), p = 0.026

## WTA Results
- WTA estimation sample: 3161
- WTA LL recipients: 839

- WTA rank_change_12w: coef = -11.79 (SE = 3.07), p = 0
- WTA rank_change_26w: coef = -13.06 (SE = 4.29), p = 0.002
- WTA rank_change_52w: coef = -14.68 (SE = 8.18), p = 0.073

## Output Files

### Data
- `Data/cleaned/qualifying_match_probabilities.rds`
- `Data/cleaned/selection_probabilities.rds`
- `Data/cleaned/selection_model_results.rds`
- `Data/cleaned/selection_model_mechanisms.rds`
- `Data/cleaned/wta_selection_model_results.rds`

### Tables
- `Tables/table_first_stage_selection.tex`
- `Tables/table_selection_model_results.tex`

### Figures
- `Figures/fig_selection_probability.pdf`
- `Figures/fig_three_designs_comparison.pdf`
