# Tournament Performance Model: Results Summary

Generated: 2026-03-23 01:35:49.399777

## Parameters
- Years: 2000-2024
- T_horizon: 10 tournaments
- Calendar cap: 365 days
- Truncation: at_next_event
- Bootstrap replications: 50

## Sample
- Events (qualifying losses): 235804
  - Treated (LL entry): 5422
  - Control: 230382
- Match-level observations: 850161

## Main Results (Pooled CF-IV)
- delta (ll_entry, log-odds): -0.2179
- Analytic SE: 0.0413
- Bootstrap SE: 0.0413
- Odds ratio: 0.8042
- p-value (analytic): 0

## Endogeneity Test
- rho (vhat): 0.103
- rho p-value: 0
- Interpretation: Evidence of endogeneity; CF correction needed

## Tournament-Level Treatment Effects
- Match-level ATE (avg marginal effect on P(win)): NA
- Tournament-level ATE on expected wins (E[dW]): NA
- Tournament-level ATE on P(win tournament): NA

## Interpretation
A delta of -0.218 in log-odds means:
- At P(win)=0.50: shift to ~0.446
- At P(win)=0.30: shift to ~0.256
- These effects compound across tournament rounds

## Robustness
- First-LL only: delta = -0.1497
- Full window: delta = -0.1346
- Naive logit: delta = -0.0149

## Output Files
- Data/cleaned/tournament_model_results.rds
- Data/cleaned/tournament_model_event_table.rds
- Data/cleaned/tournament_model_match_data.rds
- Tables/table_tournament_first_stage.tex
- Tables/table_tournament_match_effects.tex
- Tables/table_tournament_effects.tex
- Tables/table_tournament_heterogeneity.tex
- Figures/fig_dynamic_effects.pdf
- Figures/fig_tournament_effects.pdf
