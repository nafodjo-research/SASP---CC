# Robustness Summary -- 06b_robustness.R
Generated: 2026-03-21 16:18:44.753987

## (a) Bandwidth Sensitivity

- rank_change_12w (50% bw): coef = -8.2, p = 0
- rank_change_12w (75% bw): coef = -9.2, p = 0.743
- rank_change_12w (100% bw): coef = -10.8, p = 0.674
- rank_change_12w (150% bw): coef = -4.7, p = 0.156
- rank_change_12w (200% bw): coef = -2.1, p = 0.235
- rank_change_26w (100% bw): coef = -19.1, p = 0.558
- rank_change_26w (150% bw): coef = -14.3, p = 0.143
- rank_change_26w (200% bw): coef = -5.9, p = 0.062
- elo_change_26w (100% bw): coef = 10.4, p = 0.949
- elo_change_26w (150% bw): coef = 11.1, p = 0.652
- elo_change_26w (200% bw): coef = 9.5, p = 0.495

## (b) Polynomial Order

- rank_change_12w (p=1): coef = -10.8, p = 0.161
- rank_change_12w (p=2): coef = -18.4, p = 0.223
- rank_change_26w (p=1): coef = -19.1, p = 0.053
- rank_change_26w (p=2): coef = -32, p = 0.24
- elo_change_26w (p=1): coef = 10.4, p = 0.585
- elo_change_26w (p=2): coef = 7.6, p = 0.98

## (c) Density Test

- Test: manual_chi_squared, p = 0.669

## (d) Placebo Cutoffs (Sharp Reduced-Form)

- Cutoff at R_tilde=-2: coef = -3.2, p = 0.294 (not significant, as expected)
- Cutoff at R_tilde=-1: coef = 2.8, p = 0.867 (not significant, as expected)
- Cutoff at R_tilde=2: coef = -29.7, p = 0.002 *** SIGNIFICANT (concern!) ***
- Cutoff at R_tilde=3: coef = 35.2, p = 0.001 *** SIGNIFICANT (concern!) ***

## (e) Donut-Hole RDD

- rank_change_12w: coef = -8.9, p = 0.615
- rank_change_26w: coef = -12.6, p = 0.311
- elo_change_26w: coef = 28.1, p = 0.654

## (f) Tournament-Year Clustering

- rank_change_12w: coef = -10.8, SE = 11.5, p = 0.175
- rank_change_26w: coef = -18.5, SE = 17.3, p = 0.063
- elo_change_26w: coef = 10.4, SE = 16.3, p = 0.569

## (g) Single LL-Slot Tournaments

- No results (insufficient observations or estimation failed)

## (j) Pre-Treatment Ranking Change Placebo

- rank_change_pre12w (12 weeks before event): coef = -10.5, SE = 16.4, p = 0.304 -- PASS (no pre-trend)

## (h) RDHonest -- Honest CIs for Discrete Running Variable

- rank_change_12w: RDHonest infeasible with this discrete running variable. The running variable has very few mass points per tournament (4-16), which causes numerical instability in the honest CI computation. This is a known limitation; the local randomization framework (rdrandinf, reported in main results) is the appropriate alternative for discrete running variables (Cattaneo et al. 2020, 2024).
- rank_change_26w: RDHonest infeasible with this discrete running variable. The running variable has very few mass points per tournament (4-16), which causes numerical instability in the honest CI computation. This is a known limitation; the local randomization framework (rdrandinf, reported in main results) is the appropriate alternative for discrete running variables (Cattaneo et al. 2020, 2024).
- elo_change_26w: RDHonest infeasible with this discrete running variable. The running variable has very few mass points per tournament (4-16), which causes numerical instability in the honest CI computation. This is a known limitation; the local randomization framework (rdrandinf, reported in main results) is the appropriate alternative for discrete running variables (Cattaneo et al. 2020, 2024).

## (i) Holm-Bonferroni FWER Correction (approximates Romano-Wolf)

- rank_change_4w: raw p = 0.676, Holm p = 1
- rank_change_8w: raw p = 0.429, Holm p = 1
- rank_change_12w: raw p = 0.148, Holm p = 1
- rank_change_26w: raw p = 0.035, Holm p = 0.354
- rank_change_52w: raw p = 0.304, Holm p = 1
- rank_change_4w: raw p = 0.733, Holm p = 1
- rank_change_8w: raw p = 0.454, Holm p = 1
- rank_change_12w: raw p = 0.161, Holm p = 1
- rank_change_26w: raw p = 0.053, Holm p = 0.473
- rank_change_52w: raw p = 0.492, Holm p = 1

## Output Files

- Tables/table5_robustness.tex
- Figures/fig4_bw_sensitivity_*.pdf
- Data/cleaned/robustness_*.rds (7 result files)

