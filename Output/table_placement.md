# Table Placement: First-LL as Primary Analysis

This document maps each table to its location in the paper.
First-LL (type-specific restriction) tables appear in the MAIN TEXT.
Full-sample tables and additional robustness appear in the APPENDIX.

---

## MAIN TEXT Tables (First-LL, Type-Specific Restriction)

### Mechanisms Section (Tournament Performance)

| Table File | Description | Paper Location |
|------------|-------------|----------------|
| `table_tournament_firstll_gs_atp.tex` | GS-ATP main effects (first-LL) | Mechanisms section, main result |
| `table_tournament_firstll_gs_wta.tex` | GS-WTA main effects (first-LL) | Mechanisms section, main result |
| `table_tournament_dose_gs_atp.tex` | Dose-response, ATP (centered) | Mechanisms section, dose analysis |
| `table_tournament_dose_gs_wta.tex` | Dose-response, WTA (centered) | Mechanisms section, dose analysis |
| `table_tournament_firstll_horizon_gs_atp.tex` | Horizon heterogeneity, ATP | Mechanisms section, persistence |
| `table_tournament_firstll_horizon_gs_wta.tex` | Horizon heterogeneity, WTA | Mechanisms section, persistence |

---

## APPENDIX Tables

### Full-Sample Results (Appendix A: Full Sample)

| Table File | Description |
|------------|-------------|
| `table_tournament_gs_atp.tex` | GS-ATP full sample (all LL events, not just first) |
| `table_tournament_gs_wta.tex` | GS-WTA full sample |

### Robustness (Appendix B: Robustness)

| Table File | Description |
|------------|-------------|
| `table_tournament_firstll_robust_gs_atp.tex` | First-LL robustness, GS-ATP |
| `table_tournament_firstll_robust_gs_wta.tex` | First-LL robustness, GS-WTA |
| `table_tournament_anytype_gs_atp.tex` | Any-type restriction, GS-ATP |
| `table_tournament_anytype_gs_wta.tex` | Any-type restriction, GS-WTA |
| `table_tournament_robustness_gs_atp.tex` | Full-sample robustness, GS-ATP |
| `table_tournament_robustness_gs_wta.tex` | Full-sample robustness, GS-WTA |

### Any-Type Variant (Appendix C: Alternative Restrictions)

| Table File | Description |
|------------|-------------|
| `table_tournament_anytype_horizon_gs_atp.tex` | Any-type horizon, GS-ATP |
| `table_tournament_anytype_horizon_gs_wta.tex` | Any-type horizon, GS-WTA |
| `table_tournament_anytype_robust_gs_atp.tex` | Any-type robustness, GS-ATP |
| `table_tournament_anytype_robust_gs_wta.tex` | Any-type robustness, GS-WTA |

### Non-Grand Slam Results (Appendix D: Non-GS Events)

| Table File | Description |
|------------|-------------|
| `table_tournament_firstll_nongs_atp.tex` | First-LL, nonGS-ATP |
| `table_tournament_firstll_nongs_wta.tex` | First-LL, nonGS-WTA |
| `table_tournament_dose_nongs_atp.tex` | Dose-response, nonGS-ATP (centered) |
| `table_tournament_dose_nongs_wta.tex` | Dose-response, nonGS-WTA (centered) |
| `table_tournament_firstll_horizon_nongs_atp.tex` | Horizon, nonGS-ATP |
| `table_tournament_firstll_horizon_nongs_wta.tex` | Horizon, nonGS-WTA |
| `table_tournament_nongs_atp.tex` | Full-sample, nonGS-ATP |
| `table_tournament_nongs_wta.tex` | Full-sample, nonGS-WTA |
| `table_tournament_firstll_robust_nongs_atp.tex` | Robustness, nonGS-ATP |
| `table_tournament_firstll_robust_nongs_wta.tex` | Robustness, nonGS-WTA |
| `table_tournament_anytype_nongs_atp.tex` | Any-type, nonGS-ATP |
| `table_tournament_anytype_nongs_wta.tex` | Any-type, nonGS-WTA |
| `table_tournament_anytype_horizon_nongs_atp.tex` | Any-type horizon, nonGS-ATP |
| `table_tournament_anytype_horizon_nongs_wta.tex` | Any-type horizon, nonGS-WTA |
| `table_tournament_anytype_robust_nongs_atp.tex` | Any-type robustness, nonGS-ATP |
| `table_tournament_anytype_robust_nongs_wta.tex` | Any-type robustness, nonGS-WTA |
| `table_tournament_robustness_nongs_atp.tex` | Full-sample robustness, nonGS-ATP |
| `table_tournament_robustness_nongs_wta.tex` | Full-sample robustness, nonGS-WTA |
| `table_tournament_horizon_nongs_atp.tex` | Full-sample horizon, nonGS-ATP |
| `table_tournament_horizon_nongs_wta.tex` | Full-sample horizon, nonGS-WTA |

---

## Rationale

The first-LL restriction is PRIMARY because:
1. It isolates the clean treatment effect: players experiencing their first-ever LL opportunity
2. Avoids contamination from prior LL experience (learning, confidence effects)
3. Cleaner comparison group: no treated players appear multiple times with varying doses
4. The full sample (including repeat LL recipients) is provided as robustness in the appendix

Grand Slam results are primary because:
1. GS events use a lottery for LL assignment (true randomization)
2. Non-GS events use ranking-based assignment (requires CF/IV correction)
3. GS results are the cleanest causal estimates; non-GS results validate external validity
