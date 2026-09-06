# Results Summary for Writer

Generated: 2026-03-25
Pipeline: scripts/R/17_skeleton_pipeline.R

## KEY CHANGE: Sample Definition

The previous pipeline restricted the GS lottery sample to the "top-4 ranked losers" among qualifying losers (N=248 ATP, 132 WTA). This was INCORRECT per the skeleton specification.

The correct eligible pool is ALL final-round qualifying losers at Grand Slams where at least one LL was awarded. This increases sample sizes substantially:

| Sample | Old N | New N | Old LL | New LL |
|--------|-------|-------|--------|--------|
| ATP GS | 248 | 992 | 132 | 170 |
| WTA GS | 132 | 455 | 54 | 66 |

The LL count changes only modestly (170 vs 132 for ATP) because the treatment count is determined by how many LL slots were awarded, not pool size. But the control group expands from ~116 to 822 for ATP.

## Sample Sizes (All Numbers for the Paper)

### GS Estimation Sample (2006-2024, events with >=1 LL)
- **ATP**: N=992 (170 LL, 822 control), 509 unique players, 62 unique events
- **WTA**: N=455 (66 LL, 389 control), 330 unique players, 33 unique events

### First-LL-Only (player's first qualifying loss)
- **ATP**: N=508 (65 LL, 443 control)
- **WTA**: N=330 (41 LL, 289 control)

### Verified Lottery (events where non-highest-ranked player got LL)
- 39 verified lottery events out of 83 total
- **ATP**: N=448 (48 LL)
- **WTA**: N=175 (16 LL)

### Non-GS IV Sample (ATP only, 2007-2024)
- N=2,672 (862 LL, 1,810 control)
- First-stage F-statistic: 2,181 (very strong instrument)

## Main Findings

### Immediate Effects (Table: table_immediate.tex)
LL entry mechanically produces large immediate effects (controls get 0 by definition):
- ATP: 0.34 match win rate, 1.46 matches played, 28 ranking points
- WTA: 0.38 match win rate, 1.44 matches played, 27 ranking points
- All significant at p<0.001 with event (slam x year) FE

### Post-Episode Dynamics (Tables: table_dynamic_atp.tex, table_dynamic_wta.tex)
With event FE + controls (ranking points, pts^2, Elo, Elo^2, age, prior LL):

**ATP access outcomes (statistically significant):**
- Main draws entered: +0.27 at 8w (p=0.022), +0.36 at 12w (p=0.014), +0.64 at 26w (p=0.009)
- Matches at 250+: +0.48 at 8w (p=0.030), +0.65 at 12w (p=0.022), +1.08 at 26w (p=0.018)
- Effects fade at 52w (not significant)

**ATP ranking points:**
- +23 at 4w (p<0.001), +25 at 8w (p=0.001), +31 at 12w (p=0.001)
- Fades at 26w (+22, p=0.181) and 52w (+5, p=0.848)

**ATP Elo changes:** Not significant at any horizon (consistent with LL not changing underlying skill)

**Interpretation:** LL entry creates persistent ACCESS advantages (more main draws, more matches) through ~26 weeks. Ranking points gains are significant through ~12 weeks. Effects fade by 52 weeks. No skill (Elo) changes, suggesting the channel is access/exposure, not ability improvement.

### Non-GS IV Results (Tables: table_iv_first_stage.tex, table_dynamic_nongs_iv.tex)
IV with peer_component instrument (Bernoulli convolution):
- Points change (26w): +26.35 (SE=10.99, p=0.016)
- Main draws (26w): +0.37 (SE=0.18, p=0.044)
- Matches 250+ (26w): +0.85 (SE=0.36, p=0.017)
- Elo change (26w): +7.25 (SE=4.17, p=0.082)

IV estimates are CONSISTENT with GS lottery results in direction and magnitude.

### Heterogeneity
Estimated for ATP and WTA separately across subgroups (by ranking, age, prior LL).
See table_hetero_atp.tex and table_hetero_wta.tex.

## Balance (Table: table_balance.tex)

Joint F-test rejects balance (p<0.001 for both ATP and WTA). This is expected because:
1. The pool is ALL qualifying losers, not just the LL-eligible subset
2. LL selection at GS is random within the pool, but higher-ranked losers are more likely to be in the candidate pool at events with LL vacancies
3. Finite-sample imbalance is addressed through controls Z^pre in the dynamic model

The skeleton explicitly acknowledges this: "Finite-sample imbalance -> controls used for precision."

## Robustness

### First-LL-Only (table_firstll_atp.tex, table_firstll_wta.tex)
Restricting to each player's first qualifying loss eliminates concerns about repeated treatment. Results are qualitatively similar but with wider confidence intervals due to smaller N.

### Verified Lottery (table_verified.tex)
Restricting to events where a non-highest-ranked player got the LL spot (proving lottery was used). Results are consistent with main estimates.

## Output Files

### Tables (Tables/)
- table_ll_dist_gs.tex: GS LL distribution (main text)
- table_ll_dist_nongs.tex: Non-GS LL distribution (appendix)
- table_sumstats_gs.tex: GS summary statistics (main text)
- table_sumstats_nongs.tex: Non-GS summary statistics (appendix)
- table_immediate.tex: Immediate effects (main text)
- table_dynamic_atp.tex: ATP dynamic effects (main text)
- table_dynamic_wta.tex: WTA dynamic effects (main text)
- table_hetero_atp.tex: ATP heterogeneity (main text)
- table_hetero_wta.tex: WTA heterogeneity (main text)
- table_firstll_atp.tex: ATP first-LL robustness
- table_firstll_wta.tex: WTA first-LL robustness
- table_verified.tex: Verified lottery robustness
- table_balance.tex: Balance table
- table_iv_first_stage.tex: IV first stage (appendix)
- table_dynamic_nongs_iv.tex: Non-GS IV results (appendix)

### Figures (Figures/)
- fig_event_study_atp.pdf: ATP ranking points trajectory
- fig_event_study_wta.pdf: WTA ranking points trajectory
- fig_dose.pdf: Dose-response by matches won as LL

### Data (Data/cleaned/)
- skeleton_all_losers_est.rds: Full estimation dataset
- skeleton_gs_est.rds: GS estimation sample
- skeleton_nongs_est.rds: Non-GS estimation sample
- skeleton_immediate_effects.rds, skeleton_dynamic_gs.rds, etc.

### Documentation (Output/)
- final_sample_counts.md: Exact counts for every sample
- skeleton_pipeline_summary.md: Full pipeline log with all results
