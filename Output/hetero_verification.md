# Heterogeneity Specification Verification
Generated: 2026-03-27 11:57:49.425477

## Verification Result: CORRECT -- Each dimension estimated SEPARATELY

The heterogeneity analysis in scripts/R/30_zpre_cf_fixes.R (lines 661-708)
runs a SEPARATE regression for each heterogeneity dimension:

1. **Ranking (rank_above_med):**
   `outcome ~ got_ll:horizon + got_ll:horizon:rank_above_med + Z^pre | slam_year + horizon`

2. **Age (age_above_med):**
   `outcome ~ got_ll:horizon + got_ll:horizon:age_above_med + Z^pre | slam_year + horizon`

3. **Prior LL (prior_ll_dum):**
   `outcome ~ got_ll:horizon + got_ll:horizon:prior_ll_dum + Z^pre | slam_year + horizon`

The for loop in `run_hetero_horizon()` iterates over `dim_info` list elements,
creating a new `fml` and calling `feols()` separately for each dimension.
This is the correct approach -- NOT all interactions simultaneously.

For non-GS with CF (lines 722-773), the same pattern holds with added
v_hat:horizon and v_hat:horizon:cat terms.

## No changes needed.
