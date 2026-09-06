# WTA Non-GS Fix Summary

Generated: 2026-03-25

## Issues Fixed

### ISSUE 1: WTA Non-GS sample was empty (N=0)

**Root cause:** `build_qualifiers()` filtered on `tourney_level %in% c("G", "M", "A")`, which matched ATP levels but not WTA levels (PM, P, I).

**Fix:** Made `build_qualifiers()` tour-aware:
- ATP non-GS: `tourney_level %in% c("G", "M", "A")`, years >= 2007
- WTA non-GS: `tourney_level %in% c("G", "PM", "P", "I")`, years >= 2009 (WTA tier system change)

Also fixed `build_future_data()` which filtered `match_app` on `c("G", "M", "A")` -- now includes `c("G", "M", "A", "PM", "P", "I")` so WTA 250+ matches count toward dynamic outcomes.

**Result:** WTA non-GS sample now has N=2,545 (LL=755, Control=1,790) across PM/P/I tiers.

### ISSUE 2: Non-GS frequency table missing WTA data

**Root cause:** `level_label` mapping only covered `M` and `A` (ATP). WTA levels mapped to "Other".

**Fix:** Extended level mapping:
- PM -> "WTA 1000 (Masters equiv.)"
- P -> "WTA 500"
- I -> "WTA 250"

Table now shows all five tier rows with proper ATP/WTA distribution.

### ISSUE 3: Non-GS Z^pre did not match GS Z^pre

**Root cause:** GS regressions used full Z^pre = {points, points^2, Elo, Elo^2, age, had_prior_ll} but non-GS IV used only {points, points^2, age}.

**Fix:**
- Updated non-GS IV formula to use identical Z^pre: `pre_rank_pts + pre_rank_pts_sq + pre_elo + pre_elo_sq + player_age + had_prior_ll`
- Added Elo imputation for non-GS players missing Elo (sample median)
- Applied consistently to first stage and all second-stage specifications

### ISSUE 4: Dose table needed SE and significance stars

**Root cause:** Old `table_dose_final.tex` showed raw means without standard errors or statistical significance indicators.

**Fix:** Rebuilt dose table with:
- Each dose-group cell shows: mean, (SE), and significance stars from t-test vs control
- OLS interaction columns show coefficient with stars and SE in parentheses
- Stars: * p<0.10, ** p<0.05, *** p<0.01

### ISSUE 5: Non-GS tables now mirror GS tables 1-to-1

**New tables produced:**

| GS Table | Non-GS Mirror | Status |
|----------|---------------|--------|
| `table_ll_dist_gs.tex` | `table_ll_dist_nongs.tex` | Updated with WTA tiers |
| `table_sumstats_gs.tex` | `table_sumstats_nongs.tex` | Updated with WTA panel |
| `table_immediate.tex` | `table_immediate_nongs.tex` | NEW -- IV instead of OLS |
| `table_dynamic_atp.tex` | `table_dynamic_nongs.tex` | Updated with IV estimates |
| `table_dynamic_wta.tex` | `table_dynamic_nongs_wta.tex` | NEW -- WTA non-GS IV |
| `table_hetero_atp.tex` | `table_hetero_nongs.tex` | NEW -- WTA non-GS subgroups |
| `table_hetero_wta.tex` | `table_hetero_nongs_wta.tex` | NEW -- WTA non-GS subgroups |
| `table_firstll_atp.tex` | `table_firstll_nongs.tex` | NEW |
| `table_firstll_wta.tex` | `table_firstll_nongs_wta.tex` | NEW |

Additional IV tables:
- `table_dynamic_nongs_iv.tex` -- ATP IV second-stage (compact)
- `table_dynamic_nongs_wta_iv.tex` -- WTA IV second-stage (compact)

## Sample Counts After Fix

| Sample | N | LL | Control |
|--------|---|-----|---------|
| ATP GS | 248 | 132 | 116 |
| WTA GS | 132 | 54 | 78 |
| ATP non-GS | 2,666 | 860 | 1,806 |
| WTA non-GS | 2,545 | 755 | 1,790 |
| ATP non-GS IV | 2,672 | 862 | 1,810 |
| WTA non-GS IV | 2,545 | 755 | 1,790 |

## WTA Non-GS LL Distribution by Tier

| Tier | LL | Control | Total |
|------|-----|---------|-------|
| WTA 1000 (Premier Mandatory) | 58 | 237 | 295 |
| WTA 500 (Premier) | 332 | 808 | 1,140 |
| WTA 250 (International) | 365 | 745 | 1,110 |
| **Total** | **755** | **1,790** | **2,545** |

## Technical Notes

- WTA win model built separately from ATP (tour-specific logit for Bernoulli convolution instrument)
- WTA non-GS restricted to 2009+ (WTA tier system standardized in 2009)
- Elo imputed with sample median for non-GS players without Elo history
- VCOV warnings (non-positive semi-definite) appear for some WTA subgroup regressions -- expected given smaller cells
- All consistency checks pass (dist table N = estimation N, sumstats N = estimation N)
