# Tournament Match Fix Summary (Script 24)
Generated: 2026-03-26 11:00:06.662857

## Fixes Applied

### FIX 1: Include ALL matches (main + qual + challenger)
The build_match_level() function in script 22 filtered match_source == 'main',
dropping all qualifying and challenger matches. Players who return to challengers
after a GS LL spot were invisible. Fix: removed the filter.

### FIX 2: All covariate coefficients in tables
Tables now report ALL covariates (log_rank_ratio, surface, H2H, etc.),
not just delta and rho.

### FIX 3: Regenerated 4-panel figure with corrected data
fig_tournament_4panel.pdf now uses the full match dataset.

### FIX 4: Points-weighted robustness with corrected data
Panel C of each table re-estimated with corrected match data.

## Match Count Comparison (OLD vs NEW)

| Sample | Old Matches | New Matches | Old Players | New Players |
|--------|-------------|-------------|-------------|-------------|
| GS-ATP | 2,287 | 8,370 | 124 | 130 |
| GS-WTA | 1,749 | 5,287 | 97 | 98 |
| nonGS-ATP | 19,367 | 76,708 | 612 | 796 |
| nonGS-WTA | 19,926 | 66,990 | 540 | 703 |

## Delta Estimates (NEW, corrected data)

## Phase 1: Data Loaded
- Total combined matches: 779724
- Main draw: 143530
- Qual/Challenger: 636194

## FIX 1: Match-Level Datasets (ALL match sources)
- GS-ATP: 8370 matches (was 2287), 172 events, 130 players (was 124)
- GS-WTA: 5287 matches (was 1749), 112 events, 98 players (was 97)
- nonGS-ATP: 76708 matches (was 19367), 2126 events, 796 players (was 612)
- nonGS-WTA: 66990 matches (was 19926), 2036 events, 703 players (was 540)

## Phase 3-5: Re-estimation Results
- GS-ATP: delta=-0.0061 (SE=0.0987, p=0.9511), N=8370
- GS-WTA: delta=0.2102 (SE=0.1261, p=0.0955), N=5287
- nonGS-ATP: delta=0.0310 (bootSE=0.0587), rho=0.0445, N=76708
- nonGS-WTA: delta=-0.0142 (bootSE=0.0638), rho=-0.0076, N=66990


