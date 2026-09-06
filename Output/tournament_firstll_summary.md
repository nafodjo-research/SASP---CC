# Tournament First-LL + Dose Analysis Summary (Script 28)
Generated: 2026-03-26 16:12:46.673321

## Design

### Part 1: First-LL Restriction
Two restriction variants:
- **firstll (type-specific)**: No prior LL of SAME type (GS for GS samples, nonGS for nonGS)
- **anytype**: No prior LL of ANY type (had_prior_ll == 0)

For each of 8 samples (4 subsamples x 2 truncation rules):
- Main specification (pooled delta)
- Robustness table (4 specs: unw/wt x any/same stop)
- Horizon heterogeneity (5 horizons, points-weighted, cumulative effects)

### Part 2: Treatment Dose
Interactions: ll_entry x matches_won + ll_entry x matches_won_sq
Implied total effects at 0, 1, 2, 3 wins

## FIRSTLL Results

### GS-ATP
- delta = -0.0415 (SE = 0.1039, p = 0.6899)
- OR = 0.959
- N_matches = 7,347, N_events = 151, N_players = 130

### GS-WTA
- delta = 0.2610 (SE = 0.1223, p = 0.0328)
- OR = 1.298
- N_matches = 5,109, N_events = 107, N_players = 98

### NONGS-ATP
- delta = 0.0360 (SE = 0.0682, p = 0.5977)
- OR = 1.037
- N_matches = 52,177, N_events = 1453, N_players = 792

- rho = 0.0677 (SE = 0.0500, p = 0.1761)

### NONGS-WTA
- delta = -0.0137 (SE = 0.0778, p = 0.8602)
- OR = 0.986
- N_matches = 47,361, N_events = 1388, N_players = 694

- rho = -0.0011 (SE = 0.0492, p = 0.9822)

## ANYTYPE Results

### GS-ATP
- delta = -0.1048 (SE = 0.1425, p = 0.4623)
- OR = 0.901
- N_matches = 4,428, N_events = 93, N_players = 84

### GS-WTA
- delta = 0.2745 (SE = 0.1476, p = 0.0628)
- OR = 1.316
- N_matches = 3,743, N_events = 75, N_players = 70

### NONGS-ATP
- delta = 0.0166 (SE = 0.0768, p = 0.8285)
- OR = 1.017
- N_matches = 41,737, N_events = 1183, N_players = 715

- rho = 0.0956 (SE = 0.0561, p = 0.0883)

### NONGS-WTA
- delta = -0.0126 (SE = 0.0751, p = 0.8670)
- OR = 0.988
- N_matches = 40,363, N_events = 1171, N_players = 639

- rho = 0.0151 (SE = 0.0476, p = 0.7510)

## DOSE RESULTS (firstll type-specific sample)

### GS-ATP
- base delta = -0.3731 (SE = 0.1454)
- dose coef = 0.0075 (SE = 0.0034)
- dose_sq coef = -0.0000 (SE = 0.0000)
- Implied total effects:
  - At 0 wins: -0.3731 (SE = 0.1454)
  - At 1 wins: -0.3656 (SE = 0.1430)
  - At 2 wins: -0.3581 (SE = 0.1407)
  - At 3 wins: -0.3507 (SE = 0.1385)

### GS-WTA
- base delta = 0.2020 (SE = 0.2348)
- dose coef = -0.0000 (SE = 0.0039)
- dose_sq coef = 0.0000 (SE = 0.0000)
- Implied total effects:
  - At 0 wins: 0.2020 (SE = 0.2348)
  - At 1 wins: 0.2020 (SE = 0.2318)
  - At 2 wins: 0.2020 (SE = 0.2289)
  - At 3 wins: 0.2020 (SE = 0.2259)

### NONGS-ATP
- base delta = -0.0805 (SE = 0.0859)
- dose coef = 0.0011 (SE = 0.0022)
- dose_sq coef = -0.0000 (SE = 0.0000)
- Implied total effects:
  - At 0 wins: -0.0805 (SE = 0.0859)
  - At 1 wins: -0.0794 (SE = 0.0847)
  - At 2 wins: -0.0783 (SE = 0.0835)
  - At 3 wins: -0.0772 (SE = 0.0824)

### NONGS-WTA
- base delta = -0.1754 (SE = 0.0953)
- dose coef = 0.0046 (SE = 0.0022)
- dose_sq coef = -0.0000 (SE = 0.0000)
- Implied total effects:
  - At 0 wins: -0.1754 (SE = 0.0953)
  - At 1 wins: -0.1709 (SE = 0.0943)
  - At 2 wins: -0.1664 (SE = 0.0934)
  - At 3 wins: -0.1618 (SE = 0.0924)


## Execution Log

## Phase 1: Data Loaded
- GS-ATP events: 172 | matches: 8370
- GS-WTA events: 112 | matches: 5287
- nonGS-ATP events: 2222 | matches: 76708
- nonGS-WTA events: 2155 | matches: 66990

## Phase 2: Match Data Built
- GS-ATP: same=8370, any=6128
- GS-WTA: same=5287, any=3993
- nonGS-ATP: same=76708, any=75089
- nonGS-WTA: same=66990, any=66161

## Phase 3: First-LL Sample Counts
  firstll/gs_atp_any: events=151 (LL=58), matches=5372
  firstll/gs_atp_same: events=151 (LL=58), matches=7347
  firstll/gs_wta_any: events=107 (LL=37), matches=3884
  firstll/gs_wta_same: events=107 (LL=37), matches=5109
  firstll/nongs_atp_any: events=1524 (LL=354), matches=51363
  firstll/nongs_atp_same: events=1520 (LL=354), matches=52177
  firstll/nongs_wta_any: events=1461 (LL=319), matches=46874
  firstll/nongs_wta_same: events=1461 (LL=319), matches=47361

  anytype/gs_atp_any: events=93 (LL=34), matches=3445
  anytype/gs_atp_same: events=93 (LL=34), matches=4428
  anytype/gs_wta_any: events=75 (LL=27), matches=3086
  anytype/gs_wta_same: events=75 (LL=27), matches=3743
  anytype/nongs_atp_any: events=1243 (LL=270), matches=41281
  anytype/nongs_atp_same: events=1238 (LL=269), matches=41737
  anytype/nongs_wta_any: events=1236 (LL=261), matches=40010
  anytype/nongs_wta_same: events=1236 (LL=261), matches=40363

