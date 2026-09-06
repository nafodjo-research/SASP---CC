# Dose Model Fix Diagnostics

Generated: 2026-03-26 19:35:58.430919

## Problem

The uncentered dose model had base delta (effect at matches_won=0) that
was far from the pooled delta because:
- All control observations have matches_won = 0
- The interaction terms (ll x matches_won, ll x matches_won^2) are
  perfectly collinear with got_ll for control observations
- This inflates standard errors and produces unstable base estimates

## Fix

Center matches_won at its treated-group mean before creating interactions:
- matches_won_c = matches_won - mean(matches_won | treated)
- Now the base coefficient on got_ll = effect at the AVERAGE dose
- This should be close to the pooled delta (which averages over all doses)

## Results

### GS-ATP

- Centering mean: 67.941
- N treated: 34, N control: 59
- Pooled delta (no dose): -0.1048 (SE=0.1425, p=0.462, N=4428)
- OLD dose base (uncentered, at wins=0): -0.3731 (SE=0.1454, p=0.010)
- NEW dose base (centered, at mean dose): -0.0129 (SE=0.1906, p=0.946)
- Implied effects:
  - At 0 wins: -0.3257 (SE=0.1641, p=0.047)
  - At 1 wins: -0.3202 (SE=0.1615, p=0.047)
  - At 2 wins: -0.3148 (SE=0.1589, p=0.048)
  - At 3 wins: -0.3094 (SE=0.1564, p=0.048)

### GS-WTA

- Centering mean: 97.333
- N treated: 27, N control: 48
- Pooled delta (no dose): 0.2745 (SE=0.1476, p=0.063, N=3743)
- OLD dose base (uncentered, at wins=0): 0.2020 (SE=0.2348, p=0.390)
- NEW dose base (centered, at mean dose): 0.2853 (SE=0.1686, p=0.091)
- Implied effects:
  - At 0 wins: -0.0159 (SE=0.2815, p=0.955)
  - At 1 wins: -0.0126 (SE=0.2781, p=0.964)
  - At 2 wins: -0.0093 (SE=0.2749, p=0.973)
  - At 3 wins: -0.0059 (SE=0.2716, p=0.983)

### NONGS-ATP

- Centering mean: 65.394
- N treated: 269, N control: 969
- Pooled delta (no dose): 0.0166 (SE=0.0414, p=0.688, N=41737)
- OLD dose base (uncentered, at wins=0): -0.0805 (SE=0.0859, p=0.349)
- NEW dose base (centered, at mean dose): -0.0351 (SE=0.0880, p=0.690)
- Implied effects:
  - At 0 wins: -0.1108 (SE=0.0959, p=0.248)
  - At 1 wins: -0.1095 (SE=0.0949, p=0.248)
  - At 2 wins: -0.1083 (SE=0.0938, p=0.249)
  - At 3 wins: -0.1070 (SE=0.0929, p=0.249)

### NONGS-WTA

- Centering mean: 71.502
- N treated: 261, N control: 975
- Pooled delta (no dose): -0.0126 (SE=0.0429, p=0.769, N=40363)
- OLD dose base (uncentered, at wins=0): -0.1754 (SE=0.0953, p=0.066)
- NEW dose base (centered, at mean dose): 0.1607 (SE=0.1201, p=0.181)
- Implied effects:
  - At 0 wins: -0.1427 (SE=0.1007, p=0.156)
  - At 1 wins: -0.1375 (SE=0.0994, p=0.166)
  - At 2 wins: -0.1323 (SE=0.0981, p=0.177)
  - At 3 wins: -0.1272 (SE=0.0969, p=0.189)

## Inconsistency Check

- GS-ATP: |pooled - dose_base| = 0.0919 -> RESOLVED
- GS-WTA: |pooled - dose_base| = 0.0108 -> RESOLVED
- NONGS-ATP: |pooled - dose_base| = 0.0518 -> RESOLVED
- NONGS-WTA: |pooled - dose_base| = 0.1733 -> STILL DIVERGENT
