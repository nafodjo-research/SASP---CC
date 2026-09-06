# Tournament Horizon Heterogeneity Summary (Script 26)
Generated: 2026-03-26 14:51:40.554898

## Design

Horizon-heterogeneous tournament performance specifications.
For 2 weighted specifications (points-weighted, any LL stop; points-weighted, same-type stop),
the single delta is replaced by 5 cumulative horizon indicators:
  D_4w = D_i * 1(t <= 4 weeks)
  D_8w = D_i * 1(t <= 8 weeks)
  D_12w = D_i * 1(t <= 12 weeks)
  D_26w = D_i * 1(t <= 26 weeks)
  D_52w = D_i * 1(t <= 52 weeks) [= D_i for all obs]

Since these are nested (D_4w=1 implies D_8w=1, etc.), the coefficient on D_hw
captures the INCREMENTAL effect of LL entry for matches within h weeks,
beyond what is already captured by longer horizons.

Total LL effect at horizon h = sum of delta_h' for all h' >= h.
E.g., total effect at 4 weeks = delta_4w + delta_8w + delta_12w + delta_26w + delta_52w.

GS models: weighted logit with player-clustered SEs (lottery assignment).
NonGS models: CF logit with 5 horizon-specific v_hat controls, 200-rep player block bootstrap.

## GS-ATP

### Any LL stop

N_matches = 6,128, N_events = 170

| Horizon | delta | SE | p-value | OR |
|---|---|---|---|---|
| 4w | -0.4266 | 0.3339 | 0.2014 | 0.653 |
| 8w | 0.2225 | 0.5485 | 0.6850 | 1.249 |
| 12w | -0.2297 | 0.4637 | 0.6203 | 0.795 |
| 26w | 0.2891 | 0.1979 | 0.1442 | 1.335 |
| 52w | -0.1917 | 0.1671 | 0.2515 | 0.826 |

Cumulative LL effects (sum from horizon h to 52w):

  Total at 4w: -0.3363 (OR = 0.714)
  Total at 8w: 0.0903 (OR = 1.094)
  Total at 12w: -0.1322 (OR = 0.876)
  Total at 26w: 0.0974 (OR = 1.102)
  Total at 52w: -0.1917 (OR = 0.826)

### Same-type stop

N_matches = 8,370, N_events = 172

| Horizon | delta | SE | p-value | OR |
|---|---|---|---|---|
| 4w | -0.4319 | 0.3175 | 0.1737 | 0.649 |
| 8w | -0.0021 | 0.5131 | 0.9968 | 0.998 |
| 12w | -0.0343 | 0.4076 | 0.9330 | 0.966 |
| 26w | 0.2086 | 0.1809 | 0.2490 | 1.232 |
| 52w | -0.0715 | 0.1651 | 0.6649 | 0.931 |

Cumulative LL effects (sum from horizon h to 52w):

  Total at 4w: -0.3312 (OR = 0.718)
  Total at 8w: 0.1007 (OR = 1.106)
  Total at 12w: 0.1028 (OR = 1.108)
  Total at 26w: 0.1370 (OR = 1.147)
  Total at 52w: -0.0715 (OR = 0.931)

## GS-WTA

### Any LL stop

N_matches = 3,993, N_events = 109

| Horizon | delta | SE | p-value | OR |
|---|---|---|---|---|
| 4w | -0.0823 | 0.4378 | 0.8509 | 0.921 |
| 8w | 1.0539 | 0.3996 | 0.0084 | 2.869 |
| 12w | -0.7293 | 0.3975 | 0.0665 | 0.482 |
| 26w | -0.7464 | 0.2956 | 0.0116 | 0.474 |
| 52w | 0.5086 | 0.1805 | 0.0048 | 1.663 |

Cumulative LL effects (sum from horizon h to 52w):

  Total at 4w: 0.0045 (OR = 1.005)
  Total at 8w: 0.0868 (OR = 1.091)
  Total at 12w: -0.9671 (OR = 0.380)
  Total at 26w: -0.2378 (OR = 0.788)
  Total at 52w: 0.5086 (OR = 1.663)

### Same-type stop

N_matches = 5,287, N_events = 112

| Horizon | delta | SE | p-value | OR |
|---|---|---|---|---|
| 4w | 0.1728 | 0.4443 | 0.6973 | 1.189 |
| 8w | 0.7349 | 0.4140 | 0.0759 | 2.085 |
| 12w | -0.8429 | 0.4079 | 0.0388 | 0.430 |
| 26w | -0.3457 | 0.3073 | 0.2606 | 0.708 |
| 52w | 0.5326 | 0.2030 | 0.0087 | 1.703 |

Cumulative LL effects (sum from horizon h to 52w):

  Total at 4w: 0.2516 (OR = 1.286)
  Total at 8w: 0.0788 (OR = 1.082)
  Total at 12w: -0.6561 (OR = 0.519)
  Total at 26w: 0.1869 (OR = 1.205)
  Total at 52w: 0.5326 (OR = 1.703)

## NONGS-ATP

### Any LL stop

N_matches = 75,089, N_events = 2124

| Horizon | delta | SE | p-value | OR |
|---|---|---|---|---|
| 4w | -0.0186 | 0.2356 | 0.9371 | 0.982 |
| 8w | -0.1639 | 0.2711 | 0.5456 | 0.849 |
| 12w | -0.0347 | 0.1936 | 0.8579 | 0.966 |
| 26w | -0.1708 | 0.1391 | 0.2194 | 0.843 |
| 52w | 0.1469 | 0.1218 | 0.2276 | 1.158 |

Cumulative LL effects (sum from horizon h to 52w):

  Total at 4w: -0.2410 (OR = 0.786)
  Total at 8w: -0.2224 (OR = 0.801)
  Total at 12w: -0.0585 (OR = 0.943)
  Total at 26w: -0.0239 (OR = 0.976)
  Total at 52w: 0.1469 (OR = 1.158)

| Horizon | rho | p-value |
|---|---|---|
| 4w | -0.2381 | 0.1577 |
| 8w | 0.2015 | 0.2184 |
| 12w | 0.0510 | 0.7507 |
| 26w | 0.0671 | 0.5705 |
| 52w | 0.0183 | 0.8446 |

### Same-type stop

N_matches = 76,708, N_events = 2126

| Horizon | delta | SE | p-value | OR |
|---|---|---|---|---|
| 4w | -0.0336 | 0.2392 | 0.8885 | 0.967 |
| 8w | -0.1538 | 0.2519 | 0.5414 | 0.857 |
| 12w | -0.0560 | 0.2038 | 0.7834 | 0.946 |
| 26w | -0.1433 | 0.1399 | 0.3058 | 0.867 |
| 52w | 0.1320 | 0.1261 | 0.2954 | 1.141 |

Cumulative LL effects (sum from horizon h to 52w):

  Total at 4w: -0.2547 (OR = 0.775)
  Total at 8w: -0.2212 (OR = 0.802)
  Total at 12w: -0.0673 (OR = 0.935)
  Total at 26w: -0.0113 (OR = 0.989)
  Total at 52w: 0.1320 (OR = 1.141)

| Horizon | rho | p-value |
|---|---|---|
| 4w | -0.2367 | 0.1220 |
| 8w | 0.2093 | 0.2400 |
| 12w | 0.0866 | 0.5850 |
| 26w | 0.0260 | 0.8254 |
| 52w | 0.0266 | 0.7545 |

## NONGS-WTA

### Any LL stop

N_matches = 66,161, N_events = 2033

| Horizon | delta | SE | p-value | OR |
|---|---|---|---|---|
| 4w | 0.0070 | 0.2482 | 0.9776 | 1.007 |
| 8w | -0.0045 | 0.2673 | 0.9867 | 0.996 |
| 12w | -0.1439 | 0.1765 | 0.4151 | 0.866 |
| 26w | 0.2124 | 0.1554 | 0.1716 | 1.237 |
| 52w | -0.1253 | 0.1468 | 0.3931 | 0.882 |

Cumulative LL effects (sum from horizon h to 52w):

  Total at 4w: -0.0543 (OR = 0.947)
  Total at 8w: -0.0612 (OR = 0.941)
  Total at 12w: -0.0568 (OR = 0.945)
  Total at 26w: 0.0871 (OR = 1.091)
  Total at 52w: -0.1253 (OR = 0.882)

| Horizon | rho | p-value |
|---|---|---|
| 4w | -0.2189 | 0.2163 |
| 8w | 0.2478 | 0.2200 |
| 12w | 0.0124 | 0.9468 |
| 26w | -0.1053 | 0.3498 |
| 52w | 0.0266 | 0.7841 |

### Same-type stop

N_matches = 66,990, N_events = 2036

| Horizon | delta | SE | p-value | OR |
|---|---|---|---|---|
| 4w | 0.0240 | 0.2242 | 0.9147 | 1.024 |
| 8w | -0.0228 | 0.2627 | 0.9309 | 0.977 |
| 12w | -0.1363 | 0.1856 | 0.4626 | 0.873 |
| 26w | 0.1998 | 0.1488 | 0.1794 | 1.221 |
| 52w | -0.1260 | 0.1350 | 0.3507 | 0.882 |

Cumulative LL effects (sum from horizon h to 52w):

  Total at 4w: -0.0613 (OR = 0.941)
  Total at 8w: -0.0853 (OR = 0.918)
  Total at 12w: -0.0626 (OR = 0.939)
  Total at 26w: 0.0738 (OR = 1.077)
  Total at 52w: -0.1260 (OR = 0.882)

| Horizon | rho | p-value |
|---|---|---|
| 4w | -0.2234 | 0.2097 |
| 8w | 0.2522 | 0.2399 |
| 12w | 0.0079 | 0.9673 |
| 26w | -0.0941 | 0.3197 |
| 52w | 0.0242 | 0.7834 |


## Execution Log

## Phase 1: Data Loaded
- GS-ATP events: 172 | same-type matches: 8370
- GS-WTA events: 112 | same-type matches: 5287
- nonGS-ATP events: 2222 | same-type matches: 76708
- nonGS-WTA events: 2155 | same-type matches: 66990

## Phase 2: Any-LL Stop Match Data
- GS-ATP: any=6128 vs same=8370
- GS-WTA: any=3993 vs same=5287
- nonGS-ATP: any=75089 vs same=76708
- nonGS-WTA: any=66161 vs same=66990

## Phase 4: Horizon Indicators Added

