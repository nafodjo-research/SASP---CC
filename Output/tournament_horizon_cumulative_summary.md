# Tournament Horizon Cumulative Effects Summary (Script 27)
Generated: 2026-03-26 15:23:14.385873

## Design

Re-estimation of horizon-heterogeneous specs from script 26, now with
cumulative total effects and proper SEs.

Model: won ~ D_4w + D_8w + D_12w + D_26w + D_52w + covariates
where D_hw = D_i * 1(t <= h).

Since D_4w=1 implies D_8w=1 etc., a match at t<=4w has ALL five dummies active.
The incremental coefficient delta_h captures the additional effect at horizon h.

Cumulative total effect at horizon h = sum(delta_j for j = h, h+1, ..., 52w)
  Total at 4w  = delta_4w + delta_8w + delta_12w + delta_26w + delta_52w
  Total at 8w  = delta_8w + delta_12w + delta_26w + delta_52w
  Total at 12w = delta_12w + delta_26w + delta_52w
  Total at 26w = delta_26w + delta_52w
  Total at 52w = delta_52w

GS SEs: from clustered vcov matrix (sqrt of sum of relevant vcov block).
NonGS SEs: from bootstrap SD of cumulative totals across 200 reps.

## GS-ATP

### Any LL stop

N_matches = 6,128, N_events = 170

| Horizon | Incr. delta | SE | p | Total | SE | p | OR (total) |
|---|---|---|---|---|---|---|---|
| 4w | -0.4266 | 0.3339 | 0.2014 | -0.3363 | 0.2958 | 0.2556 | 0.714 |
| 8w | 0.2225 | 0.5485 | 0.6850 | 0.0903 | 0.3396 | 0.7904 | 1.094 |
| 12w | -0.2297 | 0.4637 | 0.6203 | -0.1322 | 0.4126 | 0.7485 | 0.876 |
| 26w | 0.2891 | 0.1979 | 0.1442 | 0.0974 | 0.2393 | 0.6839 | 1.102 |
| 52w | -0.1917 | 0.1671 | 0.2515 | -0.1917 | 0.1671 | 0.2515 | 0.826 |

### Same-type stop

N_matches = 8,370, N_events = 172

| Horizon | Incr. delta | SE | p | Total | SE | p | OR (total) |
|---|---|---|---|---|---|---|---|
| 4w | -0.4319 | 0.3175 | 0.1737 | -0.3312 | 0.2748 | 0.2281 | 0.718 |
| 8w | -0.0021 | 0.5131 | 0.9968 | 0.1007 | 0.3140 | 0.7485 | 1.106 |
| 12w | -0.0343 | 0.4076 | 0.9330 | 0.1028 | 0.3898 | 0.7921 | 1.108 |
| 26w | 0.2086 | 0.1809 | 0.2490 | 0.1370 | 0.2158 | 0.5255 | 1.147 |
| 52w | -0.0715 | 0.1651 | 0.6649 | -0.0715 | 0.1651 | 0.6649 | 0.931 |

## GS-WTA

### Any LL stop

N_matches = 3,993, N_events = 109

| Horizon | Incr. delta | SE | p | Total | SE | p | OR (total) |
|---|---|---|---|---|---|---|---|
| 4w | -0.0823 | 0.4378 | 0.8509 | 0.0045 | 0.3711 | 0.9902 | 1.005 |
| 8w | 1.0539 | 0.3996 | 0.0084 | 0.0868 | 0.3069 | 0.7772 | 1.091 |
| 12w | -0.7293 | 0.3975 | 0.0665 | -0.9671 | 0.3200 | 0.0025 | 0.380 |
| 26w | -0.7464 | 0.2956 | 0.0116 | -0.2378 | 0.3277 | 0.4681 | 0.788 |
| 52w | 0.5086 | 0.1805 | 0.0048 | 0.5086 | 0.1805 | 0.0048 | 1.663 |

### Same-type stop

N_matches = 5,287, N_events = 112

| Horizon | Incr. delta | SE | p | Total | SE | p | OR (total) |
|---|---|---|---|---|---|---|---|
| 4w | 0.1728 | 0.4443 | 0.6973 | 0.2516 | 0.3734 | 0.5004 | 1.286 |
| 8w | 0.7349 | 0.4140 | 0.0759 | 0.0788 | 0.2959 | 0.7900 | 1.082 |
| 12w | -0.8429 | 0.4079 | 0.0388 | -0.6561 | 0.3348 | 0.0501 | 0.519 |
| 26w | -0.3457 | 0.3073 | 0.2606 | 0.1869 | 0.2912 | 0.5211 | 1.205 |
| 52w | 0.5326 | 0.2030 | 0.0087 | 0.5326 | 0.2030 | 0.0087 | 1.703 |

## NONGS-ATP

### Any LL stop

N_matches = 75,089, N_events = 2124

| Horizon | Incr. delta | SE | p | Total | SE | p | OR (total) |
|---|---|---|---|---|---|---|---|
| 4w | -0.0186 | 0.2356 | 0.9371 | -0.2410 | 0.1773 | 0.1739 | 0.786 |
| 8w | -0.1639 | 0.2711 | 0.5456 | -0.2224 | 0.1753 | 0.2046 | 0.801 |
| 12w | -0.0347 | 0.1936 | 0.8579 | -0.0585 | 0.1777 | 0.7418 | 0.943 |
| 26w | -0.1708 | 0.1391 | 0.2194 | -0.0239 | 0.1188 | 0.8406 | 0.976 |
| 52w | 0.1469 | 0.1218 | 0.2276 | 0.1469 | 0.1218 | 0.2276 | 1.158 |

### Same-type stop

N_matches = 76,708, N_events = 2126

| Horizon | Incr. delta | SE | p | Total | SE | p | OR (total) |
|---|---|---|---|---|---|---|---|
| 4w | -0.0336 | 0.2392 | 0.8885 | -0.2547 | 0.1822 | 0.1622 | 0.775 |
| 8w | -0.1538 | 0.2519 | 0.5414 | -0.2212 | 0.1708 | 0.1953 | 0.802 |
| 12w | -0.0560 | 0.2038 | 0.7834 | -0.0673 | 0.1988 | 0.7348 | 0.935 |
| 26w | -0.1433 | 0.1399 | 0.3058 | -0.0113 | 0.1162 | 0.9225 | 0.989 |
| 52w | 0.1320 | 0.1261 | 0.2954 | 0.1320 | 0.1261 | 0.2954 | 1.141 |

## NONGS-WTA

### Any LL stop

N_matches = 66,161, N_events = 2033

| Horizon | Incr. delta | SE | p | Total | SE | p | OR (total) |
|---|---|---|---|---|---|---|---|
| 4w | 0.0070 | 0.2482 | 0.9776 | -0.0543 | 0.1352 | 0.6883 | 0.947 |
| 8w | -0.0045 | 0.2673 | 0.9867 | -0.0612 | 0.2018 | 0.7616 | 0.941 |
| 12w | -0.1439 | 0.1765 | 0.4151 | -0.0568 | 0.1773 | 0.7488 | 0.945 |
| 26w | 0.2124 | 0.1554 | 0.1716 | 0.0871 | 0.1125 | 0.4389 | 1.091 |
| 52w | -0.1253 | 0.1468 | 0.3931 | -0.1253 | 0.1468 | 0.3931 | 0.882 |

### Same-type stop

N_matches = 66,990, N_events = 2036

| Horizon | Incr. delta | SE | p | Total | SE | p | OR (total) |
|---|---|---|---|---|---|---|---|
| 4w | 0.0240 | 0.2242 | 0.9147 | -0.0613 | 0.1370 | 0.6545 | 0.941 |
| 8w | -0.0228 | 0.2627 | 0.9309 | -0.0853 | 0.1831 | 0.6412 | 0.918 |
| 12w | -0.1363 | 0.1856 | 0.4626 | -0.0626 | 0.1847 | 0.7349 | 0.939 |
| 26w | 0.1998 | 0.1488 | 0.1794 | 0.0738 | 0.1131 | 0.5140 | 1.077 |
| 52w | -0.1260 | 0.1350 | 0.3507 | -0.1260 | 0.1350 | 0.3507 | 0.882 |


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

