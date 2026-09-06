# Tournament Robustness Summary (Script 25)
Generated: 2026-03-26 11:23:59.96783

## Design

4 specifications per sample, varying:
- Window stop rule: any LL opportunity (GS or non-GS) vs same-type LL only
- Weighting: unweighted vs ranking-points-weighted

GS models: simple logit with clustered SEs (lottery assignment).
NonGS models: CF logit with generalized residual v_hat, 200-rep player-level block bootstrap.

## GS-ATP

| Specification | N_matches | N_events | delta | SE | OR | rho | endog p |
|---|---|---|---|---|---|---|---|
| Unweighted, any LL stop | 6,128 | 170 | -0.0648 | 0.1188 | 0.937 | --- | --- |
| Unweighted, same-type stop | 8,370 | 172 | -0.0061 | 0.0987 | 0.994 | --- | --- |
| Points-weighted, any LL stop | 6,128 | 170 | -0.0734 | 0.1709 | 0.929 | --- | --- |
| Points-weighted, same-type stop | 8,370 | 172 | 0.0026 | 0.1600 | 1.003 | --- | --- |

## GS-WTA

| Specification | N_matches | N_events | delta | SE | OR | rho | endog p |
|---|---|---|---|---|---|---|---|
| Unweighted, any LL stop | 3,993 | 109 | 0.1557 | 0.1473 | 1.168 | --- | --- |
| Unweighted, same-type stop | 5,287 | 112 | 0.2102 | 0.1261 | 1.234 | --- | --- |
| Points-weighted, any LL stop | 3,993 | 109 | 0.1188 | 0.1854 | 1.126 | --- | --- |
| Points-weighted, same-type stop | 5,287 | 112 | 0.2978 | 0.1607 | 1.347 | --- | --- |

## NONGS-ATP

| Specification | N_matches | N_events | delta | SE | OR | rho | endog p |
|---|---|---|---|---|---|---|---|
| Unweighted, any LL stop | 75,089 | 2124 | 0.0292 | 0.0588 | 1.030 | 0.0441 | 0.309 |
| Unweighted, same-type stop | 76,708 | 2126 | 0.0310 | 0.0573 | 1.031 | 0.0445 | 0.274 |
| Points-weighted, any LL stop | 75,089 | 2124 | 0.0056 | 0.0910 | 1.006 | 0.0865 | 0.190 |
| Points-weighted, same-type stop | 76,708 | 2126 | 0.0027 | 0.0907 | 1.003 | 0.0816 | 0.215 |

## NONGS-WTA

| Specification | N_matches | N_events | delta | SE | OR | rho | endog p |
|---|---|---|---|---|---|---|---|
| Unweighted, any LL stop | 66,161 | 2033 | -0.0104 | 0.0612 | 0.990 | -0.0076 | 0.843 |
| Unweighted, same-type stop | 66,990 | 2036 | -0.0142 | 0.0615 | 0.986 | -0.0076 | 0.835 |
| Points-weighted, any LL stop | 66,161 | 2033 | -0.0386 | 0.0972 | 0.962 | -0.0050 | 0.933 |
| Points-weighted, same-type stop | 66,990 | 2036 | -0.0464 | 0.0917 | 0.955 | -0.0018 | 0.976 |


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

