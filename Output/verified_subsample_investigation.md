# Verified Subsample Investigation
Generated: 2026-03-25 17:07:19.129012

## Sample Sizes
- Verified lottery events: 34
- ATP verified: N = 100 (LL = 38)
- WTA verified: N = 44 (LL = 14)

## ATP Verified: Elo Regression Diagnostics

### Top 5 Most Influential Observations (Cook's Distance)
| Player ID | Tournament | Horizon | LL | Elo Change | Pre Rank Pts | Pre Elo | Cook's D | Leverage |
|-----------|------------|---------|-----|------------|-------------|---------|----------|----------|
| 104262 | 2009-520 | 52w | 0 | -181.9 | 732 | 1774 | 0.0477 | 0.0968 |
| 103535 | 2009-580 | 26w | 0 | -132.6 | 628 | 1696 | 0.0395 | 0.1337 |
| 106234 | 2020-520 | 26w | 0 | 185.9 | 519 | 1844 | 0.0362 | 0.0886 |
| 105041 | 2011-560 | 26w | 1 | 195 | 324 | 1678 | 0.0268 | 0.0976 |
| 104978 | 2012-520 | 52w | 0 | 199.9 | 513 | 1732 | 0.024 | 0.0693 |

Cook's D threshold (4/(n-p)): 0.0095
Observations above threshold: 30 / 458

### ATP Elo After Dropping Top 3 Influential Observations
| Horizon | Coef | SE | p-value | Significant? |
|---------|------|----|---------|-------------|
| 4w | -18.06 | 6.97 | 0.01 | YES |
| 8w | -17.87 | 8.83 | 0.043 | YES |
| 12w | -20.36 | 8.52 | 0.017 | YES |
| 26w | -18.17 | 12.55 | 0.148 | no |
| 52w | -38.28 | 15.3 | 0.012 | YES |

## WTA Verified: Treated Player Elo Change Distribution
- N treated players: 14

### Individual Treated Player Elo Changes
| Player ID | Tournament | Pre Elo | Elo 4w | Elo 12w | Elo 26w | Elo 52w |
|-----------|------------|---------|--------|---------|---------|---------|
| 201305 | 2012-W-SL-USA-01A-2012 | 1875 | 17.1 | 29.9 | 25.8 | -63.7 |
| 201318 | 2010-W-SL-FRA-01A-2010 | 1987 | 5.8 | 20.6 | 60.8 | 95.4 |
| 201387 | 2007-W-SL-FRA-01A-2007 | 1790 | 10.5 | 11.2 | 85.7 | 152 |
| 201402 | 2012-W-SL-FRA-01A-2012 | 1890 | -6.3 | 46.6 | -9.8 | -44.9 |
| 201441 | 2007-W-SL-FRA-01A-2007 | 1690 | -10.1 | -1.3 | NA | 3 |
| 201465 | 2006-W-SL-FRA-01A-2006 | 1749 | 28.6 | 84.6 | NA | -14.8 |
| 201537 | 2013-W-SL-USA-01A-2013 | 1881 | 67.8 | 54.8 | 64.8 | 37.4 |
| 201542 | 2013-W-SL-USA-01A-2013 | 1787 | -7.3 | 6.5 | 47.9 | 70.6 |
| 201568 | 2009-W-SL-GBR-01A-2009 | 1766 | 45.4 | 34.3 | 39.9 | -54.3 |
| 201573 | 2012-W-SL-FRA-01A-2012 | 1842 | 46.3 | 83.9 | NA | 168.1 |
| 201604 | 2014-W-SL-AUS-01A-2014 | 1843 | -28.4 | -30.9 | -17.4 | 102.4 |
| 206292 | 2020-520 | 1931 | -13 | NA | 4.4 | 16.1 |
| 214082 | 2015-W-SL-USA-01A-2015 | 1998 | 46.5 | NA | 131.3 | 144 |
| 215872 | 2024-560 | 2026 | 55.2 | 52.2 | NA | NA |
- Elo change 4w: mean = 18.4, sd = 30, min = -28.4, max = 67.8, N = 14
- Elo change 12w: mean = 32.7, sd = 34.2, min = -30.9, max = 84.6, N = 12
- Elo change 26w: mean = 43.4, sd = 45.5, min = -17.4, max = 131.3, N = 10
- Elo change 52w: mean = 47, sd = 81.1, min = -63.7, max = 168.1, N = 13

### WTA Elo After Dropping Top 3 Influential Observations
| Horizon | Coef | SE | p-value | Significant? |
|---------|------|----|---------|-------------|
| 4w | 17.83 | 8.77 | 0.042 | YES |
| 8w | 28.01 | 10.36 | 0.007 | YES |
| 12w | 44.92 | 13.35 | 0.001 | YES |
| 26w | 42.01 | 14.62 | 0.004 | YES |
| 52w | 39.48 | 22.78 | 0.083 | marginal |

## Full vs Verified Sample Comparison
- Full ATP GS: N = 248 (LL = 132)
- Verified ATP: N = 100 (LL = 38)
- Sample reduction ATP: 59.7%
- Full WTA GS: N = 132 (LL = 54)
- Verified WTA: N = 44 (LL = 14)
- Sample reduction WTA: 66.7%

## Interpretation
The verified subsample restricts to events where the LL was NOT the top-ranked
qualifying loser, providing stronger evidence of random lottery assignment.
Contradictions between full and verified samples may indicate:
1. Small sample bias (especially WTA with ~14 treated)
2. Composition effects (different events/years in verified sample)
3. The top-ranked LL recipients (excluded in verified) differ systematically

