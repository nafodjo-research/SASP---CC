# WTA Elo at 26w Investigation
Generated: 2026-03-25 10:01:34.018171

## Separate Regressions (one per horizon)

### 4w
- Coefficient: 6.80
- SE: 5.08
- p-value: 0.1811
- N: 127 (treat=51, ctrl=76)
- Status: not significant

### 8w
- Coefficient: 7.64
- SE: 7.06
- p-value: 0.2787
- N: 126 (treat=52, ctrl=74)
- Status: not significant

### 12w
- Coefficient: 6.55
- SE: 9.46
- p-value: 0.4884
- N: 119 (treat=47, ctrl=72)
- Status: not significant

### 26w
- Coefficient: 17.54
- SE: 10.65
- p-value: 0.0996
- N: 97 (treat=38, ctrl=59)
- Status: marginal at 10%

### 52w
- Coefficient: 15.96
- SE: 16.37
- p-value: 0.3295
- N: 108 (treat=40, ctrl=68)
- Status: not significant

## Stacked Model Results

- 4w: coef = 7.24, SE = 5.00, p = 0.1477 (not significant)
- 8w: coef = 5.97, SE = 6.51, p = 0.3594 (not significant)
- 12w: coef = 6.26, SE = 8.06, p = 0.4376 (not significant)
- 26w: coef = 14.99, SE = 9.58, p = 0.1175 (not significant)
- 52w: coef = 20.53, SE = 13.80, p = 0.1370 (not significant)

## Conclusion
- 26w separate regression significant at 5%: FALSE
- 26w stacked model significant at 5%: FALSE
- The WTA Elo at 26w is NOT significant in either specification.
- The 'null Elo' claim holds.
