# Verified Subsample Power Analysis
Generated: 2026-03-25 10:01:35.192186

## Sample: ATP Verified Lottery (N = 100)
- Treated: 38
- Control: 62
- Proportion treated: 0.380

## MDE Calculation
- Alpha = 0.05 (two-sided)
- Power = 0.80
- Formula: MDE = (z_{0.025} + z_{0.80}) * sigma / sqrt(N * p * (1-p))
- = (1.96 + 0.84) * sigma / sqrt(100 * 0.380 * 0.620)

### points_change_4w
- SD (verified): 56.7
- SD (full): 59.9
- MDE (verified, 80% power): 32.7
- Full-sample estimate: 21.61
- Full-sample SE: 8.34
- |Full estimate| > MDE? FALSE

### points_change_12w
- SD (verified): 121.5
- SD (full): 116.6
- MDE (verified, 80% power): 70.1
- Full-sample estimate: 26.02
- Full-sample SE: 15.59
- |Full estimate| > MDE? FALSE

### points_change_26w
- SD (verified): 252.2
- SD (full): 209.6
- MDE (verified, 80% power): 145.6
- Full-sample estimate: 35.31
- Full-sample SE: 31.27
- |Full estimate| > MDE? FALSE

### n_main_draws_12w
- SD (verified): 1.6
- SD (full): 1.7
- MDE (verified, 80% power): 0.9
- Full-sample estimate: 0.33
- Full-sample SE: 0.19
- |Full estimate| > MDE? FALSE

### n_main_draws_26w
- SD (verified): 2.8
- SD (full): 3.0
- MDE (verified, 80% power): 1.6
- Full-sample estimate: 0.72
- Full-sample SE: 0.42
- |Full estimate| > MDE? FALSE

## Overall Assessment
- Outcomes where verified subsample is powered: 0 / 5
- Outcomes where verified subsample is UNDERPOWERED: 5 / 5

### Interpretation
The verified lottery subsample (N=100) has insufficient power to detect
effects of the magnitude found in the full sample. The null results in the
verified subsample are UNINFORMATIVE -- they cannot distinguish between
'truly zero effect' and 'effect exists but sample is too small to detect it'.

This should be acknowledged transparently in the paper.
