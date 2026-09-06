# Correct First-Stage F-Statistic
Generated: 2026-03-25 10:01:34.016316

## ATP Non-GS IV First Stage
- N = 2672

### Method 1: t^2 from OLS first stage (full controls + year FE, clustered SE)
- peer_component coefficient: 0.8143
- Clustered SE: 0.0178
- F-statistic (t^2): 2090.8

### Method 2: fitstat() from feols IV regression
- F-statistic: 2071.9

### Method 3: Simpler controls (pre_rank_pts + pre_rank_pts_sq + player_age + year FE)
- F-statistic (t^2): 2114.6

## Resolution
- Previous text claimed F = 2,181
- Previous table showed F = 2,079
- Correct F (full spec, method 1): 2090.8
- Correct F (fitstat, method 2): 2071.9
- All methods yield F >> 10 (strong instrument)
