# Correct p_{itm} Specification
Generated: 2026-03-27 11:57:55.939444

## Match-level win probability model

The tournament performance model estimates:

```
P(i wins match m at tournament t) = Lambda(X_{ijm} beta + Z^pre_i gamma + delta * LL_i + rho * v_hat_i)
```

where Lambda() is the logistic CDF.

### X_{ijm} (match-specific pairwise variables):
1. log_rank_ratio = log(opp_rank / focal_rank)
2. log_rank_ratio_sq = log_rank_ratio^2
3. rank_diff = opp_rank - focal_rank
4. same_ioc = 1 if same country
5. is_clay = 1 if clay surface
6. is_grass = 1 if grass surface
7. age_diff = focal_age - opp_age
8. height_diff = focal_ht - opp_ht
9. hand_mismatch = 1 if different handedness
10. h2h_smoothed = smoothed head-to-head win rate
11. n_h2h = number of prior H2H meetings
12. opp_elo = opponent Elo at match time

### Z^pre_i (focal player pre-treatment characteristics):
1. pre_rank_pts = ranking points before qualifying loss
2. pre_elo = Elo rating before qualifying loss
3. player_age_at_event = age at the LL-granting event

Note: In the tournament model (scripts 24, 28), Z^pre enters WITHOUT
quadratic terms (pre_rank_pts_sq, pre_elo_sq) and WITHOUT the 4 LL
history variables. This is because:
- The quadratics are absorbed by the match-level controls (rank ratios)
- had_prior_ll is already in the model (or dropped in first-LL sample)
- The full LL history is player-level and partially collinear with player_age

### Treatment variable:
- delta: LL_i (got_ll indicator)

### Control function (non-GS only):
- rho: v_hat_i (generalized residual from selection equation)

## Formal equation for the paper:

P(i wins match m | X_{ijm}, Z^{pre}_i, D_i) = Lambda(
  beta_1 * Delta_Elo + beta_2 * Delta_SurfElo + beta_3 * H2H
  + beta_4 * n_H2H + beta_5 * Delta_Age + beta_6 * BO5
  + beta_7 * Surface + beta_8 * TourneyLevel
  + beta_9 * Delta_Height + beta_10 * Handedness
  + gamma_1 * pre_rank_pts_i + gamma_2 * pre_elo_i + gamma_3 * age_i
  + delta * LL_i + rho * v_hat_i
)

Note: The Elo variables in X_{ijm} already include the focal player's Elo
difference with the opponent, so pre_elo in Z^pre captures LEVEL effects
beyond what Elo differences capture.
