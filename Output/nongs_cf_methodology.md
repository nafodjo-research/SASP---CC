# Control Function Approach for Non-Grand Slam Tournament Performance Model

## Overview

Non-Grand Slam (non-GS) events assign Lucky Loser (LL) spots based on ranking
among qualifying-round losers, not by lottery. This creates potential selection
bias: higher-ranked qualifying losers are more likely to receive LL entry AND
may differ systematically in ability. We address this using a control function
(CF) approach based on peer qualifying match outcomes as an instrument.

## Stage 1: Treatment Probability via Bernoulli Convolution

For each qualifying loser i at event e, we compute the probability of receiving
an LL spot, P_i^{LL}, using the Bernoulli convolution method:

1. Identify all qualifying matches whose outcomes affect LL ordering
2. Each peer match j has probability p_j of the higher-ranked player winning
   (estimated from historical data / Elo ratings)
3. The LL assignment depends on the joint realization of all peer matches
4. P_i^{LL} = sum over all outcome combinations that result in player i
   receiving an LL spot, weighted by the probability of each combination

This is a Bernoulli convolution because each peer match is an independent
Bernoulli trial, and P_i^{LL} is a function of the sum/ordering of these trials.

## Stage 2: Generalized Residual (Control Function)

The generalized residual from the probit first stage is:

  v_i = D_i * phi(Phi^{-1}(P_i^{LL})) / P_i^{LL}
       - (1 - D_i) * phi(Phi^{-1}(P_i^{LL})) / (1 - P_i^{LL})

where:
  - D_i = 1 if player i received LL entry, 0 otherwise
  - phi() is the standard normal PDF
  - Phi^{-1}() is the standard normal quantile function (probit link inverse)
  - P_i^{LL} is the treatment probability from Stage 1

This is the inverse Mills ratio generalized to binary outcomes.

## Stage 3: Second-Stage Logit

### Pooled specification (no dose):

  logit(P(won_{imt} = 1)) = delta * D_i + rho * v_i + X_{imt}' * beta + Z_i' * gamma

### Dose-response specification (centered):

  logit(P(won_{imt} = 1)) = delta * D_i
    + delta_dose * D_i * (w_i - w_bar)
    + delta_dose2 * D_i * (w_i - w_bar)^2
    + rho * v_i
    + rho_dose * v_i * (w_i - w_bar)
    + rho_dose2 * v_i * (w_i - w_bar)^2
    + X_{imt}' * beta + Z_i' * gamma

where w_i = matches won at LL event and w_bar = mean(w_i | D_i = 1).

The v_hat dose interactions (rho_dose, rho_dose2) are included because the
selection correction may itself vary with dose: players who win more matches
at the LL event may have different unobserved ability profiles.

### Horizon-heterogeneous specification:

  logit(P(won_{imt} = 1)) = sum_h delta_h * D_i * 1(t <= h)
    + sum_h rho_h * v_i * 1(t <= h)
    + X_{imt}' * beta + Z_i' * gamma

where h indexes horizon windows (4w, 8w, 12w, 26w, 52w) and
v_{ih} = v_i * 1(t <= h) allows the selection correction to vary by horizon.

## Interpretation

- delta: causal effect of LL entry on match win probability (at mean dose,
  in the centered specification)
- rho: coefficient on the control function; tests for endogeneity of LL entry.
  If rho is statistically significant, naive logit without CF would be biased.
- delta_dose, delta_dose2: how the LL effect varies with tournament performance
  (matches won). Positive delta_dose suggests compound returns to winning.

## Inference

Because v_hat is a generated regressor (estimated in Stage 1), standard errors
from the second-stage logit are invalid. We use a player-level block bootstrap:

1. Resample players with replacement (preserving all events per player)
2. Recompute v_hat for the bootstrap sample (re-derive from P_i^{LL})
3. Re-estimate the second-stage logit
4. Repeat B = 200 times
5. Bootstrap standard errors = SD of bootstrap coefficient distribution

The bootstrap is at the player level (not event or match level) because:
- Multiple events per player create within-player correlation
- Multiple matches per event create within-event correlation
- Player-level resampling respects both clustering structures

## Key Assumption: Exclusion Restriction

The instrument (peer qualifying match outcomes) satisfies the exclusion
restriction if: conditional on player i's own ability (proxied by Elo, ranking,
age, etc.), the outcomes of other qualifying matches at the same event affect
player i's future career only through whether player i receives the LL spot.

This is plausible because: (a) peer match outcomes are determined by other
players' performance, not by player i; (b) conditional on player i's observable
characteristics, there is no direct channel from peer outcomes to player i's
future match results.
