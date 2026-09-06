# Tournament Performance Model Rebuild Summary
Generated: 2026-03-26 10:04:38.930926

## Audit Items Addressed
- Item 1: Final-round qualifying losers only (max Q round per tournament)
- Item 2: LL-granting events only (n_ll_slots > 0)
- Item 3-4: Four separate models (GS-ATP, GS-WTA, nonGS-ATP, nonGS-WTA)
- Item 5: Truncation at next LL opportunity (not any qualifying loss)
- Item 6: Bernoulli convolution P_i^{LL} only (no crude fallback)
- Item 7: Counterfactual formula includes rho*v_hat term
- Item 8: 200-rep player-level block bootstrap
- Item 9a: Renamed to tournament performance probability
- Item 9b: Expected ranking points E[RP|d] computed
- Item 10: Points-weighted logit robustness
- Item 11: H2H encounter count (n_h2h) in win model
- Item 12: Four-panel calendar-horizon figure (fig_tournament_4panel.pdf)
- Item 13: fig_dynamic_effects.pdf NOT regenerated
- Item 14: Enhanced win model with Z_ie^pre; P_i^{LL} imposed directly
- Item 15: Estimation sample = LL candidates only (documented)
- Item 16: GS 2006-2024, nonGS-ATP 2007+, nonGS-WTA 2009+
- Item 17: No first-stage probit (generalized residual computed analytically)
- Item 18: Four tables generated

## Sample Sizes

## Phase 1: Data Loading
- ATP main matches: 112056
- ATP qual matches: 223097
- WTA main matches: 96252
- WTA qual matches: 593885

## Phase 1D: Sample Construction
- GS-ATP events: 248 (LL: 132)
- GS-WTA events: 132 (LL: 54)
- nonGS-ATP events: 2666 (LL: 860)
- nonGS-WTA events: 2545 (LL: 755)

## Phase 2: Win Probability Models
- ATP win model N: 142026, AIC: 174298.3
- WTA win model N: 126220, AIC: 155561.6

## Phase 3: Bernoulli Convolution P_i^{LL}
- GS-ATP: N=172, mean P_ll=0.436
- GS-WTA: N=112, mean P_ll=0.375
- nonGS-ATP: N=2222, mean P_ll=0.239
- nonGS-WTA: N=2155, mean P_ll=0.223

## Phase 4: Match-Level Datasets
- GS-ATP: 2287 matches, 163 events
- GS-WTA: 1749 matches, 109 events
- nonGS-ATP: 19367 matches, 1580 events
- nonGS-WTA: 19926 matches, 1602 events

## Phase 5: CF-IV Estimation
- delta=0.1939 (SE=0.2603, p=0.4565), rho=-0.0639 (SE=0.1635, p=0.6960), N=2287
- delta=-0.2851 (SE=0.3643, p=0.4339), rho=0.2785 (SE=0.2233, p=0.2122), N=1749
- delta=-0.0733 (SE=0.0579, p=0.2058), rho=0.1145 (SE=0.0432, p=0.0080), N=19367
- delta=-0.0573 (SE=0.0553, p=0.3008), rho=-0.0182 (SE=0.0386, p=0.6366), N=19926

## Phase 6: Counterfactual Quantities
- DeltaP(match)=0.0394, DeltaE[W]=0.0603, DeltaE[RP]=1.0
- DeltaP(match)=-0.0564, DeltaE[W]=-0.0868, DeltaE[RP]=-1.5
- DeltaP(match)=-0.0148, DeltaE[W]=-0.0234, DeltaE[RP]=-0.4
- DeltaP(match)=-0.0116, DeltaE[W]=-0.0185, DeltaE[RP]=-0.3

## Phase 7: Bootstrap Inference
- GS-ATP: delta_SE=0.4790, success=200/200
- GS-WTA: delta_SE=0.6643, success=200/200
- nonGS-ATP: delta_SE=0.0897, success=200/200
- nonGS-WTA: delta_SE=0.0869, success=200/200


## Key Results

### GS-ATP
- delta (LL entry): 0.1939 (SE=0.4790)
- rho (endogeneity): -0.0639 (SE=0.1635)
- N matches: 2287, N events: 163
- DeltaP(match win): 0.0394
- DeltaE[W]: 0.0603
- DeltaE[RP]: 1.0

### GS-WTA
- delta (LL entry): -0.2851 (SE=0.6643)
- rho (endogeneity): 0.2785 (SE=0.2233)
- N matches: 1749, N events: 109
- DeltaP(match win): -0.0564
- DeltaE[W]: -0.0868
- DeltaE[RP]: -1.5

### nonGS-ATP
- delta (LL entry): -0.0733 (SE=0.0897)
- rho (endogeneity): 0.1145 (SE=0.0432)
- N matches: 19367, N events: 1580
- DeltaP(match win): -0.0148
- DeltaE[W]: -0.0234
- DeltaE[RP]: -0.4

### nonGS-WTA
- delta (LL entry): -0.0573 (SE=0.0869)
- rho (endogeneity): -0.0182 (SE=0.0386)
- N matches: 19926, N events: 1602
- DeltaP(match win): -0.0116
- DeltaE[W]: -0.0185
- DeltaE[RP]: -0.3

