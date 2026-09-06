# Tournament GS Fix Summary
Generated: 2026-03-26 10:17:43.983007

## FIX 1: GS Estimation (no control function, no bootstrap)
At Grand Slams, LL entry is by random lottery. Therefore:
- No generalized residual (v_hat) needed
- No bootstrap needed -- using clustered SEs at player level
- Simple logit: won ~ ll_entry + X_ijm + Z_ie_pre

## FIX 1: GS Re-estimation (no CF, clustered SEs)
- GS-ATP: delta=0.0995 (SE=0.1568, p=0.5258), N=2287
- GS-WTA: delta=0.1477 (SE=0.1698, p=0.3845), N=1749

## FIX 1: GS Counterfactuals (delta-method SEs)
- GS-ATP: DeltaP=0.0202 (SE=0.0320), DeltaEW=0.0309 (SE=0.0493), DeltaERP=0.5 (SE=0.8)
- GS-WTA: DeltaP=0.0295 (SE=0.0342), DeltaEW=0.0455 (SE=0.0533), DeltaERP=0.8 (SE=0.9)

## FIX 2: Stars and SEs on Counterfactual Quantities
All four tables now show:
- Point estimate with significance stars (*, **, ***)
- Standard error in parentheses
- GS tables: delta-method SEs from logit vcov
- Non-GS tables: bootstrap SEs (from script 22)

