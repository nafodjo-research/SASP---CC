# Sample Counts (v2 -- after critical fixes)
Generated: 2026-03-23 13:15:21.904641

## Grand Slam Lottery Sample

### Full GS lottery (2006+, events with >= 1 LL slot, top-4 ranked losers)
- ATP: N = 248 (treated = 132, control = 116)
- WTA: N = 132 (treated = 54, control = 78)
- Pooled: N = 380 (treated = 186, control = 194)

### First-LL-only GS lottery (2006+)
- ATP: N = 95 (treated = 52, control = 43)
- WTA: N = 83 (treated = 35, control = 48)
- Pooled: N = 178 (treated = 87, control = 91)

## Non-GS Selection Model (IV) Sample

- Full non-GS losers (events with >= 1 LL): N = 2708
- Non-GS losers with selection prob: N = 2708
- First-LL-only non-GS: N = 862
- First-LL-only IV estimation sample: N = 862 (treated = 225, control = 637)

## Event Filtering

- Total events with qualifying losers (all levels, all years): 1158
- Events with >= 1 LL spot awarded: 620
- Events with 0 LL spots (DROPPED): 538
- Observations dropped (zero-LL events): 2992

### GS 2006 filter
- GS qualifying losers pre-2006 (DROPPED): 696

## Selection Probability Coverage

- OLD (script 11): 4000 obs across 620 tournaments
- NEW (script 15): 2672 obs across 537 tournaments
- Gain: -1328 obs, -83 tournaments

## Instrument Details

- Instrument: peer_component = Pr(rank among losers <= c_t | i loses)
- Does NOT include own loss probability
- First-stage F-stat: 389.3
- First-stage coef: 0.9196

## Controls

- Pre-treatment ranking points (linear + quadratic)
- Player age
- Year fixed effects
- Clustered standard errors at player level
