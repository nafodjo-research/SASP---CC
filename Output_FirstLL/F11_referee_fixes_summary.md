GS uniform P_i: mean=0.636 range=[0.25,0.999]
v_hat_gs: mean=-0.451 range=[-3.367,1.271]

CF validation: rho values (should be near zero):
  Within-event joint F (ATP): F=2.21 p=0.046
  Within-event joint F (WTA): F=0.9 p=0.516
Pre-treatment columns available: 
Available points/ranking columns: points_t0, points_t4, points_t8, points_t12, points_t26, points_t52, points_change_4w, points_change_8w, points_change_12w, points_change_26w, points_change_52w, pre_rank_pts, pre_rank_pts_sq, pre_rank_pts_s, pre_rank_pts_sq_s
NOTE: No pre-treatment dynamic outcomes in skeleton. Plotting post-only with balance reference.

BH q-values across all primary outcomes:
  Family size: 80 tests. Survive q<0.05 (joint BH): 23
  GS ATP n=0: N=120 (T=61, C=59) across 55 events
    Treated mean pts: 482.7  Control mean pts: 447.2
  GS WTA n=0: N=82 (T=33, C=49) across 32 events
    Treated mean pts: 525.7  Control mean pts: 420.8
  GS ATP n=1: N=48 (T=21, C=27) across 36 events
    Treated mean pts: 500.7  Control mean pts: 493.5
  GS WTA n=1: N=31 (T=12, C=19) across 21 events
    Treated mean pts: 597.2  Control mean pts: 558.4
  GS ATP n=2+: N=80 (T=50, C=30) across 44 events
    Treated mean pts: 500.7  Control mean pts: 489.7
  GS WTA n=2+: N=19 (T=9, C=10) across 16 events
    Treated mean pts: 634.7  Control mean pts: 589.9

DIAGNOSIS: n=1 GS groups are very small (ATP: ~48, WTA: ~31).
With event FE absorbing substantial variation and only 21/12 treated,
estimates are unstable. Report with explicit small-sample caveat.
  NonGS-ATP mean n_main_draws_4w:
    Treated: 0.72
    Control: 0.26
  The focal event main draw appearance is in the IMMEDIATE model, not dynamic.
  Treated players are occupied at the focal event during weeks 1-2,
  reducing their ability to enter OTHER events in the 4-week window.
  By 12w+ the effect reverses as ranking gains open new doors.
