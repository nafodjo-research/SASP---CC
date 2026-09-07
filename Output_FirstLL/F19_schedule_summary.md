Post-event schedule composition (26-week window):
  mean_opp_elo = average pre-match Elo of opponents faced
  Under the schedule-composition account, treated players should face
  WEAKER opponents (negative coefficient on mean_opp_elo).
  Pooled    ATP  mean_opp_elo  :   22.419 ( 15.858) p=0.161      [control mean  1691.49, N=101]
  Verified  ATP  mean_opp_elo  :   13.502 ( 20.239) p=0.508      [control mean  1677.98, N=57]
  Pooled    ATP  n_matches     :   -2.095 (  2.437) p=0.392      [control mean    29.71, N=101]
  Verified  ATP  n_matches     :   -3.049 (  3.160) p=0.339      [control mean    29.00, N=57]
  Pooled    ATP  win_rate      :   -0.017 (  0.031) p=0.592      [control mean     0.55, N=101]
  Verified  ATP  win_rate      :   -0.037 (  0.034) p=0.279      [control mean     0.53, N=57]
  Pooled    WTA  mean_opp_elo  :   22.929 ( 14.610) p=0.121      [control mean  1832.24, N=76]
  Verified  WTA  mean_opp_elo  :   15.877 ( 24.854) p=0.527      [control mean  1820.38, N=40]
  Pooled    WTA  n_matches     :   -3.070 (  2.962) p=0.303      [control mean    26.69, N=77]
  Verified  WTA  n_matches     :   -0.309 (  2.778) p=0.912      [control mean    30.67, N=40]
  Pooled    WTA  win_rate      :    0.037 (  0.030) p=0.231      [control mean     0.54, N=76]
  Verified  WTA  win_rate      :    0.037 (  0.053) p=0.491      [control mean     0.55, N=40]

Interpretation:
  ATP: opponent strength is unchanged (+22.4 Elo, p=0.161).
     Schedule composition does not account for an Elo gain here.
  WTA: opponent strength is unchanged (+22.9 Elo, p=0.121).
     Schedule composition does not account for an Elo gain here.
