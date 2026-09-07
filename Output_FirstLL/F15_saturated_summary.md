GS strata (treated counts): n=0: 94, n=1: 33, n>=2: 59
NonGS strata (treated counts): n=0: 901, n=1: 528, n>=2: 440

Saturated model: first-LL effect and experience shifters
  beta_n0   = ATT for first-time (n=0) recipients
  gamma1    = shift for second LL (n=1); H0: gamma1 = 0
  gamma2    = shift for third-plus (n>=2); H0: gamma2 = 0
  GS     ATP  4w  : b_n0=   49.5 (p=0.003) | g1=    -2.5 (p=0.934) | g2=   -63.9 (p=0.011)
  GS     ATP  8w  : b_n0=   53.6 (p=0.002) | g1=    -0.1 (p=0.996) | g2=   -77.6 (p=0.004)
  GS     ATP  12w : b_n0=   70.7 (p=0.000) | g1=    -4.5 (p=0.871) | g2=  -102.4 (p=0.000)
  GS     ATP  26w : b_n0=  106.9 (p=0.001) | g1=   -27.9 (p=0.514) | g2=  -161.2 (p=0.000)
  GS     ATP  52w : b_n0=   49.2 (p=0.347) | g1=    53.2 (p=0.487) | g2=  -182.3 (p=0.001)
  NonGS  ATP  4w  : b_n0=    2.2 (p=0.773) | g1=    14.4 (p=0.111) | g2=    18.9 (p=0.053)
  NonGS  ATP  8w  : b_n0=    6.9 (p=0.442) | g1=     8.4 (p=0.458) | g2=     4.1 (p=0.713)
  NonGS  ATP  12w : b_n0=    4.3 (p=0.663) | g1=    11.5 (p=0.378) | g2=    -7.5 (p=0.538)
  NonGS  ATP  26w : b_n0=   40.0 (p=0.005) | g1=    -6.9 (p=0.746) | g2=   -43.7 (p=0.020)
  NonGS  ATP  52w : b_n0=   34.9 (p=0.187) | g1=   -21.4 (p=0.586) | g2=   -99.5 (p=0.003)
  GS     WTA  4w  : b_n0=   40.9 (p=0.138) | g1=    81.7 (p=0.133) | g2=    19.9 (p=0.837)
  GS     WTA  8w  : b_n0=   56.4 (p=0.026) | g1=    37.6 (p=0.476) | g2=    72.3 (p=0.476)
  GS     WTA  12w : b_n0=   32.9 (p=0.199) | g1=   -33.0 (p=0.507) | g2=    26.0 (p=0.801)
  GS     WTA  26w : b_n0=   33.0 (p=0.412) | g1=  -130.0 (p=0.032) | g2=    13.5 (p=0.905)
  GS     WTA  52w : b_n0=   76.4 (p=0.366) | g1=  -237.7 (p=0.023) | g2=  -119.2 (p=0.525)
  NonGS  WTA  4w  : b_n0=   24.2 (p=0.019) | g1=    -5.6 (p=0.642) | g2=    16.0 (p=0.260)
  NonGS  WTA  8w  : b_n0=   22.8 (p=0.036) | g1=    -6.9 (p=0.627) | g2=     2.3 (p=0.895)
  NonGS  WTA  12w : b_n0=   33.3 (p=0.009) | g1=   -18.6 (p=0.282) | g2=   -24.3 (p=0.183)
  NonGS  WTA  26w : b_n0=   24.9 (p=0.190) | g1=   -13.0 (p=0.677) | g2=   -83.0 (p=0.002)
  NonGS  WTA  52w : b_n0=   49.5 (p=0.214) | g1=   -48.2 (p=0.432) | g2=  -168.9 (p=0.004)

Joint Wald tests (H0: all experience shifters = 0):
  GS     ATP  points_change     : F=  2.77, df=10, p=0.0022 -> REJECT: effects differ by LL experience
  NonGS  ATP  points_change     : F=  1.92, df=10, p=0.0382 -> REJECT: effects differ by LL experience
  GS     WTA  points_change     : F=  1.35, df=10, p=0.1987 -> fail to reject
  NonGS  WTA  points_change     : F=  1.90, df=10, p=0.0401 -> REJECT: effects differ by LL experience
  GS     ATP  n_main_draws      : F=  2.07, df=10, p=0.0247 -> REJECT: effects differ by LL experience
  NonGS  ATP  n_main_draws      : F=  3.65, df=10, p=0.0001 -> REJECT: effects differ by LL experience
  GS     WTA  n_main_draws      : F=  0.87, df=10, p=0.5600 -> fail to reject
  NonGS  WTA  n_main_draws      : F=  2.45, df=10, p=0.0064 -> REJECT: effects differ by LL experience
