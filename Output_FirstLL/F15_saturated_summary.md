GS strata (treated counts): n=0: 94, n=1: 33, n>=2: 59
NonGS strata (treated counts): n=0: 901, n=1: 528, n>=2: 440

Saturated model: first-LL effect and experience shifters
  beta_n0   = ATT for first-time (n=0) recipients
  gamma1    = shift for second LL (n=1); H0: gamma1 = 0
  gamma2    = shift for third-plus (n>=2); H0: gamma2 = 0
  GS     ATP  4w  : b_n0=   55.2 (p=0.003) | g1=    -6.7 (p=0.860) | g2=   -79.7 (p=0.014)
  GS     ATP  8w  : b_n0=   58.4 (p=0.002) | g1=    -5.6 (p=0.873) | g2=   -87.4 (p=0.011)
  GS     ATP  12w : b_n0=   69.1 (p=0.001) | g1=     3.7 (p=0.921) | g2=   -99.3 (p=0.007)
  GS     ATP  26w : b_n0=   61.4 (p=0.135) | g1=    55.4 (p=0.360) | g2=   -58.4 (p=0.317)
  GS     ATP  52w : b_n0=    4.7 (p=0.942) | g1=    70.4 (p=0.516) | g2=   -21.4 (p=0.814)
  NonGS  ATP  4w  : b_n0=    4.2 (p=0.613) | g1=    13.3 (p=0.227) | g2=     6.1 (p=0.565)
  NonGS  ATP  8w  : b_n0=    6.0 (p=0.531) | g1=    14.4 (p=0.293) | g2=     0.4 (p=0.973)
  NonGS  ATP  12w : b_n0=    1.0 (p=0.925) | g1=    18.6 (p=0.221) | g2=     0.5 (p=0.971)
  NonGS  ATP  26w : b_n0=   34.7 (p=0.015) | g1=     5.4 (p=0.821) | g2=   -27.6 (p=0.173)
  NonGS  ATP  52w : b_n0=   28.9 (p=0.313) | g1=   -14.1 (p=0.763) | g2=   -69.1 (p=0.092)
  GS     WTA  4w  : b_n0=   38.8 (p=0.160) | g1=   111.1 (p=0.079) | g2=   -61.4 (p=0.526)
  GS     WTA  8w  : b_n0=   54.8 (p=0.032) | g1=    59.3 (p=0.367) | g2=     6.0 (p=0.949)
  GS     WTA  12w : b_n0=   27.9 (p=0.259) | g1=   -29.6 (p=0.653) | g2=    20.0 (p=0.845)
  GS     WTA  26w : b_n0=    3.6 (p=0.934) | g1=   -71.7 (p=0.402) | g2=   128.7 (p=0.263)
  GS     WTA  52w : b_n0=   18.3 (p=0.836) | g1=  -108.3 (p=0.435) | g2=    82.7 (p=0.657)
  NonGS  WTA  4w  : b_n0=   23.0 (p=0.026) | g1=    -4.1 (p=0.765) | g2=    17.4 (p=0.203)
  NonGS  WTA  8w  : b_n0=   21.2 (p=0.049) | g1=    -3.6 (p=0.815) | g2=     4.8 (p=0.775)
  NonGS  WTA  12w : b_n0=   30.7 (p=0.015) | g1=   -18.1 (p=0.340) | g2=   -13.0 (p=0.494)
  NonGS  WTA  26w : b_n0=   22.9 (p=0.214) | g1=   -17.3 (p=0.609) | g2=   -67.8 (p=0.027)
  NonGS  WTA  52w : b_n0=   47.5 (p=0.226) | g1=   -51.2 (p=0.434) | g2=  -152.6 (p=0.025)

Joint Wald tests (H0: all experience shifters = 0):
  GS     ATP  points_change     : F=  1.20, df=10, p=0.2852 -> fail to reject
  NonGS  ATP  points_change     : F=  0.54, df=10, p=0.8605 -> fail to reject
  GS     WTA  points_change     : F=  1.21, df=10, p=0.2814 -> fail to reject
  NonGS  WTA  points_change     : F=  1.08, df=10, p=0.3737 -> fail to reject
  GS     ATP  n_main_draws      : F=  2.15, df=10, p=0.0187 -> REJECT: effects differ by LL experience
  NonGS  ATP  n_main_draws      : F=  1.59, df=10, p=0.1035 -> fail to reject
  GS     WTA  n_main_draws      : F=  0.88, df=10, p=0.5525 -> fail to reject
  NonGS  WTA  n_main_draws      : F=  0.66, df=10, p=0.7671 -> fail to reject
