Treated-share weights by stratum:
  GS ATP n=0: w=0.462 (61 treated)
  GS ATP n=1: w=0.159 (21 treated)
  GS ATP n=2+: w=0.379 (50 treated)
  GS WTA n=0: w=0.611 (33 treated)
  GS WTA n=1: w=0.222 (12 treated)
  GS WTA n=2+: w=0.167 (9 treated)
  NonGS ATP n=0: w=0.461 (403 treated)
  NonGS ATP n=1: w=0.257 (225 treated)
  NonGS ATP n=2+: w=0.281 (246 treated)
  NonGS WTA n=0: w=0.501 (378 treated)
  NonGS WTA n=1: w=0.242 (183 treated)
  NonGS WTA n=2+: w=0.257 (194 treated)

Decomposition (points_change only, illustrative):
  GS-ATP 4w: pool=31.9  wavg=-43.5  gap=75.5  b0=69.3  ratio=2.17
  GS-ATP 8w: pool=31.3  wavg=-43.7  gap=75.0  b0=68.8  ratio=2.19
  GS-ATP 12w: pool=38.5  wavg=-36.1  gap=74.6  b0=79.8  ratio=2.07
  GS-ATP 26w: pool=49.2  wavg=-17.0  gap=66.2  b0=70.4  ratio=1.43
  GS-ATP 52w: pool=4.3  wavg=-54.3  gap=58.6  b0=21.4  ratio=4.95
  GS-WTA 4w: pool=64.4  wavg=76.2  gap=-11.8  b0=36.1  ratio=0.56
  GS-WTA 8w: pool=78.7  wavg=78.7  gap=-0.0  b0=49.6  ratio=0.63
  GS-WTA 12w: pool=32.3  wavg=35.8  gap=-3.6  b0=21.5  ratio=0.67
  GS-WTA 26w: pool=6.3  wavg=7.5  gap=-1.2  b0=1.3  ratio=0.20
  GS-WTA 52w: pool=6.3  wavg=12.6  gap=-6.3  b0=20.6  ratio=3.28
  NonGS-ATP 4w: pool=12.6  wavg=15.4  gap=-2.8  b0=20.3  ratio=1.61
  NonGS-ATP 8w: pool=12.1  wavg=16.6  gap=-4.5  b0=19.7  ratio=1.63
  NonGS-ATP 12w: pool=6.6  wavg=12.7  gap=-6.1  b0=13.6  ratio=2.07
  NonGS-ATP 26w: pool=24.7  wavg=32.4  gap=-7.7  b0=44.7  ratio=1.81
  NonGS-ATP 52w: pool=-8.0  wavg=11.6  gap=-19.6  b0=37.8  ratio=-4.73
  NonGS-WTA 4w: pool=27.4  wavg=21.5  gap=6.0  b0=17.9  ratio=0.65
  NonGS-WTA 8w: pool=22.2  wavg=15.5  gap=6.7  b0=19.1  ratio=0.86
  NonGS-WTA 12w: pool=23.0  wavg=14.7  gap=8.3  b0=23.4  ratio=1.02
  NonGS-WTA 26w: pool=3.1  wavg=-6.4  gap=9.5  b0=20.7  ratio=6.72
  NonGS-WTA 52w: pool=4.7  wavg=-16.3  gap=21.0  b0=59.4  ratio=12.70

======================================================================
MECHANICAL DECOMPOSITION SUMMARY
======================================================================
Interpretation:
  beta_pool  = pooled coefficient on full sample
  wavg       = w_0*beta_{n=0} + w_1*beta_{n=1} + w_{2+}*beta_{n>=2}
  gap        = pool - wavg (small: pool is stratum-weighted mean)
  beta_{n=0} = first-time-only coefficient
  ratio      = beta_{n=0} / beta_pool (paper claim: ~2x)
