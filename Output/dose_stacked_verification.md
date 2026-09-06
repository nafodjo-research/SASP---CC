# Stacked Dose Table Verification
Generated: 2026-03-27 11:57:49.43357

## ATP GS dose results at 26w (points_change):
- Base (0 wins): coef = 39.00 (SE = 28.51, p = 0.171)
- 2+ wins interaction: coef = 107.82 (SE = 59.44, p = 0.070)

## Expected values from text:
- Base null at 26w: 6.2 points (p > 0.50)
- Dose interaction for 2+ wins at 26w: +145.7 (p = 0.013)

## Interpretation:
The table is produced by script 30. The dose variable `md_matches_won` in the
stacked panel (script 30) uses the main draw matches won that was constructed
in the skeleton pipeline. Verify that this variable counts wins at the
LL-granting event correctly.

## Current table content:
\begin{tabular}{l c c c c c c}
\toprule
Horizon & $\hat{\beta}_h$ & SE & $\hat{\delta}_{1\text{win},h}$ & SE & $\hat{\delta}_{2+,h}$ & SE \\
\midrule
\multicolumn{7}{l}{\textit{Panel A: ATP GS}} \\
4w & 25.55$^{*}$ & (15.22) & 31.74 & (22.56) & -2.84 & (36.59) \\
8w & 24.57 & (16.10) & 35.52 & (23.71) & -0.48 & (37.11) \\
12w & 35.31$^{**}$ & (16.75) & 25.10 & (27.69) & -5.33 & (34.55) \\
26w & 39.00 & (28.51) & 15.88 & (39.44) & 107.82$^{*}$ & (59.44) \\
52w & -16.47 & (46.92) & 52.91 & (70.28) & 123.13 & (85.11) \\
\addlinespace
\multicolumn{7}{l}{\textit{Panel B: WTA GS}} \\
4w & 52.84$^{**}$ & (25.57) & 39.69 & (37.49) & -26.16 & (130.50) \\
8w & 56.55$^{**}$ & (27.71) & 59.98$^{*}$ & (34.81) & 70.86 & (83.16) \\
12w & 14.22 & (27.34) & 48.30 & (35.82) & 59.17 & (78.52) \\
26w & -20.50 & (41.64) & 56.26 & (53.37) & 170.75 & (164.11) \\
52w & -15.26 & (75.10) & 20.18 & (105.58) & 352.65 & (489.53) \\
\addlinespace
\midrule
\multicolumn{7}{l}{Event FE = Yes; Horizon FE = Yes} \\
\bottomrule
\end{tabular}
