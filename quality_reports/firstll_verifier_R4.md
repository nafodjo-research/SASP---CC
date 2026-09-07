# Verification Report - FirstLL R4 - Standard Mode

**Overall: PASS (9/9 checks)**

## Check Results

| # | Check | Status | Details |
|---|-------|--------|---------|
| 1 | End-to-end LaTeX compilation | PASS | 3 xelatex passes + bibtex; 62-page PDF, no errors |
| 2 | Zero undefined references | PASS | 0 undefined refs/citations, 0 LaTeX warnings |
| 3 | Zero overfull hboxes > 10 pt | PASS | 3 overfull hboxes present, max 6.383 pt (all < 10 pt) |
| 4 | \input{} table paths resolve | PASS | 26 unique paths, all exist in `Tables_FirstLL/` |
| 5 | \includegraphics figure paths resolve | PASS | 2 figures, both exist in `Figures_FirstLL/` |
| 6 | Citations resolve to Bibliography_base.bib | PASS | 21 unique keys, all present in .bib (56 entries total) |
| 7 | Prose/table numeric cross-checks | PASS | All 4 spot checks match |
| 8 | F04/F11/F14/F14c/F14d scripts | PASS | All 5 exited with code 0 |
| 9 | Page count / errors summary | PASS | 62 pages, 0 undefined, 0 file-not-found, 0 pkg errors |

## Content Cross-Checks (Check 7)

- **Sec 7.3 "35 verified ATP events yielding N=86"** matches `table_verified_firstll.tex` header.
- **Abstract ATP 26w p=0.050 / WTA 8w p=0.005** confirmed by re-running F14c (identical p-values reproduced).
- **Sec 7.2 ratios/gaps** all match `table_mechanical_decomposition.tex`: GS-ATP ratios 2.17/2.19/2.07; GS-ATP gaps ~75; NonGS-ATP 1.6-2.1; GS-WTA <1 at short horizons; NonGS-WTA rising with horizon.
- **Intro δ = -0.141 (p=0.17)** matches Appendix `tab:delta_all` GS-ATP cell (-0.141, SE 0.103 → z=1.37, p≈0.17).

## Script Runs (all exit 0)

- F04: 16 tables + 4 figures written; δ_GS_ATP = -0.1414
- F11: 6 referee-fix diagnostics ran clean
- F14: ATP verified N=86 (35 events), WTA N=47 (17 events)
- F14c: Reproduces headline verified-subsample p-values exactly
- F14d: News verification of 23 ambiguous rank-1 entries

## Log Diagnostics
- Pages: 62
- Undefined references: 0
- Overfull hboxes: 3 (all < 10 pt; max 6.383 pt)
- File-not-found errors: 0

**FINAL VERDICT: PASS**
