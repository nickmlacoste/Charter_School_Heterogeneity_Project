# Replication Code: Semiparametric Continuous Difference-in-Differences with Causal Forests and an Application to U.S. Charter Schools

Feng Chen, Douglas N. Harris, and Nicholas Lacoste

This repository contains the code that produces the estimates, tables, and
figures in the paper and its Supplemental Appendix. It is a working version of
the replication package and will be updated as the paper is revised.

## Data availability

The data are **not included** in this repository. They will be made publicly
available at a later date (TBD). See [`data/README.md`](data/README.md) for the
files the scripts expect and where they come from.

## Software

- R 4.6.1. Every script loads its packages at the top. Versions used:

  | Package | Version | Package | Version |
  |---|---|---|---|
  | grf | 2.6.1 | did | 2.5.1 |
  | policytree | 1.2.5 | splines2 | 0.5.4 |
  | dplyr | 1.2.1 | tidyr | 1.3.2 |
  | purrr | 1.2.2 | ggplot2 | 4.0.3 |
  | haven | 2.5.5 | here | 1.0.2 |
  | Matrix | 1.7.5 | knitr | 1.51 |
  | modelsummary | 2.6.0 | usmap | 1.0.0 |
  | usmapdata | 1.1.0 | sf | 1.1.3 |

- Paths are relative to the repository root via `here::here()`. The empty
  `.here` file marks the root, so run everything from there.

## Running the code

```r
Rscript run_all.R
```

`run_all.R` runs the scripts below in order, each in a fresh R process.
Switches at its top run individual steps.

| Order | Script | What it does | Run time / resources |
|---|---|---|---|
| 1 | `scripts/R/DiDCF_analysis.R` | Staggered, continuous-dose DiDCF: one `grf::lm_forest` per cohort x calendar-year cell (binary placebo forests in pre-treatment cells), event studies with multiplier-bootstrap inference, variable importance, differences-in-means, best linear predictors, quartile and k-means event studies, policy trees, and the trend-on-dose and math-vs-ELA exhibits | Several hours on a multi-core machine; saved forests take ~32 GB in `models/` |
| 2 | `scripts/R/DiDCF_dose_response_acrt.R` | ACRT(w) and ATT(w) dose-response curves (B-spline estimator of Callaway, Goodman-Bacon, and Sant'Anna) | Minutes; needs step 1 |
| 3 | `scripts/R/CSGDD_analysis.R` | Linear two-stage CGS-DiD with influence-function standard errors (including the first stage), clustered by district | Minutes |
| 4 | `scripts/R/cdp_pretest.R` (seeds 1--5), `scripts/R/cdp_table.R` | Conditional parallel-trends pre-test (`did::conditional_did_pretest`) on five random samples of 2,000 balanced units per outcome; summary table | ~10 min per math/ELA sample, ~20--35 min per AFGR sample, ~10 GB RAM; needs step 1 |

`scripts/R/cdp_prep_panel.R` is a helper sourced by `cdp_pretest.R`.

`DiDCF_analysis.R` reads saved forests instead of re-fitting them when
`RUN_ESTIMATION <- FALSE` and `LOAD_MODELS <- TRUE` are set before it is
sourced; `OUTCOMES_TO_RUN` selects outcomes. Random seeds are fixed in each
script.

## Outputs

The scripts write to:

- `paper/figures/DiDCF_analysis/`, `paper/tables/DiDCF_analysis/`
- `paper/figures/CSGDD_analysis/`, `paper/tables/CSGDD_analysis/`
- `quality_reports/results/` (saved estimates as `.rds`)
- `models/DiDCF_analysis/` (fitted forests)

Tables are written as bare LaTeX `tabular` environments; captions and notes are
in the manuscript.

## Exhibits

| Exhibit | Script | Output file(s) |
|---|---|---|
| Table 1: CGS-DiD ATT and ACRT (math, ELA) | `CSGDD_analysis.R` | `csgdd_att_acrt.tex` (specs `no_moderators`, `moderators`) |
| Figure 1: DiDCF CATT distributions | `DiDCF_analysis.R` | `didcf_catt_dist_{math,ela}.pdf` |
| Figure 2: variable importance | `DiDCF_analysis.R` | `didcf_vif_{math,ela}.pdf` |
| Table 2: differences-in-means | `DiDCF_analysis.R` | `didcf_dim_{math,ela}.tex` |
| Table 3: best linear predictors | `DiDCF_analysis.R` | `didcf_blp_{math,ela}.tex` |
| Figure 3: quartile event studies (math) | `DiDCF_analysis.R` | `didcf_event_study_math_by_*.pdf` |
| Figure 4: policy trees (math) | `DiDCF_analysis.R` | `didcf_policytree_math_d{2,3}.pdf` |
| Figure 5: ACRT(w), math and ELA | `DiDCF_dose_response_acrt.R` | `didcf_acrt_dose_{math,ela}.pdf` |
| Table A1: k-means neighborhood profiles | `DiDCF_analysis.R` | `didcf_neighborhood_profile_vifweighted_{math,ela}.tex` |
| Supplement: conditional parallel-trends pre-test | `cdp_pretest.R`, `cdp_table.R` | `cdp_pretest_summary.tex` |
| Supplement: linear trends vs. dose | `DiDCF_analysis.R` (Section 9) | `trend_dose_regression.tex` |
| Supplement: AFGR results | `DiDCF_analysis.R`, `CSGDD_analysis.R`, `DiDCF_dose_response_acrt.R` | `*_afgr.*`, `csgdd_att_acrt_afgr.tex`, `csgdd_heterogeneity_top10.tex` |
| Supplement: math vs. ELA CATT correlation | `DiDCF_analysis.R` (Section 10) | `didcf_math_ela_catt_scatter.pdf` |
| Supplement: k-means, CACRT, and dose-group event studies; ELA figures | `DiDCF_analysis.R` | `didcf_neighborhood_event_study_vifweighted_*.pdf`, `didcf_event_study_tau_*_by_*.pdf`, `es_by_dose_*.pdf`, `*_ela_*.pdf` |
| Supplement: CGS-DiD heterogeneity (math, ELA) | `CSGDD_analysis.R` | `csgdd_heterogeneity.tex` |
| Supplement: descriptive statistics | *Not yet included* | -- |

The code for the descriptive-statistics table will be added in a later update.
