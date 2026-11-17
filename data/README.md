# Data

The analysis data are not included in this repository. They will be made
publicly available at a later date (TBD).

The scripts expect these files:

| Path | Description |
|---|---|
| `data/cleaned/charter_afgr2.dta` | District-by-year panel: averaged freshman graduation rate (AFGR), 1995--2010, with charter enrollment share and district moderators |
| `data/cleaned/charter_seda.dta` | District-by-grade-by-year panel: SEDA math and ELA scores, grades 3--8, 2009--2018, with charter enrollment share and district moderators |
| `data/raw/policy3.dta` | State charter-school policy ratings (NAPCS 2014), merged on `statename` |
| `data/raw/finance.dta` | District school-finance moderators, merged on `district` |

Both analysis panels are built from the National Longitudinal School Database
(NLSD), which combines the Common Core of Data (CCD), the Stanford Education
Data Archive (SEDA), and Census SAIPE.
