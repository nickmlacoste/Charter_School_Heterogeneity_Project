# ==============================================================================
# cdp_table.R
# Summary table of the conditional_did_pretest() runs (cdp_pretest.R): one row
# per outcome, the p-value from each of the five random samples, the median,
# rejections at 5%, and the mean number of treated units per sample.
# Writes a bare booktabs tabular (INV-13): the float, caption and notes live
# in main.tex.
#
# Run from project root:  Rscript scripts/R/cdp_table.R
# ==============================================================================

library(here)
library(dplyr)

in_dir  <- here("quality_reports", "results", "conditional_pretrends")
out_dir <- here("paper", "tables", "DiDCF_analysis")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

files <- list.files(in_dir, pattern = "^cdp_(afgr|math|ela)_n2000_seed[0-9]+[.]rds$", full.names = TRUE)
runs  <- bind_rows(lapply(files, function(p) {
  z <- readRDS(p)
  tibble(outcome = z$outcome, seed = as.integer(sub(".*_seed([0-9]+)[.]rds$", "\\1", basename(p))),
         p = z$result$CvMpval, n_sampled = z$n_sampled, n_treated = sum(z$treated_by_cohort))
}))
stopifnot(nrow(runs) == 15, all(runs$n_sampled == 2000))

labels <- c(afgr = "Graduation (AFGR)", math = "Math", ela = "ELA")
fmt    <- function(x) formatC(x, format = "f", digits = 2)

rows <- vapply(names(labels), function(o) {
  r <- runs |> filter(outcome == o) |> arrange(seed)
  paste(c(labels[[o]], fmt(r$p), fmt(median(r$p)),
          sprintf("%d of %d", sum(r$p < 0.05), nrow(r)),
          formatC(mean(r$n_treated), format = "f", digits = 0)),
        collapse = " & ")
}, character(1))

tex <- c(
  "\\begin{tabular}{lcccccccc}",   # outcome + 5 samples + median + rejections + treated
  "\\toprule",
  " & \\multicolumn{5}{c}{$p$-value, by random sample} & & & \\\\",
  "\\cmidrule(lr){2-6}",
  "Outcome & (1) & (2) & (3) & (4) & (5) & Median & Reject at 5\\% & Treated \\\\",
  "\\midrule",
  paste0(rows, " \\\\"),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(tex, file.path(out_dir, "cdp_pretest_summary.tex"))
message("Wrote ", file.path(out_dir, "cdp_pretest_summary.tex"))
print(runs |> group_by(outcome) |> summarise(median_p = median(p), rejections = sum(p < 0.05),
                                             treated_mean = mean(n_treated), .groups = "drop"))
