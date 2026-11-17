# ==============================================================================
# cdp_pretest.R
# did::conditional_did_pretest() -- the integrated-moment (Cramer-von Mises)
# test that conditional parallel trends holds uniformly in X in every
# pre-period -- run as the package designs it (one call, all cohorts,
# balanced panel, est_method = "dr") on a simple random sample of units.
#
# Why a sample: the function loops over every unit and re-runs all
# pre-period (g,t) cells, keeping a unit x cell influence function each time,
# so memory and time grow with units^2 x cells. The full AFGR panel needs
# ~90 GB. N_UNITS is chosen from a pilot run.
#
# Run from project root:
#   Rscript scripts/R/cdp_pretest.R [outcomes]
# The paper uses five samples: run once per seed with CDP_SEED <- 1, ..., 5
# (see run_all.R). Requires the saved DiDCF results (run DiDCF_analysis.R first).
# Pilot: set N_UNITS <- 500; BITERS <- 99; CDP_TAG <- "pilot" before sourcing.
# Resumable: an outcome whose result file exists is skipped.
# ==============================================================================

source(here::here("scripts", "R", "cdp_prep_panel.R"))

if (!exists("N_UNITS")) N_UNITS <- 2000
if (!exists("BITERS"))  BITERS  <- 999
if (!exists("CDP_SEED")) CDP_SEED <- 42
if (!exists("CDP_TAG"))  CDP_TAG  <- sprintf("n%d_seed%d", N_UNITS, CDP_SEED)
args     <- commandArgs(trailingOnly = TRUE)
outcomes <- if (exists("CDP_OUTCOMES")) CDP_OUTCOMES else if (length(args)) args else c("afgr", "math", "ela")
out_dir  <- here("quality_reports", "results", "conditional_pretrends")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

for (outcome_name in outcomes) {
  out_file <- file.path(out_dir, sprintf("cdp_%s_%s.rds", outcome_name, CDP_TAG))
  if (file.exists(out_file)) { message("skip (exists): ", basename(out_file)); next }
  message("\n==== ", outcome_name, " | ", CDP_TAG, " ====")
  p <- build_did_panel(outcome_name)
  d <- p$data

  # balanced panel (required by the pretest), then a simple random sample of units
  n_years  <- n_distinct(d$year)
  balanced <- d |> count(unit_num) |> filter(n == n_years) |> pull(unit_num)
  set.seed(CDP_SEED)
  pick <- sample(balanced, min(N_UNITS, length(balanced)))
  s    <- d |> filter(unit_num %in% pick)

  # a missing-value dummy or covariate can be constant in the sample's
  # never-treated units even when it isn't in the full panel
  ctrl <- s[s$first_treat == 0, ]
  vars <- p$vars[vapply(p$vars, function(v) isTRUE(stats::var(ctrl[[v]]) > 0), logical(1))]
  if (length(vars) < length(p$vars))
    message("  dropped in sample (no variation among never-treated): ", paste(setdiff(p$vars, vars), collapse = ", "))

  tr_units <- s |> filter(first_treat > 0) |> distinct(unit_num, first_treat)
  message(sprintf("  balanced units %d | sampled %d | treated %d across %d cohorts | covariates: %s",
                  length(balanced), length(pick), nrow(tr_units), n_distinct(tr_units$first_treat),
                  paste(vars, collapse = ", ")))
  print(table(tr_units$first_treat))

  cl <- length(OUTCOMES[[outcome_name]]$unit_vars) > 1   # SEDA: cluster district x grade units by district
  invisible(gc(reset = TRUE))
  t0  <- Sys.time()
  res <- conditional_did_pretest(
    yname = "outcome", tname = "year", idname = "unit_num", gname = "first_treat",
    xformla = reformulate(vars), data = s, panel = TRUE, allow_unbalanced_panel = FALSE,
    control_group = "nevertreated", weightsname = "weight", est_method = "dr",
    bstrap = TRUE, cband = FALSE, biters = BITERS,
    clustervars = if (cl) c("unit_num", "district") else NULL, cores = 1
  )
  secs    <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  peak_mb <- sum(gc()[, ncol(gc())])   # "max used" (Mb) since reset
  message(sprintf("  CvM = %.4g | p = %.4g | %.1f min | peak R memory %.0f MB",
                  res$CvM, res$CvMpval, secs / 60, peak_mb))

  saveRDS(list(result = res, outcome = outcome_name, tag = CDP_TAG, n_sampled = length(pick),
               n_balanced = length(balanced), treated_by_cohort = table(tr_units$first_treat),
               covariates = vars, seconds = secs, peak_mb = peak_mb),
          out_file)
}
message("Done.")
