# ==============================================================================
# run_all.R
# Runs the full replication in order. Run from the repository root:
#
#   Rscript run_all.R
#
# Each script runs in its own R process: the scripts share function and
# configuration names, so they must not be sourced into one session.
#
# Total run time is long (the DiDCF forests take several hours); see README.md.
# Set the steps you want below.
# ==============================================================================

RUN <- c(
  didcf         = TRUE,   # DiDCF_analysis.R: forests, event studies, heterogeneity (hours)
  dose_response = TRUE,   # DiDCF_dose_response_acrt.R: ACRT(w) curves (needs didcf)
  csgdd         = TRUE,   # CSGDD_analysis.R: linear CGS-DiD
  pretest       = TRUE    # cdp_pretest.R x 5 seeds + cdp_table.R (needs didcf)
)

if (!file.exists(".here")) stop("Run run_all.R from the repository root.")

rscript <- file.path(R.home("bin"), "Rscript")

#' Run R code in a fresh R process; stop on failure.
run_step <- function(label, code) {
  message("\n", strrep("=", 70), "\n", label, "\n", strrep("=", 70))
  t0 <- Sys.time()
  status <- system2(rscript, c("-e", shQuote(code)))
  if (status != 0) stop(label, " failed (exit status ", status, ")")
  message(sprintf("%s done in %s", label, format(round(Sys.time() - t0, 1))))
}

src <- function(script) sprintf("source(here::here('scripts', 'R', '%s'))", script)

if (RUN[["didcf"]])
  run_step("DiDCF_analysis.R", src("DiDCF_analysis.R"))

if (RUN[["dose_response"]])
  run_step("DiDCF_dose_response_acrt.R", src("DiDCF_dose_response_acrt.R"))

if (RUN[["csgdd"]])
  run_step("CSGDD_analysis.R", src("CSGDD_analysis.R"))

if (RUN[["pretest"]]) {
  for (seed in 1:5)
    run_step(sprintf("cdp_pretest.R (seed %d)", seed),
             sprintf("CDP_SEED <- %d; %s", seed, src("cdp_pretest.R")))
  run_step("cdp_table.R", src("cdp_table.R"))
}

message("\nReplication complete.")
