# ==============================================================================
# cdp_prep_panel.R
# Shared setup for cdp_pretest.R (conditional parallel-trends pre-test).
#
# Loads Sections 1-7 of scripts/R/DiDCF_analysis.R (config + functions; its
# main loop is not run) and defines build_did_panel(), which returns the
# binary treated / never-treated panel `did` needs, with the outcome's top-5
# non-charter moderators as time-invariant covariates.
#
# Requires the saved DiDCF results (quality_reports/results/didcf_results_<outcome>.rds)
# for the variable-importance ranking, so run DiDCF_analysis.R first.
# ==============================================================================

library(here)
library(did)

main_src <- readLines(here("scripts", "R", "DiDCF_analysis.R"))
main_end <- grep("^# SECTION 8: MAIN EXECUTION PIPELINE", main_src) - 2
eval(parse(text = main_src[seq_len(main_end)]))   # libraries, config, functions

# Never used as did covariates:
#   charter-sector finance -- ~0 for never-treated, so they separate the arms
#     almost perfectly and break the DR propensity model
#   baseline_y -- the lagged outcome Y_{g-2}: cohort-relative, so not a fixed
#     covariate did's xformla can take, and conditioning on a pre-period
#     outcome inside a pre-trend test is mechanical (mean reversion). It is
#     also not a column of the moderator matrix.
DID_EXCLUDE <- c("charter_exp", "w_pp_exp_percdiff", "baseline_y")
N_COVARIATES <- 5

#' Top-n moderators by mean VIF at event_time == 1 in the saved DiDCF results,
#' restricted to columns of `mod_matrix` and excluding DID_EXCLUDE.
top_did_covariates <- function(outcome_name, mod_matrix, n = N_COVARIATES) {
  vif <- readRDS(here("quality_reports", "results",
                      paste0("didcf_results_", outcome_name, ".rds")))$results$vif
  ranked <- vif |>
    filter(event_time == 1, !is.na(importance)) |>
    group_by(variable) |>
    summarise(importance = mean(importance), .groups = "drop") |>
    arrange(desc(importance)) |>
    pull(variable)
  head(setdiff(intersect(ranked, names(mod_matrix)), DID_EXCLUDE), n)
}

#' Long panel for did: one row per (unit, year) with a non-missing outcome.
#' first_treat = cohort year (0 = never treated, did's convention);
#' unit_num = numeric id (SEDA units are district x grade); district is the
#' cluster. Covariate NAs go through add_missing_indicators(); covariates with
#' no variation among never-treated are dropped (DRDID's outcome regression is
#' fit on them and stops on aliased terms).
#' Returns list(data, vars, xformla, covariates_requested).
build_did_panel <- function(outcome_name) {
  cfg <- OUTCOMES[[outcome_name]]
  df  <- assign_cohorts(load_outcome_data(cfg))
  mm  <- build_moderator_matrix(df, MOD_VARS)
  top <- top_did_covariates(outcome_name, mm)

  panel <- df |>
    filter(!is.na(outcome), !is.na(weight)) |>
    transmute(unit_id, district = as.numeric(district), year, outcome, weight,
              first_treat = ifelse(is.finite(cohort), cohort, 0)) |>
    mutate(unit_num = as.integer(factor(unit_id))) |>
    inner_join(mm |> select(unit_id, all_of(top)), by = "unit_id")

  mi   <- add_missing_indicators(panel, top)
  ctrl <- mi$data[mi$data$first_treat == 0, ]
  flat <- mi$vars[vapply(mi$vars, function(v) isTRUE(stats::var(ctrl[[v]]) == 0), logical(1))]
  if (length(flat) > 0) message("  dropping covariates constant among never-treated: ", paste(flat, collapse = ", "))
  vars <- setdiff(mi$vars, flat)

  list(data = as.data.frame(mi$data), vars = vars,
       xformla = reformulate(vars), covariates_requested = top)
}
