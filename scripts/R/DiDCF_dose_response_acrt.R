# ==============================================================================
# DiDCF_dose_response_acrt.R
#
# ACRT(d) / ATT(d) dose-response curves, replicating Callaway, Goodman-Bacon &
# Sant'Anna's `contdid` package (github.com/bcallaway11/contdid) as exactly as
# possible without depending on the `pte`/`ptetools` estimation framework
# itself. Read directly from the package source (not just the paper) --
# `contdid::cont_did_acrt()` (R/cont_did.R) and the cross-(g,t) aggregation in
# `ptetools::process_dose_gt()` / `ptetools::overall_weights()`
# (R/process_dose_gt.R, R/pte_aggte.R).
#
# STANDALONE by design (per instruction): sources DiDCF_analysis.R only for
# its data-prep helpers (load_outcome_data, assign_cohorts, build_cell_data,
# OUTCOMES config) with OUTCOMES_TO_RUN <- character(0) so its own main loop
# never runs. No causal_forest()/lm_forest() call anywhere in this script --
# every fit here is a per-cell B-spline lm(), which is why this runs in
# seconds, not hours: the slow part of the rest of the pipeline is
# forest training/loading and grf's honest variance estimation, neither of
# which this needs.
#
# ------------------------------------------------------------------------------
# WHAT IS AN EXACT MATCH TO contdid, verified against its source (not the
# paper's prose):
# ------------------------------------------------------------------------------
#   - Per-cell dose-response curve: cont_did_acrt() (cont_did.R:381-489).
#     lm(dy ~ bSpline(dose[dose>0], degree, knots)), ATT(d) = fitted curve
#     minus the control arm's mean dy, ACRT(d) = derivative of the fitted
#     curve (dbs() @ coefficients), both evaluated on a dose grid.
#   - Defaults: degree=3, num_knots=0 (cont_did()'s own function defaults --
#     "the spline is global, i.e. amounts to fitting a global polynomial").
#   - Cross-(cohort, event-time) weighting: EXACT formula from
#     overall_weights() (pte_aggte.R:404-479) -- phi_g (cohort's share of the
#     ever-treated population, by unit count) split EQUALLY across however
#     many post-treatment event-times that cohort contributes:
#         weight(g, h) = phi_g / n_post_periods(g)
#     Verified line-by-line: `g_weight[g] <- this_pg / sum(is_this_g)`,
#     `out_weight <- g_weight[match(group,glist)] * keeper`, weights sum to 1.
#   - Plot spec: geom_ribbon(est +/- crit*se) + geom_line, x="Dose",
#     y="ACRT(d)"/"ATT(d)", theme_bw() (ptetools/ggpte.R:251-286,
#     autoplot.dose_obj()).
#
# ------------------------------------------------------------------------------
# WHAT IS A DELIBERATE, DISCLOSED SIMPLIFICATION (flagged to the user, not
# silently substituted):
# ------------------------------------------------------------------------------
#   - Standard errors / confidence bands. contdid gets these from a multiplier
#     bootstrap over per-unit influence functions threaded through the whole
#     pte() machinery (mboot2(), getSE(), crit_val_checks() in
#     process_dose_gt.R) -- full reimplementation of that machinery (and its
#     UNIFORM/simultaneous confidence band, not just pointwise) is a much
#     larger undertaking than the point-estimate curve itself and was traded
#     off against the "fast, no hours-long run" requirement. This script uses
#     a per-cell delta-method SE off each B-spline lm()'s own coefficient
#     covariance, combined across cells the same weighted-average way as the
#     point estimate, with a POINTWISE 1.96 critical value (not a
#     bootstrapped simultaneous band). The curve's location/shape is exact;
#     the ribbon width is a reasonable analytic approximation, not their
#     exact SE.
#   - Control group: this project's build_cell_data() uses never-treated only
#     (matches every other DiDCF figure and the paper's own def:level_catt),
#     not contdid's own default control_group="notyettreated". Kept for
#     consistency with the rest of the pipeline rather than silently
#     switched -- flagged, not hidden.
#   - Dose grid. contdid's own default (setup_pte_cont.R:90-92) builds dvals
#     from `quantile(dose[dose>0], probs=seq(.1,.99,.01))` in the panel's
#     FIRST calendar period. That's only representative in their pipeline
#     because they require dose to be time-invariant per unit (collapsed to
#     its value in the unit's first-treated year, held fixed across all
#     periods) -- so period-1 dose already equals every ever-treated unit's
#     dose. This project's dose is deliberately time-varying (that's the
#     whole point of using DiDCF instead of contdid -- see the discussion
#     2026-09-09), so the same line of code picks up only whichever handful
#     of districts happened to already be treated in the literal first year
#     of OUR panel -- for AFGR that is 16 districts in 1995, 2 of which are
#     already at 100% share, which anchors the whole top of the grid to
#     those two outliers instead of the ~5,860 actual treated-cell
#     observations the curve is fit on. Fixed here: dvals is built from the
#     FULL realized positive-dose distribution across all treated cells,
#     evenly spaced from its min to its max, so the curve is drawn across
#     the entire observed 0-100% dose range. The sparse high-dose tail will
#     have wider confidence bands -- expected and acceptable, per instruction.
#
# ==============================================================================

suppressMessages({
  library(splines2)
})

OUTCOMES_TO_RUN <- character(0)   # blocks DiDCF_analysis.R's own main loop
source(here::here("scripts", "R", "DiDCF_analysis.R"))

# ==============================================================================
# Per-cell dose-response fit -- direct translation of contdid::cont_did_acrt()
# ==============================================================================

#' @param cell_df output of build_cell_data() for one (cohort, event_time>0)
#'   cell: treated rows (this cohort) + never-treated rows, columns Z (long
#'   difference) and dose_t (continuous dose, 0 for never-treated).
#' @param dvals dose grid to evaluate the curve on.
#' @param degree,knots B-spline degree/knots -- contdid defaults degree=3,
#'   knots=numeric(0) (num_knots=0, "global polynomial").
fit_dose_response_cell <- function(cell_df, dvals, degree = 3, knots = numeric(0),
                                    min_treated = 10) {
  dose <- cell_df$dose_t[cell_df$treated]
  dy   <- cell_df$Z[cell_df$treated]
  never_dy <- cell_df$Z[!cell_df$treated]

  pos <- dose > 0
  if (sum(pos) < min_treated || length(unique(dose[pos])) < (degree + length(knots) + 1)) {
    return(NULL)  # not enough dose variation to identify the spline in this cell
  }

  bs <- splines2::bSpline(dose[pos], degree = degree, knots = knots) |> as.data.frame()
  colnames(bs) <- paste0("bs_", colnames(bs))
  bs$dy <- dy[pos]
  bs_reg <- lm(dy ~ ., data = bs)

  bs_grid <- splines2::bSpline(dvals, degree = degree, knots = knots) |> as.data.frame()
  colnames(bs_grid) <- colnames(model.matrix(bs_reg))[-1]

  att.d <- as.numeric(predict(bs_reg, newdata = bs_grid) - mean(never_dy, na.rm = TRUE))

  bs_deriv <- splines2::dbs(dvals, degree = degree, knots = knots)
  bet <- coef(bs_reg)[-1]  # drop intercept
  acrt.d <- as.numeric(bs_deriv %*% bet)

  # Delta-method SE off this cell's own B-spline coefficient covariance --
  # see header NOTE: this is the disclosed simplification, contdid uses a
  # multiplier bootstrap over per-unit influence functions instead.
  V <- vcov(bs_reg)
  Vb <- V[-1, -1, drop = FALSE]  # slope-coefficient block only, for acrt.d
  acrt.d_se <- sqrt(pmax(diag(bs_deriv %*% Vb %*% t(bs_deriv)), 0))
  X_att <- cbind(1, as.matrix(bs_grid))
  att.d_se <- sqrt(pmax(diag(X_att %*% V %*% t(X_att)), 0))

  list(att.d = att.d, att.d_se = att.d_se, acrt.d = acrt.d, acrt.d_se = acrt.d_se,
       n_treated = sum(pos))
}

# ==============================================================================
# Cross-(cohort, event-time) aggregation -- exact weight formula from
# ptetools::overall_weights(): weight(g,h) = phi_g / n_post_periods(g)
# ==============================================================================

run_dose_response <- function(cfg, degree = 3, num_knots = 0) {
  df         <- load_outcome_data(cfg)
  df         <- assign_cohorts(df)
  mod_matrix <- build_moderator_matrix(df, MOD_VARS)

  cohorts <- sort(unique(df$cohort[is.finite(df$cohort)]))
  years   <- sort(unique(df$year))

  # Dose grid: evenly spaced across the FULL realized positive-dose range
  # (not contdid's own first-calendar-period default -- see header NOTE on
  # why that default silently degenerates to a handful of outlier districts
  # once dose is time-varying). Ensures the curve is drawn across the entire
  # observed 0-100% dose range; sparse high-dose cells will simply carry a
  # noisier tail.
  treated_dose <- df$treatment[!is.na(df$treatment) & df$treatment > 0]
  dvals <- seq(min(treated_dose), max(treated_dose), length.out = 90)
  knots <- if (num_knots > 0) choose_knots_quantile_local(treated_dose, num_knots) else numeric(0)

  cell_fits <- list()  # each: list(cohort=, h=, fit=list(att.d,...))
  idx <- 0L

  for (cohort_c in cohorts) {
    base_yr   <- cohort_c - 1
    cal_times <- years[years != base_yr & years > base_yr]  # post-treatment only

    for (cal_t in cal_times) {
      cell_df <- build_cell_data(df, mod_matrix, cohort_c, cal_t)
      if (is.null(cell_df)) next

      h <- cal_t - base_yr
      fit <- fit_dose_response_cell(cell_df, dvals, degree = degree, knots = knots)
      if (is.null(fit)) next

      idx <- idx + 1L
      n_treated_units <- length(unique(cell_df$unit_id[cell_df$treated]))
      cell_fits[[idx]] <- list(cohort = cohort_c, h = h, fit = fit, n_treated = n_treated_units)
    }
  }

  if (length(cell_fits) == 0L) {
    warning("No valid (cohort, event_time) cells with enough dose variation.")
    return(NULL)
  }

  # phi_g: cohort's share of the ever-treated population, by DISTINCT
  # treated-unit count -- same quantity as aggregate_event_study_generic()'s
  # cohort_weights, verified against ptetools's own n.group there.
  cohort_sizes <- df |>
    dplyr::filter(is.finite(cohort)) |>
    dplyr::distinct(unit_id, cohort) |>
    dplyr::count(cohort, name = "n_cohort")
  cohort_sizes$phi <- cohort_sizes$n_cohort / sum(cohort_sizes$n_cohort)

  # n_post_periods(g): number of (cohort,h) cells that actually produced a
  # fit for that cohort -- matches overall_weights()'s `sum(is_this_g)`,
  # which counts valid post-treatment (g,t) pairs for that g.
  cohorts_in_fits <- sapply(cell_fits, `[[`, "cohort")
  n_post <- table(cohorts_in_fits)

  weight <- sapply(cell_fits, function(cf) {
    phi_g <- cohort_sizes$phi[cohort_sizes$cohort == cf$cohort]
    phi_g / n_post[as.character(cf$cohort)]
  })
  weight <- weight / sum(weight)  # renormalize (== 1 already if every cohort contributed to every claimed period)

  n_d <- length(dvals)
  att.d_mat  <- do.call(rbind, lapply(cell_fits, function(cf) cf$fit$att.d))
  acrt.d_mat <- do.call(rbind, lapply(cell_fits, function(cf) cf$fit$acrt.d))
  att.d_se_mat  <- do.call(rbind, lapply(cell_fits, function(cf) cf$fit$att.d_se))
  acrt.d_se_mat <- do.call(rbind, lapply(cell_fits, function(cf) cf$fit$acrt.d_se))

  att.d  <- as.numeric(weight %*% att.d_mat)
  acrt.d <- as.numeric(weight %*% acrt.d_mat)
  # weighted combination of per-cell variances (cells are estimated on
  # disjoint samples, so treated as independent for this analytic
  # approximation -- see header NOTE on the SE simplification)
  att.d_se  <- as.numeric(sqrt((weight^2) %*% (att.d_se_mat^2)))
  acrt.d_se <- as.numeric(sqrt((weight^2) %*% (acrt.d_se_mat^2)))

  list(
    dose = as.numeric(dvals),
    att.d = att.d, att.d_se = att.d_se,
    acrt.d = acrt.d, acrt.d_se = acrt.d_se,
    n_cells = length(cell_fits),
    cohorts_used = sort(unique(cohorts_in_fits))
  )
}

choose_knots_quantile_local <- function(x, num_knots) {
  quantile(x, probs = seq(0, 1, length.out = num_knots + 2))[-c(1, num_knots + 2)]
}

# ==============================================================================
# Plot -- matches ptetools::autoplot.dose_obj() exactly (ggpte.R:251-286):
# geom_ribbon(est +/- crit*se) + geom_line, labs "Dose"/"ACRT(d)", theme_bw().
# crit = 1.96 here (pointwise) vs. their bootstrapped simultaneous crit.val --
# see header NOTE.
# ==============================================================================

plot_dose_response <- function(res, label, path, type = "acrt") {
  crit <- 1.96
  if (type == "acrt") {
    plot_df <- data.frame(dose = res$dose, est = res$acrt.d, se = res$acrt.d_se)
    y_lab <- "ACRT(w)"
  } else {
    plot_df <- data.frame(dose = res$dose, est = res$att.d, se = res$att.d_se)
    y_lab <- "ATT(w)"
  }

  p <- ggplot(plot_df, aes(x = dose, y = est)) +
    geom_ribbon(aes(ymin = est - crit * se, ymax = est + crit * se),
                fill = "lightgray", alpha = 0.5) +
    geom_line(linewidth = 1, color = "steelblue4") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
    scale_x_continuous("Dose") +
    scale_y_continuous(y_lab) +
    labs(title = NULL, subtitle = paste0(label, " -- ", y_lab)) +
    theme_bw(base_family = "serif", base_size = 13)

  ggsave(path, plot = p, width = 6.5, height = 4.5, device = "pdf")
  invisible(p)
}

# ==============================================================================
# COMPOSITIONAL variant -- reuses the ALREADY-TRAINED causal forests' saved
# per-(unit, event_time) tau (CACRT) estimates directly, no refitting of any
# kind, not even the B-spline cells above. Bins post-treatment rows by their
# OWN realized dose_t and averages tau within each bin.
#
# NOT the same estimand as cont_did_acrt()'s ACRT(d): the forest assumes a
# constant tau(x) at every dose level for a given covariate profile x, so
# this curve can only move with dose through COMPOSITION (districts at
# different dose levels having different X), never through genuine
# within-unit dose curvature -- see the chat discussion. Kept as a distinct,
# complementary diagnostic; y-axis label matches the CGS-style figure per
# instruction (same title), file name kept distinct so neither overwrites
# the other.
# ==============================================================================

compositional_dose_tau <- function(results_rds_path, n_bins = 20,
                                    event_time_range = 1:7, trim = 0.05) {
  obj <- readRDS(results_rds_path)
  df  <- obj$results$catt

  post <- df |>
    dplyr::filter(event_time %in% event_time_range, !is.na(tau), !is.na(dose_t), dose_t > 0)

  lo <- quantile(post$tau, trim, na.rm = TRUE)
  hi <- quantile(post$tau, 1 - trim, na.rm = TRUE)
  post <- post |> dplyr::filter(tau >= lo, tau <= hi)

  breaks <- quantile(post$dose_t, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)
  breaks[1] <- breaks[1] - 1e-9  # include the smallest dose in bin 1
  post$dose_bin <- cut(post$dose_t, breaks = breaks, include.lowest = TRUE)

  post |>
    dplyr::group_by(dose_bin) |>
    dplyr::summarise(
      dose   = mean(dose_t, na.rm = TRUE),
      est    = mean(tau, na.rm = TRUE),
      se     = sd(tau, na.rm = TRUE) / sqrt(dplyr::n()),
      n      = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(n >= 5) |>
    dplyr::arrange(dose)
}

plot_compositional_dose_tau <- function(bin_df, label, path) {
  crit <- 1.96
  p <- ggplot(bin_df, aes(x = dose, y = est)) +
    geom_ribbon(aes(ymin = est - crit * se, ymax = est + crit * se),
                fill = "lightgray", alpha = 0.5) +
    geom_line(linewidth = 1, color = "steelblue4") +
    geom_point(size = 1.6, color = "steelblue4") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
    scale_x_continuous("Dose") +
    scale_y_continuous("ACRT(w)") +
    labs(title = NULL, subtitle = label) +
    theme_bw(base_family = "serif", base_size = 13)

  ggsave(path, plot = p, width = 6.5, height = 4.5, device = "pdf")
  invisible(p)
}

# ==============================================================================
# MAIN -- fast: per-cell lm() fits only, no forest training/loading.
# ==============================================================================

fig_dir <- here("paper", "figures", "DiDCF_analysis")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
results_dir <- here("quality_reports", "results")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

for (outcome_name in c("afgr", "math", "ela")) {
  cfg   <- OUTCOMES[[outcome_name]]
  label <- cfg$label

  message("\n", strrep("=", 60))
  message("Dose-response ACRT(d)/ATT(d): ", outcome_name, " (", label, ")")
  message(strrep("=", 60))

  t0 <- Sys.time()
  res <- run_dose_response(cfg)
  message(sprintf("  Done in %.1f sec | %d valid cells | cohorts: %s",
                  as.numeric(difftime(Sys.time(), t0, units = "secs")),
                  res$n_cells, paste(res$cohorts_used, collapse = ", ")))

  plot_dose_response(res, label,
                      file.path(fig_dir, paste0("didcf_acrt_dose_", outcome_name, ".pdf")),
                      type = "acrt")
  plot_dose_response(res, label,
                      file.path(fig_dir, paste0("didcf_att_dose_", outcome_name, ".pdf")),
                      type = "att")

  saveRDS(res, file.path(results_dir, paste0("didcf_dose_response_", outcome_name, ".rds")))

  cat("\n-- ACRT(d) summary (", outcome_name, ") --\n")
  show_idx <- seq(1, length(res$dose), by = 10)
  print(data.frame(
    dose = round(res$dose[show_idx], 3),
    acrt.d = round(res$acrt.d[show_idx], 3),
    acrt.d_se = round(res$acrt.d_se[show_idx], 3)
  ), row.names = FALSE)

  # Compositional variant -- reads the already-trained forest's saved
  # results directly, no refitting at all.
  prod_rds <- file.path(results_dir, paste0("didcf_results_", outcome_name, ".rds"))
  if (file.exists(prod_rds)) {
    bin_df <- compositional_dose_tau(prod_rds)
    plot_compositional_dose_tau(
      bin_df, label,
      file.path(fig_dir, paste0("didcf_acrt_dose_compositional_", outcome_name, ".pdf"))
    )
    cat("\n-- Compositional ACRT(d) (already-trained forest tau, binned by realized dose) --\n")
    print(as.data.frame(bin_df[, c("dose","est","se","n")]), row.names = FALSE, digits = 3)
  } else {
    message("  Skipping compositional variant: ", prod_rds, " not found")
  }
}

message("\n=== Dose-response ACRT(d)/ATT(d) complete for all 3 outcomes ===")
