# ==============================================================================
# DiDCF_analysis.R
# Staggered, continuous-dose difference-in-differences causal forest (DiDCF)
# for graduation (AFGR), math, and ELA.
#
# Per cohort g x calendar year t cell (never pooled across cohorts):
#   post-period : lm_forest of Z = Y_t - Y_{g-1} on [1{D>0}, D] using cohort-g
#                 and never-treated units -> alpha(x) (jump), tau(x) (dose slope)
#   pre-period  : binary causal forest (placebo)
# Then event studies, heterogeneity summaries, policy trees, and state maps.
#
# Outputs: models/DiDCF_analysis/, paper/{figures,tables}/DiDCF_analysis/,
#          quality_reports/results/didcf_results_<outcome>.rds
#
# Usage: set RUN_ESTIMATION, LOAD_MODELS, OUTCOMES_TO_RUN below (or before
# sourcing). Full estimation takes several hours.
# ==============================================================================

library(here)
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(grf)
library(knitr)
library(modelsummary)
library(policytree)
library(Matrix)
library(usmap)

set.seed(42)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Each constant can be pre-set before sourcing.
# RUN_ESTIMATION: fit forests. LOAD_MODELS: load saved forests instead.
if (!exists("RUN_ESTIMATION"))  RUN_ESTIMATION  <- TRUE

if (!exists("NUM_TREES"))       NUM_TREES       <- 1500
if (!exists("MIN_NODE_SIZE"))   MIN_NODE_SIZE   <- 20
if (!exists("NUM_THREADS"))     NUM_THREADS     <- NULL  # NULL = all cores

if (!exists("OUTCOMES_TO_RUN")) OUTCOMES_TO_RUN <- c("afgr", "math", "ela")

if (!exists("LOAD_MODELS"))    LOAD_MODELS    <- FALSE

if (!exists("MODELS_DIR"))     MODELS_DIR     <- here("models", "DiDCF_analysis")

# Trees for the stage-1 forest of the "two_stage" estimator.
if (!exists("M0_NUM_TREES"))   M0_NUM_TREES   <- 1000

# Baseline-performance moderator = Y_{g - BASELINE_LAG}.
if (!exists("BASELINE_LAG"))   BASELINE_LAG   <- 2

# k-means neighborhoods (Section 5C): number of clusters, minimum cluster size.
if (!exists("MATCH_N_NEIGHBORHOODS"))              MATCH_N_NEIGHBORHOODS              <- 10
if (!exists("MATCH_MIN_TREATED_PER_NEIGHBORHOOD")) MATCH_MIN_TREATED_PER_NEIGHBORHOOD <- 15

# Dose conditioning: dose-quantile fixed effects in the BLP (Section 5), and
# dose groups (low/middle/high terciles of mean post-period dose) for the
# quartile event studies (Section 5E).
if (!exists("BLP_DOSE_BINS"))  BLP_DOSE_BINS  <- 5
DOSE_GROUP_LEVELS <- c("Low", "Middle", "High")
# Moderators (top N by VIF at e = 0) that get the dose-group event studies.
if (!exists("N_DOSE_ES_MODS")) N_DOSE_ES_MODS <- 2

# Trend/dose appendix table (Section 9): report only the pooled regression per outcome.
if (!exists("TREND_POOLED_ONLY")) TREND_POOLED_ONLY <- TRUE

# State maps (Section 5D): internal event times averaged (1:7 = e 0..6), and
# small states labeled offshore.
if (!exists("STATE_MAP_EVENT_TIMES")) STATE_MAP_EVENT_TIMES <- 1:7
STATE_MAP_CALLOUTS <- c("VT", "NH", "MA", "RI", "CT", "NJ", "DE", "MD", "DC")

# Moderators (X). baseline_y is added per cell by build_cell_data().
MOD_VARS <- c(
  "baseline_y",
  "perblk", "perhsp", "perfrl", "perwht", "perspeced",
  "logenroll", "urban", "suburb", "town", "rural",
  "num_magnet", "zcoolcitypop", "zcoolcityuniv",
  "p_rev", "p_exp", "str", "tea_salary",
  "w_pp_exp_percdiff", "charter_exp",
  "napcs14_nocaps", "napcs14_transpar", "napcs14_perf",
  "napcs14_non_renew", "napcs14_exem", "napcs14_eq_funding",
  "share_author", "share_cmo", "share_emo"
)

# Display labels for moderators.
COVARIATE_NAMES <- c(
  logenroll         = "Log of Enrollment",
  perwht            = "White (%)",
  perblk            = "Black (%)",
  perhsp            = "Hispanic (%)",
  perfrl            = "Free/Reduced Lunch (%)",
  perspeced         = "Special Ed (%)",
  baseline_y        = "Baseline Performance",
  urban             = "Urban",
  suburb            = "Suburb",
  town              = "Town",
  rural             = "Rural",
  num_magnet        = "Magnet Schools (%)",
  zcoolcitypop      = "City Population (standardized)",
  zcoolcityuniv     = "University in City",
  p_rev             = "Per Pupil Revenue",
  str               = "Student-Teacher Ratio",
  tea_salary        = "Teacher Salary",
  w_pp_exp_percdiff = "TPS-Charter Spending (% diff)",
  p_exp             = "Total Spending (per-pupil)",
  charter_exp       = "Charter Spending (per-pupil)",
  napcs14_eq_funding = "Equitable Funding",
  napcs14_nocaps    = "No Caps on CS Growth",
  napcs14_perf      = "Performance-Based Contracts",
  napcs14_transpar  = "Transparent Charter Startup Policies",
  napcs14_non_renew = "Clear Charter Renewal Policies",
  napcs14_exem      = "Exempt from State/District Regs",
  share_author      = "Charter Authorizer (LEA) Share",
  share_cmo         = "Charter CMO Share",
  share_emo         = "Charter EMO Share"
)

# Dollar moderators rescaled to $1,000s in the BLP tables.
BLP_DOLLAR_VARS <- c("tea_salary", "p_rev", "p_exp", "charter_exp")

# Outcome configurations. estimator: "joint_lm" (all outcomes) or "two_stage".
OUTCOMES <- list(
  afgr = list(
    data_file  = "charter_afgr2.dta",
    outcome    = "afgr",
    treatment  = "lag_share",
    share_var  = "cha_share2",
    unit_vars  = "district",
    time_var   = "year",
    weight_var = "eweight",
    label      = "Graduation Rate (AFGR)",
    estimator  = "joint_lm"
  ),
  math = list(
    data_file  = "charter_seda.dta",
    outcome    = "st_math",
    treatment  = "lag_grade",
    share_var  = "cha_share2",
    unit_vars  = c("district", "grade"),
    time_var   = "year",
    weight_var = "eweight",
    label      = "Math Test Scores",
    estimator  = "joint_lm"
  ),
  ela = list(
    data_file  = "charter_seda.dta",
    outcome    = "st_ela",
    treatment  = "lag_grade",
    share_var  = "cha_share2",
    unit_vars  = c("district", "grade"),
    time_var   = "year",
    weight_var = "eweight",
    label      = "ELA Test Scores",
    estimator  = "joint_lm"
  )
)

# ==============================================================================
# SECTION 1: DATA PREPARATION
# ==============================================================================

#' Load an outcome panel, merge policy and finance moderators, and rename to
#' unit_id, year, outcome, treatment, share, weight.
load_outcome_data <- function(cfg) {
  df <- read_dta(here("data", "cleaned", cfg$data_file))

  # grf needs plain numeric columns
  df <- haven::zap_labels(df)

  # state policy moderators
  policy <- haven::zap_labels(read_dta(here("data", "raw", "policy3.dta")))
  df <- left_join(df, select(policy, -any_of(c("state", "postal"))),
                  by = "statename")

  # district finance moderators
  finance <- haven::zap_labels(read_dta(here("data", "raw", "finance.dta")))
  df <- left_join(df, finance, by = "district")

  # unit_id = district (AFGR) or district_grade (SEDA); district kept for clustering
  if (length(cfg$unit_vars) > 1) {
    df <- unite(df, "unit_id", all_of(cfg$unit_vars), sep = "_", remove = FALSE)
  } else {
    df <- rename(df, unit_id = all_of(cfg$unit_vars))
    df$district <- df$unit_id
  }

  df <- rename(df,
    year      = all_of(cfg$time_var),
    outcome   = all_of(cfg$outcome),
    treatment = all_of(cfg$treatment),
    share     = all_of(cfg$share_var),
    weight    = all_of(cfg$weight_var)
  ) |>
    mutate(
      ever_treated = !is.na(maxshare2) & maxshare2 > 0
    )

  recode_finance_missing(df)
}

#' Recode finance.dta zeros that denote missing values to NA (charter_exp,
#' w_pp_exp_percdiff, p_exp). Districts without charters keep charter_exp = 0.
recode_finance_missing <- function(df) {
  df <- df |>
    group_by(district) |>
    mutate(has_charter = any(share > 0, na.rm = TRUE) | any(maxshare2 > 0, na.rm = TRUE)) |>
    ungroup()

  tps_imputed <- is.na(df$tps_exp) | df$tps_exp == 0

  df <- df |>
    mutate(
      charter_exp = case_when(
        !has_charter                       ~ 0,
        is.na(charter_exp) | charter_exp == 0 ~ NA_real_,
        TRUE                               ~ charter_exp
      ),
      w_pp_exp_percdiff = case_when(
        tps_imputed         ~ NA_real_,
        !has_charter        ~ 0,
        is.na(charter_exp)  ~ NA_real_,
        TRUE                ~ w_pp_exp_percdiff
      ),
      p_exp = na_if(p_exp, 0)
    )

  d <- distinct(df, district, has_charter, charter_exp, w_pp_exp_percdiff)
  message(sprintf(
    "  Finance recode: %d districts (%d with charters) | charter_exp NA: %d | %% diff NA: %d (%d with charters) | p_exp NA rows: %d",
    nrow(d), sum(d$has_charter), sum(is.na(d$charter_exp)),
    sum(is.na(d$w_pp_exp_percdiff)), sum(is.na(d$w_pp_exp_percdiff) & d$has_charter),
    sum(is.na(df$p_exp))
  ))
  df
}

#' Cohort = first year with positive dose; never-treated units get Inf.
assign_cohorts <- function(df) {
  first_treat <- df |>
    filter(treatment > 0, !is.na(treatment)) |>
    group_by(unit_id) |>
    summarise(cohort = min(year), .groups = "drop")

  df |>
    left_join(first_treat, by = "unit_id") |>
    mutate(cohort = replace_na(cohort, Inf))
}

#' One row of moderators per unit (first observed year); napcs14_* rescaled to [0, 1].
build_moderator_matrix <- function(df, mod_vars) {
  available <- intersect(mod_vars, names(df))

  df |>
    arrange(unit_id, year) |>
    group_by(unit_id) |>
    slice(1) |>
    ungroup() |>
    select(unit_id, all_of(available)) |>
    mutate(across(
      starts_with("napcs14_"),
      ~ {
        v <- replace_na(., 0)
        mx <- max(v, na.rm = TRUE)
        if (mx > 0) v / mx else v
      }
    ))
}

# ==============================================================================
# SECTION 2: CELL-LEVEL DiDCF ESTIMATION
# ==============================================================================

#' Data for cohort g x calendar year t: cohort-g and never-treated units,
#' Z = Y_t - Y_{g-1}, dose at t, baseline_y = Y_{g - BASELINE_LAG}.
#' NULL if either arm has fewer than min_obs units.
build_cell_data <- function(df, mod_matrix, cohort_c, cal_t, min_obs = 5) {
  base_yr <- cohort_c - 1

  yt <- df |>
    filter(cohort %in% c(cohort_c, Inf), year == cal_t) |>
    select(unit_id, cohort, outcome_t = outcome, dose_t = treatment, weight, district)

  yb <- df |>
    filter(cohort %in% c(cohort_c, Inf), year == base_yr) |>
    select(unit_id, outcome_b = outcome)

  ybl <- df |>
    filter(cohort %in% c(cohort_c, Inf), year == cohort_c - BASELINE_LAG, !is.na(outcome)) |>
    select(unit_id, baseline_y = outcome)

  panel <- yt |>
    inner_join(yb,        by = "unit_id") |>
    inner_join(ybl,       by = "unit_id") |>
    inner_join(mod_matrix, by = "unit_id") |>
    mutate(
      Z          = outcome_t - outcome_b,
      treated    = (cohort == cohort_c),
      event_time = cal_t - base_yr
    ) |>
    filter(!is.na(Z), !is.na(dose_t))

  n_treated <- sum(panel$treated)
  n_control <- sum(!panel$treated)
  if (n_treated < min_obs || n_control < min_obs) return(NULL)

  panel
}

#' Forest inputs for one cell: X (NA left for grf), W, Y, weights, treated flag, clusters.
prepare_cf_inputs <- function(cell_df, mod_vars) {
  available <- intersect(mod_vars, names(cell_df))

  X <- cell_df |> select(all_of(available)) |> as.matrix()

  list(
    X           = X,
    W           = as.numeric(cell_df$dose_t),
    Y           = as.numeric(cell_df$Z),
    weights     = as.numeric(cell_df$weight),
    treated_idx = as.logical(cell_df$treated),
    clusters    = cell_df$district
  )
}

#' "two_stage" stage 1: m0_hat(x) from never-treated units; returns the treated
#' rows with Ztilde = Z - m0_hat(X).
fit_stage1_cell <- function(cell_df, mod_vars,
                            run        = RUN_ESTIMATION,
                            load       = LOAD_MODELS,
                            model_path = NULL) {

  inputs <- prepare_cf_inputs(cell_df, mod_vars)
  tr <- which(inputs$treated_idx)
  un <- which(!inputs$treated_idx)

  can_load <- load && !is.null(model_path) && file.exists(model_path)

  if (run) {
    m0_forest <- regression_forest(
      X              = inputs$X[un, , drop = FALSE],
      Y              = inputs$Y[un],
      sample.weights = inputs$weights[un],
      clusters       = as.factor(inputs$clusters[un]),
      num.trees      = M0_NUM_TREES,
      honesty        = TRUE,
      num.threads    = NUM_THREADS,
      seed           = 42
    )
    m0_hat <- predict(m0_forest, newdata = inputs$X[tr, , drop = FALSE])$predictions
    if (!is.null(model_path)) saveRDS(list(m0_hat = m0_hat), model_path)
  } else if (can_load) {
    m0_hat <- readRDS(model_path)$m0_hat
  } else {
    m0_hat <- rep(NA_real_, length(tr))
  }

  list(
    X         = inputs$X[tr, , drop = FALSE],
    Ztilde    = inputs$Y[tr] - m0_hat,
    W         = inputs$W[tr],
    weights   = inputs$weights[tr],
    clusters  = inputs$clusters[tr],
    n_treated = length(tr),
    n_control = length(un)
  )
}

#' Causal forest of Ztilde on W for one cell. Continuous W ("two_stage" stage 2):
#' catt = alpha + tau * W. Binary W (pre-period placebo): tau = placebo CATT.
#' Falls back to a regression forest if W has no variance.
estimate_stage2 <- function(X, Ztilde, W, weights, clusters, event_time,
                            num_trees     = NUM_TREES,
                            min_node_size = MIN_NODE_SIZE,
                            run           = RUN_ESTIMATION,
                            load          = LOAD_MODELS,
                            model_path    = NULL) {

  clusters <- as.factor(clusters)

  n <- length(Ztilde)

  has_dose <- n > 0 && stats::sd(W) > 1e-10
  can_load <- load && !is.null(model_path) && file.exists(model_path)

  if (run || can_load) {
    if (run) {
      if (has_dose) {
        cf <- causal_forest(
          X = X, Y = Ztilde, W = W,
          sample.weights = weights,
          clusters       = clusters,
          num.trees      = num_trees,
          min.node.size  = min_node_size,
          honesty        = TRUE,
          num.threads    = NUM_THREADS,
          seed           = 42
        )
      } else {
        cf <- regression_forest(
          X = X, Y = Ztilde,
          sample.weights = weights,
          clusters       = clusters,
          num.trees      = num_trees,
          min.node.size  = min_node_size,
          honesty        = TRUE,
          num.threads    = NUM_THREADS,
          seed           = 42
        )
      }
      if (!is.null(model_path)) saveRDS(list(cf = cf, has_dose = has_dose), model_path)
    } else {
      saved    <- readRDS(model_path)
      cf       <- saved$cf
      has_dose <- saved$has_dose
    }

    if (has_dose) {
      # OOB predictions and little-bags variances
      pred_out <- predict(cf, estimate.variance = TRUE)
      tau      <- pred_out$predictions
      tau_se   <- sqrt(pred_out$variance.estimates)

      # alpha(x) = Y.hat - tau(x) * W.hat
      alpha     <- cf$Y.hat - tau * cf$W.hat
      att_level <- alpha + tau * W
      catt_se   <- tau_se * W          # delta method, alpha held fixed

      # DR scores (get_scores handles continuous W)
      dr_scores <- tryCatch(
        grf::get_scores(cf, num.trees.for.weights = 100),
        error = function(e) {
          warning("get_scores() failed at event_time=", event_time, ": ", e$message)
          rep(NA_real_, n)
        }
      )
    } else {
      pred_out  <- predict(cf, estimate.variance = TRUE)
      alpha     <- pred_out$predictions
      att_level <- alpha
      catt_se   <- sqrt(pred_out$variance.estimates)
      tau       <- rep(NA_real_, n)
      tau_se    <- rep(NA_real_, n)
      dr_scores <- rep(NA_real_, n)
    }

    # label VIF by the columns the forest was trained on
    vif <- setNames(as.numeric(variable_importance(cf)), colnames(cf$X.orig))
  } else {
    tau <- tau_se <- alpha <- att_level <- catt_se <- dr_scores <- rep(NA_real_, n)
    vif <- setNames(rep(NA_real_, ncol(X)), colnames(X))
  }

  list(catt = att_level, catt_se = catt_se, tau = tau, tau_se = tau_se,
       alpha = alpha, vif = vif, dr_scores = dr_scores, event_time = event_time)
}

#' Doubly robust scores for an lm_forest's coefficients (Graham & Pinto 2022,
#' eq. 19). idx: rows to score. Returns a length(idx) x K matrix.
graham_pinto_scores <- function(fit, idx = NULL) {
  W    <- fit$W.orig
  What <- fit$W.hat
  Y    <- fit$Y.orig[, 1]
  Yhat <- fit$Y.hat[, 1]
  b0   <- fit$predictions

  n <- nrow(W); K <- ncol(W)
  if (is.null(idx)) idx <- seq_len(n)

  resid_W  <- W - What
  residual <- (Y - Yhat) - rowSums(b0 * resid_W)

  wts <- get_forest_weights(fit, newdata = fit$X.orig[idx, , drop = FALSE])

  psi <- matrix(NA_real_, length(idx), K, dimnames = list(NULL, colnames(W)))
  for (k in seq_along(idx)) {
    i    <- idx[k]
    dW   <- sweep(W, 2, What[i, ], "-")
    v0_i <- crossprod(dW, wts[k, ] * dW)          # local covariance of W at x_i
    psi[k, ] <- tryCatch(
      solve(v0_i, resid_W[i, ] * residual[i]) + b0[i, ],
      error = function(e) rep(NA_real_, K)        # singular v0_i -> NA
    )
  }
  psi
}

#' "joint_lm" estimator for one cell: lm_forest of Z on [1{D>0}, D] over all
#' units. Returns treated rows: catt = alpha + tau * D, tau, SEs, DR scores, VIF.
estimate_joint_lm_cell <- function(cell_df, mod_vars, event_time,
                                   num_trees     = NUM_TREES,
                                   min_node_size = MIN_NODE_SIZE,
                                   run           = RUN_ESTIMATION,
                                   load          = LOAD_MODELS,
                                   model_path    = NULL) {

  inputs  <- prepare_cf_inputs(cell_df, mod_vars)
  X       <- inputs$X
  Z       <- inputs$Y
  D       <- inputs$W
  Wmat    <- cbind(jump = as.numeric(D > 0), dose = D)
  weights <- inputs$weights
  clusters<- as.factor(inputs$clusters)
  tr      <- inputs$treated_idx
  n       <- nrow(X)

  can_load <- load && !is.null(model_path) && file.exists(model_path)

  if (run || can_load) {
    if (run) {
      fit <- lm_forest(
        X = X, Y = Z, W = Wmat,
        sample.weights = weights,
        clusters       = clusters,
        num.trees      = num_trees,
        min.node.size  = min_node_size,
        honesty        = TRUE,
        num.threads    = NUM_THREADS,
        seed           = 42
      )
      if (!is.null(model_path)) saveRDS(fit, model_path)
    } else {
      fit <- readRDS(model_path)
    }

    # alpha = jump coefficient, tau = dose coefficient
    w_cols    <- colnames(fit$W.orig)
    alpha     <- fit$predictions[, which(w_cols == "jump")]
    tau       <- fit$predictions[, which(w_cols == "dose")]
    att_level <- alpha + tau * D

    # little-bags variances
    var_pred <- predict(fit, estimate.variance = TRUE)$variance.estimates
    v_cols   <- colnames(var_pred)
    alpha_se <- sqrt(var_pred[, which(v_cols == "jump")])
    tau_se   <- sqrt(var_pred[, which(v_cols == "dose")])

    # delta method, ignoring Cov(alpha, tau)
    catt_se <- sqrt(alpha_se^2 + (D * tau_se)^2)

    # DR scores for treated rows only
    tr_idx <- which(tr)
    dr_scores_tr <- tryCatch(
      graham_pinto_scores(fit, idx = tr_idx)[, "dose"],
      error = function(e) {
        warning("graham_pinto_scores() failed at event_time=", event_time, ": ", e$message)
        rep(NA_real_, length(tr_idx))
      }
    )

    # label VIF by the columns the forest was trained on
    vif <- setNames(as.numeric(variable_importance(fit)), colnames(fit$X.orig))
  } else {
    tau <- tau_se <- alpha <- att_level <- catt_se <- rep(NA_real_, n)
    dr_scores_tr <- rep(NA_real_, sum(tr))
    vif <- setNames(rep(NA_real_, ncol(X)), colnames(X))
  }

  list(catt = att_level[tr], catt_se = catt_se[tr], tau = tau[tr], tau_se = tau_se[tr],
       alpha = alpha[tr], vif = vif, dr_scores = dr_scores_tr, event_time = event_time)
}

# ==============================================================================
# SECTION 3: MAIN DiDCF LOOP (one outcome)
# ==============================================================================

#' Fit every cohort x calendar-year cell for one outcome. Returns
#' list(catt = one row per treated unit x event time, vif = importance by cell).
run_didcf <- function(cfg, outcome_name) {
  message("  Loading: ", cfg$data_file)
  df         <- load_outcome_data(cfg)
  df         <- assign_cohorts(df)
  mod_matrix <- build_moderator_matrix(df, MOD_VARS)

  # drop cohorts with no Y_{g - BASELINE_LAG} in the panel
  cohorts <- sort(unique(df$cohort[is.finite(df$cohort)]))
  years   <- sort(unique(df$year))
  skipped <- cohorts[cohorts - BASELINE_LAG < min(years)]
  cohorts <- setdiff(cohorts, skipped)
  message("  Cohorts skipped (no Y_{g-", BASELINE_LAG, "}): ", paste(skipped, collapse = ", "))

  out_models_dir <- file.path(MODELS_DIR, outcome_name)
  if (RUN_ESTIMATION) dir.create(out_models_dir, showWarnings = FALSE, recursive = TRUE)

  mode_label <- if (RUN_ESTIMATION) "TRAINING" else if (LOAD_MODELS) "LOADING" else "OFF (placeholders)"
  message(sprintf("  Cohorts: %d  |  Years: %d  |  Mode: %s  |  Estimator: %s",
                  length(cohorts), length(years), mode_label, cfg$estimator))
  if (RUN_ESTIMATION || LOAD_MODELS)
    message("  Models → ", out_models_dir)

  cell_rows <- list()
  idx <- 0L

  for (cohort_c in cohorts) {
    base_yr   <- cohort_c - 1
    cal_times <- years[years != base_yr]

    for (cal_t in cal_times) {
      cell_df <- build_cell_data(df, mod_matrix, cohort_c, cal_t)
      if (is.null(cell_df)) next

      h <- cal_t - base_yr
      treated_rows <- cell_df[cell_df$treated, ] |>
        mutate(cohort = cohort_c, event_time = h, cal_time = cal_t,
               n_control = sum(!cell_df$treated))

      if (h < 0) {
        # pre-period placebo: binary W, both arms, raw Z
        model_path <- file.path(out_models_dir,
                                sprintf("placebo_c%d_h%+d.rds", cohort_c, h))
        s2 <- estimate_stage2(
          X          = prepare_cf_inputs(cell_df, MOD_VARS)$X,
          Ztilde     = as.numeric(cell_df$Z),
          W          = as.numeric(cell_df$treated),
          weights    = as.numeric(cell_df$weight),
          clusters   = cell_df$district,
          event_time = h,
          model_path = model_path
        )
        tr <- which(cell_df$treated)
        treated_rows$catt     <- s2$tau[tr]
        treated_rows$catt_se  <- s2$tau_se[tr]
        treated_rows$tau      <- s2$tau[tr]
        treated_rows$tau_se   <- s2$tau_se[tr]
        treated_rows$alpha    <- s2$alpha[tr]
        treated_rows$dr_score <- s2$dr_scores[tr]
        vif_h <- tibble(variable = names(s2$vif), importance = as.numeric(s2$vif),
                        event_time = h, cohort = cohort_c, n_treated = length(tr))

      } else if (cfg$estimator == "joint_lm") {
        # post-period: joint lm_forest
        model_path <- file.path(out_models_dir,
                                sprintf("joint_c%d_h%+d.rds", cohort_c, h))
        s2 <- estimate_joint_lm_cell(cell_df, MOD_VARS, event_time = h,
                                     model_path = model_path)
        treated_rows$catt     <- s2$catt
        treated_rows$catt_se  <- s2$catt_se
        treated_rows$tau      <- s2$tau
        treated_rows$tau_se   <- s2$tau_se
        treated_rows$alpha    <- s2$alpha
        treated_rows$dr_score <- s2$dr_scores
        vif_h <- tibble(variable = names(s2$vif), importance = as.numeric(s2$vif),
                        event_time = h, cohort = cohort_c, n_treated = nrow(treated_rows))

      } else {
        # post-period: two-stage
        model_path1 <- file.path(out_models_dir, sprintf("m0_c%d_t%d.rds", cohort_c, cal_t))
        s1 <- fit_stage1_cell(cell_df, MOD_VARS, model_path = model_path1)

        model_path2 <- file.path(out_models_dir, sprintf("stage2_c%d_h%+d.rds", cohort_c, h))
        s2 <- estimate_stage2(
          X = s1$X, Ztilde = s1$Ztilde, W = s1$W, weights = s1$weights,
          clusters = s1$clusters, event_time = h, model_path = model_path2
        )
        treated_rows$catt     <- s2$catt
        treated_rows$catt_se  <- s2$catt_se
        treated_rows$tau      <- s2$tau
        treated_rows$tau_se   <- s2$tau_se
        treated_rows$alpha    <- s2$alpha
        treated_rows$dr_score <- s2$dr_scores
        vif_h <- tibble(variable = names(s2$vif), importance = as.numeric(s2$vif),
                        event_time = h, cohort = cohort_c, n_treated = nrow(treated_rows))
      }

      idx <- idx + 1L
      cell_rows[[idx]] <- list(rows = treated_rows, vif = vif_h)
    }
    # free memory from loaded forests
    gc(verbose = FALSE)
    message(sprintf("    Cohort %d done", cohort_c))
  }

  cell_rows <- compact(cell_rows)
  if (length(cell_rows) == 0L) return(list(catt = tibble(), vif = tibble()))

  list(
    catt = bind_rows(lapply(cell_rows, `[[`, "rows")),
    vif  = bind_rows(lapply(cell_rows, `[[`, "vif"))
  )
}

# ==============================================================================
# SECTION 4: AGGREGATION
# ==============================================================================

#' Event-time aggregate of value_col: unit means within cohort x event time,
#' then cohort weights phi_g = n_g / sum(n_g). SEs treat unit estimates as
#' independent.
aggregate_event_study_generic <- function(results, value_col, se_col) {
  df <- results$catt
  value <- df[[value_col]]; se <- df[[se_col]]

  # phi_g: treated units per cohort
  cohort_weights <- df |>
    distinct(unit_id, cohort) |>
    count(cohort, name = "n_cohort") |>
    mutate(phi = n_cohort / sum(n_cohort))

  # step 1: cohort x event-time means and variances
  cohort_level <- df |>
    mutate(.value = value, .se = se) |>
    left_join(cohort_weights, by = "cohort") |>
    group_by(cohort, event_time) |>
    summarise(
      phi    = first(phi),
      n_g    = sum(!is.na(.value)),
      mean_g = mean(.value, na.rm = TRUE),
      n_se   = sum(!is.na(.se)),
      var_g  = if (n_se > 0) sum(.se^2, na.rm = TRUE) / n_se^2 else NA_real_,
      .groups = "drop"
    ) |>
    filter(n_g > 0)

  # step 2: combine cohorts
  cohort_level |>
    group_by(event_time) |>
    mutate(phi_norm = phi / sum(phi)) |>
    summarise(
      mean    = sum(phi_norm * mean_g),
      se      = if (any(!is.na(var_g))) sqrt(sum(phi_norm^2 * var_g, na.rm = TRUE)) else NA_real_,
      n_units = sum(n_g),
      .groups = "drop"
    )
}

#' CATT event study. With if_data, SEs come from the bootstrap (Section 4B) and
#' catt_crit (uniform band) is added.
aggregate_event_study <- function(results, if_data = NULL) {
  out <- aggregate_event_study_generic(results, "catt", "catt_se") |>
    rename(catt_mean = mean, catt_se = se)
  if (is.null(if_data)) return(out)
  b <- if_event_study_se(if_data, cohort_sizes(results))
  out |>
    left_join(b |> rename(se_if = se, catt_crit = crit_uniform), by = "event_time") |>
    mutate(catt_se_indep = catt_se,
           catt_se = ifelse(event_time %in% b$event_time, se_if, catt_se)) |>
    select(-se_if)
}

#' CACRT event study. With if_data, pre-period (placebo) SEs come from the bootstrap.
aggregate_event_study_tau <- function(results, if_data = NULL) {
  out <- aggregate_event_study_generic(results, "tau", "tau_se") |>
    rename(tau_mean = mean, tau_se = se)
  if (is.null(if_data)) return(out)
  pre_ids <- sort(unique(if_data$rows$event_time[if_data$rows$event_time < 0]))
  b <- if_event_study_se(if_data, cohort_sizes(results), event_times = pre_ids)
  out |>
    left_join(b |> rename(se_if = se, tau_crit = crit_uniform), by = "event_time") |>
    mutate(tau_se_indep = tau_se,
           tau_se = ifelse(event_time %in% b$event_time, se_if, tau_se)) |>
    select(-se_if)
}

# ==============================================================================
# SECTION 4B: BOOTSTRAP INFERENCE FOR THE EVENT STUDIES
# ==============================================================================
# Per-cell influence functions, combined with the cohort weights and a
# district-clustered Rademacher multiplier bootstrap (same draw for a district
# in every cell). Pre-period cells: AIPW ATT score of the placebo forest.
# Post-period cells: (Ztilde_i - cell mean) / n_T.

#' Treated units per cohort.
cohort_sizes <- function(results) {
  results$catt |> distinct(unit_id, cohort) |> count(cohort, name = "n_cohort")
}

#' Cohort, calendar year, and event time from a saved model filename.
parse_cell_file <- function(path) {
  nm <- basename(path)
  g  <- as.numeric(sub(".*_c([0-9]+)_.*", "\\1", nm))
  k  <- as.numeric(sub(".*_[th]([+-]?[0-9]+)\\.rds$", "\\1", nm))
  if (grepl("_t[0-9]+\\.rds$", nm)) list(cohort = g, cal_t = k, event_time = k - (g - 1))
  else                               list(cohort = g, cal_t = g - 1 + k, event_time = k)
}

#' Influence-function inputs for every cell, rebuilt from the saved models.
#' Returns list(rows = unit x cell residuals and control weights, mod_matrix).
build_if_data <- function(cfg, outcome_name, models_dir = file.path(MODELS_DIR, outcome_name)) {
  df <- assign_cohorts(load_outcome_data(cfg))
  mm <- build_moderator_matrix(df, MOD_VARS)
  joint <- cfg$estimator == "joint_lm"
  # strict filename patterns (skips OneDrive conflict copies)
  files <- c(list.files(models_dir, pattern = "^placebo_c[0-9]{4}_h-[0-9]+\\.rds$", full.names = TRUE),
             list.files(models_dir, full.names = TRUE,
                        pattern = if (joint) "^joint_c[0-9]{4}_h\\+[0-9]+\\.rds$" else "^m0_c[0-9]{4}_t[0-9]{4}\\.rds$"))

  rows <- vector("list", length(files))
  for (k in seq_along(files)) {
    f  <- files[k]; ci <- parse_cell_file(f)
    cell_df <- build_cell_data(df, mm, ci$cohort, ci$cal_t)
    if (is.null(cell_df)) next
    Z  <- as.numeric(cell_df$Z)
    tr <- cell_df$treated
    if (grepl("^placebo_", basename(f))) {
      cf <- readRDS(f)$cf
      stopifnot(length(cf$Y.orig) == length(Z), max(abs(as.numeric(cf$Y.orig) - Z)) < 1e-10)
      tau <- predict(cf)$predictions
      res <- Z - (cf$Y.hat - cf$W.hat * tau)
      r   <- ifelse(tr, NA_real_, cf$W.hat / (1 - cf$W.hat))
      keep <- rep(TRUE, length(Z))
      rm(cf)
    } else if (joint) {
      fit <- readRDS(f)
      stopifnot(length(fit$Y.orig) == length(Z), max(abs(as.numeric(fit$Y.orig) - Z)) < 1e-10)
      n    <- length(Z)
      beta <- matrix(fit$predictions, nrow = n)
      res  <- Z - (as.numeric(fit$Y.hat) - rowSums(beta * matrix(fit$W.hat, nrow = n)))
      r    <- rep(NA_real_, n); keep <- tr
      rm(fit)
    } else {
      m0 <- readRDS(f)$m0_hat
      stopifnot(length(m0) == sum(tr))
      res <- rep(NA_real_, length(Z)); res[tr] <- Z[tr] - m0
      r   <- rep(NA_real_, length(Z)); keep <- tr
    }
    rows[[k]] <- tibble(cohort = ci$cohort, event_time = ci$event_time,
                        unit_id = cell_df$unit_id, district = cell_df$district,
                        treated = tr, res = res, r = r, baseline_y = cell_df$baseline_y)[keep, ]
    if (k %% 20 == 0) gc(verbose = FALSE)
  }
  list(rows = bind_rows(rows), mod_matrix = mm)
}

#' Bootstrap SEs and uniform critical value for a cohort-weighted event study.
#' treated_ids / control_keep restrict to a subgroup; n_g = its cohort sizes.
if_event_study_se <- function(if_data, n_g, treated_ids = NULL, control_keep = NULL,
                              event_times = NULL, B = 1000, seed = 42) {
  d <- if_data$rows
  if (!is.null(event_times)) d <- d[d$event_time %in% event_times, ]
  if (!is.null(treated_ids)) d <- d[!d$treated | d$unit_id %in% treated_ids, ]
  if (!is.null(control_keep)) d <- d[d$treated | control_keep(d), ]

  # per-cell influence functions, summed within district
  cells <- d |>
    group_by(cohort, event_time) |>
    group_modify(function(x, key) {
      t <- x$treated
      if (!any(t)) return(tibble())
      phi   <- numeric(nrow(x))
      th_t  <- mean(x$res[t])
      phi[t] <- (x$res[t] - th_t) / sum(t)
      if (key$event_time < 0 && any(!t)) {
        r <- x$r[!t]
        th_c <- sum(r * x$res[!t]) / sum(r)
        phi[!t] <- -r * (x$res[!t] - th_c) / sum(r)
      }
      tibble(district = x$district, phi = phi)
    }) |>
    ungroup() |>
    group_by(cohort, event_time, district) |>
    summarise(phi = sum(phi), .groups = "drop")

  # cohort weights per event time
  info <- cells |> distinct(cohort, event_time) |> arrange(event_time, cohort) |>
    mutate(cell = row_number()) |> left_join(n_g, by = "cohort")
  cells <- cells |> left_join(info |> select(cohort, event_time, cell), by = c("cohort", "event_time"))
  ids <- sort(unique(cells$district))
  Phi <- Matrix::sparseMatrix(i = match(cells$district, ids), j = cells$cell, x = cells$phi,
                              dims = c(length(ids), nrow(info)))
  es <- sort(unique(info$event_time))
  A  <- matrix(0, nrow(info), length(es))
  for (j in seq_along(es)) {
    k <- which(info$event_time == es[j])
    A[k, j] <- info$n_cohort[k] / sum(info$n_cohort[k])
  }

  # multiplier bootstrap
  set.seed(seed)
  xi    <- matrix(sample(c(-1, 1), length(ids) * B, replace = TRUE), length(ids), B)
  draws <- crossprod(A, as.matrix(Matrix::crossprod(Phi, xi)))
  se    <- apply(draws, 1, function(v) diff(quantile(v, c(.25, .75))) / (qnorm(.75) - qnorm(.25)))
  se[!is.finite(se) | se <= 0] <- NA_real_
  ok    <- !is.na(se)
  crit  <- if (any(ok)) as.numeric(quantile(apply(abs(draws[ok, , drop = FALSE]) / se[ok], 2, max), 0.95)) else NA_real_
  tibble(event_time = es, se = se, crit_uniform = ifelse(ok, crit, NA_real_))
}

# ==============================================================================
# SECTION 5: HETEROGENEITY ANALYSIS
# ==============================================================================

#' NA -> 0 plus one `<var>_miss` dummy per distinct missingness pattern; drops
#' all-NA vars. Returns list(data, vars).
add_missing_indicators <- function(df, vars) {
  all_na  <- vars[vapply(vars, function(v) all(is.na(df[[v]])), logical(1))]
  vars    <- setdiff(vars, all_na)
  na_vars <- vars[vapply(vars, function(v) anyNA(df[[v]]), logical(1))]

  dummies <- character(0)
  for (v in na_vars) {
    miss <- as.numeric(is.na(df[[v]]))
    dup  <- Find(function(d) identical(df[[d]], miss), dummies)
    if (is.null(dup)) {
      d <- paste0(v, "_miss")
      df[[d]] <- miss
      dummies <- c(dummies, d)
    } else {
      message(sprintf("    missing dummy for %s duplicates %s -- merged", v, dup))
    }
    df[[v]][is.na(df[[v]])] <- 0
  }
  if (length(all_na) > 0)
    message("    dropped all-NA vars: ", paste(all_na, collapse = ", "))
  list(data = df, vars = c(vars, dummies))
}

#' Display label for a moderator or its missing dummy.
covariate_label <- function(v) {
  base <- sub("_miss$", "", v)
  lbl  <- if (base %in% names(COVARIATE_NAMES)) COVARIATE_NAMES[[base]] else base
  if (grepl("_miss$", v)) paste0(lbl, " (missing)") else lbl
}

#' Moderator means for units with positive vs. negative mean post-period CATT
#' (Welch t-tests).
compute_dim <- function(results, mod_vars) {
  available <- intersect(mod_vars, names(results$catt))

  post_means <- results$catt |>
    filter(event_time > 0, !is.na(catt)) |>
    group_by(unit_id) |>
    summarise(
      mean_catt = mean(catt),
      across(all_of(available), mean),
      .groups = "drop"
    ) |>
    mutate(pos_catt = (mean_catt > 0))

  if (sum(post_means$pos_catt) < 5 || sum(!post_means$pos_catt) < 5) {
    return(NULL)
  }

  map_dfr(available, function(v) {
    tt <- tryCatch(
      t.test(post_means[[v]] ~ post_means$pos_catt, var.equal = FALSE),
      error = function(e) NULL
    )
    if (is.null(tt)) return(NULL)
    tibble(
      variable   = v,
      mean_pos   = tt$estimate[[2]],
      mean_neg   = tt$estimate[[1]],
      difference = diff(tt$estimate),
      std_err    = tt$stderr,
      p_value    = tt$p.value
    )
  }) |>
    arrange(p_value)
}

#' BLP: mean post-period CATT regressed on all moderators (dollars in $1,000s,
#' missing indicators). dose = "linear" adds the mean post-period dose
#' (mean_dose); dose = "bins" adds BLP_DOSE_BINS dose-quantile fixed effects.
#' NULL if fewer than 5 units.
compute_blp <- function(results, mod_vars, dose = c("none", "linear", "bins")) {
  dose <- match.arg(dose)
  available <- intersect(mod_vars, names(results$catt))

  post_data <- results$catt |>
    filter(event_time > 0, !is.na(catt)) |>
    group_by(unit_id) |>
    summarise(
      mean_catt = mean(catt),
      mean_dose = mean(dose_t),
      across(all_of(available), mean),
      .groups = "drop"
    ) |>
    mutate(dose_bin = ntile(mean_dose, BLP_DOSE_BINS))

  if (nrow(post_data) < 5) return(NULL)

  dollar_vars <- intersect(BLP_DOLLAR_VARS, available)
  post_data <- post_data |> mutate(across(all_of(dollar_vars), ~ .x / 1000))

  mi  <- add_missing_indicators(post_data, available)
  rhs <- switch(dose, none = mi$vars, linear = c(mi$vars, "mean_dose"),
                bins = c(mi$vars, "factor(dose_bin)"))
  lm(reformulate(rhs, response = "mean_catt"), data = mi$data)
}

#' compute_dim() for tau (CACRT).
compute_dim_tau <- function(results, mod_vars) {
  available <- intersect(mod_vars, names(results$catt))

  post_means <- results$catt |>
    filter(event_time > 0, !is.na(tau)) |>
    group_by(unit_id) |>
    summarise(
      mean_tau = mean(tau),
      across(all_of(available), mean),
      .groups = "drop"
    ) |>
    mutate(pos_tau = (mean_tau > 0))

  if (sum(post_means$pos_tau) < 5 || sum(!post_means$pos_tau) < 5) {
    return(NULL)
  }

  map_dfr(available, function(v) {
    tt <- tryCatch(
      t.test(post_means[[v]] ~ post_means$pos_tau, var.equal = FALSE),
      error = function(e) NULL
    )
    if (is.null(tt)) return(NULL)
    tibble(
      variable   = v,
      mean_pos   = tt$estimate[[2]],
      mean_neg   = tt$estimate[[1]],
      difference = diff(tt$estimate),
      std_err    = tt$stderr,
      p_value    = tt$p.value
    )
  }) |>
    arrange(p_value)
}

#' compute_blp() for tau (CACRT).
compute_blp_tau <- function(results, mod_vars) {
  available <- intersect(mod_vars, names(results$catt))

  post_data <- results$catt |>
    filter(event_time > 0, !is.na(tau)) |>
    group_by(unit_id) |>
    summarise(
      mean_tau = mean(tau),
      across(all_of(available), mean),
      .groups = "drop"
    )

  if (nrow(post_data) < 5) return(NULL)

  dollar_vars <- intersect(BLP_DOLLAR_VARS, available)
  post_data <- post_data |> mutate(across(all_of(dollar_vars), ~ .x / 1000))

  mi <- add_missing_indicators(post_data, available)
  fml <- reformulate(mi$vars, response = "mean_tau")
  lm(fml, data = mi$data)
}

#' Mean VIF by moderator x event time, sorted by overall mean.
build_vif_summary <- function(results) {
  results$vif |>
    group_by(variable, event_time) |>
    summarise(importance = mean(importance, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from  = event_time,
                values_from = importance,
                names_prefix = "h") |>
    mutate(overall = rowMeans(pick(starts_with("h")), na.rm = TRUE)) |>
    arrange(desc(overall))
}

#' Top n moderators by mean VIF at event_time == 1 (e = 0).
get_top_moderators <- function(results, n = 5) {
  results$vif |>
    filter(event_time == 1, !is.na(importance)) |>
    group_by(variable) |>
    summarise(importance = mean(importance, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(importance)) |>
    slice_head(n = n) |>
    pull(variable)
}

#' Event study by moderator group (bottom quartile, middle, top quartile) of
#' value_col. With if_data, SEs come from the bootstrap (all event times for
#' catt, pre-period only for tau). cuts = c(q25, q75) overrides the quartile
#' cut points (used when results is a subset of treated units).
aggregate_event_study_by_quartile <- function(results, mod_var,
                                              value_col = "catt", se_col = "catt_se",
                                              if_data = NULL, cuts = NULL) {
  catt_df <- results$catt
  if (!mod_var %in% names(catt_df)) return(NULL)

  # quartile group per unit
  unit_mod <- catt_df |>
    group_by(unit_id) |>
    slice(1) |>
    ungroup() |>
    select(unit_id, mod_val = all_of(mod_var)) |>
    filter(!is.na(mod_val))

  q25 <- if (is.null(cuts)) quantile(unit_mod$mod_val, 0.25, na.rm = TRUE) else cuts[[1]]
  q75 <- if (is.null(cuts)) quantile(unit_mod$mod_val, 0.75, na.rm = TRUE) else cuts[[2]]

  grp_levels <- c("Top quartile", "Middle (Q2-Q3)", "Bottom quartile")

  unit_mod <- unit_mod |>
    mutate(quartile_group = factor(
      case_when(
        mod_val <= q25 ~ "Bottom quartile",
        mod_val >= q75 ~ "Top quartile",
        TRUE           ~ "Middle (Q2-Q3)"
      ),
      levels = grp_levels
    ))

  catt_aug <- inner_join(catt_df,
                         unit_mod |> select(unit_id, quartile_group),
                         by = "unit_id") |>
    mutate(.value = .data[[value_col]], .se = .data[[se_col]])

  # cohort weights within each group
  grp_cohort_w <- catt_aug |>
    distinct(quartile_group, cohort, unit_id) |>
    count(quartile_group, cohort, name = "n_cohort") |>
    group_by(quartile_group) |>
    mutate(phi = n_cohort / sum(n_cohort)) |>
    ungroup()

  # step 1: cohort x event-time means and variances
  cohort_level <- catt_aug |>
    left_join(grp_cohort_w |> select(quartile_group, cohort, phi),
              by = c("quartile_group", "cohort")) |>
    group_by(quartile_group, cohort, event_time) |>
    summarise(
      phi    = first(phi),
      n_g    = sum(!is.na(.value)),
      mean_g = mean(.value, na.rm = TRUE),
      n_se   = sum(!is.na(.se)),
      var_g  = if (n_se > 0) sum(.se^2, na.rm = TRUE) / n_se^2 else NA_real_,
      .groups = "drop"
    ) |>
    filter(n_g > 0)

  # step 2: combine cohorts
  out <- cohort_level |>
    group_by(quartile_group, event_time) |>
    mutate(phi_norm = phi / sum(phi)) |>
    summarise(
      mean    = sum(phi_norm * mean_g),
      se      = if (any(!is.na(var_g))) sqrt(sum(phi_norm^2 * var_g, na.rm = TRUE)) else NA_real_,
      n_units = sum(n_g),
      .groups = "drop"
    )
  if (is.null(if_data)) return(out)

  # bootstrap SEs per group; controls grouped by their own moderator value
  grp_of <- function(v) case_when(is.na(v)  ~ NA_character_,
                                  v <= q25  ~ "Bottom quartile",
                                  v >= q75  ~ "Top quartile",
                                  TRUE      ~ "Middle (Q2-Q3)")
  mm <- if_data$mod_matrix
  ctrl_val <- function(d) {
    if (mod_var == "baseline_y") d$baseline_y else mm[[mod_var]][match(d$unit_id, mm$unit_id)]
  }
  ets <- if (value_col == "tau") sort(unique(if_data$rows$event_time[if_data$rows$event_time < 0])) else NULL
  b <- bind_rows(lapply(grp_levels, function(g) {
    ids <- unit_mod$unit_id[unit_mod$quartile_group == g]
    if (length(ids) == 0) return(NULL)
    if_event_study_se(if_data,
                      n_g = grp_cohort_w |> filter(quartile_group == g) |> select(cohort, n_cohort),
                      treated_ids = ids,
                      control_keep = function(d) grp_of(ctrl_val(d)) %in% g,
                      event_times = ets) |>
      mutate(quartile_group = factor(g, levels = grp_levels))
  }))
  out |>
    left_join(b |> rename(se_if = se) |> mutate(.covered = TRUE), by = c("quartile_group", "event_time")) |>
    mutate(se = ifelse(!is.na(.covered), se_if, se)) |>
    select(-se_if, -.covered)
}

# ==============================================================================
# SECTION 5B: POLICY TREE
# ==============================================================================

#' Leaf index of each row of X in policy tree pt.
get_leaf_assignment <- function(pt, X) {
  nodes <- pt$nodes
  n <- nrow(X)
  leaf_ids <- integer(n)
  for (i in seq_len(n)) {
    node_id <- 1L
    while (!nodes[[node_id]]$is_leaf) {
      sv  <- nodes[[node_id]]$split_variable
      thr <- nodes[[node_id]]$split_value
      if (X[i, sv] <= thr) {
        node_id <- nodes[[node_id]]$left_child
      } else {
        node_id <- nodes[[node_id]]$right_child
      }
    }
    leaf_ids[i] <- node_id
  }
  leaf_ids
}

#' Policy-tree inputs: Gamma = (0, mean post-period DR score) per unit, X = top
#' n_top VIF moderators plus missing indicators. NULL if fewer than 20 units.
compute_policy_tree_inputs <- function(results, mod_vars, n_top = 10L) {
  catt_df <- results$catt

  post_df <- catt_df |>
    filter(event_time > 0, !is.na(dr_score))

  if (nrow(post_df) < 20L) {
    warning("Too few treated rows with DR scores for policy tree (n=", nrow(post_df), ")")
    return(NULL)
  }

  # mean DR score per unit
  unit_df <- post_df |>
    group_by(unit_id) |>
    summarise(
      mean_dr_score = mean(dr_score, na.rm = TRUE),
      .groups = "drop"
    )

  # top-n_top VIF moderators, from each unit's first post-period row
  top_vars <- get_top_moderators(results, n = n_top)
  policy_vars <- intersect(top_vars, colnames(post_df))

  covariate_df <- post_df |>
    group_by(unit_id) |>
    slice_min(event_time, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(unit_id, all_of(policy_vars))

  unit_df <- unit_df |>
    inner_join(covariate_df, by = "unit_id")

  mi <- add_missing_indicators(unit_df, policy_vars)
  unit_df <- mi$data
  X_mat <- unit_df |>
    select(all_of(mi$vars)) |>
    as.matrix()

  complete_rows <- complete.cases(X_mat)
  n_dropped <- sum(!complete_rows)
  if (n_dropped > 0)
    message(sprintf("  Policy tree: dropping %d units with NA in top-%d VIF vars", n_dropped, n_top))
  X_mat   <- X_mat[complete_rows, , drop = FALSE]
  unit_df <- unit_df[complete_rows, , drop = FALSE]

  if (ncol(X_mat) == 0L || nrow(X_mat) < 20L) {
    warning("Insufficient covariates or units for policy tree.")
    return(NULL)
  }

  Gamma <- cbind(0, unit_df$mean_dr_score)

  list(X = X_mat, Gamma = Gamma, unit_df = unit_df)
}

#' Draw a policy tree to PDF (base graphics); layout scales with depth.
draw_policy_tree_pdf <- function(pt, pt_inputs, label, covariate_names, path) {
  nodes     <- pt$nodes
  X         <- pt_inputs$X
  col_names <- colnames(X)

  readable <- function(col_idx) covariate_label(col_names[col_idx])

  # breadth-first node positions
  queue  <- list(list(id = 1L, level = 1L, pos = 0.5))
  layout <- list()
  while (length(queue) > 0) {
    item  <- queue[[1]]
    queue <- queue[-1]
    node  <- nodes[[item$id]]
    layout[[item$id]] <- list(level = item$level, pos = item$pos)
    if (!node$is_leaf) {
      half <- 0.5 / (2^item$level)
      queue <- c(queue,
        list(list(id = node$left_child,  level = item$level + 1L, pos = item$pos - half)),
        list(list(id = node$right_child, level = item$level + 1L, pos = item$pos + half))
      )
    }
  }

  assignments <- get_leaf_assignment(pt, X)
  n_levels    <- max(sapply(layout, `[[`, "level"))
  tree_depth  <- n_levels - 1L

  # box and text sizes by depth
  if (tree_depth <= 2L) {
    fig_w      <- 12;    fig_h      <- 7
    box_lf_dx  <- 0.09;  box_lf_dy  <- 0.075
    box_int_dx <- 0.135; box_int_dy <- 0.082
    cex_split  <- 0.87;  cex_action <- 0.92
    cex_n      <- 0.80;  cex_label  <- 0.78
    wrap_w     <- 16L;   perp_off   <- 0.022;  label_frac <- 0.28
  } else {
    fig_w      <- 18;    fig_h      <- 9
    box_lf_dx  <- 0.055; box_lf_dy  <- 0.060
    box_int_dx <- 0.075; box_int_dy <- 0.065
    cex_split  <- 0.87;  cex_action <- 0.92
    cex_n      <- 0.80;  cex_label  <- 0.83
    wrap_w     <- 12L;   perp_off   <- 0.022;  label_frac <- 0.42
  }

  wrap_label <- function(lbl) paste(strwrap(lbl, width = wrap_w), collapse = "\n")

  y_pos <- function(lv) 0.88 - (lv - 1) / (n_levels - 1) * 0.76

  pdf(path, width = fig_w, height = fig_h)
  par(mar = c(0.5, 0.5, 2, 0.5))
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0.02, 1.0))
  title(main = paste0("Policy Tree (depth ", tree_depth, "): ", label),
        cex.main = 1.25, font.main = 2)

  # edges
  for (id in seq_along(nodes)) {
    node <- nodes[[id]]
    if (!node$is_leaf) {
      x0 <- layout[[id]]$pos
      y0 <- y_pos(layout[[id]]$level)
      for (child_id in c(node$left_child, node$right_child)) {
        segments(x0, y0, layout[[child_id]]$pos, y_pos(layout[[child_id]]$level),
                 col = "grey60", lwd = 1.2)
      }
    }
  }

  # nodes
  for (id in seq_along(nodes)) {
    node <- nodes[[id]]
    x    <- layout[[id]]$pos
    y    <- y_pos(layout[[id]]$level)

    if (node$is_leaf) {
      action_label <- if (node$action == 2L) "Expand" else "Don't Expand"
      n_leaf       <- sum(assignments == id)
      fill_col     <- if (node$action == 2L) "#4E79A7" else "#E15759"
      rect(x - box_lf_dx, y - box_lf_dy, x + box_lf_dx, y + box_lf_dy,
           col = fill_col, border = "white", lwd = 1.5)
      text(x, y + box_lf_dy * 0.30, action_label, col = "white", cex = cex_action, font = 2)
      text(x, y - box_lf_dy * 0.40, paste0("n = ", n_leaf), col = "white", cex = cex_n)
    } else {
      sv_label  <- wrap_label(readable(node$split_variable))
      thr_label <- formatC(node$split_value, format = "g", digits = 5)
      split_txt <- paste0(sv_label, "\n<= ", thr_label)
      rect(x - box_int_dx, y - box_int_dy, x + box_int_dx, y + box_int_dy,
           col = "#F5F0E8", border = "#888888", lwd = 1.2)
      text(x, y, split_txt, cex = cex_split, adj = c(0.5, 0.5))

      # Yes/No labels beside each edge
      lc  <- node$left_child
      rc  <- node$right_child
      xl1 <- layout[[lc]]$pos;  yl1 <- y_pos(layout[[lc]]$level)
      xr1 <- layout[[rc]]$pos;  yr1 <- y_pos(layout[[rc]]$level)

      lx <- x + label_frac * (xl1 - x);  ly <- y + label_frac * (yl1 - y)
      dl <- c(xl1 - x, yl1 - y);         dl <- dl / sqrt(sum(dl^2))
      perp_l <- c(dl[2], -dl[1])
      text(lx + perp_off * perp_l[1], ly + perp_off * perp_l[2],
           "Yes", cex = cex_label, col = "grey20", font = 2)

      rx <- x + label_frac * (xr1 - x);  ry <- y + label_frac * (yr1 - y)
      dr <- c(xr1 - x, yr1 - y);         dr <- dr / sqrt(sum(dr^2))
      perp_r <- c(-dr[2], dr[1])
      text(rx + perp_off * perp_r[1], ry + perp_off * perp_r[2],
           "No", cex = cex_label, col = "grey20", font = 2)
    }
  }
  dev.off()
  invisible(path)
}

#' Leaf summary table for a policy tree (bare tabular).
save_policy_tree_table <- function(pt, pt_inputs, outcome_name, covariate_names,
                                   tbl_dir = here("paper", "tables", "DiDCF_analysis")) {
  nodes       <- pt$nodes
  X           <- pt_inputs$X
  unit_df     <- pt_inputs$unit_df
  assignments <- get_leaf_assignment(pt, X)
  col_names   <- colnames(X)

  readable <- function(raw) {
    if (!is.null(covariate_names) && raw %in% names(covariate_names))
      covariate_names[[raw]]
    else
      raw
  }

  rows <- list()
  for (id in seq_along(nodes)) {
    node <- nodes[[id]]
    if (!node$is_leaf) next
    idx         <- which(assignments == id)
    action_lbl  <- if (node$action == 2L) "Expand" else "Don't Expand"
    mean_dr     <- mean(unit_df$mean_dr_score[idx], na.rm = TRUE)
    rows[[length(rows) + 1L]] <- data.frame(
      Leaf        = as.integer(length(rows) + 1L),
      Action      = action_lbl,
      N           = length(idx),
      Mean_DR     = round(mean_dr, 4),
      stringsAsFactors = FALSE
    )
  }
  tbl <- do.call(rbind, rows)

  header <- paste0(
    "\\begin{tabular}{lllr}\n",
    "\\toprule\n",
    "Leaf & Action & $N$ & Mean DR Score \\\\\n",
    "\\midrule\n"
  )
  body <- paste(
    apply(tbl, 1, function(r) paste(r, collapse = " & ")),
    collapse = " \\\\\n"
  )
  footer <- "\\\\\n\\bottomrule\n\\end{tabular}\n"

  out_path <- file.path(
    tbl_dir,
    paste0("didcf_policytree_", outcome_name, ".tex")
  )
  writeLines(paste0(header, body, footer), out_path)
  message("  Saved policy tree table: ", out_path)
  invisible(out_path)
}

# ==============================================================================
# SECTION 5C: NEIGHBORHOOD EVENT STUDIES
# ==============================================================================
# k-means clusters of treated units on standardized moderators (optionally
# VIF-weighted), then the cohort-weighted event study within each cluster.

#' Mean VIF per moderator at event_time == 1 (e = 0).
get_vif_weights <- function(results) {
  w <- results$vif |>
    filter(event_time == 1, !is.na(importance)) |>
    group_by(variable) |>
    summarise(importance = mean(importance, na.rm = TRUE), .groups = "drop")
  setNames(w$importance, w$variable)
}

#' k-means clusters of treated units on standardized moderators (NA -> column
#' mean; columns scaled by sqrt(VIF) if vif_importance is given). Clusters
#' smaller than min_treated merge into the nearest cluster. Returns unit_id,
#' cluster_id.
build_treated_neighborhoods <- function(results, mod_vars, k = MATCH_N_NEIGHBORHOODS,
                                        min_treated = MATCH_MIN_TREATED_PER_NEIGHBORHOOD,
                                        vif_importance = NULL) {
  available <- intersect(mod_vars, names(results$catt))
  unit_X <- results$catt |>
    group_by(unit_id) |>
    slice(1) |>
    ungroup() |>
    select(unit_id, all_of(available))

  Xs <- scale(unit_X |> select(all_of(available)) |> as.matrix())
  Xs[!is.finite(Xs)] <- 0

  if (!is.null(vif_importance)) {
    w <- vif_importance[colnames(Xs)]
    w[is.na(w)] <- 0
    Xs <- sweep(Xs, 2, sqrt(w), "*")
  }

  set.seed(42)
  km <- kmeans(Xs, centers = k, nstart = 10, iter.max = 50)
  cluster_id <- km$cluster
  centers    <- km$centers

  # merge small clusters into the nearest surviving one
  sizes <- table(cluster_id)
  small <- as.integer(names(sizes)[sizes < min_treated])
  keep  <- setdiff(seq_len(k), small)
  if (length(small) > 0 && length(keep) == 0)
    stop("All neighborhoods fell below min_treated -- lower MATCH_N_NEIGHBORHOODS or min_treated.")

  for (s in small) {
    d <- sqrt(rowSums(sweep(centers[keep, , drop = FALSE], 2, centers[s, ], "-")^2))
    cluster_id[cluster_id == s] <- keep[which.min(d)]
  }

  tibble(unit_id = unit_X$unit_id, cluster_id = cluster_id)
}

#' Cohort-weighted event study of value_col within each neighborhood.
aggregate_event_study_by_neighborhood <- function(results, neighborhoods,
                                                   value_col = "catt", se_col = "catt_se") {
  catt_aug <- inner_join(results$catt, neighborhoods, by = "unit_id") |>
    mutate(.value = .data[[value_col]], .se = .data[[se_col]])

  grp_cohort_w <- catt_aug |>
    distinct(cluster_id, cohort, unit_id) |>
    count(cluster_id, cohort, name = "n_cohort") |>
    group_by(cluster_id) |>
    mutate(phi = n_cohort / sum(n_cohort)) |>
    ungroup()

  cohort_level <- catt_aug |>
    left_join(grp_cohort_w |> select(cluster_id, cohort, phi), by = c("cluster_id", "cohort")) |>
    group_by(cluster_id, cohort, event_time) |>
    summarise(
      phi    = first(phi),
      n_g    = sum(!is.na(.value)),
      mean_g = mean(.value, na.rm = TRUE),
      n_se   = sum(!is.na(.se)),
      var_g  = if (n_se > 0) sum(.se^2, na.rm = TRUE) / n_se^2 else NA_real_,
      .groups = "drop"
    ) |>
    filter(n_g > 0)

  cohort_level |>
    group_by(cluster_id, event_time) |>
    mutate(phi_norm = phi / sum(phi)) |>
    summarise(
      mean    = sum(phi_norm * mean_g),
      se      = if (any(!is.na(var_g))) sqrt(sum(phi_norm^2 * var_g, na.rm = TRUE)) else NA_real_,
      n_units = sum(n_g),
      .groups = "drop"
    )
}

# ==============================================================================
# SECTION 5D: STATE-LEVEL AGGREGATION
# ==============================================================================
# The event-study aggregation run within each state, averaged over
# STATE_MAP_EVENT_TIMES.

#' district -> state lookup.
load_district_states <- function(cfg) {
  read_dta(here("data", "cleaned", cfg$data_file),
           col_select = c("district", "statename", "postal")) |>
    haven::zap_labels() |>
    distinct(district, statename, postal)
}

#' One row per state: catt_state, n_event_times, n_units, n_districts.
aggregate_state_catt <- function(results, district_states,
                                 event_times = STATE_MAP_EVENT_TIMES) {
  catt <- results$catt |> left_join(district_states, by = "district")
  stopifnot(!anyNA(catt$postal))

  es <- catt |>
    group_by(postal, statename) |>
    group_modify(function(d, key) aggregate_event_study_generic(list(catt = d), "catt", "catt_se")) |>
    ungroup() |>
    filter(event_time %in% event_times)

  n_units <- catt |>
    filter(event_time %in% event_times) |>
    group_by(postal) |>
    summarise(n_units = n_distinct(unit_id), n_districts = n_distinct(district), .groups = "drop")

  es |>
    group_by(postal, statename) |>
    summarise(catt_state = mean(mean), n_event_times = n(), .groups = "drop") |>
    left_join(n_units, by = "postal")
}

# ==============================================================================
# SECTION 5E: QUARTILE EVENT STUDIES WITHIN DOSE GROUPS
# ==============================================================================
# Treated units are split into terciles of mean post-period dose; within each
# tercile, the quartile event study of Section 5 (moderator cut points from all
# treated units, bootstrap SEs) is computed.

#' Dose group (DOSE_GROUP_LEVELS) per treated unit, from terciles of its mean
#' post-period dose, with a facet label giving the dose range and unit count.
assign_dose_groups <- function(results) {
  results$catt |>
    filter(event_time > 0) |>
    group_by(unit_id) |>
    summarise(mean_dose = mean(dose_t), .groups = "drop") |>
    mutate(dose_group = DOSE_GROUP_LEVELS[ntile(mean_dose, length(DOSE_GROUP_LEVELS))]) |>
    group_by(dose_group) |>
    mutate(dose_label = sprintf("%s dose (%.1f-%.1f%%, n = %d)", dose_group,
                                100 * min(mean_dose), 100 * max(mean_dose), n())) |>
    ungroup()
}

#' aggregate_event_study_by_quartile() within each dose group, with the
#' moderator quartile cut points taken from all treated units.
aggregate_event_study_by_quartile_dose <- function(results, mod_var, if_data = NULL) {
  if (!mod_var %in% names(results$catt)) return(NULL)
  dose_groups <- assign_dose_groups(results)

  mod_val <- results$catt |> group_by(unit_id) |> slice(1) |> ungroup() |> pull(all_of(mod_var))
  cuts <- quantile(mod_val, c(0.25, 0.75), na.rm = TRUE)

  bind_rows(lapply(DOSE_GROUP_LEVELS, function(lv) {
    dg  <- dose_groups |> filter(dose_group == lv)
    sub <- list(catt = results$catt |> filter(unit_id %in% dg$unit_id))
    aggregate_event_study_by_quartile(sub, mod_var, if_data = if_data, cuts = cuts) |>
      mutate(dose_group = lv, dose_label = first(dg$dose_label))
  }))
}

# ==============================================================================
# SECTION 6: FIGURES
# ==============================================================================

#' Histogram of post-period unit x event-time estimates. The 5% tails are
#' trimmed for display; the ATT line uses all estimates.
plot_catt_hist <- function(results, label, path, value_col = "catt", label_prefix = "CATT") {
  post_data <- results$catt |>
    filter(event_time > 0, !is.na(.data[[value_col]]))

  att <- mean(post_data[[value_col]], na.rm = TRUE)

  trim_lo <- quantile(post_data[[value_col]], 0.05, na.rm = TRUE)
  trim_hi <- quantile(post_data[[value_col]], 0.95, na.rm = TRUE)
  plot_data <- post_data |> filter(.data[[value_col]] >= trim_lo, .data[[value_col]] <= trim_hi)

  p <- ggplot(plot_data, aes(x = .data[[value_col]])) +
    geom_histogram(bins = 50, fill = "#2171b5", color = "white", linewidth = 0.15) +
    geom_vline(xintercept = att, linetype = "dashed", linewidth = 0.5) +
    annotate("text",
             x = att, y = Inf,
             label = paste0("ATT = ", round(att, 3)),
             hjust = -0.1, vjust = 1.5,
             size = 5, family = "serif") +
    labs(
      x = paste0("District × Event-Time ", label_prefix, " — ", label),
      y = "District × Event-Times"
    ) +
    theme_minimal(base_family = "serif", base_size = 15) +
    theme(
      panel.grid.minor = element_blank(),
      axis.title        = element_text(size = 16),
      axis.text         = element_text(size = 14)
    )

  ggsave(path, plot = p, width = 6.5, height = 4.5, device = "pdf")
  invisible(p)
}

# Plot axis uses e = t - g (base year at e = -1); internal event_time = e + 1.
to_plot_event_time <- function(event_time) event_time - 1
PLOT_EVENT_BREAKS <- -4:6
PLOT_TREATMENT_LINE <- -0.5
PLOT_EVENT_XLAB <- "Event Time"

#' Event-study plot of <prefix>_mean. With <prefix>_crit, the ribbon is the
#' uniform band and error bars are pointwise 95%; otherwise the ribbon is pointwise.
plot_event_study <- function(agg, label, path, prefix = "catt", y_label = "Mean CATT") {
  mean_col <- paste0(prefix, "_mean"); se_col <- paste0(prefix, "_se")
  crit_col <- paste0(prefix, "_crit")
  if (!crit_col %in% names(agg)) agg[[crit_col]] <- NA_real_

  # add the base period (0 by construction); keep e = -4..6
  base_row <- tibble(event_time = 0, n_units = NA_integer_) |>
    mutate(!!mean_col := 0, !!se_col := 0, !!crit_col := 0)
  plot_data <- bind_rows(agg, base_row) |>
    filter(event_time %in% -3:7) |>
    arrange(event_time) |>
    rename(.mean = all_of(mean_col), .se = all_of(se_col), .crit = all_of(crit_col))

  if (all(is.na(plot_data$.mean[plot_data$event_time != 0]))) {
    message("  Skipping event study plot (no estimates): ", basename(path))
    return(invisible(NULL))
  }
  plot_data <- plot_data |> mutate(event_time = to_plot_event_time(event_time))
  uniform   <- any(!is.na(plot_data$.crit[plot_data$event_time != -1]))
  band_data <- if (uniform) filter(plot_data, !is.na(.crit)) else mutate(plot_data, .crit = 1.96)

  p <- ggplot(plot_data, aes(x = event_time, y = .mean)) +
    geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
    geom_vline(xintercept = PLOT_TREATMENT_LINE, linetype = "dashed", color = "grey60", linewidth = 0.4) +
    geom_ribbon(data = band_data, aes(ymin = .mean - .crit * .se, ymax = .mean + .crit * .se),
                alpha = 0.15, fill = "steelblue") +
    { if (uniform) geom_errorbar(aes(ymin = .mean - 1.96 * .se, ymax = .mean + 1.96 * .se),
                                 color = "steelblue", width = 0.15, linewidth = 0.5) } +
    geom_line(color  = "steelblue", linewidth = 0.9) +
    geom_point(color = "steelblue", size = 2.2) +
    scale_x_continuous(breaks = PLOT_EVENT_BREAKS) +
    labs(x = PLOT_EVENT_XLAB,
         y = paste(y_label, "—", label)) +
    theme_minimal(base_family = "serif", base_size = 15) +
    theme(
      panel.grid.minor = element_blank(),
      axis.title        = element_text(size = 16),
      axis.text         = element_text(size = 14)
    )

  ggsave(path, plot = p, width = 7.5, height = 5, device = "pdf")
  invisible(p)
}

#' Neighborhood event-study lines (grey) with the overall event study (red). The
#' red band is the uniform 95% band if <prefix>_crit covers every event time,
#' otherwise pointwise 95%.
plot_neighborhood_event_study <- function(agg_neighborhoods, agg_overall, label, path,
                                          prefix = "catt", y_label = "Mean CATT") {
  mean_col <- paste0(prefix, "_mean"); se_col <- paste0(prefix, "_se")
  crit_col <- paste0(prefix, "_crit")

  base_row_n <- agg_neighborhoods |> distinct(cluster_id) |> mutate(event_time = 0, mean = 0)
  neigh_data <- bind_rows(agg_neighborhoods |> select(cluster_id, event_time, mean), base_row_n) |>
    filter(event_time %in% -3:7) |>
    mutate(cluster_id = factor(cluster_id))

  base_row_o <- tibble(event_time = 0) |> mutate(!!mean_col := 0, !!se_col := 0)
  overall_data <- bind_rows(agg_overall, base_row_o) |>
    filter(event_time %in% -3:7) |>
    rename(mean = all_of(mean_col), se = all_of(se_col))
  crit <- if (crit_col %in% names(overall_data)) overall_data[[crit_col]][overall_data$event_time != 0] else NA
  overall_data$crit <- if (length(crit) > 0 && !anyNA(crit)) crit[1] else 1.96

  if (all(is.na(neigh_data$mean[neigh_data$event_time != 0]))) {
    message("  Skipping neighborhood event study plot (no estimates): ", basename(path))
    return(invisible(NULL))
  }
  neigh_data   <- neigh_data   |> mutate(event_time = to_plot_event_time(event_time))
  overall_data <- overall_data |> mutate(event_time = to_plot_event_time(event_time))

  p <- ggplot() +
    geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
    geom_vline(xintercept = PLOT_TREATMENT_LINE, linetype = "dashed", color = "grey60", linewidth = 0.4) +
    geom_line(data = neigh_data, aes(x = event_time, y = mean, group = cluster_id),
              color = "grey60", linewidth = 0.5, alpha = 0.7) +
    geom_ribbon(data = overall_data,
                aes(x = event_time, ymin = mean - crit * se, ymax = mean + crit * se),
                inherit.aes = FALSE, alpha = 0.15, fill = "firebrick", na.rm = TRUE) +
    geom_line(data = overall_data, aes(x = event_time, y = mean),
              color = "firebrick", linewidth = 1.4) +
    geom_point(data = overall_data, aes(x = event_time, y = mean),
               color = "firebrick", size = 2.6) +
    scale_x_continuous(breaks = PLOT_EVENT_BREAKS) +
    labs(x = PLOT_EVENT_XLAB,
         y = paste(y_label, "—", label)) +
    theme_minimal(base_family = "serif", base_size = 15) +
    theme(
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 16),
      axis.text  = element_text(size = 14)
    )

  ggsave(path, plot = p, width = 7.5, height = 5.5, device = "pdf")
  invisible(p)
}

#' Bar chart of treated units per neighborhood.
plot_neighborhood_sizes <- function(neighborhoods, label, path) {
  plot_data <- neighborhoods |> count(cluster_id, name = "n_treated")

  p <- ggplot(plot_data, aes(x = factor(cluster_id), y = n_treated)) +
    geom_col(fill = "steelblue", width = 0.6) +
    geom_text(aes(label = n_treated), vjust = -0.4, size = 3.2, family = "serif") +
    labs(x = "Neighborhood", y = paste("Treated districts —", label)) +
    theme_minimal(base_family = "serif", base_size = 13) +
    theme(panel.grid.minor = element_blank())

  ggsave(path, plot = p, width = 8, height = 4.5, device = "pdf")
  invisible(p)
}

#' VIF heatmap (moderators x post-period event times), rows sorted by VIF at e = 0.
plot_vif_heatmap <- function(results, outcome_name, path) {
  vif_agg <- results$vif |>
    group_by(variable, event_time) |>
    summarise(importance = mean(importance, na.rm = TRUE), .groups = "drop") |>
    filter(event_time %in% 1:7)

  present_vars <- vif_agg |> distinct(variable) |> pull(variable)
  all_vars  <- names(COVARIATE_NAMES)[names(COVARIATE_NAMES) %in% present_vars]
  full_grid <- expand.grid(variable  = all_vars, event_time = 1:7,
                            stringsAsFactors = FALSE) |> as_tibble()
  vif_long  <- left_join(full_grid, vif_agg, by = c("variable", "event_time"))

  var_order <- vif_long |>
    filter(event_time == 1) |>
    arrange(desc(importance)) |>
    pull(variable)

  plot_data <- vif_long |>
    mutate(variable = factor(variable, levels = rev(var_order)))

  label_fn <- function(x) {
    lbl <- COVARIATE_NAMES[x]
    ifelse(is.na(lbl), x, lbl)
  }

  p <- ggplot(plot_data, aes(x = factor(to_plot_event_time(event_time)), y = variable, fill = importance)) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_text(
      aes(label = ifelse(is.na(importance), "", sprintf("%.2f", importance))),
      size = 4, family = "serif", color = "black"
    ) +
    scale_fill_gradient(
      low = "#fee0d2", high = "#a50f15",
      name = "Importance", na.value = "grey90"
    ) +
    scale_y_discrete(labels = label_fn) +
    labs(x = "Event Time", y = NULL) +
    theme_minimal(base_family = "serif", base_size = 13) +
    theme(
      panel.grid   = element_blank(),
      axis.text.y  = element_text(hjust = 1, size = 13),
      axis.text.x  = element_text(size = 13),
      legend.title = element_text(size = 13),
      legend.text  = element_text(size = 12),
      legend.position = "right"
    )

  ggsave(path, plot = p, width = 11, height = 8, device = "pdf")
  invisible(p)
}

#' Event study with one line per moderator quartile group.
plot_event_study_quartile <- function(agg_by_quartile, label, mod_label, path,
                                      prefix = "catt", y_label = "Mean CATT") {
  grp_levels <- c("Top quartile", "Middle (Q2-Q3)", "Bottom quartile")

  base_rows <- tibble(
    quartile_group = factor(grp_levels, levels = grp_levels),
    event_time     = 0L,
    mean           = 0,
    se             = 0,
    n_units        = NA_integer_
  )

  plot_data <- bind_rows(agg_by_quartile, base_rows) |>
    filter(event_time %in% -3:7) |>
    mutate(quartile_group = factor(quartile_group, levels = grp_levels)) |>
    arrange(quartile_group, event_time)

  if (all(is.na(plot_data$mean[plot_data$event_time != 0]))) {
    message("  Skipping quartile event study (no estimates): ", basename(path))
    return(invisible(NULL))
  }
  plot_data <- plot_data |> mutate(event_time = to_plot_event_time(event_time))

  group_colors <- c(
    "Top quartile"    = "#2166ac",
    "Middle (Q2-Q3)"  = "#969696",
    "Bottom quartile" = "#d6604d"
  )
  group_shapes <- c(
    "Top quartile"    = 16L,
    "Middle (Q2-Q3)"  = 15L,
    "Bottom quartile" = 17L
  )

  p <- ggplot(plot_data,
              aes(x = event_time, y = mean,
                  color = quartile_group, fill = quartile_group,
                  shape = quartile_group)) +
    geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
    geom_vline(xintercept = PLOT_TREATMENT_LINE, linetype = "dashed", color = "grey60",
               linewidth = 0.4) +
    geom_ribbon(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se),
                alpha = 0.12, color = NA) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.2) +
    scale_x_continuous(breaks = PLOT_EVENT_BREAKS) +
    scale_color_manual(values = group_colors, name = mod_label) +
    scale_fill_manual(values  = group_colors, name = mod_label) +
    scale_shape_manual(values = group_shapes, name = mod_label) +
    labs(
      x = PLOT_EVENT_XLAB,
      y = paste(y_label, "—", label)
    ) +
    theme_minimal(base_family = "serif", base_size = 15) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      legend.title     = element_text(size = 14, face = "bold"),
      legend.text      = element_text(size = 13),
      axis.title       = element_text(size = 16),
      axis.text        = element_text(size = 14)
    )

  ggsave(path, plot = p, width = 8.5, height = 5.5, device = "pdf")
  invisible(p)
}

#' Quartile event study with one panel per dose group (low, middle, high).
plot_event_study_quartile_dose <- function(agg_by_quartile_dose, mod_label, path) {
  grp_levels <- c("Top quartile", "Middle (Q2-Q3)", "Bottom quartile")
  dose_lbls  <- agg_by_quartile_dose |>
    distinct(dose_group, dose_label) |>
    arrange(match(dose_group, DOSE_GROUP_LEVELS)) |>
    pull(dose_label)

  base_rows <- tidyr::expand_grid(quartile_group = grp_levels, dose_label = dose_lbls) |>
    mutate(event_time = 0, mean = 0, se = 0)

  plot_data <- bind_rows(agg_by_quartile_dose |> mutate(quartile_group = as.character(quartile_group)),
                         base_rows) |>
    filter(event_time %in% -3:7) |>
    mutate(event_time     = to_plot_event_time(event_time),
           quartile_group = factor(quartile_group, levels = grp_levels),
           dose_label     = factor(dose_label, levels = dose_lbls))

  group_colors <- c("Top quartile" = "#2166ac", "Middle (Q2-Q3)" = "#969696", "Bottom quartile" = "#d6604d")
  group_shapes <- c("Top quartile" = 16L, "Middle (Q2-Q3)" = 15L, "Bottom quartile" = 17L)

  p <- ggplot(plot_data,
              aes(x = event_time, y = mean,
                  color = quartile_group, fill = quartile_group, shape = quartile_group)) +
    geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
    geom_vline(xintercept = PLOT_TREATMENT_LINE, linetype = "dashed", color = "grey60",
               linewidth = 0.4) +
    geom_ribbon(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se),
                alpha = 0.12, color = NA) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.9) +
    facet_wrap(~ dose_label, nrow = 1) +
    scale_x_continuous(breaks = PLOT_EVENT_BREAKS) +
    scale_color_manual(values = group_colors, name = mod_label) +
    scale_fill_manual(values  = group_colors, name = mod_label) +
    scale_shape_manual(values = group_shapes, name = mod_label) +
    labs(x = PLOT_EVENT_XLAB, y = "Mean CATT") +
    theme_minimal(base_family = "serif", base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold"),
      strip.text       = element_text(face = "bold", size = 12)
    )

  ggsave(path, plot = p, width = 12, height = 4.8, device = "pdf")
  invisible(p)
}

#' US map of the sign of each state's CATT, labeled with the estimate. States in
#' STATE_MAP_CALLOUTS are labeled offshore with leader lines.
plot_state_catt_map <- function(state_catt, path, digits = 3) {
  lvls <- c("Positive", "Negative", "Either always treated or no data")
  map_df <- tibble(state = usmap::statepop$abbr) |>
    left_join(state_catt |> select(state = postal, catt_state), by = "state") |>
    mutate(sign = factor(case_when(is.na(catt_state) ~ lvls[3],
                                   catt_state > 0    ~ lvls[1],
                                   TRUE              ~ lvls[2]), levels = lvls))

  # label positions
  cl  <- usmapdata::centroid_labels("states")
  xy  <- sf::st_coordinates(cl)
  lab <- tibble(state = cl$abbr, cx = xy[, 1], cy = xy[, 2]) |>
    inner_join(map_df |> filter(!is.na(catt_state)), by = "state") |>
    mutate(text    = sprintf(paste0("%.", digits, "f"), catt_state),
           callout = state %in% STATE_MAP_CALLOUTS,
           col     = ifelse(callout | catt_state <= 0, "black", "white"))

  # offshore label column
  bb    <- sf::st_bbox(usmap::us_map("states"))
  col_x <- bb[["xmax"]] + 1.2e5
  out   <- lab |> filter(callout) |> arrange(desc(cy)) |>
    mutate(lx = col_x, ly = 4.6e5 - (row_number() - 1) * 1.15e5)
  ins   <- lab |> filter(!callout)

  p <- plot_usmap(data = map_df, values = "sign", color = "white", linewidth = 0.3) +
    geom_segment(data = out, aes(x = lx - 1.5e4, y = ly, xend = cx, yend = cy),
                 color = "grey35", linewidth = 0.3) +
    geom_point(data = out, aes(x = cx, y = cy), color = "grey20", size = 0.5) +
    geom_text(data = out, aes(x = lx, y = ly, label = paste(state, text)),
              hjust = 0, size = 2.6, family = "serif") +
    geom_text(data = ins, aes(x = cx, y = cy, label = text, color = col),
              size = 2.6, family = "serif", show.legend = FALSE) +
    scale_color_identity() +
    scale_fill_manual(values = setNames(c("#2166ac", "#f4a582", "grey85"), lvls),
                      name = "State CATT", drop = FALSE) +
    coord_sf(crs = sf::st_crs(cl), datum = NA, expand = FALSE,
             xlim = c(bb[["xmin"]], col_x + 5.5e5), ylim = c(bb[["ymin"]], bb[["ymax"]])) +
    theme(text = element_text(family = "serif"),
          legend.position = "bottom",
          legend.title    = element_text(size = 13, face = "bold"),
          legend.text     = element_text(size = 12))

  ggsave(path, plot = p, width = 8, height = 5.5, device = "pdf")
  invisible(p)
}

# ==============================================================================
# SECTION 7: TABLE EXPORT (bare tabular — no \begin{table} wrapper)
# ==============================================================================

#' VIF summary table.
save_vif_table <- function(results, outcome_name,
                           tbl_dir = here("paper", "tables", "DiDCF_analysis")) {
  vif_sum <- build_vif_summary(results)
  path    <- file.path(tbl_dir, paste0("didcf_vif_", outcome_name, ".tex"))

  vif_sum |>
    rename(Variable = variable, Overall = overall) |>
    kable(format = "latex", digits = 3, booktabs = TRUE) |>
    writeLines(path)
}

#' Diff-in-means table (CATT): top n_top VIF moderators at e = 0, sorted by VIF.
save_dim_table <- function(dim_res, outcome_name, results, n_top = 10,
                           tbl_dir = here("paper", "tables", "DiDCF_analysis")) {
  if (is.null(dim_res)) return(invisible(NULL))
  path <- file.path(tbl_dir, paste0("didcf_dim_", outcome_name, ".tex"))

  vif_order <- results$vif |>
    filter(event_time == 1, !is.na(importance)) |>
    group_by(variable) |>
    summarise(importance = mean(importance, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(importance)) |>
    slice_head(n = n_top) |>
    pull(variable)

  dim_res |>
    filter(variable %in% vif_order) |>
    mutate(sort_key = match(variable, vif_order)) |>
    arrange(sort_key) |>
    mutate(Variable = {
      lbl <- COVARIATE_NAMES[variable]
      ifelse(is.na(lbl), variable, lbl)
    }) |>
    select(Variable,
           `Mean (pos. CATT)` = mean_pos,
           `Mean (neg. CATT)` = mean_neg,
           Difference         = difference,
           `p-value`          = p_value) |>
    kable(format = "latex", digits = 2, booktabs = TRUE) |>
    writeLines(path)
}

#' BLP table (CATT): an estimate / SE / p-value block per model in the
#' named list `blp_models` (names are the column headers, e.g. no dose control,
#' linear dose, dose-quintile FE). Rows: intercept, top-10 VIF moderators plus
#' their missing dummies, and the mean charter share. Bare tabular.
save_blp_table <- function(blp_models, outcome_name, results,
                           tbl_dir = here("paper", "tables", "DiDCF_analysis")) {
  blp_models <- compact(blp_models)
  if (length(blp_models) == 0) return(invisible(NULL))
  path <- file.path(tbl_dir, paste0("didcf_blp_", outcome_name, ".tex"))

  top10 <- results$vif |>
    filter(event_time == 1, !is.na(importance)) |>
    group_by(variable) |>
    summarise(importance = mean(importance, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(importance)) |>
    slice_head(n = 10) |>
    pull(variable)

  cs   <- lapply(blp_models, function(m) coef(summary(m))[, c("Estimate", "Std. Error", "Pr(>|t|)"), drop = FALSE])
  rows <- unique(unlist(lapply(cs, rownames)))
  rows <- rows[rows %in% c("(Intercept)", top10, paste0(top10, "_miss"), "mean_dose")]

  row_label <- function(nm) {
    if (nm == "(Intercept)") return("(Intercept)")
    if (nm == "mean_dose")   return("Mean charter share")
    lbl <- covariate_label(nm)
    if (nm %in% BLP_DOLLAR_VARS) lbl <- paste0(lbl, " ($1,000s)")
    gsub("([%$])", "\\\\\\1", lbl)
  }
  # two decimals, no negative zero
  fmt <- function(x) { x <- round(x, 2); x[x == 0] <- 0; sprintf("%.2f", x) }
  cells <- function(nm) unlist(lapply(cs, function(m)
    if (nm %in% rownames(m)) fmt(m[nm, ]) else rep("", 3)))

  body <- vapply(rows, function(nm) paste0(paste(c(row_label(nm), cells(nm)), collapse = " & "), "\\\\"),
                 character(1))
  # \addlinespace every 5 rows, as in the knitr::kable() layout
  brk  <- seq_along(body) %% 5 == 0 & seq_along(body) < length(body)
  body <- as.vector(rbind(body, ifelse(brk, "\\addlinespace", NA)))
  body <- body[!is.na(body)]

  k     <- length(cs)
  first <- 2 + 3 * (seq_len(k) - 1)
  tex <- c(
    sprintf("\\begin{tabular}{l%s}", strrep("r", 3 * k)),
    "\\toprule",
    paste0(" & ", paste(sprintf("\\multicolumn{3}{c}{(%d) %s}", seq_len(k), names(cs)), collapse = " & "), "\\\\"),
    paste(sprintf("\\cmidrule(lr){%d-%d}", first, first + 2), collapse = ""),
    paste0(" & ", paste(rep("Est. & SE & $p$", k), collapse = " & "), "\\\\"),
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}")
  writeLines(tex, path)
}

#' save_dim_table() for tau (CACRT).
save_dim_table_tau <- function(dim_res, outcome_name, results, n_top = 10,
                               tbl_dir = here("paper", "tables", "DiDCF_analysis")) {
  if (is.null(dim_res)) return(invisible(NULL))
  path <- file.path(tbl_dir, paste0("didcf_dim_tau_", outcome_name, ".tex"))

  vif_order <- results$vif |>
    filter(event_time == 1, !is.na(importance)) |>
    group_by(variable) |>
    summarise(importance = mean(importance, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(importance)) |>
    slice_head(n = n_top) |>
    pull(variable)

  dim_res |>
    filter(variable %in% vif_order) |>
    mutate(sort_key = match(variable, vif_order)) |>
    arrange(sort_key) |>
    mutate(Variable = {
      lbl <- COVARIATE_NAMES[variable]
      ifelse(is.na(lbl), variable, lbl)
    }) |>
    select(Variable,
           `Mean (pos. CACRT)` = mean_pos,
           `Mean (neg. CACRT)` = mean_neg,
           Difference          = difference,
           `p-value`           = p_value) |>
    kable(format = "latex", digits = 2, booktabs = TRUE) |>
    writeLines(path)
}

#' save_blp_table() for tau (CACRT).
save_blp_table_tau <- function(blp_model, outcome_name, results,
                               tbl_dir = here("paper", "tables", "DiDCF_analysis")) {
  if (is.null(blp_model)) return(invisible(NULL))
  path <- file.path(tbl_dir, paste0("didcf_blp_tau_", outcome_name, ".tex"))

  top10 <- results$vif |>
    filter(event_time == 1, !is.na(importance)) |>
    group_by(variable) |>
    summarise(importance = mean(importance, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(importance)) |>
    slice_head(n = 10) |>
    pull(variable)

  cs <- coef(summary(blp_model))
  keep_rows <- rownames(cs) %in% c("(Intercept)", top10, paste0(top10, "_miss"))
  cs_sub <- cs[keep_rows, c("Estimate", "Std. Error", "Pr(>|t|)"), drop = FALSE]

  plain_names <- sapply(rownames(cs_sub), function(nm) {
    if (nm == "(Intercept)") return("(Intercept)")
    lbl <- covariate_label(nm)
    if (nm %in% BLP_DOLLAR_VARS) paste0(lbl, " ($1,000s)") else lbl
  })
  rownames(cs_sub) <- plain_names

  knitr::kable(cs_sub, format = "latex", digits = 2, booktabs = TRUE,
               col.names = c("Estimate", "Std. Error", "p-value")) |>
    writeLines(path)
}

#' Neighborhood profile table: raw moderator means per neighborhood (rows sorted
#' by VIF, columns by ATT then ACRT), plus ATT and ACRT rows. Bare tabular inside
#' \resizebox.
save_neighborhood_profile_table <- function(results, neighborhoods, agg_neigh, agg_neigh_tau,
                                            mod_vars, outcome_name,
                                            tbl_dir = here("paper", "tables", "DiDCF_analysis")) {
  available <- intersect(mod_vars, names(results$catt))

  unit_X <- results$catt |>
    group_by(unit_id) |>
    slice(1) |>
    ungroup() |>
    select(unit_id, all_of(available)) |>
    inner_join(neighborhoods, by = "unit_id")

  cluster_means <- unit_X |>
    group_by(cluster_id) |>
    summarise(across(all_of(available), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

  att <- agg_neigh |>
    filter(event_time > 0) |>
    group_by(cluster_id) |>
    summarise(ATT = mean(mean, na.rm = TRUE), .groups = "drop")
  acrt <- agg_neigh_tau |>
    filter(event_time > 0) |>
    group_by(cluster_id) |>
    summarise(ACRT = mean(mean, na.rm = TRUE), .groups = "drop")

  # columns: by rounded ATT, then ACRT
  summary_stats <- inner_join(att, acrt, by = "cluster_id") |>
    arrange(desc(round(ATT, 2)), desc(ACRT))
  cluster_order <- summary_stats$cluster_id
  n_col <- length(cluster_order)

  var_order <- results$vif |>
    filter(event_time == 1, !is.na(importance)) |>
    group_by(variable) |>
    summarise(importance = mean(importance, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(importance)) |>
    pull(variable)
  var_order <- intersect(var_order, available)

  mat <- matrix(NA_real_, nrow = length(var_order), ncol = n_col, dimnames = list(var_order, NULL))
  for (j in seq_along(cluster_order)) {
    row <- cluster_means[cluster_means$cluster_id == cluster_order[j], ]
    mat[, j] <- as.numeric(row[1, var_order])
  }

  # escape LaTeX special characters in labels
  escape_latex <- function(x) {
    x <- gsub("\\\\", "\\\\textbackslash{}", x)
    gsub("([%$#_{}&])", "\\\\\\1", x)
  }

  row_labels <- sapply(var_order, function(v) {
    lbl <- COVARIATE_NAMES[v]; if (is.na(lbl)) v else lbl
  })
  row_labels <- escape_latex(row_labels)
  fmt <- function(x) ifelse(is.finite(x), sprintf("%.2f", x), "--")

  colspec  <- paste0("l", strrep("r", n_col))
  span_row <- sprintf(" & \\multicolumn{%d}{c}{Covariate Grouping} \\\\\n", n_col)
  cmid_row <- sprintf("\\cmidrule(lr){2-%d}\n", n_col + 1)
  num_row  <- paste0(" & ", paste0("(", seq_len(n_col), ")", collapse = " & "), " \\\\\n")

  body <- paste(
    vapply(seq_along(var_order), function(i) {
      paste0(row_labels[i], " & ", paste(fmt(mat[i, ]), collapse = " & "), " \\\\")
    }, character(1)),
    collapse = "\n"
  )

  summary_rows <- paste0(
    "ATT & ",  paste(fmt(summary_stats$ATT),  collapse = " & "), " \\\\\n",
    "ACRT & ", paste(fmt(summary_stats$ACRT), collapse = " & "), " \\\\"
  )

  tex <- paste0(
    "\\resizebox{\\textwidth}{!}{%\n",
    "\\begin{tabular}{", colspec, "}\n",
    "\\toprule\n",
    span_row, cmid_row, num_row,
    "\\midrule\n",
    body, "\n",
    "\\midrule\n",
    summary_rows, "\n",
    "\\bottomrule\n",
    "\\end{tabular}%\n",
    "}\n"
  )

  path <- file.path(tbl_dir, paste0("didcf_neighborhood_profile_vifweighted_", outcome_name, ".tex"))
  writeLines(tex, path)
  invisible(path)
}

# ==============================================================================
# SECTION 8: MAIN EXECUTION PIPELINE
# ==============================================================================

results_dir <- here("quality_reports", "results")
fig_dir_all <- here("paper", "figures", "DiDCF_analysis")
tbl_dir_all <- here("paper", "tables",  "DiDCF_analysis")

dir.create(results_dir,  showWarnings = FALSE, recursive = TRUE)
dir.create(MODELS_DIR,   showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir_all,  showWarnings = FALSE, recursive = TRUE)
dir.create(tbl_dir_all,  showWarnings = FALSE, recursive = TRUE)

all_results <- vector("list", length(OUTCOMES_TO_RUN))
names(all_results) <- OUTCOMES_TO_RUN

for (outcome_name in OUTCOMES_TO_RUN) {
  cfg   <- OUTCOMES[[outcome_name]]
  label <- cfg$label

  message("\n", strrep("=", 60))
  message("Outcome: ", outcome_name, " (", label, ")")
  message(strrep("=", 60))

  # estimation, event studies, heterogeneity summaries
  results      <- run_didcf(cfg, outcome_name)
  if_data      <- if (RUN_ESTIMATION || LOAD_MODELS) build_if_data(cfg, outcome_name) else NULL
  agg          <- aggregate_event_study(results, if_data)
  agg_tau      <- aggregate_event_study_tau(results, if_data)
  dim_res      <- compute_dim(results, MOD_VARS)
  blp_res      <- compute_blp(results, MOD_VARS)
  blp_res_dose <- list(linear = compute_blp(results, MOD_VARS, dose = "linear"),
                       bins   = compute_blp(results, MOD_VARS, dose = "bins"))
  dim_res_tau  <- compute_dim_tau(results, MOD_VARS)
  blp_res_tau  <- compute_blp_tau(results, MOD_VARS)

  # histograms and event studies
  fig_dir <- here("paper", "figures", "DiDCF_analysis")
  plot_catt_hist(
    results, label,
    file.path(fig_dir, paste0("didcf_catt_dist_", outcome_name, ".pdf"))
  )
  plot_catt_hist(
    results, label,
    file.path(fig_dir, paste0("didcf_tau_dist_", outcome_name, ".pdf")),
    value_col = "tau", label_prefix = "CACRT"
  )
  plot_event_study(
    agg, label,
    file.path(fig_dir, paste0("didcf_event_study_", outcome_name, ".pdf"))
  )
  plot_event_study(
    agg_tau, label,
    file.path(fig_dir, paste0("didcf_event_study_tau_", outcome_name, ".pdf")),
    prefix = "tau", y_label = "Mean CACRT"
  )

  # quartile event studies for the top-5 VIF moderators (old files removed first)
  stale <- list.files(
    fig_dir,
    pattern    = sprintf("^didcf_event_study(_tau)?_%s_by_.*\\.pdf$", outcome_name),
    full.names = TRUE
  )
  if (length(stale) > 0) file.remove(stale)

  top_mods <- get_top_moderators(results, n = 5)
  if (length(top_mods) == 0) {
    message("  Skipping quartile event studies (no VIF data available)")
  } else {
    message("  Top 5 moderators (VIF h=1): ", paste(top_mods, collapse = ", "))
    for (mod_var in top_mods) {
      mod_label <- if (mod_var %in% names(COVARIATE_NAMES)) COVARIATE_NAMES[[mod_var]] else mod_var
      safe_name <- gsub("[^A-Za-z0-9_]", "_", mod_var)

      agg_q <- aggregate_event_study_by_quartile(results, mod_var, if_data = if_data)
      if (is.null(agg_q) || nrow(agg_q) == 0) {
        message("  Skipping quartile plot (level) for: ", mod_var)
      } else {
        plot_event_study_quartile(
          agg_q, label, mod_label,
          file.path(fig_dir, sprintf("didcf_event_study_%s_by_%s.pdf",
                                     outcome_name, safe_name))
        )
      }

      agg_q_tau <- aggregate_event_study_by_quartile(results, mod_var,
                                                      value_col = "tau", se_col = "tau_se",
                                                      if_data = if_data)
      if (is.null(agg_q_tau) || nrow(agg_q_tau) == 0) {
        message("  Skipping quartile plot (slope) for: ", mod_var)
      } else {
        plot_event_study_quartile(
          agg_q_tau, label, mod_label,
          file.path(fig_dir, sprintf("didcf_event_study_tau_%s_by_%s.pdf",
                                     outcome_name, safe_name)),
          prefix = "tau", y_label = "Mean CACRT"
        )
      }
    }
  }

  # quartile event studies within low/middle/high dose terciles (Section 5E),
  # top N_DOSE_ES_MODS VIF moderators (old files removed first)
  stale_dose <- list.files(fig_dir, pattern = sprintf("^es_by_dose_%s_by_.*\\.pdf$", outcome_name),
                           full.names = TRUE)
  if (length(stale_dose) > 0) file.remove(stale_dose)

  for (mod_var in get_top_moderators(results, n = N_DOSE_ES_MODS)) {
    mod_label  <- if (mod_var %in% names(COVARIATE_NAMES)) COVARIATE_NAMES[[mod_var]] else mod_var
    agg_q_dose <- aggregate_event_study_by_quartile_dose(results, mod_var, if_data = if_data)
    if (is.null(agg_q_dose) || nrow(agg_q_dose) == 0) {
      message("  Skipping quartile plot (by dose) for: ", mod_var)
    } else {
      plot_event_study_quartile_dose(
        agg_q_dose, mod_label,
        file.path(fig_dir, sprintf("es_by_dose_%s_by_%s.pdf",
                                   outcome_name, gsub("[^A-Za-z0-9_]", "_", mod_var)))
      )
    }
  }

  # neighborhood event studies (unweighted and VIF-weighted k-means)
  neighborhoods     <- build_treated_neighborhoods(results, MOD_VARS)
  vif_w             <- get_vif_weights(results)
  neighborhoods_vif <- build_treated_neighborhoods(results, MOD_VARS, vif_importance = vif_w)

  agg_neigh         <- aggregate_event_study_by_neighborhood(results, neighborhoods)
  agg_neigh_tau     <- aggregate_event_study_by_neighborhood(results, neighborhoods,
                                                              value_col = "tau", se_col = "tau_se")
  agg_neigh_vif     <- aggregate_event_study_by_neighborhood(results, neighborhoods_vif)
  agg_neigh_vif_tau <- aggregate_event_study_by_neighborhood(results, neighborhoods_vif,
                                                              value_col = "tau", se_col = "tau_se")

  plot_neighborhood_event_study(
    agg_neigh, agg, label,
    file.path(fig_dir, paste0("didcf_neighborhood_event_study_", outcome_name, ".pdf"))
  )
  plot_neighborhood_event_study(
    agg_neigh_tau, agg_tau, label,
    file.path(fig_dir, paste0("didcf_neighborhood_event_study_tau_", outcome_name, ".pdf")),
    prefix = "tau", y_label = "Mean CACRT"
  )
  plot_neighborhood_event_study(
    agg_neigh_vif, agg, label,
    file.path(fig_dir, paste0("didcf_neighborhood_event_study_vifweighted_", outcome_name, ".pdf"))
  )
  plot_neighborhood_event_study(
    agg_neigh_vif_tau, agg_tau, label,
    file.path(fig_dir, paste0("didcf_neighborhood_event_study_vifweighted_tau_", outcome_name, ".pdf")),
    prefix = "tau", y_label = "Mean CACRT"
  )
  plot_neighborhood_sizes(
    neighborhoods, label,
    file.path(fig_dir, paste0("didcf_neighborhood_sizes_", outcome_name, ".pdf"))
  )
  plot_neighborhood_sizes(
    neighborhoods_vif, label,
    file.path(fig_dir, paste0("didcf_neighborhood_sizes_vifweighted_", outcome_name, ".pdf"))
  )
  save_neighborhood_profile_table(
    results, neighborhoods_vif, agg_neigh_vif, agg_neigh_vif_tau, MOD_VARS, outcome_name
  )

  # VIF heatmap
  plot_vif_heatmap(
    results, outcome_name,
    file.path(fig_dir, paste0("didcf_vif_", outcome_name, ".pdf"))
  )

  # state CATT map (4 decimals for AFGR)
  state_catt <- aggregate_state_catt(results, load_district_states(cfg))
  plot_state_catt_map(
    state_catt,
    file.path(fig_dir, paste0("didcf_state_catt_map_", outcome_name, ".pdf")),
    digits = if (outcome_name == "afgr") 4 else 3
  )

  # tables
  save_dim_table(dim_res, outcome_name, results)
  save_blp_table(list(`No dose control`   = blp_res,
                      `Linear dose`       = blp_res_dose$linear,
                      `Dose-quintile FE`  = blp_res_dose$bins),
                 outcome_name, results)
  save_dim_table_tau(dim_res_tau, outcome_name, results)
  save_blp_table_tau(blp_res_tau, outcome_name, results)

  # policy trees, depth 2 and 3 (cached; loaded when LOAD_MODELS)
  pt_inputs <- compute_policy_tree_inputs(results, MOD_VARS)
  if (!is.null(pt_inputs)) {
    pt_models_dir <- file.path(MODELS_DIR, outcome_name)
    dir.create(pt_models_dir, showWarnings = FALSE, recursive = TRUE)

    for (d in c(2L, 3L)) {
      pt_model_path <- file.path(pt_models_dir, sprintf("policytree_d%d.rds", d))

      if (LOAD_MODELS && file.exists(pt_model_path)) {
        pt <- readRDS(pt_model_path)
      } else {
        set.seed(42)
        pt <- policy_tree(pt_inputs$X, pt_inputs$Gamma, depth = d)
        saveRDS(pt, pt_model_path)
      }

      draw_policy_tree_pdf(
        pt, pt_inputs,
        label           = toupper(outcome_name),
        covariate_names = COVARIATE_NAMES,
        path            = file.path(fig_dir,
                            sprintf("didcf_policytree_%s_d%d.pdf", outcome_name, d))
      )
      message("  Policy tree depth-", d, " done: ", outcome_name)
    }
  } else {
    message("  Policy tree skipped (insufficient data): ", outcome_name)
  }

  # save results
  saveRDS(
    list(results = results, agg_catt = agg, agg_tau = agg_tau, dim = dim_res, blp = blp_res,
         blp_dose = blp_res_dose, state_catt = state_catt),
    file.path(results_dir, paste0("didcf_results_", outcome_name, ".rds"))
  )

  all_results[[outcome_name]] <- list(agg_catt = agg)
  message("  Done: ", outcome_name)
}

message("\nAll outcomes complete. Set RUN_ESTIMATION <- TRUE to run full estimation.")

# ==============================================================================
# SECTION 9: DISTRICT-SPECIFIC LINEAR TRENDS vs. DOSE (Appendix table)
# ==============================================================================
# Regresses each treated unit's pre-treatment outcome slope on its dose and the
# top-VIF moderators (district-clustered SEs). Reads saved results.

N_TREND_MODS <- 10    # moderators in X
MAX_TREND_H  <- 10    # last event time reported

#' Per-unit OLS slope of outcome on year over pre-treatment years (all years
#' for never-treated); needs at least 2 years.
estimate_unit_trends <- function(df) {
  df |>
    filter(is.infinite(cohort) | year < cohort, !is.na(outcome)) |>
    group_by(unit_id) |>
    filter(n_distinct(year) >= 2) |>
    summarise(gamma = coef(lm(outcome ~ year))[["year"]], .groups = "drop")
}

#' Cluster-robust (CR1) standard errors for an lm fit.
cluster_se <- function(fit, cluster) {
  X <- model.matrix(fit); u <- as.numeric(residuals(fit)); cl <- factor(cluster)
  k <- ncol(X); n <- nrow(X); G <- nlevels(cl)
  bread <- solve(crossprod(X))
  meat  <- Reduce(`+`, lapply(split(seq_len(n), cl), function(idx)
    tcrossprod(crossprod(X[idx, , drop = FALSE], u[idx]))))
  V <- bread %*% meat %*% bread * (G / (G - 1)) * ((n - 1) / (n - k))
  setNames(sqrt(pmax(diag(V), 0)), colnames(X))
}

#' Partial correlation of y and w given X.
partial_cor <- function(y, w, X) {
  qrX <- qr(cbind(1, X))
  cor(qr.resid(qrX, y), qr.resid(qrX, w))
}

#' Trend-on-dose regressions for one outcome: pooled and by event time 1..max_h.
compute_trend_dose_test <- function(cfg, outcome_name, results,
                                    n_mods = N_TREND_MODS, max_h = MAX_TREND_H) {
  df  <- assign_cohorts(load_outcome_data(cfg))
  top <- get_top_moderators(results, n = n_mods)
  message("  Trend/dose X (", outcome_name, "): ", paste(top, collapse = ", "))

  post <- results$catt |>
    filter(event_time > 0, !is.na(dose_t)) |>
    select(unit_id, district, event_time, dose_t, all_of(top)) |>
    inner_join(estimate_unit_trends(df), by = "unit_id")
  mi   <- add_missing_indicators(post, top)
  post <- mi$data
  fml  <- reformulate(c("dose_t", mi$vars), response = "gamma")

  fit_one <- function(d, label) {
    d <- d[complete.cases(d[, c("gamma", "dose_t", mi$vars)]), ]
    if (nrow(d) < length(mi$vars) + 10) return(NULL)
    f <- lm(fml, data = d)
    b <- coef(f)[["dose_t"]]; s <- cluster_se(f, d$district)[["dose_t"]]
    tibble(outcome = outcome_name, event_time = label, n = nrow(d),
           coef = b, se = s, t = b / s, p = 2 * pnorm(-abs(b / s)),
           partial_r = partial_cor(d$gamma, d$dose_t, as.matrix(d[, mi$vars])))
  }
  hs <- sort(unique(post$event_time[post$event_time <= max_h]))
  bind_rows(fit_one(post, "pooled"),
            map_dfr(hs, function(h) fit_one(post[post$event_time == h, ], as.character(h))))
}

#' Bare tabular for the trend/dose table. pooled_only: report only the pooled
#' regression per outcome (drops the Event time column); otherwise also each event time.
save_trend_dose_table <- function(trend_df, tbl_dir = tbl_dir_all, pooled_only = TREND_POOLED_ONLY) {
  if (pooled_only) trend_df <- trend_df |> filter(event_time == "pooled")
  out <- trend_df |>
    transmute(Outcome = outcome, `Event time` = event_time, N = n,
              `Dose coef.` = sprintf("%.4f", coef), `Cl. SE` = sprintf("%.4f", se),
              `$t$` = sprintf("%.2f", t), `$p$` = sprintf("%.3f", p),
              `Partial $r$` = sprintf("%.4f", partial_r))
  if (pooled_only) out <- select(out, -`Event time`)
  path <- file.path(tbl_dir, "trend_dose_regression.tex")
  colspec <- if (pooled_only) "lrrrrrr" else "llrrrrrr"
  writeLines(c(sprintf("\\begin{tabular}{%s}", colspec), "\\toprule",
               paste0(paste(names(out), collapse = " & "), " \\\\"), "\\midrule",
               apply(out, 1, function(r) paste0(paste(r, collapse = " & "), " \\\\")),
               "\\bottomrule", "\\end{tabular}"), path)
  message("  Saved trend/dose table: ", path)
}

# All outcomes with saved results; skipped when sourced for functions only.
trend_outcomes <- if (length(OUTCOMES_TO_RUN) == 0) character(0) else
  names(OUTCOMES)[file.exists(file.path(
    results_dir, paste0("didcf_results_", names(OUTCOMES), ".rds")))]
trend_df <- map_dfr(trend_outcomes, function(o) {
  res <- readRDS(file.path(results_dir, paste0("didcf_results_", o, ".rds")))$results
  compute_trend_dose_test(OUTCOMES[[o]], o, res)
})
if (nrow(trend_df) > 0) {
  save_trend_dose_table(trend_df)
  saveRDS(trend_df, file.path(results_dir, "didcf_trend_dose.rds"))
}

# ==============================================================================
# SECTION 10: MATH vs. ELA DISTRICT-LEVEL CATT SCATTER
# ==============================================================================
# Binscatter of district-level math vs. ELA CATTs. Reads saved results.

#' District mean of value_col over post-period event times (and grades).
district_post_means <- function(results, value_col = "catt") {
  results$catt |>
    filter(event_time > 0, !is.na(.data[[value_col]])) |>
    group_by(district) |>
    summarise(value = mean(.data[[value_col]]), .groups = "drop")
}

#' Math vs. ELA district estimates, trimmed to [trim, 1 - trim] in each subject.
#' Points are n_bins bin means (NULL = all districts); fit line and r use
#' district-level data.
plot_math_ela_scatter <- function(res_math, res_ela, path,
                                  value_col = "catt", label_prefix = "CATT",
                                  trim = 0.05, n_bins = 20) {
  d_all <- inner_join(district_post_means(res_math, value_col),
                      district_post_means(res_ela, value_col),
                      by = "district", suffix = c("_math", "_ela"))
  in_range <- function(x) x >= quantile(x, trim) & x <= quantile(x, 1 - trim)
  d <- d_all |> filter(in_range(value_math), in_range(value_ela))
  r <- cor(d$value_math, d$value_ela)

  pts <- if (is.null(n_bins)) d else
    d |>
      mutate(bin = ntile(value_math, n_bins)) |>
      group_by(bin) |>
      summarise(value_math = mean(value_math), value_ela = mean(value_ela), .groups = "drop")
  bin_txt <- if (is.null(n_bins)) "" else sprintf(", %d bins", n_bins)

  p <- ggplot(d, aes(x = value_math, y = value_ela)) +
    geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
    geom_vline(xintercept = 0, color = "grey60", linewidth = 0.4) +
    geom_smooth(method = "lm", formula = y ~ x, color = "firebrick",
                fill = "firebrick", alpha = 0.15, linewidth = 0.9) +
    { if (is.null(n_bins)) geom_point(data = pts, color = "#2171b5", alpha = 0.5, size = 1.8)
      else                 geom_point(data = pts, color = "#2171b5", size = 3) } +
    annotate("text", x = -Inf, y = Inf,
             label = sprintf("r = %.2f  (n = %d districts%s)", r, nrow(d), bin_txt),
             hjust = -0.05, vjust = 1.5, size = 5, family = "serif") +
    scale_x_continuous(expand = expansion(mult = 0.12)) +
    scale_y_continuous(expand = expansion(mult = 0.12)) +
    labs(x = paste("District", label_prefix, "— Math Test Scores"),
         y = paste("District", label_prefix, "— ELA Test Scores")) +
    theme_minimal(base_family = "serif", base_size = 15) +
    theme(
      panel.grid.minor = element_blank(),
      axis.title       = element_text(size = 16),
      axis.text        = element_text(size = 14)
    )

  ggsave(path, plot = p, width = 7, height = 6, device = "pdf")
  message(sprintf("  Saved math/ELA %s scatter (trimmed r = %.3f, n = %d; full r = %.3f, n = %d): %s",
                  label_prefix, r, nrow(d), cor(d_all$value_math, d_all$value_ela),
                  nrow(d_all), path))
  invisible(p)
}

# Runs once both SEDA outcomes have saved results; skipped when sourced for
# functions only.
seda_files <- file.path(results_dir, paste0("didcf_results_", c("math", "ela"), ".rds"))
if (length(OUTCOMES_TO_RUN) > 0 && all(file.exists(seda_files))) {
  res_math <- readRDS(seda_files[1])$results
  res_ela  <- readRDS(seda_files[2])$results
  plot_math_ela_scatter(res_math, res_ela,
                        file.path(fig_dir_all, "didcf_math_ela_catt_scatter.pdf"))
}
