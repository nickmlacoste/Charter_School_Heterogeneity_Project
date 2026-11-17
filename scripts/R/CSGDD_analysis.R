# ==============================================================================
# CSGDD_analysis.R
# Staggered, continuous-dose CGS-DiD: the linear parametric counterpart of the
# DiDCF (paper Section 2.2, eq. DiD_spec).
#
# Per cohort g x calendar year t cell (never pooled across cohorts):
#   Z = Y_t - Y_{g-1}
#   Stage 1 (never-treated, WLS): Z = gamma_0 + gamma_1'X + u -> m_hat(x)
#   Stage 2 (treated, WLS):       Z - m_hat(X) = a_0 + a_1'X + t_0 W + t_1'(W X) + e
#   CATT_i = alpha(X_i) + tau(X_i) W_i, CACRT_i = tau(X_i)
# Pre-period cells drop the dose terms (alpha(x) = placebo CATT).
#
# Inference: exact influence functions (including the stage-1 term), clustered
# by district; event studies use a Rademacher multiplier bootstrap.
#
# Specs (OUTCOMES[[o]]$specs):
#   no_moderators  : X empty
#   moderators     : full X, cell-specific coefficients (math, ELA)
#   top_moderators : full X in stage 1, TOP_MODS in stage 2 (AFGR)
#   stacked        : one stage-2 regression over all cells, common moderator slopes
#
# Outputs: paper/{figures,tables}/CSGDD_analysis/,
#          quality_reports/results/csgdd_results_<outcome>.rds
# ==============================================================================

library(here)
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(Matrix)

set.seed(42)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

if (!exists("OUTCOMES_TO_RUN")) OUTCOMES_TO_RUN <- c("math", "ela", "afgr")
if (!exists("BOOT_REPS"))       BOOT_REPS       <- 1000

# Heterogeneity tables: also report the CACRT (W x X) rows and a Component column.
if (!exists("HET_SHOW_CACRT"))  HET_SHOW_CACRT  <- FALSE

# Baseline-performance moderator = Y_{g - BASELINE_LAG}.
if (!exists("BASELINE_LAG"))    BASELINE_LAG    <- 2

# Cohorts to drop by hand (cohorts without Y_{g - BASELINE_LAG} drop automatically).
if (!exists("DROP_COHORTS"))    DROP_COHORTS    <- numeric(0)

# Moderators: DiDCF set minus rural (reference category), zcoolcitypop, and
# zcoolcityuniv. baseline_y is added per cell.
MOD_VARS <- c(
  "baseline_y",
  "perblk", "perhsp", "perfrl", "perwht", "perspeced",
  "logenroll", "urban", "suburb", "town",
  "num_magnet",
  "p_rev", "p_exp", "str", "tea_salary",
  "w_pp_exp_percdiff", "charter_exp",
  "napcs14_nocaps", "napcs14_transpar", "napcs14_perf",
  "napcs14_non_renew", "napcs14_exem", "napcs14_eq_funding",
  "share_author", "share_cmo", "share_emo"
)

# Stage-2 moderators of the "top_moderators" spec: ELA's top-10 DiDCF VIF at e = 0.
TOP_MODS <- c("baseline_y", "perfrl", "str", "logenroll", "perblk",
              "perhsp", "perwht", "p_exp", "tea_salary", "perspeced")

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

# Dollar moderators rescaled to $1,000s before estimation.
DOLLAR_VARS <- c("tea_salary", "p_rev", "p_exp", "charter_exp")

OUTCOMES <- list(
  math = list(
    data_file  = "charter_seda.dta",
    outcome    = "st_math",
    treatment  = "lag_grade",
    share_var  = "cha_share2",
    unit_vars  = c("district", "grade"),
    time_var   = "year",
    weight_var = "eweight",
    label      = "Math Test Scores",
    specs      = c("no_moderators", "moderators", "stacked")
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
    specs      = c("no_moderators", "moderators", "stacked")
  ),
  afgr = list(
    data_file  = "charter_afgr2.dta",
    outcome    = "afgr",
    treatment  = "lag_share",
    share_var  = "cha_share2",
    unit_vars  = "district",
    time_var   = "year",
    weight_var = "eweight",
    label      = "Graduation Rate (AFGR)",
    specs      = c("no_moderators", "top_moderators", "stacked")
  )
)

SPECS <- c("no_moderators", "moderators", "top_moderators", "stacked")
SPEC_LABELS <- c(no_moderators = "None", moderators = "Cell-specific",
                 top_moderators = "Top 10", stacked = "Common")
SPEC_SUFFIX <- c(no_moderators = "_nomod", moderators = "", top_moderators = "_top10",
                 stacked = "_stacked")
OUTCOME_HEADERS <- c(math = "Math", ela = "ELA", afgr = "Graduation")

# ==============================================================================
# SECTION 1: DATA PREPARATION (as in DiDCF_analysis.R)
# ==============================================================================

#' Load an outcome panel, merge policy and finance moderators, and rename columns.
load_outcome_data <- function(cfg) {
  df <- read_dta(here("data", "cleaned", cfg$data_file))
  df <- haven::zap_labels(df)

  policy <- haven::zap_labels(read_dta(here("data", "raw", "policy3.dta")))
  df <- left_join(df, select(policy, -any_of(c("state", "postal"))),
                  by = "statename")

  finance <- haven::zap_labels(read_dta(here("data", "raw", "finance.dta")))
  df <- left_join(df, finance, by = "district")

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
    mutate(ever_treated = !is.na(maxshare2) & maxshare2 > 0)

  recode_finance_missing(df)
}

#' Recode finance.dta zeros that denote missing values to NA.
recode_finance_missing <- function(df) {
  df <- df |>
    group_by(district) |>
    mutate(has_charter = any(share > 0, na.rm = TRUE) | any(maxshare2 > 0, na.rm = TRUE)) |>
    ungroup()

  tps_imputed <- is.na(df$tps_exp) | df$tps_exp == 0

  df |>
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

#' NA -> 0 plus one `<var>_miss` dummy per distinct missingness pattern.
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

#' CGS-DiD moderator matrix: dollars in $1,000s, centered at enrollment-weighted
#' means, then missing indicators. top_vars get their own missing dummies,
#' returned as `top_vars` for the "top_moderators" stage 2.
build_csgdd_moderators <- function(df, mod_vars, top_vars = character(0)) {
  mm <- build_moderator_matrix(df, mod_vars)
  w1 <- df |> arrange(unit_id, year) |> group_by(unit_id) |> slice(1) |> ungroup() |>
    select(unit_id, w1 = weight)
  mm <- left_join(mm, w1, by = "unit_id")

  # rescale dollars and center
  vars <- setdiff(names(mm), c("unit_id", "w1"))
  mm <- mm |>
    mutate(across(any_of(DOLLAR_VARS), ~ .x / 1000)) |>
    mutate(across(all_of(vars), ~ {
      ok <- !is.na(.x) & !is.na(w1)
      .x - weighted.mean(.x[ok], w1[ok])
    }))

  top     <- intersect(top_vars, vars)
  top_pat <- lapply(setNames(top, top), function(v) as.numeric(is.na(mm[[v]])))

  mi   <- add_missing_indicators(mm, vars)
  data <- mi$data

  # missing dummies named after the top vars
  top_miss <- character(0)
  for (v in intersect(top, mi$vars)) {
    p <- top_pat[[v]]
    if (!any(p == 1)) next
    if (any(vapply(top_miss, function(d) identical(data[[d]], p), logical(1)))) next
    d <- paste0(v, "_miss")
    data[[d]] <- p
    top_miss <- c(top_miss, d)
  }

  list(mod_matrix = select(data, -w1), x_vars = mi$vars,
       top_vars = c(intersect(top, mi$vars), top_miss))
}

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
    filter(!is.na(Z), !is.na(dose_t), !is.na(weight))

  n_treated <- sum(panel$treated)
  n_control <- sum(!panel$treated)
  if (n_treated < min_obs || n_control < min_obs) return(NULL)

  panel
}

# ==============================================================================
# SECTION 2: CELL-LEVEL CGS-DiD ESTIMATION
# ==============================================================================

#' WLS dropping aliased columns. Returns b on kept columns k, H = X_k'WX_k, residuals.
wls <- function(X, y, w) {
  q <- qr(X * sqrt(w), tol = 1e-7)
  k <- sort(q$pivot[seq_len(q$rank)])
  Xk <- X[, k, drop = FALSE]
  H  <- crossprod(Xk * w, Xk)
  b  <- drop(solve(H, crossprod(Xk * w, y)))
  list(b = b, k = k, H = H, resid = drop(y - Xk %*% b))
}

#' Names of the W x X coefficients ("W:<var>").
w_names <- function(x_vars) if (length(x_vars)) paste0("W:", x_vars) else character(0)

#' Stage 1 for one cell: WLS of Z on (1, X) among never-treated units. Returns
#' the treated rows' Ztilde, stage-2 design (1, X2), and the pieces needed for
#' the stage-1 influence-function term. x2_vars defaults to x_vars.
stage1_cell <- function(cell_df, x_vars, x2_vars = x_vars) {
  tr <- cell_df$treated
  w  <- cell_df$weight

  # center baseline_y within the cell
  all_vars <- union(x_vars, x2_vars)
  Xc <- as.matrix(cell_df[, all_vars, drop = FALSE])
  if ("baseline_y" %in% all_vars)
    Xc[, "baseline_y"] <- Xc[, "baseline_y"] - weighted.mean(Xc[, "baseline_y"], w)
  x  <- cbind(`(Intercept)` = 1, Xc[, x_vars,  drop = FALSE])
  x2 <- cbind(`(Intercept)` = 1, Xc[, x2_vars, drop = FALSE])

  s1 <- wls(x[!tr, , drop = FALSE], cell_df$Z[!tr], w[!tr])
  xT <- x2[tr, , drop = FALSE]
  xT1 <- x[tr, s1$k, drop = FALSE]
  list(Ztilde     = cell_df$Z[tr] - drop(xT1 %*% s1$b),
       xT         = xT,
       xT1        = xT1,
       xC1        = x[!tr, s1$k, drop = FALSE],
       uC         = w[!tr] * s1$resid,
       H1inv      = solve(s1$H),
       district_C = cell_df$district[!tr])
}

#' Two-stage CGS-DiD for one cell (post = FALSE: pre-period placebo). Returns
#' per-unit catt/tau and influence functions, stage-2 coefficients, and their
#' district-level influence functions.
fit_csgdd_cell <- function(cell_df, x_vars, post, x2_vars = x_vars) {
  tr <- cell_df$treated
  w  <- cell_df$weight
  W  <- cell_df$dose_t

  # stage 1
  s1  <- stage1_cell(cell_df, x_vars, x2_vars)
  xC1 <- s1$xC1

  # stage 2
  xT  <- s1$xT
  xT1 <- s1$xT1
  WT  <- W[tr]; wT <- w[tr]; nT <- sum(tr)
  Zt  <- s1$Ztilde

  if (post) {
    Wx <- WT * xT
    colnames(Wx) <- c("W", w_names(x2_vars))
    d    <- cbind(xT, Wx)
    dW   <- cbind(matrix(0, nT, ncol(xT)), xT)   # d(design)/dW, for tau
  } else {
    d  <- xT
    dW <- NULL
  }
  s2 <- wls(d, Zt, wT)
  dk <- d[, s2$k, drop = FALSE]

  catt <- drop(dk %*% s2$b)
  if (post) {
    dWk <- dW[, s2$k, drop = FALSE]
    tau <- drop(dWk %*% s2$b)
  } else {
    dWk <- dk                                     # pre-period: tau = placebo CATT
    tau <- catt
  }

  # influence functions
  H2inv <- solve(s2$H)
  H1inv <- s1$H1inv
  G     <- crossprod(dk * wT, xT1)
  eT    <- wT * s2$resid
  uC    <- s1$uC

  # IF of theta = mean_T a_i'b (treated and control parts)
  if_functional <- function(A) {
    abar  <- colMeans(A)
    theta <- sum(abar * s2$b)
    v     <- H2inv %*% abar
    s     <- H1inv %*% crossprod(G, v)
    list(theta = theta,
         phiT  = (drop(A %*% s2$b) - theta) / nT + eT * drop(dk %*% v),
         phiC  = -uC * drop(xC1 %*% s))
  }
  f_catt <- if_functional(dk)
  f_tau  <- if_functional(dWk)

  # IF of the stage-2 coefficients, summed by district
  S       <- H1inv %*% t(G) %*% H2inv
  coefIF  <- rbind(eT * (dk %*% H2inv), -uC * (xC1 %*% S))
  coefIF  <- rowsum(coefIF, c(cell_df$district[tr], cell_df$district[!tr]))

  coef <- setNames(rep(NA_real_, ncol(d)), colnames(d))
  coef[s2$k] <- s2$b
  coef_if <- matrix(0, nrow(coefIF), ncol(d), dimnames = list(rownames(coefIF), colnames(d)))
  coef_if[, s2$k] <- coefIF

  list(
    treated  = tibble(unit_id = cell_df$unit_id[tr], district = cell_df$district[tr],
                      catt = catt, tau = tau,
                      phi_catt = f_catt$phiT, phi_tau = f_tau$phiT),
    controls = tibble(district = cell_df$district[!tr],
                      phi_catt = f_catt$phiC, phi_tau = f_tau$phiC),
    coef     = coef,
    coef_if  = coef_if,
    n_treated = nT,
    n_control = sum(!tr)
  )
}

# ==============================================================================
# SECTION 2B: STACKED CGS-DiD (spec "stacked")
# ==============================================================================
# Stage 1 per cell; stage 2 is one WLS over the treated rows of all post-period
# cells with cell-specific alpha_0, tau_0 and common moderator coefficients.
# Pre-period cells get their own stacked regression. common = FALSE interacts X
# with the cells and reproduces the cell-by-cell spec.

#' Cell key "c<g>_h<h>".
cell_key <- function(g, h) sprintf("c%d_h%+d", g, h)

#' Stacked stage-2 design: cell intercepts, cell dose slopes, X, and W X
#' (cell-interacted if !common). dW = d(design)/dW, NULL for pre-period cells.
build_stacked_design <- function(cid, keys, XT, W, post, common) {
  n <- length(cid); C <- length(keys)
  E <- matrix(0, n, C, dimnames = list(NULL, keys))
  E[cbind(seq_len(n), cid)] <- 1
  Xb <- XT
  if (!common && ncol(XT) > 0) {
    Xb <- do.call(cbind, lapply(seq_len(C), function(c) XT * E[, c]))
    colnames(Xb) <- paste0(rep(colnames(XT), C), ":", rep(keys, each = ncol(XT)))
  }
  a0 <- E; colnames(a0) <- paste0("a0:", keys)
  if (!post) return(list(d = cbind(a0, Xb), dW = NULL))
  t0 <- E * W;  colnames(t0) <- paste0("t0:", keys)
  WX <- W * Xb; colnames(WX) <- w_names(colnames(Xb))
  list(d  = cbind(a0, t0, Xb, WX),
       dW = cbind(a0 * 0, E, Xb * 0, Xb))
}

#' One stacked regression over the candidate cells `cand` (all post or all pre).
#' Returns per-unit rows, per-cell influence functions, coefficients b and their
#' district influence functions Psi, and cell treated shares.
fit_stacked_regime <- function(df, mm, x_vars, cand, post, common, districts) {
  # pass 1: stage 1 per cell, collect treated rows
  cells <- list()
  for (r in seq_len(nrow(cand))) {
    cell_df <- build_cell_data(df, mm, cand$cohort[r], cand$cal_t[r])
    if (is.null(cell_df)) next
    s1 <- stage1_cell(cell_df, x_vars)
    tr <- cell_df$treated
    h  <- cand$cal_t[r] - (cand$cohort[r] - 1)
    out_rows <- cell_df[tr, c("unit_id", intersect(c(x_vars, MOD_VARS), names(cell_df)))]
    cells[[length(cells) + 1]] <- list(
      cohort = cand$cohort[r], cal_t = cand$cal_t[r], event_time = h,
      Zt = s1$Ztilde, W = cell_df$dose_t[tr], w = cell_df$weight[tr],
      XT = s1$xT[, -1, drop = FALSE], district = cell_df$district[tr],
      out_rows = out_rows[, !duplicated(names(out_rows))])
  }
  if (length(cells) == 0) return(NULL)
  keys <- vapply(cells, function(z) cell_key(z$cohort, z$event_time), character(1))
  nc   <- vapply(cells, function(z) length(z$Zt), integer(1))
  cid  <- rep(seq_along(cells), nc)
  pull_all <- function(f) unlist(lapply(cells, `[[`, f), use.names = FALSE)
  Zt <- pull_all("Zt"); W <- pull_all("W"); w <- pull_all("w"); dist <- pull_all("district")
  XT <- do.call(rbind, lapply(cells, `[[`, "XT"))

  # stage 2
  des <- build_stacked_design(cid, keys, XT, W, post, common)
  s2  <- wls(des$d, Zt, w)
  dk  <- des$d[, s2$k, drop = FALSE]
  dWk <- if (post) des$dW[, s2$k, drop = FALSE] else dk
  H2inv <- solve(s2$H)
  catt  <- drop(dk %*% s2$b)
  tau   <- drop(dWk %*% s2$b)

  # district IF of b: treated part
  Psi <- matrix(0, length(districts), length(s2$k))
  add_rows <- function(Psi, M, d) {
    rs <- rowsum(M, match(d, districts))
    ix <- as.integer(rownames(rs))
    Psi[ix, ] <- Psi[ix, ] + rs
    Psi
  }
  Psi <- add_rows(Psi, (w * s2$resid) * (dk %*% H2inv), dist)

  # pass 2: stage-1 part, one cell at a time
  for (r in seq_along(cells)) {
    cell_df <- build_cell_data(df, mm, cells[[r]]$cohort, cells[[r]]$cal_t)
    s1  <- stage1_cell(cell_df, x_vars)
    ix  <- which(cid == r)
    G   <- crossprod(dk[ix, , drop = FALSE] * w[ix], s1$xT1)
    S   <- s1$H1inv %*% t(G) %*% H2inv
    Psi <- add_rows(Psi, -(s1$uC * s1$xC1) %*% S, s1$district_C)
  }

  # per-cell mean catt / tau and their IFs
  cell_if <- function(A, vals) {
    Abar  <- t(rowsum(A, cid)) / rep(nc, each = ncol(A))
    theta <- drop(crossprod(Abar, s2$b))
    M <- matrix(0, length(cid), length(cells))
    M[cbind(seq_along(cid), cid)] <- (vals - theta[cid]) / nc[cid]
    own <- matrix(0, length(districts), length(cells))
    rs  <- rowsum(M, match(dist, districts))
    own[as.integer(rownames(rs)), ] <- rs
    list(theta = theta, IF = Psi %*% Abar + own)
  }
  f_catt <- cell_if(dk, catt)
  f_tau  <- cell_if(dWk, tau)

  rows <- bind_rows(lapply(seq_along(cells), function(r) {
    ix <- which(cid == r)
    bind_cols(tibble(district = dist[ix], catt = catt[ix], tau = tau[ix]),
              cells[[r]]$out_rows) |>
      mutate(cohort = cells[[r]]$cohort, event_time = cells[[r]]$event_time,
             cal_time = cells[[r]]$cal_t, catt_se = NA_real_, tau_se = NA_real_)
  }))
  if_cells <- bind_rows(lapply(seq_along(cells), function(r) {
    nz <- which(f_catt$IF[, r] != 0 | f_tau$IF[, r] != 0)
    tibble(district = districts[nz], phi_catt = f_catt$IF[nz, r], phi_tau = f_tau$IF[nz, r],
           cohort = cells[[r]]$cohort, event_time = cells[[r]]$event_time)
  }))

  list(rows = rows, if_cells = if_cells, b = setNames(s2$b, colnames(dk)), Psi = Psi,
       f_catt = f_catt, f_tau = f_tau, keys = keys, wcell = nc / sum(nc),
       n_total = sum(nc))
}

#' Stacked CGS-DiD for one outcome. Coefficients: treated-share-weighted means of
#' the cell alpha_0 / tau_0, the common moderator slopes, and overall ATT / ACRT.
run_csgdd_stacked <- function(df, mm, x_vars, common = TRUE) {
  cohorts   <- analysis_cohorts(df)
  years     <- sort(unique(df$year))
  districts <- sort(unique(df$district))
  cand <- bind_rows(lapply(cohorts, function(g) tibble(cohort = g, cal_t = years[years != g - 1]))) |>
    mutate(h = cal_t - (cohort - 1))

  post <- fit_stacked_regime(df, mm, x_vars, filter(cand, h > 0), TRUE,  common, districts)
  message("    [stacked] post-period regression done")
  pre  <- fit_stacked_regime(df, mm, x_vars, filter(cand, h < 0), FALSE, common, districts)
  message("    [stacked] pre-period regression done")

  # linear functionals a'b with IF-based SEs
  kept  <- names(post$b)
  blank <- setNames(numeric(length(kept)), kept)
  lin <- function(term, a)
    tibble(term = term, estimate = sum(a * post$b), se = sqrt(sum((post$Psi %*% a)^2)))
  share_over <- function(prefix) {
    cols <- paste0(prefix, post$keys)
    ok   <- cols %in% kept
    a    <- blank
    a[cols[ok]] <- post$wcell[ok] / sum(post$wcell[ok])
    a
  }
  coefs <- bind_rows(lin("(Intercept)", share_over("a0:")), lin("W", share_over("t0:")))
  if (common) {
    for (v in c(x_vars, w_names(x_vars))) {
      if (v %in% kept) {
        a <- blank; a[v] <- 1
        coefs <- bind_rows(coefs, lin(v, a))
      } else {
        coefs <- bind_rows(coefs, tibble(term = v, estimate = NA_real_, se = NA_real_))
      }
    }
  }
  agg <- function(f) c(est = sum(post$wcell * f$theta), se = sqrt(sum((f$IF %*% post$wcell)^2)))
  a1 <- agg(post$f_catt); a2 <- agg(post$f_tau)
  coefs <- bind_rows(coefs,
                     tibble(term = c("ATT", "ACRT"), estimate = c(a1[["est"]], a2[["est"]]),
                            se = c(a1[["se"]], a2[["se"]]))) |>
    mutate(n_treated_obs = post$n_total)

  list(catt    = bind_rows(post$rows, pre$rows),
       if_rows = bind_rows(post$if_cells, pre$if_cells),
       coefs   = coefs)
}

# ==============================================================================
# SECTION 3: MAIN LOOP (one outcome x spec)
# ==============================================================================

#' Treated cohorts with an observed Y_{g - BASELINE_LAG}, minus DROP_COHORTS.
analysis_cohorts <- function(df) {
  cohorts <- sort(unique(df$cohort[is.finite(df$cohort)]))
  setdiff(cohorts[cohorts - BASELINE_LAG >= min(df$year)], DROP_COHORTS)
}

#' Cell-by-cell CGS-DiD for one outcome and spec. Returns catt (unit x cell rows),
#' if_rows (district influence functions per cell), and coefs (treated-share-
#' weighted coefficients and overall ATT / ACRT with SEs).
run_csgdd <- function(df, mm, x_vars, spec, x2_vars = NULL) {
  if (spec == "no_moderators") x_vars <- character(0)
  if (spec != "top_moderators") x2_vars <- x_vars
  stopifnot(all(x2_vars %in% c(names(mm), "baseline_y")))

  cohorts <- analysis_cohorts(df)
  years   <- sort(unique(df$year))
  districts <- sort(unique(df$district))

  coef_names <- c("(Intercept)", x2_vars, "W", w_names(x2_vars), "ATT", "ACRT")
  num <- setNames(numeric(length(coef_names)), coef_names)
  den <- num
  Psi <- matrix(0, length(districts), length(coef_names),
                dimnames = list(districts, coef_names))

  cells <- list(); idx <- 0L
  for (cohort_c in cohorts) {
    base_yr <- cohort_c - 1
    for (cal_t in years[years != base_yr]) {
      cell_df <- build_cell_data(df, mm, cohort_c, cal_t)
      if (is.null(cell_df)) next
      h    <- cal_t - base_yr
      post <- h > 0
      fit  <- fit_csgdd_cell(cell_df, x_vars, post, x2_vars)

      tr_rows <- cell_df[cell_df$treated, c("unit_id", intersect(c(x_vars, MOD_VARS), names(cell_df)))]
      tr_rows <- tr_rows[, !duplicated(names(tr_rows))]
      rows <- bind_cols(select(fit$treated, -unit_id), tr_rows) |>
        mutate(cohort = cohort_c, event_time = h, cal_time = cal_t, catt_se = NA_real_, tau_se = NA_real_)

      if_cell <- bind_rows(fit$treated |> select(district, phi_catt, phi_tau),
                           fit$controls) |>
        group_by(district) |>
        summarise(phi_catt = sum(phi_catt), phi_tau = sum(phi_tau), .groups = "drop") |>
        mutate(cohort = cohort_c, event_time = h)

      # accumulate treated-share-weighted coefficients and IFs over post cells
      if (post) {
        nT <- fit$n_treated
        b  <- fit$coef
        ok <- names(b)[!is.na(b)]
        num[ok] <- num[ok] + nT * b[ok]
        den[ok] <- den[ok] + nT
        ri <- match(rownames(fit$coef_if), districts)
        Psi[ri, ok] <- Psi[ri, ok] + nT * fit$coef_if[, ok, drop = FALSE]

        num[c("ATT", "ACRT")] <- num[c("ATT", "ACRT")] + nT * c(mean(fit$treated$catt), mean(fit$treated$tau))
        den[c("ATT", "ACRT")] <- den[c("ATT", "ACRT")] + nT
        ri <- match(if_cell$district, districts)
        Psi[ri, "ATT"]  <- Psi[ri, "ATT"]  + nT * if_cell$phi_catt
        Psi[ri, "ACRT"] <- Psi[ri, "ACRT"] + nT * if_cell$phi_tau
      }

      idx <- idx + 1L
      cells[[idx]] <- list(rows = rows, if_cell = if_cell)
    }
    message(sprintf("    [%s] cohort %d done", spec, cohort_c))
  }

  est <- num / den
  se  <- sqrt(colSums(sweep(Psi, 2, den, "/")^2))
  coefs <- tibble(term = coef_names, estimate = as.numeric(est), se = as.numeric(se),
                  n_treated_obs = as.numeric(den)) |>
    filter(n_treated_obs > 0)

  list(
    catt    = bind_rows(lapply(cells, `[[`, "rows")),
    if_rows = bind_rows(lapply(cells, `[[`, "if_cell")),
    coefs   = coefs
  )
}

# ==============================================================================
# SECTION 4: AGGREGATION AND EVENT-STUDY INFERENCE
# ==============================================================================

#' Treated units per cohort.
cohort_sizes <- function(results) {
  results$catt |> distinct(unit_id, cohort) |> count(cohort, name = "n_cohort")
}

#' Cohort-weighted (phi_g) event-time means of value_col.
aggregate_event_study_mean <- function(results, value_col) {
  n_g <- cohort_sizes(results)
  results$catt |>
    group_by(cohort, event_time) |>
    summarise(mean_g = mean(.data[[value_col]], na.rm = TRUE), n_g = n(), .groups = "drop") |>
    left_join(n_g, by = "cohort") |>
    group_by(event_time) |>
    summarise(mean = sum(n_cohort / sum(n_cohort) * mean_g), n_units = sum(n_g), .groups = "drop")
}

#' Multiplier-bootstrap SEs and uniform critical value from the cell influence
#' functions (same Rademacher draw for a district in every cell).
if_event_study_se <- function(if_rows, n_g, phi_col, B = BOOT_REPS, seed = 42) {
  info <- if_rows |> distinct(cohort, event_time) |> arrange(event_time, cohort) |>
    mutate(cell = row_number()) |> left_join(n_g, by = "cohort")
  d   <- if_rows |> left_join(select(info, cohort, event_time, cell), by = c("cohort", "event_time"))
  ids <- sort(unique(d$district))
  Phi <- Matrix::sparseMatrix(i = match(d$district, ids), j = d$cell, x = d[[phi_col]],
                              dims = c(length(ids), nrow(info)))
  es <- sort(unique(info$event_time))
  A  <- matrix(0, nrow(info), length(es))
  for (j in seq_along(es)) {
    k <- which(info$event_time == es[j])
    A[k, j] <- info$n_cohort[k] / sum(info$n_cohort[k])
  }
  set.seed(seed)
  xi    <- matrix(sample(c(-1, 1), length(ids) * B, replace = TRUE), length(ids), B)
  draws <- crossprod(A, as.matrix(Matrix::crossprod(Phi, xi)))
  se    <- apply(draws, 1, function(v) diff(quantile(v, c(.25, .75))) / (qnorm(.75) - qnorm(.25)))
  se[!is.finite(se) | se <= 0] <- NA_real_
  ok    <- !is.na(se)
  crit  <- if (any(ok)) as.numeric(quantile(apply(abs(draws[ok, , drop = FALSE]) / se[ok], 2, max), 0.95)) else NA_real_
  tibble(event_time = es, se = se, crit_uniform = ifelse(ok, crit, NA_real_))
}

#' Event study of value_col: means, bootstrap SEs, uniform critical value.
aggregate_event_study <- function(results, value_col = "catt", prefix = "catt") {
  m <- aggregate_event_study_mean(results, value_col)
  b <- if_event_study_se(results$if_rows, cohort_sizes(results), paste0("phi_", value_col))
  left_join(m, b, by = "event_time") |>
    rename(!!paste0(prefix, "_mean") := mean,
           !!paste0(prefix, "_se")   := se,
           !!paste0(prefix, "_crit") := crit_uniform)
}

# ==============================================================================
# SECTION 5: FIGURES
# ==============================================================================

#' Histogram of post-period unit x event-time estimates (5% tails trimmed for display).
plot_catt_hist <- function(results, label, path, value_col = "catt", label_prefix = "CATT") {
  post_data <- results$catt |> filter(event_time > 0, !is.na(.data[[value_col]]))
  att <- mean(post_data[[value_col]], na.rm = TRUE)

  trim_lo <- quantile(post_data[[value_col]], 0.05, na.rm = TRUE)
  trim_hi <- quantile(post_data[[value_col]], 0.95, na.rm = TRUE)
  plot_data <- post_data |> filter(.data[[value_col]] >= trim_lo, .data[[value_col]] <= trim_hi)

  p <- ggplot(plot_data, aes(x = .data[[value_col]])) +
    geom_histogram(bins = 50, fill = "#2171b5", color = "white", linewidth = 0.15) +
    geom_vline(xintercept = att, linetype = "dashed", linewidth = 0.5) +
    annotate("text", x = att, y = Inf, label = paste0("Mean = ", round(att, 2)),
             hjust = -0.1, vjust = 1.5, size = 5, family = "serif") +
    labs(x = paste0("District × Event-Time ", label_prefix, " — ", label),
         y = "District × Event-Times") +
    theme_minimal(base_family = "serif", base_size = 15) +
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size = 16), axis.text = element_text(size = 14))

  ggsave(path, plot = p, width = 6.5, height = 4.5, device = "pdf")
  invisible(p)
}

# Plot axis uses e = t - g (base year at e = -1); internal event_time = e + 1.
to_plot_event_time <- function(event_time) event_time - 1
PLOT_EVENT_BREAKS   <- -4:6
PLOT_TREATMENT_LINE <- -0.5
PLOT_EVENT_XLAB     <- "Event Time"

#' Event-study plot: ribbon = uniform 95% band, error bars = pointwise 95%.
plot_event_study <- function(agg, label, path, prefix = "catt", y_label = "Mean CATT") {
  mean_col <- paste0(prefix, "_mean"); se_col <- paste0(prefix, "_se")
  crit_col <- paste0(prefix, "_crit")

  base_row <- tibble(event_time = 0, n_units = NA_integer_) |>
    mutate(!!mean_col := 0, !!se_col := 0, !!crit_col := 0)
  plot_data <- bind_rows(agg, base_row) |>
    filter(event_time %in% -3:7) |>
    arrange(event_time) |>
    rename(.mean = all_of(mean_col), .se = all_of(se_col), .crit = all_of(crit_col)) |>
    mutate(event_time = to_plot_event_time(event_time))
  band_data <- filter(plot_data, !is.na(.crit))

  p <- ggplot(plot_data, aes(x = event_time, y = .mean)) +
    geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
    geom_vline(xintercept = PLOT_TREATMENT_LINE, linetype = "dashed", color = "grey60", linewidth = 0.4) +
    geom_ribbon(data = band_data, aes(ymin = .mean - .crit * .se, ymax = .mean + .crit * .se),
                alpha = 0.15, fill = "steelblue") +
    geom_errorbar(aes(ymin = .mean - 1.96 * .se, ymax = .mean + 1.96 * .se),
                  color = "steelblue", width = 0.15, linewidth = 0.5, na.rm = TRUE) +
    geom_line(color = "steelblue", linewidth = 0.9) +
    geom_point(color = "steelblue", size = 2.2) +
    scale_x_continuous(breaks = PLOT_EVENT_BREAKS) +
    labs(x = PLOT_EVENT_XLAB, y = paste(y_label, "—", label)) +
    theme_minimal(base_family = "serif", base_size = 15) +
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size = 16), axis.text = element_text(size = 14))

  ggsave(path, plot = p, width = 7.5, height = 5, device = "pdf")
  invisible(p)
}

# ==============================================================================
# SECTION 6: TABLES (bare tabular -- INV-13)
# ==============================================================================

#' Escape LaTeX special characters.
escape_latex <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  gsub("([%$#_{}&])", "\\\\\\1", x)
}

#' Significance stars from a normal test.
stars <- function(est, se) {
  p <- 2 * pnorm(-abs(est / se))
  ifelse(is.na(p), "", ifelse(p < 0.01, "^{***}", ifelse(p < 0.05, "^{**}", ifelse(p < 0.10, "^{*}", ""))))
}

#' Estimate with stars over its SE, in one cell.
fmt_cell <- function(est, se) {
  if (is.na(est)) return("")
  sprintf("\\shortstack{$%.3f%s$\\\\{\\scriptsize (%.3f)}}", est, stars(est, se), se)
}

#' Display label for a moderator or its missing dummy.
moderator_label <- function(v) {
  base <- sub("_miss$", "", v)
  lbl  <- if (base %in% names(COVARIATE_NAMES)) COVARIATE_NAMES[[base]] else base
  if (base %in% DOLLAR_VARS) lbl <- paste0(lbl, " ($1,000s)")
  if (grepl("_miss$", v)) lbl <- paste0(lbl, " (missing)")
  escape_latex(lbl)
}

#' Overall ATT / ACRT table: columns = outcome x spec ("--" if not run).
save_att_acrt_table <- function(all_coefs, path) {
  outs <- intersect(OUTCOMES_TO_RUN, unique(all_coefs$outcome))
  cols <- expand.grid(spec = SPECS, outcome = outs, stringsAsFactors = FALSE)
  get  <- function(o, s, term) all_coefs |> filter(outcome == o, spec == s, term == !!term)
  cell <- function(o, s, term) { r <- get(o, s, term); if (nrow(r)) fmt_cell(r$estimate, r$se) else "--" }
  row  <- function(term) paste0(term, " & ", paste(mapply(cell, cols$outcome, cols$spec, term),
                                                   collapse = " & "), " \\\\")
  nobs <- paste0("Treated observations & ", paste(mapply(function(o, s) {
    r <- get(o, s, "ATT"); if (nrow(r)) format(r$n_treated_obs, big.mark = ",") else "--"
  }, cols$outcome, cols$spec), collapse = " & "), " \\\\")

  k       <- length(SPECS)
  first   <- 2 + k * (seq_along(outs) - 1)
  hdr_out <- paste(sprintf("\\multicolumn{%d}{c}{%s}", k, OUTCOME_HEADERS[outs]), collapse = " & ")
  cmid    <- paste(sprintf("\\cmidrule(lr){%d-%d}", first, first + k - 1), collapse = "")
  modrow  <- paste0("Moderator coefficients & ", paste(SPEC_LABELS[cols$spec], collapse = " & "), " \\\\")

  tex <- paste0(
    "\\begin{tabular}{l", strrep("c", nrow(cols)), "}\n\\toprule\n",
    " & ", hdr_out, " \\\\\n", cmid, "\n",
    " & ", paste0("(", seq_len(nrow(cols)), ")", collapse = " & "), " \\\\\n\\midrule\n",
    row("ATT"), "\n", row("ACRT"), "\n\\midrule\n", nobs, "\n", modrow, "\n\\bottomrule\n\\end{tabular}\n")
  writeLines(tex, path)
}

#' ATT / ACRT table (SEs below estimates) for the given outcomes and specs.
save_att_acrt_panel <- function(all_coefs, path, outcomes, specs) {
  cols <- expand.grid(spec = specs, outcome = outcomes, stringsAsFactors = FALSE)
  get  <- function(o, s, term) all_coefs |> filter(outcome == o, spec == s, term == !!term)
  est_row <- function(term) paste0(term, " & ", paste(mapply(function(o, s) {
    r <- get(o, s, term); sprintf("\\(%.3f%s\\)", r$estimate, stars(r$estimate, r$se))
  }, cols$outcome, cols$spec), collapse = " & "), " \\\\")
  se_row <- function(term) paste0("& ", paste(mapply(function(o, s) {
    sprintf("(%.3f)", get(o, s, term)$se)
  }, cols$outcome, cols$spec), collapse = " & "), " \\\\")

  k     <- length(specs)
  first <- 2 + k * (seq_along(outcomes) - 1)
  tex <- c(
    sprintf("\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}l%s@{}}", strrep("c", nrow(cols))),
    "\\toprule",
    paste0("& ", paste(sprintf("\\multicolumn{%d}{c}{%s}", k, OUTCOME_HEADERS[outcomes]), collapse = " & "), " \\\\"),
    paste(sprintf("\\cmidrule(lr){%d-%d}", first, first + k - 1), collapse = ""),
    paste0("& ", paste0("(", seq_len(nrow(cols)), ")", collapse = " & "), " \\\\"),
    "\\midrule",
    est_row("ATT"), se_row("ATT"), "\\addlinespace[2pt]",
    est_row("ACRT"), se_row("ACRT"), "\\addlinespace[2pt]",
    paste0("Treated observations & ", paste(mapply(function(o, s)
      format(get(o, s, "ATT")$n_treated_obs, big.mark = ","), cols$outcome, cols$spec), collapse = " & "), " \\\\"),
    "\\midrule",
    paste0("Moderators & ", paste(SPEC_LABELS_PANEL[cols$spec], collapse = " & "), " \\\\"),
    "\\bottomrule",
    "\\end{tabular*}")
  writeLines(tex, path)
}
SPEC_LABELS_PANEL <- c(no_moderators = "None", moderators = "All", top_moderators = "Top 10",
                       stacked = "All (common slopes)")

# Heterogeneity-table row groups and labels, in table order.
HET_GROUPS <- list(
  "Student Demographics" = c(
    perwht    = "White student share",
    perblk    = "Black student share",
    perhsp    = "Hispanic student share",
    perfrl    = "Free or reduced-price lunch share",
    perspeced = "Special education share"),
  "Market Setting" = c(
    baseline_y = "Baseline outcome",
    urban      = "Urban district",
    suburb     = "Suburban district",
    town       = "Town district",
    logenroll  = "Log enrollment",
    num_magnet = "Magnet schools"),
  "School Revenue and Spending" = c(
    p_exp              = "Per-pupil spending",
    p_rev              = "Per-pupil revenue",
    w_pp_exp_percdiff  = "TPS--charter spending gap",
    napcs14_eq_funding = "Equitable operational funding",
    charter_exp        = "Charter-school spending",
    tea_salary         = "Teacher salary",
    str                = "Student--teacher ratio"),
  "Charter Characteristics" = c(
    share_author = "LEA authorizer share",
    share_cmo    = "CMO share",
    share_emo    = "EMO share"),
  "Other State Charter-School Policies" = c(
    napcs14_exem      = "Regulatory exemptions",
    napcs14_nocaps    = "No caps on charter-school growth",
    napcs14_non_renew = "Clear renewal and nonrenewal procedures",
    napcs14_transpar  = "Transparency",
    napcs14_perf      = "Performance-based contracts")
)

#' Heterogeneity-table label for a moderator or its missing dummy.
het_label <- function(v) {
  base <- sub("_miss$", "", v)
  lbl  <- unlist(unname(HET_GROUPS))[base]
  if (is.na(lbl)) lbl <- if (base %in% names(COVARIATE_NAMES)) COVARIATE_NAMES[[base]] else base
  if (grepl("_miss$", v)) return(escape_latex(paste(lbl, "missing")))
  if (base %in% DOLLAR_VARS) lbl <- paste0(lbl, " ($1,000s)")
  escape_latex(lbl)
}

# Table notes (%d = BASELINE_LAG).
# Row descriptions for the <<ROWS>> slot in the notes (see save_heterogeneity_table).
HET_ROWS_LEVEL <- r"---(the first row reports \(\alpha_{0}\), the effect of positive charter exposure (\(D=1\{W>0\}\)) at the mean moderator vector, and each moderator row reports \(\alpha_{k}\), the coefficient on \(D\times X_{k}\); the dose coefficients \(\tau_{0}\) and \(\tau_{k}\) are estimated but not reported)---"
HET_ROWS_CACRT <- r"---(the Level rows report \(\alpha_{0}\) and \(\alpha_{k}\), the coefficients on \(D=1\{W>0\}\) and \(D\times X_{k}\), and the CACRT rows report \(\tau_{0}\) and \(\tau_{k}\), the coefficients on \(W\) and \(W\times X_{k}\), so \(CACRT(X)=\tau_{0}+X'\tau\))---"

HET_NOTES <- r"---(\textit{Notes:} The table reports heterogeneity estimates from the staggered, continuous-dose CGS-DiD (Equation~\ref{eq:DiD_spec}), estimated separately in each cohort \(g\) by calendar-year \(t\) cell. In the first stage, the outcome change \(Y_{t}-Y_{g-1}\) of never-treated district-grades is regressed on the moderators \(X\) to estimate the counterfactual trend. In the second stage, the residualized change of cohort-\(g\) district-grades is regressed on \((1, X, W, WX)\), where \(W\) is the charter enrollment share, so \(CATT(w,X)=\alpha_{0}+X'\alpha+w(\tau_{0}+X'\tau)\). Reported coefficients average the cell estimates over post-treatment cells, weighted by each cell's number of treated observations. Moderators are centered at their enrollment-weighted means: <<ROWS>>. Moderators are measured in the first observed year for each district-grade, except the baseline outcome, \(Y_{g-%d}\), which is centered within each cell; missing values are set to the sample mean and flagged by the missing-value indicators. Dollar amounts are in thousands; urban, suburban, and town districts are relative to rural. Both stages are weighted by grade-level enrollment. Standard errors (in parentheses) are clustered by district and account for the first-stage estimation. \(^{*}p<0.10\), \(^{**}p<0.05\), and \(^{***}p<0.01\).)---"

HET_NOTES_STACKED <- r"---(\textit{Notes:} The table reports heterogeneity estimates from the stacked, staggered, continuous-dose CGS-DiD, using charter enrollment share \(W\) and \(D=1\{W>0\}\). For each cohort \(g\) and post-treatment calendar year \(t\), the outcome change \(Y_{t}-Y_{g-1}\) of never-treated units is first regressed on the moderators \(X\), which removes the counterfactual trend \(\gamma_{0,gt}+X'\gamma_{gt}\) (not reported). The residualized changes of treated units in all post-treatment cells are then stacked and regressed on cell-specific intercepts \(\alpha_{0,gt}\) and dose slopes \(\tau_{0,gt}\) and on \((X, WX)\) with coefficients common to all cells, so \(CATT_{gt}(w,X)=\alpha_{0,gt}+X'\alpha+w(\tau_{0,gt}+X'\tau)\) and \(CACRT_{gt}(X)=\tau_{0,gt}+X'\tau\). Each cell compares cohort \(g\) with never-treated units from base year \(g-1\); only the moderator coefficients are pooled across cells. Moderators are centered at their enrollment-weighted sample means, and the baseline rows report the averages of \(\alpha_{0,gt}\) and \(\tau_{0,gt}\) over post-treatment cells, weighted by each cell's number of treated observations. For each moderator \(k\), the Level row reports \(\alpha_{k}\), the coefficient on \(D\times X_{k}\), which is the treatment-by-moderator interaction reported in a binary-treatment difference-in-differences; the CACRT row reports \(\tau_{k}\), the coefficient on \(W\times X_{k}\). Because the dose is continuous, the gradient of the CATT with respect to \(X_{k}\) at dose \(w\) is \(\alpha_{k}+\tau_{k}w\), so \(\alpha_{k}\) is that gradient as \(w\to 0^{+}\). An alternative summary evaluates the gradient at the mean treated dose, \(\alpha_{k}+\tau_{k}\bar{w}\); we do not report it. Units are district-grades for math and ELA and districts for graduation. Urban, suburban, and town coefficients are interpreted relative to rural districts. Moderators are measured in the first observed year for each unit, except the baseline outcome, which is \(Y_{g-%d}\) centered within each cell. State policy measures are rescaled to \([0,1]\) before centering. Missing moderator values are set to the sample mean and flagged by the missing-value indicators. Per-pupil spending, per-pupil revenue, charter-school spending, and teacher salary are expressed in thousands of dollars. Both stages are weighted by grade-level enrollment (math and ELA) or the graduation-rate denominator (graduation). Standard errors are clustered at the district level and account for estimation of the first stage. Standard errors are shown in parentheses below point estimates. \(^{*}p<0.10\), \(^{**}p<0.05\), and \(^{***}p<0.01\).)---"

HET_NOTES_TOP10 <- r"---(\textit{Notes:} The table reports heterogeneity estimates for the graduation rate from the staggered, continuous-dose CGS-DiD (Equation~\ref{eq:DiD_spec}), estimated separately in each cohort \(g\) by calendar-year \(t\) cell. In the first stage, the outcome change \(Y_{t}-Y_{g-1}\) of never-treated districts is regressed on the full set of moderators \(X\) to estimate the counterfactual trend. In the second stage, the residualized change of cohort-\(g\) districts is regressed on \((1, X_{2}, W, WX_{2})\), where \(W\) is the charter enrollment share and \(X_{2}\) contains the ten moderators with the highest causal-forest variable importance for ELA test scores at \(e=0\) (so they are not selected on graduation data) and their missing-value indicators; hence \(CATT(w,X_{2})=\alpha_{0}+X_{2}'\alpha+w(\tau_{0}+X_{2}'\tau)\). Reported coefficients average the cell estimates over post-treatment cells, weighted by each cell's number of treated observations. Moderators are centered at their weighted means: <<ROWS>>. Moderators are measured in the first observed year for each district, except the baseline outcome, \(Y_{g-%d}\), which is centered within each cell; missing values are set to the sample mean and flagged by the missing-value indicators. Dollar amounts are in thousands. Both stages are weighted by the graduation-rate denominator. Standard errors (in parentheses) are clustered by district and account for the first-stage estimation. \(^{*}p<0.10\), \(^{**}p<0.05\), and \(^{***}p<0.01\).)---"

# Graduation-only version of HET_NOTES_STACKED.
HET_NOTES_STACKED_AFGR <- local({
  n <- sub("Units are district-grades for math and ELA and districts for graduation. ",
           "Units are districts. ", HET_NOTES_STACKED, fixed = TRUE)
  n <- sub("weighted by grade-level enrollment (math and ELA) or the graduation-rate denominator (graduation).",
           "weighted by the graduation-rate denominator.", n, fixed = TRUE)
  stopifnot(!grepl("district-grades|grade-level", n))
  n
})

#' Table notes for a heterogeneity table that omits the CACRT rows: the
#' sentences describing the baseline and Level/CACRT rows are rewritten.
notes_without_cacrt <- function(notes) {
  swaps <- list(
    # HET_NOTES
    c(r"---(so the baseline rows report the level component \(\alpha_{0}\) and CACRT \(\tau_{0}\) at the mean moderator vector.)---",
      r"---(so the baseline row reports the level component \(\alpha_{0}\) at the mean moderator vector.)---"),
    # HET_NOTES_STACKED
    c(r"---(and the baseline rows report the averages of \(\alpha_{0,gt}\) and \(\tau_{0,gt}\) over post-treatment cells,)---",
      r"---(and the baseline row reports the average of \(\alpha_{0,gt}\) over post-treatment cells,)---"),
    # HET_NOTES and HET_NOTES_STACKED
    c(r"---(For each moderator \(k\), the Level row reports \(\alpha_{k}\), the coefficient on \(D\times X_{k}\), which is the treatment-by-moderator interaction reported in a binary-treatment difference-in-differences; the CACRT row reports \(\tau_{k}\), the coefficient on \(W\times X_{k}\).)---",
      r"---(For each moderator \(k\), the table reports \(\alpha_{k}\), the coefficient on \(D\times X_{k}\), which is the treatment-by-moderator interaction reported in a binary-treatment difference-in-differences. The coefficients on the dose terms \(W\) and \(W\times X_{k}\) are estimated but not reported.)---"),
    # HET_NOTES_TOP10
    c(r"---(so the baseline rows report \(\alpha_{0}\) and \(\tau_{0}\) at the mean moderator vector.)---",
      r"---(so the baseline row reports \(\alpha_{0}\) at the mean moderator vector.)---"),
    c(r"---(For each moderator \(k\), the Level row reports \(\alpha_{k}\), the coefficient on \(D\times X_{k}\), and the CACRT row reports \(\tau_{k}\), the coefficient on \(W\times X_{k}\); the gradient)---",
      r"---(For each moderator \(k\), the table reports \(\alpha_{k}\), the coefficient on \(D\times X_{k}\); the coefficients on the dose terms \(W\) and \(W\times X_{k}\) are estimated but not reported. The gradient)---")
  )
  for (s in swaps) notes <- sub(s[1], s[2], notes, fixed = TRUE)
  stopifnot(!grepl("CACRT row|Level row|baseline rows", notes))
  notes
}

#' Heterogeneity table (one column per outcome). Self-contained xltabular with
#' caption and notes. Each moderator gets a Level row (alpha_k); with show_cacrt
#' it also gets a CACRT row (tau_k) and the table has a Component column.
#' Missing-value dummies are estimated but not reported unless show_missing.
save_heterogeneity_table <- function(all_coefs, path, spec = "moderators",
                                     caption = "Heterogeneity in the Effects of Charter-School Expansion: CGS-DiD",
                                     label = "tab:csgdd_heterogeneity", notes = HET_NOTES,
                                     outcomes = OUTCOMES_TO_RUN, show_missing = FALSE,
                                     show_cacrt = HET_SHOW_CACRT) {
  co   <- all_coefs |> filter(spec == !!spec)
  outs <- intersect(outcomes, unique(co$outcome))
  get <- function(o, term) { r <- co |> filter(outcome == o, term == !!term); if (nrow(r)) fmt_cell(r$estimate, r$se) else "" }
  line <- function(lbl, comp, term) {
    cells <- paste(sapply(outs, get, term = term), collapse = " & ")
    if (show_cacrt) paste0(lbl, " & ", comp, " & ", cells, " \\\\") else paste0(lbl, " & ", cells, " \\\\")
  }
  n_lab     <- if (show_cacrt) 2 else 1          # label columns before the outcome columns
  n_col     <- n_lab + length(outs)
  group_hdr <- function(g) sprintf("\\multicolumn{%d}{@{}l}{\\textit{%s}} \\\\", n_col, g)
  if (grepl("<<ROWS>>", notes, fixed = TRUE)) {
    notes <- sub("<<ROWS>>", if (show_cacrt) HET_ROWS_CACRT else HET_ROWS_LEVEL, notes, fixed = TRUE)
  } else if (!show_cacrt) {
    notes <- notes_without_cacrt(notes)
  }

  # moderator rows by group; missing dummies and unlisted moderators last
  x_terms <- unique(co$term[!co$term %in% c("(Intercept)", "W", "ATT", "ACRT") & !grepl("^W:", co$term)])
  miss    <- x_terms[grepl("_miss$", x_terms)]
  groups  <- lapply(HET_GROUPS, function(g) intersect(names(g), x_terms))
  if (show_missing) groups[["Missing-Value Indicators"]] <- miss
  groups[["Other Moderators"]] <- setdiff(x_terms, c(unlist(groups), miss))
  groups  <- groups[lengths(groups) > 0]
  if (!show_missing)
    notes <- sub("flagged by the missing-value indicators.",
                 "flagged by missing-value indicators, which are included in the regressions but not reported.",
                 notes, fixed = TRUE)

  cacrt_row <- function(lbl, term) if (show_cacrt) line(lbl, "CACRT", term) else character(0)
  body <- c(group_hdr("Baseline Treatment Effects"), "\\addlinespace[2pt]",
            line("Positive charter exposure", "Level", "(Intercept)"),
            cacrt_row("Charter enrollment share", "W"))
  for (g in names(groups)) {
    body <- c(body, "\\addlinespace[6pt]", group_hdr(g))
    for (v in groups[[g]])
      body <- c(body, "\\addlinespace[2pt]", line(het_label(v), "Level", v), cacrt_row("", paste0("W:", v)))
  }

  n_treated <- sapply(outs, function(o)
    format(co$n_treated_obs[co$outcome == o & co$term == "ATT"], big.mark = ","))

  lead <- strrep("& ", n_lab)                       # empty label cells
  header <- c(
    "\\toprule",
    paste0(lead, paste(OUTCOME_HEADERS[outs], collapse = " & "), " \\\\"),
    paste(sprintf("\\cmidrule(lr){%d-%d}", n_lab + seq_along(outs), n_lab + seq_along(outs)), collapse = ""),
    paste0(if (show_cacrt) "Moderator & Component & " else "Moderator & ",
           paste0("(", seq_along(outs), ")", collapse = " & "), " \\\\"),
    "\\midrule")

  label_cols <- if (show_cacrt)
    ">{\\raggedright\\arraybackslash}p{0.44\\textwidth}>{\\raggedright\\arraybackslash}p{0.12\\textwidth}" else
    ">{\\raggedright\\arraybackslash}p{0.50\\textwidth}"
  tex <- c(
    "\\clearpage", "\\begingroup", "\\footnotesize",
    "\\setlength{\\LTleft}{0pt}", "\\setlength{\\LTright}{0pt}",
    "\\setlength{\\tabcolsep}{4pt}", "\\renewcommand{\\arraystretch}{1.10}",
    sprintf("\\begin{xltabular}{\\textwidth}{@{}%s*{%d}{>{\\centering\\arraybackslash}X}@{}}",
            label_cols, length(outs)),
    sprintf("\\caption{%s}", caption),
    sprintf("\\label{%s}\\\\", label),
    header, "\\endfirsthead",
    sprintf("\\multicolumn{%d}{@{}l}{\\textit{Table \\thetable\\ continued}} \\\\", n_col),
    header, "\\endhead",
    "\\midrule", sprintf("\\multicolumn{%d}{r}{\\textit{Continued on next page}} \\\\", n_col), "\\endfoot",
    "\\bottomrule", "\\endlastfoot",
    body,
    "\\midrule",
    paste0("Treated observations ", lead, paste(n_treated, collapse = " & "), " \\\\"),
    "\\end{xltabular}",
    "\\vspace{-0.8em}",
    # notes as an ordinary paragraph so they can continue onto the next page
    "{\\singlespacing\\footnotesize", "\\setlength{\\parindent}{0pt}", "\\setlength{\\parskip}{0pt}",
    sprintf(notes, BASELINE_LAG),
    "\\par}", "\\endgroup")
  writeLines(tex, path)
}

# ==============================================================================
# SECTION 7: MAIN EXECUTION
# ==============================================================================

results_dir <- here("quality_reports", "results")
fig_dir     <- here("paper", "figures", "CSGDD_analysis")
tbl_dir     <- here("paper", "tables",  "CSGDD_analysis")
for (d in c(results_dir, fig_dir, tbl_dir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# Skipped when sourced for functions only (OUTCOMES_TO_RUN empty).
if (length(OUTCOMES_TO_RUN) > 0) {
all_coefs <- vector("list", length(OUTCOMES_TO_RUN))
names(all_coefs) <- OUTCOMES_TO_RUN

for (outcome_name in OUTCOMES_TO_RUN) {
  cfg   <- OUTCOMES[[outcome_name]]
  label <- cfg$label
  message("\n", strrep("=", 60), "\nOutcome: ", outcome_name, "\n", strrep("=", 60))

  # data and moderators
  df  <- assign_cohorts(load_outcome_data(cfg))
  mod <- build_csgdd_moderators(df, setdiff(MOD_VARS, "baseline_y"),
                                top_vars = setdiff(TOP_MODS, "baseline_y"))
  x_vars  <- c(mod$x_vars, "baseline_y")
  x2_vars <- c(intersect(TOP_MODS, c(mod$top_vars, "baseline_y")), grep("_miss$", mod$top_vars, value = TRUE))
  message("  Cohorts: ", paste(analysis_cohorts(df), collapse = ", "))
  message("  Stage-2 moderators (top_moderators): ", paste(x2_vars, collapse = ", "))

  # estimate each spec, then event studies and histograms
  specs <- cfg$specs
  res <- setNames(vector("list", length(specs)), specs)
  for (spec in specs) {
    r <- if (spec == "stacked") run_csgdd_stacked(df, mod$mod_matrix, x_vars)
         else run_csgdd(df, mod$mod_matrix, x_vars, spec, x2_vars)
    r$agg_catt <- aggregate_event_study(r, "catt", "catt")
    r$agg_tau  <- aggregate_event_study(r, "tau",  "tau")
    res[[spec]] <- r

    sfx <- SPEC_SUFFIX[[spec]]
    plot_event_study(r$agg_catt, label,
                     file.path(fig_dir, sprintf("csgdd_event_study_%s%s.pdf", outcome_name, sfx)))
    plot_event_study(r$agg_tau, label,
                     file.path(fig_dir, sprintf("csgdd_event_study_tau_%s%s.pdf", outcome_name, sfx)),
                     prefix = "tau", y_label = "Mean CACRT")
    if (spec != "no_moderators") {
      plot_catt_hist(r, label,
                     file.path(fig_dir, sprintf("csgdd_catt_dist_%s%s.pdf", outcome_name, sfx)))
      plot_catt_hist(r, label,
                     file.path(fig_dir, sprintf("csgdd_tau_dist_%s%s.pdf", outcome_name, sfx)),
                     value_col = "tau", label_prefix = "CACRT")
    }
  }

  all_coefs[[outcome_name]] <- bind_rows(lapply(specs, function(s) mutate(res[[s]]$coefs, spec = s))) |>
    mutate(outcome = outcome_name)

  saveRDS(lapply(res, function(r) r[c("catt", "coefs", "agg_catt", "agg_tau")]),
          file.path(results_dir, sprintf("csgdd_results_%s.rds", outcome_name)))
  message("  Done: ", outcome_name)
}

# tables
all_coefs <- bind_rows(all_coefs)
save_att_acrt_table(all_coefs, file.path(tbl_dir, "csgdd_att_acrt.tex"))
if ("afgr" %in% OUTCOMES_TO_RUN)
  save_att_acrt_panel(all_coefs, file.path(tbl_dir, "csgdd_att_acrt_afgr.tex"),
                      outcomes = "afgr", specs = c("no_moderators", "top_moderators"))
save_heterogeneity_table(all_coefs, file.path(tbl_dir, "csgdd_heterogeneity.tex"))
save_heterogeneity_table(all_coefs, file.path(tbl_dir, "csgdd_heterogeneity_stacked.tex"),
                         spec    = "stacked",
                         caption = "Heterogeneity in the Effects of Charter-School Expansion: Stacked CGS-DiD",
                         label   = "tab:csgdd_heterogeneity_stacked",
                         notes   = HET_NOTES_STACKED)
save_heterogeneity_table(all_coefs, file.path(tbl_dir, "csgdd_heterogeneity_afgr.tex"),
                         spec     = "stacked",
                         caption  = "Heterogeneity in the Effects of Charter-School Expansion on Graduation: Stacked CGS-DiD",
                         label    = "tab:csgdd_heterogeneity_afgr",
                         notes    = HET_NOTES_STACKED_AFGR,
                         outcomes = "afgr")
save_heterogeneity_table(all_coefs, file.path(tbl_dir, "csgdd_heterogeneity_top10.tex"),
                         spec    = "top_moderators",
                         caption  = "Heterogeneity in the Effects of Charter-School Expansion on Graduation: CGS-DiD with Ten Moderators",
                         label    = "tab:csgdd_heterogeneity_top10",
                         notes    = HET_NOTES_TOP10,
                         outcomes = "afgr")
message("\nCGS-DiD complete.")
}
