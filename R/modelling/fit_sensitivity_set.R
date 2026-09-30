#' Fit the Step 5 sensitivity variants of the primary model
#'
#' Each variant changes exactly one analytic choice and leaves everything else
#' as specified for the primary model, so any difference in the cross-lags is
#' attributable to that choice. The set is fitted for both model families,
#' which is why the fitting function is passed in rather than hard-coded —
#' `fit_riclpm()` and `fit_clpm()` take the same arguments and each keeps its
#' own covariate default.
#'
#' The variants correspond to the Step 5 candidate list in ANALYSIS_PLAN.md:
#' the proposal's complete-case criterion, the two SDQ subscale composites,
#' the parent-reported SDQ (informant robustness), the alternative outlier
#' rule, and the transformed video game scale.
#'
#' @title fit_sensitivity_set
#' @param fit_fun Either `fit_riclpm` or `fit_clpm`.
#' @param primary The already-fitted primary model, included as the reference
#'   row rather than refitted.
#' @param df_model Wide analysis data from `make_model_data()`.
#' @param df_model_na As `df_model`, but built from data where flagged video
#'   game outliers were set to missing rather than winsorised.
#' @param df_complete As `df_model`, restricted to complete cases.
#' @return A named list of fitted `lavaan` objects.
#' @author Taren Sanders
#' @export
fit_sensitivity_set <- function(
  fit_fun,
  primary,
  df_model,
  df_model_na,
  df_complete
) {
  list(
    "Primary specification" = primary,
    "Complete cases only" = fit_fun(df_complete),
    "SDQ internalising" = fit_fun(df_model, y = "sdq_int"),
    "SDQ externalising" = fit_fun(df_model, y = "sdq_ext"),
    "Parent-reported SDQ" = fit_fun(df_model, y = "sdq_p1"),
    "Outliers set to missing" = fit_fun(df_model_na),
    "Log video game hours" = fit_fun(df_model, x = "vg_log")
  )
}

#' Compact summary of the cross-lagged paths across sensitivity analyses
#'
#' One row per analysis per model family, showing only the two paths the
#' hypotheses are about plus enough fit information to judge the model. The
#' point is whether the H1 conclusions survive each change, so full estimate
#' tables would bury the comparison.
#'
#' @title make_sensitivity_table
#' @param sets Named list (one entry per model family) of named lists of
#'   fitted models, as returned by `fit_sensitivity_set()`.
#' @return A tibble, one row per analysis per model family.
#' @author Taren Sanders
#' @export
make_sensitivity_table <- function(sets) {
  purrr::imap(sets, function(fits, family) {
    purrr::imap(fits, function(fit, analysis) {
      std <- std_solution(fit)
      m <- lavaan::fitMeasures(fit)
      tibble::tibble(
        Model = family,
        Analysis = analysis,
        N = lavaan::lavInspect(fit, "nobs"),
        `VG → outcome (H1a)` = cross_lag_cell(std, "cl_xy"),
        `Outcome → VG (H1b)` = cross_lag_cell(std, "cl_yx"),
        CFI = fmt_fit(unname(m[["cfi.robust"]])),
        RMSEA = fmt_fit(unname(m[["rmsea.robust"]]))
      )
    }) |>
      dplyr::bind_rows()
  }) |>
    dplyr::bind_rows()
}

#' Format one cross-lag as "β [CI]" with significance stars
#'
#' Stars test the unstandardised estimate; see `std_solution()`.
#' @noRd
cross_lag_cell <- function(std, base) {
  row <- std[std$label == base, ]
  if (nrow(row) == 0) {
    return("—")
  }
  sprintf(
    "%s%s [%s, %s]",
    fmt_fit(row$est.std[1]),
    sig_stars(row$pvalue[1]),
    fmt_fit(row$ci.lower[1]),
    fmt_fit(row$ci.upper[1])
  )
}
