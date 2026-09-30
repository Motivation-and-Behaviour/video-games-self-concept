#' Standardised solution with the unstandardised p-values
#'
#' `lavaan::standardizedSolution()` tests each standardised estimate with its
#' own delta-method SE. That test is not invariant to the non-linear rescaling
#' from b to β, so the two p-values for the same path can fall on opposite
#' sides of .05 (the within-person video game autoregression at the first lag
#' is .068 for b but .046 for β). Decided 2026-09-30: significance is always
#' taken from the test of the unstandardised parameter, as in the estimates
#' tables, and β is reported as the effect size. This swaps the p-values in so
#' that every β-only output (path diagrams, sensitivity and stationarity
#' tables) agrees with the estimates tables.
#'
#' The β CIs are left as the standardised ones, so for a borderline path the
#' CI and the p-value can disagree — as they do in the estimates tables.
#'
#' @title std_solution
#' @param fit A fitted `lavaan` object.
#' @return `lavaan::standardizedSolution(fit)` with `pvalue` replaced by the
#'   p-value of the corresponding unstandardised estimate.
#' @author Taren Sanders
#' @export
std_solution <- function(fit) {
  std <- lavaan::standardizedSolution(fit)
  est <- lavaan::parameterEstimates(fit)
  keys <- intersect(c("lhs", "op", "rhs", "block", "group"), names(std))
  keys <- intersect(keys, names(est))
  std$pvalue <- NULL
  dplyr::left_join(
    std,
    dplyr::select(est, dplyr::all_of(c(keys, "pvalue"))),
    by = keys
  )
}
