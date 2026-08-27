#' Fit a traditional cross-lagged panel model
#'
#' Autoregressive and cross-lagged paths across the three age bands, with
#' within-wave (residual) covariances and, by default, time-invariant
#' covariates predicting every observed variable. The CLPM is the comparison
#' model for the RI-CLPM: it conflates between- and within-person variance,
#' which is exactly the limitation the RI-CLPM addresses.
#'
#' Estimation follows the analysis plan: robust maximum likelihood (MLR) with
#' full-information maximum likelihood for missing data. `fixed.x = FALSE` so
#' that FIML also covers missingness on the covariates themselves (SES is
#' missing for 214 children); with the lavaan default the model would fall
#' back to listwise deletion on those rows.
#'
#' @title fit_clpm
#' @param df_model Wide analysis data from `make_model_data()`.
#' @param x,y Variable prefixes for the two panel variables.
#' @param waves Age bands, in order.
#' @param covariates Time-invariant covariate column names. Pass
#'   `character(0)` for the covariate-free version used in the like-for-like
#'   fit comparison against the RI-CLPM (fit indices are only comparable
#'   across models fitted to the same set of variables).
#' @param constrain Which lagged paths are held equal across lags: "cross",
#'   "all", or "none".
#' @return A fitted `lavaan` object.
#' @author Taren Sanders
#' @export
fit_clpm <- function(
  df_model,
  x = "vg",
  y = "sdq",
  waves = c(10, 12, 14),
  covariates = c("female", "ses_z_10"),
  constrain = c("cross", "all", "none")
) {
  syntax <- clpm_syntax(
    x = x,
    y = y,
    waves = waves,
    covariates = covariates,
    constrain = match.arg(constrain)
  )
  fit_panel_model(syntax, df_model, x, y, waves)
}
