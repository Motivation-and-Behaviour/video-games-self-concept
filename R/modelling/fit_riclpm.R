#' Fit a random-intercept cross-lagged panel model
#'
#' The primary model for H1. Random intercepts absorb stable between-person
#' differences in video game use and self-reported difficulties, so the lagged
#' paths estimate within-person prospective effects (Hamaker et al., 2015;
#' Mulder & Hamaker, 2021).
#'
#' Time-invariant covariates are omitted by default (decided 2026-08-27). The
#' random intercepts already absorb every stable between-person difference —
#' observed and unobserved, including sex and SES — so the within-person
#' lagged paths are free of time-invariant confounding by construction, and
#' adding covariates can only partition random-intercept variance. Regressing
#' the random intercepts on sex and SES also fits badly here, because sex's
#' association with gaming strengthens with age (r = -.24 -> -.48) and a path
#' to a stable intercept can only reproduce a constant association. That
#' specification is retained as a Step 5 between-person descriptive model.
#'
#' Three waves is the minimum for an RI-CLPM. It is identified, but leaves
#' only 1 df when all lagged paths are free, so the primary model pools the
#' cross-lags (`constrain = "cross"`).
#'
#' @title fit_riclpm
#' @param df_model Wide analysis data from `make_model_data()`.
#' @param x,y Variable prefixes for the two panel variables.
#' @param waves Age bands, in order.
#' @param covariates Time-invariant covariate column names; empty by default.
#' @param constrain Which lagged paths are held equal across lags: "cross",
#'   "all", or "none".
#' @param covariates_on "between" (covariates predict the random intercepts)
#'   or "within"; ignored when `covariates` is empty.
#' @return A fitted `lavaan` object.
#' @author Taren Sanders
#' @export
fit_riclpm <- function(
  df_model,
  x = "vg",
  y = "sdq",
  waves = c(10, 12, 14),
  covariates = character(0),
  constrain = c("cross", "all", "none"),
  covariates_on = c("between", "within")
) {
  syntax <- riclpm_syntax(
    x = x,
    y = y,
    waves = waves,
    covariates = covariates,
    constrain = match.arg(constrain),
    covariates_on = match.arg(covariates_on)
  )
  fit_panel_model(syntax, df_model, x, y, waves)
}

#' Estimate a panel model with the project's standard lavaan options
#'
#' Shared by `fit_clpm()` and `fit_riclpm()` so both use identical estimator,
#' missing-data, and mean-structure settings.
#'
#' lavaan warns that the parameter covariance matrix is not positive definite
#' whenever equality constraints are imposed with labels: the constrained
#' parameters appear twice in `vcov()`, which is rank-deficient by
#' construction. The warning is an artefact of that parameterisation, not a
#' sign of underidentification, so it is suppressed here.
#'
#' @param syntax lavaan model syntax.
#' @param df_model Wide analysis data.
#' @param x,y Variable prefixes.
#' @param waves Age bands, in order.
#' @return A fitted `lavaan` object.
#' @noRd
fit_panel_model <- function(syntax, df_model, x, y, waves) {
  dat <- prep_sem_data(
    df_model,
    modelled = c(paste0(x, "_", waves), paste0(y, "_", waves))
  )

  withCallingHandlers(
    lavaan::lavaan(
      model = syntax,
      data = dat,
      estimator = "MLR",
      missing = "fiml",
      fixed.x = FALSE,
      meanstructure = TRUE,
      int.ov.free = TRUE,
      int.lv.free = FALSE,
      auto.var = TRUE,
      auto.cov.y = TRUE,
      auto.cov.lv.x = TRUE
    ),
    warning = function(w) {
      if (grepl("positive definite", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
