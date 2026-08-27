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
#' @param group Optional grouping column for a multi-group model (Step 4
#'   moderation). Rows with a missing group are dropped.
#' @param cross_equal_across_groups Constrain the cross-lags equal across
#'   groups. The moderation test compares this against the free version.
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
  covariates_on = c("between", "within"),
  group = NULL,
  cross_equal_across_groups = FALSE
) {
  dat <- prep_sem_data(
    df_model,
    modelled = c(paste0(x, "_", waves), paste0(y, "_", waves)),
    group = group
  )
  syntax <- riclpm_syntax(
    x = x,
    y = y,
    waves = waves,
    covariates = covariates,
    constrain = match.arg(constrain),
    covariates_on = match.arg(covariates_on),
    n_groups = n_model_groups(dat, group),
    cross_equal_across_groups = cross_equal_across_groups
  )
  fit_panel_model(syntax, dat, group)
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
#' sign of underidentification, so it is suppressed here. So is the multi-group
#' warning about single labels imposing cross-group equality, which is exactly
#' what the constrained moderation model is for.
#'
#' `group.label` is passed explicitly because lavaan otherwise orders groups
#' alphabetically rather than by factor level, which would silently swap which
#' group the `_g1`/`_g2` path labels refer to.
#'
#' @param syntax lavaan model syntax.
#' @param dat Prepared analysis data from `prep_sem_data()`.
#' @param group Optional grouping column name.
#' @return A fitted `lavaan` object.
#' @noRd
fit_panel_model <- function(syntax, dat, group = NULL) {
  withCallingHandlers(
    lavaan::lavaan(
      model = syntax,
      data = dat,
      group = group,
      group.label = if (is.null(group)) NULL else levels(factor(dat[[group]])),
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
      expected <- c("positive definite", "single label per parameter")
      if (
        any(vapply(
          expected,
          grepl,
          logical(1),
          x = conditionMessage(w),
          fixed = TRUE
        ))
      ) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
