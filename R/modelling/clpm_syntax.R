#' Build lavaan syntax for the cross-lagged panel models
#'
#' Both builders are parameterised by variable prefix (`x`, `y`) and age band
#' so that the Step 5 secondary outcomes (internalising, externalising,
#' parent-reported SDQ) reuse the same code.
#'
#' Path labels are shared between the two models so downstream tables can pull
#' the same parameters from either fit:
#'   ar_x   autoregression of x        ar_y   autoregression of y
#'   cl_xy  x -> y cross-lag (H1a)     cl_yx  y -> x cross-lag (H1b)
#'
#' `constrain` sets which lagged paths are held equal across the two lags:
#'   "cross"  cross-lags equal, autoregressions free (the primary
#'            specification; decided 2026-08-27 — the full stationarity
#'            constraint is rejected, but score tests localise the failure to
#'            the autoregressions, so only the H1 paths are pooled)
#'   "all"    full stationarity, as originally planned
#'   "none"   every lagged path freely estimated
#' Freely-estimated paths get a lag suffix (`ar_x1`, `ar_x2`); constrained
#' paths share one unsuffixed label.
#'
#' @title clpm_syntax / riclpm_syntax
#' @param x,y Variable prefixes (e.g. "vg", "sdq").
#' @param waves Age bands, in order, forming the column suffixes.
#' @param covariates Time-invariant covariate column names; `character(0)`
#'   for none.
#' @param constrain Which lagged paths to hold equal across lags.
#' @return A length-one character vector of lavaan model syntax.
#' @author Taren Sanders
#' @export
clpm_syntax <- function(
  x = "vg",
  y = "sdq",
  waves = c(10, 12, 14),
  covariates = c("female", "ses_z_10"),
  constrain = c("cross", "all", "none"),
  n_groups = 1,
  cross_equal_across_groups = FALSE
) {
  constrain <- match.arg(constrain)
  xv <- paste0(x, "_", waves)
  yv <- paste0(y, "_", waves)

  lagged <- lagged_block(
    xv,
    yv,
    constrain,
    n_groups,
    cross_equal_across_groups
  )

  # Within-wave association: covariance at wave 1, residual covariance after.
  cov_within <- paste0(xv, " ~~ ", yv)

  paste(
    "# Autoregressive and cross-lagged paths",
    lagged,
    "",
    "# Within-wave (residual) covariances",
    paste(cov_within, collapse = "\n"),
    "",
    "# Covariates predict every observed variable",
    covariate_block(c(xv, yv), covariates),
    sep = "\n"
  )
}

#' @rdname clpm_syntax
#' @param covariates_on Level at which covariates enter the RI-CLPM:
#'   "between" (predicting the random intercepts) or "within" (predicting the
#'   within-person components). Only relevant when `covariates` is non-empty;
#'   the primary model omits them entirely because the random intercepts
#'   already absorb all stable between-person differences.
#' @export
riclpm_syntax <- function(
  x = "vg",
  y = "sdq",
  waves = c(10, 12, 14),
  covariates = character(0),
  constrain = c("cross", "all", "none"),
  covariates_on = c("between", "within"),
  n_groups = 1,
  cross_equal_across_groups = FALSE
) {
  constrain <- match.arg(constrain)
  covariates_on <- match.arg(covariates_on)
  xv <- paste0(x, "_", waves)
  yv <- paste0(y, "_", waves)
  wx <- paste0("w", x, "_", waves)
  wy <- paste0("w", y, "_", waves)

  # Random intercepts: unit loadings on every occasion (between-person part).
  ri <- paste0(
    "RI_x =~ ",
    paste0("1*", xv, collapse = " + "),
    "\n",
    "RI_y =~ ",
    paste0("1*", yv, collapse = " + ")
  )

  # Within-person components: one per occasion, unit loading, and the
  # observed residual variance fixed to zero so the observed variance splits
  # entirely into between (RI) and within parts.
  within <- paste(
    paste0(c(wx, wy), " =~ 1*", c(xv, yv), collapse = "\n"),
    paste0(c(xv, yv), " ~~ 0*", c(xv, yv), collapse = "\n"),
    sep = "\n"
  )

  lagged <- lagged_block(
    wx,
    wy,
    constrain,
    n_groups,
    cross_equal_across_groups
  )
  cov_within <- paste0(wx, " ~~ ", wy)

  # The random intercepts must be orthogonal to the wave-1 within-person
  # components, otherwise the between/within split is not identified
  # (Mulder & Hamaker, 2021).
  orthogonal <- paste0(
    c("RI_x", "RI_x", "RI_y", "RI_y"),
    " ~~ 0*",
    c(wx[1], wy[1], wx[1], wy[1]),
    collapse = "\n"
  )

  cov_targets <- if (identical(covariates_on, "between")) {
    c("RI_x", "RI_y")
  } else {
    c(wx, wy)
  }
  cov_header <- if (identical(covariates_on, "between")) {
    "# Covariates predict the random intercepts (between-person level)"
  } else {
    "# Covariates predict the within-person components"
  }

  paste(
    "# Random intercepts (between-person, stable individual differences)",
    ri,
    "",
    "# Within-person components (occasion-specific deviations)",
    within,
    "",
    "# Lagged effects among the within-person components",
    lagged,
    "",
    "# Within-wave (residual) covariances of the within-person components",
    paste(cov_within, collapse = "\n"),
    "",
    "# Random intercept covariance",
    "RI_x ~~ RI_y",
    "",
    "# Random intercepts orthogonal to the wave-1 within-person components",
    orthogonal,
    if (length(covariates)) "" else NULL,
    if (length(covariates)) cov_header else NULL,
    covariate_block(cov_targets, covariates),
    sep = "\n"
  )
}

#' Autoregressive and cross-lagged path syntax
#'
#' In a multi-group model a bare label constrains a parameter to be equal
#' across groups, while `c(lab_g1, lab_g2)` lets it differ. The moderation
#' test needs the cross-lags equal across groups in one model and free in the
#' other, with everything else — including the autoregressions — free in both,
#' so that the chi-square difference isolates the cross-lags.
#'
#' @param xs,ys Variable names for the two panel variables, in wave order.
#' @param constrain Which lagged paths to hold equal across lags.
#' @param n_groups Number of groups; 1 for a single-group model.
#' @param cross_equal_across_groups Constrain the cross-lags equal across
#'   groups? Ignored when `n_groups` is 1.
#' @return lavaan syntax for the lagged structure.
#' @noRd
lagged_block <- function(
  xs,
  ys,
  constrain,
  n_groups = 1,
  cross_equal_across_groups = FALSE
) {
  lags <- seq_len(length(xs) - 1)
  # A constrained path drops its lag suffix, so both lags share one label.
  lab <- function(base, i) {
    free <- switch(
      constrain,
      all = FALSE,
      none = TRUE,
      cross = startsWith(base, "ar_")
    )
    nm <- paste0(base, if (free) i else "")
    if (n_groups == 1) {
      return(nm)
    }
    if (startsWith(base, "cl_") && cross_equal_across_groups) {
      nm
    } else {
      paste0("c(", paste0(nm, "_g", seq_len(n_groups), collapse = ", "), ")")
    }
  }

  paths <- vapply(
    lags,
    function(i) {
      paste0(
        xs[i + 1],
        " ~ ",
        lab("ar_x", i),
        "*",
        xs[i],
        " + ",
        lab("cl_yx", i),
        "*",
        ys[i],
        "\n",
        ys[i + 1],
        " ~ ",
        lab("ar_y", i),
        "*",
        ys[i],
        " + ",
        lab("cl_xy", i),
        "*",
        xs[i]
      )
    },
    character(1)
  )
  paste(paths, collapse = "\n")
}

#' Regress a set of variables on a set of covariates
#'
#' @param outcomes Character vector of left-hand-side variable names.
#' @param covariates Character vector of right-hand-side variable names.
#' @return lavaan syntax, or "" when there are no covariates.
#' @noRd
covariate_block <- function(outcomes, covariates) {
  if (length(covariates) == 0) {
    return("")
  }
  paste0(outcomes, " ~ ", paste(covariates, collapse = " + "), collapse = "\n")
}
