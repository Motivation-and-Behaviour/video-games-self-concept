#' Fit comparison table for a set of panel models
#'
#' Robust (MLR-corrected) fit indices are reported throughout, since all
#' models are estimated with `estimator = "MLR"`.
#'
#' Fit indices and information criteria are only comparable across models
#' fitted to the *same* set of variables, so the table carries an explicit
#' "Variables" column: the covariate-adjusted CLPM is fitted to eight
#' variables and cannot be compared with the six-variable RI-CLPM.
#'
#' @title compare_model_fit
#' @param fits Named list of fitted `lavaan` objects; names become row labels.
#' @return A tibble, one row per model.
#' @author Taren Sanders
#' @export
compare_model_fit <- function(fits) {
  purrr::imap(fits, function(fit, nm) {
    m <- lavaan::fitMeasures(fit)
    get <- function(key) if (key %in% names(m)) unname(m[[key]]) else NA_real_
    tibble::tibble(
      Model = nm,
      Variables = length(lavaan::lavNames(fit, "ov")),
      N = lavaan::lavInspect(fit, "nobs"),
      `χ² (df)` = sprintf(
        "%.1f (%d)",
        get("chisq.scaled"),
        as.integer(get("df"))
      ),
      p = fmt_p(get("pvalue.scaled")),
      CFI = fmt_fit(get("cfi.robust")),
      TLI = fmt_fit(get("tli.robust")),
      `RMSEA [90% CI]` = sprintf(
        "%s [%s, %s]",
        fmt_fit(get("rmsea.robust")),
        fmt_fit(get("rmsea.ci.lower.robust")),
        fmt_fit(get("rmsea.ci.upper.robust"))
      ),
      SRMR = fmt_fit(get("srmr")),
      AIC = sprintf("%.0f", get("aic")),
      BIC = sprintf("%.0f", get("bic"))
    )
  }) |>
    dplyr::bind_rows()
}

#' Test the stationarity (equal-lags) assumption
#'
#' Two things are reported. First, scaled chi-square difference tests of the
#' constrained models against the freely-estimated model — the omnibus
#' stationarity test. Second, univariate score tests on the fully constrained
#' model, which localise *which* lagged paths violate the constraint. That
#' second part is what motivated the primary specification: the
#' autoregressions differ across lags but the cross-lags do not.
#'
#' @title test_stationarity
#' @param fit_free Model with all lagged paths freely estimated.
#' @param fit_cross Model with cross-lags constrained, autoregressions free.
#' @param fit_all Model with all lagged paths constrained.
#' @return A list with `comparisons` (scaled Δχ² tests) and `constraints`
#'   (univariate score tests from the fully constrained model).
#' @author Taren Sanders
#' @export
test_stationarity <- function(fit_free, fit_cross, fit_all) {
  compare <- function(constrained, label) {
    lrt <- suppressWarnings(lavaan::lavTestLRT(fit_free, constrained))
    tibble::tibble(
      Comparison = label,
      `Δχ²` = sprintf("%.2f", lrt[["Chisq diff"]][2]),
      `Δdf` = as.integer(lrt[["Df diff"]][2]),
      p = fmt_p(lrt[["Pr(>Chisq)"]][2]),
      Decision = if (lrt[["Pr(>Chisq)"]][2] < .05) {
        "Constraint rejected"
      } else {
        "Constraint retained"
      }
    )
  }

  comparisons <- dplyr::bind_rows(
    compare(fit_all, "All lagged paths equal vs free"),
    compare(fit_cross, "Cross-lags equal, autoregressions free vs free")
  )

  # Score tests say what releasing each equality constraint would buy. The
  # constraint identifiers (.p12. etc) are matched back to the path labels
  # via the parameter table.
  score <- lavaan::lavTestScore(fit_all, univariate = TRUE, cumulative = FALSE)
  pt <- lavaan::parTable(fit_all)
  labels <- stats::setNames(pt$label, pt$plabel)
  path_names <- c(
    ar_x = "Video games autoregression",
    ar_y = "SDQ autoregression",
    cl_xy = "Video games → SDQ (H1a)",
    cl_yx = "SDQ → video games (H1b)"
  )

  constraints <- tibble::tibble(
    Path = unname(path_names[labels[score$uni$lhs]]),
    `χ² (1)` = sprintf("%.2f", score$uni$X2.scaled),
    p = fmt_p(score$uni$p.value.scaled),
    Verdict = ifelse(
      score$uni$p.value.scaled < .05,
      "Differs across lags",
      "Equal across lags"
    )
  )

  list(
    comparisons = comparisons,
    constraints = constraints,
    cross_lags = compare_cross_lags(fit_free)
  )
}

#' Are the cross-lags equal across lags once the autoregressions are free?
#'
#' The score tests in `test_stationarity()` come from the *fully* constrained
#' model, so they are computed with the autoregressive equality constraints
#' still imposed — and those are badly violated, which can distort them. This
#' asks the same question of the freely estimated model, alongside each lag's
#' own estimate, so that a pooled cross-lag can be checked against the two
#' values it pools. Each lag's p-value tests the unstandardised estimate; see
#' `std_solution()`.
#'
#' @param fit_free Model with all lagged paths freely estimated.
#' @return A tibble, one row per cross-lag.
#' @noRd
compare_cross_lags <- function(fit_free) {
  path_names <- c(
    cl_xy = "Video games → SDQ (H1a)",
    cl_yx = "SDQ → video games (H1b)"
  )
  std <- std_solution(fit_free)

  cell <- function(label) {
    row <- std[std$label == label, ]
    if (nrow(row) == 0) {
      return("—")
    }
    sprintf(
      "%s [%s, %s], p %s",
      fmt_fit(row$est.std[1]),
      fmt_fit(row$ci.lower[1]),
      fmt_fit(row$ci.upper[1]),
      sub("^([^<])", "= \\1", fmt_p(row$pvalue[1]))
    )
  }

  purrr::imap(path_names, function(label, base) {
    w <- suppressWarnings(lavaan::lavTestWald(
      fit_free,
      constraints = sprintf("%s1 == %s2", base, base)
    ))
    tibble::tibble(
      Path = label,
      `Lag 1 (10/11 → 12/13)` = cell(paste0(base, "1")),
      `Lag 2 (12/13 → 14/15)` = cell(paste0(base, "2")),
      `χ² (1)` = sprintf("%.2f", w$stat),
      p = fmt_p(w$p.value),
      Verdict = if (w$p.value < .05) {
        "Differs across lags"
      } else {
        "Equal across lags"
      }
    )
  }) |>
    dplyr::bind_rows()
}

#' Format a fit index to two decimals with a leading dot stripped
#' @noRd
fmt_fit <- function(x) {
  ifelse(is.na(x), "—", sub("^(-?)0\\.", "\\1.", sprintf("%.3f", x)))
}

#' Format a p-value for reporting
#' @noRd
fmt_p <- function(p) {
  ifelse(
    is.na(p),
    "—",
    ifelse(p < .001, "< .001", sub("0\\.", ".", sprintf("%.3f", p)))
  )
}
