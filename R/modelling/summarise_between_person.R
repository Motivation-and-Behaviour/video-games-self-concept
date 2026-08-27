#' Who games more, and reports more difficulties, on average?
#'
#' The between-person companion to the primary model. The primary RI-CLPM
#' omits time-invariant covariates because its random intercepts already
#' absorb all stable between-child differences; this asks the separate
#' descriptive question of which children sit high on those stable levels.
#'
#' Estimated on each child's mean across the waves they have data for, rather
#' than by regressing the RI-CLPM's random intercepts on the covariates. That
#' model was fitted first (see `diagnose_between_riclpm()`) and returns an
#' improper solution with negative random-intercept variances, so its
#' coefficients cannot be interpreted. Person means answer the same question
#' with an estimator that behaves.
#'
#' @title summarise_between_person
#' @param df_model Wide analysis data from `make_model_data()`.
#' @param waves Age bands, in order.
#' @return A tibble of covariate effects on each child's average level.
#' @author Taren Sanders
#' @export
summarise_between_person <- function(df_model, waves = c(10, 12, 14)) {
  require(dplyr)

  dat <- df_model |>
    dplyr::mutate(
      female = as.numeric(sex == "Female"),
      vg_mean = rowMeans(
        dplyr::pick(dplyr::all_of(paste0("vg_", waves))),
        na.rm = TRUE
      ),
      sdq_mean = rowMeans(
        dplyr::pick(dplyr::all_of(paste0("sdq_", waves))),
        na.rm = TRUE
      )
    )

  outcomes <- c(
    vg_mean = "Average video game use (hrs/week)",
    sdq_mean = "Average SDQ total difficulties"
  )
  predictor_names <- c(
    female = "Female sex",
    ses_z_10 = "Socioeconomic position (z)"
  )

  purrr::imap(outcomes, function(label, var) {
    model <- stats::lm(
      stats::reformulate(names(predictor_names), var),
      data = dat
    )
    ci <- stats::confint(model)
    coefs <- summary(model)$coefficients
    # Standardised slope: b * SD(predictor) / SD(outcome).
    sd_y <- stats::sd(stats::model.frame(model)[[var]])
    keep <- names(predictor_names)
    tibble::tibble(
      Outcome = label,
      Predictor = unname(predictor_names[keep]),
      `b (SE)` = sprintf("%.3f (%.3f)", coefs[keep, 1], coefs[keep, 2]),
      `95% CI` = sprintf("[%.3f, %.3f]", ci[keep, 1], ci[keep, 2]),
      p = fmt_p(coefs[keep, 4]),
      `β` = fmt_fit(
        coefs[keep, 1] *
          vapply(keep, function(v) stats::sd(dat[[v]], na.rm = TRUE), 1) /
          sd_y
      ),
      `R²` = c(
        sprintf("%.3f", summary(model)$r.squared),
        rep("", length(keep) - 1)
      )
    )
  }) |>
    dplyr::bind_rows()
}

#' Why the random-intercept version of the between-person model is not used
#'
#' Regressing the RI-CLPM's random intercepts on sex and SES was the
#' originally planned specification. It returns an improper solution: both
#' random-intercept variances go negative, with standard errors several times
#' their own estimates, and standardised estimates cannot be computed at all.
#' This reports the evidence for that so the rejection is documented rather
#' than asserted.
#'
#' @title diagnose_between_riclpm
#' @param fit An RI-CLPM fitted with `covariates_on = "between"`.
#' @return A one-column-per-quantity tibble of fit and variance diagnostics.
#' @author Taren Sanders
#' @export
diagnose_between_riclpm <- function(fit) {
  m <- lavaan::fitMeasures(fit)
  variances <- lavaan::parameterEstimates(fit) |>
    dplyr::filter(op == "~~", lhs == rhs, lhs %in% c("RI_x", "RI_y"))

  tibble::tibble(
    Quantity = c(
      "χ² (df)",
      "CFI / RMSEA",
      "Random intercept variance, video games",
      "Random intercept variance, SDQ"
    ),
    Value = c(
      sprintf("%.1f (%d)", m[["chisq.scaled"]], as.integer(m[["df"]])),
      sprintf(
        "%s / %s",
        fmt_fit(unname(m[["cfi.robust"]])),
        fmt_fit(unname(m[["rmsea.robust"]]))
      ),
      sprintf(
        "%.2f (SE %.2f)",
        variances$est[variances$lhs == "RI_x"],
        variances$se[variances$lhs == "RI_x"]
      ),
      sprintf(
        "%.2f (SE %.2f)",
        variances$est[variances$lhs == "RI_y"],
        variances$se[variances$lhs == "RI_y"]
      )
    )
  )
}
