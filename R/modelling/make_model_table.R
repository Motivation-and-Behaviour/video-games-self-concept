#' Estimates table for a fitted panel model
#'
#' Reports unstandardised estimates with robust SEs and 95% CIs alongside
#' fully standardised estimates with their own CIs, for the structural
#' parameters: lagged paths, within-wave (residual) correlations, and — for
#' the RI-CLPM — the random-intercept variances and correlation.
#'
#' Constrained paths appear once. Freely-estimated paths appear once per lag,
#' labelled with the age bands they span.
#'
#' @title make_model_table
#' @param fit A fitted `lavaan` object from `fit_clpm()` or `fit_riclpm()`.
#' @param waves Age bands, in order, used to build the path labels.
#' @return A tibble of structural parameter estimates.
#' @author Taren Sanders
#' @export
make_model_table <- function(fit, waves = c(10, 12, 14)) {
  require(dplyr)

  bands <- age_band_labels(waves)
  lag_labels <- paste(bands[-length(bands)], "→", bands[-1])

  est <- lavaan::parameterEstimates(fit, ci = TRUE) |>
    dplyr::select(lhs, op, rhs, label, est, se, pvalue, ci.lower, ci.upper)
  std <- lavaan::standardizedSolution(fit) |>
    dplyr::select(
      lhs,
      op,
      rhs,
      est.std,
      std.lower = ci.lower,
      std.upper = ci.upper
    )
  all_est <- dplyr::left_join(est, std, by = c("lhs", "op", "rhs"))

  # --- Lagged paths -------------------------------------------------------
  path_names <- c(
    ar_x = "Video games → video games",
    ar_y = "SDQ → SDQ",
    cl_xy = "Video games → SDQ (H1a)",
    cl_yx = "SDQ → video games (H1b)"
  )
  lagged <- all_est |>
    dplyr::filter(label != "", op == "~") |>
    dplyr::mutate(
      base = sub("[0-9]+$", "", label),
      lag = suppressWarnings(as.integer(sub("^[a-z_]+", "", label)))
    ) |>
    dplyr::filter(base %in% names(path_names)) |>
    # A constrained path is duplicated across lags in the parameter table.
    dplyr::distinct(label, .keep_all = TRUE) |>
    dplyr::mutate(
      Parameter = unname(path_names[base]),
      Lag = ifelse(is.na(lag), "Both lags (constrained)", lag_labels[lag])
    ) |>
    dplyr::arrange(match(base, names(path_names)), lag)

  # --- Within-wave (residual) correlations --------------------------------
  x_within <- lavaan_within_names(fit, "vg", waves)
  y_within <- lavaan_within_names(fit, "sdq", waves)
  within <- all_est |>
    dplyr::filter(op == "~~", lhs %in% x_within, rhs %in% y_within) |>
    dplyr::mutate(
      Parameter = "Video games ↔ SDQ, within-wave",
      Lag = bands[match(lhs, x_within)]
    )

  out <- dplyr::bind_rows(lagged, within)

  # --- Random intercepts (RI-CLPM only) -----------------------------------
  if (all(c("RI_x", "RI_y") %in% lavaan::lavNames(fit, "lv"))) {
    ri <- all_est |>
      dplyr::filter(op == "~~", lhs %in% c("RI_x", "RI_y"), rhs == lhs) |>
      dplyr::mutate(
        Parameter = ifelse(
          lhs == "RI_x",
          "Random intercept variance, video games",
          "Random intercept variance, SDQ"
        ),
        Lag = "Between-person"
      )
    ri_cov <- all_est |>
      dplyr::filter(op == "~~", lhs == "RI_x", rhs == "RI_y") |>
      dplyr::mutate(
        Parameter = "Random intercept correlation",
        Lag = "Between-person"
      )
    out <- dplyr::bind_rows(out, ri, ri_cov)
  }

  out |>
    dplyr::transmute(
      Parameter,
      Lag,
      `b (SE)` = sprintf("%.3f (%.3f)", est, se),
      `95% CI` = sprintf("[%.3f, %.3f]", ci.lower, ci.upper),
      p = fmt_p(pvalue),
      `β` = fmt_fit(est.std),
      `β 95% CI` = sprintf(
        "[%s, %s]",
        fmt_fit(std.lower),
        fmt_fit(std.upper)
      )
    )
}

#' Age band labels of the form "10/11"
#' @noRd
age_band_labels <- function(waves) {
  paste0(waves, "/", waves + 1)
}

#' Label for a given lag, e.g. "10/11 → 12/13" for lag 1
#' @noRd
lag_label <- function(lag, waves = c(10, 12, 14)) {
  bands <- age_band_labels(waves)
  paste(bands[-length(bands)], "→", bands[-1])[lag]
}

#' Within-person component names for a prefix, if the model has them
#'
#' Falls back to the observed variable names for the CLPM, which has no
#' within-person latent layer.
#' @noRd
lavaan_within_names <- function(fit, prefix, waves) {
  w <- paste0("w", prefix, "_", waves)
  if (all(w %in% lavaan::lavNames(fit, "lv"))) w else paste0(prefix, "_", waves)
}
