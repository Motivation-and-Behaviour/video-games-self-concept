#' Continuous warmth × cross-lag interaction model (H2 sensitivity)
#'
#' The multi-group model is the primary test of H2, but it categorises warmth
#' and discards the middle tertile. This sensitivity model keeps warmth
#' continuous and uses everyone, by adding mean-centred product terms to a
#' path-model CLPM: baseline warmth × the predictor at each lag, predicting the
#' next wave's outcome. A significant product term is moderation.
#'
#' Deliberately a CLPM rather than an RI-CLPM. A continuous interaction with
#' the within-person components would be a latent interaction, which needs
#' LMS/QML estimation that lavaan does not provide and which is not practically
#' estimable in a three-wave model. That is why the multi-group model — which
#' *can* be fitted in both families — is primary.
#'
#' Two limitations worth stating with the results. Products of variables that
#' each have missing data are themselves missing more often, and with
#' `fixed.x = FALSE` lavaan models the products as if multivariate normal,
#' which they are not; MLR standard errors mitigate but do not remove this.
#'
#' @title fit_clpm_interaction
#' @param df_moderation Data from `add_warmth_groups()`, which supplies the
#'   mean-centred `warm_c_10`.
#' @param x,y Variable prefixes for the two panel variables.
#' @param waves Age bands, in order.
#' @param covariates Time-invariant covariate column names.
#' @return A fitted `lavaan` object.
#' @author Taren Sanders
#' @export
fit_clpm_interaction <- function(
  df_moderation,
  x = "vg",
  y = "sdq",
  waves = c(10, 12, 14),
  covariates = c("female", "ses_z_10")
) {
  xv <- paste0(x, "_", waves)
  yv <- paste0(y, "_", waves)
  lags <- seq_len(length(waves) - 1)

  dat <- prep_sem_data(df_moderation, modelled = c(xv, yv))
  # Product terms: baseline warmth by each lag's predictor.
  for (v in c(xv[lags], yv[lags])) {
    dat[[paste0(v, "_xw")]] <- dat[[v]] * dat$warm_c_10
  }

  lagged <- vapply(
    lags,
    function(i) {
      paste0(
        xv[i + 1],
        " ~ ar_x",
        i,
        "*",
        xv[i],
        " + cl_yx*",
        yv[i],
        " + warm_c_10 + mod_yx*",
        yv[i],
        "_xw\n",
        yv[i + 1],
        " ~ ar_y",
        i,
        "*",
        yv[i],
        " + cl_xy*",
        xv[i],
        " + warm_c_10 + mod_xy*",
        xv[i],
        "_xw"
      )
    },
    character(1)
  )

  syntax <- paste(
    "# Lagged paths, warmth main effect, and warmth x predictor interactions",
    paste(lagged, collapse = "\n"),
    "",
    "# Within-wave (residual) covariances",
    paste(paste0(xv, " ~~ ", yv), collapse = "\n"),
    "",
    "# Covariates predict every observed variable",
    covariate_block(c(xv, yv), covariates),
    sep = "\n"
  )

  fit_panel_model(syntax, dat)
}

#' Interaction estimates from the continuous moderation model
#'
#' @title make_interaction_table
#' @param fit A fitted model from `fit_clpm_interaction()`.
#' @return A tibble of the cross-lag and interaction estimates.
#' @author Taren Sanders
#' @export
make_interaction_table <- function(fit) {
  term_names <- c(
    cl_xy = "Video games → SDQ (H1a)",
    cl_yx = "SDQ → video games (H1b)",
    mod_xy = "Warmth × video games → SDQ (H2)",
    mod_yx = "Warmth × SDQ → video games (H2)"
  )

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

  dplyr::left_join(est, std, by = c("lhs", "op", "rhs")) |>
    dplyr::filter(label %in% names(term_names)) |>
    dplyr::distinct(label, .keep_all = TRUE) |>
    dplyr::mutate(Term = unname(term_names[label])) |>
    dplyr::arrange(match(label, names(term_names))) |>
    dplyr::transmute(
      Term,
      `b (SE)` = sprintf("%.3f (%.3f)", est, se),
      `95% CI` = sprintf("[%.3f, %.3f]", ci.lower, ci.upper),
      p = fmt_p(pvalue),
      `β` = fmt_fit(est.std),
      `β 95% CI` = sprintf("[%s, %s]", fmt_fit(std.lower), fmt_fit(std.upper))
    )
}
