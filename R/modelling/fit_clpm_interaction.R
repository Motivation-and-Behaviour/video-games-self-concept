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
#' @param df_moderation Data from `add_parenting_groups()`, which supplies the
#'   mean-centred moderator columns.
#' @param moderator Name of the mean-centred moderator column.
#' @param x,y Variable prefixes for the two panel variables.
#' @param waves Age bands, in order.
#' @param covariates Time-invariant covariate column names.
#' @return A fitted `lavaan` object.
#' @author Taren Sanders
#' @export
fit_clpm_interaction <- function(
  df_moderation,
  moderator = "warm_c",
  x = "vg",
  y = "sdq",
  waves = c(10, 12, 14),
  covariates = c("female", "ses_z_10")
) {
  xv <- paste0(x, "_", waves)
  yv <- paste0(y, "_", waves)
  lags <- seq_len(length(waves) - 1)

  dat <- prep_sem_data(df_moderation, modelled = c(xv, yv))
  # Product terms: the baseline moderator by each lag's predictor.
  for (v in c(xv[lags], yv[lags])) {
    dat[[paste0(v, "_xw")]] <- dat[[v]] * dat[[moderator]]
  }
  dat$moderator <- dat[[moderator]]

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
        " + moderator + mod_yx*",
        yv[i],
        "_xw\n",
        yv[i + 1],
        " ~ ar_y",
        i,
        "*",
        yv[i],
        " + cl_xy*",
        xv[i],
        " + moderator + mod_xy*",
        xv[i],
        "_xw"
      )
    },
    character(1)
  )

  syntax <- paste(
    "# Lagged paths, moderator main effect, and moderator x predictor terms",
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

#' Fit the continuous moderation model for every parenting construct
#'
#' @title fit_interaction_set
#' @param df_moderation Data from `add_parenting_groups()`.
#' @param moderators Named character vector of mean-centred moderator columns.
#' @return A named list of fitted `lavaan` objects.
#' @author Taren Sanders
#' @export
fit_interaction_set <- function(
  df_moderation,
  moderators = moderator_centred()
) {
  purrr::map(moderators, function(m) {
    fit_clpm_interaction(df_moderation, moderator = m)
  })
}

#' Interaction estimates from the continuous moderation models
#'
#' Only the two interaction terms are reported: the cross-lags themselves are
#' shown in the primary results, and across five moderators the full set would
#' bury the moderation test.
#'
#' @title make_interaction_table
#' @param fits Named list of models from `fit_interaction_set()`.
#' @return A tibble of the interaction estimates, one row per term per model.
#' @author Taren Sanders
#' @export
make_interaction_table <- function(fits) {
  purrr::imap(fits, function(fit, moderator) {
    interaction_terms(fit) |>
      dplyr::mutate(Moderator = moderator, .before = 1)
  }) |>
    dplyr::bind_rows()
}

#' Interaction rows from one fitted continuous moderation model
#' @noRd
interaction_terms <- function(fit) {
  term_names <- c(
    mod_xy = "Moderator × video games → SDQ",
    mod_yx = "Moderator × SDQ → video games"
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
