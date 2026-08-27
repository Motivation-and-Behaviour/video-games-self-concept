#' Fit the multi-group moderation models for every parenting construct
#'
#' For each moderator, two models: one with the cross-lagged paths free to
#' differ between the lower and upper tertile groups, and one with them
#' constrained equal. Everything else — autoregressions, variances, the
#' random-intercept structure — is free across groups in both, so the
#' comparison isolates the paths H2 is about.
#'
#' @title fit_moderation_set
#' @param fit_fun Either `fit_riclpm` or `fit_clpm`.
#' @param df_moderation Data from `add_parenting_groups()`.
#' @param groups Named character vector of grouping columns, keyed by display
#'   label. Defaults to all five parenting constructs.
#' @return A named list of `list(free = , equal = )` pairs, one per moderator.
#' @author Taren Sanders
#' @export
fit_moderation_set <- function(
  fit_fun,
  df_moderation,
  groups = moderator_groups()
) {
  purrr::map(groups, function(g) {
    list(
      free = fit_fun(df_moderation, group = g),
      equal = fit_fun(
        df_moderation,
        group = g,
        cross_equal_across_groups = TRUE
      )
    )
  })
}

#' Test whether each parenting construct moderates the cross-lagged paths
#'
#' Two levels of test per moderator per model family. The omnibus test is a
#' scaled chi-square difference between the cross-lags-free and
#' cross-lags-equal models; it is the formal test of H2. The follow-up Wald
#' tests ask the same question of each cross-lag separately, so a moderation
#' confined to one direction is not hidden by the omnibus test.
#'
#' @title test_moderation
#' @param sets Named list of model families; each a named list of moderators;
#'   each of those a `list(free = , equal = )` pair.
#' @return A list with `omnibus` and `paths` tibbles.
#' @author Taren Sanders
#' @export
test_moderation <- function(sets) {
  path_names <- c(
    cl_xy = "Video games → SDQ (H1a)",
    cl_yx = "SDQ → video games (H1b)"
  )

  omnibus <- purrr::imap(sets, function(family, family_name) {
    purrr::imap(family, function(pair, moderator) {
      lrt <- suppressWarnings(lavaan::lavTestLRT(pair$free, pair$equal))
      # Named `p_value`, not `p`: tibble() evaluates columns sequentially, so a
      # later reference to `p` would pick up the formatted character column
      # rather than the number, and compare strings.
      p_value <- lrt[["Pr(>Chisq)"]][2]
      conclusion <- if (p_value < .05) {
        "Cross-lags differ"
      } else {
        "No evidence of moderation"
      }
      # A chi-square difference test is only meaningful if both models
      # converged to admissible solutions, so improper ones are flagged
      # rather than silently reported.
      improper <- c(
        if (has_improper_solution(pair$free)) "free",
        if (has_improper_solution(pair$equal)) "constrained"
      )
      tibble::tibble(
        Moderator = moderator,
        Model = family_name,
        `Δχ²` = sprintf("%.2f", lrt[["Chisq diff"]][2]),
        `Δdf` = as.integer(lrt[["Df diff"]][2]),
        p = fmt_p(p_value),
        Conclusion = conclusion,
        Solution = if (length(improper)) {
          sprintf(
            "Negative variance in %s model",
            paste(improper, collapse = " and ")
          )
        } else {
          "Admissible"
        }
      )
    }) |>
      dplyr::bind_rows()
  }) |>
    dplyr::bind_rows()

  paths <- purrr::imap(sets, function(family, family_name) {
    purrr::imap(family, function(pair, moderator) {
      purrr::imap(path_names, function(label, base) {
        w <- suppressWarnings(lavaan::lavTestWald(
          pair$free,
          constraints = sprintf("%s_g1 == %s_g2", base, base)
        ))
        verdict <- if (w$p.value < .05) "Differs" else "No difference"
        tibble::tibble(
          Moderator = moderator,
          Model = family_name,
          Path = label,
          `χ² (1)` = sprintf("%.2f", w$stat),
          p = fmt_p(w$p.value),
          Verdict = verdict
        )
      }) |>
        dplyr::bind_rows()
    }) |>
      dplyr::bind_rows()
  }) |>
    dplyr::bind_rows()

  list(
    omnibus = dplyr::arrange(omnibus, Moderator, Model),
    paths = dplyr::arrange(paths, Moderator, Model, Path)
  )
}

#' Cross-lagged estimates by moderator group
#'
#' Restricted to the two cross-lagged paths, since those are what H2 concerns
#' and a full parameter table across five moderators and two model families
#' would be unreadable.
#'
#' @title make_moderation_table
#' @param sets Named list of model families, as passed to `test_moderation()`.
#' @return A tibble of cross-lagged estimates by moderator, family, and group.
#' @author Taren Sanders
#' @export
make_moderation_table <- function(sets) {
  moderation_cross_lags(sets) |>
    dplyr::transmute(
      Moderator,
      Model,
      Path = Parameter,
      Group,
      `b (SE)` = sprintf("%.3f (%.3f)", est, se),
      p = fmt_p(pvalue),
      `β` = fmt_fit(est.std),
      `β 95% CI` = sprintf("[%s, %s]", fmt_fit(std.lower), fmt_fit(std.upper))
    )
}

#' Cross-lagged rows from every fitted moderation model
#'
#' Shared by the table and the plot so both read the same estimates.
#' @noRd
moderation_cross_lags <- function(sets) {
  purrr::imap(sets, function(family, family_name) {
    purrr::imap(family, function(pair, moderator) {
      moderation_estimates(pair$free) |>
        dplyr::filter(grepl("H1", Parameter)) |>
        dplyr::mutate(Moderator = moderator, Model = family_name)
    }) |>
      dplyr::bind_rows()
  }) |>
    dplyr::bind_rows()
}

#' Does a fitted model have any negative latent variance?
#'
#' A negative estimated variance is an inadmissible solution: the model has
#' not converged to a point that can be interpreted, so its estimates and any
#' chi-square difference test built on it are unreliable.
#' @noRd
has_improper_solution <- function(fit) {
  variances <- lavaan::parameterEstimates(fit) |>
    dplyr::filter(op == "~~", lhs == rhs, lhs %in% lavaan::lavNames(fit, "lv"))
  nrow(variances) > 0 && any(variances$est < 0)
}

#' Lagged-path estimates from a multi-group fit, one row per path per group
#'
#' Group numbers are mapped back to their labels via the fit itself rather
#' than assumed, because lavaan orders groups by its own `group.label`.
#' @noRd
moderation_estimates <- function(fit) {
  path_names <- c(
    ar_x = "Video games → video games",
    ar_y = "SDQ → SDQ",
    cl_xy = "Video games → SDQ (H1a)",
    cl_yx = "SDQ → video games (H1b)"
  )
  group_labels <- lavaan::lavInspect(fit, "group.label")

  est <- lavaan::parameterEstimates(fit, ci = TRUE) |>
    dplyr::select(
      lhs,
      op,
      rhs,
      group,
      label,
      est,
      se,
      pvalue,
      ci.lower,
      ci.upper
    )
  std <- lavaan::standardizedSolution(fit) |>
    dplyr::select(
      lhs,
      op,
      rhs,
      group,
      est.std,
      std.lower = ci.lower,
      std.upper = ci.upper
    )

  dplyr::left_join(est, std, by = c("lhs", "op", "rhs", "group")) |>
    dplyr::filter(label != "", op == "~") |>
    dplyr::mutate(
      # Labels look like "ar_x1_g2" (lag 1, group 2) or "cl_xy_g2" (constrained
      # across lags). Strip the group suffix, then any lag digit.
      stem = sub("_g[0-9]+$", "", label),
      lag = suppressWarnings(as.integer(sub("^[a-z_]+", "", stem))),
      base = sub("[0-9]+$", "", stem)
    ) |>
    dplyr::filter(base %in% names(path_names)) |>
    dplyr::distinct(label, group, .keep_all = TRUE) |>
    dplyr::mutate(
      Parameter = unname(path_names[base]),
      Lag = ifelse(is.na(lag), "Both", lag_label(lag)),
      Group = group_labels[group]
    ) |>
    dplyr::arrange(match(base, names(path_names)), lag, group)
}
