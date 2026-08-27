#' Test whether parental warmth moderates the cross-lagged paths
#'
#' Two levels of test, for each model family. The omnibus test is a scaled
#' chi-square difference between a model with both cross-lags free to differ
#' between the warmth groups and one with them constrained equal; it is the
#' formal test of H2. The follow-up Wald tests ask the same question of each
#' cross-lag separately, so that a moderation confined to one direction is not
#' hidden by the omnibus test.
#'
#' Everything other than the cross-lags — autoregressions, variances, and the
#' random-intercept structure — is free across groups in both models, so the
#' difference isolates the paths H2 is about.
#'
#' @title test_moderation
#' @param models Named list of `list(free = , equal = )` fitted model pairs,
#'   one per model family. Names become the "Model" column.
#' @return A list with `omnibus` and `paths` tibbles.
#' @author Taren Sanders
#' @export
test_moderation <- function(models) {
  path_names <- c(
    cl_xy = "Video games → SDQ (H1a)",
    cl_yx = "SDQ → video games (H1b)"
  )

  omnibus <- purrr::imap(models, function(pair, nm) {
    lrt <- suppressWarnings(lavaan::lavTestLRT(pair$free, pair$equal))
    p_value <- lrt[["Pr(>Chisq)"]][2]
    # Named `p_value`, not `p`: tibble() evaluates columns sequentially, so a
    # later reference to `p` would pick up the formatted character column
    # rather than the number, and compare strings.
    conclusion <- if (p_value < .05) {
      "Cross-lags differ by warmth"
    } else {
      "No evidence of moderation"
    }
    tibble::tibble(
      Model = nm,
      `Δχ²` = sprintf("%.2f", lrt[["Chisq diff"]][2]),
      `Δdf` = as.integer(lrt[["Df diff"]][2]),
      p = fmt_p(p_value),
      Conclusion = conclusion
    )
  }) |>
    dplyr::bind_rows()

  paths <- purrr::imap(models, function(pair, nm) {
    purrr::imap(path_names, function(label, base) {
      w <- suppressWarnings(lavaan::lavTestWald(
        pair$free,
        constraints = sprintf("%s_g1 == %s_g2", base, base)
      ))
      verdict <- if (w$p.value < .05) "Differs by warmth" else "No difference"
      tibble::tibble(
        Model = nm,
        Path = label,
        `χ² (1)` = sprintf("%.2f", w$stat),
        p = fmt_p(w$p.value),
        Verdict = verdict
      )
    }) |>
      dplyr::bind_rows()
  }) |>
    dplyr::bind_rows()

  list(omnibus = omnibus, paths = paths)
}

#' Cross-lagged estimates by warmth group
#'
#' @title make_moderation_table
#' @param models Named list of fitted multi-group models (the free versions).
#' @return A tibble of lagged-path estimates by model family and group.
#' @author Taren Sanders
#' @export
make_moderation_table <- function(models) {
  purrr::imap(models, function(fit, nm) {
    moderation_estimates(fit) |>
      dplyr::mutate(Model = nm, .before = 1)
  }) |>
    dplyr::bind_rows() |>
    dplyr::transmute(
      Model,
      Parameter,
      Lag,
      Group,
      `b (SE)` = sprintf("%.3f (%.3f)", est, se),
      `95% CI` = sprintf("[%.3f, %.3f]", ci.lower, ci.upper),
      p = fmt_p(pvalue),
      `β` = fmt_fit(est.std),
      `β 95% CI` = sprintf("[%s, %s]", fmt_fit(std.lower), fmt_fit(std.upper))
    )
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
      Lag = ifelse(is.na(lag), "Both lags (constrained)", lag_label(lag)),
      Group = group_labels[group]
    ) |>
    dplyr::arrange(match(base, names(path_names)), lag, group)
}
