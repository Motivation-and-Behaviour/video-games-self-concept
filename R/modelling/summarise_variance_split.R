#' Split observed variance into between- and within-person parts
#'
#' In an RI-CLPM each observed variable is the sum of a random intercept (the
#' child's stable average) and a within-person component (their deviation from
#' that average at that occasion). The squared standardised random-intercept
#' loading is therefore the proportion of that variable's variance made up of
#' stable between-child differences; the remainder is within-child variation.
#'
#' This is what makes the between-person and within-person results
#' interpretable side by side: it says how much of each variable there even is
#' at each level.
#'
#' @title summarise_variance_split
#' @param fit A fitted RI-CLPM from `fit_riclpm()`.
#' @return A tibble with the between/within split per variable per age band.
#' @author Taren Sanders
#' @export
summarise_variance_split <- function(fit) {
  require(dplyr)

  var_names <- c(RI_x = "Video game use", RI_y = "SDQ total difficulties")

  lavaan::standardizedSolution(fit) |>
    dplyr::filter(op == "=~", lhs %in% names(var_names)) |>
    dplyr::mutate(
      Variable = unname(var_names[lhs]),
      # rhs is the observed variable name, e.g. "vg_10".
      `Age band` = age_band_labels(as.integer(sub("^.*_", "", rhs))),
      between = est.std^2
    ) |>
    dplyr::transmute(
      Variable,
      `Age band`,
      `Between-person %` = sprintf("%.1f", 100 * between),
      `Within-person %` = sprintf("%.1f", 100 * (1 - between))
    )
}
