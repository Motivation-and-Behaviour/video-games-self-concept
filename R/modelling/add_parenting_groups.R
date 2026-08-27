#' The parenting constructs tested as moderators of H2
#'
#' Five constructs, drawn from two different informants because that is how
#' LSAC collects them. Warmth and angry parenting are Parent 1 self-reports.
#' Responsiveness, autonomy-granting and demandingness come only from the study
#' child's own questionnaire, rating their mother and their father separately;
#' there is no P1 version. The mother version is used (decided 2026-08-27):
#' it has the highest coverage of the three, and adding the father version
#' gains only three children.
#'
#' Each entry maps a display label to the baseline (age 10/11) column it is
#' derived from.
#'
#' @title parenting_moderators
#' @return A named character vector of baseline column names.
#' @author Taren Sanders
#' @export
parenting_moderators <- function() {
  c(
    "Warmth (P1)" = "warm_10",
    "Angry parenting (P1)" = "anger_10",
    "Responsiveness (child-rated)" = "response_m_10",
    "Autonomy-granting (child-rated)" = "autonomy_m_10",
    "Demandingness (child-rated)" = "demand_m_10"
  )
}

#' Split children into baseline tertile groups on each parenting moderator
#'
#' The multi-group tests compare children in the lowest and highest thirds of
#' each construct at age 10/11. Tertile extremes are used rather than a median
#' split because a median split discards the contrast the hypothesis is about,
#' while the extremes keep the two groups interpretable and well separated.
#' Children in the middle third get `NA` and are excluded from that construct's
#' multi-group models; the continuous-interaction sensitivity uses everyone.
#'
#' Grouping is at baseline only, so a child stays in one group across all three
#' waves — a time-varying grouping would make the comparison uninterpretable.
#'
#' All five constructs run "higher = more of the named quality"; the three
#' child-reported scales are reverse-scored upstream in `clean_data()` to make
#' that true.
#'
#' @title add_parenting_groups
#' @param df_model Wide analysis data from `make_model_data()`.
#' @return `df_model` with a `<name>_group` factor and a mean-centred
#'   `<name>_c` column for each moderator.
#' @author Taren Sanders
#' @export
add_parenting_groups <- function(df_model) {
  require(dplyr)

  moderators <- parenting_moderators()
  out <- df_model

  for (i in seq_along(moderators)) {
    var <- moderators[[i]]
    stem <- moderator_stem(var)
    x <- out[[var]]
    cuts <- stats::quantile(x, probs = c(1 / 3, 2 / 3), na.rm = TRUE)

    # cut() would treat an NA in `labels` as a literal level name rather than a
    # missing value, so the middle third is dropped explicitly afterwards.
    banded <- cut(
      x,
      breaks = c(-Inf, cuts, Inf),
      labels = c("Lower third", "Middle", "Upper third")
    )
    out[[paste0(stem, "_group")]] <- factor(
      dplyr::if_else(
        banded == "Middle",
        NA_character_,
        as.character(banded)
      ),
      levels = c("Lower third", "Upper third")
    )
    out[[paste0(stem, "_c")]] <- x - mean(x, na.rm = TRUE)
  }

  out
}

#' Short stem for a moderator column, e.g. "response_m_10" -> "response"
#' @noRd
moderator_stem <- function(var) {
  sub("_(m|f)$", "", sub("_[0-9]+$", "", var))
}

#' Group column name for each moderator, keyed by display label
#' @noRd
moderator_groups <- function() {
  moderators <- parenting_moderators()
  stats::setNames(
    paste0(vapply(moderators, moderator_stem, character(1)), "_group"),
    names(moderators)
  )
}

#' Mean-centred column name for each moderator, keyed by display label
#' @noRd
moderator_centred <- function() {
  moderators <- parenting_moderators()
  stats::setNames(
    paste0(vapply(moderators, moderator_stem, character(1)), "_c"),
    names(moderators)
  )
}
