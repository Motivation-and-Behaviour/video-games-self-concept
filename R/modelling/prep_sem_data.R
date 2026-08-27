#' Prepare the wide dataset for lavaan
#'
#' Adds the numeric covariate codings lavaan needs (factors cannot be used as
#' exogenous variables) and drops rows with no data at all on the modelled
#' variables. Those rows contribute nothing under FIML and lavaan would drop
#' them anyway; removing them here makes the model N explicit and stable.
#'
#' For a multi-group model, rows with a missing group are also dropped — a
#' child with no baseline warmth score, or one in the middle warmth tertile,
#' cannot be placed in either group.
#'
#' @title prep_sem_data
#' @param df_model Wide analysis data from `make_model_data()`.
#' @param modelled Column names the model uses as outcomes/predictors.
#' @param group Optional grouping column name.
#' @return A data frame with a numeric `female` column, filtered to rows with
#'   at least one non-missing modelled variable (and a non-missing group).
#' @author Taren Sanders
#' @export
prep_sem_data <- function(df_model, modelled, group = NULL) {
  require(dplyr)

  dat <- df_model |>
    dplyr::mutate(female = as.numeric(sex == "Female")) |>
    dplyr::filter(dplyr::if_any(dplyr::all_of(modelled), ~ !is.na(.x)))

  if (!is.null(group)) {
    dat <- dat |>
      dplyr::filter(!is.na(.data[[group]])) |>
      dplyr::mutate(dplyr::across(dplyr::all_of(group), droplevels))
  }
  dat
}

#' Number of groups a model will be fitted across
#'
#' @param dat Prepared analysis data.
#' @param group Optional grouping column name.
#' @return 1 for a single-group model, otherwise the number of group levels.
#' @noRd
n_model_groups <- function(dat, group) {
  if (is.null(group)) 1L else nlevels(factor(dat[[group]]))
}
