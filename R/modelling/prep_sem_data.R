#' Prepare the wide dataset for lavaan
#'
#' Adds the numeric covariate codings lavaan needs (factors cannot be used as
#' exogenous variables) and drops rows with no data at all on the modelled
#' variables. Those rows contribute nothing under FIML and lavaan would drop
#' them anyway; removing them here makes the model N explicit and stable.
#'
#' @title prep_sem_data
#' @param df_model Wide analysis data from `make_model_data()`.
#' @param modelled Column names the model uses as outcomes/predictors.
#' @return A data frame with a numeric `female` column, filtered to rows with
#'   at least one non-missing modelled variable.
#' @author Taren Sanders
#' @export
prep_sem_data <- function(df_model, modelled) {
  require(dplyr)

  df_model |>
    dplyr::mutate(female = as.numeric(sex == "Female")) |>
    dplyr::filter(dplyr::if_any(dplyr::all_of(modelled), ~ !is.na(.x)))
}
