#' Restrict to children with complete data on the modelled variables
#'
#' Reproduces the proposal's inclusion criterion — "responses across at least
#' three consecutive time points", which with three waves means complete cases
#' — so that it can be compared against the FIML analysis the plan adopted
#' instead. This is the Step 5 sensitivity analysis for that deviation.
#'
#' @title filter_complete_cases
#' @param df_model Wide analysis data from `make_model_data()`.
#' @param x,y Variable prefixes for the two panel variables.
#' @param waves Age bands, in order.
#' @return `df_model` filtered to rows with no missing modelled variable.
#' @author Taren Sanders
#' @export
filter_complete_cases <- function(
  df_model,
  x = "vg",
  y = "sdq",
  waves = c(10, 12, 14)
) {
  modelled <- c(paste0(x, "_", waves), paste0(y, "_", waves))
  df_model[stats::complete.cases(df_model[modelled]), ]
}
