#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df_clean
#' @return
#' @author Taren Sanders
#' @export
summarise_missingness <- function(df_clean) {
  require(dplyr)

  key_vars <- c(
    "videogames_hrs",
    "sdq_total",
    "sdq_internalising",
    "sdq_externalising",
    "sdq_total_p1",
    "parenting_warm_p1",
    "parenting_warm_p2",
    "parenting_angry_p1",
    "parenting_angry_p2",
    "parenting_response_m",
    "parenting_response_f",
    "parenting_autonomy_m",
    "parenting_autonomy_f",
    "parenting_demand_m",
    "parenting_demand_f",
    "ses",
    "sex"
  )

  var_names <- purrr::map_chr(
    key_vars,
    ~ labelled::var_label(df_clean[[.x]]) %||% .x
  )

  df_clean |>
    dplyr::group_by(age_cat) |>
    dplyr::summarise(
      n_children = dplyr::n(),
      dplyr::across(dplyr::all_of(key_vars), ~ sum(!is.na(.x)))
    ) |>
    tidyr::pivot_longer(
      -c(age_cat, n_children),
      names_to = "variable",
      values_to = "n_obs"
    ) |>
    dplyr::mutate(
      cell = sprintf(
        "%s (%.1f%%)",
        format(n_obs, big.mark = ","),
        100 * n_obs / n_children
      ),
      age_col = sprintf(
        "Age %d/%d (N = %s)",
        age_cat,
        age_cat + 1,
        format(n_children, big.mark = ",")
      ),
      variable = factor(variable, levels = key_vars, labels = var_names)
    ) |>
    dplyr::select(Variable = variable, age_col, cell) |>
    tidyr::pivot_wider(names_from = age_col, values_from = cell) |>
    dplyr::arrange(Variable)
}
