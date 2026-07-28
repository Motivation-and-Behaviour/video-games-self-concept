#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df_clean
#' @return
#' @author Taren Sanders
#' @export
make_model_data <- function(df_clean) {
  require(dplyr)

  first_valid <- function(x) x[which(!is.na(x))[1]]

  # Time-varying study variables, one column per age band (vg_10, sdq_12, ...).
  wide <- df_clean |>
    dplyr::transmute(
      id,
      age_cat,
      vg = videogames_hrs,
      vg_z = videogames_hrs_z,
      sdq = sdq_total,
      sdq_int = sdq_internalising,
      sdq_ext = sdq_externalising,
      sdq_p1 = sdq_total_p1,
      warm = parenting_warm_p1,
      warm_z = parenting_warm_p1_z,
      warm_p2 = parenting_warm_p2
    ) |>
    tidyr::pivot_wider(
      id_cols = id,
      names_from = age_cat,
      values_from = -c(id, age_cat),
      names_glue = "{.value}_{age_cat}"
    )

  # Sex is time-invariant but only recorded in waves the child responded to,
  # so take it from any available wave. SES and age are baseline (age 10/11)
  # covariates per the analysis plan.
  invariant <- df_clean |>
    dplyr::group_by(id) |>
    dplyr::summarise(sex = first_valid(sex))

  baseline <- df_clean |>
    dplyr::filter(age_cat == 10) |>
    dplyr::select(
      id,
      ses_10 = ses,
      ses_z_10 = ses_z,
      age_months_10 = age_months
    )

  wide |>
    dplyr::left_join(invariant, by = "id") |>
    dplyr::left_join(baseline, by = "id")
}
