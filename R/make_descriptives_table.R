#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df_clean
#' @return
#' @author Taren Sanders
#' @export
make_descriptives_table <- function(df_clean) {
  require(dplyr)

  table_df <- df_clean |>
    dplyr::select(
      age_cat,
      sex,
      age_years,
      ses,
      videogames_hrs,
      sdq_total,
      sdq_internalising,
      sdq_externalising,
      sdq_prosoc,
      sdq_total_p1,
      parenting_warm_p1,
      parenting_warm_p2
    ) |>
    janitor::remove_empty("cols") |>
    dplyr::mutate(
      age_cat = factor(
        sprintf("Age %d/%d", age_cat, age_cat + 1),
        levels = c("Age 10/11", "Age 12/13", "Age 14/15")
      )
    )

  make_table <- function(strata) {
    # Stratifying variables can't also be summary rows (e.g. drop sex from the
    # rows when stratifying by sex).
    vars <- setdiff(colnames(table_df), c("age_cat", strata))
    tab <- tableone::CreateTableOne(
      vars = vars,
      strata = strata,
      data = table_df,
      test = FALSE
    ) |>
      print(printToggle = FALSE, noSpaces = TRUE, varLabels = TRUE)
    tab[grepl("NA|NaN", tab)] <- "-"
    tab
  }

  list(
    by_age = make_table("age_cat"),
    by_sex_age = make_table(c("sex", "age_cat"))
  )
}
