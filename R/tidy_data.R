#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param waves_joined
#' @return
#' @author Taren Sanders
#' @export
tidy_data <- function(waves_joined) {
  require(dplyr)

  # Recode the LSAC missing-data sentinels to NA. Verified against the wave
  # 6-8 B-cohort files: the only sentinel occurring in the numeric study
  # variables is -9 (egweek, scagem), and every numeric variable except SES
  # is non-negative by definition, so any negative value is a missing code.
  # SES (?sep2) must be excluded: it is a z-scored composite in which
  # negative values are real data.
  sentinel_vars <- setdiff(
    names(waves_joined)[sapply(waves_joined, is.numeric)],
    "ses"
  )

  waves_joined |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(sentinel_vars),
        ~ replace(.x, .x < 0, NA)
      ),
      dplyr::across(
        where(is.factor),
        ~ dplyr::case_when(
          .x %in% c("-1", "-2", "-3", "-4", "-9") ~ NA,
          TRUE ~ .x
        )
      ),
      dplyr::across(where(is.factor), forcats::fct_drop),
      # B cohort only (see ANALYSIS_PLAN.md): the K cohort's age-14 wave did
      # not field the video game time item. Waves 6-8 map onto design-age
      # bands 10/11, 12/13, and 14/15.
      age_cat = dplyr::case_when(
        wave == 6 ~ 10,
        wave == 7 ~ 12,
        wave == 8 ~ 14,
        TRUE ~ NA_real_
      )
    )
}
