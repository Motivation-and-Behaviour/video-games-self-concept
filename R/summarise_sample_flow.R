#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df_clean
#' @param df_model
#' @return
#' @author Taren Sanders
#' @export
summarise_sample_flow <- function(df_clean, df_model) {
  require(dplyr)

  # "Study data" = any of the three core study variables (video game hours,
  # self-report SDQ total, P1 warmth). Covariates alone (sex, SES) don't
  # qualify a child for the FIML analysis sample.
  wave_status <- df_clean |>
    dplyr::group_by(id, age_cat) |>
    dplyr::summarise(
      has_study = any(
        !is.na(videogames_hrs) | !is.na(sdq_total) | !is.na(parenting_warm_p1)
      ),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = age_cat,
      values_from = has_study,
      names_prefix = "w",
      values_fill = FALSE
    )

  complete_core <- df_model |>
    dplyr::filter(
      !is.na(vg_10) & !is.na(vg_12) & !is.na(vg_14),
      !is.na(sdq_10) & !is.na(sdq_12) & !is.na(sdq_14)
    )
  complete_full <- complete_core |>
    dplyr::filter(!is.na(warm_10), !is.na(ses_10), !is.na(sex))

  n_total <- nrow(wave_status)
  flow <- tibble::tibble(
    Criterion = c(
      "B-cohort children in any wave 6–8 data file",
      "Study data (video games, SDQ, or warmth) at ≥1 wave [analysis sample]",
      "Study data at age 10/11",
      "Study data at age 12/13",
      "Study data at age 14/15",
      "Study data at ≥2 waves",
      "Complete cases: video games and SDQ at all three waves",
      "Complete cases with baseline warmth, sex, and SES"
    ),
    `N children` = c(
      n_total,
      sum(wave_status$w10 | wave_status$w12 | wave_status$w14),
      sum(wave_status$w10),
      sum(wave_status$w12),
      sum(wave_status$w14),
      sum(rowSums(wave_status[c("w10", "w12", "w14")]) >= 2),
      nrow(complete_core),
      nrow(complete_full)
    )
  ) |>
    dplyr::mutate(
      `% of cohort file` = sprintf(
        "%.1f",
        100 * `N children` / n_total
      )
    )

  # Wave-participation patterns (the per-pattern missingness view), among
  # children with any study data.
  patterns <- wave_status |>
    dplyr::filter(w10 | w12 | w14) |>
    dplyr::count(w10, w12, w14, name = "n") |>
    dplyr::arrange(dplyr::desc(w10), dplyr::desc(w12), dplyr::desc(w14)) |>
    dplyr::mutate(
      dplyr::across(
        c(w10, w12, w14),
        ~ ifelse(.x, "✓", "–")
      ),
      `%` = sprintf("%.1f", 100 * n / sum(n)),
      n = format(n, big.mark = ",")
    ) |>
    dplyr::select(
      `Age 10/11` = w10,
      `Age 12/13` = w12,
      `Age 14/15` = w14,
      N = n,
      `%`
    )

  list(flow = flow, patterns = patterns)
}
