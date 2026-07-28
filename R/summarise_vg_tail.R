#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df_clean
#' @return
#' @author Taren Sanders
#' @export
summarise_vg_tail <- function(df_clean) {
  require(dplyr)

  # Uses the unscreened hours so the checkpoint decision (winsorise vs
  # set-to-NA) can be made by looking at the actual flagged values.
  df_clean |>
    dplyr::filter(!is.na(videogames_hrs_raw)) |>
    dplyr::group_by(age_cat) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean(videogames_hrs_raw),
      sd = sd(videogames_hrs_raw),
      threshold = mean + 4 * sd,
      n_flagged = sum(videogames_hrs_raw > threshold),
      max = max(videogames_hrs_raw),
      flagged_values = paste(
        sort(
          round(videogames_hrs_raw[videogames_hrs_raw > threshold], 1),
          decreasing = TRUE
        ),
        collapse = ", "
      )
    ) |>
    dplyr::transmute(
      `Age band` = sprintf("%d/%d", age_cat, age_cat + 1),
      N = format(n, big.mark = ","),
      `Mean (SD), hrs/wk` = sprintf("%.1f (%.1f)", mean, sd),
      `4 SD threshold` = sprintf("%.1f", threshold),
      `n flagged` = n_flagged,
      `Flagged values (hrs/wk)` = flagged_values
    )
}
