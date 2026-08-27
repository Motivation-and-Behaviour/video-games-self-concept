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
      # Only the largest values are listed. The full list runs to ~110
      # characters, which no reasonable page width accommodates, and the point
      # of the column is to show that the tail is a smooth continuation with
      # round-number heaping rather than to enumerate every case.
      flagged_values = list(sort(
        round(videogames_hrs_raw[videogames_hrs_raw > threshold], 1),
        decreasing = TRUE
      ))
    ) |>
    dplyr::transmute(
      `Age band` = sprintf("%d/%d", age_cat, age_cat + 1),
      N = format(n, big.mark = ","),
      `Mean (SD), hrs/wk` = sprintf("%.1f (%.1f)", mean, sd),
      `4 SD threshold` = sprintf("%.1f", threshold),
      `n flagged` = n_flagged,
      `Largest flagged values (hrs/wk)` = vapply(
        flagged_values,
        function(v) {
          shown <- utils::head(v, 8)
          paste0(
            paste(shown, collapse = ", "),
            if (length(v) > length(shown)) {
              sprintf(", … (%d more)", length(v) - length(shown))
            } else {
              ""
            }
          )
        },
        character(1)
      )
    )
}
