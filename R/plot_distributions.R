#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df_clean
#' @return
#' @author Taren Sanders
#' @export
plot_distributions <- function(df_clean) {
  require(ggplot2)

  plot_data <- df_clean |>
    dplyr::transmute(
      age_lab = sprintf("Age %d/%d", age_cat, age_cat + 1),
      `Video game use (hrs/week)` = videogames_hrs,
      `SDQ total difficulties` = sdq_total,
      `Parental warmth (P1)` = parenting_warm_p1
    ) |>
    tidyr::pivot_longer(-age_lab, names_to = "variable") |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(
      variable = factor(
        variable,
        levels = c(
          "Video game use (hrs/week)",
          "SDQ total difficulties",
          "Parental warmth (P1)"
        )
      )
    )

  ggplot(plot_data, aes(x = value)) +
    geom_histogram(bins = 30, fill = "#4269D0", colour = NA) +
    facet_wrap(
      variable ~ age_lab,
      scales = "free",
      ncol = 3
    ) +
    labs(x = NULL, y = "Count") +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 9),
      panel.spacing = unit(1, "lines")
    )
}
