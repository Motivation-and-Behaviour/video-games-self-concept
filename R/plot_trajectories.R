#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df_clean
#' @return
#' @author Taren Sanders
#' @export
plot_trajectories <- function(df_clean) {
  require(ggplot2)

  plot_data <- df_clean |>
    dplyr::transmute(
      age_lab = sprintf("%d/%d", age_cat, age_cat + 1),
      `Video game use (hrs/week)` = videogames_hrs,
      `SDQ total difficulties` = sdq_total
    ) |>
    tidyr::pivot_longer(-age_lab, names_to = "variable") |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(
      variable = factor(
        variable,
        levels = c("Video game use (hrs/week)", "SDQ total difficulties")
      )
    )

  means <- plot_data |>
    dplyr::group_by(variable, age_lab) |>
    dplyr::summarise(
      mean = mean(value),
      se = sd(value) / sqrt(dplyr::n()),
      .groups = "drop"
    )

  ggplot(plot_data, aes(x = age_lab, y = value)) +
    geom_violin(fill = "#4269D0", alpha = 0.25, colour = NA, width = 0.6) +
    geom_line(
      data = means,
      aes(y = mean, group = 1),
      colour = "#4269D0",
      linewidth = 0.7
    ) +
    geom_pointrange(
      data = means,
      aes(y = mean, ymin = mean - 1.96 * se, ymax = mean + 1.96 * se),
      colour = "#4269D0",
      size = 0.4
    ) +
    geom_text(
      data = means,
      aes(y = mean, label = sprintf("%.1f", mean)),
      colour = "grey30",
      size = 3,
      hjust = -0.5,
      vjust = -0.6
    ) +
    facet_wrap(~variable, scales = "free_y") +
    labs(x = "Age band", y = NULL) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())
}
