#' Cross-lagged estimates by parenting moderator group
#'
#' One panel per moderator per model family, showing the two cross-lagged
#' paths for the lower and upper tertile groups with 95% confidence intervals
#' on the standardised estimates. H2 predicts weaker (less positive)
#' associations in the upper-tertile group, which reads off as the amber
#' intervals sitting to the left of the blue ones. Heavily overlapping
#' intervals are the visual form of a null moderation test.
#'
#' @title plot_moderation
#' @param sets Named list of model families, as passed to `test_moderation()`.
#' @param wrap_width Characters at which to wrap moderator labels in the strip.
#' @return A ggplot object.
#' @author Taren Sanders
#' @export
plot_moderation <- function(sets, wrap_width = 18) {
  require(ggplot2)

  plot_data <- moderation_cross_lags(sets) |>
    dplyr::mutate(
      # Reversed so H1a sits at the top, matching the order used in the tables.
      Parameter = factor(
        Parameter,
        levels = c("SDQ → video games (H1b)", "Video games → SDQ (H1a)"),
        labels = c("SDQ → VG (H1b)", "VG → SDQ (H1a)")
      ),
      Group = factor(Group, levels = c("Lower third", "Upper third")),
      Model = factor(Model, levels = names(sets)),
      Moderator = factor(
        stringr::str_wrap(Moderator, wrap_width),
        levels = stringr::str_wrap(names(sets[[1]]), wrap_width)
      )
    )

  ggplot(plot_data, aes(x = est.std, y = Parameter, colour = Group)) +
    ggplot2::geom_vline(
      xintercept = 0,
      colour = "grey60",
      linetype = "22",
      linewidth = 0.4
    ) +
    ggplot2::geom_pointrange(
      aes(xmin = std.lower, xmax = std.upper),
      position = ggplot2::position_dodge(width = 0.6),
      size = 0.3,
      linewidth = 0.5
    ) +
    ggplot2::facet_grid(Moderator ~ Model) +
    ggplot2::scale_colour_manual(
      values = c("Lower third" = "#4269D0", "Upper third" = "#EFB118")
    ) +
    ggplot2::labs(
      x = "Standardised estimate (95% CI)",
      y = NULL,
      colour = "Baseline tertile on the moderator"
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      strip.text.y = element_text(size = 7, angle = 0),
      legend.position = "bottom"
    )
}
