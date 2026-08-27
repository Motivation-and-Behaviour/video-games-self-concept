#' Cross-lagged estimates by parental warmth group
#'
#' Plots the two cross-lagged paths side by side for the lower and higher
#' warmth groups, in both model families, with 95% confidence intervals on the
#' standardised estimates. H2 predicts weaker (less positive) associations in
#' the higher-warmth group, which reads off as the amber intervals sitting to
#' the left of the blue ones. Heavily overlapping intervals are the visual
#' form of a null moderation test.
#'
#' @title plot_moderation
#' @param models Named list of fitted multi-group models (the free versions).
#' @return A ggplot object.
#' @author Taren Sanders
#' @export
plot_moderation <- function(models) {
  require(ggplot2)

  plot_data <- purrr::imap(models, function(fit, nm) {
    moderation_estimates(fit) |>
      dplyr::filter(grepl("H1", Parameter)) |>
      dplyr::mutate(Model = nm)
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      # Reversed so H1a sits at the top, matching the order used in the tables.
      Parameter = factor(
        Parameter,
        levels = c("SDQ → video games (H1b)", "Video games → SDQ (H1a)")
      ),
      Group = factor(Group, levels = c("Lower warmth", "Higher warmth")),
      # Keep the primary model in the first panel rather than alphabetically.
      Model = factor(Model, levels = names(models))
    )

  ggplot(
    plot_data,
    aes(x = est.std, y = Parameter, colour = Group)
  ) +
    ggplot2::geom_vline(
      xintercept = 0,
      colour = "grey60",
      linetype = "22",
      linewidth = 0.4
    ) +
    ggplot2::geom_pointrange(
      aes(xmin = std.lower, xmax = std.upper),
      position = ggplot2::position_dodge(width = 0.5),
      size = 0.4,
      linewidth = 0.6
    ) +
    ggplot2::facet_wrap(~Model) +
    ggplot2::scale_colour_manual(
      values = c(
        "Lower warmth" = "#4269D0",
        "Higher warmth" = "#EFB118"
      )
    ) +
    ggplot2::labs(
      x = "Standardised estimate (95% CI)",
      y = NULL,
      colour = "Baseline parental warmth"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = "bottom"
    )
}
