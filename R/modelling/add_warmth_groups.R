#' Split children into baseline parental warmth groups
#'
#' The H2 moderation test compares children in the lowest and highest thirds
#' of Parent 1-reported warmth at age 10/11. Tertile extremes are used rather
#' than a median split because a median split discards the contrast the
#' hypothesis is about, while the extremes keep the two groups interpretable
#' and well separated. Children in the middle third get `NA` and are excluded
#' from the multi-group models; the continuous-interaction sensitivity
#' analysis uses everyone.
#'
#' Warmth is grouped at baseline only, so a child stays in one group across
#' all three waves — a time-varying grouping would make the multi-group
#' comparison uninterpretable.
#'
#' @title add_warmth_groups
#' @param df_model Wide analysis data from `make_model_data()`.
#' @return `df_model` with a `warmth_group` factor and a mean-centred
#'   `warm_c_10` column for the continuous-interaction models.
#' @author Taren Sanders
#' @export
add_warmth_groups <- function(df_model) {
  require(dplyr)

  cuts <- stats::quantile(
    df_model$warm_10,
    probs = c(1 / 3, 2 / 3),
    na.rm = TRUE
  )

  # cut() would treat an NA in `labels` as a literal level name rather than a
  # missing value, so the middle third is dropped explicitly afterwards.
  df_model |>
    dplyr::mutate(
      warmth_group = cut(
        warm_10,
        breaks = c(-Inf, cuts, Inf),
        labels = c("Lower warmth", "Middle", "Higher warmth")
      ),
      warmth_group = factor(
        dplyr::if_else(
          warmth_group == "Middle",
          NA_character_,
          as.character(warmth_group)
        ),
        levels = c("Lower warmth", "Higher warmth")
      ),
      warm_c_10 = warm_10 - mean(warm_10, na.rm = TRUE)
    )
}
