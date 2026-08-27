#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df_model
#' @return
#' @author Taren Sanders
#' @export
summarise_attrition <- function(df_model) {
  require(dplyr)

  # Baseline = any study data at age 10/11; retained = any study data at
  # either follow-up wave. Comparing retained vs lost on baseline
  # characteristics documents the plausibility of MAR (attrition related to
  # *observed* baseline variables is exactly what FIML can adjust for).
  baseline <- df_model |>
    dplyr::filter(!is.na(vg_10) | !is.na(sdq_10) | !is.na(warm_10)) |>
    dplyr::mutate(
      retained = !is.na(vg_12) |
        !is.na(sdq_12) |
        !is.na(warm_12) |
        !is.na(vg_14) |
        !is.na(sdq_14) |
        !is.na(warm_14)
    )

  format_p <- function(p) {
    ifelse(p < .001, "< .001", sprintf("= %.3f", p))
  }

  cont_vars <- c(
    vg_10 = "Video game use (hrs/week)",
    sdq_10 = "SDQ total difficulties",
    warm_10 = "Parental warmth (P1)",
    ses_10 = "Socioeconomic position"
  )

  cont_rows <- purrr::map2(names(cont_vars), cont_vars, function(var, label) {
    x <- baseline[[var]][baseline$retained]
    y <- baseline[[var]][!baseline$retained]
    tt <- t.test(x, y)
    smd <- (mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)) /
      sqrt((var(x, na.rm = TRUE) + var(y, na.rm = TRUE)) / 2)
    tibble::tibble(
      Variable = label,
      retained = sprintf(
        "%.2f (%.2f)",
        mean(x, na.rm = TRUE),
        sd(x, na.rm = TRUE)
      ),
      lost = sprintf("%.2f (%.2f)", mean(y, na.rm = TRUE), sd(y, na.rm = TRUE)),
      SMD = sprintf("%.2f", smd),
      Test = sprintf("t(%.0f) = %.2f", tt$parameter, tt$statistic),
      p = format_p(tt$p.value)
    )
  }) |>
    dplyr::bind_rows()

  # Sex: χ² plus the two-proportion standardised difference.
  p1 <- mean(baseline$sex[baseline$retained] == "Female", na.rm = TRUE)
  p2 <- mean(baseline$sex[!baseline$retained] == "Female", na.rm = TRUE)
  chi <- chisq.test(table(baseline$sex, baseline$retained))
  sex_row <- tibble::tibble(
    Variable = "Female, %",
    retained = sprintf("%.1f", 100 * p1),
    lost = sprintf("%.1f", 100 * p2),
    SMD = sprintf(
      "%.2f",
      (p1 - p2) / sqrt((p1 * (1 - p1) + p2 * (1 - p2)) / 2)
    ),
    Test = sprintf("χ²(%d) = %.2f", chi$parameter, chi$statistic),
    p = format_p(chi$p.value)
  )

  out <- dplyr::bind_rows(sex_row, cont_rows)
  names(out)[names(out) == "retained"] <- sprintf(
    "Retained (n = %s)",
    format(sum(baseline$retained), big.mark = ",")
  )
  names(out)[names(out) == "lost"] <- sprintf(
    "Lost (n = %s)",
    format(sum(!baseline$retained), big.mark = ",")
  )
  out
}
