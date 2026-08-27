#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df_model
#' @return
#' @author Taren Sanders
#' @export
make_correlation_table <- function(df_model) {
  require(dplyr)

  vars <- c(
    "Video games 10/11" = "vg_10",
    "Video games 12/13" = "vg_12",
    "Video games 14/15" = "vg_14",
    "SDQ total 10/11" = "sdq_10",
    "SDQ total 12/13" = "sdq_12",
    "SDQ total 14/15" = "sdq_14",
    "Warmth (P1) 10/11" = "warm_10",
    "Warmth (P1) 12/13" = "warm_12",
    "Warmth (P1) 14/15" = "warm_14",
    "SES 10/11" = "ses_10",
    "Female sex" = "sex_female"
  )

  dat <- df_model |>
    dplyr::mutate(sex_female = as.numeric(sex == "Female")) |>
    dplyr::select(dplyr::all_of(unname(vars)))

  k <- length(vars)
  cells <- matrix("", nrow = k, ncol = k)
  for (i in seq_len(k)) {
    cells[i, i] <- "—"
    for (j in seq_len(i - 1)) {
      ct <- cor.test(dat[[i]], dat[[j]], use = "pairwise.complete.obs")
      stars <- dplyr::case_when(
        ct$p.value < .001 ~ "\\*\\*\\*",
        ct$p.value < .01 ~ "\\*\\*",
        ct$p.value < .05 ~ "\\*",
        TRUE ~ ""
      )
      cells[i, j] <- paste0(
        sub("0\\.", ".", sprintf("%.2f", ct$estimate)),
        stars
      )
    }
  }
  colnames(cells) <- as.character(seq_len(k))

  tibble::tibble(
    Variable = sprintf("%d. %s", seq_len(k), names(vars)),
    M = sprintf("%.2f", colMeans(dat, na.rm = TRUE)),
    SD = sprintf("%.2f", apply(dat, 2, sd, na.rm = TRUE))
  ) |>
    dplyr::bind_cols(tibble::as_tibble(cells))
}
