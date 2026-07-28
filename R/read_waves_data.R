#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param filepath
#' @return
#' @author Taren Sanders
#' @export
read_waves_data <- function(filepath) {
  require(dplyr)

  age_num <- stringr::str_extract(filepath, "\\d{1,2}(?=\\.)")
  cohort_letter <- stringr::str_extract(filepath, "[bk](?=\\d{1,2}\\.)")

  wave_num <- dplyr::case_when(
    stringr::str_detect(filepath, "b6\\.sav") ~ 4,
    stringr::str_detect(filepath, "b8\\.sav") ~ 5,
    stringr::str_detect(filepath, "b10\\.sav") ~ 6,
    stringr::str_detect(filepath, "b12\\.sav") ~ 7,
    stringr::str_detect(filepath, "b14\\.sav") ~ 8,
    stringr::str_detect(filepath, "b16\\.sav") ~ 9.1,
    stringr::str_detect(filepath, "b17\\.sav") ~ 9.2,
    stringr::str_detect(filepath, "k10\\.sav") ~ 4,
    stringr::str_detect(filepath, "k12\\.sav") ~ 5,
    stringr::str_detect(filepath, "k14\\.sav") ~ 6,
    stringr::str_detect(filepath, "k16\\.sav") ~ 7,
    stringr::str_detect(filepath, "k18\\.sav") ~ 8,
    stringr::str_detect(filepath, "k20\\.sav") ~ 9.1,
    stringr::str_detect(filepath, "k21\\.sav") ~ 9.2,
    TRUE ~ NA_real_
  )
  wave_letter <-
    dplyr::case_when(
      age_num == 4 ~ "c",
      age_num == 6 ~ "d",
      age_num == 8 ~ "e",
      age_num == 10 ~ "f",
      age_num == 12 ~ "g",
      age_num == 14 ~ "h",
      age_num == 16 & cohort_letter == "b" ~ "i1",
      age_num == 16 & cohort_letter == "k" ~ "i",
      age_num == 17 ~ "i2",
      age_num == 18 ~ "j",
      age_num == 20 ~ "k1",
      age_num == 21 ~ "k2"
    )

  raw_data <-
    foreign::read.spss(
      filepath,
      to.data.frame = TRUE,
      use.value.labels = TRUE
    ) |>
    dplyr::as_tibble() |>
    dplyr::rename(
      id = hicid,
      wave = wave,
      sex = zf02m1,
      age_years = any_of(glue::glue("{wave_letter}f03m1")),
      age_months = any_of(glue::glue("{wave_letter}scagem")),
      ses = any_of(glue::glue("{wave_letter}sep2")),
      videogames = any_of(glue::glue("{wave_letter}egweek")),
      parenting_warm_p1 = any_of(glue::glue("{wave_letter}awarm")),
      parenting_warm_p2 = any_of(glue::glue("{wave_letter}bwarm")),
      parenting_warm_m = any_of(glue::glue("{wave_letter}mwarm")),
      parenting_warm_f = any_of(glue::glue("{wave_letter}fwarm")),
      parenting_angry_p1 = any_of(glue::glue("{wave_letter}aang")),
      parenting_angry_p2 = any_of(glue::glue("{wave_letter}bang")),
      parenting_angry_m = any_of(glue::glue("{wave_letter}mang")),
      parenting_angry_f = any_of(glue::glue("{wave_letter}fang")),
      parenting_selfefficacy_p1 = any_of(glue::glue("{wave_letter}aeffic")),
      parenting_selfefficacy_p2 = any_of(glue::glue("{wave_letter}beffic")),
      parenting_selfefficacy_m = any_of(glue::glue("{wave_letter}meffic")),
      parenting_selfefficacy_f = any_of(glue::glue("{wave_letter}feffic")),
      parenting_response_p1 = any_of(glue::glue("{wave_letter}aresponse")),
      parenting_response_p2 = any_of(glue::glue("{wave_letter}bresponse")),
      parenting_response_m = any_of(glue::glue("{wave_letter}mresponse")),
      parenting_response_f = any_of(glue::glue("{wave_letter}fresponse")),
      parenting_autonomy_p1 = any_of(glue::glue("{wave_letter}aautonomy")),
      parenting_autonomy_p2 = any_of(glue::glue("{wave_letter}bautonomy")),
      parenting_autonomy_m = any_of(glue::glue("{wave_letter}mautonomy")),
      parenting_autonomy_f = any_of(glue::glue("{wave_letter}fautonomy")),
      parenting_demand_p1 = any_of(glue::glue("{wave_letter}ademand")),
      parenting_demand_p2 = any_of(glue::glue("{wave_letter}bdemand")),
      parenting_demand_m = any_of(glue::glue("{wave_letter}mdemand")),
      parenting_demand_f = any_of(glue::glue("{wave_letter}fdemand")),
      sdq_prosoc = any_of(glue::glue("{wave_letter}cpsoc")),
      sdq_hyper = any_of(glue::glue("{wave_letter}chypr")),
      sdq_emotional = any_of(glue::glue("{wave_letter}cemot")),
      sdq_peer = any_of(glue::glue("{wave_letter}cpeer")),
      sdq_conduct = any_of(glue::glue("{wave_letter}ccondb")),
      sdq_total = any_of(glue::glue("{wave_letter}csdqtb")),
      sdq_total_p1 = any_of(glue::glue("{wave_letter}asdqtb")),
      family_cohesion = any_of(glue::glue("{wave_letter}re06a")),
    ) |>
    dplyr::select(
      id,
      wave,
      cohort,
      sex,
      age_years,
      age_months,
      ses,
      videogames,
      family_cohesion,
      starts_with(c("sdq", "parenting_"))
    )

  raw_data
}
