#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df_tidy
#' @param vg_outliers
#' @return
#' @author Taren Sanders
#' @export
clean_data <- function(df_tidy, vg_outliers = c("winsorise", "na")) {
  require(dplyr)
  vg_outliers <- match.arg(vg_outliers)

  scale_vars <- c(
    "videogames_hrs",
    "ses",
    "parenting_warm_p1",
    "parenting_warm_p2"
  )

  df_clean <- df_tidy |>
    dplyr::mutate(
      # egweek is minutes in an average week; analyse as hours/week.
      videogames_hrs = videogames / 60,
      # Unscreened copy, kept for the outlier-tail inspection in the report.
      videogames_hrs_raw = videogames_hrs
    ) |>
    dplyr::select(-videogames)

  # Video game hours > 4 SD above the within-wave mean are winsorised to the
  # threshold (decided 2026-07-27): the tail is a smooth continuation of the
  # distribution with round-number heaping — real heavy gamers overstating —
  # and deleting values because they are large is missing-not-at-random.
  # Set-to-NA is the Step 5 sensitivity analysis (vg_outliers = "na").
  # SES and warmth are deliberately not screened: SES is LSAC's computed
  # composite (extreme values are real families), and low warmth scores are
  # genuine — exactly the contrast the H2 moderation needs.
  df_clean <- df_clean |>
    dplyr::group_by(wave) |>
    dplyr::mutate(
      vg_threshold = mean(videogames_hrs, na.rm = TRUE) +
        4 * sd(videogames_hrs, na.rm = TRUE),
      videogames_hrs = dplyr::if_else(
        videogames_hrs > vg_threshold,
        if (vg_outliers == "winsorise") vg_threshold else NA_real_,
        videogames_hrs
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-vg_threshold)

  # Standardise continuous predictors within each wave.
  df_clean <- df_clean |>
    dplyr::group_by(wave) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(scale_vars),
      ~ scale(.x)[, 1],
      .names = "{.col}_z"
    )) |>
    dplyr::ungroup()

  # SDQ internalising (emotional + peer) and externalising (conduct +
  # hyperactivity) composites: recommended over the five subscales in
  # low-risk community samples (Goodman et al., 2010).
  df_clean <- df_clean |>
    dplyr::mutate(
      sdq_internalising = sdq_emotional + sdq_peer,
      sdq_externalising = sdq_conduct + sdq_hyper
    )

  labelled::var_label(df_clean) <- list(
    age_years = "Age (Years)",
    age_months = "Age (Months)",
    age_cat = "Age Group",
    sex = "Sex",
    ses = "Socioeconomic Position",
    videogames_hrs = "Video Game Use (hrs/week)",
    videogames_hrs_raw = "Video Game Use (hrs/week, unscreened)",
    sdq_total = "SDQ Total Difficulties (Self-Report)",
    sdq_total_p1 = "SDQ Total Difficulties (Parent 1)",
    sdq_emotional = "SDQ Emotional Symptoms",
    sdq_peer = "SDQ Peer Problems",
    sdq_conduct = "SDQ Conduct Problems",
    sdq_hyper = "SDQ Hyperactivity",
    sdq_prosoc = "SDQ Prosocial",
    sdq_internalising = "SDQ Internalising (Emotional + Peer)",
    sdq_externalising = "SDQ Externalising (Conduct + Hyperactivity)",
    parenting_warm_p1 = "Parental Warmth (Parent 1)",
    parenting_warm_p2 = "Parental Warmth (Parent 2)",
    family_cohesion = "Family Cohesion"
  )

  df_clean
}
