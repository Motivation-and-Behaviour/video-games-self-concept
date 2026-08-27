library(targets)
library(tarchetypes)

tar_option_set(
  packages = c(
    "lavaan",
    "dplyr",
    "forcats",
    "labelled",
    "tableone",
    "janitor",
    "stringr",
    "glue",
    "ggplot2",
    "tidyr",
    "purrr",
    "broom",
    "tibble"
  ),
  controller = crew::crew_controller_local(
    workers = min(parallel::detectCores() - 2, 20),
    seconds_idle = 15
  )
)

tar_source()

lsac_files <- c(
  "lsacgrb10.sav",
  "lsacgrb12.sav",
  "lsacgrb14.sav"
)
lsac_path <- "/data/2 LSAC 10 General Release/Survey Data/SPSS"
input_files <- file.path(lsac_path, lsac_files)

list(
  tar_files_input(waves, input_files),
  tar_target(
    waves_data,
    read_waves_data(waves),
    pattern = map(waves),
    iteration = "list",
    format = "qs"
  ),
  tar_target(waves_joined, dplyr::bind_rows(waves_data), format = "qs"),
  tar_target(df_tidy, tidy_data(waves_joined), format = "qs"),
  tar_target(df_clean, clean_data(df_tidy), format = "qs"),
  tar_target(df_model, make_model_data(df_clean), format = "qs"),
  # Step 1 checkpoint outputs
  tar_target(missingness_table, summarise_missingness(df_clean)),
  tar_target(vg_tail_table, summarise_vg_tail(df_clean)),
  tar_target(distributions_plot, plot_distributions(df_clean)),
  # Step 2: sample, descriptives, and missingness
  tar_target(sample_flow, summarise_sample_flow(df_clean, df_model)),
  tar_target(descriptives_table, make_descriptives_table(df_clean)),
  tar_target(attrition_table, summarise_attrition(df_model)),
  tar_target(correlation_table, make_correlation_table(df_model)),
  tar_target(trajectory_plot, plot_trajectories(df_clean)),
  # Step 3: primary models for H1 (bidirectional video games <-> SDQ).
  #
  # Decision (2026-08-27): the RI-CLPM omits the time-invariant covariates.
  # Its random intercepts already absorb every stable between-person
  # difference, so the within-person cross-lags are unconfounded by sex and
  # SES by construction. Regressing the random intercepts on them also fits
  # badly, because sex's association with gaming strengthens with age and a
  # path to a stable intercept can only reproduce a constant association.
  # That between-person model moves to Step 5.
  #
  # Decision (2026-08-27): the primary model constrains the cross-lags equal
  # across lags but leaves the autoregressions free. Full stationarity is
  # rejected, but univariate score tests localise the failure entirely to the
  # autoregressions; the cross-lags of interest are equal across lags.
  #
  # The CLPM keeps the covariates (they do adjustment work there). A
  # covariate-free CLPM is also fitted, because fit indices and information
  # criteria are only comparable across models with the same variable set.
  tar_target(clpm_free, fit_clpm(df_model, constrain = "none")),
  tar_target(clpm_cross, fit_clpm(df_model, constrain = "cross")),
  tar_target(clpm_all, fit_clpm(df_model, constrain = "all")),
  tar_target(
    clpm_nocov,
    fit_clpm(df_model, covariates = character(0), constrain = "cross")
  ),
  tar_target(riclpm_free, fit_riclpm(df_model, constrain = "none")),
  tar_target(riclpm_cross, fit_riclpm(df_model, constrain = "cross")),
  tar_target(riclpm_all, fit_riclpm(df_model, constrain = "all")),
  tar_target(
    model_fit_table,
    compare_model_fit(list(
      "CLPM (covariates)" = clpm_cross,
      "CLPM (no covariates)" = clpm_nocov,
      "RI-CLPM (primary)" = riclpm_cross
    ))
  ),
  tar_target(
    stationarity_riclpm,
    test_stationarity(riclpm_free, riclpm_cross, riclpm_all)
  ),
  tar_target(
    stationarity_clpm,
    test_stationarity(clpm_free, clpm_cross, clpm_all)
  ),
  tar_target(riclpm_table, make_model_table(riclpm_cross)),
  tar_target(variance_split, summarise_variance_split(riclpm_cross)),
  tar_target(clpm_table, make_model_table(clpm_cross)),
  tar_target(path_diagram, plot_path_diagram(riclpm_cross)),
  tar_target(clpm_path_diagram, plot_path_diagram(clpm_cross)),
  # Step 4: moderation by parental warmth (H2).
  #
  # Decision (2026-08-27): every moderation model is fitted in both families,
  # so the choice of primary model stays reversible. The multi-group models
  # free everything except the cross-lags across warmth groups, so the
  # constrained-vs-free comparison isolates the paths H2 is about.
  #
  # The continuous-interaction sensitivity is CLPM-only by necessity: the
  # RI-CLPM equivalent is a latent interaction, which needs LMS/QML estimation
  # lavaan does not provide and which is not estimable with three waves.
  tar_target(df_moderation, add_parenting_groups(df_model), format = "qs"),
  tar_target(
    moderation_riclpm,
    fit_moderation_set(fit_riclpm, df_moderation)
  ),
  tar_target(moderation_clpm, fit_moderation_set(fit_clpm, df_moderation)),
  tar_target(
    moderation_sets,
    list("RI-CLPM" = moderation_riclpm, "CLPM" = moderation_clpm)
  ),
  tar_target(moderation_test, test_moderation(moderation_sets)),
  tar_target(moderation_table, make_moderation_table(moderation_sets)),
  tar_target(moderation_plot, plot_moderation(moderation_sets)),
  tar_target(
    moderation_fit_table,
    compare_model_fit(purrr::list_flatten(
      purrr::map(moderation_sets, ~ purrr::map(.x, "free")),
      name_spec = "{outer}, {inner}"
    ))
  ),
  tar_target(clpm_interaction, fit_interaction_set(df_moderation)),
  tar_target(interaction_table, make_interaction_table(clpm_interaction)),
  # Step 5: sensitivity and secondary analyses.
  #
  # Each variant changes one analytic choice and leaves the rest as specified,
  # and the whole set is fitted in both model families for the same
  # reversibility reason as Step 4.
  tar_target(
    df_clean_na,
    clean_data(df_tidy, vg_outliers = "na"),
    format = "qs"
  ),
  tar_target(df_model_na, make_model_data(df_clean_na), format = "qs"),
  tar_target(df_complete, filter_complete_cases(df_model), format = "qs"),
  tar_target(
    sensitivity_riclpm,
    fit_sensitivity_set(
      fit_riclpm,
      riclpm_cross,
      df_model,
      df_model_na,
      df_complete
    )
  ),
  tar_target(
    sensitivity_clpm,
    fit_sensitivity_set(
      fit_clpm,
      clpm_cross,
      df_model,
      df_model_na,
      df_complete
    )
  ),
  tar_target(
    sensitivity_table,
    make_sensitivity_table(list(
      "RI-CLPM" = sensitivity_riclpm,
      "CLPM" = sensitivity_clpm
    ))
  ),
  # Between-person descriptive model: sex and SES predicting the random
  # intercepts. Not a confounding adjustment (see the Step 3 decision) but the
  # answer to "who games more, and reports more difficulties, on average".
  tar_target(
    riclpm_between,
    fit_riclpm(
      df_model,
      covariates = c("female", "ses_z_10"),
      covariates_on = "between"
    )
  ),
  # The planned specification (covariates -> random intercepts) returns an
  # improper solution, so the reported between-person estimates come from
  # person means instead; the diagnostic documents the rejection.
  tar_target(between_diagnostics, diagnose_between_riclpm(riclpm_between)),
  tar_target(between_table, summarise_between_person(df_model)),
  # Exploratory sex moderation. `female` drops out of the CLPM covariates
  # because it has no within-group variance once sex is the grouping variable.
  tar_target(riclpm_sex_free, fit_riclpm(df_model, group = "sex")),
  tar_target(
    riclpm_sex_equal,
    fit_riclpm(df_model, group = "sex", cross_equal_across_groups = TRUE)
  ),
  tar_target(
    clpm_sex_free,
    fit_clpm(df_model, group = "sex", covariates = "ses_z_10")
  ),
  tar_target(
    clpm_sex_equal,
    fit_clpm(
      df_model,
      group = "sex",
      covariates = "ses_z_10",
      cross_equal_across_groups = TRUE
    )
  ),
  tar_target(
    sex_moderation_sets,
    list(
      "RI-CLPM" = list(
        Sex = list(free = riclpm_sex_free, equal = riclpm_sex_equal)
      ),
      "CLPM" = list(Sex = list(free = clpm_sex_free, equal = clpm_sex_equal))
    )
  ),
  tar_target(sex_moderation_test, test_moderation(sex_moderation_sets)),
  tar_target(sex_moderation_plot, plot_moderation(sex_moderation_sets)),
  tar_quarto(
    report,
    "doc/report.qmd",
    extra_files = "doc/reference-portrait.docx"
  )
)
