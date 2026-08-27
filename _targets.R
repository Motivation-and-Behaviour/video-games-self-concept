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
  tar_quarto(report, "doc/report.qmd")
)
