library(targets)
library(tarchetypes)

tar_option_set(
  # lavaan + semTools join this list at Step 3 (they need a container rebuild
  # to install; see ANALYSIS_PLAN.md "Packages to add").
  packages = c(
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
  tar_quarto(report, "doc/report.qmd")
)
