library(targets)
library(tarchetypes)

tar_option_set(
  # packages = c("dplyr"), # Uncomment and add any packages your targets need
  controller = crew::crew_controller_local(
    workers = min(parallel::detectCores() - 2, 20),
    seconds_idle = 15
  )
)

tar_source() # Source all R files in the R/ folder

# Replace these example targets with your own analysis steps
list(
  tar_target(example_data, make_example_data()),
  tar_target(example_summary, summarise_example(example_data))
)
