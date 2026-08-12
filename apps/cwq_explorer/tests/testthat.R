# Runner script to run all tests for the cwq_explorer Shiny App

library(testthat)
library(here)

# Source all helper functions from the app's R/ directory
app_r_files <- list.files(
  here("apps/cwq_explorer/R"),
  pattern = "\\.[Rr]$",
  full.names = TRUE
)
purrr::walk(app_r_files, source)

# Run test files
test_dir(path = here("apps/cwq_explorer/tests/testthat"))
