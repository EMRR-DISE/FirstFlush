# Tests for functions in R/02_utils_filter_logic.R

library(testthat)
library(tibble)

# Mock data matching the structure of cwq_data_strata()
df_mock <- tibble(
  stratum = c(
    "Cache Slough and Liberty Island",
    "Cache Slough and Liberty Island",
    "Confluence",
    "Confluence"
  ),
  station_abbr = c("UCS", "LIB", "RVB", "ANH")
)

station_order <- c("FPT", "SRH", "UCS", "LIB", "RVB", "ANH", "MAL")

test_that("compute_station_updates selects all available stations when parameter changes", {
  # Scenario: User had only 'LIB' selected under 'turbidity', then switches to 'sp_cond'
  res <- compute_station_updates(
    current_strata = c("Cache Slough and Liberty Island", "Confluence"),
    prev_strata = c("Cache Slough and Liberty Island", "Confluence"),
    current_stations = "LIB",
    current_param = "sp_cond",
    prev_param = "turbidity",
    df_filt = df_mock,
    station_order = station_order
  )

  # Should reset and select ALL stations available in df_mock in factor order
  expect_equal(res$choices, c("UCS", "LIB", "RVB", "ANH"))
  expect_equal(res$selected, c("UCS", "LIB", "RVB", "ANH"))
})

test_that("compute_station_updates preserves existing selections when parameter is unchanged", {
  # Scenario: User has 'LIB' selected and parameter remains 'turbidity'
  res <- compute_station_updates(
    current_strata = c("Cache Slough and Liberty Island", "Confluence"),
    prev_strata = c("Cache Slough and Liberty Island", "Confluence"),
    current_stations = c("LIB", "RVB"),
    current_param = "turbidity",
    prev_param = "turbidity",
    df_filt = df_mock,
    station_order = station_order
  )

  expect_equal(res$selected, c("LIB", "RVB"))
})

test_that("compute_station_updates auto-adds stations when a new stratum is selected", {
  # Scenario: User adds 'Confluence' stratum while keeping existing 'Cache Slough' selection ('LIB')
  res <- compute_station_updates(
    current_strata = c("Cache Slough and Liberty Island", "Confluence"),
    prev_strata = "Cache Slough and Liberty Island",
    current_stations = "LIB",
    current_param = "turbidity",
    prev_param = "turbidity",
    df_filt = df_mock, # Now contains Confluence stations (RVB, ANH)
    station_order = station_order
  )

  # Should preserve 'LIB' and auto-add 'RVB' and 'ANH' from the newly added Confluence stratum
  expect_equal(res$selected, c("LIB", "RVB", "ANH"))
})

test_that("compute_station_updates handles initial NULL prev_param without erroring", {
  # Scenario: Initial app load where prev_param is NULL
  res <- compute_station_updates(
    current_strata = "Cache Slough and Liberty Island",
    prev_strata = character(0),
    current_stations = character(0),
    current_param = "turbidity",
    prev_param = NULL,
    df_filt = df_mock[1:2, ],
    station_order = station_order
  )

  # Fallback should select all available choices
  expect_equal(res$choices, c("UCS", "LIB"))
  expect_equal(res$selected, c("UCS", "LIB"))
})
