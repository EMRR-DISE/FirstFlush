# Functions used in the targets workflow for processing continuous water quality data
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Calculate daily averages of continuous (15-min) data for a single target branch
# Converts daily averages to NA if the number of missing values in a single day is greater than
# 12 hours (48 15-minute timestamps)
aggregate_to_daily <- function(df_data, data_freq) {
  # If the dataset is empty, just return it
  if (nrow(df_data) == 0) {
    return(df_data)
  }

  if (data_freq == "daily") {
    # If it's already daily data; return as-is
    return(df_data)
  } else if (data_freq == "15-min") {
    # Provide messaging
    parameter <- unique(df_data$parameter)
    station_abbr <- unique(df_data$station_abbr)
    survey <- unique(df_data$survey)
    cli::cli_alert_info(
      "Calculating daily averages for {.val {parameter}} for station {.val {station_abbr}} (collected by {.val {survey}})"
    )

    # Convert continuous timestamps to daily dates and calculate averages
    df_data |>
      dplyr::mutate(date = lubridate::date(datetime)) |>
      dplyr::summarize(
        num_na = sum(is.na(value)),
        value = round(mean(value, na.rm = TRUE), digits = 2),
        .by = c(survey, station_abbr, data_freq, date, parameter)
      ) |>
      dplyr::mutate(
        value = dplyr::if_else(num_na > 48, NA_real_, value),
        .keep = "unused"
      )
  }
}
