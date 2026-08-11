# Functions used in the targets workflow for processing continuous water quality data
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Finish cleaning water quality data to create final data set of daily average values for analysis
finish_cwq_data <- function(df_data) {
  # Define WQ measurements to be used in final cleaning
  wq_meas <- c(
    "water_temp",
    "sp_cond",
    "turbidity",
    "ssc",
    "discharge",
    "discharge_tf",
    "velocity_tf"
  )

  df_data |>
    tidyr::pivot_wider(
      id_cols = c(station_abbr, date),
      names_from = parameter,
      values_from = value
    ) |>
    dplyr::select(station_abbr, date, tidyselect::all_of(wq_meas)) |>
    # Remove rows where all WQ measurements are missing
    dplyr::filter_out(dplyr::if_all(tidyselect::all_of(wq_meas), is.na)) |>
    # Convert station_abbr to factor using custom station order
    dplyr::mutate(station_abbr = convert_fct_station_abbr(station_abbr)) |>
    dplyr::arrange(station_abbr, date)
}
