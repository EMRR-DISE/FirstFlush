# Helper functions used in the targets workflow for processing continuous water quality data
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Read the stations metadata YAML and convert it into a flat tibble for branching
read_cwq_stations_meta <- function(yaml_file) {
  yaml_data <- yaml::read_yaml(yaml_file)

  # Bind the list elements into rows of a data frame
  yaml_data$stations |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      parameters = purrr::imap(parameters, \(x, idx) {
        tibble::tibble(parameter_name = idx, parameter_code = as.character(x))
      })
    ) |>
    tidyr::unnest(cols = parameters, keep_empty = TRUE)
}

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

# Finish cleaning water quality data to create final data set of daily average values for analysis
finish_cwq_data <- function(df_data) {
  df_data |>
    tidyr::pivot_wider(
      id_cols = c(station_abbr, date),
      names_from = parameter,
      values_from = value
    ) |>
    dplyr::select(
      station_abbr,
      date,
      water_temp,
      sp_cond,
      turbidity,
      ssc,
      discharge,
      discharge_tf
    ) |>
    dplyr::arrange(station_abbr, date)
}

# Generate station metadata file from cwq_station_metadata, processed_data, and stations shapefile
generate_station_metadata <- function(station_metadata, processed_data) {
  # Import continuous WQ station coordinates for all stations
  sf_stations <- sf::read_sf(
    "data/processed/spatial/first_flush_spatial_data.gpkg",
    layer = "cont_wq_stations"
  )

  # Extract Latitude and Longitude from geom and convert to sf_stations to a tibble
  coords <- sf::st_coordinates(sf_stations)
  df_stations <- sf_stations |>
    dplyr::mutate(latitude = coords[, "Y"], longitude = coords[, "X"]) |>
    sf::st_drop_geometry() |>
    dplyr::rename_with(stringr::str_to_snake)

  # Determine parameters used for each survey and station in processed data
  df_parameters <-
    dplyr::bind_rows(processed_data) |>
    dplyr::distinct(survey, station_abbr, parameter) |>
    dplyr::mutate(
      parameters = paste0(parameter, collapse = ", "),
      .by = c(survey, station_abbr)
    ) |>
    dplyr::distinct(survey, station_abbr, parameters)

  # Join station_metadata to df_stations only keeping stations used in targets workflow
  station_metadata |>
    dplyr::distinct(survey, station_abbr, station_name, api_station_id) |>
    # Add in parameters used
    dplyr::left_join(
      df_parameters,
      by = dplyr::join_by(survey, station_abbr)
    ) |>
    dplyr::left_join(
      df_stations,
      by = dplyr::join_by(survey, api_station_id == station_id)
    ) |>
    dplyr::select(-api_station_id) |>
    tidyr::replace_na(list(stratum = "Out of Bounds")) |>
    dplyr::arrange(station_abbr, survey)
}
