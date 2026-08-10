# Helper functions used in the targets workflow for processing continuous water quality data
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Convert station abbreviations to factor using custom station order
convert_fct_station_abbr <- function(vec) {
  # Define custom station order
  station_order <- c(
    "FPT",
    "SRH",
    "GES",
    "MIR",
    "SXS",
    "C62",
    "DWS",
    "RYI-RYF",
    "UCS",
    "LIB",
    "NMR",
    "SMR",
    "LPS",
    "MOK",
    "SJR",
    "MSD",
    "TWA",
    "VCU",
    "ORB",
    "MDM",
    "OBI",
    "HLT",
    "ORQ",
    "SJG",
    "PPT",
    "RVB",
    "SJJ",
    "ANH",
    "MAL",
    "HON",
    "RYC",
    "GZL",
    "NSL",
    "BDL",
    "FMB",
    "GOD",
    "SBR",
    "MRZ",
    "CRQ"
  )

  # Convert vector to factor
  factor(vec, levels = station_order)
}

# Convert stratum to factor using custom strata order
convert_fct_stratum <- function(vec) {
  # Define strata order
  strata_order <- c(
    "Sacramento River Mainstem",
    "Sacramento River Deep Water Ship Channel",
    "Cache Slough and Liberty Island",
    "North and South Forks Mokelumne River",
    "San Joaquin River upstream of Delta",
    "South Delta",
    "Confluence",
    "Suisun and Honker Bays",
    "Suisun Marsh and Montezuma Slough",
    "San Pablo Bay and Carquinez Strait"
  )

  # Convert vector to factor
  factor(vec, levels = strata_order)
}

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
