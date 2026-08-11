# Functions to process continuous water quality data from various sources for the targets workflow
# These are general functions to standardize data formats before calculating daily averages
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Main process data function that handles the various specific processing functions
process_data <- function(
  df_raw,
  data_api,
  data_api_type,
  survey,
  station_abbr,
  parameter_name,
  data_freq,
  end_date
) {
  # If a download failed or returned an empty tibble, skip processing
  if (nrow(df_raw) == 0) {
    return(tibble::tibble())
  }

  # Provide messaging based on df_raw and argument inputs
  data_src <- if (!is.na(data_api_type)) {
    paste(data_api, data_api_type, sep = "-")
  } else {
    data_api
  }

  station_abbr <- unique(df_raw$station_abbr)
  if ("parameter" %in% names(df_raw)) {
    cli::cli_alert_info(
      "Processing {.val {unique(df_raw$parameter)}} from source {.val {data_src}} for station {.val {station_abbr}} (collected by {.val {survey}})"
    )
  } else {
    cli::cli_alert_info(
      "Processing all parameters from source {.val {data_src}} for station {.val {station_abbr}} (collected by {.val {survey}})"
    )
  }

  # Build the custom processing function names dynamically
  # Construct specific function name based on specific station, parameter (if specified), and
  # data frequency attributes
  clean_station <- tolower(station_abbr)
  clean_freq <- stringr::str_remove_all(tolower(data_freq), "-")
  specific_fn_name <- if (!is.na(parameter_name)) {
    paste(
      "clean",
      clean_station,
      tolower(parameter_name),
      clean_freq,
      sep = "_"
    )
  } else {
    paste("clean", clean_station, clean_freq, sep = "_")
  }

  # Construct default API function name
  default_fn_name <- if (!is.na(data_api_type)) {
    paste("clean", tolower(data_api), tolower(data_api_type), sep = "_")
  } else {
    paste("clean", tolower(data_api), sep = "_")
  }

  # Append survey to default_fn_name for all inputs except for those from the USGS API
  default_fn_name <- if (tolower(data_api) == "usgs") {
    default_fn_name
  } else {
    survey_clean <- tolower(stringr::str_remove_all(survey, "\\s|\\)"))
    survey_clean <- stringr::str_replace_all(survey_clean, "[:punct:]", "_")
    default_fn_name <- paste(default_fn_name, survey_clean, sep = "_")
  }

  # If a specific function exists, use that one, otherwise fall back to default function
  if (exists(specific_fn_name)) {
    cli::cli_alert_info(
      "Applying custom processing with {.fun {specific_fn_name}}"
    )
    function_name <- specific_fn_name
  } else if (exists(default_fn_name)) {
    function_name <- default_fn_name
  } else {
    # Provide error if neither functions exist in the sourced environment
    cli::cli_abort(c(
      "x" = "Neither {.fun {specific_fn_name}} nor default {.fun {default_fn_name}} exist",
      "!" = "At least one of these functions must be available in the sourced functions within the {.val code/functions} directory"
    ))
  }

  # Route the data to its specific cleaning sub-function
  clean_fun <- get(function_name)

  # Define all possible arguments for clean_fun into a single named list
  all_available_args <- tibble::lst(df_raw, end_date)

  # Find out exactly which arguments the target function accepts
  expected_arg_names <- names(formals(clean_fun))

  # Filter all possible arguments to match only what the function expects
  matched_args <- all_available_args[
    names(all_available_args) %in% expected_arg_names
  ]

  # Dynamically execute the function with the custom-tailored argument list
  processed_df <- do.call(clean_fun, matched_args)

  return(processed_df)
}

# Helper function to add explicit NA values for any missing timestamps for continuous (15-min) data
fill_missing_datetime <- function(df_data) {
  df_data |>
    # Make sure all timestamps fall on an exact 15-minute interval
    dplyr::mutate(
      datetime = lubridate::round_date(datetime, unit = "15 minute")
    ) |>
    # Remove any overlapping timestamps
    dplyr::distinct(datetime, .keep_all = TRUE) |>
    tidyr::complete(datetime = seq(min(datetime), max(datetime), by = "15 min"))
}

# Process USGS continuous (15-min) data for a single target branch
clean_usgs_continuous <- function(df_raw) {
  df_clean <- df_raw |>
    dplyr::select(
      survey,
      station_abbr,
      data_freq,
      datetime = time,
      parameter,
      value
    ) |>
    # Remove NA values so cleaning up duplicates doesn't result in an NA value being selected
    tidyr::drop_na(value) |>
    # Fill in any missing timestamps
    fill_missing_datetime() |>
    tidyr::fill(survey, station_abbr, data_freq, parameter)

  return(df_clean)
}

# Process USGS daily mean data for a single target branch
clean_usgs_daily <- function(df_raw) {
  df_clean <- df_raw |>
    dplyr::select(
      survey,
      station_abbr,
      data_freq,
      date = time,
      parameter,
      value
    ) |>
    # Remove any duplicate values
    tidyr::drop_na(value) |>
    dplyr::distinct(date, .keep_all = TRUE)

  return(df_clean)
}

# Process DWR-CEMP continuous (15-min) data from EDI data package for a single target branch
clean_edi_dwr_cemp <- function(df_raw, end_date) {
  df_clean <- df_raw |>
    # Create datetime column from date and time
    dplyr::mutate(
      datetime = lubridate::ymd_hms(
        paste(date, hms::as_hms(time)),
        tz = "Etc/GMT+8"
      )
    ) |>
    dplyr::select(
      survey,
      station_abbr,
      data_freq,
      datetime,
      water_temp = watertemperature,
      sp_cond = spc,
      turbidity
    ) |>
    # Remove rows with NA values for all measurements so cleaning up duplicates doesn't result
    # in NA values being selected
    dplyr::filter_out(dplyr::if_all(
      c(water_temp, sp_cond, turbidity),
      is.na
    )) |>
    # Fill in any missing timestamps
    fill_missing_datetime() |>
    dplyr::filter(lubridate::date(datetime) <= end_date) |>
    tidyr::fill(survey, station_abbr, data_freq) |>
    tidyr::pivot_longer(
      cols = tidyselect::where(is.numeric),
      names_to = "parameter",
      values_to = "value"
    )

  return(df_clean)
}

# Process DWR-NCRO (WQ) continuous (15-min) data from EDI data package for a single target branch
clean_edi_dwr_ncro_wq <- function(df_raw, end_date) {
  df_clean <- df_raw |>
    # Parse Date_Time column to datetime
    dplyr::mutate(datetime = lubridate::mdy_hms(Date_Time, tz = "Etc/GMT+8")) |>
    dplyr::select(
      survey,
      station_abbr,
      data_freq,
      datetime,
      water_temp = Water_Temperature,
      sp_cond = Specific_Conductance,
      turbidity = Turbidity
    ) |>
    # Fill in any missing timestamps
    # Remove rows with NA values for all measurements so cleaning up duplicates doesn't result
    # in NA values being selected
    dplyr::filter_out(dplyr::if_all(
      c(water_temp, sp_cond, turbidity),
      is.na
    )) |>
    fill_missing_datetime() |>
    dplyr::filter(lubridate::date(datetime) <= end_date) |>
    tidyr::fill(survey, station_abbr, data_freq) |>
    tidyr::pivot_longer(
      cols = tidyselect::where(is.numeric),
      names_to = "parameter",
      values_to = "value"
    )

  return(df_clean)
}

# Process DWR-NCRO (WQ) or (Tide) continuous (15-min) data from CNRA data portal for a single
# target branch. This is a general function to be used in both wq and tide variations of this
# cleaning function, because the processing steps are identical.
clean_cnra_continuous_dwr_ncro_gen <- function(df_raw, end_date) {
  df_clean <- df_raw |>
    # Parse Date column to datetime
    dplyr::mutate(datetime = lubridate::mdy_hms(Date, tz = "Etc/GMT+8")) |>
    dplyr::select(
      survey,
      station_abbr,
      data_freq,
      datetime,
      parameter,
      value = tidyselect::any_of(c("Point", "Inst"))
    ) |>
    # Remove NA values so cleaning up duplicates doesn't result in an NA value being selected
    tidyr::drop_na(value) |>
    # Fill in any missing timestamps
    fill_missing_datetime() |>
    dplyr::filter(lubridate::date(datetime) <= end_date) |>
    tidyr::fill(survey, station_abbr, data_freq, parameter)

  return(df_clean)
}

clean_cnra_continuous_dwr_ncro_wq <- clean_cnra_continuous_dwr_ncro_gen
clean_cnra_continuous_dwr_ncro_tide <- clean_cnra_continuous_dwr_ncro_gen

# Process DWR-NCRO (WQ) or (Tide) continuous (15-min) data downloaded from the WDL and saved
# locally for a single target branch. This is a general function to be used in both wq and tide
# variations of this cleaning function, because the processing steps are identical.
clean_local_wdl_dwr_ncro_gen <- function(df_raw, end_date) {
  df_clean <- df_raw |>
    # Parse Date Time column to datetime
    dplyr::mutate(
      datetime = lubridate::mdy_hm(`Date Time`, tz = "Etc/GMT+8")
    ) |>
    dplyr::select(
      survey,
      station_abbr,
      data_freq,
      datetime,
      parameter,
      value = tidyselect::contains(c("Raw Point Data"))
    ) |>
    # Remove NA values so cleaning up duplicates doesn't result in an NA value being selected
    tidyr::drop_na(value) |>
    # Fill in any missing timestamps
    fill_missing_datetime() |>
    dplyr::filter(lubridate::date(datetime) <= end_date) |>
    tidyr::fill(survey, station_abbr, data_freq, parameter)

  return(df_clean)
}

clean_local_wdl_dwr_ncro_wq <- clean_local_wdl_dwr_ncro_gen
clean_local_wdl_dwr_ncro_tide <- clean_local_wdl_dwr_ncro_gen

# Process DWR-WQA continuous (15-min) data from the WQP database for a single target branch
clean_wqp_dwr_wqa <- function(df_raw) {
  df_clean <- df_raw |>
    # Make sure timezone is set to PST
    dplyr::mutate(datetime = lubridate::force_tz(time, tzone = "Etc/GMT+8")) |>
    # Remove all values flagged with a qaqc_flag_id as X = "bad"
    dplyr::filter_out(qaqc_flag_id == "X") |>
    dplyr::select(
      survey,
      station_abbr,
      data_freq,
      datetime,
      parameter,
      value
    ) |>
    # Remove NA values so cleaning up duplicates doesn't result in an NA value being selected
    tidyr::drop_na(value) |>
    # Fill in any missing timestamps
    fill_missing_datetime() |>
    tidyr::fill(survey, station_abbr, data_freq, parameter)

  return(df_clean)
}
