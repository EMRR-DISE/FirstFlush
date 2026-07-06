# Functions to download continuous WQ data from various sources

# Main download function that handles the various specific download functions
download_data <- function(
  station_id,
  source,
  data_type,
  source_id,
  parameter_code,
  parameter_name,
  end_date
) {
  cli::cli_alert_info(
    "Downloading {.val {parameter_name} ({data_type})} from source {.val {source}} for station {.val {station_id}}"
  )

  # Construct the exact function name dynamically based on metadata
  function_name <- paste0("download_", tolower(source), "_", tolower(data_type))

  # Check if that function exists in the sourced environment, and provide error if not
  if (!exists(function_name)) {
    cli::cli_abort(c(
      "x" = "The download function {.fun {function_name}} does not exist",
      "!" = "This function must be available in the sourced functions within {.val code/functions}"
    ))
  }

  # Dynamic function call: fetches the function by its string name and executes it
  download_fun <- get(function_name)
  out <- download_fun(source_id, parameter_code, parameter_name, end_date)

  # Return an empty tibble of out contains no rows
  if (nrow(out) == 0) {
    return(tibble::tibble())
  }

  # Add station_id, data_type, and parameter columns
  out$station_id <- station_id
  out$data_type <- data_type
  out$parameter <- parameter_name

  return(out)
}

# Helper function to translate parameter name to USGS parameter code
# get_usgs_param_code <- function(parameter) {
#   # Create main list of parameter names and their codes
#   params <- c(
#     "water_temp" = "00010",
#     "sp_cond" = "00095",
#     "turbidity" = "63680",
#     "discharge" = "00060",
#     "discharge_tf" = "72137"
#   )

#   unname(params[parameter])
# }

# Download USGS Instantaneous (15-min) data for a single station and parameter
download_usgs_continuous <- function(
  source_id,
  parameter_code,
  parameter_name,
  end_date
) {
  # Determine period of record for parameter
  param_meta <- dataRetrieval::read_waterdata_ts_meta(
    monitoring_location_id = source_id,
    parameter_code = parameter_code,
    computation_identifier = "Instantaneous"
  )

  # Warn and safely return an empty tibble if the parameter has never been collected
  # at the station
  if (nrow(param_meta) == 0) {
    cli::cli_bullets(c(
      "!" = "There is no record of continuous {.val {parameter_name}} data being collected at {.val {source_id}}",
      "i" = "Returning an empty tibble"
    ))
    return(tibble::tibble())
  }

  min_date <- lubridate::date(min(param_meta$begin_utc, na.rm = TRUE))
  max_date <- lubridate::date(max(param_meta$end_utc, na.rm = TRUE))

  # Warn and safely return an empty tibble if min_date is greater than end_date
  if (min_date >= end_date) {
    cli::cli_bullets(c(
      "!" = "{.var end_date} is before the period of record for {.val {parameter_name}} at {.val {source_id}}",
      "i" = "The minimum date for the period of record is {.val {min_date}}, while {.var end_date} is set to {.val {end_date}}",
      "i" = "Returning an empty tibble"
    ))
    return(tibble::tibble())
  }

  # Resolve max_date to use end_date as the absolute maximum
  max_date <- if (max_date > end_date) end_date else max_date

  # Add two days to max date and subtract two days from min date to force the API to download
  # data through max date and account for timezone being in UTC
  min_date <- min_date - 2
  max_date <- max_date + 2

  # NOTE: The current service that delivers this data only allows up to 3 years of continuous
  # data to be requested at once. This function will download data for single calendar year
  # and combine them at the end.
  # Define calendar year dates in a tibble to then be used in the purrr loop
  year_range <- lubridate::year(c(min_date, max_date))
  date_ranges <- lubridate::make_date(
    seq(from = year_range[1], to = year_range[2])
  )
  df_times <- tibble::tibble(
    start = c(min_date, date_ranges[-1]),
    end = c(date_ranges[-1], max_date)
  )

  # Download data one calendar year at a time
  ls_data <- df_times |>
    purrr::pmap(
      \(start, end) {
        dataRetrieval::read_waterdata_continuous(
          monitoring_location_id = source_id,
          parameter_code = parameter_code,
          time = c(start, end)
        )
      }
    )

  # Combine and clean data
  purrr::list_rbind(ls_data) |>
    # Remove overlapping data
    dplyr::distinct() |>
    # Convert to PST time zone and filter to end_date
    dplyr::mutate(time = lubridate::with_tz(time, tzone = "Etc/GMT+8")) |>
    dplyr::filter(lubridate::date(time) <= end_date)
}

# Download USGS daily mean data for a single station and parameter
download_usgs_daily <- function(
  source_id,
  parameter_code,
  parameter_name,
  end_date
) {
  # Define statistic_id for daily mean values
  stat_id <- "00003"

  # Determine period of record for parameter
  param_meta <- dataRetrieval::read_waterdata_ts_meta(
    monitoring_location_id = source_id,
    parameter_code = parameter_code,
    statistic_id = stat_id,
    computation_period_identifier = "Daily"
  )

  # Warn and safely return an empty tibble if the parameter has never been collected
  # at the station
  if (nrow(param_meta) == 0) {
    cli::cli_bullets(c(
      "!" = "There is no record of daily {.val {parameter_name}} data being collected at {.val {source_id}}",
      "i" = "Returning an empty tibble"
    ))
    return(tibble::tibble())
  }

  # Warn and safely return an empty tibble if min_date is greater than end_date
  min_date <- lubridate::date(min(param_meta$begin_utc, na.rm = TRUE))
  if (min_date >= end_date) {
    cli::cli_bullets(c(
      "!" = "{.var end_date} is before the period of record for {.val {parameter_name}} at {.val {source_id}}",
      "i" = "The minimum date for the period of record is {.val {min_date}}, while {.var end_date} is set to {.val {end_date}}",
      "i" = "Returning an empty tibble"
    ))
    return(tibble::tibble())
  }

  # Download daily mean values if the above checks passed
  dataRetrieval::read_waterdata_daily(
    monitoring_location_id = source_id,
    parameter_code = parameter_code,
    statistic_id = stat_id,
    skipGeometry = TRUE,
    time = paste0("../", end_date)
  )
}
