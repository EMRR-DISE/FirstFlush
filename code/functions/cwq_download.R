# Functions to download continuous water quality data from various sources for the targets workflow
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Main download function that handles the various specific download functions
download_data <- function(
  station_abbr,
  survey,
  data_api,
  data_api_type,
  api_data_id,
  api_station_id,
  parameter_code,
  parameter_name,
  data_freq,
  end_date
) {
  # Provide messaging based on argument inputs
  data_src <- if (!is.na(data_api_type)) {
    paste(data_api, data_api_type, sep = "-")
  } else {
    data_api
  }

  data_src <- if (!is.na(api_data_id)) {
    paste0(data_src, " (", api_data_id, ")")
  } else {
    data_src
  }

  if (!is.na(parameter_name)) {
    cli::cli_alert_info(
      "Downloading {.val {parameter_name}} from source {.val {data_src}} for station {.val {station_abbr}}"
    )
  } else {
    cli::cli_alert_info(
      "Downloading all parameters from source {.val {data_src}} for station {.val {station_abbr}}"
    )
  }

  # Construct the exact function name dynamically based on metadata
  function_name <- if (!is.na(data_api_type)) {
    paste("download", tolower(data_api), tolower(data_api_type), sep = "_")
  } else {
    paste("download", tolower(data_api), sep = "_")
  }

  # Check that function exists in the sourced environment, and provide error if not
  if (!exists(function_name)) {
    cli::cli_abort(c(
      "x" = "The download function {.fun {function_name}} does not exist",
      "!" = "This function must be available in the sourced functions within {.val code/functions}"
    ))
  }

  # Dynamic function call: fetches the function by its string name
  download_fun <- get(function_name)

  # Define all possible arguments for download_fun into a single named list
  all_available_args <- tibble::lst(
    api_station_id,
    parameter_code,
    parameter_name,
    api_data_id,
    end_date
  )

  # Find out exactly which arguments the target function accepts
  expected_arg_names <- names(formals(download_fun))

  # Filter all possible arguments to match only what the function expects
  matched_args <- all_available_args[
    names(all_available_args) %in% expected_arg_names
  ]

  # Dynamically execute the function with the custom-tailored argument list
  out <- do.call(download_fun, matched_args)

  # Return an empty tibble if out contains no rows
  if (nrow(out) == 0) {
    return(tibble::tibble())
  }

  # Add survey, station_abbr, data_freq, and parameter columns
  out$survey <- survey
  out$station_abbr <- station_abbr
  out$data_freq <- data_freq

  if (!is.na(parameter_name)) {
    out$parameter <- parameter_name
  }

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

# Execute an API Call with Exponential Backoff and Jitter Retry Logic. Wraps a downloading or
# data-fetching function inside a retry loop. The function aborts execution immediately if an
# unrecoverable HTTP 404 error is identified, but handles transient network errors by calculating
# an exponential backoff delay paired with a random uniform jitter to prevent server overload.
api_with_retry <- function(api_fn, max_attempts, initial_delay, ...) {
  current_delay <- initial_delay

  for (attempt in 1:max_attempts) {
    # Establish dynamic cli status text
    cli::cli_progress_message(
      "Attempt {attempt}/{max_attempts}: Downloading data..."
    )

    # Evaluate the API expression
    result <- rlang::try_fetch(
      expr = {
        data <- api_fn(...)
        list(status = "success", data = data)
      },
      # Catch and evaluate any error condition object (cnd)
      error = function(cnd) {
        error_msg <- conditionMessage(cnd)

        # Look for explicit HTTP 404 Indicators
        is_404 <- stringr::str_detect(
          error_msg,
          stringr::regex("404|Not Found", ignore_case = TRUE)
        )

        if (isTRUE(is_404)) {
          # Terminate execution immediately for unrecoverable 404 errors
          cli::cli_abort(
            "Server returned {.val 404 Not Found}. Execution halted.",
            parent = cnd
          )
        }

        # Handle all other transient network errors (like partial transfers)
        cli::cli_alert_warning(
          "Attempt {.val {attempt}} failed: {.emph {error_msg}}"
        )
        return(list(status = "transient_error", data = NULL))
      }
    )

    # Evaluate outcomes and handle control flow
    # Return data if successful
    if (result$status == "success") {
      return(result$data)
    }

    # If it is a transient error, execute exponential backoff with jitter
    if (attempt < max_attempts) {
      jitter <- runif(1, 0, 1)
      sleep_time <- current_delay + jitter

      cli::cli_alert_info(
        "Waiting {.val {round(sleep_time, 2)}} seconds before retrying..."
      )
      Sys.sleep(sleep_time)

      current_delay <- current_delay * 2
    } else {
      cli::cli_abort(
        "Download failed permanently after {.val {max_attempts}} attempts."
      )
    }
  }
}

# Download USGS Instantaneous (15-min) data for a single station and parameter
download_usgs_continuous <- function(
  api_station_id,
  parameter_code,
  parameter_name,
  end_date
) {
  # Determine period of record for parameter
  param_meta <- api_with_retry(
    api_fn = dataRetrieval::read_waterdata_ts_meta,
    max_attempts = 5,
    initial_delay = 2,
    monitoring_location_id = api_station_id,
    parameter_code = parameter_code,
    computation_identifier = "Instantaneous"
  )

  # Warn and safely return an empty tibble if the parameter has never been collected
  # at the station
  if (nrow(param_meta) == 0) {
    cli::cli_bullets(c(
      "!" = "There is no record of continuous {.val {parameter_name}} data being collected at {.val {api_station_id}}",
      "i" = "Returning an empty tibble"
    ))
    return(tibble::tibble())
  }

  min_date <- lubridate::date(min(param_meta$begin_utc, na.rm = TRUE))
  max_date <- lubridate::date(max(param_meta$end_utc, na.rm = TRUE))

  # Warn and safely return an empty tibble if min_date is greater than end_date
  if (min_date >= end_date) {
    cli::cli_bullets(c(
      "!" = "{.var end_date} is before the period of record for {.val {parameter_name}} at {.val {api_station_id}}",
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
        api_with_retry(
          api_fn = dataRetrieval::read_waterdata_continuous,
          max_attempts = 5,
          initial_delay = 2,
          monitoring_location_id = api_station_id,
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
  api_station_id,
  parameter_code,
  parameter_name,
  end_date
) {
  # Define statistic_id for daily mean values
  stat_id <- "00003"

  # Determine period of record for parameter
  param_meta <- api_with_retry(
    api_fn = dataRetrieval::read_waterdata_ts_meta,
    max_attempts = 5,
    initial_delay = 2,
    monitoring_location_id = api_station_id,
    parameter_code = parameter_code,
    statistic_id = stat_id,
    computation_period_identifier = "Daily"
  )

  # Warn and safely return an empty tibble if the parameter has never been collected
  # at the station
  if (nrow(param_meta) == 0) {
    cli::cli_bullets(c(
      "!" = "There is no record of daily {.val {parameter_name}} data being collected at {.val {api_station_id}}",
      "i" = "Returning an empty tibble"
    ))
    return(tibble::tibble())
  }

  # Warn and safely return an empty tibble if min_date is greater than end_date
  min_date <- lubridate::date(min(param_meta$begin_utc, na.rm = TRUE))
  if (min_date >= end_date) {
    cli::cli_bullets(c(
      "!" = "{.var end_date} is before the period of record for {.val {parameter_name}} at {.val {api_station_id}}",
      "i" = "The minimum date for the period of record is {.val {min_date}}, while {.var end_date} is set to {.val {end_date}}",
      "i" = "Returning an empty tibble"
    ))
    return(tibble::tibble())
  }

  # Download daily mean values if the above checks passed
  api_with_retry(
    api_fn = dataRetrieval::read_waterdata_daily,
    max_attempts = 5,
    initial_delay = 2,
    monitoring_location_id = api_station_id,
    parameter_code = parameter_code,
    statistic_id = stat_id,
    skipGeometry = TRUE,
    time = paste0("../", end_date)
  )
}

# Download specified data entity from an EDI package
download_edi <- function(api_data_id, api_station_id) {
  # Obtain all data entities for EDI data package
  df_data_ent <- api_with_retry(
    api_fn = EDIutils::read_data_entity_names,
    max_attempts = 5,
    initial_delay = 2,
    packageId = api_data_id
  )

  # Define EDI entityId to download
  entity_id <- df_data_ent$entityId[
    which(df_data_ent$entityName == api_station_id)
  ]

  # Download specified entity as a binary file
  data_raw <- api_with_retry(
    api_fn = EDIutils::read_data_entity,
    max_attempts = 5,
    initial_delay = 2,
    packageId = api_data_id,
    entityId = entity_id
  )

  # Import binary file using read_csv
  readr::read_csv(data_raw, show_col_types = FALSE)
}

# Download continuous WQ data from the CNRA data portal
download_cnra_continuous <- function(
  api_station_id,
  parameter_code,
  parameter_name
) {
  # Execute web API call for the continuous WQ download links table
  # Build SQL Query for continuous WQ download links table based on station_number (api_station_id)
  sql_query_cwq_links <- glue::glue_sql(
    'SELECT * FROM "cdb5dd35-c344-4969-8ab2-d0e2d6c00821" ',
    'WHERE "station_number"::text = {api_station_id}',
    .con = DBI::ANSI()
  )

  # Build the URL request
  req_cwq_links <-
    httr2::request(
      "https://data.cnra.ca.gov/api/3/action/datastore_search_sql"
    ) |>
    httr2::req_url_query(sql = sql_query_cwq_links) |>
    httr2::req_retry(max_tries = 5)

  # Perform the query safely and catch server responses
  resp_cwq_links <- rlang::try_fetch(
    expr = {
      httr2::req_perform(req_cwq_links)
    },
    error = function(cnd) {
      cli::cli_abort(
        "CNRA Datastore API request execution failed",
        parent = cnd
      )
    }
  )

  # Extract and parse the payload structures cleanly
  payload_cwq_links <- httr2::resp_body_json(resp_cwq_links)
  records_cwq_links <- purrr::pluck(payload_cwq_links, "result", "records")

  # Safety check: Handle instances where zero records match the filter criteria
  # Warn and safely return an empty tibble if the parameter has never been collected
  # at the station
  if (length(records_cwq_links) == 0) {
    cli::cli_bullets(c(
      "!" = "There is no record of continuous data being collected at {.val {api_station_id}}",
      "i" = "Returning an empty tibble"
    ))
    return(tibble::tibble())
  }

  # Clean output
  df_cwq_links <- records_cwq_links |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    # Remove any duplicated records for links
    dplyr::select(-c(`_id`, `_full_text`)) |>
    dplyr::distinct() |>
    # Convert start and end time fields to POSIXct
    dplyr::mutate(dplyr::across(c(start_time, end_time), lubridate::ymd_hms))

  # Filter data frame for "RAW" and specified parameter_code
  df_cwq_links_filt <- df_cwq_links |>
    dplyr::filter(output_interval == "RAW", parameter == parameter_code)

  # Warn and safely return an empty tibble if the parameter has never been collected
  # at the station
  if (nrow(df_cwq_links_filt) == 0) {
    cli::cli_bullets(c(
      "!" = "There is no record of continuous {.val {parameter_name}} data being collected at {.val {api_station_id}}",
      "i" = "Returning an empty tibble"
    ))
    return(tibble::tibble())
  }

  # Download continuous values if the above checks passed
  # Build the URL request
  req_cwq_data <-
    httr2::request(df_cwq_links_filt$download_link) |>
    httr2::req_retry(max_tries = 5)

  # Perform the query safely and catch server responses
  resp_cwq_data <- rlang::try_fetch(
    expr = {
      httr2::req_perform(req_cwq_data)
    },
    error = function(cnd) {
      cli::cli_abort(
        "CNRA Datastore API request execution failed",
        parent = cnd
      )
    }
  )

  # Safety check: Handle instances where resp_cwq_data contains zero records
  if (length(resp_cwq_data$body) == 0) {
    cli::cli_bullets(c(
      "!" = "No matching records found for {.val {parameter_name}} at {.val {api_station_id}}",
      "i" = "Returning an empty tibble"
    ))
    return(tibble::tibble())
  }

  # Extract and parse the payload structures cleanly
  raw_cwq_data <- httr2::resp_body_string(resp_cwq_data)

  # Parse the text string directly into a clean data frame/tibble
  readr::read_csv(file = I(raw_cwq_data), skip = 2, show_col_types = FALSE)
}
