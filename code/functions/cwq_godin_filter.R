# Godin filter functions used in the targets workflow for processing continuous water quality data
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Godin filter:
# It includes three passes in time domain, with windows 24, 24, and 25 hrs long
# will assume 15-min data here, so for first pass, 48 points have to be skipped
# and last 44 window is 12-1-11 hrs in length, i.e. use previous 12 hours and
# subsequent 11 hrs to define result at 13th hour.
godin_filter <- function(value) {
  value_filt <- rep_len(NA, length.out = length(value))

  # do first pass, with window of 12, 1, 11 hrs
  for (i in seq(from = 49, to = (length(value) - 44))) {
    value_filt[i] <- sum(value[(i - 48):(i + 44)]) / 93
  }

  value_filt1 <- value_filt
  # now 2nd pass, same approach but window is 11, 1, 12 hrs
  for (i in seq(from = 45, to = (length(value) - 48))) {
    value_filt1[i] <- sum(value_filt[(i - 44):(i + 48)]) / 93
  }
  value_filt <- value_filt1
  # now 3rd pass with 12-1-12 window
  for (i in seq(from = 49, to = (length(value) - 48))) {
    value_filt[i] <- sum(value_filt1[(i - 48):(i + 48)]) / 97
  }
  # Now throw away 36 hrs at each end
  value_filt[1:(36 * 4)] <- NA
  value_length <- length(value_filt)
  value_filt[(value_length - 36 * 4):value_length] <- NA

  return(value_filt)
}

# Helper function to apply the Godin filter to continuous (15-min) discharge and velocity data
# Adds explicit NA values for any missing timestamps and fills in any gaps of up to 2 hours
# using linear interpolation
apply_godin_filter <- function(df_data, parameter_name, data_freq) {
  # If the dataset is empty, just return it
  if (nrow(df_data) == 0) {
    return(df_data)
  }

  # Only run the filter if this branch represents a raw discharge or velocity parameter
  if (
    !is.na(parameter_name) &&
      parameter_name %in% c("discharge", "velocity") &&
      data_freq == "15-min"
  ) {
    # Provide messaging
    station_abbr <- unique(df_data$station_abbr)
    survey <- unique(df_data$survey)
    cli::cli_alert_info(
      "Applying Godin tidal filter to {.val {parameter_name}} data for station {.val {station_abbr}} (collected by {.val {survey}})"
    )

    # Prepare data for Godin filter
    df_data <- df_data |>
      # Ensure data is ordered chronologically before interpolation and running Godin filter
      dplyr::arrange(datetime) |>
      # Interpolate gaps up to 2 hours
      dplyr::mutate(value = imputeTS::na_interpolation(value, maxgap = 8))

    # Run Godin filter
    df_data$value <- godin_filter(df_data$value)

    # Update the parameter label to show it is now filtered
    df_data$parameter <- paste0(df_data$parameter, "_tf")
  }

  return(df_data)
}
