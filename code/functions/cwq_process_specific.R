# Functions to process continuous water quality data from various sources for the targets workflow
# These are specific functions unique to station-parameter-data_freq used to standardize data
# formats before calculating daily averages
# These functions will override the general cleaning functions in cwq_process.R
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# General cleaning function for continuous (15-min) USGS stations with overlapping sub-locations
# It resolves duplicated timestamps for stations where the periods of record of sub-locations
# overlap. It resolves duplicates by using the values from sub-locations identified by their
# time_series_id in the order provided in ts_ids. If the value from the first time_series_id is not
# missing, its value is used. Otherwise if it is missing, it uses the value from the second
# time_series_id, and so on.
clean_usgs_15min_overlaps <- function(df_raw, ts_ids) {
  # Prepare the data to apply coalesce rules to
  df_prep <- df_raw |>
    dplyr::mutate(
      survey,
      station_abbr,
      data_freq,
      # Make sure all timestamps fall on an exact 15-minute interval
      datetime = lubridate::round_date(time, unit = "15 minute"),
      parameter,
      value,
      time_series_id = stringr::str_sub(time_series_id, end = 10),
      .keep = "none"
    ) |>
    # Remove any overlapping timestamps for same time_series_id to ensure pivot wider doesn't
    # result in list columns. Remove NA values so cleaning up duplicates doesn't result in an
    # NA value being selected
    tidyr::drop_na(value) |>
    dplyr::distinct(datetime, time_series_id, .keep_all = TRUE) |>
    # Pivot data wider to make time_series_ids as column names
    tidyr::pivot_wider(names_from = time_series_id, values_from = value)

  # Pull out data from time_series_id columns into a list to then be coalesced
  ls_values <- purrr::map(ts_ids, \(x) dplyr::pull(df_prep, x))

  # Combine values into one column using the order defined in ts_ids
  df_prep |>
    dplyr::mutate(value = dplyr::coalesce(!!!ls_values)) |>
    dplyr::select(!tidyselect::all_of(ts_ids))
}

# Custom cleaning for FPT continuous (15-min) water temperature data
clean_fpt_water_temp_15min <- function(df_raw) {
  # Process raw data - clean up overlapping sub-locations for water temperature data
  # Prefer data collected from the Right Bank Pump Stand - these data were collected for the
  # entire period of record - and use data collected by the BGC Project as a fallback if RBPS
  # is missing
  df_raw |>
    clean_usgs_15min_overlaps(c("b63de35d05", "113047af1c")) |>
    # Fill in any missing timestamps
    fill_missing_datetime() |>
    tidyr::fill(survey, station_abbr, data_freq, parameter)
}

# Custom cleaning for FPT continuous (15-min) turbidity data
clean_fpt_turbidity_15min <- function(df_raw) {
  # Process raw data - clean up overlapping sub-locations for turbidity data
  # Prefer data collected from the BGC Project during the overlapping period - this project
  # collected data for a longer period - and use data collected by MEDIAN TS087 as a fallback
  # if BGC is missing
  df_raw |>
    clean_usgs_15min_overlaps(c("b6cc344d4c", "a97927d6dc")) |>
    # Fill in any missing timestamps
    fill_missing_datetime() |>
    tidyr::fill(survey, station_abbr, data_freq, parameter)
}

# Custom cleaning for C62 continuous (15-min) water temperature data
clean_c62_water_temp_15min <- function(df_raw) {
  # Process raw data - clean up overlapping sub-locations for water temperature data
  # Prefer data collected from the BGC Project - these data were collected for the entire period
  # of record - and use data collected by the DWS-BOR Project as a fallback if BGC is missing
  df_raw |>
    clean_usgs_15min_overlaps(c("2dd66cf018", "9a6beeb3ef")) |>
    # Fill in any missing timestamps
    fill_missing_datetime() |>
    tidyr::fill(survey, station_abbr, data_freq, parameter)
}

# Custom cleaning for C62 continuous (15-min) specific conductance data
clean_c62_sp_cond_15min <- function(df_raw) {
  # Process raw data - clean up overlapping sub-locations for specific conductance data
  # Prefer data collected from the BGC Project - these data were collected for the entire period
  # of record - and use data collected by the DWS-BOR Project as a fallback if BGC is missing
  df_raw |>
    clean_usgs_15min_overlaps(c("e9554fbfe8", "d54126ae16")) |>
    # Fill in any missing timestamps
    fill_missing_datetime() |>
    tidyr::fill(survey, station_abbr, data_freq, parameter)
}

# Custom cleaning for C62 continuous (15-min) turbidity data
clean_c62_turbidity_15min <- function(df_raw) {
  # Process raw data - clean up overlapping sub-locations for turbidity data
  # Prefer data collected from the BGC Project - these data were collected for the entire period
  # of record - and use data collected by the DWS-BOR Project as a fallback if BGC is missing
  df_raw |>
    clean_usgs_15min_overlaps(c("8e96bc7fc3", "9ffcf9e275")) |>
    # Fill in any missing timestamps
    fill_missing_datetime() |>
    tidyr::fill(survey, station_abbr, data_freq, parameter)
}

# Custom cleaning for RYI continuous (15-min) water temperature data
clean_ryi_water_temp_15min <- function(df_raw) {
  # Process raw data - clean up overlapping sub-locations for water temperature data
  # Prefer data collected from the BGC Project and use data collected by the other project as a
  # fallback if BGC is missing
  df_raw |>
    clean_usgs_15min_overlaps(c("ffac1ba809", "cf4bda3807")) |>
    # Fill in any missing timestamps
    fill_missing_datetime() |>
    tidyr::fill(survey, station_abbr, data_freq, parameter)
}

# Custom cleaning for RYI continuous (15-min) turbidity data
clean_ryi_turbidity_15min <- function(df_raw) {
  # Process raw data - clean up overlapping sub-locations for turbidity data
  # Prefer data collected from the BGC Project and use data collected by MEDIAN TS087 as a
  # fallback if BGC is missing
  df_raw |>
    clean_usgs_15min_overlaps(c("87ba4cf896", "a76d8997c8")) |>
    # Fill in any missing timestamps
    fill_missing_datetime() |>
    tidyr::fill(survey, station_abbr, data_freq, parameter)
}

# Custom cleaning for LIB continuous (15-min) water temperature data
clean_lib_water_temp_15min <- function(df_raw) {
  # Process raw data - clean up overlapping sub-locations for water temperature data
  # Prefer data collected from the BGC Project and use data collected by Chlor Intercal Project
  # as a fallback if BGC is missing
  df_raw |>
    clean_usgs_15min_overlaps(c("9a54a37837", "45a0b6576d")) |>
    # Fill in any missing timestamps
    fill_missing_datetime() |>
    tidyr::fill(survey, station_abbr, data_freq, parameter)
}

# Custom cleaning for LIB continuous (15-min) specific conductance data
clean_lib_sp_cond_15min <- function(df_raw) {
  # Process raw data - clean up overlapping sub-locations for specific conductance data
  # Prefer data collected from the BGC Project and use data collected by Chlor Intercal Project
  # as a fallback if BGC is missing
  df_raw |>
    clean_usgs_15min_overlaps(c("b869d99ace", "bf89044614")) |>
    # Fill in any missing timestamps
    fill_missing_datetime() |>
    tidyr::fill(survey, station_abbr, data_freq, parameter)
}

# Custom cleaning for LIB continuous (15-min) turbidity data
clean_lib_turbidity_15min <- function(df_raw) {
  # Process raw data - clean up overlapping sub-locations for turbidity data
  # Prefer data collected from the BGC Project and use data collected by Chlor Intercal Project
  # as a fallback if BGC is missing
  df_raw |>
    clean_usgs_15min_overlaps(c("e779ffaffb", "654b8ed517", "5aea059b94")) |>
    # Fill in any missing timestamps
    fill_missing_datetime() |>
    tidyr::fill(survey, station_abbr, data_freq, parameter)
}

# Custom cleaning for SJJ continuous (15-min) water temperature data
clean_sjj_water_temp_15min <- function(df_raw) {
  # Process raw data - clean up overlapping sub-locations for water temperature data
  # Prefer data collected from the BGC Project and use data collected by an unknown Project
  # as a fallback if BGC is missing
  df_raw |>
    clean_usgs_15min_overlaps(c("6495b35527", "e909f63153")) |>
    # Fill in any missing timestamps
    fill_missing_datetime() |>
    tidyr::fill(survey, station_abbr, data_freq, parameter)
}

# Custom cleaning for SJJ continuous (15-min) specific conductance data
clean_sjj_sp_cond_15min <- function(df_raw) {
  # Process raw data - clean up overlapping sub-locations for specific conductance data
  # Prefer data collected from the BGC Project and use data collected by an unknown Project
  # as a fallback if BGC is missing
  df_raw |>
    clean_usgs_15min_overlaps(c("33f1a0871c", "4959cb047f")) |>
    # Fill in any missing timestamps
    fill_missing_datetime() |>
    tidyr::fill(survey, station_abbr, data_freq, parameter)
}

# Custom cleaning for SJJ continuous (15-min) turbidity data
clean_sjj_turbidity_15min <- function(df_raw) {
  # Process raw data - clean up overlapping sub-locations for turbidity data
  # Prefer data collected from the BGC Project and use data collected by an unknown Project
  # as a fallback if BGC is missing. There is no overlap between the first and second project.
  df_raw |>
    clean_usgs_15min_overlaps(c("b8a9939110", "5dc1f7d05d", "b06e7321ea")) |>
    # Fill in any missing timestamps
    fill_missing_datetime() |>
    tidyr::fill(survey, station_abbr, data_freq, parameter)
}

# Custom cleaning for FMB continuous (15-min) water temperature data
clean_fmb_water_temp_15min <- function(df_raw) {
  # Process raw data - clean up overlapping sub-locations for water temperature data
  # Prefer data collected from the BGC Project and use data collected by an unknown Project
  # as a fallback if BGC is missing
  df_raw |>
    clean_usgs_15min_overlaps(c("3ae2b2ecd0", "793ea63723")) |>
    # Fill in any missing timestamps
    fill_missing_datetime() |>
    tidyr::fill(survey, station_abbr, data_freq, parameter)
}

# Custom cleaning for FMB continuous (15-min) specific conductance data
clean_fmb_sp_cond_15min <- function(df_raw) {
  # Process raw data - clean up overlapping sub-locations for specific conductance data
  # Prefer data collected from the BGC Project and use data collected by an unknown Project
  # as a fallback if BGC is missing
  df_raw |>
    clean_usgs_15min_overlaps(c("f88a41451b", "a2ab2909f0")) |>
    # Fill in any missing timestamps
    fill_missing_datetime() |>
    tidyr::fill(survey, station_abbr, data_freq, parameter)
}

# Custom cleaning for FMB continuous (15-min) turbidity data
clean_fmb_turbidity_15min <- function(df_raw) {
  # Process raw data - clean up overlapping sub-locations for specific conductance data
  # Prefer data collected from the BGC Project and use data collected by an unknown Project
  # as a fallback if BGC is missing
  df_raw |>
    clean_usgs_15min_overlaps(c("c7b4dfee45", "dd236ffcc8")) |>
    # Fill in any missing timestamps
    fill_missing_datetime() |>
    tidyr::fill(survey, station_abbr, data_freq, parameter)
}

# Custom cleaning for SBR continuous (15-min) water temperature data
clean_sbr_water_temp_15min <- function(df_raw) {
  # Process raw data - clean up overlapping sub-locations for water temperature data
  # Only keep the values for the upper (surface) sampling location
  df_raw |>
    dplyr::filter(stringr::str_sub(time_series_id, end = 10) == "b64f950158") |>
    # Finish cleaning using the general cleaning function
    clean_usgs_continuous()
}

# Custom cleaning for SBR continuous (15-min) specific conductance data
clean_sbr_sp_cond_15min <- function(df_raw) {
  # Process raw data - clean up overlapping sub-locations for specific conductance data
  # Only keep the values for the upper (surface) sampling location
  df_raw |>
    dplyr::filter(stringr::str_sub(time_series_id, end = 10) == "8dc1d071f4") |>
    # Finish cleaning using the general cleaning function
    clean_usgs_continuous()
}

# Custom cleaning for SBR continuous (15-min) turbidity data
clean_sbr_turbidity_15min <- function(df_raw) {
  # Process raw data - clean up overlapping sub-locations for turbidity data
  # Only keep the values for the upper (surface) sampling location
  df_raw |>
    dplyr::filter(
      stringr::str_sub(time_series_id, end = 10) %in%
        c("13a1f928d4", "371fc6d842")
    ) |>
    # Finish cleaning using the general cleaning function
    clean_usgs_continuous()
}

# Custom cleaning for CRQ continuous (15-min) water temperature data
clean_crq_water_temp_15min <- function(df_raw) {
  # Process raw data - clean up overlapping sub-locations for water temperature data
  # Only keep the values for the upper (surface) sampling location
  df_raw |>
    dplyr::filter(stringr::str_sub(time_series_id, end = 10) == "da6a82cc6f") |>
    # Finish cleaning using the general cleaning function
    clean_usgs_continuous()
}

# Custom cleaning for CRQ continuous (15-min) specific conductance data
clean_crq_sp_cond_15min <- function(df_raw) {
  # Process raw data - clean up overlapping sub-locations for specific conductance data
  # Only keep the values for the upper (surface) sampling location
  df_raw |>
    dplyr::filter(stringr::str_sub(time_series_id, end = 10) == "0e847b95f2") |>
    # Finish cleaning using the general cleaning function
    clean_usgs_continuous()
}

# Custom cleaning for CRQ continuous (15-min) turbidity data
clean_crq_turbidity_15min <- function(df_raw) {
  # Process raw data - clean up overlapping sub-locations for turbidity data
  # Only keep the values for the upper (surface) sampling location
  df_raw |>
    dplyr::filter(stringr::str_sub(time_series_id, end = 10) == "56c2f6569d") |>
    # Finish cleaning using the general cleaning function
    clean_usgs_continuous()
}
