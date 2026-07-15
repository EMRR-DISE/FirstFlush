# Functions used in the targets workflow for specific merging of continuous water quality data
# from multiple sources
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Main merging function that handles the various specific merging functions
resolve_station_merges <- function(df_data) {
  # If the dataset is empty, just return it
  if (nrow(df_data) == 0) {
    return(df_data)
  }

  # Merge water_temp values for FPT
  if (exists("merge_water_temp_fpt")) {
    df_data <- merge_water_temp_fpt(df_data)
  }

  # Merge water_temp values for SJR
  if (exists("merge_water_temp_sjr")) {
    df_data <- merge_water_temp_sjr(df_data)
  }

  return(df_data)
}

# Merge daily and 15-min water temperature data collected at FPT
merge_water_temp_fpt <- function(df_data) {
  df_target <- df_data |>
    dplyr::filter(station_abbr == "FPT", parameter == "water_temp")

  df_remaining <- df_data |> dplyr::anti_join(df_target, by = names(df_data))

  # Prefer 15-min data during the overlapping period
  df_composite <- df_target |>
    dplyr::arrange(date, factor(data_freq, levels = c("15-min", "daily"))) |>
    dplyr::slice(1, .by = date) |>
    dplyr::mutate(data_freq = "merged")

  dplyr::bind_rows(df_composite, df_remaining)
}

# Merge daily and 15-min water temperature data collected at SJR (by USGS and DWR-CEMP)
merge_water_temp_sjr <- function(df_data) {
  df_target <- df_data |>
    dplyr::filter(station_abbr == "SJR", parameter == "water_temp")

  df_remaining <- df_data |> dplyr::anti_join(df_target, by = names(df_data))

  # Use daily USGS values from 1972-1998, then use DWR-CEMP values from 2005-2025
  df_composite <- df_target |>
    dplyr::filter_out(survey == "USGS", data_freq == "15-min") |>
    dplyr::arrange(date, factor(survey, levels = c("DWR-CEMP", "USGS"))) |>
    dplyr::slice(1, .by = date) |>
    dplyr::mutate(data_freq = "merged")

  dplyr::bind_rows(df_composite, df_remaining)
}
