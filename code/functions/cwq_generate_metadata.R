# Functions to generate metadata for the targets workflow
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Generate data source metadata file from cwq_station_metadata and processed_data for each
# single target branch
generate_data_src_metadata <- function(station_metadata, processed_data) {
  # Determine parameter(s) and data_freq from processed data
  df_parameter <- processed_data |>
    dplyr::mutate(parameter = paste0(parameter, " (", data_freq, ")")) |>
    dplyr::distinct(parameter)

  # Consolidate parameters in df_parameter to a single string
  parameters <- paste0(df_parameter$parameter, collapse = ", ")

  # Add parameters to the data source metadata from cwq_station_metadata
  station_metadata |>
    # Consolidate data_api, data_api_type, and api_data_id from cwq_station_metadata into a
    # single string
    dplyr::mutate(
      data_source = dplyr::if_else(
        !is.na(data_api_type),
        paste(data_api, data_api_type, sep = "-"),
        data_api
      ),
      data_source = dplyr::if_else(
        !is.na(api_data_id),
        paste0(data_source, " (", api_data_id, ")"),
        data_source
      ),
      parameters = parameters
    ) |>
    dplyr::select(
      survey,
      station_abbr,
      station_name,
      data_source,
      parameters
    ) |>
    dplyr::arrange(station_abbr, survey)
}

# Generate station metadata file from cwq_station_metadata and stations shapefile
generate_station_metadata <- function(station_metadata) {
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

  # Join station_metadata to df_stations only keeping stations used in targets workflow
  station_metadata |>
    dplyr::distinct(survey, station_abbr, station_name, api_station_id) |>
    dplyr::left_join(
      df_stations,
      by = dplyr::join_by(survey, api_station_id == station_id)
    ) |>
    tidyr::replace_na(list(stratum = "Out of Bounds")) |>
    dplyr::select(
      survey,
      station_abbr,
      station_name,
      stratum,
      latitude,
      longitude
    ) |>
    dplyr::arrange(station_abbr, survey)
}

# Generate period of record metadata for each survey, station, and parameter
generate_por_metadata <- function(final_data) {
  final_data |>
    tidyr::pivot_longer(
      cols = dplyr::where(is.numeric),
      names_to = "parameter",
      values_to = "value",
      values_drop_na = TRUE
    ) |>
    dplyr::summarize(
      min_date = min(date),
      max_date = max(date),
      .by = c(station_abbr, parameter)
    )
}
