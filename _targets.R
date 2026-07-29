library(targets)
library(tarchetypes)

tar_source("code/functions")

tar_option_set(
  # Default storage format for data frames (highly efficient)
  format = "qs",
  # Let the pipeline keep moving if a single station produces an error
  error = "continue",
  # Clear out memory after a target runs to keep things efficient
  garbage_collection = TRUE
)

# Declared list of target objects
tar_plan(
  # Define the global end date constant
  global_end_date = lubridate::as_date("2025-09-30"),
  # Track the YAML configuration file that defines all CWQ stations, read it in and convert
  # it into a flat tibble for branching
  tar_file_read(
    cwq_station_metadata_raw,
    "data/cwq_stations.yml",
    read_cwq_stations_meta(!!.x)
  ),
  # Group metadata by station, parameter, and data frequency (creates one branch per row
  # in metadata)
  tar_group_by(
    cwq_station_metadata,
    cwq_station_metadata_raw,
    station_abbr,
    survey,
    parameter_name,
    data_freq
  ),
  # Download data dynamically per station
  tar_target(
    raw_data,
    download_data(
      station_abbr = cwq_station_metadata$station_abbr,
      survey = cwq_station_metadata$survey,
      data_api = cwq_station_metadata$data_api,
      data_api_type = cwq_station_metadata$data_api_type,
      api_data_id = cwq_station_metadata$api_data_id,
      api_station_id = cwq_station_metadata$api_station_id,
      parameter_code = cwq_station_metadata$parameter_code,
      parameter_name = cwq_station_metadata$parameter_name,
      data_freq = cwq_station_metadata$data_freq,
      end_date = global_end_date
    ),
    pattern = map(cwq_station_metadata),
    iteration = "list"
  ),
  # Process data dynamically per branch
  tar_target(
    processed_data,
    process_data(
      df_raw = raw_data,
      data_api = cwq_station_metadata$data_api,
      data_api_type = cwq_station_metadata$data_api_type,
      survey = cwq_station_metadata$survey,
      station_abbr = cwq_station_metadata$station_abbr,
      parameter_name = cwq_station_metadata$parameter_name,
      data_freq = cwq_station_metadata$data_freq,
      end_date = global_end_date
    ),
    pattern = map(raw_data, cwq_station_metadata),
    iteration = "list"
  ),
  # Apply Godin filter to continuous (15-min) discharge or velocity data specifically
  tar_target(
    tidally_filt_data,
    apply_godin_filter(
      df_data = processed_data,
      parameter_name = cwq_station_metadata$parameter_name,
      data_freq = cwq_station_metadata$data_freq
    ),
    pattern = map(processed_data, cwq_station_metadata),
    iteration = "list"
  ),
  # Calculate daily averages
  tar_target(
    daily_avg_data,
    aggregate_to_daily(
      df_data = tidally_filt_data,
      data_freq = cwq_station_metadata$data_freq
    ),
    pattern = map(tidally_filt_data, cwq_station_metadata),
    iteration = "list"
  ),
  # Combine everything back into a single dataset
  combined_data = dplyr::bind_rows(daily_avg_data),
  # Resolve and merge any overlapping data
  merged_data = resolve_station_merges(combined_data),
  # Finish cleaning merged data by pivoting wider and final polishing
  final_cwq_data = finish_cwq_data(merged_data),
  # Generate data source metadata file from cwq_station_metadata and processed_data
  tar_target(
    data_src_metadata,
    generate_data_src_metadata(
      station_metadata = cwq_station_metadata,
      processed_data = processed_data
    ),
    pattern = map(cwq_station_metadata, processed_data)
  ),
  # Generate station metadata file from cwq_station_metadata_raw, combined data, and spatial data
  station_metadata = generate_station_metadata(
    cwq_station_metadata_raw,
    combined_data
  ),
  # Generate period of record metadata file from final_cwq_data
  por_metadata = generate_por_metadata(final_cwq_data),
  # Export the final cleaned dataset of daily average water quality values to a qdata file that
  # everyone can read (qdata is a highly compressed format provided by the qs2 package)
  tar_file(
    export_final_cwq_data,
    {
      output_path <- "data/processed/wq/cwq_data_dv_all.qdata"
      qs2::qd_save(final_cwq_data, output_path)
      output_path
    }
  ),
  # Export the data source metadata file to a rds file
  tar_file(
    export_data_src_metadata,
    {
      output_path <- "data/processed/wq/cwq_data_source_metadata.rds"
      data_src_metadata |>
        dplyr::reframe(
          parameters = paste0(parameters, collapse = ", "),
          .by = c(survey, station_abbr, station_name, data_source)
        ) |>
        saveRDS(output_path)
      output_path
    }
  ),
  # Export the station metadata file to a rds file
  tar_file(
    export_station_metadata,
    {
      output_path <- "data/processed/wq/cwq_station_metadata.rds"
      saveRDS(station_metadata, output_path)
      output_path
    }
  ),
  # Export the period of record metadata file to a rds file
  tar_file(
    export_por_metadata,
    {
      output_path <- "data/processed/wq/cwq_por_metadata.rds"
      saveRDS(por_metadata, output_path)
      output_path
    }
  )
)

# Run tar_make() to run the pipeline and tar_read(obj) to view the results
