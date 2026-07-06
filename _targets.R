library(targets)
library(tarchetypes)

tar_source("code/functions")

tar_option_set(
  # Packages required by custom functions
  # packages = c("yaml", "dplyr", "dataRetrieval"),
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
    cwq_station_metadata,
    "data/cwq_stations.yml",
    read_cwq_stations_meta(!!.x)
  ),
  # Download data dynamically per station
  tar_target(
    raw_data,
    download_data(
      station_id = cwq_station_metadata$station_id,
      source = cwq_station_metadata$data_source,
      data_type = cwq_station_metadata$data_type,
      source_id = cwq_station_metadata$source_id,
      parameter_code = cwq_station_metadata$param_code,
      parameter_name = cwq_station_metadata$param_name,
      end_date = global_end_date
    ),
    pattern = map(cwq_station_metadata),
    iteration = "list"
  ) #,
  # # Process data dynamically per branch
  # tar_target(
  #   processed_data,
  #   process_data(raw_data),
  #   pattern = map(raw_data)
  # ),
  # # Combine everything back into a single dataset
  # tar_target(
  #   combined_dataset,
  #   bind_rows(processed_data)
  # ),
  # # Export the final product to a plain file everyone can read
  # tar_target(
  #   exported_csv,
  #   {
  #     output_path <- "data/combined_stations_data.csv"
  #     readr::write_csv(combined_dataset, output_path)
  #     return(output_path) # Returns the path so targets can track the file
  #   },
  #   format = "file"
  # )
)

# Run tar_make() to run the pipeline and tar_read(data_summary) to view the results
