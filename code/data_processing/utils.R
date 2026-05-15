# Global functions for retrieving and processing data
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Package checking
if (!requireNamespace("rlang", quietly = TRUE)) {
  stop(
    "'rlang' is required for the sourced functions",
    "\nTo install it, run: install.packages('rlang')",
    call. = FALSE
  )
}

rlang::check_installed(
  c(
    "dplyr",
    "EDIutils",
    "glue",
    "purrr",
    "jsonlite",
    "tidyr",
    "stringr",
    "tidyselect"
  ),
  reason = "for the sourced functions"
)

# Get data entity names for specified EDI ID
get_edi_data_entities <- function(edi_id) {
  df_data_ent <- EDIutils::read_data_entity_names(edi_id)
  rlang::inform(c(
    "i" = paste0(
      "Data entities for ",
      edi_id,
      " include:\n",
      paste(df_data_ent$entityName, collapse = "\n"),
      "\n"
    )
  ))
  return(df_data_ent$entityName)
}

# Download specified data entities from an EDI package and save raw bytes files to a temporary
# directory
get_edi_data <- function(edi_id, entity_names) {
  df_data_ent <- EDIutils::read_data_entity_names(edi_id)
  df_data_ent_filt <- df_data_ent %>%
    dplyr::filter(entityName %in% entity_names)

  ls_data_raw <-
    purrr::map(df_data_ent_filt$entityId, \(x) {
      EDIutils::read_data_entity(edi_id, entityId = x)
    }) %>%
    rlang::set_names(df_data_ent_filt$entityName)

  temp_dir <- tempdir()
  for (i in 1:length(ls_data_raw)) {
    file_raw <- file.path(temp_dir, glue::glue("{names(ls_data_raw)[i]}.bin"))
    con <- file(file_raw, "wb")
    writeBin(ls_data_raw[[i]], con)
    close(con)
  }
}

# Download and import continuous WQ data from the CNRA data portal
get_cnra_cwq_data <- function(station_num, parameters = NULL) {
  # Generate HTTP request URL for continuous WQ download links table for station_num
  links_url <- paste0(
    "https://data.cnra.ca.gov/api/3/action/datastore_search?resource_id=cdb5dd35-c344-4969-8ab2-d0e2d6c00821&q=",
    station_num
  )

  # Call on the API, transform JSON data into data frame
  df_station_links <- links_url %>%
    jsonlite::read_json() %>%
    purrr::pluck("result", "records") %>%
    tidyr::tibble() %>%
    tidyr::unnest_wider(1) |>
    # Remove any duplicated records for links
    dplyr::select(-c(`_id`, rank)) |>
    dplyr::distinct()

  # Filter data frame for "RAW" and
  if (is.null(parameters)) {
    df_station_links_filt <- df_station_links %>%
      dplyr::filter(
        station_number == station_num,
        output_interval == "RAW"
      )
  } else {
    df_station_links_filt <- df_station_links %>%
      dplyr::filter(
        station_number == station_num,
        output_interval == "RAW",
        parameter %in% parameters
      )
  }

  # Read in CSV links from data frame, format and join resultant data frames
  df_station_links_filt %>%
    dplyr::mutate(
      df_data = purrr::map(download_link, \(x) {
        readr::read_csv(x, skip = 2, show_col_types = FALSE)
      })
    ) %>%
    dplyr::select(station_number, parameter, df_data) |>
    tidyr::unnest(df_data)
}

# Function for combining multiple columns into one using regex patterns for column selection
comb_cols <- function(df, col_names_regex) {
  col_names <- stringr::str_subset(names(df), col_names_regex)
  ls_col_names <- purrr::map(col_names, \(x) dplyr::pull(df, x))

  df |>
    dplyr::mutate(turbidity = dplyr::coalesce(!!!ls_col_names)) |>
    dplyr::select(!tidyselect::all_of(col_names))
}
