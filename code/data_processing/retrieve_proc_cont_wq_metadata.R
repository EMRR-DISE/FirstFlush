# Retrieve, compile, and filter metadata for continuous water quality data collected in the SFE -
# legal Delta and Suisun Marsh and Bay
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(dataRetrieval)
library(sf)
# Make sure we are using `deltamapr` version 1.0.1, commit fe34697b3d1aaa2945bbfc647582a19e251abf67
# install.packages("devtools")
# devtools::install_github(
# "InteragencyEcologicalProgram/deltamapr", ref = "fe34697b3d1aaa2945bbfc647582a19e251abf67"
# )
library(deltamapr)
library(EDIutils)
library(rlang)
library(glue)
library(wqpr)
library(here)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Source data download functions
source(here("code/functions/cwq_download.R"))

# Define Spatial Extent -----------------------------------------------------------------------

# Define the spatial extent of the continuous water quality stations of interest as the legal
# Delta, Suisun Marsh and Bay, and San Pablo Bay. Use the R_Delta shapefile from deltamapr which
# is the legal Delta boundary and combine the Suisun Marsh and Bay and San Pablo Bay Strata
# polygons from the LTM design team effort.

# Import Strata polygons and remove Central and South SF Bays
sf_strata <-
  read_sf(
    here("data/processed/spatial/first_flush_spatial_data.gpkg"),
    layer = "design_strata"
  ) |>
  filter(!Stratum %in% c("Out of Bounds", "Central SF Bay", "South SF Bay"))

# Combine Suisun Marsh and Bay and San Pablo Bay Strata to the legal Delta boundary - this is
# the boundary used to restrict stations
sf_boundary <- R_Delta %>%
  st_transform(st_crs(sf_strata)) %>%
  select(geometry) %>%
  st_union(st_union(sf_strata)) %>%
  # Transform to crs 4326 to align with station coordinates
  st_transform(crs = 4326) %>%
  # Resolve topology errors
  st_make_valid()


# USGS ----------------------------------------------------------------------------------------

# Define a bounding box of sf_boundary to use to filter USGS stations
sf_delta_bbox <- st_bbox(sf_boundary)

# List all USGS stations within the sf_boundary bounding box
sf_usgs_sta <- read_waterdata_monitoring_location(bbox = sf_delta_bbox)

# Filter USGS stations to those within sf_boundary
sf_usgs_sta_filt <- sf_usgs_sta %>% st_filter(sf_boundary)

# Define parameters of interest
params <- c(
  "00010", # Water Temperature (Celcius)
  "00300", # Dissolved Oxygen (mg/L)
  "00095", # Specific Conductance at 25 C (uS/cm)
  "00400", # pH
  "63680", # Turbidity (FNU)
  "32316", # Chlorophyll concentration estimated from reference material (ug/L)
  "80154", # Suspended sediment concentration, milligrams per liter
  "72255", # Mean water velocity (ft/sec)
  "00060", # Discharge (cfs)
  "72137" # Discharge, tidally-filtered (cfs)
)

# Download metadata for parameters of interest for all USGS stations within sf_boundary
df_usgs_sta_meta <-
  # Needs to be broken up to work with API
  tibble(
    istart = seq(from = 1, to = nrow(sf_usgs_sta_filt), by = 250),
    iend = c(
      seq(from = 250, to = nrow(sf_usgs_sta_filt), by = 250),
      nrow(sf_usgs_sta_filt)
    ),
    df_meta = map2(
      istart,
      iend,
      \(x, y) {
        read_waterdata_ts_meta(
          monitoring_location_id = sf_usgs_sta_filt$monitoring_location_id[x:y],
          parameter_code = params,
          skipGeometry = TRUE
        )
      }
    ),
    df_meta_nrow = map_int(df_meta, nrow)
  ) %>%
  # Remove records without metadata
  filter(df_meta_nrow > 0) %>%
  select(df_meta) %>%
  unnest(df_meta)

# Filter metadata to the stations we're interested in
df_usgs_sta_meta_filt <- df_usgs_sta_meta %>%
  filter(computation_identifier %in% c("Mean", "Instantaneous")) %>%
  select(
    monitoring_location_id,
    parameter_name,
    computation_identifier,
    begin_utc,
    end_utc
  ) |>
  # select(
  #   Station_ID = monitoring_location_id,
  #   Parameter = parameter_name,
  #   Interval = computation_identifier,
  #   Start = begin_utc,
  #   End = end_utc
  # ) %>%
  drop_na(begin_utc, end_utc) %>%
  summarize(
    Start = min(begin_utc, na.rm = TRUE),
    End = max(end_utc, na.rm = TRUE),
    .by = c(monitoring_location_id, parameter_name, computation_identifier)
  ) %>%
  # Only include active stations or ones that ended in 2023
  filter(year(End) %in% 2023:year(today()))

# Add location name and geometry to filtered metadata, standardize for integration
sf_usgs_sta_meta <- df_usgs_sta_meta_filt %>%
  left_join(
    sf_usgs_sta_filt %>%
      select(monitoring_location_id, monitoring_location_name),
    by = join_by(monitoring_location_id)
  ) %>%
  mutate(
    Survey = "USGS",
    Data_Source = "USGS API",
    Station_ID = monitoring_location_id,
    Station_Name = monitoring_location_name,
    Parameter = parameter_name,
    Interval = computation_identifier,
    across(where(is.POSIXct), \(x) date(with_tz(x, tzone = "Etc/GMT+8"))),
    geometry,
    .keep = "none"
  ) |>
  st_as_sf(sf_column_name = "geometry")


# DWR - NCRO from EDI publications -----------------------------------------------------------

# Get all data entity names for NCRO EDI publications
edi_ids_ncro <- c(
  south_delta = "edi.2180.2",
  yolo_bypass = "edi.2363.1",
  rock_slough = "edi.2375.1",
  central_delta = "edi.2385.1"
)

edi_data_ent_ncro <-
  map(edi_ids_ncro, read_data_entity_names) |>
  map(\(x) filter(x, !str_detect(entityName, "^Instrument")))

# Import station metadata for each publication
df_ncro_sta_edi <- edi_data_ent_ncro |>
  map(\(x) filter(x, str_detect(entityName, "^Station"))) |>
  enframe() |>
  unnest(value) |>
  left_join(enframe(edi_ids_ncro, value = "edi_pkg_id"), by = "name") |>
  mutate(
    df_data_raw = map2(
      edi_pkg_id,
      entityId,
      \(x, y) {
        api_with_retry(
          api_fn = read_data_entity,
          max_attempts = 5,
          initial_delay = 2,
          packageId = x,
          entityId = y
        )
      }
    ),
    df_data = map(df_data_raw, read_csv)
  ) |>
  select(edi_pkg_id, df_data) |>
  unnest(df_data)

# Convert station metadata to a sf object and filter to the geographical area of
# interest (sf_boundary)
sf_ncro_sta_edi <- df_ncro_sta_edi |>
  select(
    edi_pkg_id,
    Station_Code,
    Station_Name,
    Latitude,
    Longitude,
    WDL_Station_Code
  ) |>
  # Assume all coordinates are in WGS84
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) |>
  st_filter(sf_boundary)

# Import data for all entities within sf_boundary from EDI publications
df_ncro_data_edi <- edi_data_ent_ncro |>
  map(\(x) filter(x, !str_detect(entityName, "^Station"))) |>
  enframe() |>
  unnest(value) |>
  left_join(enframe(edi_ids_ncro, value = "edi_pkg_id"), by = "name") |>
  filter(entityName %in% unique(sf_ncro_sta_edi$Station_Code)) |>
  mutate(
    df_data_raw = map2(
      edi_pkg_id,
      entityId,
      \(x, y) {
        api_with_retry(
          api_fn = read_data_entity,
          max_attempts = 5,
          initial_delay = 2,
          packageId = x,
          entityId = y
        )
      }
    ),
    df_data = map(df_data_raw, read_csv)
  )

# Determine start and end dates for each station-parameter combination
df_ncro_sta_meta_edi <- df_ncro_data_edi |>
  mutate(
    df_sta_summ = map(
      df_data,
      \(x) {
        mutate(x, Date_Time = mdy_hms(Date_Time, tz = "Etc/GMT+8")) |>
          select(any_of(c(
            "Station_Code",
            "Date_Time",
            "Water_Temperature",
            "Specific_Conductance",
            "DO",
            "Turbidity",
            "pH",
            "Chlorophyll"
          ))) |>
          pivot_longer(
            cols = where(is.numeric),
            names_to = "Parameter",
            values_to = "result",
            values_drop_na = TRUE
          ) |>
          summarize(
            Start = min(date(Date_Time)),
            End = max(date(Date_Time)),
            .by = c(Station_Code, Parameter)
          )
      }
    ),
    .keep = "none"
  ) |>
  unnest(df_sta_summ)

# Add NCRO station metadata to station info
sf_ncro_sta_meta_edi <- sf_ncro_sta_edi |>
  left_join(df_ncro_sta_meta_edi, by = join_by(Station_Code)) |>
  mutate(
    Survey = "DWR-NCRO (WQ)",
    Data_Source = paste0("EDI (", edi_pkg_id, ")"),
    Station_ID = Station_Code,
    Station_Name,
    Parameter,
    Start,
    End,
    Interval = "15-min",
    geometry,
    WDL_Station_Code,
    .keep = "none"
  )


# DWR - NCRO from WDL -------------------------------------------------------------------------

# Import csv file from CNRA data portal with station info for DWR's continuous water quality data
df_ncro_sta_wdl <- read_csv(
  "https://data.cnra.ca.gov/dataset/fcba3a88-a359-4a71-a58c-6b0ff8fdc53f/resource/c2b08f48-acfd-4a5b-9799-0f3e07d83192/download/stations.csv"
)

# Import csv file from CNRA data portal with metadata for DWR's continuous water quality data
df_ncro_sta_meta_wdl <- read_csv(
  "https://data.cnra.ca.gov/dataset/fcba3a88-a359-4a71-a58c-6b0ff8fdc53f/resource/cdb5dd35-c344-4969-8ab2-d0e2d6c00821/download/station-trace-download-links.csv"
)

# Find stations where DWR collects parameters of interest
df_ncro_sta_meta_wdl_filt <- df_ncro_sta_meta_wdl %>%
  filter(
    parameter %in%
      c(
        "Chlorophyll",
        "DissolvedOxygen",
        "ECat25C",
        "pH",
        "Turbidity",
        "WaterTemp",
        "StreamFlow",
        "Velocity"
      ),
    station_type != "Groundwater",
    output_interval == "RAW"
  )

# Filter station info to only those where DWR collects parameters of interest and are within
# sf_boundary. Also remove stations already accounted for in the NCRO EDI publications above
sf_ncro_sta_wdl_filt <- df_ncro_sta_wdl %>%
  filter(
    station_number %in% unique(df_ncro_sta_meta_wdl_filt$station_number)
  ) |>
  filter_out(
    station_number %in% unique(sf_ncro_sta_meta_edi$WDL_Station_Code)
  ) |>
  filter_out(
    station_number %in%
      c(
        "B9504500",
        "B9504400",
        "B9505000",
        "B9512000",
        "B9545800",
        "B9409500",
        "B9525100",
        "B9510800",
        "B9520000",
        "B9502900",
        "B9506100",
        "B9561600"
      )
  ) |>
  # Convert to sf object, assume all coordinates are in WGS84
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_filter(sf_boundary) |>
  # Remove some random stations that I know we don't want included
  filter_out(str_detect(
    station_name,
    "^Blacklock Wetland Breach|Lindsey Slough Tidal Wetland|Westervelt Tidal Wetland|Yolo Bypass Tidal Wetland"
  ))

# Add metadata to stations we're interested in, standardize for integration
sf_ncro_sta_meta_wdl <- sf_ncro_sta_wdl_filt %>%
  select(station_number, station_name, station_type) |>
  left_join(
    df_ncro_sta_meta_wdl_filt |> select(station_number, parameter),
    by = join_by(station_number)
  ) |>
  mutate(
    Survey = recode_values(
      station_type,
      "Water Quality" ~ "DWR-NCRO (WQ)",
      "Surface Water" ~ "DWR-NCRO (SW)",
      "Tide Station" ~ "DWR-NCRO (Tide)"
    ),
    Data_Source = "WDL",
    Station_ID = station_number,
    Station_Name = station_name,
    Parameter = parameter,
    Interval = "15-min",
    geometry,
    .keep = "none"
  )


# DWR - CEMP from EDI publication -------------------------------------------------------------

# Get all data entity names for CEMP EDI publication (edi.1177.8)
edi_id_cemp <- "edi.1177.8"
edi_data_ent_cemp <- as_tibble(read_data_entity_names(edi_id_cemp))

# Import station metadata
df_cemp_sta <- edi_data_ent_cemp |>
  filter(entityName == "Station Metadata") |>
  mutate(
    df_data_raw = map(
      entityId,
      \(x) {
        api_with_retry(
          api_fn = read_data_entity,
          max_attempts = 5,
          initial_delay = 2,
          packageId = edi_id_cemp,
          entityId = x
        )
      }
    ),
    df_data = map(df_data_raw, read_csv)
  ) |>
  select(df_data) |>
  unnest(df_data)

# Convert station metadata to a sf object and filter to the geographical area of
# interest (sf_boundary)
sf_cemp_sta <- df_cemp_sta |>
  select(station, description, latitude, longitude) |>
  # Assume all coordinates are in WGS84
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_filter(sf_boundary)

# Import data for all entities within sf_boundary from EDI publication
df_cemp_data <- edi_data_ent_cemp |>
  filter_out(
    entityName %in% c("Station Metadata", "Instrument Specifications")
  ) |>
  mutate(
    df_data_raw = map(
      entityId,
      \(x) {
        api_with_retry(
          api_fn = read_data_entity,
          max_attempts = 5,
          initial_delay = 2,
          packageId = edi_id_cemp,
          entityId = x
        )
      }
    ),
    df_data = map(df_data_raw, read_csv)
  )

# Determine start and end dates for each station-parameter combination
df_cemp_sta_meta <- df_cemp_data |>
  mutate(
    df_sta_summ = map(
      df_data,
      \(x) {
        mutate(
          x,
          datetime = ymd_hms(paste(date, time), tz = "Etc/GMT+8"),
          .keep = "unused"
        ) |>
          pivot_longer(
            cols = where(is.numeric),
            names_to = "Parameter",
            values_to = "result",
            values_drop_na = TRUE
          ) |>
          summarize(
            Start = min(date(datetime)),
            End = max(date(datetime)),
            .by = c(station, Parameter)
          )
      }
    ),
    .keep = "none"
  ) |>
  unnest(df_sta_summ)

# Add CEMP station metadata to station info
sf_cemp_sta_meta <- sf_cemp_sta |>
  left_join(df_cemp_sta_meta, by = join_by(station)) %>%
  mutate(
    Survey = "DWR-CEMP",
    Data_Source = paste0("EDI (", edi_id_cemp, ")"),
    Station_ID = station,
    Station_Name = description,
    Parameter,
    Start,
    End,
    Interval = "15-min",
    geometry,
    .keep = "none"
  )


# DWR - WQA from WQP --------------------------------------------------------------------------

# Import Suisun Marsh station info from the WQP database
df_suisun_sta <- wqp_stations()

# Define station_codes for the stations maintained by the WQA group
wqa_sta_codes <- c(
  "A-96",
  "C-2B",
  "S-21",
  "S-28",
  "S-33",
  "S-35",
  "S-37",
  "S-4",
  "S-42",
  "S-49",
  "S-54",
  "S-64",
  "S-71",
  "S-72",
  "S-77",
  "S-97"
)

# Filter Suisun Marsh station info to those maintained by the WQA group and within sf_boundary
sf_wqa_sta <- df_suisun_sta %>%
  filter(station_code %in% wqa_sta_codes) %>%
  select(station_code, station_name, latitude, longitude) %>%
  # Convert to sf object, assume all coordinates are in WGS84
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_filter(sf_boundary)

# Import all result details from WQP to be filtered
df_wqp_rd <- wqp_result_details()

# Filter result details to stations and parameters of interest
df_wqp_rd_filt <- df_wqp_rd %>%
  mutate(station_id = str_extract(station_name, "(?<=\\().+(?=\\))")) %>%
  filter(
    reading_type_name == "Time Series",
    interval_name == "15 min",
    station_id %in% unique(sf_wqa_sta$station_code),
    analyte_name %in%
      c(
        "Chlorophyll",
        "Dissolved Oxygen",
        "DOCONC",
        "pH",
        "Specific Conductance",
        "Turbidity",
        "Water Temperature"
      )
  )

# Import all data collected by WQA from the WQP database
ndf_wqa_data <- df_wqp_rd_filt %>%
  select(result_id, station_id, analyte_name) %>%
  mutate(df_data = map(result_id, wqp_result_data))

# Determine start and end dates for each station-parameter combination
df_wqa_sta_meta <- ndf_wqa_data %>%
  select(-result_id) %>%
  mutate(df_data = map(df_data, \(x) chuck(x, 1))) %>%
  unnest(df_data) %>%
  drop_na(value) %>%
  summarize(
    Start = min(date(time)),
    End = max(date(time)),
    .by = c(station_id, analyte_name)
  )

# Add WQA station metadata to station info
sf_wqa_sta_meta <- sf_wqa_sta %>%
  left_join(df_wqa_sta_meta, by = join_by(station_code == station_id)) %>%
  # Only include active stations or ones that ended in 2023
  filter(year(End) %in% 2023:year(today())) %>%
  mutate(
    Survey = "DWR-WQA",
    Data_Source = "WQP (DWR internal)",
    Station_ID = station_code,
    Station_Name = station_name,
    Parameter = analyte_name,
    Start,
    End,
    Interval = "15-min",
    geometry,
    .keep = "none"
  )


# Combine Metadata ----------------------------------------------------------------------------

# Create lookup table for standardizing parameter names
df_params_std <- tibble(
  from = list(
    c("Chlorophyll", "fChl, water, in situ", "fluorescence"),
    c(
      "Dissolved oxygen",
      "Dissolved Oxygen",
      "dissolvedoxygen",
      "DissolvedOxygen",
      "DO"
    ),
    c(
      "ECat25C",
      "spc",
      "Specific cond at 25C",
      "Specific Conductance",
      "Specific_Conductance"
    ),
    c("ph", "pH"),
    c("Suspnd sedmnt conc"),
    c(
      "Temperature, water",
      "Water Temperature",
      "Water_Temperature",
      "WaterTemp",
      "watertemperature"
    ),
    c("turbidity", "Turbidity", "Turbidity, FNU"),
    c("Discharge", "StreamFlow"),
    c("Discharge,tide fltrd"),
    c("Mean water velocity", "Velocity")
  ),
  to = c(
    "Chlorophyll",
    "Dissolved Oxygen",
    "Specific Conductance",
    "pH",
    "Suspended Sediment Conc",
    "Water Temperature",
    "Turbidity",
    "Discharge",
    "Tidally-filtered Discharge",
    "Mean Water Velocity"
  )
)

# Combine metadata for all continuous water quality stations
sf_meta_comb <-
  bind_rows(
    sf_usgs_sta_meta,
    sf_ncro_sta_meta_edi,
    sf_ncro_sta_meta_wdl,
    sf_cemp_sta_meta,
    sf_wqa_sta_meta
  ) %>%
  # Standardize Parameter and Interval columns
  mutate(
    Parameter = recode_values(
      Parameter,
      from = df_params_std$from,
      to = df_params_std$to,
      unmatched = "error"
    ),
    Interval = recode_values(
      Interval,
      c("15-min", "Instantaneous") ~ "15-min",
      "Mean" ~ "Daily Mean",
      unmatched = "error"
    )
  ) %>%
  select(
    Survey,
    Data_Source,
    Station_ID,
    Station_Name,
    Parameter,
    Interval,
    Start,
    End,
    geometry
  ) |>
  arrange(Survey, Station_ID, Parameter, Interval)

# Export combined metadata and station coordinates to be used in interactive maps
sf_meta_comb |>
  # Add Strata
  st_transform(crs = st_crs(sf_strata)) |>
  st_join(sf_strata, join = st_intersects) |>
  st_transform(crs = 4326) |>
  distinct(Survey, Data_Source, Station_ID, Stratum, geometry) |>
  write_sf(
    here("data/processed/spatial/first_flush_spatial_data.gpkg"),
    layer = "cont_wq_stations"
  )

sf_meta_comb %>%
  st_drop_geometry() %>%
  saveRDS(here("data/processed/wq/cont_wq_metadata.rds"))
