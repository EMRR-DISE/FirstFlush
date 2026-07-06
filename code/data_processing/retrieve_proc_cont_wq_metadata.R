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

# Source data functions
source(here("code/functions/utils.R"))


# Define Spatial Extent -----------------------------------------------------------------------

# Define the spatial extent of the continuous water quality stations of interest as the legal Delta
# and Suisun Marsh and Bay. Use the R_Delta shapefile from deltamapr which is the legal Delta
# boundary and combine the Suisun Marsh and Bay Subregions from the EDSM shapefile in deltamapr.

# Import EDSM subregions from deltamapr and only keep Suisun Marsh and Bay Subregions
sf_suisun <- R_EDSM_Subregions_Mahardja_FLOAT %>%
  filter(
    SubRegion %in%
      c(
        "Lower Sacramento River",
        "Confluence",
        "Suisun Marsh",
        "Honker Bay",
        "Grizzly Bay",
        "Mid Suisun Bay",
        "West Suisun Bay"
      )
  ) %>%
  st_union()

# Combine Suisun Marsh and Bay to the legal Delta boundary - this is the boundary used to restrict
# stations
sf_delta_suisun <- R_Delta %>%
  st_transform(st_crs(sf_suisun)) %>%
  select(geometry) %>%
  st_union(sf_suisun) %>%
  # Transform to crs 4326 to align with station coordinates
  st_transform(crs = 4326) %>%
  # Resolve topology errors
  st_make_valid()


# USGS ----------------------------------------------------------------------------------------

# Define a bounding box of sf_delta_suisun to use to filter USGS stations
sf_delta_bbox <- st_bbox(sf_delta_suisun)

# List all USGS stations within the delta-suisun bounding box
sf_usgs_sta <- read_waterdata_monitoring_location(bbox = sf_delta_bbox)

# Filter USGS stations to those within sf_delta_suisun
sf_usgs_sta_filt <- sf_usgs_sta %>% st_filter(sf_delta_suisun)

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

# Download metadata for parameters of interest for all USGS stations within sf_delta_suisun
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
  filter(computation_identifier %in% c("Mean", "Median", "Instantaneous")) %>%
  select(
    Station_ID = monitoring_location_id,
    Parameter = parameter_name,
    Interval = computation_identifier,
    Start = begin,
    End = end
  ) %>%
  drop_na(Start, End) %>%
  summarize(
    Start = min(Start, na.rm = TRUE),
    End = max(End, na.rm = TRUE),
    .by = c(Station_ID, Parameter, Interval)
  ) %>%
  # Only include active stations or ones that ended in 2023
  filter(year(End) %in% 2023:year(today()))

# Add location name and geometry to filtered metadata, standardize for integration
sf_usgs_sta_meta <- df_usgs_sta_meta_filt %>%
  left_join(
    sf_usgs_sta_filt %>%
      select(monitoring_location_id, monitoring_location_name),
    by = join_by(Station_ID == monitoring_location_id)
  ) %>%
  rename(Station_Name = monitoring_location_name) %>%
  relocate(Station_Name, .after = Station_ID) %>%
  st_as_sf(sf_column_name = "geometry") %>%
  mutate(across(where(is.POSIXct), \(x) {
    with_tz(force_tz(x, tzone = "UTC"), tzone = "Etc/GMT+8")
  }))


# DWR - NCRO from WDL -------------------------------------------------------------------------

# Import csv file from CNRA data portal with station info for DWR's continuous water quality data
df_ncro_sta <- read_csv(
  "https://data.cnra.ca.gov/dataset/fcba3a88-a359-4a71-a58c-6b0ff8fdc53f/resource/c2b08f48-acfd-4a5b-9799-0f3e07d83192/download/stations.csv"
)

# Import csv file from CNRA data portal with links to DWR's continuous water quality data
df_ncro_sta_meta <- read_csv(
  "https://data.cnra.ca.gov/dataset/fcba3a88-a359-4a71-a58c-6b0ff8fdc53f/resource/cdb5dd35-c344-4969-8ab2-d0e2d6c00821/download/station-trace-download-links.csv"
)

# Find stations where DWR collects parameters of interest
df_ncro_sta_meta_filt <- df_ncro_sta_meta %>%
  filter(
    parameter %in%
      c(
        "Chlorophyll",
        "DissolvedOxygen",
        "ECat25C",
        "pH",
        "Turbidity",
        "WaterTemp"
      ),
    station_type %in% c("Not known", "Surface Water", "Water Quality"),
    output_interval == "RAW",
    # Only include active stations or ones that ended in 2023
    year(end_time) %in% 2023:2023:year(today())
  )

# Filter station info to only those where DWR collects parameters of interest and are within
# sf_delta_suisun
sf_ncro_sta_filt <- df_ncro_sta %>%
  filter(station_number %in% unique(df_ncro_sta_meta_filt$station_number)) %>%
  # Convert to sf object, assume all coordinates are in WGS84
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_filter(sf_delta_suisun)

# Add metadata to stations we're interested in, standardize for integration
sf_ncro_sta_meta <- sf_ncro_sta_filt %>%
  select(station_number, station_name, cdec_id, station_type) %>%
  left_join(
    df_ncro_sta_meta_filt %>%
      select(station_number, parameter, start_time, end_time),
    by = join_by(station_number)
  ) %>%
  mutate(
    Interval = "15-min",
    across(where(is.POSIXct), \(x) force_tz(x, tzone = "Etc/GMT+8"))
  ) %>%
  select(
    Station_ID = station_number,
    Station_Name = station_name,
    CDEC_ID = cdec_id,
    Station_Type = station_type,
    Parameter = parameter,
    Interval,
    Start = start_time,
    End = end_time,
    geometry
  )


# DWR - CEMP from EDI publication -------------------------------------------------------------

# Get all data entity names for CEMP EDI publication (edi.1177.8)
edi_id_cemp <- "edi.1177.8"
edi_data_ent_cemp <- get_edi_data_entities(edi_id = edi_id_cemp)

# Import CEMP station info from EDI publication
get_edi_data(edi_id = edi_id_cemp, entity_names = "Station Metadata")
df_cemp_sta <- read_csv(file.path(tempdir(), "Station Metadata.bin"))

# Import data for each CEMP station from EDI publication
# All stations are within the geographical area of interest
get_edi_data(edi_id = edi_id_cemp, entity_names = df_cemp_sta$station)
df_cemp_data <- df_cemp_sta$station %>%
  map(\(x) paste0(x, ".bin")) %>%
  map(\(x) read_csv(file.path(tempdir(), x))) %>%
  list_rbind()

# Determine start and end dates for each station-parameter combination
df_cemp_sta_meta <- df_cemp_data %>%
  mutate(
    datetime = ymd_hms(paste(date, time), tz = "Etc/GMT+8"),
    .keep = "unused"
  ) %>%
  pivot_longer(
    cols = where(is.numeric),
    names_to = "Parameter",
    values_to = "result",
    values_drop_na = TRUE
  ) %>%
  summarize(
    Start = min(datetime),
    End = max(datetime),
    .by = c(station, Parameter)
  )

# Add CEMP station metadata to station info
sf_cemp_sta_meta <- df_cemp_sta %>%
  select(
    Station_ID = station,
    Station_Name = description,
    latitude,
    longitude
  ) %>%
  left_join(df_cemp_sta_meta, by = join_by(Station_ID == station)) %>%
  mutate(Interval = "15-min") %>%
  # Convert to sf object, assume all coordinates are in WGS84
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


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

# Filter Suisun Marsh station info to those maintained by the WQA group and within sf_delta_suisun
sf_wqa_sta <- df_suisun_sta %>%
  filter(station_code %in% wqa_sta_codes) %>%
  select(
    Station_ID = station_code,
    Station_Name = station_name,
    CDEC_ID = cdec_code,
    latitude,
    longitude
  ) %>%
  # Add CDEC_ID for Hill Slough
  mutate(CDEC_ID = if_else(Station_ID == "S-4", "HSL", CDEC_ID)) %>%
  # Convert to sf object, assume all coordinates are in WGS84
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_filter(sf_delta_suisun)

# Import all result details from WQP to be filtered
df_wqp_rd <- wqp_result_details()

# Filter result details to stations and parameters of interest
df_wqp_rd_filt <- df_wqp_rd %>%
  mutate(station_id = str_extract(station_name, "(?<=\\().+(?=\\))")) %>%
  filter(
    reading_type_name == "Time Series",
    interval_name == "15 min",
    station_id %in% unique(sf_wqa_sta$Station_ID),
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
    Start = min(time),
    End = max(time),
    .by = c(station_id, analyte_name)
  )

# Add WQA station metadata to station info
sf_wqa_sta_meta <- sf_wqa_sta %>%
  left_join(df_wqa_sta_meta, by = join_by(Station_ID == station_id)) %>%
  # Only include active stations or ones that ended in 2023
  filter(year(End) %in% 2023:year(today())) %>%
  rename(Parameter = analyte_name) %>%
  mutate(Interval = "15-min")


# Combine Metadata ----------------------------------------------------------------------------

# Create lookup table for standardizing parameter names
df_params_std <- tibble(
  from = list(
    c("Chlorophyll", "fChl, water, in situ", "fluorescence"),
    c(
      "Dissolved oxygen",
      "Dissolved Oxygen",
      "dissolvedoxygen",
      "DissolvedOxygen"
    ),
    c("ECat25C", "spc", "Specific cond at 25C", "Specific Conductance"),
    c("ph", "pH"),
    c("Suspnd sedmnt conc"),
    c(
      "Temperature, water",
      "Water Temperature",
      "WaterTemp",
      "watertemperature"
    ),
    c("turbidity", "Turbidity", "Turbidity, FNU"),
    c("Discharge"),
    c("Discharge,tide fltrd"),
    c("Mean water velocity")
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
  list(
    "USGS" = sf_usgs_sta_meta,
    "DWR-NCRO" = sf_ncro_sta_meta,
    "DWR-CEMP" = sf_cemp_sta_meta,
    "DWR-WQA" = sf_wqa_sta_meta
  ) %>%
  bind_rows(.id = "Source") %>%
  relocate(CDEC_ID, .after = Station_Name) %>%
  # Standardize Parameter and Interval columns, consolidate Station_Type and Source columns
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
    ),
    Source = case_when(
      is.na(Station_Type) ~ Source,
      Station_Type == "Water Quality" ~ paste(Source, "(WQ)"),
      Station_Type == "Surface Water" ~ paste(Source, "(SW)")
    ),
    .keep = "unused"
  ) %>%
  arrange(Source, Station_ID, Parameter, Interval)

# Import Region polygons
sf_regions <- read_sf(
  here("data/processed/spatial/first_flush_spatial_data.gpkg"),
  layer = "design_regions"
)

# Export combined metadata and station coordinates to be used in interactive maps
sf_meta_comb |>
  # Add Regions
  st_transform(crs = st_crs(sf_regions)) |>
  st_join(sf_regions, join = st_intersects) |>
  st_transform(crs = 4326) |>
  distinct(Source, Station_ID, Region, geometry) |>
  write_sf(
    here("data/processed/spatial/first_flush_spatial_data.gpkg"),
    layer = "cont_wq_stations"
  )

sf_meta_comb %>%
  st_drop_geometry() %>%
  saveRDS(here("data/processed/wq/cont_wq_metadata.rds"))
