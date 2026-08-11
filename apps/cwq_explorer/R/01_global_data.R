# Loads and prepares data for cwq_explorer Shiny App
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Load packages
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(qs2)
library(sf)
library(here)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Load shared functions from functions/shared
here("functions/shared") |>
  list.files(pattern = "\\.[Rr]$", full.names = TRUE, recursive = TRUE) |>
  walk(source)

# Import data
fp_processed_data <- here("data/processed")
cwq_data <- qd_read(file.path(fp_processed_data, "wq/cwq_data_dv_all.qdata"))
cwq_stations <- readRDS(
  file.path(fp_processed_data, "wq/cwq_station_metadata.rds")
)
load(file.path(fp_processed_data, "storms/StormData.RData"))

# Import Strata polygons
sf_strata <-
  read_sf(
    here("data/processed/spatial/first_flush_spatial_data.gpkg"),
    layer = "design_strata"
  ) |>
  filter(
    !Stratum %in%
      c("Out of Bounds", "Napa River", "Central SF Bay", "South SF Bay")
  ) |>
  st_transform(crs = 4326)

# Convert stations metadata to sf object to be used in map
sf_stations <- cwq_stations |>
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
  )

# Calculate bounding boxes for sf_stations and sf_strata for zoom buttons
bbox_strata <- st_bbox(sf_strata)
bbox_stations <- st_bbox(sf_stations)

# Prepare station metadata for reactable and to join to continuous WQ data to add stratum
cwq_stations_c <- cwq_stations |>
  distinct(station_abbr, station_name, stratum) |>
  distinct(station_abbr, stratum, .keep_all = TRUE) |>
  mutate(
    station_abbr = convert_fct_station_abbr(
      replace_values(station_abbr, "RYI" ~ "RYI-RYF")
    ),
    station_name = if_else(
      station_abbr == "RYI-RYF",
      "Cache Slough near Ryer Island - combined stations",
      station_name
    )
  ) |>
  drop_na(station_abbr)

# Prepare continuous WQ data for plots
cwq_data_c <- cwq_data |>
  # Consolidate discharge and discharge_tf
  mutate(discharge = coalesce(discharge_tf, discharge)) |>
  select(-c(discharge_tf, ssc)) |>
  pivot_longer(
    cols = where(is.numeric),
    names_to = "parameter",
    values_to = "value",
    values_drop_na = TRUE
  ) |>
  group_by(station_abbr, parameter) |>
  complete(date = seq(min(date), max(date), by = "1 day")) |>
  ungroup() |>
  left_join(
    cwq_stations_c |> select(station_abbr, stratum),
    by = join_by(station_abbr)
  ) |>
  mutate(
    wy_adj = assign_wy_adj(date),
    dowy_adj = assign_wy_adj_day(date)
  ) |>
  filter(
    between(wy_adj, min(Sacflow_wstorms$WY), max(Sacflow_wstorms$WY)),
    !month(date) %in% 6:8
  )

# Create data frame of first days of all storms
df_storms <- StormStartEnd |>
  mutate(
    wy_adj = assign_wy_adj(FirstDay),
    dowy_adj = assign_wy_adj_day(FirstDay),
    .keep = "none"
  ) |>
  filter(between(wy_adj, min(cwq_data_c$wy_adj), max(cwq_data_c$wy_adj)))

# Define range of dowy_adj for slider defaults
dowy_adj_rng <- range(cwq_data_c$dowy_adj)

# Define strata and station order from levels within continuous WQ data
strata_order <- levels(cwq_data_c$stratum)
station_order <- levels(cwq_data_c$station_abbr)
