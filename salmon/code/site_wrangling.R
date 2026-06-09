# estimate the "river km" for each DJFMP site location
library(here)
library(sf)
library(nhdplusTools)
library(janitor)
library(tidyverse)

# load DJFMP station data & wrangle

# first, region labels
region_lookup <- tibble(
  region = factor(1:7),
  region_label = factor(c("Lower Sacramento River", "North Delta",
                          "Central Delta", "South Delta",
                          "San Joaquin River",
                          "San Francisco & San Pablo Bays",
                          "Sacramento River"))
)

# 1. create the base stations dataset from the DJFMP data (dt5 and dt3)
stations <-
  # stations lat/lon, etc
  read_rds(here("salmon/data/raw/dt5.rds")) %>%
  clean_names() %>%
  mutate(station = station_code) %>%
  tibble() %>%
  # select stations used by beach seine program
  left_join(read_rds(here("salmon/data/raw/dt3.rds")) %>%
              clean_names() %>%
              mutate(region = region_code,
                     station = station_code,
                     .keep = "unused") %>%
              arrange(region, station, location) %>%
              ungroup() %>%
              dplyr::select(station, location, region) %>%
              distinct(),
            by = "station",
            keep = F) %>%
  # add region labels
  left_join(region_lookup, by = "region") %>%
  select(station = station_code, lon = longitude, lat = latitude, location = location.x,
         region, region_label, method_code)

# 2. convert tibble to a spatial sf object (WGS84)
stations_sf <- stations %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# 3. download Sacramento River flowlines using a bounding box from our stations
# NOT using 3 lines below bc difficulties w USGS server; using saved flowline file
# bbox <- st_bbox(stations_sf)
# nhd_data <- get_nhdplus(AOI = st_as_sfc(bbox), realization = "flowline")

# load the flowlines and project to California Albers (meters) for accurate math
# flowlines <- st_read(nhd_data, layer = "NHDFlowline", quiet = TRUE) %>% st_transform(crs = 3310)

stations_projected <- stations_sf %>%
  st_transform(crs = 3310)
flowlines <- read_rds(here("salmon/data/processed/flowlines")) %>%
  st_transform(crs = 3310)

# 4. snap stations to the river network and calculate River Kilometer (RKM)
# find the nearest river line segment index for each point
nearest_indexes <- st_nearest_feature(stations_projected, flowlines)

# extract NHD flowline attributes (NHD provides upstream/downstream network distance properties)
# 'pathlength' in NHD indicates kilometers to the basin outlet (defs for Sac R and SJR?!)
stations_with_rkm <- stations %>%
  mutate(
    nhd_segment_index = nearest_indexes,
    # pull the baseline distance-to-outlet value from the matched NHD streamline segment
    river_km = flowlines$pathlength[nearest_indexes] # 'lengthkm' doesn't work!
  ) %>%
  select(-nhd_segment_index)

# check it out; note: these are ALL the beach seine stations
print(stations_with_rkm)

# save your work!
write_rds(stations_with_rkm, here("salmon/data/processed/stations_with_rkm.rds"))
