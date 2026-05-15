# Retrieve continuous WQ data, integrate, and calculate daily values
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Currently this script downloads and processes continuous WQ data for the following parameters
# and stations:
# Parameters: Turbidity and suspended sediment
# Stations:
# USGS Sacramento River at Freeport (11447650)
# USGS San Joaquin River near Vernalis (11303500)
# DWR_CEMP San Joaquin River McCune Station near Vernalis (SJR)
# DWR_CEMP Sacramento River at Rio Vista Bridge (RVB)
# USGS San Joaquin River at Jersey Point (11337190)
# DWR_CEMP San Joaquin River at Prisoners Point (PPT)
# DWR-NCRO (WQES) Old River at Quimby Island near Bethel Island (B9520000)
# DWR-NCRO (WQES) Middle River near Holt (B9545800)
# DWR-NCRO (WQES) OBI at USGS Pile (B9525100)
# USGS Middle River at Middle River (11312676)
# USGS Old River near Byron (11313315)
# DWR-NCRO (WQES) Grant Line Canal near Clifton Court Forebay (B9529500)
# DWR-NCRO (WQES) Old River at Tracy Wildlife Association (B9537800)

# Load packages
library(tidyverse)
library(dataRetrieval)
library(sf)
library(here)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Source data functions
source(here("code/data_processing/utils.R"))

# Define end date
END_DATE <- "2025-09-30"

# Define station abbreviations, full names, and ID's for data access
df_stations <- tribble(
  ~data_source , ~station_full_name                               , ~station_id     , ~station_abbr ,
  "USGS"       , "Sacramento River at Freeport"                   , "USGS-11447650" , "FPT"         ,
  "USGS"       , "San Joaquin River near Vernalis"                , "USGS-11303500" , "SJR_USGS"    ,
  "DWR_CEMP"   , "San Joaquin River McCune Station near Vernalis" , "SJR"           , "SJR_DWR"     ,
  "DWR_CEMP"   , "Sacramento River at Rio Vista Bridge"           , "RVB"           , "RVB"         ,
  "USGS"       , "San Joaquin River at Jersey Point"              , "USGS-11337190" , "SJJ"         ,
  "DWR_CEMP"   , "San Joaquin River at Prisoners Point"           , "PPT"           , "PPT"         ,
  "DWR_NCRO"   , "Old River at Quimby Island near Bethel Island"  , "B9520000"      , "ORQ"         ,
  "DWR_NCRO"   , "Middle River near Holt"                         , "B9545800"      , "HLT"         ,
  "DWR_NCRO"   , "Old River near Bacon Island at USGS Pile"       , "B9525100"      , "OBI"         ,
  "USGS"       , "Middle River at Middle River"                   , "USGS-11312676" , "MDM"         ,
  "USGS"       , "Old River near Byron"                           , "USGS-11313315" , "ORB"         ,
  "DWR_NCRO"   , "Grant Line Canal near Clifton Court Forebay"    , "B9529500"      , "GLC"         ,
  "DWR_NCRO"   , "Old River at Tracy Wildlife Association"        , "B9537800"      , "TWA"
)

# Convert to list for data downloads
ls_stations <- df_stations |>
  select(data_source, station_abbr, station_id) |>
  nest(df_data = c(station_abbr, station_id)) |>
  mutate(df_data = map(df_data, deframe)) |>
  deframe()

# Download Data ------------------------------------------------------------------------------

## USGS --------------------------------------------------------------------------------------

# Daily suspended sediment concentration
ls_usgs_ssc <- ls_stations$USGS[c("FPT", "SJR_USGS")] |>
  map(
    \(x) {
      read_waterdata_daily(
        monitoring_location_id = x,
        parameter_code = "80154",
        statistic_id = "00003",
        skipGeometry = TRUE,
        time = paste0("../", END_DATE)
      )
    }
  )

# Instantaneous (15-min) turbidity data
ls_usgs_turb <- ls_stations$USGS[!names(ls_stations$USGS) == "SJR_USGS"] |>
  map(\(x) str_remove(x, "^USGS-")) |>
  map(
    \(x) {
      readNWISuv(
        siteNumbers = x,
        parameterCd = "63680",
        tz = "Etc/GMT+8",
        endDate = END_DATE
      )
    }
  )

# Import and clean up station information
sf_station_info_usgs <- read_waterdata_monitoring_location(
  monitoring_location_id = ls_stations$USGS,
  properties = c("monitoring_location_id", "geometry")
)

df_station_info_usgs <-
  bind_cols(
    st_drop_geometry(sf_station_info_usgs),
    st_coordinates(sf_station_info_usgs)
  ) |>
  rename(
    station_id = monitoring_location_id,
    longitude = X,
    latitude = Y
  )

## DWR_CEMP ----------------------------------------------------------------------------------

# Define EDI ID and data entities for CEMP EDI publication (edi.1177.7)
edi_id_cemp <- "edi.1177.7"
stations_cemp <- ls_stations$DWR_CEMP

# Check if Data_ID's in df_stations_cemp match data entities from EDI
edi_data_ent_cemp <- get_edi_data_entities(edi_id = edi_id_cemp)

map(stations_cemp, \(x) any(x == edi_data_ent_cemp))

# Import data for each CEMP station from EDI publication
get_edi_data(edi_id = edi_id_cemp, entity_names = stations_cemp)
ls_cemp <- stations_cemp |>
  map(\(x) paste0(x, ".bin")) |>
  map(\(x) read_csv(file.path(tempdir(), x)))

# Import and clean up station information
get_edi_data(edi_id = edi_id_cemp, entity_names = "Stations Metadata")
df_station_info_cemp <- read_csv(file.path(tempdir(), "Stations Metadata.bin"))

df_station_info_cemp_c <- df_station_info_cemp |>
  filter(`Station Acronym` %in% ls_stations$DWR_CEMP) |>
  select(station_id = `Station Acronym`, Latitude, Longitude) |>
  rename_with(str_to_lower)

## DWR_NCRO ----------------------------------------------------------------------------------

ls_ncro_turb <- ls_stations$DWR_NCRO |>
  map(\(x) get_cnra_cwq_data(x, parameters = "Turbidity"))

# Import and clean up station information
df_station_info_ncro <- read_csv(
  "https://data.cnra.ca.gov/dataset/fcba3a88-a359-4a71-a58c-6b0ff8fdc53f/resource/c2b08f48-acfd-4a5b-9799-0f3e07d83192/download/stations.csv"
)

df_station_info_ncro_c <- df_station_info_ncro |>
  filter(station_number %in% ls_stations$DWR_NCRO) |>
  select(station_id = station_number, latitude, longitude)


# Prepare and Aggregate Data -----------------------------------------------------------------

# Continuous turbidity data
df_usgs_turb <- ls_usgs_turb |>
  map(\(x) as_tibble(select(x, -c(site_no, ends_with("_cd"))))) |>
  map(\(x) comb_cols(x, "_63680_")) |>
  list_rbind(names_to = "station_abbr") |>
  mutate(date = date(dateTime), .keep = "unused")

df_cemp_turb <- ls_cemp |>
  map(\(x) select(x, date, turbidity)) |>
  list_rbind(names_to = "station_abbr")

df_ncro_turb <- ls_ncro_turb |>
  map(\(x) select(x, Date, turbidity = Point)) |>
  list_rbind(names_to = "station_abbr") |>
  mutate(date = date(mdy_hms(Date)), .keep = "unused")

# Combine and calculate daily averages and medians for continuous turbidity data
df_turb_dv <- bind_rows(df_usgs_turb, df_cemp_turb, df_ncro_turb) |>
  drop_na(turbidity) |>
  filter(date <= END_DATE) |>
  summarize(
    turbidity_mean = mean(turbidity),
    turbidity_median = median(turbidity),
    .by = c(station_abbr, date)
  )

# Prepare SCC data for export
df_usgs_ssc <- ls_usgs_ssc |>
  map(\(x) select(x, date = time, ssc = value)) |>
  list_rbind(names_to = "station_abbr")

# Combine station information for export
df_station_info_all <-
  bind_rows(
    df_station_info_usgs,
    df_station_info_cemp_c,
    df_station_info_ncro_c
  ) |>
  left_join(df_stations, by = join_by(station_id)) |>
  select(
    data_source,
    station_abbr,
    station_id,
    station_full_name,
    latitude,
    longitude
  )

# Export Data ---------------------------------------------------------------------------------

# Export data as .rds files
fp_export <- here("data/processed/wq")

df_turb_dv %>% saveRDS(file.path(fp_export, "turbidity_dv.rds"))
df_usgs_ssc %>% saveRDS(file.path(fp_export, "ssc_dv.rds"))
df_station_info_all |>
  saveRDS(file.path(fp_export, "cont_wq_station_info_analysis.rds"))
