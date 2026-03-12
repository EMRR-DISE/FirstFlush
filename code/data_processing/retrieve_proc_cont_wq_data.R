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
library(here)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Source data functions
source(here("code/data_processing/01_data_retrieve_process_functions.R"))

# Define station abbreviations, full names, and ID's for data access
df_stations <- tribble(
  ~source    , ~full_name                                       , ~data_id   , ~abbr ,
  "USGS"     , "Sacramento River at Freeport"                   , "11447650" , "FPT" ,
  "USGS"     , "San Joaquin River near Vernalis"                , "11303500" , "SJR" ,
  "DWR_CEMP" , "San Joaquin River McCune Station near Vernalis" , "SJR"      , "SJR" ,
  "DWR_CEMP" , "Sacramento River at Rio Vista Bridge"           , "RVB"      , "RVB" ,
  "USGS"     , "San Joaquin River at Jersey Point"              , "11337190" , "SJJ" ,
  "DWR_CEMP" , "San Joaquin River at Prisoners Point"           , "PPT"      , "PPT" ,
  "DWR_NCRO" , "Old River at Quimby Island near Bethel Island"  , "B9520000" , "ORQ" ,
  "DWR_NCRO" , "Middle River near Holt"                         , "B9545800" , "HLT" ,
  "DWR_NCRO" , "Old River near Bacon Island at USGS Pile"       , "B9525100" , "OBI" ,
  "USGS"     , "Middle River at Middle River"                   , "11312676" , "MDM" ,
  "USGS"     , "Old River near Byron"                           , "11313315" , "ORB" ,
  "DWR_NCRO" , "Grant Line Canal near Clifton Court Forebay"    , "B9529500" , "GLC" ,
  "DWR_NCRO" , "Old River at Tracy Wildlife Association"        , "B9537800" , "TWA"
)

# Convert to list for data downloads
ls_stations <- df_stations |>
  select(source, abbr, data_id) |>
  nest(df_data = c(abbr, data_id)) |>
  mutate(df_data = map(df_data, deframe)) |>
  deframe()

# Download Data ------------------------------------------------------------------------------

## USGS --------------------------------------------------------------------------------------

# Define end date
end_date <- "2025-09-30"

# Daily suspended sediment concentration
ls_usgs_ssc <- ls_stations$USGS[c("FPT", "SJR")] |>
  map(\(x) paste0("USGS-", x)) |>
  map(
    \(x) {
      read_waterdata_daily(
        monitoring_location_id = x,
        parameter_code = "80154",
        statistic_id = "00003",
        skipGeometry = TRUE,
        time = paste0("../", end_date)
      )
    }
  )

# Instantaneous (15-min) turbidity data
ls_usgs_turb <- ls_stations$USGS[!names(ls_stations$USGS) == "SJR"] |>
  map(
    \(x) {
      readNWISuv(
        siteNumbers = x,
        parameterCd = "63680",
        tz = "Etc/GMT+8",
        endDate = end_date
      )
    }
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

## DWR_NCRO ----------------------------------------------------------------------------------

ls_ncro_turb <- ls_stations$DWR_NCRO |>
  map(\(x) get_cnra_cwq_data(x, parameters = "Turbidity"))

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
  filter(date <= end_date) |>
  summarize(
    turbidity_mean = mean(turbidity),
    turbidity_median = median(turbidity),
    .by = c(station_abbr, date)
  ) |>
  left_join(
    df_stations |>
      filter(!(source == "USGS" & abbr == "SJR")) |>
      select(station_long = full_name, abbr),
    by = join_by(station_abbr == abbr)
  ) |>
  relocate(station_long, .after = station_abbr)

# Prepare SCC data for export
df_usgs_ssc <- ls_usgs_ssc |>
  map(\(x) select(x, date = time, ssc = value)) |>
  list_rbind(names_to = "station_abbr") |>
  left_join(
    df_stations |>
      filter(source == "USGS") |>
      select(station_long = full_name, abbr),
    by = join_by(station_abbr == abbr)
  ) |>
  relocate(station_long, .after = station_abbr)

# Export Data ---------------------------------------------------------------------------------

# Export turbidity and SSC data as .rds files
df_turb_dv %>% saveRDS(here("data/processed/wq/turbidity_dv.rds"))
df_usgs_ssc %>% saveRDS(here("data/processed/wq/ssc_dv.rds"))
