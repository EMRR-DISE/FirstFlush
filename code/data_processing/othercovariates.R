#### Pull other covariate data for fish analysis ####
# Install libraries
{
  library(dplyr)
  library(tidyverse)
  library(deltafish) #package to pull all fish and survey data from SFBD
  library("deltamapr") #package with spatial data for SFBD
  library(sf)
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(here)
  library(slider) #used to calculate rolling means
  library(readr)
  library(httr2)
  library(jsonlite)
}


## ---- Importing Dayflow from DWR to get daily X2 position ---- ####

# Download Dayflow X2 data

dayflow_1997_2023 <- read_csv(
  "https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fa674536/resource/21c377fe-53b8-4bd6-9e1f-2025221be095/download/dayflow-results-1997-2023.csv",
  show_col_types = FALSE
)

dayflow_2024 <- read_csv(
  "https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/6a7cb172-fb16-480d-9f4f-0322548fee83/download/dayflowcalculations2024.csv",
  show_col_types = FALSE
)

dayflow_2025 <- read_csv(
  "https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/541fe1b7-919a-467d-ac1e-ddbb8328c8f1/download/dayflowcalculations2025.csv",
  show_col_types = FALSE
)

# Combine and clean
dayflow_x2 <- bind_rows(
  dayflow_1997_2023 %>%
    mutate(
      Date = as.Date(Date, format = "%m/%d/%Y")
    ),

  dayflow_2024 %>%
    mutate(
      Date = as.Date(Date)
    ),

  dayflow_2025 %>%
    mutate(
      Date = as.Date(Date)
    )
) %>%
  select(Date, X2) %>%
  filter(!is.na(Date)) %>%
  distinct(Date, .keep_all = TRUE) %>%
  arrange(Date) %>%
  mutate(Month = lubridate::month(Date),
         Year = lubridate::year(Date),
         WY_adj = if_else(Month >= 9, Year + 1L, Year),
         Sep1 = as.Date(paste0(WY_adj - 1, "-09-01")),
         DOWY_adj = as.integer(Date - Sep1) + 1) %>%
  select(-Sep1, -Month, -Year)

# Check result
head(dayflow_x2)
tail(dayflow_x2)

# Check date range
range(dayflow_x2$Date)

# Check number of observations
nrow(dayflow_x2)

#plot to visualize
ggplot(data = dayflow_x2, aes(x = DOWY_adj, y = X2, color = WY_adj)) +
  geom_line() +
  labs(y = "Daily X2")

## ---- FMWT Indices ---- ####
FMWTindex_raw <- read_csv(here("data", "processed", "fish", "FMWTindices.csv"))

FMWTindex <- FMWTindex_raw %>%
  select(Year, "Delta Smelt", "Longfin Smelt") %>%
  arrange(Year) %>%                                   # ensure correct order
  rename(WY_adj = Year, LFSIndex_FMWT = "Longfin Smelt", DSMIndex_FMWT = "Delta Smelt") %>%     # rename column
  mutate(DSMIndex_PriorWY = dplyr::lag(DSMIndex_FMWT, n = 1),
         LFSIndex_PriorWY = dplyr::lag(LFSIndex_FMWT, n = 1))

## ---- Water Year Categories from CDEC ---- ####
wy_raw <- read_csv(here("data", "raw", "DWR_WaterYearHydrologicalClassificationIndices.csv"))

DWR_wy <- wy_raw %>%
  filter(WY >= 1958) %>%
  select(WY, `Yr-type_Sac`) %>% #Sacramento Valley WY Classification from https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST
  rename(WY_adj = WY)

DWR_wy$WY_adj <- as.numeric(DWR_wy$WY_adj)


## ---- Combine Covariate data ---- ####

# Determine the earliest and latest water years represented
min_wy <- min(DWR_wy$WY_adj, na.rm = TRUE)
max_wy <- max(DWR_wy$WY_adj, na.rm = TRUE)

# Create one row for every day across all water years
all_dates <- tibble(
  Date = seq(
    from = as.Date(paste0(min_wy - 1, "-09-01")),
    to   = as.Date(paste0(max_wy, "-08-31")),
    by   = "day"
  )
) %>%
  mutate(
    WY_adj = if_else(
      month(Date) >= 9,
      year(Date) + 1L,
      year(Date)
    ),
    DOWY_adj = as.integer(
      Date - as.Date(paste0(WY_adj - 1, "-08-31"))
    )
  )

# Add daily X2 where it exists
othercovariates <- all_dates %>%
  left_join(
    dayflow_x2 %>%
      select(Date, X2),
    by = "Date"
  ) %>%
  left_join(
    DWR_wy,
    by = "WY_adj"
  ) %>%
  left_join(
    FMWTindex,
    by = "WY_adj"
  ) %>%
  arrange(Date)

#save csv
write.csv(
  othercovariates,
  here("data", "processed", "othercovariates.csv"))
