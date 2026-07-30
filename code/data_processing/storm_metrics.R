#### Script to generate storm_metrics.csv for analysis ####


# Install libraries
{
  library(dplyr)
  library(tidyverse)
  library(deltafish) #package to pull all fish and survey data from SFBD
  library("deltamapr") #package with spatial data for SFBD
  library(sf)
  library(DBI) #to run summaries on data tables
  library(RSQLite)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(here)
  library(slider) #used to calculate rolling means
}

## ---- Pull Storm Data ---- ####
load(here("data", "processed", "storms", "StormData.RData"))

## ---- Prepare storm dataset and define water year ---- ####

allstorms <- Sacflow_wstorms %>%
  mutate(
    Month = lubridate::month(Date),
    WY_adj = if_else(Month >= 9, Year + 1L, Year)
  ) %>%
  filter(WY_adj >= 1958) %>%
  select(-WY, -SAC, -YOLO, -SJR, -ID)



## ---- Identify first flush day and calculate rolling averages ---- ####

# First flush = first day YSchange crosses >=6500
day_ff <- allstorms %>%
  filter(YSchange >= 6500) %>%
  group_by(WY_adj) %>%
  summarise(
    DayofFF = min(Date),
    .groups = "drop"
  ) %>%
  mutate(
    Sep1 = as.Date(paste0(WY_adj - 1, "-09-01")),
    FFDOWY_adj = as.integer(DayofFF - Sep1) + 1
  ) %>%
  select(-Sep1)

# Join first flush dates back into dataset

allstorms <- allstorms %>%
  left_join(
    day_ff,
    by = "WY_adj"
  )

# Calculate 3-day pre- and post-first flush volumes

allstorms <- allstorms %>%
  arrange(WY_adj, Date) %>%
  group_by(WY_adj) %>%
  mutate(

    `3DayAvgVol_preFF` = if (!is.na(first(DayofFF))) {
      mean(
        Volume[
          Date >= first(DayofFF) - 3 & #3 days before first flush event
            Date < first(DayofFF)
        ],
        na.rm = TRUE
      )
    } else {
      NA_real_
    },

    `3DayAvgVol_postFF` = if (!is.na(first(DayofFF))) {
      mean(
        Volume[
          Date >= first(DayofFF) &
            Date <= first(DayofFF) + 2 #day of first flugh and 2 following days
        ],
        na.rm = TRUE
      )
    } else {
      NA_real_
    }

  ) %>%
  ungroup()



## ---- Identify storm events after first flush ---- ##
# Storm = threshold crossing YSchange >=6500

storm_events <- allstorms %>%
  arrange(WY_adj, Date) %>%
  group_by(WY_adj) %>%
  mutate(
    StormStart = YSchange >= 6500 &
      lag(YSchange < 6500, default = TRUE)
  ) %>%
  filter(StormStart) %>%
  select(
    WY_adj,
    StormDate = Date
  ) %>%
  left_join(
    day_ff %>% select(WY_adj, DayofFF),
    by = "WY_adj"
  ) %>%
  mutate(
    AfterFirstFlush = StormDate > DayofFF
  ) %>%
  ungroup()

# Count storms after first flush
storm_counts <- storm_events %>%
  filter(AfterFirstFlush) %>%
  count(
    WY_adj,
    name = "NumStorms_postFF"
  )

# Add storm counts back to daily dataset
allstorms <- allstorms %>%
  left_join(
    storm_counts,
    by = "WY_adj"
  ) %>%
  mutate(
    NumStorms_postFF = tidyr::replace_na(
      NumStorms_postFF,
      0
    )
  )



## ---- Find peak flow after first flush ---- ####

first_flush_peak <- allstorms %>%
  filter(!is.na(DayofFF)) %>%
  group_by(WY_adj) %>%
  filter(
    Date >= first(DayofFF),
    Date <= first(DayofFF) + 14 #CALCULATES THE PEAK WITHIN 14 DAYS OF THE FIRST FLUSH EVENT
  ) %>%
  slice_max(
    order_by = Volume,
    n = 1,
    with_ties = FALSE
  ) %>%
  summarise(
    PeakDate = first(Date),
    PeakVolume = first(Volume),
    .groups = "drop"
  )


## ---- Create final storm metric table ---- ####
# Join peak metrics back and calculate response metrics
allstorms <- allstorms %>%
  left_join(
    first_flush_peak,
    by = "WY_adj"
  ) %>%
  mutate(

    # Days between first flush and peak
    FFtoPeak_days = as.integer(
      PeakDate - DayofFF
    ),

    # Increase from pre-first flush baseline to peak
    PreFFtoPeak_mag = PeakVolume - `3DayAvgVol_preFF`

  )

# Create final storm metrics table

storm_metrics <- allstorms %>%
  distinct(
    WY_adj, #starts Sep 1
    DayofFF, #date of first flush event for each WY_adj
    FFDOWY_adj, #day of WY_adj that first flush occurs
    `3DayAvgVol_preFF`, #rolling average volume of 3 days prior to first flush
    `3DayAvgVol_postFF`, #rolling average volume of first flush day and 2 following days
    NumStorms_postFF, #number of storms (YSchange >= 6500) after the first flush event for each WY_adj
    PeakDate, #the day volume peaks in the 14 days after each first flush event
    PeakVolume, #the volume of water on the PeakDate
    FFtoPeak_days, #lag time between first flush event and peak date
    PreFFtoPeak_mag #volumne difference between peak and rolling 3 day average before the first flush
  )

write.csv(
  storm_metrics,
  here("data", "processed", "storms", "storm_metrics.csv"),
  row.names = FALSE)


## ---- Explore storm metric plots ---- ####
#timing
ggplot(storm_metrics, aes(x = WY_adj, y = FFDOWY_adj)) +
  geom_point(size = 3) +
  geom_line() +
  theme_minimal() +
  labs(
    x = "Water Year",
    y = "Day of Water Year",
    title = "Timing of First Flush by Water Year"
  )

#magnitude
ggplot(storm_metrics, aes(
  x = WY_adj,
  y = PreFFtoPeak_mag
)) +
  geom_col() +
  theme_minimal() +
  labs(
    x = "Water Year",
    y = "Increase from Pre-FF Volume to Peak",
    title = "Magnitude of First Flush Flow Response"
  )

#lag time between first flush and peak
ggplot(storm_metrics, aes(
  x = WY_adj,
  y = FFtoPeak_days
)) +
  geom_col() +
  theme_minimal() +
  labs(
    x = "Water Year",
    y = "Days from First Flush to Peak",
    title = "First Flush Hydrograph Response Time"
  )

#storm reponses all metrics
ggplot(
  storm_metrics,
  aes(
    x = FFtoPeak_days,
    y = PreFFtoPeak_mag,
    size = PeakVolume,
    color = NumStorms_postFF,
    label = WY_adj
  )
) +
  geom_point(alpha = 0.8) +
  ggrepel::geom_text_repel(size = 3) +
  theme_minimal() +
  labs(
    x = "Days from First Flush to Peak",
    y = "First Flush Magnitude Increase",
    size = "Peak Volume",
    color = "Storms After FF",
    title = "Water Year Storm Response"
  )


# plot ff and storm events following for each WY_adj
storm_heat <- storm_events %>%
  mutate(
    WY_start = as.Date(paste0(WY_adj - 1, "-09-01")),
    StormDay_WY = as.integer(StormDate - WY_start) + 1
  )

# First flush
ff_heat <- storm_events %>%
  distinct(WY_adj, DayofFF) %>%
  mutate(
    WY_start = as.Date(paste0(WY_adj - 1, "-09-01")),
    FFDay_WY = as.integer(DayofFF - WY_start) + 1
  )

ggplot(storm_heat, aes(x = StormDay_WY, y = factor(WY_adj))) +
  geom_tile(
    aes(fill = "Storm"),
    height = 0.8) +
  geom_point(
    data = ff_heat,
    aes(x = FFDay_WY, y = factor(WY_adj)),
    color = "red",
    size = 3) +
  scale_fill_manual(values = "black") +
  theme_minimal() +
  labs(
    x = "Day of Water Year",
    y = "Water Year",
    fill = NULL,
    title = "Storm Timing Relative to First Flush",
    subtitle = "Black = storm threshold crossing | Red = first flush") +
  theme(
    legend.position = "none")



## ---- QAQC Rosie's Firststorms df to Jordan's storm_summary df -- ####
# Extract first flush dates from Firststorms
Firststorms_check <- Firststorms %>%
  filter(FirstStorm == "First Storm") %>%   # adjust if your label differs
  select(WY, FirstStorm_Date = Date)

# Compare against storm_summary
FF_comparison <- storm_summary %>%
  select(WY_adj, StormSummary_FF_Date = DayofFF) %>%
  left_join(
    Firststorms_check,
    by = c("WY_adj" = "WY")
  ) %>%
  mutate(
    Match = StormSummary_FF_Date == FirstStorm_Date
  )

# View mismatches
FF_comparison %>%
  filter(!Match)

table(FF_comparison$Match, useNA = "always")

FF_comparison %>%
  mutate(
    Date_difference_days = as.numeric(StormSummary_FF_Date - FirstStorm_Date)
  ) %>%
  arrange(abs(Date_difference_days))

FF_comparison %>%
  filter(is.na(FirstStorm_Date) | is.na(StormSummary_FF_Date))
