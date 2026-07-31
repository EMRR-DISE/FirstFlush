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



## ---- Find peak flow after first flush (event-bounded window) ---- ####
#
# Replaces the fixed 14-day window. The peak search now stops at
# whichever comes first:
#   (a) the day before the NEXT storm event begins (so a later,
#       unrelated storm can't be misattributed as the first-flush peak), or
#   (b) 7 days after DayofFF (a cap, in case the next storm is far off)
#
# Requires `storm_events` (already built in your script) to know when
# the next storm starts.

# Step 1: for each WY_adj, find the date of the next storm AFTER first flush
next_storm_after_ff <- storm_events %>%
  filter(AfterFirstFlush) %>%
  group_by(WY_adj) %>%
  summarise(
    NextStormDate = min(StormDate),
    .groups = "drop"
  )

# Step 2: compute the window end date for each WY_adj
peak_window_bounds <- day_ff %>%
  select(WY_adj, DayofFF) %>%
  left_join(
    next_storm_after_ff,
    by = "WY_adj"
  ) %>%
  mutate(
    # cap end: 7 days after first flush
    CapEnd = DayofFF + 7,
    # if a next storm exists, stop the day before it starts;
    # otherwise fall back to the 7-day cap
    WindowEnd = if_else(
      !is.na(NextStormDate),
      pmin(NextStormDate - 1, CapEnd),
      CapEnd
    )
  )

# Step 3: find the peak within [DayofFF, WindowEnd] for each WY_adj
first_flush_peak <- allstorms %>%
  select(WY_adj, Date, Volume) %>%
  inner_join(
    peak_window_bounds %>% select(WY_adj, DayofFF, WindowEnd),
    by = "WY_adj"
  ) %>%
  filter(
    Date >= DayofFF,
    Date <= WindowEnd
  ) %>%
  group_by(WY_adj) %>%
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

## ---- Diagnostic v2: Validate the event-bounded peak window ---- ####
#
# Run this AFTER the new "Find peak flow after first flush (event-bounded
# window)" block, so `first_flush_peak`, `peak_window_bounds`, `day_ff`,
# and `storm_events` all reflect the new logic.

library(dplyr)
library(tidyr)
library(purrr)

## ---- 0. Edge case check: does any window collapse to <= 1 day? ---- ####
# WindowEnd should always be strictly after DayofFF. If NextStormDate is
# DayofFF or DayofFF+1, WindowEnd could end up <= DayofFF, which would
# produce an empty or degenerate search window.

cat("---- Edge case check: WindowEnd <= DayofFF ----\n")
bad_windows <- peak_window_bounds %>%
  filter(WindowEnd <= DayofFF)

if (nrow(bad_windows) == 0) {
  cat("None found — all windows are valid.\n")
} else {
  cat(nrow(bad_windows), "year(s) with a degenerate window:\n")
  print(bad_windows, n = Inf)
}

# Also flag very short windows (1-2 days), since those may still be too
# tight to reliably capture a peak even if not technically degenerate
cat("\n---- Windows of 2 days or less ----\n")
peak_window_bounds %>%
  mutate(WindowLength = as.integer(WindowEnd - DayofFF) + 1) %>%
  filter(WindowLength <= 2) %>%
  select(WY_adj, DayofFF, NextStormDate, WindowEnd, WindowLength) %>%
  print(n = Inf)


## ---- 1. Peak attribution check (same as before, using new peaks) ---- ####

storm_check <- storm_events %>%
  select(WY_adj, StormDate, DayofFF) %>%
  left_join(
    first_flush_peak,
    by = "WY_adj"
  ) %>%
  filter(!is.na(PeakDate)) %>%
  mutate(
    DaysFromStormToPeak = abs(as.integer(PeakDate - StormDate))
  ) %>%
  group_by(WY_adj) %>%
  slice_min(
    order_by = DaysFromStormToPeak,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  rename(NearestStormDate = StormDate) %>%
  mutate(
    PeakIsFirstFlushStorm = NearestStormDate == DayofFF,
    FFtoPeak_days = as.integer(PeakDate - DayofFF)
  )

cat("\n---- Peak attribution summary (event-bounded window) ----\n")
storm_check %>%
  count(PeakIsFirstFlushStorm) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()


## ---- 2. Compare new event-bounded peak vs. old fixed windows ---- ####

find_peak_fixed <- function(data, window_days) {
  data %>%
    filter(!is.na(DayofFF)) %>%
    group_by(WY_adj) %>%
    filter(
      Date >= first(DayofFF),
      Date <= first(DayofFF) + window_days
    ) %>%
    slice_max(order_by = Volume, n = 1, with_ties = FALSE) %>%
    summarise(
      PeakDate = first(Date),
      PeakVolume = first(Volume),
      .groups = "drop"
    )
}

windows <- c(7, 14)

peak_by_window <- map_dfr(
  windows,
  ~ find_peak_fixed(allstorms, .x) %>% mutate(window_days = .x)
)

peak_wide <- peak_by_window %>%
  select(WY_adj, window_days, PeakDate, PeakVolume) %>%
  pivot_wider(
    names_from = window_days,
    values_from = c(PeakDate, PeakVolume),
    names_glue = "{.value}_{window_days}d"
  ) %>%
  left_join(
    first_flush_peak %>%
      rename(PeakDate_eventbound = PeakDate, PeakVolume_eventbound = PeakVolume),
    by = "WY_adj"
  )

cat("\n---- Peak comparison: event-bounded vs. fixed 7d/14d windows ----\n")
print(peak_wide, n = Inf)

# Flag years where the new event-bounded peak differs from the old
# fixed 14-day peak (this is what actually changed for your analysis)
changed_from_14d <- peak_wide %>%
  filter(PeakDate_eventbound != PeakDate_14d) %>%
  mutate(
    volume_diff_from_14d = PeakVolume_eventbound - PeakVolume_14d,
    days_diff_from_14d = as.integer(PeakDate_eventbound - PeakDate_14d)
  ) %>%
  select(
    WY_adj, PeakDate_14d, PeakDate_eventbound,
    volume_diff_from_14d, days_diff_from_14d
  )

cat("\n---- Years where the new window changed the peak vs. old 14-day window ----\n")
print(changed_from_14d, n = Inf)

cat(
  "\nOf", nrow(peak_wide), "years,", nrow(changed_from_14d),
  "(", round(100 * nrow(changed_from_14d) / nrow(peak_wide), 1),
  "%) changed peak date/volume under the new event-bounded window.\n"
)

# Specifically re-check the previously worst-offending years
flagged_years <- c(2006, 1970, 2023, 1975, 1967, 1969, 1986, 2024)

cat("\n---- Previously flagged years: old vs. new peak ----\n")
peak_wide %>%
  filter(WY_adj %in% flagged_years) %>%
  select(
    WY_adj, PeakDate_14d, PeakVolume_14d,
    PeakDate_eventbound, PeakVolume_eventbound
  ) %>%
  arrange(WY_adj) %>%
  print(n = Inf)


## ---- Diagnostic v3: Investigate remaining peak mismatches ---- ####
#
# Run this after diagnose_peak_window_v2.R (needs `storm_check`,
# `storm_events`, `peak_window_bounds`, `first_flush_peak`, `day_ff`).
#
# Goal: for the years still flagged as PeakIsFirstFlushStorm == FALSE,
# figure out whether the mismatch is (a) a false alarm - the peak is
# still part of the first-flush storm's own multi-day rise, but a
# different storm (before or after) happens to sit closer in time to
# PeakDate - or (b) a genuine case of a different storm's peak leaking
# into the window.

library(dplyr)

## ---- 1. Pull the mismatched years ---- ####

mismatched_years <- storm_check %>%
  filter(!PeakIsFirstFlushStorm) %>%
  pull(WY_adj)

cat("---- Mismatched years ----\n")
print(mismatched_years)


## ---- 2. Full context for each mismatched year ---- ####
# Show every storm_events entry for these years (before AND after
# DayofFF), plus the window bounds and the peak that was found.

mismatch_context <- storm_events %>%
  filter(WY_adj %in% mismatched_years) %>%
  select(WY_adj, StormDate, DayofFF, AfterFirstFlush) %>%
  left_join(
    peak_window_bounds %>% select(WY_adj, WindowEnd),
    by = "WY_adj"
  ) %>%
  left_join(
    first_flush_peak,
    by = "WY_adj"
  ) %>%
  mutate(
    StormToDayofFF = as.integer(StormDate - DayofFF),
    StormToPeak = as.integer(StormDate - PeakDate)
  ) %>%
  arrange(WY_adj, StormDate)

cat("\n---- All storm events for mismatched years, relative to DayofFF and PeakDate ----\n")
print(mismatch_context, n = Inf)


## ---- 3. Classify each mismatch ---- ####
# For each mismatched year, identify the nearest storm to PeakDate and
# whether it falls BEFORE DayofFF (likely a false alarm - peak still
# belongs to the first-flush storm's rise) or AFTER DayofFF but before
# WindowEnd (a genuine different-storm leak, which shouldn't happen
# given the new window logic, and is worth a closer look) or is simply
# DayofFF itself with a multi-day lag (also a false alarm).

mismatch_classified <- storm_check %>%
  filter(!PeakIsFirstFlushStorm) %>%
  select(WY_adj, DayofFF, PeakDate, FFtoPeak_days, NearestStormDate, DaysFromStormToPeak) %>%
  left_join(
    peak_window_bounds %>% select(WY_adj, WindowEnd),
    by = "WY_adj"
  ) %>%
  mutate(
    Classification = case_when(
      NearestStormDate < DayofFF ~ "Nearest storm is BEFORE DayofFF (likely false alarm - peak is part of FF storm's rise)",
      NearestStormDate > DayofFF & NearestStormDate <= WindowEnd ~ "Nearest storm is AFTER DayofFF but within window (unexpected - investigate)",
      NearestStormDate > WindowEnd ~ "Nearest storm is AFTER the window closed (false alarm - just the closest labeled storm, not a leak)",
      TRUE ~ "Other"
    )
  ) %>%
  arrange(WY_adj)

cat("\n---- Classification of each mismatch ----\n")
print(mismatch_classified, n = Inf)

cat("\n---- Classification summary ----\n")
mismatch_classified %>%
  count(Classification) %>%
  print(n = Inf)


## ---- 4. Save for review ---- ####

write.csv(mismatch_context, here("data", "processed", "storms", "mismatch_context.csv"), row.names = FALSE)
write.csv(mismatch_classified, here("data", "processed", "storms", "mismatch_classified.csv"), row.names = FALSE)

cat("\nSaved: mismatch_context.csv, mismatch_classified.csv\n")

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
