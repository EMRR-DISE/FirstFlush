#### Script to generate prepost_storm_wq.csv for analysis ####

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
  library(readr)
  library(qs2) #if getting error, make sure Rtools is pointing in the right spot
  library(purrr)
}


## -- Pull in Dave's continuous water quality data ---- ####

#data
continuouswq <- qd_read(here("data", "processed", "wq", "cwq_data_dv_all.qdata"))

#metadata
continuouswqmeta <- readRDS(here("data", "processed", "wq", "cwq_station_metadata.rds"))

ryi_ryf_stratum <- continuouswqmeta %>%
  filter(station_abbr == "RYI") %>%   # or "RYF" -- same value if they match
  pull(stratum)

continuouswqmeta <- continuouswqmeta %>%
  bind_rows(
    tibble(
      station_abbr = "RYI-RYF",
      station_name = "Rio Vista/Cache Slough (RYI-RYF combined)",  # adjust to something meaningful
      stratum = ryi_ryf_stratum,
      latitude = NA_real_,   # fill in if you have/want coordinates for this combined station
      longitude = NA_real_
    )
  )

#add strata to wq data
continuouswq <- continuouswq %>%
  left_join(select(continuouswqmeta, station_abbr, station_name, stratum, latitude, longitude),
            by = "station_abbr", relationship = "many-to-many")

## -- Run the entire "storm_metrics" script ---- ####
source(here("code", "data_processing", "storm_metrics.R")) #storm_summary not needed

## -- WQ for analysis ---- ####
wq <- continuouswq %>%
  filter(date >= "1973-08-15") %>% #dates that match for storm and lfs metrics
  mutate(
    Month = lubridate::month(date),
    Year = lubridate::year(date),
    WY_adj = if_else(Month >= 9, Year + 1L, Year)
  )


vars <- c("water_temp", "sp_cond", "turbidity", "discharge_tf", "velocity_tf")
safe_mean <- function(x) if (length(x) == 0 || all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

## -- Step 1: collapse wq to stratum-day level ---- ####
wq_stratum_daily <- wq %>%
  group_by(stratum, date) %>%
  summarise(across(all_of(vars), safe_mean), .groups = "drop")

## -- Step 2: one row per storm event x stratum ---- ####
strata <- wq_stratum_daily %>% distinct(stratum)

storm_wq <- storm_events %>%
  cross_join(strata)

## -- Step 3: pre/post storm 3-day averages ---- ####
compute_storm_windows <- function(stratum_i, StormDate_i) {
  pre <- wq_stratum_daily %>%
    filter(stratum == stratum_i, date >= StormDate_i - 3, date < StormDate_i) %>%
    summarise(across(all_of(vars), safe_mean))
  names(pre) <- paste0(names(pre), "_preStorm3DayAvg")

  post <- wq_stratum_daily %>%
    filter(stratum == stratum_i, date >= StormDate_i, date <= StormDate_i + 2) %>%
    summarise(across(all_of(vars), safe_mean))
  names(post) <- paste0(names(post), "_postStorm3DayAvg")

  bind_cols(pre, post)
}

storm_window_results <- pmap_dfr(
  list(storm_wq$stratum, storm_wq$StormDate),
  compute_storm_windows
)

storm_wq <- bind_cols(storm_wq, storm_window_results)

storm_wq <- storm_wq %>%
  filter(StormDate >= "1973-08-15") %>%
  mutate(
    Sep1 = as.Date(paste0(WY_adj - 1, "-09-01")),
    StormDOWY_adj = as.integer(StormDate - Sep1) + 1
  )


## ---- WQ pre and post storm plots ---- ####
storm_wq_prepost_long <- storm_wq %>%
  mutate(storm_id = paste(WY_adj, StormDate, stratum, sep = "_")) %>%
  pivot_longer(
    cols = matches("_(preStorm3DayAvg|postStorm3DayAvg)$"),
    names_to = c("Parameter", "Period"),
    names_pattern = "(.*)_(preStorm3DayAvg|postStorm3DayAvg)$",
    values_to = "Value"
  ) %>%
  mutate(
    Period = recode(Period, preStorm3DayAvg = "Pre", postStorm3DayAvg = "Post"),
    Period = factor(Period, levels = c("Pre", "Post"))
  )

#FF storms -- all strata combined
storm_wq_prepost_long %>%
  filter(AfterFirstFlush == FALSE) %>%
  ggplot(aes(x = Period, y = Value, group = storm_id, color = WY_adj)) +
  geom_line(alpha = 0.4) +
  geom_point() +
  facet_wrap(~ Parameter, scales = "free_y") +
  labs(title = "3 Day Rolling Average Pre and Post First Flush Events - all strata")

#FF storms -- confluence only
storm_wq_prepost_long %>%
  filter(AfterFirstFlush == FALSE, stratum == "Confluence") %>%
  ggplot(aes(x = Period, y = Value, group = storm_id, color = WY_adj)) +
  geom_line(alpha = 0.4) +
  geom_point() +
  facet_wrap(~ Parameter, scales = "free_y") +
  labs(title = "3 Day Rolling Average Pre and Post First Flush Events - Confluence only")

#FF storms -- faceted by parameter and strata
storm_wq_prepost_long %>%
  filter(AfterFirstFlush == FALSE) %>%
  ggplot(aes(x = Period, y = Value, group = storm_id, color = WY_adj)) +
  geom_line(alpha = 0.4) +
  geom_point() +
  facet_grid(Parameter ~ stratum, scales = "free_y") +
  labs(title = "3 Day Rolling Average Pre and Post First Flush Events")

## Storms after FF -- all strata combined
storm_wq_prepost_long %>%
  filter(AfterFirstFlush == TRUE) %>%
  ggplot(aes(x = Period, y = Value, group = storm_id, color = WY_adj)) +
  geom_line(alpha = 0.4) +
  geom_point() +
  facet_wrap(~ Parameter, scales = "free_y") +
  labs(title = "3 Day Rolling Average Pre and Post Storm Events After First Flush - all strata")


#Storms after FF -- faceted by parameter and strata
storm_wq_prepost_long %>%
  filter(AfterFirstFlush == TRUE) %>%
  ggplot(aes(x = Period, y = Value, group = storm_id, color = WY_adj)) +
  geom_line(alpha = 0.4) +
  geom_point() +
  facet_grid(Parameter ~ stratum, scales = "free_y") +
  labs(title = "3 Day Rolling Average Pre and Post Storm Events After First Flush")

#All storms for 2017
storm_wq_prepost_long %>%
  filter(WY_adj == 2017) %>%
  ggplot(aes(x = StormDOWY_adj, y = Value, shape = Period)) +
  geom_line(aes(group = 1), color = "grey50", alpha = 0.7) +
  geom_point(aes(color = AfterFirstFlush), size = 2) +
  facet_grid(Parameter ~ stratum, scales = "free_y") +
  labs(title = "WY 2017: Storm Timing vs Parameter Values - Confluence only")


## ---- Calculate change in wq values pre and post storm ---- ####
prepost_storm_wq <- storm_wq_prepost_long %>%
  select(
    WY_adj, StormDate,DayofFF, AfterFirstFlush, stratum,
    StormDOWY_adj, Parameter, Period, Value
  ) %>%
  pivot_wider(
    names_from = Period,
    values_from = Value
  ) %>%
  mutate(
    Change = Post - Pre
  ) %>%
  select(WY_adj, StormDate, DayofFF, AfterFirstFlush, stratum, StormDOWY_adj, Parameter, Change
  ) %>%
  pivot_wider(
    names_from = Parameter,
    values_from = Change
  ) %>%
  rename(
    "discharge_tf_Δ" = "discharge_tf",
    "sp_cond_Δ" = "sp_cond",
    "turbidity_Δ" = "turbidity",
    "velocity_tf_Δ" = "velocity_tf",
    "water_temp_Δ" = "water_temp"
  )


write_csv(prepost_storm_wq, here("data", "processed", "storms", "prepost_storm_wq.csv"))

## ---- Plots for change in wq pre and post storms ---- ####
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)

## ---- Pivot prepost_storm_wq to long format for faceted plotting ---- ####
# prepost_storm_wq is wide (one Δ column per parameter). Pivot so Parameter
# becomes a column value, same structure as storm_wq_prepost_long was before,
# just with a single Change value per storm/stratum/parameter instead of a
# Pre/Post pair.
prepost_storm_wq_long <- prepost_storm_wq %>%
  pivot_longer(
    cols = ends_with("_\u0394"),
    names_to = "Parameter",
    values_to = "Change"
  ) %>%
  mutate(Parameter = str_remove(Parameter, "_\u0394$"))

## ---- Plot: change by stratum, faceted by parameter ---- ####
prepost_storm_wq_long %>%
  filter(AfterFirstFlush == TRUE) %>%
  ggplot(aes(x = stratum, y = Change, color = factor(WY_adj))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +  # zero reference: no change
  geom_jitter(width = 0.15, alpha = 0.6, size = 2) +
  geom_boxplot(aes(color = NULL), alpha = 0.15, outlier.shape = NA, width = 0.5) +
  facet_wrap(~ Parameter, scales = "free_y") +
  labs(
    title = "Change (Post − Pre) in WQ Parameters by Stratum for First Flush Storms",
    x = "Stratum", y = "Change (3-Day Avg Post − Pre)", color = "Water Year"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
