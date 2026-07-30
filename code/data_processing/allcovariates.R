#### Combine storm, wq, and other covariate data for fish analysis ####
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

## ---- Bring in all storm, wq, and covariate data ---- ####
storm_metrics     <- read_csv(here("data", "processed", "storms", "storm_metrics.csv"))
prepost_storm_wq  <- read_csv(here("data", "processed", "storms", "prepost_storm_wq.csv"))
othercovariates   <- read_csv(here("data", "processed", "othercovariates.csv"))

## ---- Prep each source table before joining ---- ####

# storm_metrics: drop DayofFF since it's already in prepost_storm_wq
# (avoids .x/.y duplicate suffixes on join)
storm_metrics_clean <- storm_metrics %>%
  select(-DayofFF)

# othercovariates splits into two grains that need to be joined differently:
#   - annual_covariates: one value per WY_adj (confirmed via n_distinct check below)
#     -> join by WY_adj only, so gaps in the daily calendar don't cost real annual values
#   - daily_covariates: genuinely date-specific (X2)
#     -> join by exact date match
# Dropping "...1" (leftover row-index column) and DOWY_adj (already correct in
# prepost_storm_wq once dates align) since they're not needed going forward.
annual_covariates <- othercovariates %>%
  distinct(WY_adj, `Yr-type_Sac`, DSMIndex_FMWT, LFSIndex_FMWT, DSMIndex_PriorWY, LFSIndex_PriorWY)

daily_covariates <- othercovariates %>%
  select(Date, X2)

## ---- QAQC: confirm join assumptions BEFORE joining ---- ####

# 1. Date classes must match or the join silently fails/misbehaves
stopifnot(class(prepost_storm_wq$StormDate) == "Date")
stopifnot(class(othercovariates$Date) == "Date")

# 2. Every StormDate should exist somewhere in othercovariates$Date
#    (0 = full coverage, confirmed)
n_unmatched_dates <- sum(!prepost_storm_wq$StormDate %in% othercovariates$Date)
stopifnot(n_unmatched_dates == 0)

# 3. Confirm DSMIndex_FMWT (and by extension the other "annual" columns) really is
#    one distinct value per water year -- i.e. safe to collapse with distinct()
#    without silently dropping real within-year variation
dup_annual_values <- othercovariates %>%
  group_by(WY_adj) %>%
  summarise(n_distinct_DSMIndex = n_distinct(DSMIndex_FMWT), .groups = "drop") %>%
  filter(n_distinct_DSMIndex > 1)
stopifnot(nrow(dup_annual_values) == 0)

# 4. Confirm distinct() didn't create duplicate WY_adj rows in annual_covariates
#    (would cause a many-to-many join blowup)
dup_annual_rows <- annual_covariates %>% count(WY_adj) %>% filter(n > 1)
stopifnot(nrow(dup_annual_rows) == 0)

## ---- Combine data ---- ####
allcovariates <- prepost_storm_wq %>%
  left_join(storm_metrics_clean, by = "WY_adj") %>%
  left_join(annual_covariates,   by = "WY_adj") %>%
  left_join(daily_covariates,    by = c("StormDate" = "Date")) %>%
  rename(Yr_type_Sac = `Yr-type_Sac`)

## ---- QAQC: confirm join results AFTER joining ---- ####

# No column name collisions from the joins
stopifnot(anyDuplicated(colnames(allcovariates)) == 0)

# Every WY_adj in covariates should have a match in annual_covariates
# (empty result = no join gaps)
missing_wy <- setdiff(unique(allcovariates$WY_adj), unique(annual_covariates$WY_adj))
stopifnot(length(missing_wy) == 0)

# NA counts by column -- expected pattern:
#   X2: ~1300 NAs, real missingness inherited from source data (X2 has gaps pre-dating
#       consistent monitoring)
#   DSMIndex_FMWT / LFSIndex_FMWT: ~135 NAs, entirely from water years before the FMWT
#       survey began (1958-1966) plus known survey-gap years (1974, 1979) -- confirmed
#       below, not a join artifact
na_summary <- allcovariates %>%
  summarise(across(c(X2, DSMIndex_FMWT, LFSIndex_FMWT), ~ sum(is.na(.))))
print(na_summary)

# Which water years are driving the DSMIndex_FMWT/LFSIndex_FMWT NAs
years_missing_fmwt <- annual_covariates %>%
  filter(is.na(DSMIndex_FMWT)) %>%
  pull(WY_adj)
print(years_missing_fmwt)

# Share of rows affected, for context
pct_missing_fmwt <- mean(is.na(allcovariates$DSMIndex_FMWT))
print(pct_missing_fmwt)

## Export csv
write_csv(allcovariates, here("data", "processed", "allcovariates.csv"))
