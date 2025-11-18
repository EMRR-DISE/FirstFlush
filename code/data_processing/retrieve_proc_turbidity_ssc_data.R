# Retrieve and process turbidity and suspended sediment data for the following stations:
  # USGS Sacramento at Freeport station (11447650)
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(dataRetrieval)
library(here)
library(labelled)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())


# Download Data -------------------------------------------------------------------------------

# Define end date
end_date <- "2024-09-30"

# Sacramento at Freeport USGS Station:
# Daily average suspended sediment concentration
df_sac_fpt_ssc <- read_waterdata_daily(
  monitoring_location_id = "USGS-11447650",
  parameter_code = "80154",
  statistic_id = "00003",
  skipGeometry = TRUE,
  time = paste0("../", end_date)
)

df_sac_fpt_ssc_c <- df_sac_fpt_ssc %>% select(Date = time, SSC = value)

# Instantaneous (15-min) turbidity data
df_sac_fpt_turb <- readNWISuv(
  siteNumbers = "11447650",
  parameterCd = "63680",
  tz = "Etc/GMT+8",
  endDate = end_date
)


# Process Data --------------------------------------------------------------------------------

# Calculate daily averages for turbidity
df_sac_fpt_turb_avg <- as_tibble(df_sac_fpt_turb) %>%
  mutate(Date = date(dateTime)) %>%
  summarize(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .by = Date) %>%
  mutate(
    Turbidity = coalesce(
      X_MEDIAN.TS087..YSI.model.6136_63680_00000,
      X_BGC.PROJECT...East.Fender._63680_00000
    ),
    .keep = "unused"
  )

# Combine SSC and Turbidity data
df_sac_fpt_ssc_turb <- df_sac_fpt_ssc_c %>%
  full_join(df_sac_fpt_turb_avg, by = join_by(Date)) %>%
  # Add station info
  mutate(Station = "Sacramento River at Freeport", .before = 1) %>%
  set_variable_labels(SSC = "SSC (mg/L)", Turbidity = "Turbidity (FNU)")


# Export Data ---------------------------------------------------------------------------------

# Saving single data frame as an .rds file for now. If we add more data, we'll switch to
  # an .Rdata file
df_sac_fpt_ssc_turb %>% saveRDS(here("data/processed/wq/sac_fpt_ssc_turb.rds"))

