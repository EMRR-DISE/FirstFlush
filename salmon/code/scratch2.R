library(dplyr)
library(lubridate)
library(readr)

# Load data
df <- read_rds(here("salmon", "data", "processed", "raw.rds"))

# Convert date field
df <- df %>%
  mutate(date = as.Date(date),
         year = year(date),
         month = month(date))

# Limit to target time series
# rows: 956,435
df_filtered <- df %>%
  filter(year >= 1977, year <= 2025)

# Determine how many months were sampled per year
monthly_coverage <- df_filtered %>%
  distinct(id_code, station_code, year, month) %>%
  group_by(id_code, station_code, year) %>%
  summarise(
    months_sampled = n(),
    .groups = "drop"
  )

# Identify years where all 12 months were sampled
full_years <- monthly_coverage %>%
  filter(months_sampled >= 10)

# Count how many such years exist for each sampling effort
effort_summary <- full_years %>%
  group_by(id_code, station_code) %>%
  summarise(
    years_full_monthly_sampling = n(),
    .groups = "drop"
  )

# Keep only those with >= 30 years
regular_sampling_efforts <- effort_summary %>%
  filter(years_full_monthly_sampling >= 20)

# View results
regular_sampling_efforts

# regularize -----
# attempt to regularize 'coverage' for both month and year...
# test with...

df <-
  tibble(station_code = c(rep("AM001S", 3), rep("SR010E", 3)),
         year = c(1976, rep(1977,5)),
         month = c(5, 6, 12, 2, 4, 5),
         region = c(rep(2,3),rep(4,3)),
         sampled = rep(1,6))

df_regular <- df %>%
  complete(
    station_code,
    year = min(year):max(year),
    month = 1:12,
    fill = list(sampled = 0)
  )
