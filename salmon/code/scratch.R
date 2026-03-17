# coverage -----
library(tidyverse)
library(here)

## raw data ----
df <- read_rds(here("salmon", "data", "processed", "raw.rds"))

df2 <- df %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date)
  ) %>%
  filter(year >= 1977, year <= 2025)

## monthly coverage ----
# create monthly coverage indicator and
# retain region_code
coverage2 <- df2 %>%
  distinct(station_code, year, month, region_code) %>%
  # 'tag' months sampled
  mutate(sampled = 1)

# create tibble of station_code and region_code for later use
sta_reg <-
  coverage2 %>%
  distinct(station_code, region_code)

## regularize ts ----

coverage2_full <-
  coverage2 %>%
  # regularize the time series w complete()
  tidyr::complete(
    station_code,
    year = 1977:2025,
    # adds missing months as needed
    month = 1:12,
    # enters '0' for months w no sampling
    fill = list(sampled = 0)
  ) %>%
  # months added bc no sampling have NAs for region_code; this references 'sta_reg' to add the correct region_code
  rows_patch(sta_reg, by = "station_code")

# (1) Lower Sacramento River,
# (5) Lower San Joaquin River,
# (2) North Delta, (3) Central Delta, (4) South Delta,
# (6) San Francisco and San Pablo Bays
reg_labs <-
  tibble(region_code = factor(1:6),
         region_label = c("Lower Sac R", "N Delta", "Cen Delta", "S Delta",
                          "Lower SJR", "SF & SP Bays"))

# convert month to labels
coverage2_full <- coverage2_full %>%
  mutate(
    month_label = factor(month, levels = 1:12, labels = month.abb)
  ) %>%
  left_join(reg_labs, by = "region_code") %>%
  mutate(
    region_label = factor(region_label,
                          levels = c("Lower Sac R",
                                     "Lower SJR",
                                     "N Delta",
                                     "Cen Delta",
                                     "S Delta",
                                     "SF & SP Bays")))



## heatmaps -----
# create calendar-style heatmap

# generate calendar-style heat map (months run horizontally, years vertically)
ggplot(coverage2_full, aes(month_label, year, fill = factor(sampled))) +
  geom_tile(color = "grey85") +
  facet_wrap(~station_code) +
  scale_fill_manual(values = c("white", "steelblue"), guide = "none") +
  labs(
    x = "Month",
    y = "Year",
    title = "Calendar-Style Heatmap of Monthly Sampling Coverage (1977–2025)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 7)
  )
# WAAAY too many sites to adequately display the info;
# figure out how to reduce the number of sites...

# 'eye-balled' the figures to identify the 'good' sites, those with better
# annual and seasonal coverage:

# 'good' sites
good <- c("AM001S", "DS002S", "GS010E", "LP003E", "MK004W", "MR010W", "MS001N",
          "SA001M", "SA004W", "SA007E", "SA008W", "SA009E", "SA010W", "SF014E",
          "SJ001S", "SJ005N", "SJ032S", "SJ041N", "SJ051E","SJ056E", "SJ058W",
          "SJ063W", "SP000W", "SP001W", "SP003E", "SR012W", "SR014W", "SR017E",
          "SR024E", "SR043W", "SR049E", "SR060E", "SR071E", "SR080E", "SR090W",
          "SR094E", "SR130E", "SR138E", "SR144W", "SS011N", "TM001N", "WD002W",
          "XC001N")

# filter to retain only 'good' sites (station_code)
coverage2_good <-
  coverage2_full %>%
  filter(station_code %in% good)

# associate each site (station_code) with the appropriate region
station_order <- coverage2_good %>%
  distinct(station_code, region_label) %>%
  arrange(region_label, station_code)

# set factor levels in the desired order
coverage2_good <- coverage2_good %>%
  mutate(
    station_code = factor(station_code,
                          levels = station_order$station_code)
  )

### good heatmaps ----
sampling_effort_g <-
  ggplot(coverage2_good,
       aes(month_label, year,
                           # Only fill if sampled is 1, otherwise set to NA
                           fill = ifelse(sampled == 1, as.character(region_code), NA))) +
  geom_tile(color = "grey85") +
  facet_wrap(~station_code) +
  # This uses a discrete color palette for regions and makes NAs white
  scale_fill_discrete(na.value = "white", name = "Region") +
  labs(
    x = "Month",
    y = "Year",
    title = "Site-Specific Sampling Effort by Region (1977–2025)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 7)
  )

ggsave("sampling_effort_g.pdf",
       path = here("salmon", "plots"))

# pivot probs -----
# seine data are in long format, including all fish spp...I want to retain that long
# format, sum CHN counts for each seine--including 0's when no CHN are caught in
# a given seine

long <-
  tibble(sample_id = c(1,1,1,2,2,3,3,3),
         temp = c(20, 20, 20, 18, 18, 19, 19, 19),
         organism_code = c("CHN", "CHN", "SACPIK", "SACPIK", "SACPIK", "WESMOS", "SACPIK", "CHN"),
         count = c(2,1,1,1,3,6,1,1)) %>%
  print()

wider <-
  long %>% pivot_wider(
  # group by and retain 'temp'
  id_cols = c(sample_id, temp),
  # name new columns
  names_from = organism_code,
  # values from match
  values_from = count,
  # value if no match
  values_fill = 0,
  # add up multiple matches
  values_fn = sum) %>%
  print()

# note that this 'long' version retains the counts (including 0's) for all spp in each sample_id
long_again <-
  wider %>%
  pivot_longer(
    cols = CHN:WESMOS,
    names_to = "organism_code",
    values_to = "count"
  ) %>%
  print()

Petes_data <- here("~/Library/CloudStorage/OneDrive-CaliforniaDepartmentofWaterResources/3-Projects/09b-first flush/data files PN")

# standardize function -----------
standardize <- function(x, center = FALSE, norm = FALSE) {
  if(!is.numeric(x)) {
    stop('ERROR: x must be numeric')
  }
  if(center) {
    x <- x - mean(x)
  }
  if(norm) {
    x <- x/sd(x)
  }
  return(x)
}

test_df <- c(10, 12, 15, 18, 20)
standardize(test_df) # leaves original unchanged bc NULL = FALSE
standardize(test_df, TRUE, TRUE) # test_df centered and normalized

test2_df <- c(10, 12, 15, 18, "wonky")
standardize(test2_df) # leaves original unchanged bc NULL = FALSE
standardize(test2_df, TRUE, TRUE)

# using 'ifelse' (genus for falcon is 'Falco')
isfalcon <-
  ifelse(speciestable$genus == 'Falco',
         'falcon',
         'not a falcon'
         )

dat <- data.frame(x = 1, y = 2)


# Load the necessary library
library(dplyr)

# Create a sample tibble
data <- tibble(
  id = 1:4,
  value = c(10, 20, 30, 40)
)

# Use transmute() to create a new variable 'value_doubled' and keep only it and 'id'
# (Note: if 'id' wasn't included in the function call, it would also be dropped)
result_transmute <- data %>%
  transmute(
    id = id,             # Keep the 'id' variable as is
    value = value * 2    # Create a new 'value' and drop the old one
  )

# Print the results
print(data)
print(result_transmute)
