#### Script to generate longfin_clean.csv ####

# Install libraries
{
  library(tidyverse)
  library(deltafish)
  library(sf)
  library(DBI)
  library(RSQLite)
  library(here)
  library(deltamapr)
  library(readr)
}

## ---- Pulling data from deltafish -- ####

#open and query data, first need to open a connection to the database
# dplyr required for queries below

con <- open_database()

# open our two data tables
surv <- open_survey(con)
fish <- open_fish(con)


# pull a structural summary to know what data is in each table
dbListFields(con, "survey")
dbGetQuery(con, "PRAGMA table_info(survey)")

dbGetQuery(con, "
  SELECT DISTINCT Source
  FROM survey
") #lists all survey names within the source column of the survey data table

dbListFields(con, "fish")
dbGetQuery(con, "PRAGMA table_info(fish)")


# filter for sources and taxa of interest
# this returns all survey records

# Create a lazy query for trimmed survey (aka not a df yet)
survey_trimmed <- tbl(con, "survey") %>%
  select(-Sal_bot,
         -Secchi_estimated,
         -Cable_length,
         -Tow_direction,
         -Notes_tow,
         -Notes_flowmeter) #removes unnecessary columns before joining

# Lazy query: filter fish and join
joined_lazy <- tbl(con, "fish") %>%
  filter(Taxa == "Spirinchus thaleichthys") %>%
  inner_join(survey_trimmed, by = "SampleID") #joining fish and survey data tables by SampleID

# At this point, no data is in R yet
# Only when you call collect() the query runs and brings the data into R
result <- joined_lazy %>% collect()

## ---- Add strata to fish data ---- ####
# LTM design team strata polygons via Dave's email 6/8/2026

ffspatial <- st_read(here("data", "processed", "spatial", "first_flush_spatial_data.gpkg"),
                     layer = "design_strata") #read in strata layer specifically

#check strata by mapping
ggplot() +
  geom_sf(data = WW_Delta, fill = "lightblue", color = "gray") +  # base map
  geom_sf(
    data = ffspatial,
    aes(color = Stratum),
    size = 2,
    alpha = 0.8) +
  theme(legend.position = "bottom")


#add strata to result df
all_lfs <- result %>%
  filter(!is.na(Longitude),
         !is.na(Latitude),
         !is.na(Length)) %>% # remove rows without coordinates or length
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  filter(Count >= 0, Source %in% c("DJFMP", "20mm", "Bay Study", "FMWT", "SLS")) #filter for surveys of interest

#check coordinate systems
st_crs(ffspatial)
st_crs(all_lfs)

all_lfs_utm <- st_transform(all_lfs, st_crs(ffspatial)) #now both are in EPSG:26910

# set up final longfin df with strata
longfin <- st_join(all_lfs_utm, ffspatial["Stratum"])

#double check missing coodinates and out of bounds sites
missing <- longfin[is.na(longfin$Stratum), ] #all the points that fall outside the regions, aka east of benicia and north Sac river
#longfin catches outside of stratums are DJFMP at Colusa St Park and Knightslanding, those were removed

outofbounds <- longfin %>%
  filter(Stratum == "Out of Bounds") #longfin catches included in "Out of Bounds" were from DJFMP's Elkhorn site in 1986 (n=11)


## -- Longfin DF with Strata -- ####

longfin <- longfin %>%
  filter(!is.na(Stratum))

#check NA's in strata column
cat("Total remaining NAs:", sum(is.na(longfin$Stratum)), "\n")

## -- Add lifestage -- ####
# check length frequency histogram first to make sure we only have adults (Merz et al 213, with some modifications due to gaps)

longfin_lifestage <- longfin %>%
  mutate(
    Date_parsed = ymd(Date),
    Year        = year(Date_parsed),
    Month_num   = month(Date_parsed),
    Month_label = month(Date_parsed, label = TRUE, abbr = TRUE),
    WY          = if_else(Month_num >= 10, Year + 1L, Year),
    WY_adj      = if_else(Month_num >= 9, Year + 1L, Year), #adjust water year to start 9/1
    Length_int  = as.integer(Length)
  ) %>%
  mutate(
    Lifestage = case_when(

      # --
      # Missing Length
      # --
      is.na(Length_int) ~ "Other",

      # --
      # Larvae
      # --
      Month_label == "Jan" & Length_int < 16 ~ "Larvae",
      Month_label == "Feb" & Length_int < 16 ~ "Larvae",
      Month_label == "Mar" & Length_int < 16 ~ "Larvae",
      Month_label == "Apr" & Length_int < 16 ~ "Larvae",
      Month_label == "May" & Length_int < 16 ~ "Larvae",
      Month_label == "Jun" & Length_int < 16 ~ "Larvae",

      # --
      # Juvenile (FIX: fill Jan–Feb gap)
      # --
      Month_label == "Jan" & Length_int >= 16 & Length_int <= 39 ~ "Juvenile",
      Month_label == "Feb" & Length_int >= 16 & Length_int <= 41 ~ "Juvenile",
      Month_label == "Mar" & Length_int >= 16 & Length_int <= 51 ~ "Juvenile",
      Month_label == "Apr" & Length_int >= 16 & Length_int <= 51 ~ "Juvenile",
      Month_label == "May" & Length_int >= 16 & Length_int <= 58 ~ "Juvenile",
      Month_label == "Jun" & Length_int >= 16 & Length_int <= 66 ~ "Juvenile",
      Month_label == "Jul" & Length_int < 71 ~ "Juvenile",
      Month_label == "Aug" & Length_int < 75 ~ "Juvenile",
      Month_label == "Sep" & Length_int < 80 ~ "Juvenile",
      Month_label == "Oct" & Length_int < 83 ~ "Juvenile",

      # --
      # Sub-Adult
      # --
      Month_label == "Jan" & Length_int >= 40 & Length_int <= 89 ~ "Sub-Adult",
      Month_label == "Feb" & Length_int >= 42 & Length_int <= 92 ~ "Sub-Adult",
      Month_label == "Mar" & Length_int >= 46 & Length_int <= 95 ~ "Sub-Adult",
      Month_label == "Apr" & Length_int >= 52 & Length_int <= 99 ~ "Sub-Adult",
      Month_label == "May" & Length_int >= 59 & Length_int <= 104 ~ "Sub-Adult",
      Month_label == "Jun" & Length_int >= 67 & Length_int <= 107 ~ "Sub-Adult",
      Month_label == "Nov" & Length_int < 85 ~ "Sub-Adult",
      Month_label == "Dec" & Length_int < 87 ~ "Sub-Adult",

      # --
      # Anadromous (UNCHANGED)
      # --
      Month_label == "Mar" & Length_int >= 46 & Length_int <= 95 ~ "Anadromous",
      Month_label == "Apr" & Length_int >= 52 & Length_int <= 99 ~ "Anadromous",
      Month_label == "May" & Length_int >= 59 & Length_int <= 104 ~ "Anadromous",
      Month_label == "Jun" & Length_int >= 67 & Length_int <= 107 ~ "Anadromous",
      Month_label == "Jul" & Length_int >= 71 & Length_int <= 110 ~ "Anadromous",
      Month_label == "Aug" & Length_int >= 75 & Length_int <= 113 ~ "Anadromous",
      Month_label == "Sep" & Length_int >= 80 & Length_int <= 116 ~ "Anadromous",
      Month_label == "Oct" & Length_int >= 83 & Length_int <= 119 ~ "Anadromous",
      Month_label == "Nov" & Length_int >= 85 & Length_int <= 122 ~ "Anadromous",
      Month_label == "Dec" & Length_int >= 87 & Length_int <= 122 ~ "Anadromous",

      # --
      # Adult (FIX: fill June–November gap)
      # --
      Month_label == "Jan" & Length_int > 89 ~ "Adult",
      Month_label == "Feb" & Length_int > 92 ~ "Adult",
      Month_label == "Mar" & Length_int > 95 ~ "Adult",
      Month_label == "Apr" & Length_int > 99 ~ "Adult",
      Month_label == "May" & Length_int > 104 ~ "Adult",
      Month_label == "Jun" & Length_int > 107 ~ "Adult",
      Month_label == "Jul" & Length_int > 110 ~ "Adult",
      Month_label == "Aug" & Length_int > 113 ~ "Adult",
      Month_label == "Sep" & Length_int > 116 ~ "Adult",
      Month_label == "Oct" & Length_int > 119 ~ "Adult",
      Month_label == "Nov" & Length_int > 122 ~ "Adult",
      Month_label == "Dec" & Length_int >= 87 ~ "Adult",

      # --
      # Catch-all
      # --
      TRUE ~ "Needs Fixing"
    )
  )


longfin_lifestage$Month_label <- as.factor(longfin_lifestage$Month_label)

longfin_lifestage$Month_label <- factor(longfin_lifestage$Month_label,
                                        levels = c("Sep", "Oct", "Nov", "Dec",
                                                   "Jan", "Feb", "Mar",
                                                   "Apr", "May", "Jun",
                                                   "Jul", "Aug"))
#check every fish has a life stage
needs_fixing <- longfin_lifestage %>%
  filter(Lifestage == "Needs Fixing")

# generate lfs dfs for analysis
longfin_adults <- longfin_lifestage %>%
  filter(
    Source %in% c("Bay Study", "FMWT", "DJFMP"),
    (Lifestage == c("Anadromous", "Adult", "Sub-Adult")))

longfin_20mmSLS <- longfin_lifestage %>%
  filter(Source %in% c("20mm", "SLS"),
         Month_label %in% c("Jan", "Feb", "Mar", "Apr", "May", "Jun"))

## ---- Export csv's for analysis -- ####
#all longfin data without lifestage
write_csv(
  longfin,
  here("data", "processed", "fish", "longfin_clean.csv"))

#lifestage column added and filtered for survey and adults
write_csv(
  longfin_adults,
  here("data", "processed", "fish", "longfin_adults.csv"))

#lifestage column added and filtered for survey and larvae & juveniles
write_csv(
  longfin_20mmSLS,
  here("data", "processed", "fish", "longfin_20mmSLS.csv"))

## ---- Explore Adult Catch -- ####
#histogram of adults by survey
ggplot(longfin_adults, aes(x = Length, weight = Count, fill = Source)) +
  geom_histogram(binwidth = 5) +
  facet_grid(rows = vars(Month_label), scales = "free_y") +  # Free y-axis per month
  scale_x_continuous(limits = c(0, 175)) +  # Limit x-axis to 0–150 mm
  labs(
    x = "Fork Length (mm)",
    y = "Number of Fish",
    fill = "Survey",
    title = "Adult LFS Length Frequency Histogram"
  ) +
  theme_minimal()

#map adult lfs catch by survey
ggplot() +
  geom_sf(data = WW_Delta, fill = "lightblue", color = "gray") +
  geom_sf(
    data = longfin_adults %>% filter(Source == "Bay Study"),
    aes(color = Stratum, size = Count),
    alpha = 0.8) +
  scale_size(range = c(1, 8)) +
  labs(title = "Bay Study Adult LFS Catch Distribution (1980-2022)")

ggplot() +
  geom_sf(data = WW_Delta, fill = "lightblue", color = "gray") +
  geom_sf(
    data = longfin_adults %>% filter(Source == "FMWT"),
    aes(color = Stratum, size = Count),
    alpha = 0.8) +
  scale_size(range = c(1, 8)) +
  labs(title = "FMWT Adult LFS Catch Distribution (1967-2023)")

ggplot() +
  geom_sf(data = WW_Delta, fill = "lightblue", color = "gray") +
  geom_sf(
    data = longfin_adults %>% filter(Source == "DJFMP"),
    aes(color = Stratum, size = Count),
    alpha = 0.8) +
  scale_size(range = c(1, 8)) +
  labs(title = "DJFMP Adult LFS Catch Distribution (1976-2023)")

## ---- Explore Jan-Jun 20mm & SLS Catch -- ####
#histogram of larval and juvenile lfs by survey
ggplot(longfin_20mmSLS, aes(x = Length, weight = Count, fill = Source)) +
  geom_histogram(binwidth = 5) +
  facet_grid(rows = vars(Month_label), scales = "free_y") +  # Free y-axis per month
  scale_x_continuous(limits = c(0, 50)) +  # Limit x-axis to 0–150 mm
  labs(
    x = "Fork Length (mm)",
    y = "Number of Fish",
    fill = "Survey",
    title = "Larval & Juvenile LFS Length Frequency Histogram"
  ) +
  theme_minimal()

#map larval and juvenile catch by survey
ggplot() +
  geom_sf(data = WW_Delta, fill = "lightblue", color = "gray") +
  geom_sf(
    data = longfin_20mmSLS %>% filter(Source == "20mm"),
    aes(color = Stratum, size = Count),
    alpha = 0.8) +
  scale_size(range = c(1, 8)) +
  labs(title = "20mm LFS Catch Distribution (1995-2023)")

ggplot() +
  geom_sf(data = WW_Delta, fill = "lightblue", color = "gray") +
  geom_sf(
    data = longfin_20mmSLS %>% filter(Source == "SLS"),
    aes(color = Stratum, size = Count),
    alpha = 0.8) +
  scale_size(range = c(1, 8)) +
  labs(title = "SLS LFS Catch Distribution (2009-2024)")





