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


## -- Final Longfin DF with Strata -- ####

longfin <- longfin %>%
  filter(!is.na(Stratum))

#check NA's in strata column
cat("Total remaining NAs:", sum(is.na(longfin$Stratum)), "\n")

# export longfin csv
write.csv(
  longfin,
  here("data", "processed", "fish", "longfin_clean.csv"),
  row.names = FALSE)
