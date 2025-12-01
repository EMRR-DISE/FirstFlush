#Check out all the listed species in Salvage.

#Hypothesis: Salvage of covered fish species will be lower in years with an earlier and/or stronger first flush event.​

#covered species: Longfin, Chinook, Delta smelt, Green Sturgeon, White sturgeon, steelhead(?)

library(tidyverse)
library(deltafish)

  ######pull Salsvage data ############
# create_fish_db()
# con <- open_database()
#
# # open our two data tables
# surv <- open_survey(con)
# fish <- open_fish(con)
#
#
# surv_salvage <- surv %>%
#   filter(Source == "Salvage" & Date > "1990-01-01") %>%
#   select(SampleID, Date, Source, Station, Sal_surf, Temp_surf, TurbidityNTU, Secchi, Tow_volume, Notes_tow)
#
# fish_listed <- fish %>%
#   filter(Taxa %in% c("Hypomesus transpacificus", "Oncorhynchus tshawytscha", "Spirinchus thaleichthys",
#                      "Acipenser medirostris", "Acipenser transmontanus", "Oncorhynchus mykiss"))
#
#
#
# # do a join and collect_data the resulting data frame
# # collect_data executes the sql query, converts Date and Datetime columns to the correct format and timezone, and gives you a table
# df_salvage <- left_join(surv_salvage, fish_listed) %>%
#   collect_data()
#
# # close connection to database
# close_database(con)

#save(df_salvage, file = "data/raw/salvage.RData")
load("data/raw/salvage.RData")
load("data/processed/storms/StormData.RData")


#brian recommended total AF from the first storm. How do I do that?

#I have CFS, multiply it by time of storm.

#to converst CFS to AF, for an approximate result, divide the volume value by 43560
#then mluitply by number of seconds in a day - 86400

Sacflow_wstorms = mutate(Sacflow_wstorms, Volume = YoloSac/43560*86400)

Storms = group_by(Sacflow_wstorms, ID) %>%
  summarize(TotalFlow = sum(Volume))

FirstStorms = left_join(Firststorms, Storms)


#OK, need total number of fish salvaged per species per year.

annual_salvage = df_salvage %>%
  mutate(Year = year(Date), Month = month(Date),
         WY = case_when(Month %in% c(10,11,12) ~ Year+1, TRUE ~ Year),
                        CPUE = Count/Tow_volume,
         CPUE = Count/Tow_volume) %>%
  group_by(WY, Taxa) %>%
  summarize(CPUE = sum(CPUE, na.rm =T), Count = sum(Count)) %>%
  left_join(select(FirstStorms, Date, WY, SAC, YOLO, TotalFlow)) %>%
mutate(Month = month(Date), DOWY = case_when(Month %in% c(10,11,12) ~ yday(Date)-275, TRUE ~  yday(Date)+91))


#timing of first flush versus annualsalvage
ggplot(annual_salvage, aes(x = DOWY, y = log(CPUE)))+ geom_point() + geom_smooth(method = "lm")+
  facet_wrap(~Taxa, scales = "free_y")+ ylab("Log salvage CPUE")+
  xlab("Day of water year of first flush")


ggplot(annual_salvage, aes(x = DOWY, y = log(Count)))+ geom_point() + geom_smooth(method = "lm")+
  facet_wrap(~Taxa, scales = "free_y")+
  xlab("Day of water year of first flush")

#fish seen in predator flush that don't count toward salvage.

#Get tables for each species from FTP site.

#figure out where deltafish is pulling salvage data from.
#strenth of first flush versus annualsalvage
ggplot(annual_salvage, aes(x = log(YOLO+SAC), y = log(Count)))+ geom_point() + geom_smooth(method = "lm")+
  facet_wrap(~Taxa, scales = "free_y")+ xlab("Strength of first flush")

#strenth of first flush versus annualsalvage
ggplot(annual_salvage, aes(x = log(TotalFlow), y = log(Count)))+ geom_point() +
  geom_smooth(method = "lm")+
  facet_wrap(~Taxa, scales = "free_y")+ xlab("log-transformed Strength of first flush (total AF)")

#look at effect of exports on this relationsihp. BiOps try and limit entrainment.

#magnitude of exports during first flush driving seasonal salvage?
 #Look at % of first flush exported?
