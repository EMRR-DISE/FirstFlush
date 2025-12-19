#Check out all the listed species in Salvage.

#Hypothesis: Salvage of covered fish species will be lower in years with an earlier and/or stronger first flush event.​

#covered species: Longfin, Chinook, Delta smelt, Green Sturgeon, White sturgeon, steelhead(?)

library(tidyverse)
library(effects)
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

#add exports
load("data/external/Dayflow.RData")

Exports = select(Dayflow, Date, Year, CVP, SWP) %>%
  mutate(Exports = CVP+SWP)

Sacflow_wstorms = Sacflow_wstorms %>%
  left_join(Exports)


Storms = group_by(Sacflow_wstorms, ID) %>%
  summarize(TotalFlow = sum(Volume), Exports = sum(Exports, na.rm =T))

FirstStorms = left_join(Firststorms, Storms)


#OK, need total number of fish salvaged per species per year.

annual_salvage = df_salvage %>%
  mutate(Year = year(Date), Month = month(Date),
         WY = case_when(Month %in% c(10,11,12) ~ Year+1, TRUE ~ Year),
                        CPUE = Count/Tow_volume,
         CPUE = Count/Tow_volume) %>%
  group_by(WY, Taxa) %>%
  summarize(CPUE = sum(CPUE, na.rm =T), Count = sum(Count)) %>%
  left_join(select(FirstStorms, Date, WY, SAC, YOLO, TotalFlow, Exports)) %>%
mutate(Month = month(Date), DOWY = case_when(Month %in% c(10,11,12) ~ yday(Date)-275, TRUE ~  yday(Date)+91),
       regime = case_when(WY < 2008 ~ "pre-2008",
                          WY >= 2008 ~ "post-2008"))


#timing of first flush versus annualsalvage
ggplot(annual_salvage, aes(x = DOWY, y = log(CPUE)))+ geom_point() + geom_smooth(method = "lm")+
  facet_wrap(~Taxa, scales = "free_y")+ ylab("Log salvage CPUE")+
  xlab("Day of water year of first flush")


ggplot(annual_salvage, aes(x = DOWY, y = log(Count)))+ geom_point() + geom_smooth(method = "lm")+
  facet_wrap(~Taxa, scales = "free_y")+
  xlab("Day of water year of first flush") +ylab("Log of annual salvage")

#fish seen in predator flush that don't count toward salvage.

#Get tables for each species from FTP site.

#figure out where deltafish is pulling salvage data from.
#strenth of first flush versus annualsalvage
ggplot(annual_salvage, aes(x = log(YOLO+SAC), y = log(Count)))+ geom_point() + geom_smooth(method = "lm")+
  facet_wrap(~Taxa, scales = "free_y")+ xlab("Strength of first flush")

#strenth of first flush versus annualsalvage
ggplot(annual_salvage, aes(x = log(TotalFlow), y = log(Count)))+ geom_point() +
  geom_smooth(method = "lm")+
  facet_wrap(~Taxa, scales = "free_y")+ xlab("log-transformed Strength of first flush (total AF)")+
  ylab("total annual salvage (log-transformed)")

#Exports versus annual salvage

ggplot(annual_salvage, aes(x = log(Exports), y = log(Count)))+ geom_point() +
  geom_smooth(method = "lm")+
  facet_wrap(~Taxa, scales = "free_y")+ xlab("log-transformed exports during first flush")

#Salvage versus expot/inflow ratio

ggplot(annual_salvage, aes(x = Exports/TotalFlow, y = log(Count)))+ geom_point() +
  geom_smooth(method = "lm")+
  facet_wrap(~Taxa, scales = "free_y")+ xlab("percentage of inflow exported")

#look at effect of exports on this relationsihp. BiOps try and limit entrainment.

#magnitude of exports during first flush driving seasonal salvage?
 #Look at % of first flush exported?

#OK, now see if there is a significant relationship if i seperate pre-2008 from post-2008 ########################
annual_salvage = mutate(annual_salvage, regime = case_when(WY < 2008 ~ "pre-2008",
                                                           WY >= 2008 ~ "post-2008"))


ggplot(annual_salvage, aes(x = log(TotalFlow), y = log(Count)))+ geom_point() +
  geom_smooth(method = "lm")+
  facet_grid(Taxa ~ regime, scales = "free_y")+ xlab("log-transformed Strength of first flush (total AF)")+
  ylab("total annual salvage (log-transformed)")

ggplot(annual_salvage, aes(x = log(Exports), y = log(Count)))+ geom_point() +
  geom_smooth(method = "lm")+
  facet_grid(Taxa ~ regime, scales = "free_y")+ xlab("log-transformed exports during first flush (total AF)")+
  ylab("total annual salvage (log-transformed)")

#now some statistics

smelt = filter(annual_salvage, Taxa == "Hypomesus transpacificus")
longfin = filter(annual_salvage, Taxa == "Spirinchus thaleichthys" )

lm1 = lm(log(Count)~ log(TotalFlow)+ WY, data = smelt)
summary(lm1)
#nope

#timing
lm1 = lm(log(Count)~DOWY+ WY, data = smelt)
summary(lm1)
#also nope

#has there been a change in strength of first flush over tiem?

ggplot(annual_salvage, aes(x = WY, y = TotalFlow)) + geom_point()+ geom_smooth()

ggplot(FirstStorms, aes(x = WY, y = log(TotalFlow))) + geom_point()+ geom_smooth(method = "lm")


ggplot(filter(FirstStorms, Exports !=0), aes(x = WY, y = Exports)) + geom_point()+ geom_smooth(method = "lm")

#has ff been changing its timing?

FirstStorms = mutate(FirstStorms, Month = month(Date), DOWY = case_when(Month %in% c(10,11,12) ~ yday(Date)-275, TRUE ~  yday(Date)+91))



ggplot(FirstStorms, aes(x = WY, y = DOWY)) + geom_point()+ geom_smooth(method = "lm")


#what was going on in 2006?

ggplot(filter(Sacflow_wstorms, WY == 2006), aes(x = Date, y = YoloSac))+ geom_point()+ geom_line()


ggplot(filter(Sacflow_wstorms, WY == 2017), aes(x = Date, y = YoloSac))+ geom_point()+ geom_line()


ggplot(filter(Sacflow_wstorms, WY == 2022), aes(x = Date, y = YoloSac))+ geom_point()+ geom_line()

ggplot(filter(Sacflow_wstorms, WY == 2017), aes(x = Date, y = YoloSac))+ geom_point()+ geom_line()

#OK, a few other things to look at
# number of storms versus total salvage #################################
#oh, can i add the FMWT index as a factor?
ScientificNames = data.frame(CommonName = c("Threadfin Shad", "American Shad", "Delta Smelt",
                                            "Longfin Smelt", "Striped Bass Age0", "Splittail"),
                             Taxa = c("Dorosoma petenense", "Alosa sapidissima", "Hypomesus transpacificus",
                                       "Spirinchus thaleichthys", "Morone saxatilis", "Pogonichthys macrolepidotus"))

FMWT = read_csv("data/raw/FMWTindices.csv")

ggplot(FMWT, aes(x = Year, y = `Threadfin Shad`)) + geom_point()+ geom_line()

FMWTsmelt =FMWT %>%
  pivot_longer(cols = c(`Threadfin Shad`:last_col()), names_to = "CommonName", values_to = "FMWTIndex") %>%
  rename(WY = Year) %>%
  left_join(ScientificNames)

annual_salvageb = left_join(annual_salvage, FMWTsmelt)


smelt = filter(annual_salvageb, Taxa == "Hypomesus transpacificus")
longfin = filter(annual_salvageb, Taxa == "Spirinchus thaleichthys" )

lm2 = lm(log(Count)~ log(TotalFlow)+ log(FMWTIndex+1), data = smelt)
summary(lm2)
plot(allEffects(lm2))
#nope

#timing
lm3 = lm(log(Count)~DOWY+ FMWTIndex, data = smelt)
summary(lm3)
#also nope

#longfin?
lm4 = lm(log(Count+1)~DOWY+ FMWTIndex, data = filter(longfin, !is.na(DOWY)))
summary(lm4)
#definitely no

lm4 = lm(log(Count+1)~log(TotalFlow)+ log(FMWTIndex), data = filter(longfin, !is.na(DOWY)))
summary(lm4)
#oh, something's here! Higher FF means less entrainment.
plot(allEffects(lm4))

#OK, now total number of storms in the winter ##################################
#Would it be number of storms? Number of storm days? or amount of storm flow?

StormSummary = Sacflow_wstorms %>%
  group_by(WY) %>%
  summarize(StormDays = length(YSStorm[which(!is.na(YSStorm))]), StormVolume = sum(Volume[which(!is.na(YSStorm))]),
            StormN = length(unique(ID[which(!is.na(ID))])), StormExports = sum(Exports[which(!is.na(YSStorm))]),
            Exports = sum(Exports, na.rm =T))

annual_salvagec = annual_salvageb %>%
  left_join(StormSummary)

smelt = filter(annual_salvagec, Taxa == "Hypomesus transpacificus")
longfin = filter(annual_salvagec, Taxa == "Spirinchus thaleichthys" )

ggplot(annual_salvagec, aes(x = log(StormVolume), y = log(Count+1))) + geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Taxa, scales = "free_y")+
  ylab("Salvage (log-transformed)")+ xlab("Total volume during storms for the whole water year")

ggplot(annual_salvagec, aes(x = log(StormExports), y = log(Count+1))) + geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Taxa, scales = "free_y")+
  ylab("Salvage (log-transformed)")+ xlab("Total exports during storms for the whole water year")


ggplot(annual_salvagec, aes(x = StormDays, y = log(Count+1))) + geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Taxa, scales = "free_y")+ xlab("number of storm days")


ggplot(annual_salvagec, aes(x = StormN, y = log(Count+1))) + geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Taxa, scales = "free_y")+ xlab("number of storm events")

#ok, now a model ###################
lm5 = lm(log(Count+1)~StormVolume+StormExports+ log(FMWTIndex+1), data = filter(longfin, !is.na(DOWY)))
summary(lm5)
#beleh
library(car)
vif(lm5)
#volume and exports are pretty correlated

lm6 = lm(log(Count+1)~StormVolume+ log(FMWTIndex+1), data = filter(longfin, !is.na(DOWY)))
summary(lm6)
#beleh
vif(lm6)
plot(allEffects(lm6))
#much better

#what about delta smelt?

lm6s = lm(log(Count+1)~StormVolume+WY+ log(FMWTIndex+1), data = filter(smelt, !is.na(DOWY)))
summary(lm6s)
#beleh
vif(lm6s)
#And WY is pretty autocorrelated with the FMWT index. Sigh.
plot(allEffects(lm6s))

ggplot(annual_salvagec, aes(x = StormVolume, y = StormExports)) + geom_smooth(method = "lm")+
  geom_point()
#nope
ggplot(annual_salvagec, aes(x = log(StormVolume), y = log(TotalFlow))) + geom_smooth(method = "lm")+
  geom_point()

#check whether FF volume or all storm volumes are better predictors for longfin salvage #############################
#get abundance indices for sturgeon and salmon

STN = read_csv("data/raw/SummerTownetCatchPerTow1959-2024.csv") %>%
  group_by(Year) %>%
  summarize(Threadfin = sum(`ThreadfinShad`), tows = n(), Stations = length(unique(StationCode)),
            ThreadfinPerTow = Threadfin/tows, AmShad = sum(`AmericanShad`), AmShadPerTow = AmShad/tows)

ggplot(STN, aes(x = Year, y = ThreadfinPerTow)) + geom_point()+ geom_line()


ggplot(STN, aes(x = Year, y = AmShadPerTow)) + geom_point()+ geom_line()


ggplot(FMWT, aes(x = Year, y = `American Shad`)) + geom_point()+ geom_line()


#seperate by project ###################################

annual_salvage_P = df_salvage %>%
  mutate(Year = year(Date), Month = month(Date),
         WY = case_when(Month %in% c(10,11,12) ~ Year+1, TRUE ~ Year),
         Facility = case_when(Station %in% c("SWP NA", "SWP New State Facility", "SWP Old State Facility") ~ "SWP",
                              Station == "CVP Federal Facility" ~ "CVP")) %>%
  group_by(WY, Taxa, Facility) %>%
  summarize(Count = sum(Count)) %>%
  left_join(select(FirstStorms, Date, WY, SAC, YOLO, TotalFlow, Exports)) %>%
  mutate(Month = month(Date), DOWY = case_when(Month %in% c(10,11,12) ~ yday(Date)-275, TRUE ~  yday(Date)+91),
         regime = case_when(WY < 2008 ~ "pre-2008",
                            WY >= 2008 ~ "post-2008"))


#timing of first flush versus annualsalvage
ggplot(annual_salvage_P, aes(x = DOWY, y = log(Count), color = Facility))+ geom_point() + geom_smooth(method = "lm")+
  facet_wrap(~Taxa, scales = "free_y")+ ylab("Log salvage CPUE")+
  xlab("Day of water year of first flush")

#strenth of first flush versus annualsalvage
ggplot(annual_salvage_P, aes(x = log(TotalFlow), y = log(Count), color = Facility))+ geom_point() +
  geom_smooth(method = "lm")+
  facet_wrap(~Taxa, scales = "free_y")+ xlab("log-transformed Strength of first flush (total AF)")+
  ylab("total annual salvage (log-transformed)")

#Exports versus annual salvage

ggplot(annual_salvage_P, aes(x = log(Exports), y = log(Count), color = Facility))+ geom_point() +
  geom_smooth(method = "lm")+
  facet_wrap(~Taxa, scales = "free_y")+ xlab("log-transformed exports during first flush")

#Salvage versus expot/inflow ratio

ggplot(annual_salvage_P, aes(x = Exports/TotalFlow, y = log(Count), color = Facility))+ geom_point() +
  geom_smooth(method = "lm")+
  facet_wrap(~Taxa, scales = "free_y")+ xlab("percentage of inflow exported")
#yuck

#add FMWT and total storms (by project) ############################

annual_salvage_p2= left_join(annual_salvage_P, FMWTsmelt) %>%
  left_join(StormSummary, by = "WY")


smelt_p = filter(annual_salvage_p2, Taxa == "Hypomesus transpacificus")
longfin_p = filter(annual_salvage_p2, Taxa == "Spirinchus thaleichthys" )

lm2p = lm(log(Count+1)~ log(TotalFlow)*Facility+ log(FMWTIndex+1), data = smelt_p)
summary(lm2p)
plot(allEffects(lm2p))
#nope

#timing
lm3p = lm(log(Count+1)~DOWY*Facility+ FMWTIndex, data = smelt_p)
summary(lm3p)
#also nope

#longfin?
lm4p = lm(log(Count+1)~DOWY*Facility+ FMWTIndex, data = filter(longfin_p, !is.na(DOWY)))
summary(lm4p)
#definitely no

lm4p = lm(log(Count+1)~log(TotalFlow)*Facility+ log(FMWTIndex), data = filter(longfin_p, !is.na(DOWY)))
summary(lm4p)
#oh, something's here! Higher FF means less entrainment.
plot(allEffects(lm4p))

#total volumen in all the storms

lm4p2 = lm(log(Count+1)~log(StormVolume)*Facility+ log(FMWTIndex), data = filter(longfin_p, !is.na(DOWY)))
summary(lm4p2)
#total volume of all storms is a lot more significant
#still no interaction with facility
plot(allEffects(lm4p2))

