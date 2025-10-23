#play with some data to look at first flush stuff


library(tidyverse)
library(discretewq)
library(deltafish)
library(cder)

######pull smelt data ############
create_fish_db()
con <- open_database()

# open our two data tables
surv <- open_survey(con)
fish <- open_fish(con)

surv_skt = surv %>%
  filter(Source == "SKT" & Date > "1990-01-01") %>%
  select(SampleID, Date, Source, Station, Sal_surf, Temp_surf,
         TurbidityNTU, Secchi, Tow_volume, Notes_tow,
         Latitude, Longitude)


surv_fmwt = surv %>%
  filter(Source == "FMWT" & Date > "1990-01-01") %>%
  select(SampleID, Date, Source, Station, Sal_surf,
         Temp_surf, TurbidityNTU, Secchi, Tow_volume, Notes_tow,
         Latitude, Longitude)


surv_salvage <- surv %>%
  filter(Source == "Salvage" & Date > "1990-01-01") %>%
  select(SampleID, Date, Source, Station, Sal_surf, Temp_surf, TurbidityNTU, Secchi, Tow_volume, Notes_tow)

fish_smelt <- fish %>%
  filter(Taxa %in% c("Hypomesus transpacificus"))

fish_shad =  fish %>%
  filter(Taxa %in% c("Alosa sapidissima"))


fish_splitts =  fish %>%
  filter(Taxa %in% c("Pogonichthys macrolepidotus"))

fish_stripers = fish %>%
  filter(Taxa %in% "Morone saxatilis")

# do a join and collect_data the resulting data frame
# collect_data executes the sql query, converts Date and Datetime columns to the correct format and timezone, and gives you a table
df <- left_join(surv_salvage, fish_smelt) %>%
  collect_data()

dfsplits <- left_join(surv_salvage, fish_splitts) %>%
  collect_data()
dfshad = left_join(surv_salvage, fish_shad) %>%
  collect_data()

dfstripers = left_join(surv_salvage, fish_stripers) %>%
  collect_data()

dfskt <- left_join(surv_skt, fish_smelt) %>%
  collect_data()

dffmwt <- left_join(surv_fmwt, fish_smelt) %>%
  collect_data()
# close connection to database
close_database(con)

save(df, dfsplits, dfshad, dfstripers, dfskt, dffmwt, file = "data/processed/fish/fishdata.RData")

###########plot salvage#############
ggplot(df, aes(x = Date, y = Count, color = Station)) + geom_line()

smelt = group_by(df, Date, Station) %>%
  summarise(smelt = mean(Count)) %>% #i'm not sure if this should be sum or mean.
mutate(DOY = yday(Date),
       WY = case_when(month(Date) %in% c(10,11,12) ~ year(Date) +1,
                      TRUE ~ year(Date)),
       DOWY = case_when(month(Date) %in% c(10,11,12) ~ DOY -273,
                        TRUE ~ DOY + 92)) %>%
  group_by(WY, Station) %>%
  mutate(CumSmelt = cumsum(smelt) ) %>%
  ungroup() %>%
  filter(Station != "SWP NA")

ggplot(smelt, aes(x = Date, y = smelt, color = Station)) + geom_line()
ggplot(smelt, aes(x = DOWY, y = CumSmelt, color = as.factor(WY))) + geom_line() +
  facet_wrap(~Station)

########recerate grimaldo et al plot
#Grimaldo et al used 8-day running average (combined)
library(zoo)
smelt2 = df%>%
  filter(Station != "SWP NA") %>%
group_by(Date) %>%
  summarise(smelt = sum(Count)) %>% #i'm not sure if this should be sum or mean.
  mutate(DOY = yday(Date),
         WY = case_when(month(Date) %in% c(10,11,12) ~ year(Date) +1,
                        TRUE ~ year(Date)),
         DOWY = case_when(month(Date) %in% c(10,11,12) ~ DOY -273,
                          TRUE ~ DOY + 92)) %>%
  group_by(WY) %>%
  mutate(MeanSmelt = rollmean(smelt, 8, na.pad = T),
         ROC = MeanSmelt - lag(MeanSmelt)) %>% #calculate rate of change to find start of peak?
  ungroup()


shad2 = dfshad%>%
  filter(Station != "SWP NA") %>%
  group_by(Date) %>%
  summarise(smelt = sum(Count)) %>% #i'm not sure if this should be sum or mean.
  mutate(DOY = yday(Date),
         WY = case_when(month(Date) %in% c(10,11,12) ~ year(Date) +1,
                        TRUE ~ year(Date)),
         DOWY = case_when(month(Date) %in% c(10,11,12) ~ DOY -273,
                          TRUE ~ DOY + 92)) %>%
  group_by(WY) %>%
  mutate(MeanSmelt = rollmean(smelt, 8, na.pad = T),
         ROC = MeanSmelt - lag(MeanSmelt)) %>% #calculate rate of change to find start of peak?
  ungroup()

stripers2 = dfstripers%>%
  filter(Station != "SWP NA") %>%
  group_by(Date) %>%
  summarise(smelt = sum(Count)) %>% #i'm not sure if this should be sum or mean.
  mutate(DOY = yday(Date),
         WY = case_when(month(Date) %in% c(10,11,12) ~ year(Date) +1,
                        TRUE ~ year(Date)),
         DOWY = case_when(month(Date) %in% c(10,11,12) ~ DOY -273,
                          TRUE ~ DOY + 92)) %>%
  group_by(WY) %>%
  mutate(MeanSmelt = rollmean(smelt, 8, na.pad = T),
         ROC = MeanSmelt - lag(MeanSmelt)) %>% #calculate rate of change to find start of peak?
  ungroup() %>%
  left_join(Dayflow) %>%
  left_join(OMR)

splits2 = dfsplits%>%
  filter(Station != "SWP NA") %>%
  group_by(Date) %>%
  summarise(smelt = sum(Count)) %>% #i'm not sure if this should be sum or mean.
  mutate(DOY = yday(Date),
         WY = case_when(month(Date) %in% c(10,11,12) ~ year(Date) +1,
                        TRUE ~ year(Date)),
         DOWY = case_when(month(Date) %in% c(10,11,12) ~ DOY -273,
                          TRUE ~ DOY + 92)) %>%
  group_by(WY) %>%
  mutate(MeanSmelt = rollmean(smelt, 8, na.pad = T),
         ROC = MeanSmelt - lag(MeanSmelt)) %>% #calculate rate of change to find start of peak?
  ungroup() %>%
  left_join(Dayflow) %>%
  left_join(OMR)


#now we need turbidity and flow
load("data/external/Dayflow.RData")
OMR = cdec_query("OMR", 41, start.date = ymd("2001-01-01"), end.date = today()) %>%
  mutate(Date = date(ObsDate)) %>%
  select(Date, Value) %>%
  rename(OMR = Value)
smelt2 = left_join(smelt2, Dayflow) %>%
  left_join(OMR)

shad2 = left_join(shad2, Dayflow) %>%
  left_join(OMR)

ggplot(smelt2, aes(x = OMR, y = ROC)) + geom_point()+ geom_smooth()

#OK, what if I scale and center flow and smelt....

smelt3 = smelt2 %>%
  filter(WY < 2012, DOWY <200) %>%
  mutate(MeanSmeltC = scale(MeanSmelt),
                OUTc = scale(OUT), OMRc = scale(OMR),
         Taxa = "Delta Smelt")

shad3 = shad2 %>%
  filter(WY < 2012, DOWY <200) %>%
  mutate(MeanSmeltC = scale(MeanSmelt),
         OUTc = scale(OUT), OMRc = scale(OMR),
         Taxa = "American Shad")

stripers3 = stripers2 %>%
  filter(WY < 2012, DOWY <200) %>%
  mutate(MeanSmeltC = scale(MeanSmelt),
         OUTc = scale(OUT), OMRc = scale(OMR),
         Taxa = "Striped bass")

splits3 = splits2 %>%
  filter(WY < 2012, DOWY <200) %>%
  mutate(MeanSmeltC = scale(MeanSmelt),
         OUTc = scale(OUT), OMRc = scale(OMR),
         Taxa = "Splittail")

fish3 = bind_rows(smelt3, shad3, stripers3, splits3)

ggplot(fish3, aes(x = DOWY, y = MeanSmeltC, color = Taxa))+
  geom_line()+
  geom_line(aes(y = OUTc), color = "black")+
  facet_wrap(~WY, scales = "free_y")


ggplot(smelt3, aes(x = DOWY, y = MeanSmeltC))+
  geom_line(color = "black")+
  geom_line(aes(y = OUTc), color = "blue")+
  facet_wrap(~WY, scales = "free_y")

ggplot(smelt3, aes(x = DOWY, y = MeanSmeltC))+
  geom_line(color = "black")+
  geom_line(data = shad3, color = "green")+
  geom_line(data = stripers3, color = "orange")+
  geom_line(aes(y = OUTc), color = "blue")+
  facet_wrap(~WY, scales = "free_y")



#########smelt distribution###############
#i guess I'll put the FMWT and SKT data together and calculate the center of distribution
#wieghted by CPUE.

smeltsktfmwt = bind_rows(dfskt, dffmwt) %>%
  filter(Date > min(dfskt$Date), !is.na(Latitude)) %>%
  mutate(DOY = yday(Date),
         WY = case_when(month(Date) %in% c(10,11,12) ~ year(Date) +1,
                        TRUE ~ year(Date)),
         DOWY = case_when(month(Date) %in% c(10,11,12) ~ DOY -273,
                          TRUE ~ DOY + 92))

#we probably want bi-weekly average distribution or something
#I guess I hve to do something about the zeros?
smeltdist = smeltsktfmwt %>%
    group_by(Date, DOWY, WY) %>%
  summarize(MeanLat = sum(Latitude*Count, na.rm =T)/sum(Count, na.rm =T),
            MeanLong = sum(Longitude*Count, na.rm =T)/sum(Count, na.rm =T)) %>%
  filter(!is.nan(MeanLat)) %>%
  mutate(Month = month(Date), Fortnight = round(DOWY/14)) %>%
  group_by(WY, Fortnight) %>%
  mutate(MeanLat = mean(MeanLat, na.rm =T), MeanLong = mean(MeanLong, na.rm =T)) %>%
  ungroup() %>%
  left_join(Dayflow)

ggplot(smeltdist, aes(y = MeanLat, x = MeanLong, color = DOWY)) + geom_point()+
  facet_wrap(~WY)+
  scale_color_viridis_c()

ggplot(smeltdist, aes(x = log(OUT), y = MeanLong, color = WY)) + geom_point()+
  scale_color_viridis_b()+
  geom_smooth(method = "lm")+
  ylab("Center of Distribution (longitude)")+
  xlab("Outflow (log-transformed)")
