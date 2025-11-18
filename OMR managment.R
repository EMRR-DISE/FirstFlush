#turbidity and flow stuff

library(cder)
library(tidyverse)
library(deltafish)

#OBI, holland, OSJ
#Compare to OMR, Outflow, smelt catch

turb = cdec_query(c("OBI", "HOL", "OSJ"),  c(221), start.date = ymd("2008-01-01"), end.date = ymd("2024-12-30"))

load("Dayflow.RData")

OMRstuff = cdec_query(c("OMR"),  c(41), start.date = ymd("2008-01-01"), end.date = ymd("2024-12-30"))

turbdaily = mutate(turb, Date = date(ObsDate)) %>%
  filter(Value >0) %>%
  group_by(Date, StationID) %>%
  summarize(Turbidity = mean(Value, na.rm =T))

OMRstuff_t = mutate(OMRstuff, Date = date(ObsDate)) %>%
  rename(OMR = Value) %>%
  select(-StationID) %>%
  left_join(turbdaily)

OMRstuff_t  %>%
  filter(month(Date) %in% c(10,11,12,1,2,3,4,5))%>%
  ggplot(aes(x = OMR, y = Turbidity)) + geom_point(aes(color = StationID))



#see when teh OMR flows versus secchi triggers from 20mm history?
#when the average Secchi disk depth in the most recent survey is > 1 meter.
#The Secchi disk depth shall be calculated as the average measurement from all
#sampled stations on the San Joaquin River upstream of Jersey Point and stations
#south of the lower San Joaquin River.If the average Secchi disk depth in the most
#recent survey is < 1 meter, Permittee shall, in coordination with Reclamation,
#adjust south Delta exports to achieve a 7-day average of the OMR index no more negative
#than -3,500 cfs until the average Secchi disk depth has increased to > 1 meter.

con <- open_database()

# open our two data tables
surv <- open_survey(con)
fish <- open_fish(con)

surv_20mm = surv %>%
  filter(Source == "20mm") %>%
  select(SampleID, Date, Source, Station, Sal_surf, Temp_surf,
         TurbidityNTU, Secchi, Tow_volume, Notes_tow,
         Latitude, Longitude)

fish_smelt <- fish %>%
  filter(Taxa %in% c("Hypomesus transpacificus"))

df20mm <- left_join(surv_20mm, fish_smelt) %>%
  collect_data()

close_database(con)

save(df20mm, file = "data/20mmsmelt.RData")

Target_stations = c("809", "812", "815", "906", "910", "912", "901", "902", "914", "915", "918")


df20mmsub = filter(df20mm, Station %in% Target_stations) %>%
  mutate(Survey = case_when(mday(Date) <= 15 ~ paste(year(Date), month(Date), 1),
                            mday(Date) > 15 ~ paste(year(Date), month(Date), 2)),
         Month = month(Date), Year = year(Date)) %>%
  group_by(Year, Month, Survey) %>%
  summarize(Secchi = mean(Secchi, na.rm =T), nsmelt = sum(Count), cpuesmelt = mean(Count/Tow_volume)) %>%
  mutate(Trigger = case_when(Secchi >100 ~ F, Secchi <= 100 ~ T))

ggplot(df20mmsub, aes(x = Secchi, y = nsmelt))   + geom_point(aes(color = Year))+ geom_smooth()

ggplot(df20mmsub, aes(x = log(Secchi), y = log(nsmelt+1)))   + geom_point(aes(color = Year))+ geom_smooth()

OMRmeans = mutate(OMRstuff) %>%
  mutate(Date = date(ObsDate),
         Survey = case_when(mday(Date) <= 15 ~ paste(year(Date), month(Date), 1),
                            mday(Date) > 15 ~ paste(year(Date), month(Date), 2)),
         Month = month(Date), Year = year(Date)) %>%
  group_by(Year, Month, Survey) %>%
  summarize(OMR = mean(Value, na.rm =T))

df20mmsub2 = left_join(df20mmsub, OMRmeans) %>%
  mutate(OMRbin = case_when(OMR >0 ~ "positive",
                            OMR < 0 & OMR > -2000 ~ "0:-2000",
                            OMR < -2000 & OMR >= -3000 ~ "-2000:-3000",
                            OMR < -3000 & OMR >= -4500 ~ "-3000:-4500",
                            OMR < -4500 & OMR >= -5500 ~ "-4500:-5500",
                            TRUE ~ "Really negative"),
         OMRbin = factor(OMRbin, levels = c("positive", "0:-2000", "-2000:-3000", "-3000:-4500",
                                            "-4500:-5500", "Really negative")))


ggplot(df20mmsub2, aes(x = OMRbin, y = Secchi)) + geom_boxplot()

ggplot(filter(df20mmsub2, !is.na(OMR)), aes(x = log(Secchi), y = log(nsmelt+1)))   +
  geom_point(aes(color = OMR))+ geom_smooth()

ggplot(filter(df20mmsub2, !is.na(OMR)), aes(x = OMR, y = nsmelt )) +
  geom_point(aes(color = Trigger))+ geom_smooth()


ggplot(df20mmsub2, aes(x = Trigger, y = nsmelt )) +
  geom_boxplot()

#how does secchi disk respond to OMR levles
#periods where we have -3500 OMR
#periods where we have -5000 OMR
#relative change in secchi between surveys.
#how does secchi change bsed on when OMR is?

df20mmsub3 = df20mmsub2 %>%
  group_by(Year) %>%
  mutate(leadSecchi = lead(Secchi), diffSecchi = leadSecchi-Secchi) %>%
  ungroup()

ggplot(df20mmsub3, aes(x = OMRbin, y =  Secchi)) + geom_boxplot() + geom_point()
ggplot(df20mmsub3, aes(x = OMRbin, y =  diffSecchi)) + geom_boxplot() + geom_point()
ggplot(df20mmsub3, aes(x = OMRbin, y =  leadSecchi)) + geom_boxplot()

seclm = lm(diffSecchi ~ OMRbin, data = df20mmsub3)
summary(seclm)
anova(seclm)
plot(allEffects(seclm))

library(lme4)
library(lmerTest)
library(effects)

seclm2 = lm(diffSecchi ~ OMRbin + Month, data = df20mmsub3)
summary(seclm2)


seclm3 = lm(diffSecchi ~ OMRbin, data = filter(df20mmsub3, OMRbin %in% c("-3000:-4500", "-4500:-5500")))
summary(seclm3)
anova(seclm3)
plot(seclm3)

df2008 = filter(df, year(Date) >2007) %>%
  mutate(Survey = case_when(mday(Date) <= 15 ~ paste(year(Date), month(Date), 1),
                            mday(Date) > 15 ~ paste(year(Date), month(Date), 2)),
         Month = month(Date), Year = year(Date)) %>%
  group_by(Survey, Month, Year) %>%
  summarize(Smelt = sum(Count))

df20mmsub4 = left_join(df20mmsub3, df2008)

ggplot(df20mmsub4, aes(x = OMRbin, y = Smelt)) + geom_boxplot()

ggplot(OMRstuff, aes(x = ObsDate, y = Value)) + geom_line()+
  geom_hline(yintercept = -5000, color = "red")
