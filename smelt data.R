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


# do a join and collect_data the resulting data frame
# collect_data executes the sql query, converts Date and Datetime columns to the correct format and timezone, and gives you a table
df <- left_join(surv_salvage, fish_smelt) %>%
  collect_data()

dfskt <- left_join(surv_skt, fish_smelt) %>%
  collect_data()

dffmwt <- left_join(surv_fmwt, fish_smelt) %>%
  collect_data()
# close connection to database
close_database(con)

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


#now we need turbidity and flow
load("C:/Users/rhartman/OneDrive - California Department of Water Resources/salinity control gates/SFHA_synthesis/data/dayflow_w2024.RData")
OMR = cdec_query("OMR", 41, start.date = ymd("2001-01-01"), end.date = today()) %>%
  mutate(Date = date(ObsDate)) %>%
  select(Date, Value) %>%
  rename(OMR = Value)
smelt2 = left_join(smelt2, Dayflow) %>%
  left_join(OMR)

ggplot(smelt2, aes(x = OMR, y = ROC)) + geom_point()+ geom_smooth()

#OK, what if I scale and center flow and smelt....

smelt3 = smelt2 %>%
  filter(WY < 2012, DOWY <200) %>%
  mutate(MeanSmeltC = scale(MeanSmelt),
                OUTc = scale(OUT), OMRc = scale(OMR))

ggplot(smelt3, aes(x = DOWY, y = MeanSmeltC))+
  geom_line(color = "red")+
  geom_line(aes(y = OUTc), color = "black")+
  geom_line(aes(y = OMRc), color = "blue")+
  facet_wrap(~WY)


ggplot(smelt3, aes(x = DOWY, y = MeanSmeltC))+
  geom_line(color = "black")+
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
  geom_smooth()
