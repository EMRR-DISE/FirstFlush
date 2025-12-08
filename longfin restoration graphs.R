#get FRP fish data for Brian - time series of long fin smelt before and after restoration

library(tidyverse)

#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.269.5

#fish data

FRPfish  <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/269/5/ecee55c9b666a5f83045f274be8f6406")


#site visit data
sitevisit  <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/269/5/1c10140e26b4aa5c53593dfe9eefdd0a")

#filter to just Tule Red, Ryer, Grizzly Bay

TR = filter(sitevisit, Location %in% c("Grizzly Bay", "Tule Red", "Ryer Island" )) %>%
  left_join(FRPfish) %>%
  mutate(CPUE = Count/effort) %>%
  filter(GearTypeAbbreviation != "HOOK")

#add in zeros

TR_z = pivot_wider(TR, id_cols = c(VisitNo, Location, Date, Temp, SC, DO, Turbidity,
                                   Chlorophyll, LatitudeStart, LongitudeStart, effort),
                   names_from = CommonName, values_from = CPUE, values_fill = 0, values_fn = sum)

ggplot(TR_z, aes(x = Date, y = `Longfin Smelt`)) + geom_point()+ geom_line() +
  facet_wrap(~Location)+ geom_vline(xintercept = ymd("2019-10-15"), color = "red", linetype =2)+
  theme_bw()

#maybe just annual total? Then it's easier to add in 2024?

TR_z2 = TR_z %>%
  mutate(Year = year(Date)) %>%
  group_by(Location, Year) %>%
  summarize(Longfin = mean(`Longfin Smelt`, na.rm =T))

#longfin from 2024 which I got from the FRP report
Longfin2024 = data.frame(Location = c("Tule Red", "Grizzly Bay", "Ryer Island"),
                         Longfin = c(36, 108, 6), Year = 2024)

#longfin from the FRP data from 2024
Longfin2024_frp = read_csv("data/raw/2024.fish.FRP.EDI_2025-08-21.csv") %>%
  filter(year(Date) == 2024, Location %in% c("Tule Red", "Grizzly Bay", "Ryer Island"), GearTypeAbbreviation != "HOOK") %>%
  mutate(CPUE = Count/effort, Year = year(Date)) %>%
  pivot_wider(id_cols = c(VisitNo, Year,Location, Date,  LatitudeStart, LongitudeStart, effort),
              names_from = CommonName, values_from = CPUE, values_fill = 0, values_fn = sum)%>%
  group_by(Location, Year) %>%
  summarize(Longfin = mean(`Longfin Smelt`, na.rm =T))

#let's try catch per 10000 cubpic meters

TR_z2z = bind_rows(TR_z2, Longfin2024_frp) %>%
  mutate(CPUE2 = Longfin*10000, loglong = log(CPUE2+1))

ggplot(TR_z2z, aes(x = Year, y = Longfin)) + geom_point()+ geom_line() +
  facet_wrap(~Location)+ geom_vline(xintercept = 2019, color = "red", linetype =2)+
  theme_bw()+ ylab("Catch of Longfin Smelt per unit effort")


ggplot(TR_z2z, aes(x = Year, y = loglong)) + geom_point()+ geom_line() +
  facet_wrap(~Location)+ geom_vline(xintercept = 2019, color = "red", linetype =2)+
  theme_bw()+ ylab("Catch of Longfin Smelt per unit effort (log-transformed)")



#what about EDSM data from Grizzly Bay?
EDSM = read_csv("data/raw/EDSM_KDTR.csv") %>%
  filter(Subregion == "Grizzly Bay")

#add zeros
EDSM0s = mutate(EDSM, CPUE = Count/Volume) %>%
  pivot_wider(id_cols = c(Subregion, StationCode, Latitude, Longitude, SampleDate, Volume),
                     names_from = OrganismCode, values_fill = 0, values_from = CPUE, values_fn = sum) %>%
  select(Subregion, StationCode, Latitude, Longitude, SampleDate, Volume, LFS) %>%
  mutate(Year = year(SampleDate))

ggplot(EDSM0s, aes(x = SampleDate, y =  LFS))+ geom_point()

ggplot(EDSM0s, aes(x = as.factor(Year), y =  LFS))+ geom_boxplot()

#mean CPUE

EDSMmean = group_by(EDSM0s, Year) %>%
  summarise(LFS = mean(LFS, na.rm =T)) %>%
  mutate(Location = "EDSM Grizzly Bay", CPUE2 = LFS*10000, loglong = log(CPUE2+1))

TR_z3 = bind_rows(TR_z2z, EDSMmean) %>%
  mutate(Location = case_match(Location, "Grizzly Bay" ~ "FRP Grizzly Bay",
                               "Ryer Island" ~ "FRP Ryer Island",
                               "Tule Red" ~ "FRP Tule Red",
                               .default = Location))

ggplot(TR_z3, aes(x = Year, y =  loglong))+ geom_point()+ geom_line()+
  geom_vline(xintercept = 2019, color = "red", linetype =2)+ facet_wrap(~Location, nrow =1)+
  ylab("Mean catch of longfin smelt per 10000m3, \nlog-transformed")+ theme_bw()

ggsave("plots/TuleRedLongfin.tiff", width =8, height =5)

#export FRP data for record#export FRP datlocation()a for record
Longfin2024_frp_forreport = read_csv("data/raw/2024.fish.FRP.EDI_2025-08-21.csv") %>%
  filter(year(Date) == 2024, Location %in% c("Tule Red", "Grizzly Bay", "Ryer Island"), GearTypeAbbreviation != "HOOK") %>%
  mutate(CPUE = Count/effort*10000, Year = year(Date)) %>%
  pivot_wider(id_cols = c(SampleID_frp, Year,Location, Date,  LatitudeStart, LongitudeStart, effort, GearTypeAbbreviation),
              names_from = CommonName, values_from = CPUE, values_fill = 0, values_fn = sum) %>%
  select(SampleID_frp, LatitudeStart, LongitudeStart, GearTypeAbbreviation, effort, Location, Date, `Longfin Smelt`) %>%
  rename(Longfin_CPUE = `Longfin Smelt`)

write.csv(Longfin2024_frp_forreport, "data/Longfin_2024_frp.csv", row.names = F)
