#get FRP fish data for Brian - time series of long fin smelt before and after restoration

library(tidyverse)

#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.269.5

#fish data

FRPfish  <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/269/5/ecee55c9b666a5f83045f274be8f6406")


#site visit data
sitevisit  <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/269/5/1c10140e26b4aa5c53593dfe9eefdd0a")

#filter to just Tule Red, Ryer, Grizzly Bay

TR = filter(sitevisit, Location %in% c("Grizzly Bay", "Tule Red", "Ryer Island" )) %>%
  left_join(FRPfish)

#add in zeros

TR_z = pivot_wider(TR, id_cols = c(VisitNo, Location, Date, Temp, SC, DO, Turbidity,
                                   Chlorophyll, LatitudeStart, LongitudeStart, effort),
                   names_from = CommonName, values_from = Count, values_fill = 0, values_fn = sum)

ggplot(TR_z, aes(x = Date, y = `Longfin Smelt`)) + geom_point()+ geom_line() +
  facet_wrap(~Location)+ geom_vline(xintercept = ymd("2019-10-15"), color = "red", linetype =2)+
  theme_bw()

#maybe just annual total? Then it's easier to add in 2024?

TR_z2 = TR_z %>%
  mutate(Year = year(Date)) %>%
  group_by(Location, Year) %>%
  summarize(Longfin = sum(`Longfin Smelt`, na.rm =T))

Longfin2024 = data.frame(Location = c("Tule Red", "Grizzly Bay", "Ryer Island"),
                         Longfin = c(36, 108, 6), Year = 2024)

TR_z2z = bind_rows(TR_z2, Longfin2024)

ggplot(TR_z2z, aes(x = Year, y = Longfin)) + geom_point()+ geom_line() +
  facet_wrap(~Location)+ geom_vline(xintercept = 2019, color = "red", linetype =2)+
  theme_bw()+ ylab("Total catch of Longfin Smelt per year")
