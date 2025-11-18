#sooplankton data

library(zooper)
library(tidyverse)

FFzoops = Zoopsynther(Data_type = "Community", Sources = c("EMP", "FMWT"), Size_class = "Meso",
                      Years = c(1995:2024))

load("StormData.RData")

FFzoop2 = left_join(FFzoops, Sacflow_wstorms)

FFpseudo = filter(FFzoop2, Genus == "Pseudodiaptomus", Lifestage == "Adult") %>%
  group_by(SampleID, Date, SAC, ID, FirstDay, LastDay, FirstStorm, Latitude, Longitude, Year, YSStorm, Month) %>%
  summarize(CPUE = sum(CPUE))

ggplot(filter(FFpseudo, Month %in% c(11,12,1,2,3)), aes(x = YSStorm, y = CPUE)) + geom_boxplot()+
  facet_wrap(~Month)

ggplot(filter(FFpseudo, Month %in% c(11,12,1,2,3)), aes(x = YSStorm, y = log(CPUE+1))) + geom_boxplot()+
  facet_wrap(~Month)
