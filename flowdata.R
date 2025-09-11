#we probalby want to use flow data to look at storms
#This could be dayflow again, but could use the USGS gagues for finer resolution data if needed
#Trigger for the first flush maangement aciton is XXX cfs at freeport at 50 NTU at Freeport.

library(tidyverse)
library(dataRetrieval)
library(zoo)

#flow data only available from USGS since 2007, turbidity since 2013
freparams = whatNWISdata(siteNumber = "11447650")
codes = filter(parameterCdFile, parameter_cd %in% freparams$parm_cd,
               parameter_group_nm %in% c("Physical", "Information"))


#ok, they have a bunch of different values for discharge:

#00060 Discharge, cubic feet per second Stream flow, mean. daily

#30208 Discharge, cubic meters per second

#000613 Discharge, instantaneous, cubic feet per second


#30209 Physical Discharge, instantaneous, cubic meters per second

#72137 Discharge, tidally filtered, cubic feet per second



freeport = readNWISdata(sites = "11447650", parameterCd = c("00061", "00060", "72137"),
                        startDate = "2014-05-01T00:00", endDate = "2024-05-01T12:00", service = "iv")
#probably easier to use dayflow for flow, just turbidity from sondes

load(Dayflow.RData)

#select just sac river flow
Sacflow = select(Dayflow, Date, Year, Mo, SAC) %>%
  mutate(change = SAC-lag(SAC), rollchange = rollsum(change, 2, na.pad = T)) #rate of change

ggplot(Sacflow, aes(x = Date, y = SAC))+ geom_line()


ggplot(Sacflow, aes(x = Date, y = change))+ geom_line()

#there is probalby some cut off we can use to identify storms
quantile(Sacflow$change, c(0.5, 0.8, 0.9, 0.95, 0.99), na.rm =T)
quantile(filter(Sacflow, rollchange>0)$rollchange, c(0.5, 0.8, 0.9, 0.95, 0.99), na.rm =T)

#maybe just the positive vlaues
quantile(filter(Sacflow, change>0)$change, c(0.5, 0.8, 0.9, 0.95, 0.99), na.rm =T)

#start with the 95% quantile?

Sacflow = mutate(Sacflow, Storm = case_when(change >=6500 ~ "STORM"), Storm2 = case_when(rollchange >=12000 ~ "STORM"))

#maybe I need two days with high change or something?


#limit it to more recent years, just to see what I'm doing better
SacflowRecent = filter(Sacflow, Year > 2010)
storms = filter(SacflowRecent, Storm == "STORM")
storms2 = filter(SacflowRecent, Storm2 == "STORM")
ggplot()+
  geom_line(data = SacflowRecent, aes(x = Date, y = SAC))+
  geom_line(data = filter(SacflowRecent, change >0),
            aes(x = Date,y = change), color = "blue")+
  geom_hline(yintercept = 25000, color = "red", linetype =2)+
  geom_vline(data = storms, aes(xintercept = Date), color = "chartreuse")

ggsave("plots/sacflow.png", width = 20, height =8)

#look at high rate of change as a way to identify storms

ggplot()+
  geom_line(data = SacflowRecent, aes(x = Date, y = SAC))+
  geom_line(data = filter(SacflowRecent, change >0),
            aes(x = Date,y = change), color = "blue")+
  geom_hline(yintercept = 25000, color = "red", linetype =2)+
  geom_vline(data = storms2, aes(xintercept = Date), color = "chartreuse")

