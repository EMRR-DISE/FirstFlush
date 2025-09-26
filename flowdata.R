#we probalby want to use flow data to look at storms
#This could be dayflow again, but could use the USGS gagues for finer resolution data if needed
#Trigger for the first flush maangement aciton is XXX cfs at freeport at 50 NTU at Freeport.

library(tidyverse)
library(dataRetrieval)
library(zoo)
library(cder)

#flow data only available from USGS since 2007, turbidity since 2013
freparams = whatNWISdata(siteNumber = "11447650")
codes = filter(parameterCdFile, parameter_cd %in% filter(freparams, count_nu>60)$parm_cd,
               parameter_group_nm %in% c("Physical", "Sediment"))


#turbidity related codes:

#00076 - Turbidity NTU

#70300 - TDS
#80154 - Susbended sediment - mg/L
#80155 - suspended sediment discharge - tons per day




freeport = readNWISdata(sites = "11447650", parameterCd = c("00076", "70300"),
                        startDate = "2014-05-01T00:00", endDate = "2024-05-01T12:00", service = "iv")


freeport2 = readNWISdata(sites = "11447650", parameterCd = c("80154", "80155"),
                        startDate = "2014-05-01T00:00", endDate = "2024-05-01T12:00", service = "dv")

#UGH! Not working
#i just downloaded it via the website
FTPss = read_csv("data/FTPdaily.csv")
summary(FTPss)
#that only pulled back to 1996, but I'll figure out how to get the rest later


FTPtest = read_tsv("https://waterdata.usgs.gov/nwis/dv?cb_80154=on&format=rdb&site_no=11447650&legacy=&referred_module=sw&period=&begin_date=1956-09-18&end_date=2023-09-18https://waterdata.usgs.gov/nwis/dv?cb_80154=on&format=rdb&site_no=11447650&legacy=&referred_module=sw&period=&begin_date=1956-09-18&end_date=2023-09-18")

FPT = cdec_query("FPT", c(27, 221, 63), start.date = ymd("2014-05-01"), end.date = today())
#cdec doesn't have the suspended sediment

FTPssx = rename(FTPss, Date = time, SS = value)

#calculate daily averages
FPTave  = FPT %>%
  filter(Value >0) %>%
  mutate(Date = date(ObsDate)) %>%
  group_by(Date, SensorNumber, SensorType, SensorUnits, Duration) %>%
  summarize(Turbidity = mean(Value, na.rm =T))

FTPssT = left_join(FTPssx, FPTave)

ggplot(FTPssT, aes(x = log(SS), y = log(Turbidity))) + geom_point()+ geom_smooth()

ggplot(FTPssT, aes(x = SS, y = Turbidity)) + geom_point()+ geom_smooth()

#start with Dayflow for flow
 load("Dayflow.RData")

#select just sac river flow
Sacflow = select(Dayflow, Date, Year, Mo, SAC, YOLO, SJR) %>%
  mutate(RollSac = rollmean(SAC, 3, na.pad = T),
    change = SAC-lag(SAC), SJRchange =  SJR-lag(SJR),
    YoloSac = YOLO+SAC, YSchange = YoloSac-lag(YoloSac),
    rollchange = rollsum(change, 2, na.pad = T))%>% #rate of change
left_join(FTPssT) %>%
  mutate(RollTurb = rollmean(Turbidity, 3, na.pad = T))

#take a quick look
ggplot(Sacflow, aes(x = Date, y = SAC))+ geom_line()


ggplot(Sacflow, aes(x = Date, y = change))+ geom_line()
#flow versus turbidity

ggplot(Sacflow, aes(x = SAC, y = Turbidity)) + geom_point()+ geom_smooth()

ggplot(Sacflow, aes(x = SAC, y = log(Turbidity))) + geom_point()+ geom_smooth()


ggplot(Sacflow, aes(x = SAC, y = SS)) + geom_point()+ geom_smooth()


ggplot(Sacflow, aes(x = SAC, y = log(SS))) + geom_point()+ geom_smooth()

#those are all pretty gross.


#there is probalby some cut off we can use to identify storms
quantile(Sacflow$change, c(0.5, 0.8, 0.9, 0.95, 0.99), na.rm =T)
quantile(filter(Sacflow, rollchange>0)$rollchange, c(0.5, 0.8, 0.9, 0.95, 0.99), na.rm =T)

#maybe just the positive vlaues
quantile(filter(Sacflow, change>0)$change, c(0.5, 0.8, 0.9, 0.95, 0.99), na.rm =T)
#95th is 6500
quantile(filter(Sacflow, SJRchange>0)$SJRchange, c(0.5, 0.8, 0.9, 0.95, 0.99), na.rm =T)
#an order of manitude lower
quantile(filter(Sacflow, YSchange>0)$YSchange, c(0.5, 0.8, 0.9, 0.95, 0.99), na.rm =T)
#now we're at 10212 for 95th percentile, which i think is too high

#start with the 95% quantile?

Sacflow = mutate(Sacflow, Storm = case_when(change >=6500 ~ "STORM"),
                 Storm2 = case_when(rollchange >=12000 ~ "STORM"),
                 FF = case_when(RollSac > 25000 ~ "STORM"),
                 FFwTurb = case_when(RollSac > 2500 & RollTurb >50 ~ "STORM"),
                 YSstorm = case_when(YSchange >= 6500 ~ "STORM"),
                 SJRstorm = case_when(SJRchange >= 650 ~ "STORM"))

#maybe I need two days with high change or something?


#limit it to more recent years, just to see what I'm doing better
SacflowRecent = filter(Sacflow, Year > 2010)
storms = filter(SacflowRecent, Storm == "STORM")
storms2 = filter(SacflowRecent, Storm2 == "STORM")
stormsFF = filter(SacflowRecent, FF == "STORM")
stormsSJR = filter(SacflowRecent, SJRstorm == "STORM")
stormswYolo= filter(SacflowRecent, YSstorm == "STORM")
stormsFFturb= filter(SacflowRecent, FFwTurb == "STORM")



ggplot()+
  geom_line(data = SacflowRecent, aes(x = Date, y = SJR))+
  geom_line(data = filter(SacflowRecent, change >0),
            aes(x = Date,y = SJRchange), color = "blue")+
 # geom_hline(yintercept = 25000, color = "red", linetype =2)+
  geom_vline(data = stormsSJR, aes(xintercept = Date), color = "orange")+
  coord_cartesian(xlim = c(ymd("2016-10-01"), ymd("2017-06-01")))+
  theme_bw()


ggplot(data = SacflowRecent, aes(x = SJR, y = SAC))+
  geom_point()+ geom_smooth()+ geom_abline(slope =1, intercept =0, color ="red")




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

#stom options
# 1. Sac over 25,000 - ITP requirement
# 2. Rate of change > 6500
# 3. Two-day change >12000
# time interval - no more than one storme in a 7-day period or soething. Or if ROC is constanatnish it's all one storm.

ggplot()+
  geom_line(data = SacflowRecent, aes(x = Date, y = RollSac))+
  geom_line(data = filter(SacflowRecent, change >0),
            aes(x = Date,y = change), color = "blue")+
  geom_hline(yintercept = 25000, color = "red", linetype =2)+
  geom_vline(data = storms2, aes(xintercept = Date), color = "chartreuse")

ggplot()+
  geom_line(data = SacflowRecent, aes(x = Date, y = RollSac))+
  geom_line(data = filter(SacflowRecent, change >0),
            aes(x = Date,y = change), color = "blue")+
  geom_hline(yintercept = 25000, color = "red", linetype =2)+
  geom_vline(data = storms2, aes(xintercept = Date), color = "chartreuse")+
  coord_cartesian(xlim = c(ymd("2022-10-01"), ymd("2023-06-01")))

ggplot()+
  geom_line(data = SacflowRecent, aes(x = Date, y = RollSac))+
  geom_line(data = filter(SacflowRecent, change >0),
            aes(x = Date,y = change), color = "blue")+
  geom_hline(yintercept = 25000, color = "red", linetype =2)+
  geom_vline(data = storms2, aes(xintercept = Date), color = "chartreuse")+
  coord_cartesian(xlim = c(ymd("2018-10-01"), ymd("2019-06-01")))

ggplot()+
  geom_line(data = SacflowRecent, aes(x = Date, y = RollSac))+
  geom_line(data = filter(SacflowRecent, change >0),
            aes(x = Date,y = change), color = "blue")+
  geom_hline(yintercept = 25000, color = "red", linetype =2)+
  geom_vline(data = storms2, aes(xintercept = Date), color = "chartreuse")+
  coord_cartesian(xlim = c(ymd("2016-10-01"), ymd("2017-06-01")))


ggplot()+
  geom_line(data = SacflowRecent, aes(x = Date, y = RollSac))+
  geom_line(data = filter(SacflowRecent, change >0),
            aes(x = Date,y = change), color = "blue")+
  geom_hline(yintercept = 25000, color = "red", linetype =2)+
  geom_vline(data = storms2, aes(xintercept = Date), color = "chartreuse")

####################################
#catch the first time flow is high

SacStorms = SacflowRecent  %>%
  mutate(run = with(rle(Storm), rep(seq_along(lengths), lengths))) %>%
  group_by(run) %>%
  mutate(N = n()) %>%
  filter(Storm == "STORM") %>%
  slice_head(n = 1) %>%
  ungroup()

#first time flow > 25000
SacFF = SacflowRecent  %>%
  mutate(run = with(rle(FF), rep(seq_along(lengths), lengths))) %>%
  group_by(run) %>%
  mutate(N = n()) %>%
  filter(FF == "STORM") %>%
  slice_head(n = 1) %>%
  ungroup()

#first time change i Sac+Yolo > 6500
SacYolo = SacflowRecent  %>%
  mutate(run = with(rle(YSstorm), rep(seq_along(lengths), lengths))) %>%
  group_by(run) %>%
  mutate(N = n()) %>%
  filter(YSstorm == "STORM") %>%
  slice_head(n = 1) %>%
  ungroup()



ggplot()+
  geom_line(data = SacflowRecent, aes(x = Date, y = SAC))+
  geom_line(data = filter(SacflowRecent, change >0),
            aes(x = Date,y = change), color = "blue")+
  geom_hline(yintercept = 25000, color = "red", linetype =2)+
  geom_vline(data = SacStorms, aes(xintercept = Date), color = "chartreuse")


ggplot()+
  geom_line(data = SacflowRecent, aes(x = Date, y = SAC))+
  geom_line(data = SacflowRecent, aes(x = Date, y = YOLO), color = "purple")+
  geom_line(data = filter(SacflowRecent, change >0),
            aes(x = Date,y = change), color = "blue")+
  geom_hline(yintercept = 25000, color = "red", linetype =2)+
  geom_vline(data = SacStorms, aes(xintercept = Date), color = "chartreuse")+
  coord_cartesian(xlim = c(ymd("2016-10-01", "2017-06-01")))

ggplot()+
  geom_line(data = SacflowRecent, aes(x = Date, y = SAC))+
  geom_line(data = filter(SacflowRecent, change >0),
            aes(x = Date,y = change), color = "blue")+
  geom_hline(yintercept = 25000, color = "red", linetype =2)+
  geom_vline(data = SacStorms, aes(xintercept = Date), color = "chartreuse", linewidth = 1)+
  coord_cartesian(xlim = c(ymd("2022-10-01", "2023-06-01")))


ggplot()+
  geom_line(data = SacflowRecent, aes(x = Date, y = SAC))+
  geom_line(data = SacflowRecent, aes(x = Date, y = YOLO), color = "purple")+
  geom_line(data = filter(SacflowRecent, change >0),
            aes(x = Date,y = change), color = "blue")+
  geom_hline(yintercept = 25000, color = "red", linetype =2)+
  geom_vline(data = SacStorms, aes(xintercept = Date), color = "chartreuse", linewidth = 1)+
  coord_cartesian(xlim = c(ymd("2021-10-01", "2021-11-11")), ylim = c(0,50000))


#how long is each storm?
#is the "storm" the increasing arm of the hydrograph? Or time spent above a certain flow rate?

#come up with plots for one year identified in several different ways
#is fremont weir overtopping? Is the lower peaks later on because water going through thte bypass?

#first flush is not likely to overtop Yolo, later storm events are more likeky to include yolo.
#check for first storm and Yolo

#does Yolo act as a soruce or sink for sediment? It's complex. Don' really konw. Dave did
#some stuff once. Mass-balance of TSS.

#find a correlation bewteen turbidity and flow and then apply it to the past

#get fremont weir data instead of yolo calculation? Does it matter?

#CCF has turbiidty back to 198somethign

#########################################################

#three-day average flow > 25000 (similar to current)
ggplot()+
  geom_line(data = SacflowRecent, aes(x = Date, y = SAC, color = "Sac River Flow"))+
  geom_line(data = SacflowRecent,
            aes(x = Date,y = change, color = "Change in Sac Flow"))+
  geom_hline(linetype =2, aes(yintercept = 25000, color = "First Flush Trigger"))+
  geom_vline(data = stormsFF, aes(xintercept = Date, color = "Storm"), alpha = 0.8)+
  coord_cartesian(xlim = c(ymd("2023-10-1"), ymd("2024-06-1")))+ theme_bw()+
  scale_color_manual(values = c("blue", "red", "black", "orange"))


#first day in a series of > 25000
ggplot()+
  geom_line(data = SacflowRecent, aes(x = Date, y = SAC, color = "Sac River Flow"))+
  geom_line(data = SacflowRecent,
            aes(x = Date,y = change, color = "Change in Sac Flow"))+
  geom_hline(linetype =2, aes(yintercept = 25000, color = "First Flush Trigger"))+
  geom_vline(data = SacFF, aes(xintercept = Date, color = "First Flush"), alpha = 0.8)+
  #coord_cartesian(xlim = c(ymd("2016-10-1"), ymd("2017-06-1")))+ theme_bw()+
  coord_cartesian(xlim = c(ymd("2023-10-1"), ymd("2024-06-1")))+ theme_bw()+

    scale_color_manual(values = c("blue", "orange", "red", "black", "orange"))


#plot showing all days with ROC >6500 - Sac only
ggplot()+
  geom_line(data = SacflowRecent, aes(x = Date, y = SAC, color = "Sac River Flow"))+
  geom_line(data = SacflowRecent,
            aes(x = Date,y = change, color = "Change in Sac Flow"))+
  geom_hline(linetype =2, aes(yintercept = 25000, color = "First Flush Trigger"))+
  geom_vline(data = storms, aes(xintercept = Date, color = "Storm"), alpha = 0.8)+
  coord_cartesian(xlim = c(ymd("2023-10-1"), ymd("2024-06-1")))+ theme_bw()+

  # coord_cartesian(xlim = c(ymd("2016-10-1"), ymd("2017-06-1")))+ theme_bw()+
  scale_color_manual(values = c("blue", "red", "black", "orange"))

#plot showing just the first day of ROC>6500
ggplot()+
  geom_line(data = SacflowRecent, aes(x = Date, y = SAC, color = "Sac River Flow"))+
  geom_line(data = SacflowRecent,
            aes(x = Date,y = change, color = "Change in Sac Flow"))+
  geom_hline(linetype =2, aes(yintercept = 25000, color = "First Flush Trigger"))+
  geom_vline(data = SacStorms, aes(xintercept = Date, color = "Storm"), alpha = 0.8)+

  coord_cartesian(xlim = c(ymd("2023-10-1"), ymd("2024-06-1")))+ theme_bw()+

   # coord_cartesian(xlim = c(ymd("2016-10-1"), ymd("2017-06-1")))+ theme_bw()+
  scale_color_manual(values = c("blue", "red", "black", "orange"))

#now look at Yolo +Sac flow
ggplot()+
  geom_line(data = SacflowRecent, aes(x = Date, y = SAC, color = "Sac River Flow"))+
  geom_line(data = SacflowRecent,
            aes(x = Date,y = change, color = "Change in Sac Flow"))+
  geom_line(data = SacflowRecent,
            aes(x = Date,y = YOLO, color = "Yolo Flow"))+
  geom_hline(linetype =2, aes(yintercept = 25000, color = "First Flush Trigger"))+
  coord_cartesian(xlim = c(ymd("2023-10-1"), ymd("2024-06-1")),
                  ylim = c(0, 70000 ))+ theme_bw()+

   geom_vline(data = stormswYolo, aes(xintercept = Date, color = "Storm"), alpha = 0.8)+
#  coord_cartesian(xlim = c(ymd("2016-10-1"), ymd("2017-06-1")))+ theme_bw()+
  scale_color_manual(values = c("blue", "red", "black", "orange", "purple"))

#look at it a slightly diffrent way

ggplot()+
  geom_line(data = SacflowRecent, aes(x = Date, y = YoloSac, color = "Sac+Yolo Flow"))+
  geom_line(data = SacflowRecent,
            aes(x = Date,y = YSchange, color = "Change in Sac + Yolo Flow"))+
  geom_hline(linetype =2, aes(yintercept = 25000, color = "First Flush Trigger"))+
  geom_vline(data = stormswYolo, aes(xintercept = Date, color = "Storm"), alpha = 0.8)+
  coord_cartesian(xlim = c(ymd("2021-10-1"), ymd("2022-06-1")),
                  ylim = c(0, 100000))+ theme_bw()+

  #coord_cartesian(xlim = c(ymd("2016-10-1"), ymd("2017-06-1")))+ theme_bw()+
  scale_color_manual(values = c("blue", "red", "black", "orange"))

#first day Sac + yolo rate of change is high


ggplot()+
  geom_line(data = SacflowRecent, aes(x = Date, y = YoloSac, color = "Sac+Yolo Flow"))+
  geom_line(data = SacflowRecent,
            aes(x = Date,y = YSchange, color = "Change in Sac + Yolo Flow"))+
  geom_hline(linetype =2, aes(yintercept = 25000, color = "First Flush Trigger"))+
  geom_vline(data = SacYolo, aes(xintercept = Date, color = "Storm"), alpha = 0.8)+
  coord_cartesian(xlim = c(ymd("2016-10-1"), ymd("2017-06-1")),
                  ylim = c(0, 250000))+ theme_bw()+
  scale_color_manual(values = c("blue", "red", "black", "orange"))

###########turbidity###############################################################

ggplot()+
  geom_line(data = SacflowRecent, aes(x = Date, y = YoloSac, color = "Sac+Yolo Flow"))+
  geom_line(data = SacflowRecent,
            aes(x = Date,y = YSchange, color = "Change in Sac + Yolo Flow"))+
  geom_hline(linetype =2, aes(yintercept = 25000, color = "First Flush Trigger"))+
  geom_vline(data = stormsFFturb, aes(xintercept = Date, color = "Storm"), alpha = 0.8)+
  coord_cartesian(xlim = c(ymd("2016-10-1"), ymd("2017-06-1")))+ theme_bw()+
  scale_color_manual(values = c("blue", "red", "black", "orange"))

#follow up - Use Yolo + Sac
#Look at turbidity more
#identify first storm
# think about how to identify duration.
#see whether there is a difference in early first flush versus later first flush
#are there differences by species?

#Duration ideas:
# Length of fime before ROC goes negaive
#number of days at a certain turbidity?
#It's a storm-driven flow event, so back-to-back ARs count as the same.
# STORM - Smelt Turbidity Origins Regarding MIgration

#We can calculate Suspended sediment loads - but not for YOlo
#could look at Fremont weird flow/stage instead of Dayflow data.

#what other datasets do we need?
#Delta smelt migraiton
#longfin smelt migraiton
#salmon migration
#chlorophyll
#zooplankton

#Pete will start looking at fish data - Katie can help with the fish data.
#Dave knows how to use the deltafish package, can help. Christina can also help wiht survye experties

#Rosie will flag the storms from first day >6500 until ROC goes negative

#Dave to look at turbidity / suspended sediment. Also Chl.

#Christina can look into zooplankton data a bit.

#Find hte sharepoint

#FMWT and Bay study for smelt
#Could also include salvage, but we can't rely on
#data from before 1993 or so (for smelt).
#DJFMP for salmon. Chipps island (also good for longfin)/sac trawl/Mossdale for when things move into the system
#beach seine for distribution.
#New rotory screw traps

#lower trophic - EMP, all the rest of hte zoops.
#Check out continuous sondes for chlorophyll - it might just
#dilute immediately. Any increase will be lagged. Also need to adjust for
#turbidity.

#the oldest turbiidty is 2007
#CC Forebay may have data back to 1988 ,but it's just one station.

#look for long-term SS data on our aciton items.

#How do we identify migration?
## shifts in distribution -

