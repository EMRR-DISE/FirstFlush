#####################################################


library(tidyverse)
library(zoo)

#the official storm dataset (for now)
load("data/external/Dayflow.RData")

#select just the most relevent parameters
Sacflow = select(Dayflow, Date, Year, SAC, YOLO, SJR, OUT) %>%
  mutate(Sacchange = SAC-lag(SAC), #Sacramento rate of change
         SJRchange =  SJR-lag(SJR), #San Joaquin rate of change
         YoloSac = YOLO+SAC,  #Yolo + sac river flow
         YSchange = YoloSac-lag(YoloSac)) #Yolo + Sac rate of change


Test = select(Sacflow, Date, YSchange)
write.csv(Test, "data/interim/YSflow.csv")
##calculate the 95% quantile of the sac river flow
#maybe just the positive vlaues
quantile(filter(Sacflow, Sacchange>0)$Sacchange, c(0.5, 0.8, 0.9, 0.95, 0.99), na.rm =T)
#95th is 6500

#samething on the San Joaquin river
quantile(filter(Sacflow, SJRchange>0)$SJRchange, c(0.5, 0.8, 0.9, 0.95, 0.99), na.rm =T)
#an order of manitude lower

#95th percentile of Yolo+Sac
quantile(filter(Sacflow, YSchange>0)$YSchange, c(0.5, 0.8, 0.9, 0.95, 0.99), na.rm =T)
#now we're at 10212 for 95th percentile, which i think is too high

#Initial analysis seems to say 6500 was a pretty good metric.
#flag the first day ROC is above 6500, then find the next time ROC goes negative

Sacflow = mutate(Sacflow, Storm = case_when(Sacchange >=6500 ~ "STORM"),
                 YSstorm = case_when(YSchange >= 6500 ~ "STORM"),
                 SJRstorm = case_when(SJRchange >= 650 ~ "STORM"))

#

#first time change in Sac+Yolo > 6500
SacYolo = Sacflow  %>%
  mutate(run = with(rle(YSstorm), rep(seq_along(lengths), lengths))) %>%
  group_by(run) %>%
  mutate(N = n()) %>%
  filter(YSstorm == "STORM") %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  rename(YSstormStart = YSstorm) %>%
  select(Date, YSstormStart)

Sacflow = left_join(Sacflow, SacYolo)

# find starts and ends
starts <- which(Sacflow$YSstormStart == "STORM")
ends   <- which(Sacflow$YSchange < 0)

# build periods
periods <- map(
  starts,   # pass in index too
  function(s) {
    e <- ends[ends > s][1]    # first end after start
    storm1 = Sacflow[s:(e-1), ]
    storm1 = mutate(storm1, ID = s)
    return(storm1)
  }
)
# combine into one dataframe
periods_df <- bind_rows(periods) %>%
  mutate(YSStorm = "STORM") %>%
  select(Date, YSStorm, ID) %>%
  arrange(ID) %>%
  group_by(Date, YSStorm) %>%
  summarize(ID = first(ID)) %>%
  ungroup()

#pull out first day and last day of each storm

StormStartEnd = periods_df%>%
  group_by(ID) %>%
  summarize(FirstDay = min(Date),
         LastDay = max(Date))
#add back to origional dataset
Sacflow_wstorms = select(Sacflow, Date, Year, SAC, YOLO, SJR, OUT, YoloSac,
                         YSchange) %>%
  left_join(periods_df) %>%
  mutate(Month = month(Date),
         WY = case_when(Month %in% c(10:12) ~ Year +1,
                        TRUE ~ Year)) %>%
  group_by(WY) %>%
  mutate(FirstStorm = case_when(ID == min(ID, na.rm =T) ~ "First Storm")) %>%
  ungroup()

#filter out first date of first storm for plotting
Firststorms = Sacflow_wstorms %>%
  filter(FirstStorm == "First Storm")  %>%
  group_by(ID) %>%
  filter(Date == first(Date)) %>%
  ungroup()

#demo with WY 2017
ggplot(filter(Sacflow_wstorms, Date >ymd("2016-10-01"), Date <ymd("2017-06-01")), aes(x = Date, y = YoloSac))+
  geom_rect(data = filter(StormStartEnd, FirstDay > ymd("2016-10-01"),LastDay < ymd("2017-06-01")), aes(xmin = FirstDay,
                                  xmax =  LastDay,
                                  ymin =0, ymax = 300000), fill = "skyblue", alpha = 0.5,
            inherit.aes = F)+ geom_line()+
  geom_vline(data = filter(Firststorms,Date > ymd("2016-10-01"), Date < ymd("2017-06-01")),
             aes(xintercept = Date), color = "blue") +
  theme_bw() + xlab("Date - 2017")

#demo with WY 2022
ggplot(filter(Sacflow_wstorms, Date >ymd("2021-10-01"), Date <ymd("2022-06-01")), aes(x = Date, y = YoloSac))+
  geom_rect(data = filter(StormStartEnd, FirstDay > ymd("2021-10-01"),LastDay < ymd("2022-06-01")),
            aes(xmin = FirstDay, xmax =  LastDay, ymin =0, ymax = 40000), fill = "skyblue", alpha = 0.5,
            inherit.aes = F)+ geom_line()+
  geom_vline(data = filter(Firststorms,Date > ymd("2021-10-01"), Date < ymd("2022-06-01")),
             aes(xintercept = Date), color = "blue") +
  theme_bw()+ xlab("Date - 2022")

#demo with WY 2024
ggplot(filter(Sacflow_wstorms, Date >ymd("2023-10-01"), Date <ymd("2024-06-01")), aes(x = Date, y = YoloSac))+
  geom_rect(data = filter(StormStartEnd, FirstDay > ymd("2023-10-01"),LastDay < ymd("2024-06-01")),
            aes(xmin = FirstDay, xmax =  LastDay,  ymin =0, ymax = 100000), fill = "skyblue", alpha = 0.5,
            inherit.aes = F)+ geom_line()+
  geom_vline(data = filter(Firststorms,Date > ymd("2023-10-01"), Date < ymd("2024-06-01")),
             aes(xintercept = Date), color = "blue") +
  theme_bw()+ xlab("Date - 2024")

#export storm files
save(Sacflow_wstorms, StormStartEnd, Firststorms, file = "data/processed/storms/StormData.RData")

write.csv(Sacflow_wstorms, "data/processed/storms/Sacflow_wstorms.csv", row.names = F)
write.csv(StormStartEnd, "data/processed/storms/StormsStartEnd.csv", row.names =F)
