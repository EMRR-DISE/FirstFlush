#Generic BRMS code

####packages####
library(tidyverse)
library(brms)
library(tidybayes)
library(bayesplot)
library(bayestestR)
library(ggeffects)


####prepare data, and explore raw
data <- read.csv("C:/Users/mjyoung/Desktop/DesktopR/BRMSprimer/dat_th 1.csv")
data$hit2 <- ifelse(data$hit==TRUE,1,0)
data$reg2 <- as.factor(data$Region)

#apply model only to DJFMP at Chipps, for 90%
datause <- data%>%
  filter(Region==2,Source=="DJFMP",threshold=="0.9")

#data for Bay Study, 90%
datause2 <- data%>%
  filter(Region%in%c(1,2,3),Source=="Bay Study",threshold=="0.9")

datauseplot <- ggplot(datause2)+
  geom_point(aes(y=hit2,x=YSchange_sc,color=as.factor(WY)),alpha=0.5)+
  geom_smooth(aes(y=hit2,x=YSchange_sc))+
  theme_bw()
datauseplot

####run example model####
brmtest <- brm(formula = hit2 ~ FFtoCatchDiff_days_sc*reg2 + 
                 #reg2 +
                 #YSchange_sc + 
                 #Julian Day yday+
                 (1|WY),
                 #reg2,
               data=datause2,
               family=bernoulli(),
               warmup=1000,iter=3000,chains=3,cores=3,thin=10,
               control=list(adapt_delta=0.99))

brmtest2 <- brm(formula = hit2 ~ FFtoCatchDiff_days_sc*reg2 + 
                 YSchange + 
                 #Julian Day yday+
                 (1|WY),
               data=datause2,
               family=bernoulli(),
               warmup=1000,iter=3000,chains=3,cores=3,thin=10, ##
               control=list(adapt_delta=0.99))

#### example exploration of model output ####
#look at model summary
summary(brmtest)
plot(brmtest)
mcmc_plot(brmtest)
plot(conditional_effects(brmtest),theme=theme_bw())

#posterior prediction check
pp_check(brmtest)
pp_check(brmtest, type = "stat", stat = "mean")

#bayesian p - target is ~ 0.5
T_obs <- mean(datause2$hit2)
T_rep <- apply(posterior_predict(brmtest, draws = 1000), 1, mean)
bayes_p <- mean(T_rep >= T_obs)     # Proportion of times T_rep >= T_obs

#region of practical equivalence (ROPE)
# - user defined ROPE - how much of prob. distribution overlaps
# with a 'negligible effect size', "not zero, but basically meaningless"
rope_result <- rope(brmtest, range = c(-0.1, 0.1))
rope_result

#probability of direction (max probability of effect)
# - probability that parameter is strictly positive/negative, 
# analog to p-value for individual estimates
ppd <- p_direction(brmtest)
ppd


#model comparison
waic(brmtest,brmtest2)
loo(brmtest,brmtest2)

#how much variability is there in your random effect?
#if random effect
brmtest %>%
  spread_draws(r_WY[WY], sd_WY__Intercept) %>%
  head(15)

r_draws <- brmtest %>%
  spread_draws(r_WY[WY], sd_WY__Intercept) 

ggplot(data=r_draws,aes(x = r_WY, y = WY)) +
  stat_halfeye(aes(group=WY))+
  theme_bw()



#### predictions and plot####
#order of parameters matters. If parameter is not included, it is 
#set to mean as default
output_brmtest <- data.frame(ggpredict(brmtest, terms = c("reg2","FFtoCatchDiff_days_sc")))
output_brmtest <- data.frame(ggpredict(brmtest, terms = c("FFtoCatchDiff_days_sc","reg2")))

#generate higher resolution of predictions as desired
predgrid <- expand.grid(
  FFtoCatchDiff_days_sc     = seq(from=min(datause2$FFtoCatchDiff_days_sc,na.rm=TRUE), 
                                  to=max(datause2$FFtoCatchDiff_days_sc,na.rm=TRUE), 
                                  length.out=10),   
  reg2 = datause2$reg2
)

pred <- predict_response(brmtest, terms = predgrid)   # preferred for grid control
plot(pred)  # quick plot


#probability 90% exceeded for each region by FFday_sc
ggplot(output_brmtest,aes(x=x,
                          y=predicted))+
  geom_ribbon(aes(ymin=conf.low,ymax=conf.high,fill=group),alpha=0.5)+
  geom_line(aes(color=group))+
  #geom_errorbar(aes(ymin=conf.low,ymax=conf.high),width=0.5)+
  geom_point()+
  labs(x="scaled difference")+
  #facet_grid(.~x)+
  theme_bw()

ggplot(output_brmtest,aes(x=x,y=predicted))+
  geom_ribbon(aes(ymin=conf.low,ymax=conf.high,fill=group),alpha=0.5)+
  geom_line(aes(color=group))+
  #geom_errorbar(aes(ymin=conf.low,ymax=conf.high),width=0.5)+
  geom_point()+
  labs(x="scaled difference")+
  #facet_grid(.~x)+
  theme_bw()

ggplot(pred,aes(x=group,y=predicted))+
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high),width=0.5)+
  geom_point()+
  labs(x="Region")+
  facet_grid(.~x)+
  theme_bw()

