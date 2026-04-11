# scratch

# The code below is retained in case it's useful later, but the approaches are problematic. First, there was the initial attempt to use all years and all months. This proved unsatisfactory because, prior to 1992, seines were completed sporadically between April and December. Because monthly coverage improved in 1992, I also tried to limit the time series to the years from 1992 onward, retaining data for all months of the year. This second approach may be worth a second look; arguably, it's more honest than the approach used above where I used data from all years but only for the months of January, February and April.


# m1-all-yrs

# using all years and all months

# reaching the 10% benchmark
m.day10.wy <-
  brm(data = percentile_days, # use the percentile_days data
      # use a gaussian (normal) distribution
      family= gaussian,
      # specify the model
      day_10 ~ 1 + wy,
      # specify parameters for executing the Markov chains
      iter = 2000, warmup= 1000, chains= 4, cores=4,
      # setting the "seed" determines which randomnumbers will get sampled
      # so that team gets the same results when running the model
      seed = 42,
      file = here("salmon/results/m.day10.wy"),
      file_refit = "on_change")

# reaching the 50% benchmark
m.day50.wy <-
  brm(data = percentile_days,
      # Choose a gaussian (normal) distribution
      family= gaussian,
      day_50 ~ 1 + wy,
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 42,
      file = here("salmon/results/m.day50.wy"),
      file_refit = "on_change")

# reaching the 90% benchmark
m.day90.wy <-
  brm(data = percentile_days,
      family= gaussian,
      day_90 ~ 1 + wy,
      iter = 2000, warmup = 1000, chains = 4, cores =4,
      seed = 42,
      file = here("salmon/results/m.day90.wy"),
      file_refit = "on_change")

m.day10.wy <- read_rds(here("salmon/results/m.day10.wy.rds"))
m.day50.wy <- read_rds(here("salmon/results/m.day50.wy.rds"))
m.day90.wy <- read_rds(here("salmon/results/m.day90.wy.rds"))

summary(m.day10.wy)
summary(m.day50.wy)
summary(m.day90.wy)

# run the model plots for m.d_10.wy, m.d_50.wy, m.d_90.wy
plot(m.day10.wy)
plot(m.day50.wy)
plot(m.day90.wy) # less certain than the others

# calc the prob that the slope is less than 0
m.day10.wy %>%
  spread_draws(b_wy) %>%
  summarise(p_slope_lessthan_zero = sum(b_wy < 0) / n())
# virtually certain!

m.day50.wy %>%
  spread_draws(b_wy) %>%
  summarise(p_slope_lessthan_zero = sum(b_wy < 0) / n())

m.day90.wy %>%
  spread_draws(b_wy) %>%
  summarise(p_slope_lessthan_zero = sum(b_wy < 0) / n())

# Because seine samples were particularly inconsistent Apr-Dec prior to 1992, I tried working with wy>=1992.


# m2-92+


percentile_days92 <-
  percentile_days %>%
  filter(wy >= 1992)

# reaching the 10% benchmark
m2.day10.wy <-
  brm(data = percentile_days92, # use the percentile_days92 data
      # use a gaussian (normal) distribution
      family= gaussian,
      # specify the model
      day_10 ~ 1 + wy,
      # specify parameters for executing the Markov chains
      iter = 2000, warmup= 1000, chains= 4, cores=4,
      # setting the "seed" determines which randomnumbers will get sampled
      # so that team gets the same results when running the model
      seed = 42,
      file = here("salmon/results/m2.day10.wy"),
      file_refit = "on_change")

# reaching the 50% benchmark
m2.day50.wy <-
  brm(data = percentile_days92,
      # Choose a gaussian (normal) distribution
      family= gaussian,
      day_50 ~ 1 + wy,
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 42,
      file = here("salmon/results/m2.day50.wy"),
      file_refit = "on_change")

# reaching the 90% benchmark
m2.day90.wy <-
  brm(data = percentile_days92,
      family= gaussian,
      day_90 ~ 1 + wy,
      iter = 2000, warmup = 1000, chains = 4, cores =4,
      seed = 42,
      file = here("salmon/results/m2.day90.wy"),
      file_refit = "on_change")

m2.day10.wy <- read_rds(here("salmon/results/m2.day10.wy.rds"))
m2.day50.wy <- read_rds(here("salmon/results/m2.day50.wy.rds"))
m2.day90.wy <- read_rds(here("salmon/results/m2.day90.wy.rds"))

summary(m2.day10.wy)
summary(m2.day50.wy)
summary(m2.day90.wy)

plot(m2.day10.wy)
plot(m2.day50.wy)
plot(m2.day90.wy)

# model results aren't encouraging, but let's calc the prob that the slope is less than 0 just for fun

m2.day10.wy %>%
  spread_draws(b_wy) %>%
  summarise(p_slope_lessthan_zero = sum(b_wy < 0) / n())

m2.day50.wy %>%
  spread_draws(b_wy) %>%
  summarise(p_slope_lessthan_zero = sum(b_wy < 0) / n())

m2.day90.wy %>%
  spread_draws(b_wy) %>%
  summarise(p_slope_lessthan_zero = sum(b_wy < 0) / n())

# more scratch

benchmark_trends <-
  pctl_days %>%
  ggplot(
    # color points according to threshold (10, 50, 90%)
    aes(x = wy, y = wd, color = percentile)
  ) +
  geom_point(size = 4) +
  scale_color_fish(discrete = TRUE,
                   option = "Oncorhynchus_tshawytscha",
                   alpha = 0.6) +
  geom_smooth(data = pctl_days %>% filter(wy > 1977),
              alpha = 0.2,
              # show linear trends
              method = "lm",
              se = TRUE) +
  labs(title = "Chinook Salmon Outmigration Timing",
       subtitle = "all months; 1977 excluded from trend analyses",
       x = "Water Year",
       y = "Day of the Water Year",
       color = "Percentile") +
  theme_bw()


