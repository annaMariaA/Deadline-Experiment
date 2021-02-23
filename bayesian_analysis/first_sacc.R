# set the seed, so running this file will give exactly the same numbers as in our paper (hopefully)!
set.seed(2020)

knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(fig.height = 4, fig.align = 'center') 

library(brms)
library(tidyverse)
library(tidybayes)
library(patchwork)


rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# import data
# source("import_and_tidy_data.R")
dat_rt_acc <- read_csv("dat_acc_rt.csv")
dat_fix <- read_csv("dat_fix.csv")


d_sacc1 <- filter(dat_fix, n == 1) %>%
  select(-n, -hetero_fix, -ts)

d_fix2 <- filter(dat_fix, n== 2) %>%
  select(-n, -duration, -ts)

d_init_sacc <- full_join(d_sacc1, d_fix2) %>%
  filter(is.finite(hetero_fix)) %>%
  mutate(
    saccade_to = if_else(hetero_fix == 1, "heterogeneous", "homogeneous"),
    observer = as_factor(observer))

d_init_sacc %>% ggplot(aes(x= duration, fill = saccade_to)) + geom_density(alpha = 0.5)  + facet_grid(observer~cd)


# let's just look ate median fixation duration
d_init_sacc %>% group_by(observer, saccade_to, cd) %>%
  summarise(median_duration = median(duration)) %>%
  ggplot(aes(y = median_duration, fill = saccade_to)) + geom_boxplot() + facet_wrap(~cd)

d_init_sacc %>% filter(observer != "d2") %>%
  group_by(observer, saccade_to, cd) %>%
  summarise(median_duration = median(duration)) %>%
  group_by(cd, saccade_to) %>% 
  summarise(mean_med_dur = mean(median_duration))

