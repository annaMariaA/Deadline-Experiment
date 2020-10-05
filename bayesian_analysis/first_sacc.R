# set the seed, so running this file will give exactly the same numbers as in our paper (hopefully)!
set.seed(2020)

knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(fig.height = 4, fig.align = 'center') 

library(brms)
library(tidyverse)
library(tidybayes)
library(patchwork)
library(lmerTest)

rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# import data
# source("import_and_tidy_data.R")
dat_rt_acc <- read_csv("dat_acc_rt.csv")
dat_fix <- read_csv("dat_fix.csv")


d_sacc1 <- filter(dat_fix, n == 1) %>%
  select(-n, hetero_fix, ts)

d_fix2 <- filter(dat_fix, n== 2) %>%
  select(-n, -duration)

d_init_sacc <- fUll_join(d_sacc1, d_fix2)
