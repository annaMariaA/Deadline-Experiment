library(tidyverse)

# Read in Acc and RT data
deadline_dat <- as_tibble(readRDS("../data/processedRTandAccData.Rda"))
reward_dat <- as_tibble(readRDS("../../Reward-Experiment/data/processedRTandAccData.Rda"))

subjectsToRemove = c(22,19,12)#22 and 19 accuracy on homogenous trials below 50%, 12 RT on homogenous trials over 8s
reward_dat = (reward_dat[!(reward_dat$subj%in% subjectsToRemove),])
reward_dat$subj = as.factor(reward_dat$subj)
rm(subjectsToRemove)

# Tidy up a little
deadline_dat %>% 
	select(
		observer = "subj", 
		condition = "version", 
		targ_side = "targSide",
		block = "completed",
		t = "trial",
		rt = "RT",
		acc) %>%
	mutate(
		block = as.factor(block),
		observer = paste("d", observer, sep = "")) -> deadline_dat

reward_dat %>% 
	select(
		observer = "subj", 
		condition = "incentive", 
		targ_side = "targSide",
		block,
		t = "trial",
		rt = "RT",
		acc) %>%
	mutate(
	  observer = paste("r", observer, sep = ""),
	  condition = if_else(block == " 1", "flat", condition)) -> reward_dat


bind_rows(deadline_dat, reward_dat) %>%
	mutate(
		condition = as_factor(condition),
		condition = fct_recode(condition, 
		                       reward = "r", flat = "f", long = "U", brief = "T"),
		condition = fct_relevel(condition, "long", "flat", "brief", "reward"),
		block = as_factor(block),
		block = fct_recode(block, "block 1" = "1", "block 2" = "2"),
		t = as.numeric(t),
		t = if_else(block == "block 2", t + 96, t))  -> dat_rt_acc

rm(deadline_dat, reward_dat)

### Read in fixation data
# read Deadline data
deadline_dat <- as_tibble(readRDS("../data/processedFixData.Rda")) 

# get Reward data
reward_dat <- as_tibble(readRDS("../../Reward-Experiment/data/processedFixData.Rda"))
reward_dat %>% mutate(fixX  = fixXflipped + 512) -> reward_dat

subjectsToRemove = c(22,19,12)#22 and 19 accuracy on homogenous trials below 50%, 12 RT on homogenous trials over 8s
reward_dat = (reward_dat[!(reward_dat$subj%in% subjectsToRemove),])
reward_dat$subj = as.factor(reward_dat$subj)
rm(subjectsToRemove)


assign_fixation_side <- function(df) {
	# classify every fixation as homo (right), central, or hetero (left)
	centralWidth = 64 #change to 1 visual degree
	df$side = 'central'
	df$side[which(df$fixX <(512-centralWidth/2))] = "hetero"
	df$side[which(df$fixX >(512+centralWidth/2))] = "homo"
	df$side = as.factor(df$side)
	return(df)
}

deadline_dat <- assign_fixation_side(deadline_dat)
reward_dat <- assign_fixation_side(reward_dat)

deadline_dat %>% 
	select(
		observer = "subj", 
		condition = "version", 
		block = "completed",
		t = "trial", 
		n = "fixNum",
		side) %>%
	mutate(observer = paste("d", observer, sep = ""))-> deadline_dat

reward_dat %>% 
	select(
		observer = "subj", 
		condition = "incentive", 
		block,
		t = "trialNum", 
		n = "fixNum",
		side) %>%
	mutate(
	  observer = paste("r", observer, sep = ""),
	  condition = if_else(block == " 1", "flat", condition)) -> reward_dat


bind_rows(deadline_dat, reward_dat) %>%
	filter(n > 1, n < 6) %>%
	mutate(
		condition = as_factor(condition),
		condition = fct_recode(condition, 
		                       long = "U", flat = "f", brief = "T", reward = "r"),
		condition = fct_relevel(condition, "long", "flat", "brief", "reward"),
		hetero_fix = as.numeric(side == "hetero"),
		block = as_factor(block),
		block = fct_recode(block, "block 1" = " 1", "block 2" = " 2"),
		t = if_else(block == "block 2", t + 96, t),
		ts = scale(t, center = FALSE)[,1]) %>%
	select(-side) -> dat_fix

rm(reward_dat, deadline_dat)