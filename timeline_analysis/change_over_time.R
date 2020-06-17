library(brms)
library(tidyverse)
library(tidybayes)
library(RcppRoll)

rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# read Deadline data
deadline_dat <- as_tibble(readRDS("../data/processedFixData.Rda")) 
# get Reward data
reward_dat <- as_tibble(readRDS("../../Reward-Experiment/data/processedFixData.Rda"))
reward_dat %>% mutate(fixX  = fixXflipped + 512) -> reward_dat


subjectsToRemove = c(22,19,12)#22 and 19 accuracy on homogenous trials below 50%, 12 RT on homogenous trials over 8s
deadline_dat = (deadline_dat[!(deadline_dat$subj%in% subjectsToRemove),])
deadline_dat$subj = as.factor(deadline_dat$subj)


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
	filter(
	  targSide == "absent",
	  accuracy == 1) %>%
	select(
		subject = "subj", 
		condition = "version", 
		block = "completed",
		t = "trial", 
		n = "fixNum",
		side) %>%
	mutate(subject = paste("d", subject, sep = "-"))-> deadline_dat

reward_dat %>% 
	filter(
	  targPresent == "absent",
	  accuracy == 1) %>%
	select(
		subject = "subj", 
		condition = "incentive", 
		block,
		t = "trialNum", 
		n = "fixNum",
		side) %>%
	mutate(subject = paste("r", subject, sep = "-")) %>%
	bind_rows(deadline_dat) %>%
	filter(n > 1, n < 8) %>%
	mutate(
		n = as.factor(n),
		condition = as_factor(condition),
		condition = fct_recode(condition, 
			reward = "r", flat = "f", untimed = "U", timed = "T"),
		hetero_fix = as.numeric(side == "hetero"),
		block = as_factor(block),
		block = fct_recode(block, "block 1" = " 1", "block 2" = " 2"),
		t = if_else(block == "block 2", t + 96, t)) %>%
	select(-side) -> dat_m

		
dat_agg <- dat_m %>% group_by(block, condition, t, n) %>%
	summarise(mean_hetero_fix = mean(hetero_fix))


# m_trial <- brm(
# 	data = dat_m,
# 	hetero_fix ~ t * n + (n | subject), 
# 	family = "bernoulli")
# m_trial <- add_criterion(m_trial, c("loo", "waic"))
# saveRDS(m_trial, "my_trial.model")

m_block_trial <- brm(
	data = dat_m,
	hetero_fix ~ block * t * n + (n | subject), 
	family = "bernoulli",
	cores = 4)
m_block_trial <- add_criterion(m_block_trial, c("loo", "waic"))

# m_deadline <- brm(
# 	data = dat_m,
# 	hetero_fix ~ deadline + block * t * n + (n | subject), 
# 	family = "bernoulli")
# m_deadline <- add_criterion(m_deadline, c("loo", "waic"))
# 
# m_full <- brm(
# 	data = dat_m,
# 	hetero_fix ~ condition * block * t * n + (n | subject), 
# 	family = "bernoulli")
# m_full <- add_criterion(m_full, c("loo", "waic"))
#  
# saveRDS(m_full, "my.model")
# m_full <- readRDS("my.model")

# m_deadline3 <- brm(
# 	data = dat_m,
# 	hetero_fix ~ deadline * block * t * n - deadline:block:t:n + (n | subject), 
# 	family = "bernoulli")
# m_deadline3 <- add_criterion(m_deadline3, c("loo", "waic"))

# mw <- model_weights(m_deadline2, m_deadline3, m_deadline, m_block_trial, m_block, m_trial)

windowed_mean <- function(df, blk, dl, nf, ws) {
	x <- filter(df, block == blk, condition == cd, n == nf)$mean_hetero_fix
	return(roll_mean(x , ws, align = "center"))
}

rolling_fix_prop <- tibble(
	block = as.character(), 
	condition = as.character(),
	t = as.numeric(),
	n = as.numeric(), 
	prop_fix = as.numeric())

ws <- 7

for (n in 2:8) {
	for (blk in c("block 1", "block 2")) {
		for (cd in levels(dat_m$condition)) {

			wf <- windowed_mean(dat_agg, blk, cd, n, ws)

			rolling_fix_prop %>% bind_rows(
				tibble(
					block = blk, 
					condition = cd, 
					t = 1:length(wf) + (ws+1)/2,
					n = n, 
					prop_fix = wf)) -> rolling_fix_prop
		}
	}
}

rolling_fix_prop %>% mutate(
	t = if_else(block == "block 2", t = t + 96, t)) -> rolling_fix_prop

dat_m %>% 
	group_by(block) %>%
	modelr::data_grid(condition, n, t) %>%
	# mutate(n = fct_rev(n)) %>%
	add_fitted_draws(m_trial, re_formula = NA) %>%
	ggplot(
		aes(
			x = t, y = .value, 
			colour = condition, fill = condition)) +
	geom_hline(yintercept = 0.5, linetype = 2) +	
	stat_lineribbon(aes(y = .value), .width = c(0.90, 0.75, .50), alpha = 1/4) +
	facet_grid(. ~ n) + 
	geom_vline(xintercept = c(96, 97), size = 1, colour = "grey") +
	scale_fill_brewer(name = "deadline", palette = "Set2",) + 
  	scale_color_brewer(name = "deadline", palette = "Dark2") + 
  	scale_x_continuous("trial", breaks = seq(1, 192, 24)) +
  	scale_y_continuous("prop. fix. hetero", breaks = seq(0, 1, 0.1)) +
  	coord_cartesian(ylim = c(0.1, 0.9)) + 
  	theme_tidybayes() + 
  	theme(
  		legend.justification=c(1,0), 
  		legend.position=c(1,0), 
  		legend.box.background = element_rect(size=1)) +
   # geom_point(data = rolling_fix_prop, aes(y = prop_fix), alpha = 0.25) +
   geom_path(data = rolling_fix_prop, aes(y = prop_fix), alpha = 0.33) +
ggsave("scratch/strat_over_trials.png", width = 12, height = 5)


# get_variables(m)
# m %>% gather_draws(b_trial,  `b_trial:n3`, `b_trial:n4`, `b_trial:n5`) %>%
# 	ggplot(aes(x = .value, fill = .variable)) + geom_density(alpha = 0.33)

# dat_p$p <- predict(m, dat_p)[, 1]


# ggplot(dat_p, aes(x  = trial, y = p, colour = n )) + geom_point()
