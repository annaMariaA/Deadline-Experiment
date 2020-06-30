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
	mutate(
	  subject = paste("r", subject, sep = "-"),
	  condition = if_else(block == " 1", "flat", condition)) %>%
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
		t = if_else(block == "block 2", t + 96, t),
		t = scale(t, center = FALSE)) %>%
	select(-side) -> dat_m


t_scale <- attributes(dat_m$t)$`scaled:scale`

dat_agg <- dat_m %>% group_by(block, condition, t, n) %>%
	summarise(mean_hetero_fix = mean(hetero_fix))

## Rolling mean calculation
source("prop_rolling_mean.R")

ggplot(
	rolling_fix_prop, 
	aes(
		x = t, 
		y = prop_fix, 
		colour = condition, 
		group = interaction(block, condition))) +
	geom_path(alpha = 0.5) + 
	facet_grid(. ~ n) +
	geom_vline(xintercept = 97, size = 1, colour = "darkgrey") +
	geom_hline(yintercept = 0.5, linetype = 2) +
	theme_bw() +
	scale_x_continuous("trial", breaks = c(1, 97), expand = c(0, 0)) +
	scale_y_continuous("prop. fixations", breaks = c(0, 0.5, 1), limits = c(0, 1), expand = c(0, 0))
ggsave("scratch/empirical_rolling_mean.png", width = 10, height = 2)		

## Function to plot model predictions

plot_model_predictions <- function(df = dat_m, my_model, rfp = rolling_fix_prop) 
{
	df %>% modelr::data_grid(condition, block, t, n) %>% 
		filter(
			!(t > 96/t_scale & block == "block 1"),
			!(t <=  96/t_scale & block == "block 2"),
			!(t <= 96/t_scale & condition == "reward")) %>%
		add_fitted_draws(my_model, re_formula = NA) %>%
		ggplot(
			aes(
				x = t*t_scale, y = .value, 
				colour = condition, fill = condition)) +
		geom_hline(yintercept = 0.5, linetype = 2) +	
		stat_lineribbon(aes(y = .value), .width = c(0.90, 0.75, .50), alpha = 1/4, size =0.5) +
		facet_grid(condition ~ n) + 
		geom_vline(xintercept = c(96, 97), size = 1, colour = "darkgrey") +
		geom_path(
			data = rfp, 
			aes(x = t, y = prop_fix, group = block), 
			alpha = 0.33) +
		scale_fill_brewer(name = "condition", palette = "Set2",) + 
	  	scale_color_brewer(name = "condition", palette = "Dark2") + 
	  	scale_x_continuous("trial", breaks = seq(1, 192, 48)) +
	  	scale_y_continuous("prop. fix. hetero", breaks = seq(0, 1, 0.1)) +
	  	coord_cartesian(ylim = c(0, 1)) + 
	  	theme_tidybayes() + 
	  	theme(
	  		legend.position= "bottom", 
	  		legend.box.background = element_rect(size=1))
}

## Define priors
model_priors <- get_prior(
	data = dat_m,
	hetero_fix ~  condition*block*t*n + (n | subject),
	family = "bernoulli")	

model_priors <- c(
	prior(normal(0, 0.5), class = "Intercept"),
	prior(normal(0, 1.0), class = "b"))

# # prior predictions
# m_prior <- brm(
# 	data = dat_m,
# 	hetero_fix ~ condition*block*t*n + (n | subject),
# 	family = "bernoulli",
# 	sample_prior = "only",
# 	prior = model_priors,
# 	chains = 1)
# saveRDS(m_prior, "models/my_prior.model")
m_prior <- readRDS("models/my_prior.model")

plot_model_predictions(dat_m, m_prior, rolling_fix_prop)
ggsave("scratch/prior_predictions.png")
ggsave("scratch/prior_predictions.pdf")
rm(m_prior)

## Fit model to data and plot posterior predictions
# m_full <- brm(
# 	data = dat_m,
# 	hetero_fix ~ condition*block*t*n + (n | subject),
# 	family = "bernoulli",
# 	prior = model_priors,
# 	chains = 1)
 # m_full <- add_criterion(m_full, c("loo", "waic"))

# saveRDS(m_full, "models/my_full.model")

 m_full <- readRDS("models/my_full.model")

plot_model_predictions(dat_m, m_full, rolling_fix_prop)
ggsave("scratch/posterior_predictions_full.png")
ggsave("scratch/posterior_predictions_full.pdf")
rm(m_full)

# m_full_drop4 <- brm(
# 	data = dat_m,
# 	hetero_fix ~ condition*block*t*n - condition:block:t:n + (n | subject),
# 	family = "bernoulli",
# 	prior = model_priors,
# 	chains = 1)
# m_full_drop4 <- add_criterion(m_full_drop4, c("loo", "waic"))
# saveRDS(m_full_drop4, "models/my_drop4.model")

m_full_drop_block <- brm(
	data = dat_m,
	hetero_fix ~ condition*t*n + (n | subject),
	family = "bernoulli",
	prior = model_priors,
	chains = 1)
m_full_drop_block <- add_criterion(m_full_drop_block, c("loo", "waic"))
saveRDS(m_full_drop_block, "models/my_drop_block.model")


# m_trial <- brm(
# 	data = dat_m,
# 	hetero_fix ~ t * n + (n | subject), 
# 	family = "bernoulli")
# m_trial <- add_criterion(m_trial, c("loo", "waic"))
# saveRDS(m_trial, "my_trial.model")

# m_block_trial <- brm(
# 	data = dat_m,
# 	hetero_fix ~ block * t * n + (n | subject), 
# 	family = "bernoulli",
# 	cores = 4)
# m_block_trial <- add_criterion(m_block_trial, c("loo", "waic"))
# saveRDS(m_trial, "my_bock_trial.model")

# m_no4 <- brm(
# 	data = dat_m,
# 	hetero_fix ~ condition * block * t * n - condition:block:t:n + (n | subject),
# 	family = "bernoulli",
# 	sample_prior = "only")
# m_no4 <- add_criterion(m_no4, c("loo", "waic"))

# saveRDS(m_no4, "my_no4.model")
# m_no4 <- readRDS("my_no4.model")


# m_full <- readRDS("my_full.model")

# # m_deadline3 <- brm(
# # 	data = dat_m,
# # 	hetero_fix ~ deadline * block * t * n - deadline:block:t:n + (n | subject), 
# # 	family = "bernoulli")
# # m_deadline3 <- add_criterion(m_deadline3, c("loo", "waic"))

# # mw <- model_weights(m_deadline2, m_deadline3, m_deadline, m_block_trial, m_block, m_trial)

# dat_m %>% 
# 	group_by(block) %>%
# 	modelr::data_grid(condition, n, t) %>%
# 	filter(!(block == "block 1" & condition =="reward")) %>%
# 	# mutate(n = fct_rev(n)) %>%
# 	add_fitted_draws(m_full, re_formula = NA) %>%
# 	ggplot(
# 		aes(
# 			x = t, y = .value, 
# 			colour = condition, fill = condition)) +
# 	geom_hline(yintercept = 0.5, linetype = 2) +	
# 	stat_lineribbon(aes(y = .value), .width = c(0.90, 0.75, .50), alpha = 1/4, size =0.5) +
# 	facet_wrap(. ~ n, nrow = 2) + 
# 	geom_vline(xintercept = c(96, 97), size = 1, colour = "darkgrey") +
# 	scale_fill_brewer(name = "condition", palette = "Set2",) + 
#   	scale_color_brewer(name = "condition", palette = "Dark2") + 
#   	scale_x_continuous("trial", breaks = seq(1, 192, 48)) +
#   	scale_y_continuous("prop. fix. hetero", breaks = seq(0, 1, 0.1)) +
#   	coord_cartesian(ylim = c(0, 1)) + 
#   	theme_tidybayes() + 
#   	theme(
#   		legend.justification=c(1,0), 
#   		legend.position=c(1,0), 
#   		legend.box.background = element_rect(size=1)) +
#    # geom_point(data = rolling_fix_prop, aes(y = prop_fix), alpha = 0.25) +
#    geom_path(data = rolling_fix_prop, aes(y = prop_fix, group = block), alpha = 0.33) 

#   ggsave("scratch/strat_over_trials.png", width = 9, height = 5)


# # get_variables(m)
# # m %>% gather_draws(b_trial,  `b_trial:n3`, `b_trial:n4`, `b_trial:n5`) %>%
# # 	ggplot(aes(x = .value, fill = .variable)) + geom_density(alpha = 0.33)

# # dat_p$p <- predict(m, dat_p)[, 1]


# # ggplot(dat_p, aes(x  = trial, y = p, colour = n )) + geom_point()

