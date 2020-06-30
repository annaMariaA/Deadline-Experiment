library(brms)
library(tidyverse)
library(tidybayes)
library(patchwork)

rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# import data
source("import_and_tidy_data.R")

# Look at accuracy data

dat_rt_acc %>% filter(condition %in% c("long", "brief")) %>%	
	group_by(observer, condition, targ_side) %>% 
	summarise(accuracy = mean(acc)) %>%
	ggplot(aes(x = condition, y = accuracy)) + 
	geom_boxplot(fill = "purple", alpha = 0.25) +
	facet_wrap(~ targ_side) +
	theme_bw() + 
	scale_y_continuous(limits = c(0.5, 1)) -> plt_acc

# Look at rt data

dat_rt_acc %>% filter(condition %in% c("long", "brief"), acc == 1) %>%	
	group_by(observer, condition, targ_side) %>% 
	summarise(median_rt = median(rt)) %>%
	ggplot(aes(x = condition, y = median_rt)) + 
	geom_boxplot(fill = "green", alpha = 0.25) +
	facet_wrap(~ targ_side) +
	theme_bw() + 
	scale_y_log10("median rt", breaks = c(1,2,4,8,16)) -> plt_rt

plt_acc / plt_rt

ggsave("plots/deadline_acc_rt.png", width = 4, height = 4)

dat_fix %>% left_join(dat_rt_acc) %>% 
	filter(
		condition %in% c("long", "brief"), 
		acc == 1,
		targ_side == "absent") %>%
	group_by(observer, condition, n) %>%
	summarise(prop_hetero = mean(hetero_fix)) -> d_strat

d_strat %>%
	ggplot(aes(x = n, y = prop_hetero, colour = condition)) + 
	geom_line() + 
	facet_wrap(~ observer, ncol = 3) + 
	theme_bw() + 
	scale_colour_viridis_d(end = 0.7) +
	theme(legend.position = "bottom",  
		strip.background = element_blank(),
		strip.text.x = element_blank()) -> plt_strat_obs

d_strat %>% group_by(observer, condition, n) %>%
	summarise(strategy = mean(prop_hetero)) %>%
	ggplot(aes(x = condition, y = strategy, fill = condition)) + 
	geom_boxplot(alpha = 0.25) +
	scale_fill_viridis_d(end = 0.7) + 
	theme_bw() + 
	theme(legend.position = "none",
		axis.title = element_blank()) -> plt_strategy

plt_strat_obs + plt_strategy + plot_layout(widths = c(2, 1))
ggsave("plots/deadline_fix.png", width = 7, height = 5)

plot_predictions <- function(m, title) {
	m %>% spread_draws(b_Intercept, b_conditionbrief) %>%
		mutate(
			strat_long = b_Intercept,
			strat_brief = b_Intercept + b_conditionbrief,
			difference = strat_brief - strat_long) %>%
		select(strat_long, strat_brief, difference) %>%
		pivot_longer(
			c(strat_long, strat_brief, difference), 
			names_to = "condition", values_to = ".value") %>%
		ggplot(aes(x = .value, fill = condition)) + 
			geom_density(alpha = 0.3) +
			ggtitle(title) -> plt

	plot(plt)
}

# Define some priors
model_priors <- c(
	prior(normal(0.5, 0.1), class = "Intercept"),
	prior(normal(0, 0.1), class = "b"))

# Prior Predictions
prior_model <- brm(
	data = d_strat,
	prop_hetero ~ condition + (condition | observer),
	sample_prior = "only",
	prior = model_priors)

# Extract Prior Predictions and Plot
plt_prior <- plot_predictions(prior_model, "Prior Predictions")

# Now refit model using data
my_model <- brm(
	data = d_strat,
	prop_hetero ~ condition + (condition | observer),
	prior = model_priors)

plt_posterior <- plot_predictions(my_model, "Posterior Predictions")

plt_prior + plt_posterior
ggsave("deadline_main_effect.png")

# Now repeat for reward study!
# Look at accuracy data

dat_rt_acc %>% filter(condition %in% c("flat", "reward"), block == "block 2") %>%	
	group_by(observer, condition, targ_side) %>% 
	summarise(accuracy = mean(acc)) %>%
	ggplot(aes(x = condition, y = accuracy)) + 
	geom_boxplot(fill = "purple", alpha = 0.25) +
	facet_wrap(~ targ_side) +
	theme_bw() + 
	scale_y_continuous(limits = c(0.5, 1)) -> plt_acc

# Look at rt data

dat_rt_acc %>% filter(condition %in% c("flat", "reward"), block == "block 2", acc == 1) %>%	
	group_by(observer, condition, targ_side) %>% 
	summarise(median_rt = median(rt)) %>%
	ggplot(aes(x = condition, y = median_rt)) + 
	geom_boxplot(fill = "green", alpha = 0.25) +
	facet_wrap(~ targ_side) +
	theme_bw() + 
	scale_y_log10("median rt", breaks = c(1,2,4,8,16)) -> plt_rt

plt_acc / plt_rt

ggsave("plots/reward_acc_rt.png", width = 4, height = 4)
