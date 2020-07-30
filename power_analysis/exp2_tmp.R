---
title: "Alasdair's Analysis"
author: "ADF Clarke"
date: "30/06/2020"
output: html_document
---
library(brms)
library(tidyverse)
library(tidybayes)
library(patchwork)

rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# import data
source("import_and_tidy_data.R")

dat_fix %>% left_join(dat_rt_acc,
                      by = c("observer", "condition", "block", "t")) %>%
	filter(
		condition %in% c("flat", "reward"),
		acc == 1,
		targ_side == "absent") %>%
  mutate(
    condition = if_else(block == "block 1", "initial", as.character(condition)),
    condition = as_factor(condition)) %>%
	group_by(observer, block, condition, n) %>%
	summarise(prop_hetero = mean(hetero_fix), .groups = 'drop') -> d_strat


plt_strategy <- d_strat %>%
  group_by(observer, block, condition, n) %>%
	summarise(strategy = mean(prop_hetero)) %>%
	ggplot(aes(x = condition, y = strategy, fill = block)) +
	geom_boxplot(alpha = 0.25) +
	ggthemes::scale_colour_ptol() +
	theme_bw() +
	theme(legend.position = "none",
		axis.title = element_blank())

plt_strat_obs + plt_strategy + plot_layout(widths = c(3, 1))


dat_fix %>% left_join(dat_rt_acc,
                      by = c("observer", "condition", "block", "t")) %>%
	filter(
		condition %in% c("flat", "reward"),
		acc == 1,
		targ_side == "absent") %>%
  mutate(
    condition = if_else(block == "block 1", "initial", as.character(condition)),
    condition = as_factor(condition)) %>%
	group_by(observer, block, condition, t) %>%
	summarise(prop_hetero = mean(hetero_fix), .groups = 'drop') %>%
  mutate(
    prop_hetero = if_else(prop_hetero == 0, 0.01, prop_hetero),
    prop_hetero = if_else(prop_hetero == 1, 0.99, prop_hetero))-> d_strat

plot_reward_predictions <- function(m, title) {
  
  tibble(condition = c("initial", "flat", "reward")) %>%
    add_fitted_draws(m, re_formula = NA) %>%
		ungroup() %>%
    select(.draw, condition, .value) %>%
		pivot_wider(names_from = condition, values_from = .value) %>%
    mutate(
      improv_flat = flat - initial,
      improv_reward = reward - initial,
      difference = improv_reward - improv_flat) %>%
    select(-initial, -flat, -reward) %>%
    pivot_longer(-.draw, names_to = "condition", values_to = "difference") %>%
    mutate(
      condition = as_factor(condition),
      condition = fct_relevel(condition, "improv_flat", "difference", "improv_reward")) %>%
		ggplot(aes(x = difference, fill = condition)) + 
    geom_vline(xintercept = 0, linetype = 2) +
			geom_density(alpha = 0.3) +
    theme_bw() + 
			ggtitle(title) -> plt

	 return(plt)
}

I can now see what happens when I run the mcmc sampling process with `sample_prior = 'only'`:

```{r exp2modelprior, cache=TRUE}
prior_model <- brm(
	data = d_strat,
	prop_hetero ~ 0 + condition + (condition | observer),
	sample_prior = "only",
	family = "beta",
	prior = model_priors,
	iter = 10000,
	control = list(adapt_delta = 0.95))
```

And plot, to see if it looks reasonable.

```{r, fig.show = "hide"}
plt_prior <- plot_reward_predictions(prior_model, "Prior Predictions")
```

# Now refit model using data

```{r exp2modelposterior, cache=TRUE, fig.show = "hide"}
my_model <- brm(
  data = d_strat,
  prop_hetero ~ 0 + condition + (condition | observer),
  family = "beta",
	prior = model_priors,
	iter = 10000,
	control = list(adapt_delta = 0.95))
```

```{r}
plt_posterior <- plot_reward_predictions(my_model, "Posterior Predictions")
```

```{r}
plt_prior + plt_posterior + plot_layout(guides = "collect")
ggsave("plots/reward_model.png", width = 6, height = 2.5)
```

What is the the probability of a difference > 0, given the data? 

```{r}

summary(my_model)

tibble(condition = c("initial", "flat", "reward")) %>%
  add_fitted_draws(my_model, re_formula = NA) %>%
	ungroup() %>%
  select(.draw, condition, .value) %>%
	pivot_wider(names_from = condition, values_from = .value) %>%
  mutate(
    improv_flat = flat - initial,
    improv_reward = reward - initial,
    difference = improv_reward - improv_flat) %>%
  summarise(prob_diff_greater_zero = mean(difference > 0))
```
