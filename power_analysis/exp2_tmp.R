---
title: "Alasdair's Analysis"
author: "ADF Clarke"
date: "30/06/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.height=4) 

library(brms)
library(tidyverse)
library(tidybayes)
library(patchwork)

rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# import data
source("import_and_tidy_data.R")
```

These supplementary materials contain full code for the Bayesian analysis (including power analysis).

# Experiment 1: Deadline

We will first look at descriptive statistics for accuracy and reaction time data, to check that it looks sensisble and inline with our expectations. 

## Descriptive Statistics

### Look at accuracy data

```{r}
dat_rt_acc %>% filter(condition %in% c("long", "brief")) %>%
  mutate(condition = fct_relevel(condition, "long")) %>%
	group_by(observer, condition, targ_side) %>% 
	summarise(accuracy = mean(acc), .groups = 'drop') %>%
	ggplot(aes(x = condition, y = accuracy, fill = fct_rev(condition))) + 
	geom_boxplot(alpha = 0.25) +
	facet_wrap(~ targ_side) +
	theme_bw() + 
  ggthemes::scale_fill_ptol() +
	scale_y_continuous(limits = c(0.5, 1)) +
  theme(legend.position = "none") -> p_acc
```

### Look at rt data

When we look at reaction times, we will only consider correct trials.

```{r}
dat_rt_acc %>% filter(condition %in% c("long", "brief"), acc == 1) %>%	
	group_by(observer, condition, targ_side) %>% 
	summarise(median_rt = median(rt), .groups = 'drop') %>%
	ggplot(aes(x = condition, y = median_rt, fill = fct_rev(condition))) + 
	geom_boxplot(alpha = 0.25) +
	facet_wrap(~ targ_side) +
	theme_bw() + 
  ggthemes::scale_fill_ptol() +
	scale_y_log10("median rt", breaks = c(1,2,4,8,16)) +
    theme(legend.position = "none") -> p_rt

p_acc / p_rt

ggsave("plots/exp_deadline_acc_rt.png", p_acc / p_rt, width = 8, height = 5)
```

### Saccadic Strategy

First, we need to merge (join) the fixation and accuracy data, so that we can take only correct target absent trials. We will compute the proportion of fixations to the heterogeneous side of the display for each fixation number, over all trials made by a participant. 

```{r}
dat_fix %>% left_join(dat_rt_acc, 
                      by = c("observer", "condition", "block", "t")) %>% 
	filter(
		condition %in% c("long", "brief"), 
		acc == 1,
		targ_side == "absent") %>%
	group_by(observer, condition, n) %>%
	summarise(prop_hetero = mean(hetero_fix), .groups = 'drop') -> d_strat
```

Create a facet plot of each individual's strategy.

```{r}
d_strat %>%
	ggplot(aes(x = n, y = prop_hetero, colour = condition)) + 
	geom_line() + 
	facet_wrap(~ observer, ncol = 3) + 
	theme_bw() + 
	scale_colour_viridis_d(end = 0.7) +
  scale_y_continuous("prop. hetero. fix.", breaks = c(0, 0.5, 1)) +
	theme(legend.position = "bottom",  
		strip.background = element_blank(),
		strip.text.x = element_blank()) -> plt_strat_obs
```

We can further summarise the data by creating a strategy measure, which is the proportion of all (2 - 5) fixations made by an observer over all trials. 

```{r}
d_strat %>% group_by(observer, condition) %>%
	summarise(strategy = mean(prop_hetero)) %>%
	ggplot(aes(x = condition, y = strategy, colour = fct_rev(condition), fill = fct_rev(condition))) + 
	geom_boxplot(alpha = 0.3, colour = "black") +
  geom_point(alpha = 0.5) + 
  geom_path(aes(group = observer), colour = "grey", alpha = 0.5) +
	ggthemes::scale_fill_ptol() +
  ggthemes::scale_color_ptol() +
  scale_y_continuous("prop. hetero. fix.") + 
	theme_bw() + 
	theme(legend.position = "none") + 
  ggtitle("(a) Empirical") -> plt_strategy

plt_strat_obs + plt_strategy + plot_layout(widths = c(2, 1))
```

## Bayesian Model of Saccadic Strategy

Summarise data so tha we have one strategy score per trial per observer. 

Note, as beta distributions are only defined over (0, 1), values of 0 and 1 are impossible. To get around this, we will set any such values to 0.001 and 0.999 respectively.

```{r}
dat_fix %>% 
  left_join(
    dat_rt_acc, 
    by = c("observer", "condition", "block", "t")) %>% 
	filter(
		condition %in% c("long", "brief"), 
		acc == 1,
		targ_side == "absent") %>%
	group_by(observer, condition, t) %>%
	summarise(prop_hetero = mean(hetero_fix), .groups = 'drop') %>% 
  mutate(
    prop_hetero = if_else(prop_hetero < 0.001, 0.001, prop_hetero),
    prop_hetero = if_else(prop_hetero > 0.999, 0.999, prop_hetero)) -> d_strat
```

### Define function for plotting model output

I will want to reuse this plotting code, so I will put it in a function here.

```{r}
plot_deadline_predictions <- function(m, title) 
{
  tibble(condition = c("long", "brief")) %>%
    add_fitted_draws(m, re_formula = NA) %>%
		ungroup() %>%
    select(.draw, condition, .value) %>%
		pivot_wider(names_from = condition, values_from = .value) %>%
    mutate(difference = brief - long) %>%
    pivot_longer(-.draw, names_to = "condition", values_to = "prop_hetero") %>%
    mutate(
      condition = as_factor(condition),
      condition = fct_relevel(condition, "long", "difference", "brief")) %>%
		ggplot(aes(x = prop_hetero, fill = condition)) + 
    geom_vline(xintercept = 0) + 
    geom_vline(xintercept = 0.5, linetype = 2) +
		geom_density(alpha = 0.3) +
    theme_bw() + theme(axis.title.y = element_blank()) + 
    scale_x_continuous("prop. hetero. fix.", limits = c(-0.5, 1)) + 
			ggtitle(title) -> plt

	 return(plt)
}
```

### Define Priors

```{r}
model_priors <- c(
	prior(normal(0, 1), class = "b"))
```

I can now see what happens when I run the mcmc sampling process with `sample_prior = 'only'`:

```{r exp1modelprior, cache=TRUE}
prior_model <- brm(
	data = d_strat,
	prop_hetero ~ 0 + condition + (condition | observer),
	family = "beta",
	sample_prior = "only",
	prior = model_priors,
	iter = 5000,
	control = list(adapt_delta = 0.95))
```

```{r}
plt_prior <- plot_deadline_predictions(prior_model, "(b) Prior")
plt_prior
```

### Power Analysis

We will carry out our power analysis by defining a new prior, `power_prior` which we can use to simulate data from. 

```{r exp1modelpower, cache=TRUE}

power_prior <- c(
  prior(normal(0.1, 0.25), class = "b", coef = "conditionlong"),
	prior(normal(0.7, 0.25), class = "b", coef = "conditionbrief"),
  prior(student_t(3, 0, 2), class = "sd"),
  prior(gamma(1, 10), class = "phi")
  )

power_model <- brm(
	data = d_strat,
	prop_hetero ~ 0 + condition + (condition | observer),
	family = "beta",
	sample_prior = "only",
	prior = power_prior,
	iter = 5000,
	control = list(adapt_delta = 0.95),
	refresh = 0)
```

We can now plot these distributions to check that they seem reasonable. 

```{r}
plt_power <- plot_deadline_predictions(power_model, "Power Predictions")
plt_power
```

Next, we write a function to generate a simlulated dataset.

```{r}
sample_data <- function(n_obs, n_trl) 
{
  d_strat %>% 
    modelr::data_grid(
      condition = c("long", "brief"), 
      observer = 1:n_obs) %>%
    add_fitted_draws(
      power_model, 
      n = n_trl, 
      re_formula = NULL, 
      scale = "response",
      sample_new_levels = "gaussian", 
      allow_new_levels = TRUE) %>% 
    ungroup() %>%
    select(observer, condition, prop_hetero = ".value") %>%
    mutate(
      prop_hetero = if_else(prop_hetero < 0.0001, 0.0001, prop_hetero),
      prop_hetero = if_else(prop_hetero > 0.9999, 0.9999, prop_hetero)) -> d_sim
  
  return(d_sim)
}
```

Now we also need a function that will the key statistic that we are interested in: the probability, given the data, that observers were more strategic in the brief condition than the long. 

```{r}
get_p_difference <- function(n_obs, n_trls) {
  
  # simulate some data 
  d_sim <- sample_data(n_obs, n_trls) 
  
  # fit model to simulated data
  sim_model <- brm(
  	data = d_sim,
  	prop_hetero ~ 0 + condition + (condition | observer),
  	family = "beta",
  	prior = model_priors,
  	iter = 5000,
  	control = list(adapt_delta = 0.95),
  	refresh = 0)
  
  # use fitted model to caculate p(difference between conditions > 0 | data)  
  tibble(condition = c("long", "brief")) %>%
    add_fitted_draws(sim_model, re_formula = NA, scale = "response") %>%
		ungroup() %>%
    select(.draw, condition, .value) %>%
		pivot_wider(names_from = condition, values_from = .value) %>%
    mutate(difference = brief - long) %>%
    summarise(prob_diff = mean(difference>0)) -> d
  
  return(d$prob_diff)
}
```

Finally, we run this a number of times to see how $p(\delta > 0| d)$ varies with the number of observers and trials. 

```{r runpoweranalysis, cache=TRUE, message=FALSE, warning=FALSE}
  
d_pwr <- tibble()

for (itr in 1:5) {
  n_obs = 15
  n_trls = 32
  d_pwr <- bind_rows(
    d_pwr, 
    tibble(
      n_obs = n_obs, 
      n_trls = n_trls, 
      power = get_p_difference(n_obs, n_trls)))
}
```

And plot!

```{r}
d_pwr %>%
  mutate(n_trls = as.factor(n_trls)) %>% 
  ggplot(aes(x = power)) + geom_histogram(bins = 10) + theme_bw()

d_pwr %>% summarise(over_90 = sum(power > 0.9))

d_sim <- sample_data(n_obs, n_trls) 
d_sim %>% group_by(condition) %>% summarise(mean_ph = mean(prop_hetero))
```

## Compute and Plot Posterior

 # Now refit model using data

```{r exp1modelposterior, cache=TRUE}
my_model <- brm(
  data = d_strat,
  prop_hetero ~ 0 + condition + (condition | observer),
  family = "beta",
	prior = model_priors,
	iter = 10000,
	control = list(adapt_delta = 0.95))
```

```{r}
plt_posterior <- plot_deadline_predictions(my_model, "(c) Posterior")
plt_posterior
```



```{r}
# Save plot with prior and posterior joined together 
ggsave(
  "plots/deadline_model.png", 
  plt_strategy + plt_prior + plt_posterior + plot_layout(guides = "collect"), 
  width = 8, 
  height = 2.5)
```

What is the the probability of a difference > 0, given the data? 

```{r}
tibble(condition = c("long", "brief")) %>%
    add_fitted_draws(my_model, re_formula = NA) %>%
		ungroup() %>%
    select(.draw, condition, .value) %>%
		pivot_wider(names_from = condition, values_from = .value) %>%
    mutate(difference = brief - long) %>%
  summarise(prob_diff_greater_zero = mean(difference > 0))

tibble(condition = c("long", "brief")) %>%
    add_fitted_draws(my_model, re_formula = NA) %>%
		ungroup() %>%
    select(.draw, condition, .value) %>%
		pivot_wider(names_from = condition, values_from = .value) %>%
    mutate(difference = brief - long) %>%
  median_hdci(long, brief, difference)
```

### Compute block to block R2 on the simpler aggregate data

```{r}

dat_fix %>% left_join(dat_rt_acc, 
                      by = c("observer", "condition", "block", "t")) %>% 
	filter(
		condition %in% c("long", "brief"), 
		acc == 1,
		targ_side == "absent") %>%
	group_by(observer, block, t) %>%
	summarise(prop_hetero = mean(hetero_fix), .groups = 'drop') %>% 
  group_by(observer, block) %>% 
  summarise(strategy = mean(prop_hetero)) -> dR2

d_strat %>% group_by(observer, condition) %>%
  summarise(strategy = mean(prop_hetero)) -> d_R2

bayes_R2(brm(strategy ~ observer, prior = model_priors, data = d_R2))

```

# Experiment 2: Reward

## Look at accuracy data

```{r}
dat_rt_acc %>% filter(condition %in% c("flat", "reward")) %>%
	group_by(observer, block, condition, targ_side) %>%
	summarise(accuracy = mean(acc), .groups = 'drop') %>%
	ggplot(aes(x = condition, y = accuracy, fill = block)) +
	geom_boxplot(alpha = 0.25) +
	facet_wrap(~ targ_side) +
	theme_bw() +
  ggthemes::scale_fill_ptol() +
	scale_y_continuous(limits = c(0.5, 1)) +
  theme(legend.position = "none")
```

## Look at rt data

We will only take the correct trials.

```{r}
dat_rt_acc %>% filter(condition %in% c("flat", "reward"), acc == 1) %>%
	group_by(observer, block, condition, targ_side) %>%
	summarise(median_rt = median(rt), .groups = 'drop') %>%
	ggplot(aes(x = condition, y = median_rt, fill = block)) +
	geom_boxplot(alpha = 0.25) +
	facet_wrap(~ targ_side) +
	theme_bw() +
  ggthemes::scale_fill_ptol() +
	scale_y_log10("median rt", breaks = c(1,2,4,8,16)) +
  theme(legend.position = "none")
```

## Saccadic Strategy

First, we need to merge (join) the fixation and accuracy data, so that we can take only correct target absent trials. We will compute the proportion of fixations to the heterogeneous side of the display for each fixation number, over all trials made by a participant.

```{r}
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
```

Create a facet plot of each individual's strategy.

```{r, fig.height = 8}
plt_strat_obs <- d_strat %>%
	ggplot(aes(x = n, y = prop_hetero, colour = block)) +
	geom_line() +
	facet_wrap(~ observer, ncol = 3) +
	theme_bw() +
	ggthemes::scale_colour_ptol() +
  scale_y_continuous("prop. hetero. fix.", breaks = c(0, 0.5, 1)) +
	theme(legend.position = "bottom",
		strip.background = element_blank(),
		strip.text.x = element_blank())
```

We can further summarise the data by creating a strategy measure, which is the proportion of all (2 - 5) fixations made by an observer over all trials.

```{r}
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
```

## Bayesian Model of Saccadic Strategy

Create a dataframe with the dependent variable `prop_hetero` - the proportion of the first five saccades that were directed to the heterogeneous side of the display. Note that values of 0 and 1 are shifted to 0.01 and 0.99 respectively. 

```{r}
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
```

```{r}
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
```

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

<!-- <!-- # Timecourse Analysis --> -->

<!-- <!-- I will now look to see what happens to search strategy over time: both on the scale of an individual trial, within and across blocks, and between experimental condition! First, I will fit one model to the data from both experiments.  --> -->

<!-- <!-- ```{r} --> -->
<!-- <!-- dat_fix %>% left_join(dat_rt_acc,  --> -->
<!-- <!--                       by = c("observer", "condition", "block", "t")) %>%  --> -->
<!-- <!-- 	filter( --> -->
<!-- <!-- 		acc == 1, --> -->
<!-- <!-- 		targ_side == "absent")  %>% --> -->
<!-- <!--   mutate( --> -->
<!-- <!--     condition = as.character(condition), --> -->
<!-- <!--     condition = if_else(condition == "long" & block == "block 1", "baseline", condition), --> -->
<!-- <!--     condition = if_else(condition == "long" & block == "block 2", "transfer", condition), --> -->
<!-- <!--     condition = if_else(condition == "flat", "baseline", condition), --> -->
<!-- <!--     condition = as_factor(condition), --> -->
<!-- <!--     n = as.factor(n)) -> d_strat --> -->

<!-- <!-- ``` --> -->

<!-- <!-- ```{r} --> -->
<!-- <!-- # compute rolling averages --> -->
<!-- <!-- source("prop_rolling_mean.R") --> -->
<!-- <!-- ``` --> -->

<!-- <!-- ## Prior Predictions --> -->

<!-- <!-- ```{r timecourse-prior, cache=TRUE} --> -->
<!-- <!-- model_priors <- c( --> -->
<!-- <!-- 	prior(normal(0, 1.0), class = "b")) --> -->

<!-- <!-- # m_prior <- brm( --> -->
<!-- <!-- # 	data = d_strat, --> -->
<!-- <!-- # 	hetero_fix ~ (0 + condition) * (0 +n) * block * ts  + (n | observer), --> -->
<!-- <!-- # 	family = "bernoulli", --> -->
<!-- <!-- # 	sample_prior = "only", --> -->
<!-- <!-- # 	prior = model_priors, --> -->
<!-- <!-- # 	chains = 1) --> -->
<!-- <!-- # # comment --> -->
<!-- <!-- # saveRDS(m_prior, "models/my_prior.model") --> -->

<!-- <!-- m_prior <- readRDS("models/my_prior.model") --> -->
<!-- <!-- ``` --> -->


<!-- <!-- ```{r} --> -->
<!-- <!-- plot_model_predictions <- function(df = dat_m, my_model, rfp = rolling_fix_prop)  --> -->
<!-- <!-- { --> -->
<!-- <!-- 	df %>% modelr::data_grid(condition, block, ts, n) %>%  --> -->
<!-- <!--     filter( --> -->
<!-- <!--       !(block == "block 1" & condition %in% c("transfer", "reward")), --> -->
<!-- <!--       !(block == "block 1" & ts >= 1 ), --> -->
<!-- <!--       !(block == "block 2" & ts < 1 ),) %>% --> -->
<!-- <!-- 		add_fitted_draws(my_model, re_formula = NA) %>% --> -->
<!-- <!-- 		ggplot( --> -->
<!-- <!-- 			aes( --> -->
<!-- <!-- 				x = ts*96, y = .value,  --> -->
<!-- <!-- 				colour = condition, fill = condition)) + --> -->
<!-- <!-- 		geom_hline(yintercept = 0.5, linetype = 2) +	 --> -->
<!-- <!-- 		stat_lineribbon(aes(y = .value), .width = c(0.90, 0.75, .50), alpha = 1/4, size =0.5) + --> -->
<!-- <!-- 		facet_grid(condition ~ n) +  --> -->
<!-- <!-- 		geom_vline(xintercept = c(96, 97), size = 1, colour = "darkgrey") + --> -->
<!-- <!-- 		geom_path( --> -->
<!-- <!-- 			data = rfp,  --> -->
<!-- <!-- 			aes(x = t, y = prop_fix, group = block),  --> -->
<!-- <!-- 			alpha = 0.33) + --> -->
<!-- <!-- 		scale_fill_brewer(name = "condition", palette = "Set2",) +  --> -->
<!-- <!-- 	  	scale_color_brewer(name = "condition", palette = "Dark2") +  --> -->
<!-- <!-- 	  	scale_x_continuous("trial", breaks = seq(1, 192, 48)) + --> -->
<!-- <!-- 	  	scale_y_continuous("prop. fix. hetero", breaks = seq(0, 1, 0.25)) + --> -->
<!-- <!-- 	  	coord_cartesian(ylim = c(0, 1)) +  --> -->
<!-- <!-- 	  	theme_tidybayes() +  --> -->
<!-- <!-- 	  	theme( --> -->
<!-- <!-- 	  		legend.position= "none",  --> -->
<!-- <!-- 	  		legend.box.background = element_rect(size=1)) --> -->
<!-- <!-- } --> -->
<!-- <!-- ``` --> -->

<!-- <!-- ```{r} --> -->
<!-- <!-- plot_model_predictions(d_strat, m_prior, rolling_fix_prop) --> -->
<!-- <!-- ``` --> -->


<!-- <!-- ## Posterior Predictions --> -->

<!-- <!-- ```{r timecourse-posterior, cache=TRUE} --> -->
<!-- <!-- # m_posterior <- brm( --> -->
<!-- <!-- # 	data = d_strat, --> -->
<!-- <!-- # 	hetero_fix ~ (0 + condition) * (0 +n) * block * ts  + (n | observer), --> -->
<!-- <!-- # 	family = "bernoulli", --> -->
<!-- <!-- # 	prior = model_priors, --> -->
<!-- <!-- # 	chains = 1) --> -->
<!-- <!-- #  --> -->
<!-- <!-- # m_posterior <- add_criterion(m_posterior, c("loo", "waic")) --> -->
<!-- <!-- #  --> -->
<!-- <!-- # saveRDS(m_posterior, "models/my_posterior.model") --> -->

<!-- <!-- m_posterior <- readRDS("models/my_posterior.model") --> -->
<!-- <!-- ``` --> -->


<!-- <!-- ```{r} --> -->
<!-- <!-- plot_model_predictions(d_strat, m_posterior, rolling_fix_prop) --> -->
<!-- <!-- ``` --> -->

<!-- <!-- ## Does Condition Have an Effect? --> -->

<!-- <!-- ```{r timecourse-posterior-drop-cd, cache=TRUE} --> -->
<!-- <!-- # m_posterior_drop_cd <- brm( --> -->
<!-- <!-- # 	data = d_strat, --> -->
<!-- <!-- # 	hetero_fix ~ (0 +n) * block * ts  + (n | observer), --> -->
<!-- <!-- # 	family = "bernoulli", --> -->
<!-- <!-- # 	prior = model_priors, --> -->
<!-- <!-- # 	chains = 1) --> -->
<!-- <!-- #  --> -->
<!-- <!-- # m_posterior_drop_cd <- add_criterion(m_posterior_drop_cd, c("loo", "waic") --> -->
<!-- <!-- #  --> -->
<!-- <!-- # saveRDS(m_posterior_drop_cd, "models/my_posterior.model") --> -->
<!-- <!-- #  --> -->
<!-- <!-- # ) --> -->

<!-- <!-- ``` --> -->

<!-- <!-- ```{r} --> -->
<!-- <!-- # model_weights(m_posterior, m_posterior_drop_cd) --> -->
<!-- <!-- ``` --> -->


<!-- <!-- <!-- ## Can we remove the 4 way interaction? --> --> -->

<!-- <!-- <!-- ```{r timecourse-posterior-drop4, cache=TRUE} --> --> -->
<!-- <!-- <!-- m_posterior_drop4 <- brm( --> --> -->
<!-- <!-- <!-- 	data = d_strat, --> --> -->
<!-- <!-- <!-- 	hetero_fix ~ (0 + condition) * (0 +n) * block * ts - (condition:n:block:ts) + (n | observer), --> --> -->
<!-- <!-- <!-- 	family = "bernoulli", --> --> -->
<!-- <!-- <!-- 	prior = model_priors, --> --> -->
<!-- <!-- <!-- 	chains = 1) --> --> -->

<!-- <!-- <!-- saveRDS(m_posterior_drop4, "models/my_posterior_drop4.model") --> --> -->

<!-- <!-- <!-- m_posterior_drop4 <- add_criterion(m_posterior_drop4, c("loo", "waic")) --> --> -->

<!-- <!-- <!-- ``` --> --> -->

<!-- <!-- <!-- ```{r} --> --> -->
<!-- <!-- <!-- model_weights(m_posterior, m_posterior_drop_cd, m_posterior_drop4) --> --> -->
<!-- <!-- <!-- ``` --> --> -->