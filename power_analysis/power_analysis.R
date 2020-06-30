library(tidyverse)

# Import data from Clarke et al (2020)
fix_dat <- readRDS("clarke2020_qjep_data/processedFixationData.Rda") 
trl_dat <- readRDS("clarke2020_qjep_data/processedRTandAccData.Rda")

# only take correct trials
fix_dat <- filter(left_join(fix_dat, trl_dat), accuracy == 1)
trl_dat <- filter(trl_dat, accuracy == 1)

# remove people with poor target easy/absent accuracy
fix_dat <- filter(fix_dat, !(observer %in% c(4, 21, 33, 56, 58)))
trl_dat <- filter(trl_dat, !(observer %in% c(4, 21, 33, 56, 58)))

# remove fixations falling outside of simulus
fix_dat <- filter(fix_dat, is.finite(x), is.finite(y))

# classify every fixation as homo (left), central, or hetro (right)
centralWidth <- 0.1 # used to be 64 pixels! #change to 1 visual degree
fix_dat$side <- 'central'
fix_dat$side[which(fix_dat$x <(0-centralWidth/2))] <- "homo"
fix_dat$side[which(fix_dat$x >(0+centralWidth/2))] <- "hetero"

fix_dat$side <- as_factor(fix_dat$side)

fix_dat %>% filter(targSide == "absent", n > 1, n < 6) %>%
  group_by(observer, session, trial) %>%
  summarise(prop_hetero = mean(side == "hetero")) %>%
  mutate(observer = fct_drop(observer)) -> dat


dat %>% group_by(observer, session) %>% summarise(mf = mean(prop_hetero)) %>%
  pivot_wider(names_from = session, values_from = mf) %>% 
  ggplot(aes(x = a, y = b)) + geom_point()

library(lme4)
library(lmerTest)

sample_n_trials <- function(df, obs, n_t) {
	# do sep for each session
	d_out <- tibble()

	for (sesh in c("a", "b")) {

				trls <- sample(
			unique(
				filter(df, 
					observer == obs,
					session == sesh)$trial), replace = TRUE,
			n_t)

		d_out <- bind_rows(
			d_out,
			filter(df,  observer == obs, session == sesh, trial %in% trls))
		}

	return(d_out)
}

sample_n_observers <- function(df, n_p, n_t) {
  
  # Get subset of observers
  obs <- sample(levels(df$observer), n_p)
  d <- filter(df, observer %in% obs)

  # now get a subset of trials for each person
  d_out <- map_df(obs, sample_n_trials, df = d, n_t = n_t)

  return(d_out)

}

can_we_find_effect <- function(n_p, n_t, dat=dat, a = 0.05) {

	d <- sample_n_observers(dat, n_p, n_t)
	m <- lmer(prop_hetero ~ session + (1|observer) , d)
	d_out <- tibble(people = n_p, trials = n_t, sig = summary(m)$coefficients[2, 5] < a)
	return(d_out)  
}

d_power <- tibble()

for (n_obs in seq(12, 26, 2)) {
	print(n_obs)
	for (trls in seq(36, 60, 12)) {
		for (iter in 1:500) 
		{		
			d_power <- bind_rows(
 					d_power, 
 					can_we_find_effect(n_obs, trls, dat)) 
		}
	}
}

d_power %>% group_by(people, trials) %>%
	summarise(power = mean(sig)) %>% 
	mutate(trials = as.factor(trials)) %>%
		ggplot(aes(x = people, y = power,colour = trials)) + 
		geom_point() + 
		geom_smooth(se = F) + 
		geom_hline(yintercept = 0.8) + 
		geom_vline(xintercept = 18) + 
		theme_bw()
ggsave("tmp.png")
