library(brms)
library(tidyverse)
library(tidybayes)
library(RcppRoll)

cbPalette <- c("#56B4E9", "#E69F00")

fixdat <- as_tibble(readRDS("../data/processedFixData.Rda"))

subjectsToRemove = c(22,19,12)#22 and 19 accuracy on homogenous trials below 50%, 12 RT on homogenous trials over 8s
fixdat = (fixdat[!(fixdat$subj%in% subjectsToRemove),])
fixdat$subj = as.factor(fixdat$subj)


fixdat$order = paste(fixdat$version, fixdat$completed, sep="")
fixdat$order[which(fixdat$order=="T 2")] = "UT"
fixdat$order[which(fixdat$order=="U 2")] = "TU"
fixdat$order[which(fixdat$order=="T 1")] = "TU"
fixdat$order[which(fixdat$order=="U 1")] = "UT"

fixdat$order = as.factor(fixdat$order)
levels(fixdat$order)

#fixdat$subj = as.factor(paste(fixdat$order, fixdat$subj))

# classify every fixation as homo (right), central, or hetero (left)
centralWidth = 64 #change to 1 visual degree
fixdat$side = 'central'
fixdat$side[which(fixdat$fixX <(512-centralWidth/2))] = "hetero"
fixdat$side[which(fixdat$fixX >(512+centralWidth/2))] = "homo"
fixdat$side = as.factor(fixdat$side)

#look at accuracte trials only
fixdat = fixdat[which(fixdat$accuracy == 1), ]

dat_m <- fixdat %>% 
	select(
		subject = "subj", 
		deadline = "version", 
		block = "completed",
		t = "trial", 
		n = "fixNum",
		side) %>%
	filter(n > 1, n < 7) %>%
	mutate(
		n = as.factor(n),
		hetero_fix = as.numeric(side == "hetero"),
		block = as_factor(block),
		block = fct_recode(block, "block 1" = " 1", "block 2" = " 2"),
		t = if_else(block == "block 2", t + 96, t)) %>%
	select(-side)
	
	
dat_agg <- dat_m %>% group_by(block, deadline, t, n) %>%
	summarise(mean_hetero_fix = mean(hetero_fix))

# m_block <- brm(
# 	data = dat_m,
# 	hetero_fix ~ block  * n + (n | subject), 
# 	family = "bernoulli")
# m_block <- add_criterion(m_block, c("loo", "waic"))

# m_trial <- brm(
# 	data = dat_m,
# 	hetero_fix ~ t * n + (n | subject), 
# 	family = "bernoulli")
# m_trial <- add_criterion(m_trial, c("loo", "waic"))


# m_block_trial <- brm(
# 	data = dat_m,
# 	hetero_fix ~ block * t * n + (n | subject), 
# 	family = "bernoulli")
# m_block_trial <- add_criterion(m_block_trial, c("loo", "waic"))

# m_deadline <- brm(
# 	data = dat_m,
# 	hetero_fix ~ deadline + block * t * n + (n | subject), 
# 	family = "bernoulli")
# m_deadline <- add_criterion(m_deadline, c("loo", "waic"))

# m_deadline2 <- brm(
# 	data = dat_m,
# 	hetero_fix ~ deadline * block * t * n + (n | subject), 
# 	family = "bernoulli")
# m_deadline2 <- add_criterion(m_deadline2, c("loo", "waic"))
 
# saveRDS(m_deadline2, "my.model")
m_deadline2 <- readRDS("my.model")

# m_deadline3 <- brm(
# 	data = dat_m,
# 	hetero_fix ~ deadline * block * t * n - deadline:block:t:n + (n | subject), 
# 	family = "bernoulli")
# m_deadline3 <- add_criterion(m_deadline3, c("loo", "waic"))

# mw <- model_weights(m_deadline2, m_deadline3, m_deadline, m_block_trial, m_block, m_trial)

windowed_mean <- function(df, blk, dl, nf, ws = 5) {
	x <- filter(df, block == blk, deadline == dl, n == nf)$mean_hetero_fix
	return(roll_mean(x , ws))
}

rolling_fix_prop <- tibble(
	block = as.character(), 
	deadline = as.character(),
	t = as.numeric(),
	n = as.numeric(), 
	prop_fix = as.numeric())

for (n in 2:6) {
	for (blk in c("block 1", "block 2")) {
		for (dl in c("U", "T")) {

			wf <- windowed_mean(dat_agg, blk, n, 5)

			rolling_fix_prop %>% bind_rows(
				tibble(
					block = blk, 
					deadline = dl, 
					t = 1:length(wf),
					n = n, 
					prop_fix = wf)) -> rolling_fix_prop
		}
	}
}



dat_m %>% 
	group_by(block) %>%
	modelr::data_grid(deadline, n, t) %>%
	# mutate(n = fct_rev(n)) %>%
	add_fitted_draws(m_deadline2, re_formula = NA) %>%
	ggplot(
		aes(
			x = t, y = .value, 
			colour = deadline, fill = deadline)) +
	geom_hline(yintercept = 0.5, linetype = 2) +	
	stat_lineribbon(aes(y = .value), .width = c(0.75, .50), alpha = 1/4) +
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
  		legend.box.background = element_rect(size=1))
  # geom_point(data = dat_agg, aes(y = mean_hetero_fix), alpha = 0.5)
ggsave("scratch/strat_over_trials.png", width = 10, height = 5)


# get_variables(m)
# m %>% gather_draws(b_trial,  `b_trial:n3`, `b_trial:n4`, `b_trial:n5`) %>%
# 	ggplot(aes(x = .value, fill = .variable)) + geom_density(alpha = 0.33)

# dat_p$p <- predict(m, dat_p)[, 1]


# ggplot(dat_p, aes(x  = trial, y = p, colour = n )) + geom_point()
