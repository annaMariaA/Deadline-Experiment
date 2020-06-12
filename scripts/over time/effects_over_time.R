library(tidyverse)

fixdat = readRDS(file="../../data/processedFixData.Rda")
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

# classify every fixation as homo (right), central, or hetro (left)
centralWidth = 64 #change to 1 visual degree
fixdat$side = 'central'
fixdat$side[which(fixdat$fixX <(512-centralWidth/2))] = "hetero"
fixdat$side[which(fixdat$fixX >(512+centralWidth/2))] = "homo"
fixdat$side = as.factor(fixdat$side)

#look at accuracte trials only
fixdat <- as_tibble(fixdat[which(fixdat$acc==1),])

fixdat %>% filter(
	side!="central", 
	fixNum<7,
	fixNum>1,
	targSide=="absent") %>%
	mutate(
		fixNum = as.factor(fixNum),
		propHard = if_else(side == "hetero", 1, 0)) %>%
	select(subj, version, trial, fixNum, propHard) -> d 


d %>% group_by(fixNum, version, trial) %>%
	summarise(p = mean(propHard)) %>%
	ggplot(aes(x = trial, y = p, colour = fixNum)) + 
		geom_point() +
		geom_smooth() + facet_wrap(~ version)


library(brms)
m <- brm(
	data = d, 
	propHard ~ fixNum * scale(trial) + (fixNum|subj), 
	family = binomial)

summary(m)

nd <- modelr::data_grid(d, fixNum, trial)

nd$p <- predict(m, nd, re.form = ~0, type = "response")

ggplot(nd, aes(x = trial, y = p, colour = fixNum)) + geom_path() +
	ggsci::scale_color_startrek(name = "fixation number") + ggthemes::theme_solarized() +
	scale_x_continuous("trial number") + 
	scale_y_continuous("probability of fixating the hard side", limits = c(0.5, 1), expand = c(0, 0)) +
ggsave("example.png", width = 4, height = 4)
