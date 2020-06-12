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
	fixNum<13,
	fixNum>1,
	targSide=="absent") %>%
	mutate(
		propHard = if_else(side == "hetero", 1, 0)) %>%
	select(subj, version, trial, fixNum, propHard) -> d 


d %>% group_by(subj, fixNum) %>%
	summarise(y = mean(propHard)) %>%
	rename(x = "fixNum") -> d2 

ggplot(d2, aes(x = fixNum, y)) + geom_path() + facet_wrap(~subj)

dp <- filter(d2, subj == 14)

library(brms)
b <- c(2, 0.75)
x <- rnorm(100)
y <- rnorm(100, mean = b[1] * exp(b[2] * x))

dat1 <- data.frame(x, y)

prior1 <- prior(normal(2, 2), nlpar = "b0") + 
	prior(cauchy(0, 1), nlpar = "b1") + 
	prior(cauchy(0, 1), nlpar = "b2") 

m <- brm(
	bf(y ~ 1 + b2/(x-b0)^2 - b1/x, 
	b0 + b1 +b2 ~ 1|subj, nl = TRUE),
            data = d2, prior = prior1,
            control = list(adapt_delta = 0.99),
            warmup = 5000,
            iter = 10000)

d2$p <- predict(m)[,1]

ggplot(d2, aes(x=x, y=p)) + geom_path() + geom_point(aes(x, y)) + facet_wrap(~subj)
ggsave("nl_fits.png")


m <- brm(
	bf(propHard ~ 1 + b2/(fixNum-b0)^2 - b1/fixNum, 
	b0 + b1 +b2 ~ 1|subj, nl = TRUE),
            data = d, prior = prior1,
            control = list(adapt_delta = 0.99),
            family = "bernoulli",
            warmup = 10000,
            iter = 0000)

