library(tidyverse)
library(ggthemes)
library(brms)

dat <- read_delim("../anna_data/Rtacc.txt", delim = "\t")

names(dat) <- c('condition', 'person', 'trial_number', 'targ_side', 'trial_type', 'easy_side', 'accuracy', 'rt')

#dat$targ_side <- as_factor(dat$targ_side)
dat$trial_type <- as_factor(dat$trial_type)
dat$easy_side <- as_factor(dat$easy_side)

# recode to target easy/hard/abset

targ_side <- ifelse(dat$targ_side == dat$easy_side, 'easy', 'hard')
targ_side[dat$targ_side == ' absent'] <- 'absent'

dat$targ_side <- as_factor(targ_side)
dat$targ_side <- fct_relevel(dat$targ_side, 'absent')

levels(dat$easy_side) <- c("upper", "lower")

(dat %>% group_by(person, targ_side, easy_side) %>%
    summarise(
      accuracy = mean(accuracy)
    )) -> acc_dat

plt <- ggplot(acc_dat, aes(x = targ_side, y = accuracy, fill = easy_side ))
plt <- plt + geom_bar(stat = 'identity', position = position_dodge())
plt <- plt + facet_wrap(~ person, nrow = 5)
plt <- plt + scale_fill_ptol() + theme_bw()
plt <- plt + theme(
  axis.title.x = element_blank(),
  legend.title = element_blank(),
  legend.justification=c(1,0),
  legend.position=c(1,-0.04))
ggsave("scratch/acc_each_person.pdf", width=8, height=6)

# remove people with less than 75% acc on TA trials
filter(aggregate(accuracy ~ targ_side + person, data = dat, FUN = mean), targ_side == 'absent')
people_to_remove <- c(6, 11, 14)

(acc_dat %>% 
    filter(!(person %in% people_to_remove)) %>%
    group_by(targ_side, easy_side) %>%
    summarise(accuracy = mean(accuracy))) -> acc_dat

plt <- ggplot(acc_dat, aes(x = targ_side, y = accuracy, fill = easy_side ))
plt <- plt + geom_bar(stat = 'identity', position = position_dodge())
plt <- plt + scale_fill_ptol("easy side: ") + theme_bw()
plt <- plt + scale_x_discrete(expand = c(0, 0))
plt <- plt + scale_y_continuous(limits = c(0, 1), expand = c(0.0, 0.0))
plt <- plt + theme(
  axis.title.x = element_blank(),
  panel.grid = element_blank(),
  legend.position = "top")
ggsave("scratch/acc_overall.pdf", width = 3, height = 3)

# now we look at RT data


(dat %>% 
    select(-trial_type) %>%
    filter(! person %in% people_to_remove) %>%
    mutate(rt_log2 = log(rt, 2))) -> rt_dat

m1 <- brm(
  data = rt_dat,
  rt_log2 ~ targ_side * easy_side + (targ_side + easy_side|person),
  save_all_pars = TRUE)

m2 <- brm(
  data = rt_dat,
  rt_log2 ~ targ_side + (targ_side + easy_side|person),
  save_all_pars = TRUE)


bayes_factor(m1, m2)

p_dat = tibble(
  easy_side = rep(c('upper', 'lower'), 3),
  targ_side = rep(c('absent', 'easy', 'hard'), each = 2),
  x = seq(0.75, 3.25, 0.5),
  lower = NA, upper =NA)

post <- posterior_samples(m)

p_dat$lower[1] <- rethinking::HPDI(post$b_Intercept, prob = 0.95)[1]
p_dat$upper[1] <- rethinking::HPDI(post$b_Intercept, prob = 0.95)[2]

p_dat$lower[2] <- rethinking::HPDI(post$b_Intercept + post$b_easy_sidelower, prob = 0.95)[1]
p_dat$upper[2] <- rethinking::HPDI(post$b_Intercept + post$b_easy_sidelower, prob = 0.95)[2]


p_dat$lower[3] <- rethinking::HPDI(post$b_Intercept + post$b_targ_sideeasy, prob = 0.95)[1]
p_dat$upper[3] <- rethinking::HPDI(post$b_Intercept + post$b_targ_sideeasy, prob = 0.95)[2]

p_dat$lower[4] <- rethinking::HPDI(post$b_Intercept + post$b_targ_sideeasy + post$b_easy_sidelower, prob = 0.95)[1]
p_dat$upper[4] <- rethinking::HPDI(post$b_Intercept + post$b_targ_sideeasy + post$b_easy_sidelower, prob = 0.95)[2]


p_dat$lower[5] <- rethinking::HPDI(post$b_Intercept + post$b_targ_sidehard, prob = 0.95)[1]
p_dat$upper[5] <- rethinking::HPDI(post$b_Intercept + post$b_targ_sidehard, prob = 0.95)[2]

p_dat$lower[6] <- rethinking::HPDI(post$b_Intercept + post$b_targ_sidehard + post$b_easy_sidelower, prob = 0.95)[1]
p_dat$upper[6] <- rethinking::HPDI(post$b_Intercept + post$b_targ_sidehard + post$b_easy_sidelower, prob = 0.95)[2]


factor_width = 0.20

rt_dat$x <- 0
rt_dat$x <- ifelse(rt_dat$targ_side == 'absent', 1, 0)
rt_dat$x <- ifelse(rt_dat$targ_side == 'easy', 2, rt_dat$targ_side)
rt_dat$x <- ifelse(rt_dat$targ_side == 'hard', 3, rt_dat$targ_side)

rt_dat$x <- ifelse(rt_dat$easy_side == 'upper', rt_dat$x - 0.25, rt_dat$x + 0.25)

plt <- ggplot(rt_dat, aes(x = x, y = rt_log2, colour = easy_side, shape = easy_side))
plt <- plt + geom_jitter(width = factor_width, alpha = 0.25)
plt <- plt + theme_bw() + theme(
  axis.title.x=element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.minor = element_blank(),
  legend.position="none")
plt <- plt + scale_x_continuous(
  name = 'target condition',
  breaks = 1:3, 
  labels = c('absent', 'easy', 'hard'))
plt <- plt + scale_y_continuous(
  name = 'reaction time', 
  breaks = -1:6, 
  labels = 2^(-1:6), expand = c(0,0))
plt <- plt + scale_colour_ptol()

plt <- plt + geom_errorbar(data = p_dat, 
                  aes(ymin = lower, ymax = upper, y = NULL), colour = 'black')
plt
ggsave("scratch/rt.pdf", width = 5, height =3)

