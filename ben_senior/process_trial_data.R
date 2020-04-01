library(tidyverse)

files <- dir("data/txt")


my_cols <- cols(
  person = col_double(),
  deadline = col_double(),
  block = col_double(),
  trialNum = col_character(),
  trialType = col_character(),
  targetPresent = col_character(),
  targTheta = col_double(),
  targLocX = col_double(),
  targLocY = col_character(),
  targVar = col_double(),
  response = col_character(),
  rt = col_double())

d <- tibble()

for (pp in c(2, 3, 5, 6, 7, 8, 200, 300, 500, 600, 700, 800)) {
 
	dp <- read_csv(paste("data/txt/results", pp, ".txt", sep = ""), col_types = my_cols)
dp$trialNum <- rep(1:30, 2)


	d <- bind_rows(d, dp)

}


d %>% select(-trialType, -targTheta, targLocX, - targLocY, -targetPresent) %>%
	mutate(
		deadline = as.factor(deadline),
		targVar = as.factor(round(targVar)),		
    response = as.numeric(response),
    targSide = if_else(targLocX < 16, "left", "right")
		) -> d

levels(d$targVar) = c("easy", "hard", "absent")

d$response[which(d$response ==  0)] = Inf
d$response[which(d$response == -1)] = 0

d$correct = d$response
d$correct[which(d$targVar == "absent")] = 1 - d$correct[which(d$targVar == "absent")]


# check accuracy

d %>% filter(is.finite(response)) %>%
  select(-response) %>%
  group_by(person, deadline, targVar) %>%
  summarise(
    accuacy = mean(correct),
    mean_rt = mean(rt),
    median_rt = median(rt)) %>%
  ungroup() %>%
  mutate(
    session = if_else(person %in% c(200, 300, 500, 600, 700, 800), 2, 1),
    person = if_else(person < 100, person, person/100)) %>%
  select(session, person, deadline, targVar, accuacy, median_rt) -> d_agg


write_csv(d_agg, "acc_and_rt.csv")

ggplot(d_agg, aes(x = targVar, y = accuacy, fill = deadline)) + geom_boxplot()


library(tidyverse)

read_delim("data/fixData.txt", delim = "\t") %>% 
  rename(
    person = "subNum",
    trial = " trialNo",
    n = " fixNo", 
    x = " xFix",
    y = " yFix",
    t_start = " fixStartTime",
    t_end = " fixEndTime") %>%
  mutate(
    person = as.numeric(person),
    person = if_else(person < 100, person, person/100),
    trial = as.numeric(trial),
    n = as.numeric(n),
    x = as.numeric(x),
    y = as.numeric(y),
    t_start = as.numeric(t_start),
    t_end = as.numeric(t_end)) %>%
  filter(
    person %in% 
      c(2,  3, 5, 6, 7, 8, 200, 300, 500, 600, 700, 800)) -> d_fix

# code up fixations as either hard or easy

