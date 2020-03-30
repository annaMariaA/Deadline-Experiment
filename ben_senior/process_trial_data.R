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


d %>% select(-trialType, -targTheta, -targLocX, - targLocY) %>%
	mutate(
		deadline = as.factor(deadline),
		targetPresent = as_factor(targetPresent),
		targVar = round(targVar),
		response = as.factor(response)
		) -> d

summary(d)


