library(tidyverse)

read_delim("data/fixData.txt", delim = "\t") %>% 
	rename(
		participant = "subNum",
		trial = " trialNo",
		n = " fixNo", 
		x = " xFix",
		y = " yFix",
		t_start = " fixStartTime",
		t_end = " fixEndTime") %>%
	mutate(
		participant = as.numeric(participant),
		trial = as.numeric(trial),
		n = as.numeric(n),
		x = as.numeric(x),
		y = as.numeric(y),
		t_start = as.numeric(t_start),
		t_end = as.numeric(t_end)) %>%
	filter(
		participant %in% 
			c(2,  3, 5, 6, 7, 8, 200, 300, 500, 600, 700, 800)) -> d_fix


			 