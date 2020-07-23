library(RcppRoll)

windowed_mean <- function(df, blk, dl, nf, ws) {
  
	x <- filter(df, block == blk, condition == cd, n == nf)$mean_hetero_fix
  	length(x)
  	return(roll_mean(x , n = ws, align = "center"))
}

rolling_fix_prop <- tibble(
	block = as.character(), 
	condition = as.character(),
	t = as.numeric(),
	n = as.character(),
	prop_fix = as.numeric())

dat_agg <- d_strat %>% group_by(block, condition, t, n) %>%
  summarise(mean_hetero_fix = mean(hetero_fix), .groups = "drop")

ws <- 11

for (cd in levels(dat_agg$condition)) 
  {
  for (n in unique(dat_agg$n))
    {
  	for (blk in c("block 1", "block 2")) 
  	  {    
  	  if (!(blk == "block 1" & cd %in% c("reward", "transfer"))) {
       
    			wf <- windowed_mean(dat_agg, blk, cd, n, ws)
    
    			rolling_fix_prop %>% bind_rows(
    				tibble(
    					block = blk, 
    					condition = cd, 
    					t = 1:length(wf) + (ws+1)/2,
    					n = paste("fixation", n), 
    					prop_fix = wf)) -> rolling_fix_prop
  		   }
  	}
  }
}


rolling_fix_prop %>% mutate(
	t = if_else(block == "block 2", t = t + 96, t)) -> rolling_fix_prop
