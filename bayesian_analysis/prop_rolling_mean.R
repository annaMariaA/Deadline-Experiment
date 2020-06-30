	
windowed_mean <- function(df, blk, dl, nf, ws) {
  
	x <- filter(df, block == blk, condition == cd, n == nf)$mean_hetero_fix
  	length(x)
  	return(roll_mean(x , n = ws, align = "center"))
}

rolling_fix_prop <- tibble(
	block = as.character(), 
	condition = as.character(),
	t = as.numeric(),
	n = as.numeric(), 
	prop_fix = as.numeric())

ws <- 11

for (n in 2:7) {
	for (blk in c("block 1", "block 2")) {
		for (cd in levels(dat_m$condition)) {
		    if (!(blk == "block 1" & cd == "reward")) {
     
  			wf <- windowed_mean(dat_agg, blk, cd, n, ws)
  
  			rolling_fix_prop %>% bind_rows(
  				tibble(
  					block = blk, 
  					condition = cd, 
  					t = 1:length(wf) + (ws+1)/2,
  					n = n, 
  					prop_fix = wf)) -> rolling_fix_prop
		    }
		}
	}
}

rolling_fix_prop %>% mutate(
	t = if_else(block == "block 2", t = t + 96, t)) -> rolling_fix_prop
