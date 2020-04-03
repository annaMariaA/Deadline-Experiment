library(ggplot2)
library(dplyr)
# TODO: check subjects to replace!
 setwd("C:/Users/s03an7/Documents/GitHub/PolygonLatest")


# read in reaction time and acc data:
# this will allow us to remove fixation data for incorrect trials
print("Processing RT and Acc data")
dat <- read.csv("accuracyPilot.txt", sep="\t")

dat = select(dat,subNum,trialNo, trialType, targSide, easySide, accuracy)
levels(dat$targSide) <- c("absent","present","present")

levels(dat$easySide) = c("easy", "hard")

dat$subNum = as.factor(dat$subNum)
levels(dat$subNum)

accdat  = aggregate(data=dat, accuracy ~ +subNum + easySide, FUN="mean")
ggplot(accdat, aes(x=subNum, y=accuracy*100, color=easySide)) + geom_point(shape=2, size =3, stroke=2)+
  #scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  scale_colour_discrete (name="search type")+# geom_smooth(method=lm,   # Add linear regression lines
  #             se=FALSE)+
  
  scale_x_discrete(name = "subject number",labels=c("1","2","3","4","5","6","7","8","9","10"))+#, breaks = c(seq(1,8,1)), limits = c(1,8))+
  
  scale_y_continuous(name = "accuracy (%)")
ggsave("accuracyPilot.jpg", width=6, height=4)


write.table(accdat, "aggregatedAccuracyPostSearch.txt", sep=",")


# save!!!
saveRDS(dat,file="processedRTandAccData.Rda")

# remove data for now
rm(dat)




