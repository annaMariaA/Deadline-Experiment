
library(dplyr)
# TODO: check subjects to replace!
 setwd("C:/Users/Marcin/Desktop/EA  texture search")

# read in reaction time and acc data:
# this will allow us to remove fixation data for incorrect trials
print("Processing RT and Acc data")
dat <- read.csv("DissRawData/dataFlipped.txt", sep="\t")
names(dat) = c("condition", "subj", "trialNum","fixNo","xFix","xFixFlip","yFix", "targSide", "easySide", "accuracy", "RT")
dat = select(dat, subj, trialNum, targSide, easySide, accuracy, RT)

# Turn categorical data into factor
levels(dat$targSide) = c('absent', 'left', 'right')
levels(dat$easySide) = c("left", "right")

dat$subj = as.factor(dat$subj)
levels(dat$subj)

# refdefine targSide relative to easySide
dat$targSideRel = as.factor(as.character(dat$easySide) == as.character(dat$targSide))
levels(dat$targSideRel) = levels(dat$targSideRel) = c("hard", "easy", "absent")
dat$targSideRel[which(dat$targSide=="absent")] = "absent"

# make a new, tidier version of dataframe only including the stuff we want!
dat = select(dat, subj, trialNum, targSideRel, RT, accuracy)
names(dat$targSideRel)="targSide"
write.table(dat, "processedRTandAccData.txt", sep=",")
# we don't want to be looking at RTs for incorrect trials
#rtdat$RT[rtdat$acc==0] = NaN

# save!!!
saveRDS(dat,file="data/processedRTandAccData.Rda")
write.table(dat, "data/processedRTandAccData.Rda", sep=",")
# remove data for now
rm(dat)


#############################
# now read in fixation data #
#############################

print("Processing Fix data...")
dat <- read.csv("fix9.txt", header=T, sep="\t",
	colClass = c(
	  "condition"="factor",
		"subNum"="factor", 
		"trialNo"="numeric", 
		"fixNo"="numeric", 
		"xFix" = "numeric",
		"xFixFlip" = "numeric",
		"yFix" = "numeric",
		"targSide" = "factor",
		"trialType" = "factor",
		"easySide"="factor",
		"accuracy"="numeric",
		"RT"="numeric"))
names(dat) = c("condition","subj", "trialNum","fixNum", "fixX","xFixFlip","fixY","targSide", "trialType","easySide","accuracy","RT")

levels(dat$targSide) = c("absent","left", "right")
levels(dat$easySide) = c("left", "right")

#dat$row[which(dat$targPresent=='absent')] = NaN;
#dat$column[which(dat$targPresent=='absent')] = NaN;

# refdefine targSide relative to easySide - ie, hetrogeneous array always on left
dat$targSideRel = as.factor(as.character(dat$easySide) == as.character(dat$targSide))
levels(dat$targSideRel) = levels(dat$targSideRel) = c("hard", "easy", "absent")
dat$targSideRel[which(dat$targSide=="absent")] = "absent"
dat = select(dat, subj, trialNum, fixNum, fixX, xFixFlip, targSideRel)
names(dat$targSideRel)="targSide"

saveRDS(dat,file="processedFixData.Rda")
write.table(dat, "processedFixData.txt", sep=",")




