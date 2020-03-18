
library(dplyr)
# TODO: check subjects to replace!
setwd("C:/Users/s03an7/Documents/GitHub/DeadlineExp/DeadlineExpFromGitHub")

saccInfo <- function(trialDat)
{
	# this is a funtion that calculates saccade amplitude and angle for a sequenece of fixations in a  trial
	nFix = max(trialDat$fixNum)
	saccInfo  = trialDat
	saccAmp2 = vector()
	theta = vector()
	for (f in 1:(nFix-1))
	{
		dx = trialDat$fixX[f] - trialDat$fixX[f+1]
		dy = trialDat$fixY[f] - trialDat$fixY[f+1]
		saccAmp2[f] 	= dx^2 + dy^2
		theta[f] 		= atan2(dx, dy)
	}
	saccAmp2[nFix] = NaN
	theta[nFix]    = NaN
	saccInfo$amp   = sqrt(saccAmp2)
	saccInfo$ang   = theta

	return(saccInfo)
}



# max fixation duration - remove trial if it is exceeded 
maxFixDur = 2000

# read in reaction time and acc data:

print("Processing RT and Acc data")
dat <- read.csv("data/rt18.txt", sep="\t")

dat = select(dat, subNum, version,completed, trialNo,targPresent,targSide, easySide, RT, response,accuracy)

# Turn categorical data into factor
dat$targPresent = as.factor(dat$targPresent)
levels(dat$targPresent) = c("absent", "present")
levels(dat$targSide) = c('left', 'right', 'absent')
dat$easySide = as.factor(dat$easySide)
levels(dat$easySide) = c("left", "right")
levels(dat$version) = c("T", "U")

# refdefine targSide relative to easySide
dat$targSideRel = as.factor(as.character(dat$easySide) == as.character(dat$targSide))
levels(dat$targSideRel) = levels(dat$targSideRel) = c("hetrogeneous", "homogeneous", "absent")
dat$targSideRel[which(dat$targPresent=="absent")] = "absent"
dat$trialUnique = factor(paste( dat$trialNo, dat$version))

# make a new, tidier version of dataframe only including the stuff we want!
rtdat = data.frame(subj=dat$subNum, trial=dat$trialNo, trialUnique=dat$trialUnique, targSide=dat$targSideRel, RT=dat$RT, acc=dat$accuracy, easySide=dat$easySide,completed=dat$completed, version=dat$version )
# we don't want to be looking at RTs for incorrect trials
#rtdat$RT[rtdat$acc==0] = NaN

# save!!!
saveRDS(rtdat,file="data/processedRTandAccData.Rda")
# remove data for now
rm(dat, rtdat)


#############################
# now read in fixation data #
#############################

print("Processing Fix data...")
dat <- read.csv("data/fixRtAcc.txt", header=T, sep="\t",
	colClass = c(
		"subNum"="factor",
		"version"  ="factor", 
		"completed" ="factor",  
		"trialNo"="numeric",
            "fixNo"="numeric",
		"xFix"="numeric", 
            "xFixFlipped"="numeric",
		"yFix" = "numeric",
		"fixStartTime" = "numeric",
		"fixEndTime" = "numeric",
		"targPresent" = "factor",
		"targSide" = "factor",
		"easySide" = "factor",
            "trialStart"="numeric",
		"accuracy" = "numeric"))
names(dat) = c("subj", "version", "completed", "trialNum","fixNum", "fixX","fixXflipped","fixY","fixOn","fixOff", "targPresent", "targSide", "easySide","trialStart","accuracy")

# Turn categorical data into factor
dat$targPresent = as.factor(dat$targPresent)
levels(dat$targPresent) = c("absent", "present")
levels(dat$targSide) = c('left', 'right', 'absent')
dat$easySide = as.factor(dat$easySide)
levels(dat$easySide) = c("left", "right")
levels(dat$version) = c("T", "U")

# refdefine targSide relative to easySide
dat$targSideRel = as.factor(as.character(dat$easySide) == as.character(dat$targSide))
levels(dat$targSideRel) = levels(dat$targSideRel) = c("hetrogeneous", "homogeneous", "absent")
dat$targSideRel[which(dat$targPresent=="absent")] = "absent"
dat$trialUnique = factor(paste( dat$trialNum, dat$version))

dat = select(dat, subj,  version, completed, trialNum,trialUnique, fixNum, easySide, fixX,fixXflipped, fixY, fixOn, fixOff, targPresent, targSideRel,accuracy)

# calcualte fixation durations
dat$fixDur = with(dat, fixOff - fixOn)


# #we want to filter out all incorrect trials!
# print("...removing fixation for incorrect trials and fix.dur exceptions")
# accdat = readRDS(file="data/processedRTandAccData.Rda")
# dat$acc = 0
# for (s in levels(dat$subj))
# {
# 	subjDat = filter(dat, subj==s)
#      subjDat$trialNum = factor(subjDat$trialNum)
# 	for (t in levels(subjDat$trialNum))
# 	{
#  		j = which(accdat$trial==t & accdat$subj==s)
#  		idx = which(dat$subj==s & dat$trialNum==t)
#      	if (accdat$acc[j]==1 & max(dat$fixDur[idx]) <= maxFixDur )
#      	{
#      		dat$acc[idx] = 1
#      	}
#      }
#  }
#  print(paste("... keeping ", 100*mean(dat$acc), "% of fixations"))
#  dat = filter(dat, acc==1)

fixdat=dat
 fixdat$fixX = fixdat$fixX + 512

 fixdat$fixY = fixdat$fixY


#
# get saccade info
#
#analysis of flipped trials, homogeneous on the left
fixdat$fixX<-fixdat$fixXflipped +512
print("...calcualting sacc amp and ang")
fixdat$saccAmp = NaN
fixdat$saccAng = NaN
for (s in levels(fixdat$subj))
{

	subjdat = fixdat[which(fixdat$subj==s),]
	subjdat$trialUnique = factor(subjdat$trialUnique)
	for (t in levels(subjdat$trialUnique))
	{
		if (length(which(fixdat$subj==s & fixdat$trialUnique==t))>0)
		{
			saccDat    = saccInfo(fixdat[which(fixdat$subj==s & fixdat$trialUnique==t),])		
			fixdat$saccAmp[which(fixdat$subj==s & fixdat$trialUnique==t)] = saccDat$amp
			fixdat$saccAng[which(fixdat$subj==s & fixdat$trialUnique==t)] = saccDat$ang	
			rm(saccDat)	
		}
	}

	rm(subjdat)
}
rm(s, t)

#
 dat = fixdat
fixdat = data.frame(subj=dat$subj, version=dat$version, completed=dat$completed, trial=dat$trialNum, targSide=dat$targSideRel, fixNum=dat$fixNum, fixX=dat$fixX, fixY=dat$fixY, fixOn=dat$fixOn, fixDur=dat$fixDur, saccAmp=dat$saccAmp, saccAng=dat$saccAng, easySide=dat$easySide,accuracy=dat$accuracy)



saveRDS(fixdat,file="data/processedFixData.Rda")
write.table(fixdat, "data/processedFixData.txt", sep=",")