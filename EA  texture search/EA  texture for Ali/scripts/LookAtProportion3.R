library(dplyr)
library(ggplot2)
library(binom)
library(lme4)
setwd("C:/Users/s03an7/Documents/GitHub/Polygon/Polygon10by8")

fixdat = readRDS(file="data/processedFixData.Rda")
cbPalette <- c("#56B4E9", "#E69F00")
fixdat$fixX= fixdat$xFixFlip + 512
# classify every fixation as homo (right), central, or hetro (left)
centralWidth = 64 #change to 1 visual degree
fixdat$side = 'central'

fixdat$side[which(fixdat$fixX <(512-centralWidth/2))] = "hard"
fixdat$side[which(fixdat$fixX >(512+centralWidth/2))] = "easy"
fixdat$side = as.factor(fixdat$side)

aggData = (filter(fixdat, 
  side!="central", 
  fixNum<7, 
  fixNum>1, 
  targSideRel=="absent")
  %>% group_by(fixNum,subj)
    %>% summarise(
     propHard=mean(side=="hard"), 
     nTrials=length(trialNum),
     lower = binom.confint(propHard*nTrials,nTrials, method='wilson')$lower,
     upper = binom.confint(propHard*nTrials,nTrials, method='wilson')$upper))
accdat  = aggregate(data=aggData, propHard ~ subj, FUN="mean")
write.csv(accdat, "ProportionsSearch.txt", row.names=F)

plt = ggplot(aggData, aes(x=fixNum, y=propHard, ymin=lower, ymax=upper))
plt = plt + geom_point(color="#56B4E9") + geom_path(color="#56B4E9") + geom_errorbar(color="#56B4E9")
plt = plt + theme_bw() + facet_wrap(~subj, nrow=2)
plt = plt + scale_x_continuous(name="fixation number", breaks=c(2,3,4,5,6))
plt = plt + scale_y_continuous(name="proportion of fixations to hard side",limits = c(0, 1))
ggsave("plots/proportionInd.pdf", width=7, height=3.8)
ggsave("plots/proportionInd.jpg", width=7, height=3.8)
####now the same but mean for the two groups
aggData = (filter(fixdat, 
  side!="central", 
  fixNum<7, 
  fixNum>1, 
  targSideRel=="absent") 
  %>% group_by(fixNum)
    %>% summarise(
     propHard=mean(side=="hard"), 
     nTrials=length(trialNum),
     lower = binom.confint(propHard*nTrials,nTrials, method='wilson')$lower,
     upper = binom.confint(propHard*nTrials,nTrials, method='wilson')$upper))


plt = ggplot(aggData, aes(x=fixNum, y=propHard, ymin=lower, ymax=upper))
plt = plt + geom_point(color="#56B4E9") + geom_path(color="#56B4E9") + geom_errorbar(color="#56B4E9")
plt = plt + theme_bw()
plt = plt + scale_x_continuous(name="fixation number", breaks=c(2,3,4,5,6))
plt = plt + scale_y_continuous(name="proportion of fixations to hard side",limits = c(0.5, 1))
ggsave("meanFixSide.pdf", width=4.2, height=2.8)
ggsave("meanFixSide.jpg", width=4.2, height=2.8)



####now mean position on horizontal axis
fixdat$fixX = fixdat$fixX - 512
fxdat1 = filter(fixdat, 
	targSide=="absent") 
xdat1 = (fxdat1 
		%>% group_by(fixNum, subj,condition) 
		%>% summarise(
			meanX=mean(fixX), 
			nFix=length(fixX)))

write.csv(xdat1, "data/xdat1.txt", row.names=F)

xdat1 = filter(xdat1, fixNum<=6)
plt = ggplot(xdat1, aes(y=meanX, x=fixNum, colour=condition))
plt = plt + geom_point(aes(group=subj),position=position_jitter(height=0.01, width=0.1))
plt = plt + geom_smooth(se=F)
plt = plt + scale_y_continuous(name="mean x position of fixation", expand=c(0,0), limits=c(-400,400))
plt = plt + scale_x_continuous(name="fixation number",breaks=c(1,2,3,4,5,6,7,8), expand=c(0,0.01))
plt = plt + scale_color_manual(labels = c("icons", "lines"),values = cbPalette )
ggsave("plots/meanHorizontalPos.pdf", width=7, height=4)
ggsave("plots/meanHorizontalPos.jpg", width=7, height=4)

#########calculate test re-test reliability
fixdat$fixX = fixdat$fixX - 512
fxdat1 = filter(fixdat, 
	targSide=="absent",
      fixNum<=8) 
meanX =aggregate(data=fxdat1, fixX ~ subj + version + completed, FUN="mean")
write.csv(meanX, "data/meanXreliability.txt", row.names=F)











#  now we want to compare the strategy for trials which timed out, v those that didn't.
fixdat$timedOut = FALSE
for (s in levels(fixdat$subj))
{
  for (t in levels(fixdat$trial))
  {
    
   
    trlDat = filter(fixdat, subj==s, trial==t, version=="T")
if (nrow(trlDat)>0)
      {fixdat$timedOut[which(fixdat$subj==s & fixdat$trial==t, version=="T")] = rep(max(trlDat$fixOn)>4000, nrow(trlDat))}
}
}


aggData = (filter(fixdat, version=="T", side!="central", fixNum<6, fixNum>1, targSide=="absent") 
  %>% group_by(fixNum, subj, timedOut) 
    %>% summarise(
     propHetro=mean(side=="hetro"), 
     nTrials=length(trial),
     lower = binom.confint(propHetro*nTrials,nTrials, method='wilson')$lower,
     upper = binom.confint(propHetro*nTrials,nTrials, method='wilson')$upper))

plt = ggplot(aggData, aes(x=fixNum, y=propHetro, ymin=lower, ymax=upper, colour=timedOut))
plt = plt + geom_point() + geom_path() + geom_errorbar()
plt = plt + theme_bw() + facet_wrap(~subj, nrow=2)
plt = plt + scale_x_continuous(name="fixation number", breaks=c(2,4,6,8,10))
plt = plt + scale_y_continuous(name="proportion of fixations to heterogeneous side")
ggsave("FixXtimedOut.pdf", width=9, height=4)


aggregate(trial ~ subj+targSide)
