library(dplyr)
library(ggplot2)
library(binom)
library(lme4)
#setwd("C:/Users/Marcin/Documents/Anna_Work/GitHub_Current_Projects/DeadlineExp/DeadlineExpFromGitHub/")
cbPalette <- c("#56B4E9", "#E69F00")


fixdat = readRDS(file="data/processedFixData.Rda")
#subjectsToRemove = c(22,19,12)#22 and 19 accuracy on homogenous trials below 50%, 12 RT on homogenous trials over 8s
#fixdat = (fixdat[!(fixdat$subj%in% subjectsToRemove),])
fixdat$subj = as.factor(fixdat$subj)


fixdat$order = paste(fixdat$version, fixdat$completed, sep="")
#make trial unique
fixdat$trial[which(fixdat$order=="T 2")] = fixdat$trial[which(fixdat$order=="T 2")] +96
fixdat$trial[which(fixdat$order=="U 2")] = fixdat$trial[which(fixdat$order=="U 2")] +96

fixdat$order[which(fixdat$order=="T 2")] = "UT"
fixdat$order[which(fixdat$order=="U 2")] = "TU"
fixdat$order[which(fixdat$order=="T 1")] = "TU"
fixdat$order[which(fixdat$order=="U 1")] = "UT"

fixdat$order = as.factor(fixdat$order)
levels(fixdat$order)


# classify every fixation as homo (right), central, or hetro (left)
centralWidth = 64 #change to 1 visual degree
fixdat$side = 'central'
fixdat$side[which(fixdat$fixX <(512-centralWidth/2))] = "hetro"
fixdat$side[which(fixdat$fixX >(512+centralWidth/2))] = "homo"
fixdat$side = as.factor(fixdat$side)

#look at accurate trials only
fixdat = fixdat[which(fixdat$acc==1),]

aggData = (filter(fixdat, 
  side!="central", 
  fixNum<7, 
  fixNum>1, 
  targSide=="absent") 
  %>% group_by(subj,order, version,trial)
    %>% summarise(
     propHetro=mean(side=="hetro"), 
     nTrials=length(trial),
     lower = binom.confint(propHetro*nTrials,nTrials, method='wilson')$lower,
     upper = binom.confint(propHetro*nTrials,nTrials, method='wilson')$upper))

library(lmerTest)
model1 <- lmer(propHetro~version*order +(1|subj)+(1|trial) , data=aggData)    
summary(model1)
coef(summary(model1))
aov<- anova(model1)
aov

meanSubj =aggregate(data=aggData, propHetro ~  version + order+subj, FUN="mean")
model2 <- lmer(propHetro~version*order +(1|subj) , data=meanSubj)    
summary(model2)
coef(summary(model2))
aov2<- anova(model2)
aov2


subjOrder <- aggData %>%
  group_by(subj) %>%
  summarize(meanProp=mean(propHetro))%>%
      arrange((meanProp))
aggData$subj = factor(aggData$subj,levels(aggData$subj)[c(12,7,13,11,3,9,15,18,5,6,10,17,16,2,8,14,1,4)])


aggregateFix=aggregate(data=aggData, propHetro ~ subj + completed+version, FUN="mean")
write.csv(aggregateFix, "data/aggregateFixProp.txt", row.names=F)



plt = ggplot(aggData, aes(x=fixNum, y=propHetro, ymin=lower, ymax=upper, colour=version))
plt = plt + geom_point() + geom_path() + geom_errorbar()
plt = plt + theme_bw() + facet_wrap(~subj, nrow=2) #+theme( strip.text = element_blank())
plt = plt + scale_x_continuous(name="fixation number", breaks=c(2,3,4,5,6))# + annotate(geom="text", label="Scatter plot",colour="red")
plt = plt + scale_y_continuous(name="proportion of fix. to heterogeneous side",limits = c(0,1))
plt = plt + labs(color="condition")+scale_color_manual(labels = c("Timed", "Untimed"),values = cbPalette ) 
ggsave("plots/proportionIndividual.pdf", width=6, height=3)
ggsave("plots/propInd.jpg", width=6, height=3)
####now the same but mean for the two groups
aggData = (filter(fixdat, 
  side!="central", 
  fixNum<7, 
  fixNum>1, 
  targSide=="absent") 
  %>% group_by(fixNum, version,order,subj)
    %>% summarise(
     propHetro=mean(side=="hetro"), 
     nTrials=length(trial),
     lower = binom.confint(propHetro*nTrials,nTrials, method='wilson')$lower,
     upper = binom.confint(propHetro*nTrials,nTrials, method='wilson')$upper))

aggregateFix=aggregate(data=aggData, propHetro ~ subj + completed+version, FUN="mean")
write.csv(aggregateFix, "aggregateFixMean.txt", row.names=F)
plt = ggplot(aggData, aes(x=fixNum, y=propHetro, ymin=lower, ymax=upper, colour=version))
plt = plt + geom_point() + geom_path() + geom_errorbar()
plt = plt + theme_bw() + facet_wrap(~order, nrow=2)
plt = plt + scale_x_continuous(name="fixation number", breaks=c(2,3,4,5,6))
plt = plt + scale_y_continuous(name="proportion of fix. to heterogeneous side",limits = c(0,1))
plt = plt + labs(color="condition")+scale_color_manual(labels = c("Timed", "Untimed"),values = cbPalette )
ggsave("plots/meanFixSide.pdf", width=5, height=4)
ggsave("plots/meanFixSide.jpg", width=5, height=4)





# get mean person plot
aggData2 = (filter(fixdat, fixNum<6) 
  %>% group_by(fixNum, version, completed) 
    %>% summarise(
     propHetro=mean(side=="hetro"),
     mPropHetro=mean(propHetro), 
     stddev = sd(propHetro),
     stderr=stddev/sqrt(12),
    lower=mPropHetro-1.96*stderr,
    upper=mPropHetro+1.96*stderr))

plt = ggplot(aggData2, aes(x=fixNum,y=mPropHetro, ymin=lower, ymax=upper))
plt = plt + geom_path() + geom_errorbar()# + geom_hline(y=0.5)
plt = plt + theme_bw() +facet_grid(completed~version)
plt = plt + scale_y_continuous(name="proportion of fixations to heterogeneous side", breaks=c(0,0.5,1), limits=c(0,1))
plt = plt + scale_x_continuous('fixation number', breaks=c(2,4,6,8,10))
ggsave("plots/meanPersonSide.pdf", width=4, height=4)
ggsave("plots/meanPersonSide.jpg",dpi=600, width=6, height=4)

####now mean position on horizontal axis
fixdat$fixX = fixdat$fixX - 512
fxdat1 = filter(fixdat, 
	targSide=="absent") 
xdat1 = (fxdat1 
		%>% group_by(fixNum, subj,version, completed) 
		%>% summarise(
			meanX=mean(fixX), 
			nFix=length(fixX)))
# remove entries with fewer than 8 fixations
rtdatAcc = rtdat[which(rtdat$acc==1),]
fixAnalysis=filter(xdat1, nFix>=8)
fixAnalysis = fixAnalysis[which(fixAnalysis$fixNum>1 & fixAnalysis$fixNum<6 ),]
aggregateFix=aggregate(data=fixAnalysis, meanX ~ subj + completed, FUN="mean")
write.csv(aggregateFix, "data/aggregateFix.txt", row.names=F)

xdat1 = filter(xdat1, nFix>=8, fixNum<=6)
plt = ggplot(xdat1, aes(y=meanX, x=fixNum, colour=completed))
plt = plt + geom_point(aes(group=subj),position=position_jitter(height=0.01, width=0.1))
plt = plt + geom_smooth(se=F)+facet_grid(~version)
plt = plt + scale_y_continuous(name="mean x position of fixation", expand=c(0,0), limits=c(-300,300))
plt = plt + scale_x_continuous(name="fixation number",breaks=c(1,2,3,4,5,6,7,8), expand=c(0,0.01))
plt = plt + scale_color_manual(labels = c("First", "Second"),values = cbPalette )
ggsave("plots/meanHorizontalPos.pdf", width=5, height=3)
ggsave("plots/meanHorizontalPos.jpg", width=5, height=3)

#########calculate test re-test reliability
fixdat$fixX = fixdat$fixX - 512
fxdat1 = filter(fixdat, 
	targSide=="absent",
      fixNum<=8) 
meanX =aggregate(data=fxdat1, fixX ~ subj + version + completed, FUN="mean")
write.csv(meanX, "data/meanXreliability.txt", row.names=F)
