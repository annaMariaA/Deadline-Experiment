library(ggplot2)
library(scales)
library(dplyr)
library(gridExtra)
library(Rmisc)
library(lme4)

cbPalette <- c("#E69F00", "#56B4E9", "#B5CB8B")
rtdat = read.csv(file="data/processedRTandAccData.txt")

accdat =aggregate(data=rtdat, accuracy ~ subj + targSideRel, FUN="mean")
summaryAcc <- summarySE(accdat, measurevar="accuracy", groupvars=c("targSideRel"))
accdat =aggregate(data=accdat, accuracy ~subj + targSideRel, FUN="mean")
write.csv(accdat, "data/accAggregated.txt", row.names=F)

pd <- position_dodge(width = 0.5)
pAcc = ggplot(summaryAcc, aes(x=targSideRel, y=100*accuracy)) + geom_bar(stat="identity", position="dodge") + theme_minimal()
pAcc = pAcc + scale_y_continuous(name="Accuracy (%)") + scale_x_discrete(name="target position")+scale_fill_manual(name="stimuli", values=cbPalette)#+facet_wrap(~subj)
pAcc = pAcc + geom_errorbar(position=position_dodge(.9), aes(ymin=(accuracy-ci)*100,ymax=(accuracy+ci)*100),width=.5)
ggsave("plots/Accuracy.pdf", width=6, height=3)
ggsave("plots/Accuracy.jpg", width=6, height=3)

####looking at RT now
####look at correct trials only
rtdat = rtdat[which(rtdat$acc==1),]
rtdat =aggregate(data=rtdat, RT ~subj + targSideRel, FUN="median")
summaryRT <- summarySE(rtdat, measurevar="RT", groupvars=c("targSideRel"))
#accdat =aggregate(data=accdat, RT ~subj + targSide + condition, FUN="mean")
#write.csv(accdat, "rtAggregated.txt", row.names=F)


pd <- position_dodge(width = 0.5)
pRT = ggplot(summaryRT, aes(x=targSideRel, y=RT)) + geom_bar(stat="identity", position="dodge") + theme_minimal()
pRT = pRT + scale_y_continuous(name="RT (s)", breaks=c(1,2,3,4,5,6,7,8,9,10)) + scale_x_discrete(name="target position")+scale_fill_manual(name="stimuli", values=cbPalette)#+facet_wrap(~subj)
pRT = pRT + geom_errorbar(position=position_dodge(.9), aes(ymin=(RT-ci),ymax=(RT+ci)),width=.5)
ggsave("plots/RT.pdf", width=6, height=3)
ggsave("plots/RT.jpg", width=6, height=3)

 

