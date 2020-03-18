library(ggplot2)
library(scales)
library(dplyr)
library(gridExtra)
library(Rmisc)
library(lme4)

cbPalette <- c("#E69F00", "#56B4E9", "#B5CB8B")
setwd("C:/Users/s03an7/Documents/GitHub/DeadlineExp/DeadlineExpFromGitHub")
rtdat = readRDS(file="data/processedRTandAccData.Rda")

levels(rtdat$version) = c("Timed","Untimed")
levels(rtdat$targSide) = c("Heterogeneous","Homogeneous","Absent")
rtdat$subj = as.factor(rtdat$subj)
rtdat$subj = factor(rtdat$subj)
levels(rtdat$subj)= as.character(c(1:18))

rtdat$subj= as.factor(rtdat$subj)
rtdat$completed=as.character(as.factor(rtdat$completed))

####looking at accuracy first
#calculating difference in accuracy between timed and untimed condition

#accdat =aggregate(data=rtdat, acc ~ subj + version + completed, FUN="mean")
#accdatIncrease <- accdat[which(accdat$completed==1 & accdat$version=="Timed"),]
#accdatIncrease2 <- accdat[which(accdat$completed==2 & accdat$version=="Untimed"),]
#mydata<-rbind(accdatIncrease,accdatincrease2)

#summaryAcc <- summarySE(mydata, measurevar="acc", groupvars=c("version"))

#accdat =aggregate(data=accdat, acc ~subj + version +targSide + completed, FUN="mean")
write.csv(accdat, "data/accAggregated.txt", row.names=F)

#plots with facests for individual participants
#acc
individAccuracy<- aggregate(data=rtdat, acc ~ subj +targSide, FUN="mean")
indAcc = ggplot(individAccuracy, aes(x=targSide, y=100*acc, fill=targSide)) + geom_bar(stat="identity", position="dodge") + theme_minimal() +scale_fill_manual(name="target side", values=cbPalette)
indAcc = indAcc + scale_y_continuous(name="Accuracy (%)",breaks=c(0,20,40,60,80,100)) + facet_wrap(~subj,nrow=2) + scale_x_discrete(name="",labels= NULL)
ggsave("plots/accIndividual.pdf", width=6, height=3)
ggsave("plots/accndividual.jpg", width=6, height=3)

#rt
rtdat<-rtdat[which(rtdat$acc==1),]
#calculating difference in accuracy between timed and untimed condition

RTdat =aggregate(data=rtdat, RT ~ subj + version + completed, FUN="mean")
RTIncrease <- RTdat[which(RTdat$completed==1 & RTdat$version=="Timed"),]
RTIncrease2 <- RTdat[which(RTdat$completed==2 & RTdat$version=="Untimed"),]
mydata<-rbind(RTIncrease,RTIncrease2)

summaryRT <- summarySE(mydata, measurevar="RT", groupvars=c("version"))

individRT<- aggregate(data=rtdat, RT ~ subj +targSide, FUN="mean")
indRT = ggplot(individRT, aes(x=targSide, y=RT, fill=targSide)) + geom_bar(stat="identity", position="dodge") + theme_minimal() +scale_fill_manual(name="target side", values=cbPalette)
indRT = indRT + scale_y_continuous(name="RT (seconds)",breaks=c(seq(0,20,5))) + facet_wrap(~subj,nrow=2) + scale_x_discrete(name="",labels= NULL)
ggsave("plots/RTIndividual.pdf", width=6, height=3)
ggsave("plots/RTIndividual.jpg", width=6, height=3)



pd <- position_dodge(width = 0.5)
pAcc = ggplot(summaryAcc, aes(x=completed, y=100*acc, fill=version)) + geom_bar(stat="identity", position="dodge") + theme_minimal()
pAcc = pAcc + scale_y_continuous(name="Accuracy (%)",breaks=c(0,20,40,60,80,100)) + scale_x_discrete(name="order",labels=c("First", "Second"))+scale_fill_manual(name="timing", values=cbPalette) + facet_wrap(~targSide)
pAcc = pAcc + geom_errorbar(position=position_dodge(.9), aes(ymin=(acc-ci)*100,ymax=(acc+ci)*100),width=.5)
ggsave("plots/Accuracy.pdf", width=6, height=3)
ggsave("plots/Accuracy.jpg", width=6, height=3)

#####Now RT
RTdat =aggregate(data=rtdat, RT ~ subj + targSide + version + completed, FUN="median")
summaryRT <- summarySE(RTdat, measurevar="RT", groupvars=c("targSide","version","completed"))
RTdat =aggregate(data=rtdat, RT ~subj+ version +targSide + completed, FUN="mean")
write.csv(RTdat, "data/RtAggregated1.txt", row.names=F)

pRT = ggplot(summaryRT, aes(x=completed, y=RT, fill=version)) + geom_bar(stat="identity", position="dodge") + theme_minimal()
pRT = pRT + scale_y_continuous(name="Reaction Time (s)") + scale_x_discrete(name="order",labels=c("First", "Second"))+scale_fill_manual(name="timing", values=cbPalette) + facet_wrap(~targSide)
pRT = pRT + geom_errorbar(position=position_dodge(.9), aes(ymin=(RT-ci),ymax=(RT+ci)),width=.5)
ggsave("plots/RT.pdf", width=6, height=3)
ggsave("plots/RT.jpg", width=6, height=3)


 

