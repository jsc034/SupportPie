#rm(list=ls()) #clears variables
#cat("\014") #clears console || ctrl+L also works 
#getwd()
#setwd()

library(readr)

#Malfurion!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MalfurionA <- read_csv("~/GitHub/SupportPie/SupportPie - Malfurion.csv")
Malfurion <- MalfurionA[2:nrow(MalfurionA),] #removing the avg I have on the 1st row

breaksN=15

#Times
lowerBoundX <- min(min(Malfurion$LengthS),min(Malfurion$TenS),min(Malfurion$DiffS))
upperBoundX <- max(max(Malfurion$LengthS),max(Malfurion$TenS),max(Malfurion$DiffS))
upperBoundY <- length(Malfurion$DiffS)*.5
Length_Malfurion <- hist(Lucio$LengthS,breaks=breaksN,plot=F)
Ten_Malfurion <- hist(Lucio$TenS,breaks=breaksN,plot=F)
Diff_Malfurion <- hist(Lucio$DiffS,breaks=breaksN,plot=F)
plot(Length_Malfurion, col=rgb(1,0,0,1/4),xlim=c(lowerBoundX,upperBoundX),ylim=c(0,upperBoundY),main='Malfurion Times',xlab='seconds',las=1) #1st plot should have title & labels
plot(Ten_Malfurion, col=rgb(0,1,0,1/4), add=T)
plot(Diff_Malfurion, col=rgb(0,0,1,1/4), add=T)
abline(v=mean(Malfurion$LengthS),col='red',lw=1)
abline(v=mean(Malfurion$TenS),col='green',lw=1)
abline(v=mean(Malfurion$DiffS),col='blue',lw=1)
legend("topright",inset=.05,c('Game Length','10 Reached','Difference'), fill=c(rgb(1,0,0,1/2),rgb(0,1,0,1/2),rgb(0,0,1,1/2)))

#Ults
upperBoundY <- length(Lucio$SoundBarrier)*.4
Ult_Lucio_SB <- hist(Lucio$SoundBarrier,plot=F)
UltSuccess_Lucio_SB <- hist(Lucio$SoundBarrierSuccess,plot=F)
plot(Ult_Lucio_SB, col=rgb(0,0,1,1/4), xlim=c(0,max(Lucio$SoundBarrier)),ylim=c(0,upperBoundY),main='Lucio Sound Barriers',xlab='ults',las=1) # #ults > #ult success
plot(UltSuccess_Lucio_SB, col=rgb(1,0,0,1/4), add=T)
abline(v=mean(Lucio$SoundBarrier),col='blue',lw=3)
abline(v=mean(Lucio$SoundBarrierSuccess),col='red',lw=3)
legend("topright",inset=.05,c('Ults Casted','Ult Successful'), fill=c(rgb(0,0,1,1/2),rgb(1,0,0,1/2)))

UltPercent_Lucio_SB <- hist(Lucio$SoundBarrierPercent,breaks=breaksN,plot=F)
UltPM_Lucio <- hist(Lucio$UltPM,breaks=breaksN,plot=F)
plot(UltPercent_Lucio_SB, col=rgb(0,0,1,1/4), xlim=c(0,1),main='Lucio Ult Percent & Ult Per Minute',xlab='percent/ults per min',las=1)
plot(UltPM_Lucio, col=rgb(1,0,0,1/4), add=T)
abline(v=mean(Lucio$SoundBarrierPercent),col='blue',lw=3)
abline(v=mean(Lucio$UltPM),col='red',lw=3)
legend("topleft",inset=.05,c('Ult Success Percent','Ults Per Minute'), fill=c(rgb(0,0,1,1/2),rgb(1,0,0,1/2)))

#KDA
upperBoundY <- length(Lucio$Kills)*.8
Kills_Lucio <- hist(Lucio$Kills,plot=F)
Assists_Lucio <- hist(Lucio$Assists,plot=F)
Deaths_Lucio <- hist(Lucio$Deaths,plot=F)
plot(Kills_Lucio, col=rgb(1,0,0,1/4), xlim=c(0,max(Lucio$Assists)),ylim=c(0,upperBoundY),main='Lucio KDA',xlab='value',las=1) # #assists > #deaths/#kills 
plot(Assists_Lucio, col=rgb(0,1,0,1/4), add=T)
plot(Deaths_Lucio, col=rgb(0,0,1,1/4), add=T)
abline(v=mean(Lucio$Kills),col='red',lw=3)
abline(v=mean(Lucio$Assists),col='green',lw=3)
abline(v=mean(Lucio$Deaths),col='blue',lw=3)
legend("topright",inset=.05,c('Kills','Assists','Deaths'), fill=c(rgb(1,0,0,1/2),rgb(0,1,0,1/2),rgb(0,0,1,1/2)))

lowerBoundX <- min(min(Lucio$KillsPM),min(Lucio$AssistsPM),min(Lucio$DeathsPM))
upperBoundX <- max(max(Lucio$KillsPM),max(Lucio$AssistsPM),max(Lucio$DeathsPM))
upperBoundY <- length(Lucio$KillsPM)*.5
KillsPM_Lucio <- hist(Lucio$KillsPM,breaks=breaksN,plot=F)
AssistsPM_Lucio <- hist(Lucio$AssistsPM,breaks=breaksN,plot=F)
DeathsPM_Lucio <- hist(Lucio$DeathsPM,breaks=breaksN,plot=F)
plot(KillsPM_Lucio,col=rgb(1,0,0,1/4),xlim=c(lowerBoundX,upperBoundX),ylim=c(0,upperBoundY),main='Lucio KDA Per Min',xlab='per min',las=1)
plot(AssistsPM_Lucio, col=rgb(0,1,0,1/4), add=T)
plot(DeathsPM_Lucio, col=rgb(0,0,1,1/4), add=T)
abline(v=mean(Lucio$KillsPM),col='red',lw=3)
abline(v=mean(Lucio$AssistsPM),col='green',lw=3)
abline(v=mean(Lucio$DeathsPM),col='blue',lw=3)
legend("topright",inset=.05,c('Kills Per Minute','Assists Per minute','Deaths Per Minute'), fill=c(rgb(1,0,0,1/2),rgb(0,1,0,1/2),rgb(0,0,1,1/2)))

#Stats
lowerBoundX <- min(min(Lucio$Siege),min(Lucio$Hero),min(Lucio$Healing),min(Lucio$XP))
upperBoundX <- max(max(Lucio$Siege),max(Lucio$Hero),max(Lucio$Healing),max(Lucio$XP))
upperBoundY <- length(Lucio$Siege)*.3
Siege_Lucio <- hist(Lucio$Siege,breaks=breaksN,plot=F)
Hero_Lucio <- hist(Lucio$Hero,breaks=breaksN,plot=F)
Healing_Lucio <- hist(Lucio$Healing,breaks=breaksN,plot=F)
XP_Lucio <- hist(Lucio$XP,breaks=breaksN,plot=F)
plot(Siege_Lucio, col=rgb(1,0,0,1/4), xlim=c(lowerBoundX,upperBoundX),ylim=c(0,upperBoundY),main='Lucio Stats',xlab='value',las=1)
plot(Hero_Lucio, col=rgb(0,1,0,1/4), add=T)
plot(Healing_Lucio, col=rgb(0,0,1,1/4), add=T)
plot(XP_Lucio, col=rgb(1,1,0,1/4), add=T)
abline(v=mean(Lucio$Siege),col='red',lw=3)
abline(v=mean(Lucio$Hero),col='green',lw=3)
abline(v=mean(Lucio$Healing),col='blue',lw=3)
abline(v=mean(Lucio$XP),col='yellow',lw=3)
legend("topright",inset=.05,c('Siege Damage','Hero Damage','Healing/Shielding','XP Contribution'), fill=c(rgb(1,0,0,1/2),rgb(0,1,0,1/2),rgb(0,0,1,1/2),rgb(1,1,0,1/2)))

lowerBoundX <- min(min(Lucio$SiegePM),min(Lucio$HeroPM),min(Lucio$HealingPM),min(Lucio$XPPM))
upperBoundX <- max(max(Lucio$SiegePM),max(Lucio$HeroPM),max(Lucio$HealingPM),max(Lucio$XPPM))
upperBoundY <- length(Lucio$SiegePM)*.4
SiegePM_Lucio <- hist(Lucio$SiegePM,breaks=breaksN,plot=F)
HeroPM_Lucio <- hist(Lucio$HeroPM,breaks=breaksN,plot=F)
HealingPM_Lucio <- hist(Lucio$HealingPM,breaks=breaksN,plot=F)
XPPM_Lucio <- hist(Lucio$XPPM,breaks=breaksN,plot=F)
plot(SiegePM_Lucio, col=rgb(1,0,0,1/4), xlim=c(lowerBoundX,upperBoundX),ylim=c(0,upperBoundY),main='Lucio Stats Per Min',xlab='value',las=1)
plot(HeroPM_Lucio, col=rgb(0,1,0,1/4), add=T)
plot(HealingPM_Lucio, col=rgb(0,0,1,1/4), add=T)
plot(XPPM_Lucio, col=rgb(1,1,0,1/4), add=T)
abline(v=mean(Lucio$SiegePM),col='red',lw=3)
abline(v=mean(Lucio$HeroPM),col='green',lw=3)
abline(v=mean(Lucio$HealingPM),col='blue',lw=3)
abline(v=mean(Lucio$XPPM),col='yellow',lw=3)
legend("topright",inset=.05,c('Siege Per Minute','Hero Per Minute','Healing Per Minute','XP Per Minute'), fill=c(rgb(1,0,0,1/2),rgb(0,1,0,1/2),rgb(0,0,1,1/2),rgb(1,1,0,1/2)))

#Death Pie Chart
Deaths_Pie_Lucio <- c(Lucio$D1,Lucio$D2,Lucio$D3,Lucio$D4,Lucio$D5,Lucio$D6,Lucio$D7,Lucio$D8)
Deaths_Pie_Lucio <- as.numeric(Deaths_Pie_Lucio[!is.na(Deaths_Pie_Lucio)])
lab <- c('too aggro','positioning','missed a defnesive CD','facecheck','trade','teamfight','overextended','no respect for kill potential','defending core','attacking core','ganked/map awareness','died trying to save someone','bad/greedy rotation','B-ing greedy')
indicator <- rep(F,length(lab))
counts <- rep(0,length(lab))
for(i in Deaths_Pie_Lucio){
  indicator[i] <- T
  counts[i] <- counts[i] + 1
}
title = sprintf('Lucio Deaths n=%d',length(Deaths_Pie_Lucio))
pie(table(Deaths_Pie_Lucio),labels=lab[indicator],main=title)

#Deaths by time played
Deaths_By_Time_Lucio <- rep(0,24)
Time_Lucio <- Lucio$Time[!is.na(Lucio$Time)]
for(i in 36:nrow(Lucio)){ 
  t <- Lucio[i,]$Time
  d <- as.numeric(Lucio[i,30:37]) ######################################## dangerous hard coded numbers
  Deaths_By_Time_Lucio[t+1] = Deaths_By_Time_Lucio[t+1] + length(d[!is.na(d)])
}
title = sprintf('Lucio Deaths by the Hour n=%d',sum(Deaths_By_Time_Lucio))
plot(0:23,Deaths_By_Time_Lucio,main=title,xlab='Real Time Hour',ylab='Deaths',pch=16,type='o',las=1)




#rbind(0:23,Deaths_By_Time_Lucio)

#test
# clinical.trial <-
#   data.frame(patient = 1:100,
#              age = rnorm(100, mean = 60, sd = 6),
#              treatment = gl(2, 50,
#                             labels = c("Treatment", "Control")),
#              center = sample(paste("Center", LETTERS[1:5]), 100, replace = TRUE))

#typeof(*)

# 
# #recall which() function returns indecies where expresion is true
# t = table(deaths)
# t[names(t)==1]
# which()

