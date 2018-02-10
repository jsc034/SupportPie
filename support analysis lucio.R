#rm(list=ls()) #clears variables
#cat("\014") #clears console || ctrl+L also works 
#getwd()
#setwd()

library(readr)

name <- 'Lucio'
#Lucio !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
file <- sprintf("~/GitHub/SupportPie/SupportPie - %s.csv",name)
DFA <- read_csv(file)
DF <- DFA[2:nrow(DFA),] #removing the avg I have on the 1st row

breaksN=15

#Times
lowerBoundX <- min(min(DF$LengthS),min(DF$TenS),min(DF$DiffS))
upperBoundX <- max(max(DF$LengthS),max(DF$TenS),max(DF$DiffS))
upperBoundY <- length(DF$DiffS)*.5
Length_DF <- hist(DF$LengthS,breaks=breaksN,plot=F)
Ten_DF <- hist(DF$TenS,breaks=breaksN,plot=F)
Diff_DF <- hist(DF$DiffS,breaks=breaksN,plot=F)
title <- sprintf('%s Times',name)
plot(Length_DF, col=rgb(1,0,0,1/4),xlim=c(lowerBoundX,upperBoundX),ylim=c(0,upperBoundY),main=title,xlab='seconds',las=1) #1st plot should have title & labels
plot(Ten_DF, col=rgb(0,1,0,1/4), add=T)
plot(Diff_DF, col=rgb(0,0,1,1/4), add=T)
abline(v=mean(DF$LengthS),col='red',lw=1)
abline(v=mean(DF$TenS),col='green',lw=1)
abline(v=mean(DF$DiffS),col='blue',lw=1)
legend("topright",inset=.05,c('Game Length','10 Reached','Difference'), fill=c(rgb(1,0,0,1/2),rgb(0,1,0,1/2),rgb(0,0,1,1/2)))

#Ults
upperBoundY <- length(DF$SoundBarrier)*.4
Ult_DF_1 <- hist(DF$SoundBarrier,plot=F)
UltSuccess_DF_1 <- hist(DF$SoundBarrierSuccess,plot=F)
title <- sprintf('%s Sound Barriers',name)
plot(Ult_DF_1, col=rgb(0,0,1,1/4), xlim=c(0,max(DF$SoundBarrier)),ylim=c(0,upperBoundY),main=title,xlab='ults',las=1) # #ults > #ult success
plot(UltSuccess_DF_1, col=rgb(1,0,0,1/4), add=T)
abline(v=mean(DF$SoundBarrier),col='blue',lw=3)
abline(v=mean(DF$SoundBarrierSuccess),col='red',lw=3)
legend("topright",inset=.05,c('Ults Casted','Ult Successful'), fill=c(rgb(0,0,1,1/2),rgb(1,0,0,1/2)))

UltPercent_DF_1 <- hist(DF$SoundBarrierPercent,breaks=breaksN,plot=F)
UltPM_DF <- hist(DF$UltPM,breaks=breaksN,plot=F)
title <- sprintf('%s Ult Percent & Ult Per Minute',name)
plot(UltPercent_DF_1, col=rgb(0,0,1,1/4), xlim=c(0,1),main=title,xlab='percent/ults per min',las=1)
plot(UltPM_DF, col=rgb(1,0,0,1/4), add=T)
abline(v=mean(DF$SoundBarrierPercent),col='blue',lw=3)
abline(v=mean(DF$UltPM),col='red',lw=3)
legend("topleft",inset=.05,c('Ult Success Percent','Ults Per Minute'), fill=c(rgb(0,0,1,1/2),rgb(1,0,0,1/2)))

#KDA
upperBoundY <- length(DF$Kills)*.8
Kills_DF <- hist(DF$Kills,plot=F)
Assists_DF <- hist(DF$Assists,plot=F)
Deaths_DF <- hist(DF$Deaths,plot=F)
title <- sprintf('%s KDA',name)
plot(Kills_DF, col=rgb(1,0,0,1/4), xlim=c(0,max(DF$Assists)),ylim=c(0,upperBoundY),main=title,xlab='value',las=1) # #assists > #deaths/#kills 
plot(Assists_DF, col=rgb(0,1,0,1/4), add=T)
plot(Deaths_DF, col=rgb(0,0,1,1/4), add=T)
abline(v=mean(DF$Kills),col='red',lw=3)
abline(v=mean(DF$Assists),col='green',lw=3)
abline(v=mean(DF$Deaths),col='blue',lw=3)
legend("topright",inset=.05,c('Kills','Assists','Deaths'), fill=c(rgb(1,0,0,1/2),rgb(0,1,0,1/2),rgb(0,0,1,1/2)))

lowerBoundX <- min(min(DF$KillsPM),min(DF$AssistsPM),min(DF$DeathsPM))
upperBoundX <- max(max(DF$KillsPM),max(DF$AssistsPM),max(DF$DeathsPM))
upperBoundY <- length(DF$KillsPM)*.5
KillsPM_DF <- hist(DF$KillsPM,breaks=breaksN,plot=F)
AssistsPM_DF <- hist(DF$AssistsPM,breaks=breaksN,plot=F)
DeathsPM_DF <- hist(DF$DeathsPM,breaks=breaksN,plot=F)
title <- sprintf('%s KDA Per Min',name)
plot(KillsPM_DF,col=rgb(1,0,0,1/4),xlim=c(lowerBoundX,upperBoundX),ylim=c(0,upperBoundY),main=title,xlab='per min',las=1)
plot(AssistsPM_DF, col=rgb(0,1,0,1/4), add=T)
plot(DeathsPM_DF, col=rgb(0,0,1,1/4), add=T)
abline(v=mean(DF$KillsPM),col='red',lw=3)
abline(v=mean(DF$AssistsPM),col='green',lw=3)
abline(v=mean(DF$DeathsPM),col='blue',lw=3)
legend("topright",inset=.05,c('Kills Per Minute','Assists Per minute','Deaths Per Minute'), fill=c(rgb(1,0,0,1/2),rgb(0,1,0,1/2),rgb(0,0,1,1/2)))

#Stats
lowerBoundX <- min(min(DF$Siege),min(DF$Hero),min(DF$Healing),min(DF$XP))
upperBoundX <- max(max(DF$Siege),max(DF$Hero),max(DF$Healing),max(DF$XP))
upperBoundY <- length(DF$Siege)*.3
Siege_DF <- hist(DF$Siege,breaks=breaksN,plot=F)
Hero_DF <- hist(DF$Hero,breaks=breaksN,plot=F)
Healing_DF <- hist(DF$Healing,breaks=breaksN,plot=F)
XP_DF <- hist(DF$XP,breaks=breaksN,plot=F)
title <- sprintf('%s Stats',name)
plot(Siege_DF, col=rgb(1,0,0,1/4), xlim=c(lowerBoundX,upperBoundX),ylim=c(0,upperBoundY),main=title,xlab='value',las=1)
plot(Hero_DF, col=rgb(0,1,0,1/4), add=T)
plot(Healing_DF, col=rgb(0,0,1,1/4), add=T)
plot(XP_DF, col=rgb(1,1,0,1/4), add=T)
abline(v=mean(DF$Siege),col='red',lw=3)
abline(v=mean(DF$Hero),col='green',lw=3)
abline(v=mean(DF$Healing),col='blue',lw=3)
abline(v=mean(DF$XP),col='yellow',lw=3)
legend("topright",inset=.05,c('Siege Damage','Hero Damage','Healing/Shielding','XP Contribution'), fill=c(rgb(1,0,0,1/2),rgb(0,1,0,1/2),rgb(0,0,1,1/2),rgb(1,1,0,1/2)))

lowerBoundX <- min(min(DF$SiegePM),min(DF$HeroPM),min(DF$HealingPM),min(DF$XPPM))
upperBoundX <- max(max(DF$SiegePM),max(DF$HeroPM),max(DF$HealingPM),max(DF$XPPM))
upperBoundY <- length(DF$SiegePM)*.4
SiegePM_DF <- hist(DF$SiegePM,breaks=breaksN,plot=F)
HeroPM_DF <- hist(DF$HeroPM,breaks=breaksN,plot=F)
HealingPM_DF <- hist(DF$HealingPM,breaks=breaksN,plot=F)
XPPM_DF <- hist(DF$XPPM,breaks=breaksN,plot=F)
title <- sprintf('%s Stats Per Min',name)
plot(SiegePM_DF, col=rgb(1,0,0,1/4), xlim=c(lowerBoundX,upperBoundX),ylim=c(0,upperBoundY),main=title,xlab='value',las=1)
plot(HeroPM_DF, col=rgb(0,1,0,1/4), add=T)
plot(HealingPM_DF, col=rgb(0,0,1,1/4), add=T)
plot(XPPM_DF, col=rgb(1,1,0,1/4), add=T)
abline(v=mean(DF$SiegePM),col='red',lw=3)
abline(v=mean(DF$HeroPM),col='green',lw=3)
abline(v=mean(DF$HealingPM),col='blue',lw=3)
abline(v=mean(DF$XPPM),col='yellow',lw=3)
legend("topright",inset=.05,c('Siege Per Minute','Hero Per Minute','Healing Per Minute','XP Per Minute'), fill=c(rgb(1,0,0,1/2),rgb(0,1,0,1/2),rgb(0,0,1,1/2),rgb(1,1,0,1/2)))

#Death Pie Chart
Deaths_Pie_DF <- c(DF$D1,DF$D2,DF$D3,DF$D4,DF$D5,DF$D6,DF$D7,DF$D8)
Deaths_Pie_DF <- as.numeric(Deaths_Pie_DF[!is.na(Deaths_Pie_DF)])
lab <- c('too aggro','positioning','missed a defnesive CD','facecheck','trade','teamfight','overextended','no respect for kill potential','defending core','attacking core','ganked/map awareness','died trying to save someone','bad/greedy rotation','B-ing greedy')
indicator <- rep(F,length(lab))
counts <- rep(0,length(lab))
for(i in Deaths_Pie_DF){
  indicator[i] <- T
  counts[i] <- counts[i] + 1
}
title = sprintf('%s Deaths n=%d',name,length(Deaths_Pie_DF))
pie(table(Deaths_Pie_DF),labels=lab[indicator],main=title)

#Deaths by time played
Deaths_By_Time_DF <- rep(0,24)
Time_DF <- DF$Time[!is.na(DF$Time)]
death_start <- min(which(!is.na(DF$Time))) #calculates the row of the 1st recorded real time hour
death_columns <- c('D1','D2','D3','D4','D5','D6','D7','D8')
for(i in death_start:nrow(DF)){ 
  t <- DF[i,]$Time
  d <- as.numeric(DF[i,death_columns])
  Deaths_By_Time_DF[t+1] = Deaths_By_Time_DF[t+1] + length(d[!is.na(d)])
}
title = sprintf('%s Deaths by the Hour n=%d',name,sum(Deaths_By_Time_DF))
plot(0:23,Deaths_By_Time_DF,main=title,xlab='Real Time Hour',ylab='Deaths',pch=16,type='o',las=1)




#rbind(0:23,Deaths_By_Time_DF)


#typeof(*)

# 
# #recall which() function returns indecies where expresion is true
# t = table(deaths)
# t[names(t)==1]
# which()