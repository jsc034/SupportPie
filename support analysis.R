#rm(list=ls()) #clears variables
#cat("\014") #clears console || ctrl+L also works 
#getwd()
#setwd()

library(readr)

#Lucio !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
LucioA <- read_csv("~/GitHub/SupportPie/SupportPie - Lucio.csv")
Lucio <- LucioA[2:nrow(LucioA),] #removing the avg I have on the 1st row

breaksN=15

#Times
lowerBoundX <- min(min(Lucio$LengthS),min(Lucio$TenS),min(Lucio$DiffS))
upperBoundX <- max(max(Lucio$LengthS),max(Lucio$TenS),max(Lucio$DiffS))
upperBoundY <- length(Lucio$DiffS)/2
Length_Lucio <- hist(Lucio$LengthS,breaks=breaksN,plot=F)
Ten_Lucio <- hist(Lucio$TenS,breaks=breaksN,plot=F)
Diff_Lucio <- hist(Lucio$DiffS,breaks=breaksN,plot=F)
plot(Length_Lucio, col=rgb(1,0,0,1/4),xlim=c(lowerBoundX,upperBoundX),ylim=c(0,upperBoundY),main='Lucio Times',xlab='seconds') #1st plot should have title & labels
plot(Ten_Lucio, col=rgb(0,1,0,1/4), add=T)
plot(Diff_Lucio, col=rgb(0,0,1,1/4), add=T)
abline(v=mean(Lucio$LengthS),col='red',lw=1)
abline(v=mean(Lucio$TenS),col='green',lw=1)
abline(v=mean(Lucio$DiffS),col='blue',lw=1)

#Ults
Ult_Lucio_SB <- hist(Lucio$SoundBarrier,plot=F)
UltSuccess_Lucio_SB <- hist(Lucio$SoundBarrierSuccess,plot=F)
plot(Ult_Lucio_SB, col=rgb(0,0,1,1/4), xlim=c(0,max(Lucio$SoundBarrier)),main='Sound Barrier Ults',xlab='ults') # #ults > #ult success
plot(UltSuccess_Lucio_SB, col=rgb(1,0,0,1/4), add=T)
abline(v=mean(Lucio$SoundBarrier),col='blue',lw=3)
abline(v=mean(Lucio$SoundBarrierSuccess),col='red',lw=3)

UltPercent_Lucio_SB <- hist(Lucio$SoundBarrierPercent,breaks=breaksN,plot=F)
UltPM_Lucio <- hist(Lucio$UltPM,breaks=breaksN,plot=F)
plot(UltPercent_Lucio_SB, col=rgb(0,0,1,1/4), xlim=c(0,1),main='Lucio Ult Percent & Ult Per Minute',xlab='percent/ults per min')
plot(UltPM_Lucio, col=rgb(1,0,0,1/4), add=T)
abline(v=mean(Lucio$SoundBarrierPercent),col='blue',lw=3)
abline(v=mean(Lucio$UltPM),col='red',lw=3)

#KDA
upperBoundY <- length(Lucio$Kills)*.8
Kills_Lucio <- hist(Lucio$Kills,plot=F)
Assists_Lucio <- hist(Lucio$Assists,plot=F)
Deaths_Lucio <- hist(Lucio$Deaths,plot=F)
plot(Kills_Lucio, col=rgb(1,0,0,1/4), xlim=c(0,max(Lucio$Assists)),ylim=c(0,upperBoundY),main='Lucio KDA',xlab='value') # #assists > #deaths/#kills 
plot(Assists_Lucio, col=rgb(0,1,0,1/4), add=T)
plot(Deaths_Lucio, col=rgb(0,0,1,1/4), add=T)
abline(v=mean(Lucio$Kills),col='red',lw=3)
abline(v=mean(Lucio$Assists),col='green',lw=3)
abline(v=mean(Lucio$Deaths),col='blue',lw=3)

lowerBoundX <- min(min(Lucio$KillsPM),min(Lucio$AssistsPM),min(Lucio$DeathsPM))
upperBoundX <- max(max(Lucio$KillsPM),max(Lucio$AssistsPM),max(Lucio$DeathsPM))
upperBoundY <- length(Lucio$KillsPM)/2
KillsPM_Lucio <- hist(Lucio$KillsPM,breaks=breaksN,plot=F)
AssistsPM_Lucio <- hist(Lucio$AssistsPM,breaks=breaksN,plot=F)
DeathsPM_Lucio <- hist(Lucio$DeathsPM,breaks=breaksN,plot=F)
plot(KillsPM_Lucio,col=rgb(1,0,0,1/4),xlim=c(lowerBoundX,upperBoundX),ylim=c(0,upperBoundY),main='Lucio KDA Per Min',xlab='per min')
plot(AssistsPM_Lucio, col=rgb(0,1,0,1/4), add=T)
plot(DeathsPM_Lucio, col=rgb(0,0,1,1/4), add=T)
abline(v=mean(Lucio$KillsPM),col='red',lw=3)
abline(v=mean(Lucio$AssistsPM),col='green',lw=3)
abline(v=mean(Lucio$DeathsPM),col='blue',lw=3)


#Death Pie Chart
deaths <- c(Lucio$D1,Lucio$D2,Lucio$D3) #,Lucio$`Death 4`,Lucio$`Death 5`)
deaths <- deaths[!is.na(deaths)]
#typeof(*)

#Count Death Reason & Indicators
indicator <- rep(F,length(lab))
counts <- rep(0,length(lab))
for(i in deaths){
  indicator[i] <- T
  counts[i] <- counts[i] + 1
}
lab <- c('too aggro','positioning','missed a defnesive CD','facecheck','trade','teamfight','overextended','no respect for kill potential','defending core','attacking core','ganked/map awareness','died trying to save someone','bad/greedy rotation','B-ing greedy')
title = sprintf('Lucio Deaths n=%d',length(deaths))
pie(t,labels=lab[indicator],main=title)

#test
# clinical.trial <-
#   data.frame(patient = 1:100,
#              age = rnorm(100, mean = 60, sd = 6),
#              treatment = gl(2, 50,
#                             labels = c("Treatment", "Control")),
#              center = sample(paste("Center", LETTERS[1:5]), 100, replace = TRUE))



# 
# #recall which() function returns indecies where expresion is true
# t = table(deaths)
# t[names(t)==1]
# which()