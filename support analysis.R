#rm(list=ls()) #clears variables
#cat("\014") #clears console || ctrl+L also works 
#getwd()
#setwd()

library(readr)

#Lucio !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
LucioA <- read_csv("C:/Users/Joshua/Downloads/SupportPie - Lucio.csv")
Lucio <- LucioA[2:nrow(LucioA),] #removing the avg I have on the 1st row

#histograms of Stats
breaksN=15

upperBoundX <- max(max(Lucio$`Game LengthS`),max(Lucio$`10 ReachedS`),max(Lucio$DiffS))
upperBoundY <- length(Lucio$DiffS)/2
GameLength_Lucio <- hist(Lucio$`Game LengthS`,breaks=breaksN,plot=F)
Reach10_Lucio <- hist(Lucio$`10 ReachedS`,breaks=breaksN,plot=F)
Diff_Lucio <- hist(Lucio$DiffS,breaks=breaksN,plot=F)
plot(GameLength_Lucio, col=rgb(1,0,0,1/4),xlim=c(0,upperBoundX),ylim=c(0,upperBoundY),main='Lucio Times',xlab='seconds')
plot(Reach10_Lucio, col=rgb(0,1,0,1/4), add=T)
plot(Diff_Lucio, col=rgb(0,0,1,1/4), add=T)
abline(v=mean(Lucio$`Game LengthS`),col='red',lw=1)
abline(v=mean(Lucio$`10 ReachedS`),col='green',lw=1)
abline(v=mean(Lucio$DiffS),col='blue',lw=1)


Ult_Lucio_SB <- hist(Lucio$`Sound Barrier`,plot=F)
UltSuccess_Lucio_SB <- hist(Lucio$`Sound Barrier Success`,plot=F)
plot(Ult_Lucio_SB, col=rgb(0,0,1,1/4), xlim=c(0,max(Lucio$`Sound Barrier`)),main='Sound Barrier Ults',xlab='ults')  #1st plot should have title & labels
plot(UltSuccess_Lucio_SB, col=rgb(1,0,0,1/4), add=T)
abline(v=mean(Lucio$`Sound Barrier`),col='blue',lw=3)
abline(v=mean(Lucio$`Sound Barrier Success`),col='red',lw=3)

UltPercent_Lucio_SB <- hist(Lucio$Percent,main='Lucio Avg Ult Success Percent',xlab='percent',breaks=breaksN) #############change percent to SB percent
abline(v=mean(Lucio$Percent),col='red',lw=3)

Assists_Lucio <- hist(Lucio$Assists,main='Lucio Assists',xlab='assists')
abline(v=mean(Lucio$Assists),col='red',lw=3)

Deaths_Lucio <- hist(Lucio$Deaths,main='Lucio Deaths',xlab='deaths')
abline(v=mean(Lucio$Deaths),col='red',lw=3)

#per-minute stats
upperBoundX <- max(max(Lucio$`Avg Ult Per Minute`),max(Lucio$`Kills Per Minute`),max(Lucio$`Assists Per Minute`),max(Lucio$`Deaths Per Minute`))
upperBoundY <- length(Lucio$`Kills Per Minute`)/2
UltPerMin_Lucio <- hist(Lucio$`Avg Ult Per Minute`,breaks=breaksN,plot=F) #########################change Avg Ult per min to ult per min
KillsPerMin_Lucio <- hist(Lucio$`Kills Per Minute`,breaks=breaksN,plot=F)
AssistsPerMin_Lucio <- hist(Lucio$`Assists Per Minute`,breaks=breaksN,plot=F)
DeathsPerMin_Lucio <- hist(Lucio$`Deaths Per Minute`,breaks=breaksN,plot=F)
plot(UltPerMin_Lucio, col=rgb(1,1,0,1/4), xlim=c(0,upperBound), ylim= c(0,upperBoundY), main='Lucio Stats Per Min',xlab='per min')  #1st plot should have title & labels
plot(KillsPerMin_Lucio, col=rgb(1,0,0,1/4), add=T)
plot(AssistsPerMin_Lucio, col=rgb(0,1,0,1/4), add=T)
plot(DeathsPerMin_Lucio, col=rgb(0,0,1,1/4), add=T)
abline(v=mean(Lucio$`Avg Ult Per Minute`),col='yellow',lw=1)
abline(v=mean(Lucio$`Kills Per Minute`),col='red',lw=1)
abline(v=mean(Lucio$`Assists Per Minute`),col='green',lw=1)
abline(v=mean(Lucio$`Deaths Per Minute`),col='blue',lw=1)


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