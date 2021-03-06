#rm(list=ls()) #clears variables
#cat("\014") #clears console || ctrl+L also works 
#getwd()
#setwd()
library(readr)

#Constants
#supports <- 'Ana'
supports <- c('Alexstrasza','Ana','Deckard','Lucio','Malfurion','Rehgar','Stukov','Tyrande','Uther')
breaksN <- 15
lab <- c('too aggro','positioning','missed a defnesive CD','facecheck','trade','teamfight','overextended','no respect for kill potential','defending core','attacking core','ganked/map awareness','died trying to save someone','bad rotation','B-ing greedy','bad camp invade','chasing/tunnel vision for a kill','bad tower dive')
Aggregate_Length_DF <- c(); Aggregate_Ten_DF <- c(); Aggregate_Diff_DF <- c()
Aggregate_Kills_DF <- c(); Aggregate_Assists_DF <- c(); Aggregate_Deaths_DF <- c()
Aggregate_KillsPM_DF <- c(); Aggregate_AssistsPM_DF <- c(); Aggregate_DeathsPM_DF <- c()
Aggregate_Siege_DF <- c(); Aggregate_Hero_DF <- c(); Aggregate_Healing_DF <- c(); Aggregate_XP_DF <- c()
Aggregate_SiegePM_DF <- c(); Aggregate_HeroPM_DF <- c(); Aggregate_HealingPM_DF <- c(); Aggregate_XPPM_DF <- c()
Aggregate_Deaths_Pie_DF <- c(); Aggregate_Deaths_By_Time_DF <- rep(0,24)
Aggregate_Games_By_Time_DF <- rep(0,24)
#colors
MAX=255; TRANSP=MAX/2 #1st (lighter) color is total number, 2nd (darker) color is success
####   color_1 <- rgb(,TRANSP,maxColorValue=MAX); color_2 <- rgb(,TRANSP,maxColorValue=MAX)
color_cleansing1 <- rgb(216,229,49,TRANSP,maxColorValue=MAX); color_cleansing2 <- rgb(254,102,41,TRANSP,maxColorValue=MAX)
color_LB1 <- rgb(111,234,86,TRANSP,maxColorValue=MAX); color_LB2 <- rgb(8,124,88,TRANSP,maxColorValue=MAX)
color_nano1 <- rgb(16,182,152,TRANSP,maxColorValue=MAX); color_nano2 <- rgb(8,52,166,TRANSP,maxColorValue=MAX)
color_horus1 <- rgb(132,234,250,TRANSP,maxColorValue=MAX); color_horus2 <- rgb(45,93,164,TRANSP,maxColorValue=MAX)
color_SB1 <- rgb(119,190,40,TRANSP,maxColorValue=MAX); color_SB2 <- rgb(48,65,29,TRANSP,maxColorValue=MAX)
color_dream1 <- rgb(90,203,247,TRANSP,maxColorValue=MAX); color_dream2 <- rgb(33,16,103,TRANSP,maxColorValue=MAX)
color_ancestral1 <- rgb(24,202,88,TRANSP,maxColorValue=MAX); color_ancestral2 <- rgb(51,59,13,TRANSP,maxColorValue=MAX)
color_BL1 <- rgb(238,202,70,TRANSP,maxColorValue=MAX); color_BL2 <- rgb(177,35,9,TRANSP,maxColorValue=MAX)
color_swipe1 <- rgb(67,231,59,TRANSP,maxColorValue=MAX); color_swipe2 <- rgb(8,125,88,TRANSP,maxColorValue=MAX)
color_Dshield1 <- rgb(253,179,36,TRANSP,maxColorValue=MAX); color_Dshield2 <- rgb(251,90,16,TRANSP,maxColorValue=MAX)
color_Dstorm1 <- rgb(224,95,17,TRANSP,maxColorValue=MAX); color_Dstorm2 <- rgb(124,45,9,TRANSP,maxColorValue=MAX)
color_stay1 <- rgb(248,171,70,TRANSP,maxColorValue=MAX); color_stay2 <- rgb(163,61,43,TRANSP,maxColorValue=MAX)
color_lore1 <- rgb(240,175,108,TRANSP,maxColorValue=MAX); color_lore2 <- rgb(190,96,67,TRANSP,maxColorValue=MAX)
color_shadow1 <- rgb(9,223,218,TRANSP,maxColorValue=MAX); color_shadow2 <- rgb(10,115,117,TRANSP,maxColorValue=MAX)
color_starfall1 <- rgb(139,69,252,TRANSP,maxColorValue=MAX); color_starfall2 <- rgb(19,34,234,TRANSP,maxColorValue=MAX)

color_DQ1 <- rgb(0,88,19,TRANSP,maxColorValue=MAX); color_DQ2 <- rgb(227,49,73,TRANSP,maxColorValue=MAX)
color_pacify1 <- rgb(232,165,93,TRANSP,maxColorValue=MAX); color_pacify2 <- rgb(129,29,72,TRANSP,maxColorValue=MAX)
color_IB1 <- rgb(129,213,233,TRANSP,maxColorValue=MAX); color_IB2 <- rgb(26,69,122,TRANSP,maxColorValue=MAX)
color_cleanse1 <- rgb(228,219,53,TRANSP,maxColorValue=MAX); color_cleanse2 <- rgb(172,137,24,TRANSP,maxColorValue=MAX)
color_VR1 <- rgb(222,205,101,TRANSP,maxColorValue=MAX); color_VR2 <- rgb(194,100,8,TRANSP,maxColorValue=MAX)
color_AB1 <- rgb(228,200,60,TRANSP,maxColorValue=MAX); color_AB2 <- rgb(130,86,38,TRANSP,maxColorValue=MAX)

for(name in supports){ #for loop over all supports
file <- sprintf("~/GitHub/SupportPie/SupportPie - %s.csv",name)
DFA <- read_csv(file,col_types=cols(Length=col_time(format="%M:%S"),Ten=col_time(format="%M:%S"),Diff=col_time(format="%M:%S")) ) #formatting length,ten,diff to min/sec
DF <- DFA[2:nrow(DFA),] #removing the avg I have on the 1st row


#General Hero Analysis !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Times
lowerBoundX <- min(min(DF$LengthS),min(DF$TenS,na.rm=T),min(DF$DiffS,na.rm=T)) #omit any NAs from games ending before 10
upperBoundX <- max(DF$LengthS)
upperBoundY <- length(DF$DiffS)*.5
Length_DF <- hist(DF$LengthS,breaks=breaksN,plot=F)
Ten_DF <- hist(DF$TenS,breaks=breaksN,plot=F)
Diff_DF <- hist(DF$DiffS,breaks=breaksN,plot=F)
title <- sprintf('%s Times n=%d',name,length(DF$DiffS))
plot(Length_DF, col=rgb(1,0,0,1/4),xlim=c(lowerBoundX,upperBoundX),ylim=c(0,upperBoundY),main=title,xlab='seconds',las=1) #1st plot should have title & labels
plot(Ten_DF, col=rgb(0,1,0,1/4), add=T)
plot(Diff_DF, col=rgb(0,0,1,1/4), add=T)
abline(v=mean(DF$LengthS),col='red',lw=1)
abline(v=mean(DF$TenS),col='green',lw=1)
abline(v=mean(DF$DiffS),col='blue',lw=1)
legend("topright",inset=.05,c('Game Length','10 Reached','Difference'), fill=c(rgb(1,0,0,1/2),rgb(0,1,0,1/2),rgb(0,0,1,1/2)))

#KDA
upperBoundX <- max(DF$Assists)
upperBoundY <- length(DF$Kills)*.8
Kills_DF <- hist(DF$Kills,plot=F)
Assists_DF <- hist(DF$Assists,plot=F)
Deaths_DF <- hist(DF$Deaths,plot=F)
title <- sprintf('%s KDA n=%d',name,length(DF$Kills))
plot(Kills_DF, col=rgb(1,0,0,1/4), xlim=c(0,upperBoundX),ylim=c(0,upperBoundY),main=title,xlab='value',las=1) # #assists > #deaths/#kills 
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
title <- sprintf('%s KDA Per Min n=%d',name,length(DF$KillsPM))
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
upperBoundY <- length(DF$Siege)*.4
Siege_DF <- hist(DF$Siege,breaks=breaksN,plot=F)
Hero_DF <- hist(DF$Hero,breaks=breaksN,plot=F)
Healing_DF <- hist(DF$Healing,breaks=breaksN,plot=F)
XP_DF <- hist(DF$XP,breaks=breaksN,plot=F)
title <- sprintf('%s Stats n=%d',name,length(DF$Siege))
plot(Siege_DF, col=rgb(1,0,0,1/4), xlim=c(lowerBoundX,upperBoundX),ylim=c(0,upperBoundY),main=title,xlab='value',las=1)
plot(Hero_DF, col=rgb(0,1,0,1/4), add=T)
plot(Healing_DF, col=rgb(0,0,1,1/4), add=T)
plot(XP_DF, col=rgb(1,1,0,1/4), add=T)
abline(v=mean(DF$Siege),col='red',lw=3)
abline(v=mean(DF$Hero),col='green',lw=3)
abline(v=mean(DF$Healing),col='blue',lw=3)
abline(v=mean(DF$XP),col='yellow',lw=3)
legend_text <- c('Siege Damage','Hero Damage','Healing/Shielding','XP Contribution')
colors <- c(rgb(1,0,0,1/2),rgb(0,1,0,1/2),rgb(0,0,1,1/2),rgb(1,1,0,1/2))
if(name=='Uther'){
  legend_text <- c(legend_text,'Damage Tanked')
  colors <- c(colors,rgb(1/2,0,1/2,1/2))
  Tanked_DF <- hist(DF$Tanked,breaks=breaksN,plot=F)
  plot(Tanked_DF,col=rgb(1/2,0,1/2,1/2),add=T)
  abline(v=mean(DF$Tanked),col='purple',lw=3)
}
legend("topright",inset=.05,legend_text, fill=colors)

lowerBoundX <- min(min(DF$SiegePM),min(DF$HeroPM),min(DF$HealingPM),min(DF$XPPM))
upperBoundX <- max(max(DF$SiegePM),max(DF$HeroPM),max(DF$HealingPM),max(DF$XPPM))
upperBoundY <- length(DF$SiegePM)*.4
SiegePM_DF <- hist(DF$SiegePM,breaks=breaksN,plot=F)
HeroPM_DF <- hist(DF$HeroPM,breaks=breaksN,plot=F)
HealingPM_DF <- hist(DF$HealingPM,breaks=breaksN,plot=F)
XPPM_DF <- hist(DF$XPPM,breaks=breaksN,plot=F)
title <- sprintf('%s Stats Per Min n=%d',name,length(DF$SiegePM))
plot(SiegePM_DF, col=rgb(1,0,0,1/4), xlim=c(lowerBoundX,upperBoundX),ylim=c(0,upperBoundY),main=title,xlab='value',las=1)
plot(HeroPM_DF, col=rgb(0,1,0,1/4), add=T)
plot(HealingPM_DF, col=rgb(0,0,1,1/4), add=T)
plot(XPPM_DF, col=rgb(1,1,0,1/4), add=T)
abline(v=mean(DF$SiegePM),col='red',lw=3)
abline(v=mean(DF$HeroPM),col='green',lw=3)
abline(v=mean(DF$HealingPM),col='blue',lw=3)
abline(v=mean(DF$XPPM),col='yellow',lw=3)
if(name=='Uther'){
  legend_text <- c(legend_text,'Tanked Per Minute')
  colors <- c(colors,rgb(1/2,0,1/2,1/2))
  TankedPM_DF <- hist(DF$Tanked,breaks=breaksN,plot=F)
  plot(TankedPM_DF,col=rgb(1/2,0,1/2,1/2),add=T)
  abline(v=mean(DF$Tanked),col='purple',lw=3)
}
legend("topright",inset=.05,legend_text, fill=colors)

#Death Pie Chart
Deaths_Pie_DF <- c(DF$D1,DF$D2,DF$D3,DF$D4,DF$D5,DF$D6,DF$D7,DF$D8)
Deaths_Pie_DF <- as.numeric(Deaths_Pie_DF[!is.na(Deaths_Pie_DF)])
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

#Games by time played
Games_By_Time_DF <- rep(0,24)
Time_DF <- DF$Time[!is.na(DF$Time)]
for(i in Time_DF){
  if(i!=0){ Games_By_Time_DF[i] = Games_By_Time_DF[i] + 1 }
  else{ Games_By_Time_DF[24] = Games_By_Time_DF[24] + 1 } #stores the 0th hour as 24th hour
}
Games_By_Time_DF = c(Games_By_Time_DF[24],Games_By_Time_DF[-24]) #rearranges so the 0th hour at front
#title = sprintf('%s Games by the Hour n=%d',name,sum(Games_By_Time_DF))
#plot(0:23,Games_By_Time_DF,main=title,xlab='Real Time Hour',ylab='Games',pch=16,type='o',las=1)

# Hero Specific !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
if(name=='Alexstrasza'){
  ult_names_indicator <- c(T,F)
  ult_names <- c('CleansingFlame','CleansingFlameSuccess','CleansingFlamePercent','Ult1PM','LifeBinder','LifeBinderSuccess','LifeBinderPercent','Ult2PM')
  actives_indicator <- c(T,T,F)
  actives <- c('Dragonqueen','DragonqueenSuccess','DragonqueenPercent','Pacify','PacifySuccess','PacifyPercent','g','h','i')
  colorU1 <- color_cleansing1; colorU2 <- color_cleansing2
  colorU3 <- color_LB1; colorU4 <- color_LB2
  colorA1 <- color_DQ1; colorA2 <- color_DQ2
  colorA3 <- color_pacify1; colorA4 <- color_pacify2
}else if(name=='Ana'){
  ult_names_indicator <- c(T,T)
  ult_names <- c('NanoBoost','NanoBoostSuccess','NanoBoostPercent','Ult1PM','Horus','HorusSuccess','HorusPercent','Ult2PM')
  actives_indicator <- c(F,F,F)
  actives <- c('a','b','c','d','e','f','g','h','i')
  colorU1 <- color_nano1; colorU2 <- color_nano2
  colorU3 <- color_horus1; colorU4 <- color_horus2
}else if(name=='Deckard'){
  ult_names_indicator <- c(T,T)
  ult_names <- c('Stay','StaySuccess','StayPercent','Ult1PM','Lore','LoreSuccess','LorePercent','Ult2PM')
  actives_indicator <- c(T,F,F)
  actives <- c('AB','ABSuccess','ABPercent','d','e','f','g','h','i')
  colorU1 <- color_stay1; colorU2 <- color_stay2
  colorU3 <- color_lore1; colorU4 <- color_lore2
  colorA1 <- color_AB1; colorA2 <- color_AB2
}else if(name=='Lucio'){
  ult_names_indicator <- c(T,F)
  ult_names <- c('SoundBarrier','SoundBarrierSuccess','SoundBarrierPercent','Ult1PM','ReverseAmp','ReverseAmpSuccess','ReverseAmpPercent','Ult2PM')
  actives_indicator <- c(F,F,F)
  actives <- c('a','b','c','d','e','f','g','h','i')
  colorU1 <- color_SB1; colorU2 <- color_SB2
}else if(name=='Malfurion'){
  ult_names_indicator <- c(T,F)
  ult_names <- c('Dream','DreamSuccess','DreamPercent','Ult1PM','Tranquility','TranquilitySuccess','TranquilityPercent','Ult2PM')
  actives_indicator <- c(T,T,T)
  actives <- c('IceBlock','IceBlockSuccess','IceBlockPercent','Cleanse','CleanseSuccess','CleansePercent','Blink','BlinkPercent','BlinkSuccess')
  colorU1 <- color_dream1; colorU2 <- color_dream2
  colorA1 <- color_IB1; colorA2 <- color_IB2
  colorA3 <- color_cleanse1; colorA4 <- color_cleanse2
  colorA5 <- color_dream1; colorA6 <- color_dream2
}else if(name=='Rehgar'){
  ult_names_indicator <- c(T,T)
  ult_names <- c('Ancestral','AncestralSuccess','AncestralPercent','Ult1PM','Bloodlust','BloodlustSuccess','BloodlustPercent','Ult2PM')
  actives_indicator <- c(T,F,F)
  actives <- c('Cleanse','CleanseSuccess','CleansePercent','d','e','f','g','h','i')
  colorU1 <- color_ancestral1; colorU2 <- color_ancestral2
  colorU3 <- color_BL1; colorU4 <- color_BL2
  colorA1 <- color_cleanse1; colorA2 <- color_cleanse2
}else if(name=='Stukov'){
  ult_names_indicator <- c(T,F)
  ult_names <- c('Swipe','SwipeSuccess','SwipePercent','Ult1PM','Shove','ShoveSuccess','ShovePercent','Ult2PM')
  actives_indicator <- c(T,F,F)
  actives <- c('Virulent','VirulentSuccess','VirulentPercent','d','e','f','g','h','i')
  colorU1 <- color_swipe1; colorU2 <- color_swipe2
  colorA1 <- color_VR1; colorA2 <- color_VR2
}else if(name=='Tyrande'){
  ult_names_indicator <- c(T,F)
  ult_names <- c('Shadow','ShadowSuccess','ShadowPercent','Ult1PM','e','f','g','Ult2PM')
  actives_indicator <- c(F,F,F)
  actives <- c('a','b','c','d','e','f','g','h','i')
  colorU1 <- color_shadow1; colorU2 <- color_shadow2
}else if(name=='Uther'){
  ult_names_indicator <- c(T,F)
  ult_names <- c('DShield','DShieldSuccess','DShieldPercent','Ult1PM','DStorm','DStormSuccess','DStormPercent','Ult2PM')
  actives_indicator <- c(T,F,F)
  actives <- c('Cleanse','CleanseSuccess','CleansePercent','d','e','f','g','h','i')
  colorU1 <- color_Dshield1; colorU2 <- color_Dshield2
  colorA1 <- color_cleanse1; colorA2 <- color_cleanse2
}

if(ult_names_indicator[1]){
  #Ult 1
  Ult_Cleaned_1 <- DF[[ult_names[1]]]
  Ult_Cleaned_1 <- Ult_Cleaned_1[ !is.na( Ult_Cleaned_1 ) ]
  UltSuccess_Cleaned_1 <- DF[[ult_names[2]]]
  UltSuccess_Cleaned_1 <- UltSuccess_Cleaned_1[ !is.na( UltSuccess_Cleaned_1 ) ]
  upperBoundY <- length(Ult_Cleaned_1)*.6
  Ult_DF_1 <- hist(Ult_Cleaned_1,plot=F)
  UltSuccess_DF_1 <- hist(UltSuccess_Cleaned_1,plot=F)
  title <- sprintf('%s %s n=%d',name,ult_names[1],length(Ult_Cleaned_1))
  plot(Ult_DF_1,col=colorU1,xlim=c(0,max(Ult_Cleaned_1)),ylim=c(0,upperBoundY),main=title,xlab='ults',las=1) # #ults > #ult success
  plot(UltSuccess_DF_1,col=colorU2,add=T)
  abline(v=mean(Ult_Cleaned_1),col=colorU1,lw=3)
  abline(v=mean(UltSuccess_Cleaned_1),col=colorU2,lw=3)
  label1 <- sprintf('%s Casted',ult_names[1])
  label2 <- sprintf('%s Successful',ult_names[1])
  legend("topright",inset=.05,c(label1,label2),fill=c(colorU1,colorU2))
  
  UltPercent_Cleaned_1 <- DF[[ult_names[3]]]
  UltPercent_Cleaned_1 <- UltPercent_Cleaned_1[ !is.na( UltPercent_Cleaned_1 ) ]
  UltPM_Cleaned_1 <- DF[[ult_names[4]]]
  UltPM_Cleaned_1 <- UltPM_Cleaned_1[ !is.na( UltPM_Cleaned_1 ) ]
  UltPercent_DF_1 <- hist(UltPercent_Cleaned_1,breaks=breaksN,plot=F)
  UltPM_DF_1 <- hist(UltPM_Cleaned_1,breaks=breaksN,plot=F)
  title <- sprintf('%s %s Percent & Per Minute n=%d',name,ult_names[1],length(UltPercent_Cleaned_1))
  plot(UltPercent_DF_1,col=colorU1,xlim=c(0,1),main=title,xlab='percent/ults per min',las=1)
  plot(UltPM_DF_1,col=colorU2,add=T)
  abline(v=mean(UltPercent_Cleaned_1),col=colorU1,lw=3)
  abline(v=mean(UltPM_Cleaned_1),col=colorU2,lw=3)
  label3 <- sprintf('%s Success Percent',ult_names[1])
  label4 <- sprintf('%s Per Minute',ult_names[1])
  legend("topleft",inset=.05,c(label3,label4), fill=c(colorU1,colorU2))
}

if(ult_names_indicator[2]){
  #Ult 2
  Ult_Cleaned_2 <- DF[[ult_names[5]]]
  Ult_Cleaned_2 <- Ult_Cleaned_2[ !is.na( Ult_Cleaned_2 ) ]
  UltSuccess_Cleaned_2 <- DF[[ult_names[6]]]
  UltSuccess_Cleaned_2 <- UltSuccess_Cleaned_2[ !is.na( UltSuccess_Cleaned_2 ) ]
  upperBoundY <- length(Ult_Cleaned_2)*.6
  Ult_DF_2 <- hist(Ult_Cleaned_2,plot=F)
  UltSuccess_DF_2 <- hist(UltSuccess_Cleaned_2,plot=F)
  title <- sprintf('%s %s n=%d',name,ult_names[5],length(Ult_Cleaned_2))
  plot(Ult_DF_2,col=colorU3,xlim=c(0,max(Ult_Cleaned_2)),ylim=c(0,upperBoundY),main=title,xlab='ults',las=1) # #ults > #ult success
  plot(UltSuccess_DF_2,col=colorU4, add=T)
  abline(v=mean(Ult_Cleaned_2),col=colorU3,lw=3)
  abline(v=mean(UltSuccess_Cleaned_2),col=colorU4,lw=3)
  label1 <- sprintf('%s Casted',ult_names[5])
  label2 <- sprintf('%s Successful',ult_names[5])
  legend("topright",inset=.05,c(label1,label2),fill=c(colorU3,colorU4))
  
  UltPercent_Cleaned_2 <- DF[[ult_names[7]]]
  UltPercent_Cleaned_2 <- UltPercent_Cleaned_2[ !is.na( UltPercent_Cleaned_2 ) ]
  UltPM_Cleaned_2 <- DF[[ult_names[8]]]
  UltPM_Cleaned_2 <- UltPM_Cleaned_2[ !is.na( UltPM_Cleaned_2 ) ]
  UltPercent_DF_2 <- hist(DF[[ult_names[7]]],breaks=breaksN,plot=F)
  UltPM_DF_2 <- hist(DF[[ult_names[8]]],breaks=breaksN,plot=F)
  title <- sprintf('%s %s Percent & Per Minute n=%d',name,ult_names[5],length(UltPercent_Cleaned_2))
  plot(UltPercent_DF_2,col=colorU3,xlim=c(0,1),main=title,xlab='percent/ults per min',las=1)
  plot(UltPM_DF_2,col=colorU4, add=T)
  abline(v=mean(UltPercent_Cleaned_2),col=colorU3,lw=3)
  abline(v=mean(UltPM_Cleaned_2),col=colorU4,lw=3)
  label3 <- sprintf('%s Success Percent',ult_names[5])
  label4 <- sprintf('%s Per Minute',ult_names[5])
  legend("topleft",inset=.05,c(label3,label4),fill=c(colorU3,colorU4))
}

if(actives_indicator[1]){
  Actives_Cleaned_1 <- DF[[actives[1]]]
  Actives_Cleaned_1 <- Actives_Cleaned_1[ !is.na( Actives_Cleaned_1 ) ]
  ActivesSuccess_Cleaned_1 <- DF[[actives[2]]]
  ActivesSuccess_Cleaned_1 <- ActivesSuccess_Cleaned_1[ !is.na( ActivesSuccess_Cleaned_1 ) ]
  upperBoundY <- length(Actives_Cleaned_1)
  Active_DF_1 <- hist(Actives_Cleaned_1,plot=F)
  ActiveSuccess_DF_1 <- hist(ActivesSuccess_Cleaned_1,plot=F)
  title <- sprintf('%s %s n=%d',name,actives[1],length(Actives_Cleaned_1))
  plot(Active_DF_1,col=colorA1,xlim=c(0,max(Actives_Cleaned_1)),ylim=c(0,upperBoundY),main=title,xlab='counts',las=1)
  plot(ActiveSuccess_DF_1,col=colorA2,add=T)
  abline(v=mean(Actives_Cleaned_1),col=colorA1,lw=3)
  abline(v=mean(ActivesSuccess_Cleaned_1),col=colorA2,lw=3)
  label1 <- sprintf('%s Casted',actives[1])
  label2 <- sprintf('%s Successful',actives[1])
  legend("topright",inset=.05,c(label1,label2),fill=c(colorA1,colorA2))

  ActivesPercent_Cleaned_1 <- DF[[actives[3]]]
  ActivesPercent_Cleaned_1 <- ActivesPercent_Cleaned_1[ !is.na( ActivesPercent_Cleaned_1 ) ]
  ActivePercent_DF_1 <- hist(ActivesPercent_Cleaned_1,breaks=breaksN,plot=F)
  
}
if(actives_indicator[2]){
  Actives_Cleaned_2 <- DF[[actives[4]]]
  Actives_Cleaned_2 <- Actives_Cleaned_2[ !is.na( Actives_Cleaned_2 ) ]
  ActivesSuccess_Cleaned_2 <- DF[[actives[5]]]
  ActivesSuccess_Cleaned_2 <- ActivesSuccess_Cleaned_2[ !is.na( ActivesSuccess_Cleaned_2 ) ]
  upperBoundY <- length(Actives_Cleaned_2)
  Active_DF_2 <- hist(Actives_Cleaned_2,plot=F)
  ActiveSuccess_DF_2 <- hist(ActivesSuccess_Cleaned_2,plot=F)
  title <- sprintf('%s %s n=%d',name,actives[4],length(Actives_Cleaned_2))
  plot(Active_DF_2,col=colorA3,xlim=c(0,max(Actives_Cleaned_2)),ylim=c(0,upperBoundY),main=title,xlab='counts',las=1)
  plot(ActiveSuccess_DF_2,col=colorA4,add=T)
  abline(v=mean(Actives_Cleaned_2),col=colorA3,lw=3)
  abline(v=mean(ActivesSuccess_Cleaned_2),col=colorA4,lw=3)
  label1 <- sprintf('%s Casted',actives[4])
  label2 <- sprintf('%s Successful',actives[4])
  legend("topright",inset=.05,c(label1,label2),fill=c(colorA3,colorA4))
  
  ActivesPercent_Cleaned_2 <- DF[[actives[6]]]
  ActivesPercent_Cleaned_2 <- ActivesPercent_Cleaned_2[ !is.na( ActivesPercent_Cleaned_2 ) ]
  ActivePercent_DF_2 <- hist(ActivesPercent_Cleaned_2,breaks=breaksN,plot=F)
  
}
if(actives_indicator[3]){ ###############################fix this
  Actives_Cleaned_3 <- DF[[actives[7]]]
  Actives_Cleaned_3 <- Actives_Cleaned_3[ !is.na( Actives_Cleaned_3 ) ]
  ActivesSuccess_Cleaned_3 <- DF[[actives[8]]]
  ActivesSuccess_Cleaned_3 <- ActivesSuccess_Cleaned_3[ !is.na( ActivesSuccess_Cleaned_3 ) ]
  upperBoundY <- length(Actives_Cleaned_3)
  Active_DF_3 <- hist(Actives_Cleaned_3,plot=F)
  ActiveSuccess_DF_3 <- hist(ActivesSuccess_Cleaned_3,plot=F)
  title <- sprintf('%s %s n=%d',name,actives[7],length(Actives_Cleaned_3))
  plot(Active_DF_3,col=colorA5,xlim=c(0,max(Actives_Cleaned_3)),ylim=c(0,upperBoundY),main=title,xlab='counts',las=1)
  plot(ActiveSuccess_DF_3,col=colorA6,add=T)
  abline(v=mean(Actives_Cleaned_3),col=colorA5,lw=3)
  abline(v=mean(ActivesSuccess_Cleaned_3),col=colorA6,lw=3)
  label1 <- sprintf('%s Casted',actives[7])
  label2 <- sprintf('%s Successful',actives[7])
  legend("topright",inset=.05,c(label1,label2),fill=c(colorA5,colorA6))
  
  ActivesPercent_Cleaned_3 <- DF[[actives[9]]]
  ActivesPercent_Cleaned_3 <- ActivesPercent_Cleaned_3[ !is.na( ActivesPercent_Cleaned_3 ) ]
  ActivePercent_DF_3 <- hist(ActivesPercent_Cleaned_3,breaks=breaksN,plot=F)
  
}
#legend("topleft",inset=.05,c(label5,label6), fill=c(rgb(0,0,1,1/2),rgb(1,0,0,1/2)))


# Aggregate Stats !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Aggregate_Length_DF <- c(Aggregate_Length_DF,DF$LengthS)
Aggregate_Ten_DF <- c(Aggregate_Ten_DF,DF$TenS)
Aggregate_Diff_DF <- c(Aggregate_Diff_DF,DF$DiffS)
Aggregate_Kills_DF <- c(Aggregate_Kills_DF,DF$Kills)
Aggregate_Assists_DF <- c(Aggregate_Assists_DF,DF$Assists)
Aggregate_Deaths_DF <- c(Aggregate_Deaths_DF,DF$Deaths)
Aggregate_KillsPM_DF <- c(Aggregate_KillsPM_DF,DF$KillsPM)
Aggregate_AssistsPM_DF <- c(Aggregate_AssistsPM_DF,DF$AssistsPM)
Aggregate_DeathsPM_DF <- c(Aggregate_DeathsPM_DF,DF$DeathsPM)
Aggregate_Siege_DF <- c(Aggregate_Siege_DF,DF$Siege)
Aggregate_Hero_DF <- c(Aggregate_Hero_DF,DF$Hero)
Aggregate_Healing_DF <- c(Aggregate_Healing_DF,DF$Healing)
Aggregate_XP_DF <- c(Aggregate_XP_DF,DF$XP)
Aggregate_SiegePM_DF <- c(Aggregate_SiegePM_DF,DF$SiegePM)
Aggregate_HeroPM_DF <- c(Aggregate_HeroPM_DF,DF$HeroPM)
Aggregate_HealingPM_DF <- c(Aggregate_HealingPM_DF,DF$HealingPM)
Aggregate_XPPM_DF <- c(Aggregate_XPPM_DF,DF$XPPM)
Aggregate_Deaths_Pie_DF <- c(Aggregate_Deaths_Pie_DF,Deaths_Pie_DF)
Aggregate_Deaths_By_Time_DF <- Aggregate_Deaths_By_Time_DF + Deaths_By_Time_DF
Aggregate_Games_By_Time_DF <- Aggregate_Games_By_Time_DF + Games_By_Time_DF

} #giant for-looping over all support characters

#Aggregate Times
lowerBoundX <- min(min(Aggregate_Length_DF),min(Aggregate_Ten_DF,na.rm=T),min(Aggregate_Diff_DF,na.rm=T)) #omit any NAs from games ending before 10
upperBoundX <- max(Aggregate_Length_DF)
upperBoundY <- length(Aggregate_Length_DF)*.3
title <- sprintf('Aggregate Support Times n=%d',length(Aggregate_Length_DF))
hist(Aggregate_Length_DF, col=rgb(1,0,0,1/4),breaks=breaksN,xlim=c(lowerBoundX,upperBoundX),ylim=c(0,upperBoundY),main=title,xlab='seconds',las=1)
hist(Aggregate_Ten_DF, col=rgb(0,1,0,1/4),breaks=breaksN, add=T)
hist(Aggregate_Diff_DF, col=rgb(0,0,1,1/4),breaks=breaksN, add=T)
abline(v=mean(Aggregate_Length_DF),col='red',lw=1)
abline(v=mean(Aggregate_Ten_DF),col='green',lw=1)
abline(v=mean(Aggregate_Diff_DF),col='blue',lw=1)
legend("topright",inset=.05,c('Game Length','10 Reached','Difference'), fill=c(rgb(1,0,0,1/2),rgb(0,1,0,1/2),rgb(0,0,1,1/2)))

#Aggregate KDA
upperBoundX <- max(Aggregate_Assists_DF)
upperBoundY <- length(Aggregate_Kills_DF)*.4
title <- sprintf('Aggregate Support KDA n=%d',length(Aggregate_Kills_DF))
hist(Aggregate_Kills_DF,col=rgb(1,0,0,1/4),breaks=breaksN,xlim=c(0,upperBoundX),ylim=c(0,upperBoundY),main=title,xlab='value',las=1) # #assists > #deaths/#kills 
hist(Aggregate_Assists_DF, col=rgb(0,1,0,1/4),breaks=breaksN, add=T)
hist(Aggregate_Deaths_DF, col=rgb(0,0,1,1/4),breaks=breaksN, add=T)
abline(v=mean(Aggregate_Kills_DF),col='red',lw=3)
abline(v=mean(Aggregate_Assists_DF),col='green',lw=3)
abline(v=mean(Aggregate_Deaths_DF),col='blue',lw=3)
legend("topright",inset=.05,c('Kills','Assists','Deaths'), fill=c(rgb(1,0,0,1/2),rgb(0,1,0,1/2),rgb(0,0,1,1/2)))

lowerBoundX <- min(min(Aggregate_KillsPM_DF),min(Aggregate_AssistsPM_DF),min(Aggregate_DeathsPM_DF))
upperBoundX <- max(max(Aggregate_KillsPM_DF),max(Aggregate_AssistsPM_DF),max(Aggregate_DeathsPM_DF))
upperBoundY <- length(Aggregate_KillsPM_DF)*.4
title <- sprintf('Aggregate Support KDA Per Min n=%d',length(Aggregate_KillsPM_DF))
hist(Aggregate_KillsPM_DF,col=rgb(1,0,0,1/4),breaks=breaksN,xlim=c(lowerBoundX,upperBoundX),ylim=c(0,upperBoundY),main=title,xlab='per min',las=1)
hist(Aggregate_AssistsPM_DF,col=rgb(0,1,0,1/4),breaks=breaksN,add=T)
hist(Aggregate_DeathsPM_DF,col=rgb(0,0,1,1/4),breaks=breaksN,add=T)
abline(v=mean(Aggregate_KillsPM_DF),col='red',lw=3)
abline(v=mean(Aggregate_AssistsPM_DF),col='green',lw=3)
abline(v=mean(Aggregate_DeathsPM_DF),col='blue',lw=3)
legend("topright",inset=.05,c('Kills Per Minute','Assists Per minute','Deaths Per Minute'), fill=c(rgb(1,0,0,1/2),rgb(0,1,0,1/2),rgb(0,0,1,1/2)))

#Aggregate Stats
lowerBoundX <- min(min(Aggregate_Siege_DF),min(Aggregate_Hero_DF),min(Aggregate_Healing_DF),min(Aggregate_XP_DF))
upperBoundX <- max(max(Aggregate_Siege_DF),max(Aggregate_Hero_DF),max(Aggregate_Healing_DF),max(Aggregate_XP_DF))
upperBoundY <- length(Aggregate_Siege_DF)*.4
title <- sprintf('Aggregate Support Stats n=%d',length(Aggregate_Siege_DF))
hist(Aggregate_Siege_DF, col=rgb(1,0,0,1/4),breaks=breaksN,xlim=c(lowerBoundX,upperBoundX),ylim=c(0,upperBoundY),main=title,xlab='value',las=1)
hist(Aggregate_Hero_DF, col=rgb(0,1,0,1/4),breaks=breaksN,add=T)
hist(Aggregate_Healing_DF, col=rgb(0,0,1,1/4),breaks=breaksN,add=T)
hist(Aggregate_XP_DF, col=rgb(1,1,0,1/4),breaks=breaksN,add=T)
abline(v=mean(Aggregate_Siege_DF),col='red',lw=3)
abline(v=mean(Aggregate_Hero_DF),col='green',lw=3)
abline(v=mean(Aggregate_Healing_DF),col='blue',lw=3)
abline(v=mean(Aggregate_XP_DF),col='yellow',lw=3)
legend_text <- c('Siege Damage','Hero Damage','Healing/Shielding','XP Contribution')
colors <- c(rgb(1,0,0,1/2),rgb(0,1,0,1/2),rgb(0,0,1,1/2),rgb(1,1,0,1/2))
legend("topright",inset=.05,legend_text, fill=colors)

lowerBoundX <- min(min(Aggregate_SiegePM_DF),min(Aggregate_HeroPM_DF),min(Aggregate_HealingPM_DF),min(Aggregate_XPPM_DF))
upperBoundX <- max(max(Aggregate_SiegePM_DF),max(Aggregate_HeroPM_DF),max(Aggregate_HealingPM_DF),max(Aggregate_XPPM_DF))
upperBoundY <- length(Aggregate_SiegePM_DF)*.4
title <- sprintf('Aggregate Support Stats Per Min n=%d',length(Aggregate_SiegePM_DF))
hist(Aggregate_SiegePM_DF, col=rgb(1,0,0,1/4),breaks=breaksN,xlim=c(lowerBoundX,upperBoundX),ylim=c(0,upperBoundY),main=title,xlab='value',las=1)
hist(Aggregate_HeroPM_DF, col=rgb(0,1,0,1/4),breaks=breaksN,add=T)
hist(Aggregate_HealingPM_DF, col=rgb(0,0,1,1/4),breaks=breaksN,add=T)
hist(Aggregate_XPPM_DF, col=rgb(1,1,0,1/4),breaks=breaksN,add=T)
abline(v=mean(Aggregate_SiegePM_DF),col='red',lw=3)
abline(v=mean(Aggregate_HeroPM_DF),col='green',lw=3)
abline(v=mean(Aggregate_HealingPM_DF),col='blue',lw=3)
abline(v=mean(Aggregate_XPPM_DF),col='yellow',lw=3)
legend("topright",inset=.05,legend_text, fill=colors)

#Aggregate Death Pie Chart
title = sprintf('Aggregate Suppport Deaths n=%d',length(Aggregate_Deaths_Pie_DF))
pie(table(Aggregate_Deaths_Pie_DF),labels=lab,main=title) ########################################################################### fix aggregate not matching up

#Aggregate Deaths by time played
title = sprintf('Aggregate Deaths by the Hour n=%d',sum(Aggregate_Deaths_By_Time_DF))
plot(0:23,Aggregate_Deaths_By_Time_DF,main=title,xlab='Real Time Hour',ylab='Deaths',pch=16,type='o',las=1)

#Aggregate Games by time played
title = sprintf('Aggregate Games by the Hour n=%d',sum(Aggregate_Games_By_Time_DF))
plot(0:23,Aggregate_Games_By_Time_DF,main=title,xlab='Real Time Hour',ylab='Games',pch=16,type='o',las=1)


#rbind(0:23,Deaths_By_Time_DF)
#typeof(*)

# 
# #recall which() function returns indecies where expresion is true
# t = table(deaths)
# t[names(t)==1]
# which()

#can use DF[ ['string'] ] to extract as a vector, not a DF slice

#sort() sorts the values and returns the sorted values : order() sorts the values and returns the indecies