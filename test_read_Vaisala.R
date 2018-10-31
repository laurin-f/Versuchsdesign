source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/Versuchsdesign/read_vaisala.R")
#load files####

path<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/"
profil<-read_vaisala(datum="09.10.3",offsets=offsets3)


###########################
#matrix
mins<-NULL
maxs<-NULL
library(lubridate)
mins<-parse_date_time(mins)
maxs<-parse_date_time(maxs)
tiefenstufen<--seq(2,14,by=4)
for (i in 1:4){
  mins[i]<-min(profil$date[profil$tiefe==tiefenstufen[i]])
  maxs[i]<-max(profil$date[profil$tiefe==tiefenstufen[i]])
}
min(maxs)-max(mins)
co2sub<-subset(profil,profil$date>max(mins)&profil$date<min(maxs))
length(co2sub$CO2[co2sub$tiefe==-14])
co2mat<-matrix(NA,as.numeric(diff(range(co2sub$date)))*60*60,17)
for (i in seq(2,14,by=4)){
  co2mat[,i]<-co2sub$CO2_raw[co2sub$tiefe==-i]}
image(co2mat)



length(profil$date[profil$tiefe==-2])
66031
###########################################
#test plots####
library(ggplot2)
ggplot(profil,aes(x=date,y=CO2_raw,col=as.factor(tiefe)))+
  geom_line()+
  theme_classic()
ggplot(profil,aes(x=date,y=tiefe,col=CO2))+
  geom_tile()+
  theme_classic()

ggplot(meanprofil,aes(x=CO2,y=tiefe))+
  geom_point()+
  theme_classic()

matplot(atm_tiefe1[,2:4],type="l")


#######################################################
#co2 profil
meanprofil<-aggregate(profil$CO2_raw,list(profil$tiefe) , mean)
colnames(meanprofil)<-c("tiefe","CO2")
fmexp<-glm(CO2~exp(-tiefe),data=meanprofil)
fmlog<-glm(CO2~log(-tiefe),data=meanprofil)

tiefen<-seq(-1,-17,by=-1)
preds<-predict(fmexp,newdata = data.frame(tiefe=tiefen))
plot(meanprofil$tiefe~meanprofil$CO2,ylim=c(-17,0))
lines(tiefen~preds)
