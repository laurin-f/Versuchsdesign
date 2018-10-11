path<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/vorbereitung/"

atm<-read_vaisala(datum="atm",order=c(3,2,1,4))
atmos<-read_vaisala(datum="09.10",order=c(3,2,1,4))
atmos<-atmos[atmos$date<"2018-10-09 09:50:00 CEST",]

path2<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/"
profil<-read_vaisala(pfad=path2,datum="09.10.3",offsets=rev(offsets2))
atm2<-profil[profil$date>"2018-10-10 09:10:00 CEST",]

############################
#kalibrierung


cal_atm<-tapply(atm$CO2_raw,atm$tiefe,mean)
offsets<-rev(cal_atm[1:4]-cal_atm[4])

cal_atmos<-tapply(atmos$CO2_raw,atmos$tiefe,mean)
offsets2<-rev(cal_atmos[1:4]-cal_atmos[4])

cal_atm2<-tapply(atm2$CO2_raw,atm2$tiefe,mean)
offsets3<-rev(cal_atm2[1:4]-cal_atm2[2])

vergleich_offsets<-cbind(offsets,offsets2,offsets3[c(3,2,1,4)])
matplot(vergleich_offsets)
#################################
#Sonde zuweisen

atm$Sonde<-(2-atm$tiefe)/4
atm$Sonde[atm$Sonde==1]<--3
atm$Sonde[atm$Sonde==3]<--1
atm$Sonde<-abs(atm$Sonde)

atmos$Sonde<-(2-atmos$tiefe)/4
atmos$Sonde[atmos$Sonde==1]<--3
atmos$Sonde[atmos$Sonde==3]<--1
atmos$Sonde<-abs(atmos$Sonde)

atm2$Sonde<-(2-atm2$tiefe)/4
##################################
#plots

p1<-ggplot(atm,aes(x=date,y=CO2_raw,col=as.factor(Sonde)))+
  geom_line()+
  theme_classic()+
  labs(colour="Sonde",y=expression("CO"[2]*"  [ppm]"),x="",title="Kalibrierungsmessung1")

p2<-ggplot(atmos,aes(x=date,y=CO2_raw,col=as.factor(Sonde)))+
  geom_line()+
  theme_classic()+
  labs(colour="Sonde",y=expression("CO"[2]*"  [ppm]"),x="",title="Kalibrierungsmessung2")

p3<-ggplot(atm2,aes(x=date,y=CO2_raw,col=as.factor(Sonde)))+
  geom_line()+
  theme_classic()+
  labs(colour="Sonde",y=expression("CO"[2]*"  [ppm]"),x="",title="Kalibrierungsmessung3")

library(ggplot2)
library(gridExtra)
library(grid)
mat<-rbind(c(1,1,1,1),c(2,2,3,3))
grid.arrange(p1,p2,p3)  
