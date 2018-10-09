path<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/vorbereitung/"

dicht<-read_vaisala(datum="09.10dicht")
dicht_tiefe3<-read_vaisala("dichtigkeit")
length(dicht_tiefe3$CO2)
dicht$CO2[dicht$tiefe==-10]<-NA
dicht$CO2[dicht$tiefe==-10][100:(49+length(dicht_tiefe3$CO2))]<-dicht_tiefe3$CO2[1:(length(dicht_tiefe3$CO2)-50)]
library(ggplot2)
ggplot(dicht,aes(x=date,y=CO2,col=as.factor(tiefe)))+
  geom_line()+
  theme_classic()


#atemluft<-30000#ppm
#au?enluft<-400#ppm
