path<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/vorbereitung/"
plotpath<-"C:/Users/ThinkPad/Documents/Masterarbeit/abbildungen/plots/"

dicht<-read_vaisala(datum="09.10dicht")
dicht_tiefe3<-read_vaisala("dichtigkeit")

dicht$CO2[dicht$tiefe==-10]<-NA
dicht$CO2[dicht$tiefe==-10][100:(49+length(dicht_tiefe3$CO2))]<-dicht_tiefe3$CO2[1:(length(dicht_tiefe3$CO2)-50)]

library(ggplot2)
pdf(paste0(plotpath,"dichtigkeitstest.pdf"),width = 6,height = 3)
ggplot(dicht,aes(x=date,y=CO2,col=as.factor(tiefe)))+
  geom_line()+
  theme_classic()+labs(col="Tiefe [cm]",y=expression("CO"[2]*"  [ppm]"),x="")
dev.off()

#atemluft<-30000#ppm
#au?enluft<-400#ppm
