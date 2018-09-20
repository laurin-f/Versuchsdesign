path<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/vorbereitung/"
dichtigkeit<-readLines(paste0(path,"dichtigkeitstest.txt"))
library(lubridate)

date<-parse_date_time(paste("29.08.2018",substr(dichtigkeit,1,8)),"dmyHMS")+60*60*14

CO2<-as.numeric(substr(dichtigkeit,9,15))
CO2_korr1<-as.numeric(substr(dichtigkeit,9+7,15+7))
CO2_korr2<-as.numeric(substr(dichtigkeit,9+7+7,15+7+7))


zu<-date<parse_date_time("2018-08-29 16:35:00 UTC","ymdHMS")
plot(date[zu],CO2[zu],type="l")
lines(date,CO2_korr1,col=2)
lines(date,CO2_korr2,col=3)
min(CO2)       
#atemluft<-30000#ppm
#außenluft<-400#ppm

dichtigkeit<-readLines(paste0(path,"dichtigkeit.txt"))
library(lubridate)
head(dichtigkeit)
begin<-(grep("Aug|Sep|Oct",dichtigkeit)+2):length(dichtigkeit)

date<-parse_date_time(paste("29.08.2018",substr(dichtigkeit[begin],1,8)),"dmyHMS")

CO2<-as.numeric(substr(dichtigkeit[begin],19,25))
CO2_korr1<-as.numeric(substr(dichtigkeit,19+7,25+7))
CO2_korr2<-as.numeric(substr(dichtigkeit,19+7+7,25+7+7))


plot(date,CO2,type="l")
lines(date,CO2_korr1,col=2)
lines(date,CO2_korr2,col=3)
min(CO2)  



test<-readLines(paste0(path,"sand_co2test.txt"))
library(lubridate)

begin<-(grep("Aug|Sep|Oct",test)+2)
which(nchar(test)==67)
nchar(test[2495])

date<-parse_date_time(paste("29.08.2018",substr(test[begin[1]:begin[2]][nchar(test)==67],1,8)),"dmyHMS")

CO2<-as.numeric(substr(test[begin[1]:begin[2]-5][nchar(test[begin[1]:begin[2]-5])==67],19,25))
CO2<-as.numeric(substr(test[begin[1]:begin[2]-5],19,25))
plot(CO2)
summary(CO2)
tiefe1<-as.numeric(substr(test[begin[2]:begin[3]-5][nchar(test[begin[2]:begin[3]-5])==67],19,25))
tiefe2<-as.numeric(substr(test[begin[3]:begin[4]-5][nchar(test[begin[3]:begin[4]-5])==67],19,25))
tiefe3<-as.numeric(substr(test[begin[4]:begin[5]-5][nchar(test[begin[4]:begin[5]-5])==67],19,25))
tiefe4<-as.numeric(substr(test[begin[5]:length(test)][nchar(test[begin[5]:length(test)])==67],19,25))
alle<-c(tiefe1,tiefe2,tiefe3,tiefe4)
plot(tiefe1,ylim = c(min(alle),max(alle)))
lines(tiefe2)
lines(tiefe3)
lines(tiefe4)
is.
length(grep("aug","1"))
read_vaisala.txt<-function(name,pfad=path){

lines<-readLines(paste0(pfad,name,".txt"))
library(lubridate)

  begin<-(grep("Aug|Sep|Oct|Nov",lines)+2)
if (length(begin)==0){begin<-1}

date<-parse_date_time(paste(substr(lines[begin[1]:length(lines)],1,8)),"HMS")
CO2<-as.numeric(substr(lines[begin[1]:length(lines)],19,25))
CO2_2<-as.numeric(substr(lines[begin[1]:length(lines)],19+7,25+7))
CO2_3<-as.numeric(substr(lines[begin[1]:length(lines)],19+7+7,25+7+7))
return(data.frame(date=date,CO2=CO2,CO2_2=CO2_2,CO2_3=CO2_3))}

dicht1<-read_vaisala.txt("dicht1")
dicht2<-read_vaisala.txt("dicht2")

plot(dicht1)
plot(dicht2)
resp1<-read.csv(paste0(path,"resp1.csv"),header = F)
resp1_1<-read.csv(paste0(path,"resp1_1.csv"),header = F)
resp2<-read.csv(paste0(path,"resp2.csv"),header = F)
resp1$date<-parse_date_time(resp1$V2,"HMS")
resp1_1$date<-parse_date_time(resp1_1$V2,"HMS")
resp2$date<-parse_date_time(resp2$V2,"HMS")

plot(resp1[,3],type="l")
plot(resp2[,3],type="l")
plot(resp1_1[,3],type="l")

resp1_07.09<-read.csv(paste0(path,"resp1_07.09.csv"),header = F)
resp2_07.09<-read.csv(paste0(path,"resp2_07.09.csv"),header = F)
resp1_07.09$date<-parse_date_time(resp1_07.09$V2,"HMS")
resp2_07.09$date<-parse_date_time(resp2_07.09$V2,"HMS")

plot(resp1_07.09[,3],type="l")
plot(resp2_07.09[,3],type="l")

check_sonde_tiefe2<-read_vaisala.txt("check_sonde_tiefe2")
dicht_tiefe1<-read_vaisala.txt("dicht_tiefe1")
dicht_tiefe2<-read_vaisala.txt("dicht_tiefe2")
dicht_tiefe3<-read_vaisala.txt("dicht_tiefe3")
dicht_tiefe4<-read_vaisala.txt("dicht_tiefe4")

check_sonde_tiefe2<-na.omit(check_sonde_tiefe2)
dicht_tiefe2<-na.omit(dicht_tiefe2)
dicht_tiefe3<-na.omit(dicht_tiefe3)
dicht_tiefe4<-na.omit(dicht_tiefe4)

par(mfrow=c(1,1))
plot(dicht_tiefe1,type="l")
plot(dicht_tiefe2,type="l")
plot(dicht_tiefe3,type="l")
plot(dicht_tiefe4,type="l")
plot(check_sonde_tiefe2,type="l")

summary(dicht_tiefe2)
