
#Long: funktion um txt dateien im Format wie mttty sie schreibt einzulesen####
#offsets=c(-104.61813,122.11414,0,66.67662)
#speichert im long format
read_vaisala<-function(name=0,pfad=path,datum=format(Sys.time()-3600*24,"%d.%m"),CO2_line=19,temp_line=43,offsets=c(-230 ,-24,0,80)){
  #wenn kein name angegeben wird werden die dateien tiefe1-tiefe4_datum von gestern eingelesen und in eine Liste geschrieben
  if (!is.character(name)){
    #liste anlegen
    lines<-list(1,2,3,4)
    #package f?r datumsformatierung
    library(lubridate)
    date<-NULL
    date<-parse_date_time(date)
    CO2<-NULL
    temp<-NULL
    tiefe<-NULL
    tiefenstufen<-c(-2,-6,-10,-14)
    #schleife zum einlesen der dateien
    for (i in 1:4){
      lines[[i]]<-readLines(paste0(pfad,"tiefe",i,"_",datum,".txt"))
      #timestamp finden 
      
      timestamp<-lines[[i]][grep("Aug|Sep|Oct|Nov",lines[[i]])]
      timestamp<-parse_date_time(timestamp,"ab!d!HMSY",locale = "English_United States.1252")
      monthday<-unique(format(timestamp,"%m.%d"))
      begin<-(grep("Aug|Sep|Oct|Nov",lines[[i]])+2)
      ndays<-length(monthday)
      #falls kein Timestamp vorhanden ist am anfang beginnen
      if (length(begin)==0){begin<-1}
      #subset von der ersten zeile nach dem timestamp bis ende 
      sub<-lines[[i]][begin[1]:length(lines[[i]])]
      #nur zeilen mit l?nge der ersten zeile verwenden
      sub<-sub[nchar(sub)==nchar(sub[1])]  
      if(ndays>1){
        midnight<-which(sub==lines[[i]][tail(begin,1)])-1
        day<-c(rep(monthday[1],midnight),rep(monthday[2],length(sub)-midnight))}else{
          day<-monthday
        }
      #Uhrzeit formatieren
      dati<-parse_date_time(paste0(2018,day,substr(sub,1,8)),"YmdHMS",tz = "CEST")
      date<-c(date,dati)
      #CO2 Werte rausschreiben
      cs<-c(0,0,7,0)
      CO2<-c(CO2,as.numeric(substr(sub,19+cs[i],25+cs[i])))
      
      ts<-c(29,31,43,29)
      temp<-c(temp,as.numeric(substr(sub,ts[i],ts[i]+4)))
      tiefe<-c(tiefe,rep(tiefenstufen[i],length(sub)))
    }
    CO2_korr<-CO2
    for (i in 1:4){
      CO2_korr[tiefe==tiefenstufen[i]]<-CO2[tiefe==tiefenstufen[i]]-offsets[i]
    }
    out<-data.frame(date=date,CO2_raw=CO2,CO2=CO2_korr,temp=temp,tiefe=(tiefe))
    out<-out[-which(diff(out$date)==0),]
    return(out)
  }else{
    lines<-readLines(paste0(pfad,name,".txt"))
    library(lubridate)
    
    begin<-(grep("Aug|Sep|Oct|Nov",lines)+2)
    if (length(begin)==0){begin<-1}
    sub<-lines[begin[1]:length(lines)]
    sub<-sub[nchar(sub)==nchar(sub[1])]
    date<-parse_date_time(paste(substr(sub,1,8)),"HMS",tz = "CEST")
    CO2<-as.numeric(substr(sub,CO2_line,CO2_line+6))
    temp<-as.numeric(substr(sub,temp_line,temp_line+4))
    return(data.frame(date=date,CO2=CO2,temp=temp))}}


#load files####
path<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/vorbereitung/"

atmo<-read_vaisala(datum="atmo")
atmos<-read_vaisala(datum="09.10")
dicht<-read_vaisala(datum="09.10dicht")

path<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/"
profil<-read_vaisala(datum="09.10.3",offsets=rev(offsets2))
atm2<-profil[profil$date>"2018-10-10 09:10:00 CEST",]

############################
#kalibrierung


cal_atmos<-tapply(atmos$CO2_raw,atmos$tiefe,mean)
offsets<-cal_atmos[1:4]-cal_atmos[4]

cal_atm2<-tapply(atm2$CO2_raw,atm2$tiefe,mean)
offsets2<-cal_atm2[1:4]-cal_atm2[2]

offsets
rev(offsets2)
###########################
#matrix
matrix(NA,as.numeric(diff(range(profil$date)))*60*60,5)


atm<-read_vaisala(datum="atm")
atm_tiefe1<-read_vaisala.txt("tiefe1_atm")



###########################################
#test plots####
library(ggplot2)
ggplot(profil,aes(x=date,y=CO2_raw,col=as.factor(tiefe)))+
  geom_line()+
  theme_classic()
ggplot(profil,aes(x=date,y=tiefe,col=CO2))+
  geom_tile()+
  theme_classic()

meanprofil<-aggregate(profil$CO2,list(profil$tiefe) , mean)
colnames(meanprofil)<-c("tiefe","CO2")
ggplot(meanprofil,aes(x=CO2,y=tiefe))+
  geom_line()+
  theme_classic()

matplot(atm_tiefe1[,2:4],type="l")
atmos<-atmos[atmos$date<"0000-01-01 10:31:57 LMT",]

ggplot(atmo,aes(x=date,y=CO2,col=as.factor(tiefe)))+
  geom_line()+
  theme_classic()+
  labs(colour="Sonde",y=expression("CO"[2]*"  [ppm]"),x="",title="Kalibrierungsmessung")

ggplot(atm2,aes(x=date,y=CO2_raw,col=as.factor(tiefe)))+
  geom_line()+
  theme_classic()+
  labs(colour="Sonde",y=expression("CO"[2]*"  [ppm]"),x="",title="Kalibrierungsmessung")

