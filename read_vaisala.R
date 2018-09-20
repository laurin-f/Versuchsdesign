#List: funktion um txt dateien im Format wie mttty sie schreibt einzulesen####
#speichert als liste
read_vaisala_list<-function(name=0,pfad=path,datum=format(Sys.time()-3600*24,"%d.%m")){
  #wenn kein name angegeben wird werden die dateien tiefe1-tiefe4_datum von gestern eingelesen und in eine Liste geschrieben
  if (!is.character(name)){
    #liste anlegen
    lines<-list(1,2,3,4)
    #schleife zum einlesen der dateien
    for (i in 1:4){
      lines[[i]]<-readLines(paste0(pfad,"tiefe",i,"_",datum,".txt"))
      #timestamp finden 
      begin<-(grep("Aug|Sep|Oct|Nov",lines[[i]])+2)
      #falls kein Timestamp vorhanden ist am anfang beginnen
      if (length(begin)==0){begin<-1}
      #subset von der ersten zeile nach dem timestamp bis ende 
      sub<-lines[[i]][begin[1]:length(lines[[i]])]
      #nur zeilen mit länge der ersten zeile verwenden
      sub<-sub[nchar(sub)==nchar(sub[1])]      
      #package für datumsformatierung
      library(lubridate)
      #Uhrzeit formatieren
      date<-parse_date_time(substr(sub,1,8),"HMS")
      #CO2 Werte rausschreiben
      CO2<-as.numeric(substr(sub,19,25))
      
      ts<-c(43,31,29,29)
      temp<-as.numeric(substr(sub,ts[i],ts[i]+4))
      lines[[i]]<-data.frame(date=date,CO2=CO2,temp=temp)
    }
    return(lines)
  }else{
    lines<-readLines(paste0(pfad,name,".txt"))
    library(lubridate)
    
    begin<-(grep("Aug|Sep|Oct|Nov",lines)+2)
    if (length(begin)==0){begin<-1}
    sub<-lines[begin[1]:length(lines)]
    sub<-sub[nchar(sub)==nchar(sub[1])]
    date<-parse_date_time(substr(sub,1,8),"HMS")
    CO2<-as.numeric(substr(sub,19,25))
    return(data.frame(date=date,CO2=CO2))}}



#Txt####
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
#Long: funktion um txt dateien im Format wie mttty sie schreibt einzulesen####
#speichert im long format
read_vaisala<-function(name=0,pfad=path,datum=format(Sys.time()-3600*24,"%d.%m"),CO2_line=19,temp_line=43,offsets=c(0,122.11414,-104.61813,66.67662)){
  #wenn kein name angegeben wird werden die dateien tiefe1-tiefe4_datum von gestern eingelesen und in eine Liste geschrieben
  if (!is.character(name)){
    #liste anlegen
    lines<-list(1,2,3,4)
    #package für datumsformatierung
    library(lubridate)
    date<-NULL
    date<-parse_date_time(date)
    CO2<-NULL
    temp<-NULL
    tiefe<-NULL
    #schleife zum einlesen der dateien
    for (i in 1:4){
      lines[[i]]<-readLines(paste0(pfad,"tiefe",i,"_",datum,".txt"))
      #timestamp finden 
      begin<-(grep("Aug|Sep|Oct|Nov",lines[[i]])+2)
      #falls kein Timestamp vorhanden ist am anfang beginnen
      if (length(begin)==0){begin<-1}
      #subset von der ersten zeile nach dem timestamp bis ende 
      sub<-lines[[i]][begin[1]:length(lines[[i]])]
      #nur zeilen mit länge der ersten zeile verwenden
      sub<-sub[nchar(sub)==nchar(sub[1])]      
      #Uhrzeit formatieren
      dati<-parse_date_time(substr(sub,1,8),"HMS",tz = "UTC")
      date<-c(date,dati)
      #CO2 Werte rausschreiben
      cs<-c(7,0,0,0)
      CO2<-c(CO2,as.numeric(substr(sub,19+cs[i],25+cs[i])))
      
      ts<-c(43,31,29,29)
      temp<-c(temp,as.numeric(substr(sub,ts[i],ts[i]+4)))
      tiefe<-c(tiefe,rep(i,length(sub)))
    }
    CO2_korr<-CO2
    for (i in 1:4){
      CO2_korr[tiefe==i]<-CO2[tiefe==i]-offsets[i]
    }
    out<-data.frame(date=date,CO2_raw=CO2,CO2=CO2_korr,temp=temp,tiefe=as.character(tiefe))
    return(out)
  }else{
    lines<-readLines(paste0(pfad,name,".txt"))
    library(lubridate)
    
    begin<-(grep("Aug|Sep|Oct|Nov",lines)+2)
    if (length(begin)==0){begin<-1}
    sub<-lines[begin[1]:length(lines)]
    sub<-sub[nchar(sub)==nchar(sub[1])]
    date<-parse_date_time(paste(substr(sub,1,8)),"HMS",tz = "UTC")
    CO2<-as.numeric(substr(sub,CO2_line,CO2_line+6))
    temp<-as.numeric(substr(sub,temp_line,temp_line+4))
    return(data.frame(date=date,CO2=CO2,temp=temp))}}

#test plots####
trst<-read_vaisala_list(datum="10.09")

test<-read_vaisala(datum="12.09")
atm<-read_vaisala(datum="atm")
atm_tiefe1<-read_vaisala.txt("tiefe1_atm")

test2<-read_vaisala(name="tiefe1_12.09",CO2_line = 9+7,temp_line = 33)
shift<-test2$date+(min(test$date[test$tiefe==2])-min(test2$date))
test2$date<-shift
test[test$tiefe==1,c(1,3,4)]<-test2

library(ggplot2)
ggplot(test,aes(x=date,y=CO2,col=tiefe))+
  geom_line()+
  theme_classic()

atm_tiefe1$tiefe<-"1_korr"

matplot(atm_tiefe1[,2:4],type="l")

ggplot(atm,aes(x=date,y=CO2_raw,col=tiefe))+
  geom_line()+
  theme_classic()+
  labs(colour="Sonde",y=expression("CO"[2]*"  [ppm]"),x="",title="Kalibrierungsmessung")


cal_atm<-tapply(atm$CO2,atm$tiefe,mean)
offsets<-cal_atm[1:4]-cal_atm[1]
test$CO2_cal<-1
for (i in 1:4)test$CO2_cal[test$tiefe==i]<-test$CO2[test$tiefe==i]+offsets[i]

ggplot(test,aes(x=date,y=CO2,col=tiefe))+
  geom_line()+
  theme_classic()
scale_x_datetime(limits = c(min(trst[[2]]$date),max(trst[[2]]$date)))+
  scale_y_continuous(limits = c(500,1000))

plot(trst[[2]]$date,trst[[2]]$temp,ylim=c(25,35),col=0)
for (i in 1:4) lines(trst[[i]]$date,trst[[i]]$temp,col=i)
plot(trst[[2]]$date,trst[[2]]$CO2,ylim=c(500,1000),col=0)
for (i in 1:4) lines(trst[[i]]$date,trst[[i]]$CO2,col=i)
legend("topright",paste("tiefe",1:4),col=1:4,lty = 1,bty="n")
