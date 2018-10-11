
#Long: funktion um txt dateien im Format wie mttty sie schreibt einzulesen####
#offsets=c(-104.61813,122.11414,0,66.67662)
#speichert im long format
read_vaisala<-function(name=0,#falls angegeben wird die .txt datei einzeln eingelsen
                       pfad=path,#dateipfad der dateien
                       datum=format(Sys.time()-3600*24,"%d.%m"),#falls angegeben werden die Dateien tiefe1_datum bis tiefe4_datum eingelesen und anneinandergehängt. falls name und datum nicht angegeben sind wird das datum des vortags als default angegeben 
                       CO2_line=19,#falls name angegeben wurde kann hier die position der CO2-werte in der .txt datei angepasst werden
                       temp_line=43,#falls name angegeben wurde kann hier die position der temperaturwerte in der .txt datei angepasst werden
                       offsets=c(-230 ,-24,0,80),#die abweichungen der Messsonden können hier angegeben werden
                       aggregate=T,#wenn T werden die werte auf minuten aggregiert
                       order=1:4){#die reihenfolge der Sonden in den tiefen 1 bis 4 die Sondennummern lauten wie folgt 1=respi2 2=respi3 3=PC1 4=PC2
  
  #wenn kein name angegeben wird werden die dateien tiefe1-tiefe4_datum von gestern eingelesen und in eine Liste geschrieben
  if (!is.character(name)){
    #liste anlegen
    lines<-list(1,2,3,4)
    #package fur datumsformatierung
    library(lubridate)
    date<-NULL
    date<-parse_date_time(date)
    #leere Vektoren zum befüllen
    CO2<-NULL
    temp<-NULL
    tiefe<-NULL
    #tiefenstufen der Sonden
    tiefenstufen<-c(-2,-6,-10,-14)
    #schleife zum einlesen der dateien
    for (i in 1:4){
      #.txt datei wird in liste geschrieben
      lines[[i]]<-readLines(paste0(pfad,"tiefe",i,"_",datum,".txt"))
      #timestamp finden 
      timestamp<-lines[[i]][grep("Aug|Sep|Oct|Nov",lines[[i]])]
      #timestamp formatieren
      timestamp<-parse_date_time(timestamp,"ab!d!HMSY",locale = "English_United States.1252")
      #monat und tag aus dem timestamp extrahieren
      monthday<-(format(timestamp,"%m.%d"))
      
      #position der ersten Zeile nach dem Timestamp finden
      begin<-(grep("Aug|Sep|Oct|Nov",lines[[i]])+2)

      #falls kein Timestamp vorhanden ist am anfang beginnen
      if (length(begin)==0){begin<-1}
      #subset von der ersten zeile nach dem timestamp bis ende 
      sub<-lines[[i]][begin[1]:length(lines[[i]])]
      #nur zeilen mit l?nge der ersten zeile verwenden
      sub<-sub[nchar(sub)==nchar(sub[1])]  
      day<-monthday[1]
      if(length(monthday)>1){
        day<-NULL
        for (j in 1:(length(monthday)-1)){
        start<-which(sub==lines[[i]][begin[j]])-1
        stop<-which(sub==lines[[i]][begin[j+1]])-1
        day<-c(day,rep(monthday[j],stop-start))}
        day<-c(day,rep(tail(monthday,1),length(sub)-stop))
        }
        
      #Uhrzeit formatieren
      dati<-parse_date_time(paste0(2018,day,substr(sub,1,8)),"YmdHMS",tz = "CEST")
      date<-c(date,dati)
      #CO2 Werte rausschreiben
      cs<-c(0,0,7,0)[order]
      CO2<-c(CO2,as.numeric(substr(sub,19+cs[i],25+cs[i])))
      
      ts<-c(29,31,43,29)[order]
      temp<-c(temp,as.numeric(substr(sub,ts[i],ts[i]+4)))
      tiefe<-c(tiefe,rep(tiefenstufen[i],length(sub)))
    }
    CO2_korr<-CO2
    for (i in 1:4){
      CO2_korr[tiefe==tiefenstufen[i]]<-CO2[tiefe==tiefenstufen[i]]-offsets[i]
    }
    out<-data.frame(date=date,CO2_raw=CO2,CO2=CO2_korr,temp=temp,tiefe=(tiefe))
    out<-out[-which(diff(out$date)==0),]
    if (aggregate==T){
    min<-format(out$date,"%Y%m%d%H%M")
    outmin<-aggregate(out,list(min,out$tiefe),mean)
    out<-outmin[,-(1:2)]}
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
    out<-data.frame(date=date,CO2=CO2,temp=temp)
    if (aggregate==T){
      min<-format(out$date,"%Y%m%d%H%M")
      outmin<-aggregate(out,list(min,out$tiefe),mean)
      out<-outmin[,-(1:2)]}
    return(out)}}

