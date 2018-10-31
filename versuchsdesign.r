
#######################################
#Funktion

wassermenge<-function(mm,#mm
                      zeit){#h  
  r<-7.5#cm Radius
  A<-pi*r^2#cm2 area
  path<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/vorbereitung/"
  pumpstufen<-read.csv2(paste0(path,"pumpstufen.csv"),sep="\t")
  pumpstufen$int<-(pumpstufen$Menge/1000/(A/10000))/(pumpstufen$min*60+pumpstufen$s)*3600#mm/h
  n2<-pumpstufen[pumpstufen$n_schlauch==2,]
  fm<-glm(Pumpstufe~int,data=n2)
  fm2<-glm(int~Pumpstufe,data=n2)
  
  intensity<-mm/zeit
  pumpstufe<-round(predict(fm,data.frame(int=intensity)))
  intensity_mm_h<-predict(fm2,data.frame(Pumpstufe=round(pumpstufe)))
  wasser_pro_stunde<-intensity_mm_h*A/10000*1000#ml/h
  zeit_h<-mm/intensity_mm_h
  wasser_ml<-wasser_pro_stunde*zeit_h
  zeit_h_min<-paste(floor(zeit_h),round((zeit_h-floor(zeit_h))*60),sep=":")
  return(cbind(mm,intensity_mm_h,pumpstufe,wasser_ml,zeit_h_min))
}
mm<-c(15,15,50,50)
t<-c(3,9,8,3)
wassermenge(mm,t)
wassermenge(15,4)
wassermenge(15,9)
wassermenge(50,50)
wassermenge(50,8)

wassermenge(50,3)

int50mm8h<-1267.2-380.6
mm_50mm8h<-int50mm8h/A*10000/1000

t<-60*3#zeit in minuten
b<-sqrt(5*t-(t/24)^2)#mm regenh?he starkregen
b

unterdruck<-870-980#mbar