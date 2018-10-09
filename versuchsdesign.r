
r<-7.5#cm Radius
A<-pi*r^2#cm? Fl?che

auslauf<-0.5#cm? 
anteil_auslauf<-auslauf/A
h<-17#cm H?he
Vges<-h*A/1000#l Volumen
tiefenstufen<-c(2,6,10,14)

regen<-c(10,20,40,50,60,100)#mm ; l/m? schwach mittel stark

wasser<-regen*A/10000*1000#ml
zeit<-3
wasser_pro_h<-wasser/zeit#ml/min
wasser
wasser_pro_h


#######################################
#Funktion

wassermenge<-function(intensity,#mm/h
                      zeit){#h  
  r<-7.5#cm Radius
  A<-pi*r^2#cm2 area
  path<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/vorbereitung/"
  pumpstufen<-read.csv2(paste0(path,"pumpstufen.csv"),sep="\t")
  pumpstufen$int<-(pumpstufen$Menge/1000/(A/10000))/(pumpstufen$min*60+pumpstufen$s)*3600#mm/h
  n2<-pumpstufen[pumpstufen$n_schlauch==2,]
  fm<-glm(Pumpstufe~int,data=n2)
  pumpstufe<-predict(fm,data.frame(int=intensity))
  wasser_pro_stunde<-intensity*A/10000*1000#ml/h
  wasser_ml<-wasser_pro_stunde*zeit
  return(cbind(intensity,pumpstufe,wasser_ml))
}
wassermenge(50/3,3)

t<-60*2#zeit in minuten
b<-sqrt(5*t-(t/24)^2)#mm regenh?he starkregen

unterdruck<-870-980#mbar