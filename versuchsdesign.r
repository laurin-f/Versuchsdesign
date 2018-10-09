
r<-7.5#cm Radius
A<-pi*r^2#cm? Fl?che

auslauf<-0.5#cm? 
anteil_auslauf<-auslauf/A
h<-17#cm H?he
Vges<-h*A/1000#l Volumen
tiefenstufen<-c(2,6,10,14)
Vtiefen<-tiefenstufen*A/1000#l

regen<-c(10,20,40,60,100)#mm ; l/m? schwach mittel stark
nfk_sand<-0.1
nfk_schluff<-0.08
wasserspeicher_u<-Vtiefen*nfk_schluff*1000#ml
wasserspeicher_ges<-Vges*nfk_schluff*1000#ml


wasser<-regen*A/10000*1000#ml
wasser_promin<-wasser/24/60#ml/min

wasser_anteil<-matrix(0,length(wasserspeicher_u),length(wasser))
colnames(wasser_anteil)<-paste(regen,"mm")
rownames(wasser_anteil)<-paste(-tiefenstufen,"cm")

wasser_anteil[1,]<-(wasser-wasserspeicher_u[1])*anteil_auslauf
for (i in 2:length(wasserspeicher_u)) {wasser_anteil[i,]<-(wasser-wasserspeicher_u[i]-wasser_anteil[i-1,])*anteil_auslauf
wasser_anteil[i,]<-ifelse (wasser_anteil[i,]>0,wasser_anteil[i,],0)} #ml/day
wasser_anteil#ml

wasser_unten<-wasser-wasserspeicher_ges-colSums(wasser_anteil)#ml/day
wasser_unten[wasser_unten<0]<-0
wasser_unten#ml
plot(regen,wasser_unten,type="l")
matplot(regen,t(wasser_anteil),type="l")

x<-seq(0,7.5,.5)
a<-7.5
b<-1.5
c<-sqrt(a^2+b^2)
y<-sqrt((1-x^2/a^2)*c^2)
plot(c(x,x),c(y,-y),xlim=c(-max(y),max(y)))
paste(x,y,y*2)

path<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/vorbereitung/"
pumpstufen<-read.csv2(paste0(path,"pumpstufen.csv"),sep="\t")
pumpstufen$proschlauch<-pumpstufen$Menge/pumpstufen$n_schlauch
pumpstufen$int<-(pumpstufen$Menge/1000/(A/10000))/(pumpstufen$min*60+pumpstufen$s)*3600#mm/h
pumpstufen$intproschlauch<-pumpstufen$int/pumpstufen$n_schlauch
n2<-pumpstufen[pumpstufen$n_schlauch==2,]
plot(n2$Pumpstufe~n2$int)
fm<-glm(Pumpstufe~int,data=n2)
abline(fm)

predict(fm,data.frame(int=5))
r<-7.5#cm Radius
A<-pi*r^2#cm? Fl?che
wassermenge<-function(intensity,zeit){
  path<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/vorbereitung/"
  pumpstufen<-read.csv2(paste0(path,"pumpstufen.csv"),sep="\t")
  pumpstufen$proschlauch<-pumpstufen$Menge/pumpstufen$n_schlauch
  pumpstufen$int<-(pumpstufen$Menge/1000/(A/10000))/(pumpstufen$min*60+pumpstufen$s)*3600#mm/h
  pumpstufen$intproschlauch<-pumpstufen$int/pumpstufen$n_schlauch
  n2<-pumpstufen[pumpstufen$n_schlauch==2,]
  plot(n2$Pumpstufe~n2$int)
  fm<-glm(Pumpstufe~int,data=n2)
  pumpstufe<-predict(fm,data.frame(int=intensity))
  wasser_pro_stunde<-intensity*A/10000*1000#ml/h
  wasser_ml<-wasser_pro_stunde*zeit
  return(cbind(intensity,pumpstufe,wasser_ml))
}
wassermenge(5,1)

t<-60*2#zeit in minuten
b<-sqrt(5*t-(t/24)^2)#mm regenh?he starkregen

unterdruck<-870-980#mbar