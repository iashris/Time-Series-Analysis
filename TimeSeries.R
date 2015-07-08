##Ashris Choudhury
#Summer Intern
#ETH Zurich

#ECP Package
library("ecp")
#Ashris Choudhury #Summer Intern #ETHZ
#Import the two data streams

WaspCity <- read.csv("./WaspCity.csv", header=FALSE, comment.char="#")
WaspGas <- read.csv("./WaspGas.csv", header=FALSE, comment.char="#")

#Data Cleaning Begins
#Convert to character frame
WaspCity <- data.frame(lapply(WaspCity, as.character), stringsAsFactors=FALSE)
WaspGas <- data.frame(lapply(WaspGas, as.character), stringsAsFactors=FALSE)

#generate extractions

extract<-function(x,colid){
  return(as.numeric(gsub("[^0-9.]","",x[,colid])))
}

Temp<-extract(WaspGas,2)
Pres<-extract(WaspGas,3)
Humi<-extract(WaspGas,4)
CO2<-extract(WaspGas,6)

Soun<-extract(WaspCity,2)
Lumi<-extract(WaspCity,3)
Dust<-extract(WaspCity,4)

#Units Temp->Celcius;Pres->kP;Humi %Relative;Sound->dB;Lumi->Lux;Dust->mg/m

#Extract Air Gas Data (only the Voltage ones)

gas_extract<-function(x,colid){
  #Split the V and OHM values
  a<-strsplit(x[,colid],"-")
  for (x in 1:length(a)){
    splitt<-strsplit(a[[x]]," ")
    #Extract only the kohm value//
    a[x]<-gsub("[^0-9.]","",splitt[2])
    
  }
  return(as.numeric(a))
}

O3<-gas_extract(WaspGas,5)
NO2<-gas_extract(WaspGas,7)
AP1<-gas_extract(WaspGas,8)
AP2<-gas_extract(WaspGas,9)

#All numeric vectors obtained
#Amalgation

City<-data.frame(cbind(Soun,Lumi,Dust))
Gas<-data.frame(cbind(Temp,Pres,Humi,CO2,NO2,O3,AP1,AP2))


normalize<-function(x){
  #mx<-max(x)
  #mn<-min(x)
  #return((x-mn)/(mx-mn))
  return(scale(x))
}

City<-data.frame(lapply(City,normalize))
Gas<-data.frame(lapply(Gas,normalize))

#Normalized Data Frames ready
#make time series. 

timemak<-function(x,fr){
  return(ts(x,start=0,frequency=fr))
}

C<-lapply(City,timemak,fr=23)
G<-lapply(Gas,timemak,fr=15)

#Customized plot
p<-function(h){

  
  if(h=="T") plot(G$T,main="Temperature variation",ylab="Temperature",xlab="Time in Minutes",lwd=2)
  if(h=="P") plot(G$P,main="Pressure variation",ylab="Pressure",xlab="Time in Minutes",lwd=2)
  if(h=="H") plot(G$H,main="Humidity variation",ylab="Humidity",xlab="Time in Minutes",lwd=2)
  if(h=="O") plot(G$O,main="Ozone variation",ylab="Ozone",xlab="Time in Minutes",lwd=2)
  if(h=="C") plot(G$C,main="Carbon Dioxide variation",ylab="CO2",xlab="Time in Minutes",lwd=2)
  if(h=="N") plot(G$N,main="Nitrogen Dioxide variation",ylab="NO2",xlab="Time in Minutes",lwd=2)
  if(h=="A1") plot(G$AP1,main="AP1 variation",ylab="AP1",xlab="Time in Minutes",lwd=2)
  if(h=="A2") plot(G$AP2,main="AP2 variation",ylab="AP2",xlab="Time in Minutes",lwd=2)
  if(h=="S") plot(C$S,main="Sound Pressure variation",ylab="Sound",xlab="Time in Minutes",lwd=2)
  if(h=="L") plot(C$L,main="Illuminance variation",ylab="Illuminance",xlab="Time in Minutes",lwd=2)
  if(h=="D") plot(C$D,main="Dust variation",ylab="Dust",xlab="Time in Minutes",lwd=2)
  a(h)
  
}

makematr<-function(x){
  return(matrix(x,nrow=length(x),ncol=1))
}

CM<-lapply(City,makematr)
GM<-lapply(Gas,makematr)
bpgenerator<-function(x,fr){
  y1=e.divisive(X=x,sig.lvl=0.05,R=199,k=NULL,min.size=30,alpha=1)
  indii<-y1$estimates[2:y1$k.hat]
  return(indii/fr)
}
CMind<-lapply(CM,bpgenerator,fr=23)
GMind<-lapply(GM,bpgenerator,fr=15)
#These contain the time stamp in minutes of the breakpoint

#Overlays
a<-function(h){
    if(h=="T") abline(v=GMind$T,col="#133D40",lwd=2)
    if(h=="P") abline(v=GMind$P,col="#133D40",lwd=2)
    if(h=="H") abline(v=GMind$H,col="#133D40",lwd=2)
    if(h=="O") abline(v=GMind$O,col="#133D40",lwd=2)
    if(h=="C") abline(v=GMind$C,col="#133D40",lwd=2)
    if(h=="N") abline(v=GMind$N,col="#133D40",lwd=2)
    if(h=="A1") abline(v=GMind$AP1,col="#133D40",lwd=2)
    if(h=="A2") abline(v=GMind$AP2,col="#133D40",lwd=2)
    if(h=="S") abline(v=CMind$S,col="#133D40",lwd=2)
    if(h=="L") abline(v=CMind$L,col="#133D40",lwd=2)
    if(h=="D") abline(v=CMind$D,col="#133D40",lwd=2)
    
}

plotall<-function(m,n){
  if(m*n<11){print("Please make sure that m x n is atleast 11")}
  old.par<-par(mfrow=c(m,n),bg="#ffffff",col="#BF362A",col.axis="#133D40",col.lab="#133D40",col.main="#133D40",col.sub="#133D40")
  p("T");p("P");p("H");p("O");p("C");p("N");p("S");p("L");p("D");p("A1");p("A2")
}
