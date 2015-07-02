#created by Ashris Choudhury

#Import the two data streams
WaspCity <- read.csv("C:/Users/Information Arch/Desktop/ESUM'/WaspCity.csv", header=FALSE, comment.char="#")
WaspGas <- read.csv("C:/Users/Information Arch/Desktop/ESUM'/WaspGas.csv", header=FALSE, comment.char="#")

#Data Cleaning Begins
#Convert to character frame
WaspCity <- data.frame(lapply(WaspCity, as.character), stringsAsFactors=FALSE)
WaspGas <- data.frame(lapply(WaspGas, as.character), stringsAsFactors=FALSE)

#generate extractions

regalo<-function(x,colid){
  return(as.numeric(gsub("[^0-9.]","",x[,colid])))
}

Temp<-regalo(WaspGas,2)
Pres<-regalo(WaspGas,3)
Humi<-regalo(WaspGas,4)
CO2<-regalo(WaspGas,6)

Soun<-regalo(WaspCity,2)
Lumi<-regalo(WaspCity,3)
Dust<-regalo(WaspCity,4)

#Units Temp->Celcius;Pres->kP;Humi %Relative;Sound->dB;Lumi->Lux;Dust->mg/m

#Extract Air Gas Data (only the Voltage ones)

geschenk<-function(x,colid){
  #Split the V and OHM values
  a<-strsplit(x[,colid],"-")
  for (x in 1:length(a)){
    splitt<-strsplit(a[[x]]," ")
    #Extract only the kohm value
    a[x]<-gsub("[^0-9.]","",splitt[2])
    
  }
  return(as.numeric(a))
}

O3<-geschenk(WaspGas,5)
NO2<-geschenk(WaspGas,7)
AP1<-geschenk(WaspGas,8)
AP2<-geschenk(WaspGas,9)

#All numeric vectors obtained
#Amalgation

City<-data.frame(cbind(Soun,Lumi,Dust))
Gas<-data.frame(cbind(Temp,Pres,Humi,CO2,NO2,O3,AP1,AP2))


cadeau<-function(x){
  mx<-max(x)
  mn<-min(x)
  return((x-mn)/(mx-mn))
}

City<-data.frame(lapply(City,cadeau))
Gas<-data.frame(lapply(Gas,cadeau))

#Normalized Data Frames ready

