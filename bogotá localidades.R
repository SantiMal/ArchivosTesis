library(writexl)
library(lubridate)
library(readr)
library(readxl)
library(tidyverse)
library(utf8)

#DataSet from:  

Path <- "C:/Users/MALAGON/Desktop/Pandemia 2/"
osb_name <- "osb_enftransm-covid-19_09012022.csv"

bogota <- read.csv(paste(Path,osb_name,sep = ""))
View(bogota)

#Fixing errors of UTF-8 style.
bogota$CIUDAD <- iconv(bogota$CIUDAD, "UTF-8", "WINDOWS-1252")
bogota$LOCALIDAD_ASIS <- iconv(bogota$LOCALIDAD_ASIS, "UTF-8", "WINDOWS-1252")
#bogota$FECHA_DE_INICIO_DE_SINTOMAS <- as.Date(bogota$FECHA_DE_INICIO_DE_SINTOMAS,format="%Y-%m-%d")

#Show Dataset.
View(bogota)
max(as.Date(bogota$FECHA_DIAGNOSTICO,format="%Y-%m-%d"))

#Mean of diference between Fecha inicio sintomas y fecha diagnostico
A <- bogota%>%filter( FECHA_DE_INICIO_DE_SINTOMAS!="" )
difDays <- as.numeric(difftime( as.Date(A$FECHA_DIAGNOSTICO,format="%Y-%m-%d"), as.Date(A$FECHA_DE_INICIO_DE_SINTOMAS,format="%Y-%m-%d"), units = c("days")) )
meanDif <- mean(difDays)
meanDif <-ceiling(meanDif)

#Change empty fecha de inicio to fecha diagnostico - meanDif
indexEmptySintomas <- bogota$FECHA_DE_INICIO_DE_SINTOMAS==""

for (i in 1:length(indexEmptySintomas)) {
  if(indexEmptySintomas[i]){
    bogota$FECHA_DE_INICIO_DE_SINTOMAS[i]=toString(as.Date(bogota$FECHA_DIAGNOSTICO[i])-meanDif )
  }
}


#########################################################################################################3
#Save new dataSet
write.csv(bogota,paste(Path,"BogotaSinNa.csv",sep = ""), row.names = FALSE)
########################################################################################################3

bogota <- read.csv(paste(Path,"BogotaSinNa.csv",sep = ""))
View(bogota)



#Save localidades names.
localidades <- bogota%>%distinct(LOCALIDAD_ASIS)
#Group by localidades and count num of dates (num of ingured people at this day). 
organizado <- bogota%>%group_by(LOCALIDAD_ASIS)%>%count(FECHA_DE_INICIO_DE_SINTOMAS)



#Set date and number of cases en datos and #day from first day in dia
datos <- list()
dia <- list()
for (i in 1:nrow(localidades)) {
  datos[[ localidades[i,1] ]] <- organizado%>%filter(LOCALIDAD_ASIS==localidades[i,1])
  a <-datos[[localidades[i,1]]]$FECHA_DE_INICIO_DE_SINTOMAS
  dia[[ localidades[i,1] ]] <- as.numeric(difftime( as.Date(a,format="%Y-%m-%d"), as.Date(a[1],format="%Y-%m-%d") , units = c("days")) )+1
}


#organize cases in each day, and supose the virus remain (numr) in organims
infec <- list()
recup <- list()
numr <- 10
primerDia <-list()
for (i in 1:nrow(localidades)) {
  infec[[localidades[i,1]]] <- rep(0,max( dia[[ localidades[i,1] ]] ))
  recup[[localidades[i,1]]] <- rep(0,max( dia[[ localidades[i,1] ]] ) + numr)
  for (j in 1:length(dia[[localidades[i,1]]])) {
    infec[[localidades[i,1]]][ dia[[localidades[i,1]]][j] ] <- datos[[localidades[i,1]]]$n[j]
    recup[[localidades[i,1]]][ dia[[localidades[i,1]]][j] + numr ] <- datos[[localidades[i,1]]]$n[j]
    primerDia[[ localidades[i,1] ]] <- datos[[ localidades[i,1] ]]$FECHA_DE_INICIO_DE_SINTOMAS[1]
  }
 # recup[[localidades[i,1]]] <- c(recup[[localidades[i,1]]],infec[[localidades[i,1]]])[1:max( dia[[ localidades[i,1] ]] - numr )]
}


#Create cumulative quantity of infected people for each localidad (cases)
totalInfect <- list()
totalRecov <- list()
for (i in 1:nrow(localidades)) {
  totalRecov[[localidades[i,1]]] <- cumsum( recup[[localidades[i,1]]][1:length(infec[[localidades[i,1]]])] )
  totalInfect[[localidades[i,1]]] <- cumsum( infec[[localidades[i,1]]] )-totalRecov[[localidades[i,1]]]
  }



#Set dayxcases
datosfinal <- list()
for (i in 1:nrow(localidades)) {
  cases<-totalInfect[[localidades[i,1]]]
  day<-0:(length(totalInfect[[localidades[i,1]]])-1)
  recover <- totalRecov[[localidades[i,1]]]
  datosfinal[[localidades[i,1]]] <- data.frame(day,cases,recover) 
}


#Se guardan y exportan los datos a excel 
for (i in 1:length(datosfinal)) {
  write_xlsx (datosfinal[[localidades[i,1]]], sprintf(paste(Path,"/Localidades/%s.xlsx",sep = ""),localidades[i,1]))
}




############Organice for all Bogotá
organizado2 <- bogota%>%count(FECHA_DE_INICIO_DE_SINTOMAS)
#Set data for Bogotá
a <-organizado2$FECHA_DE_INICIO_DE_SINTOMAS
dias <- as.numeric(difftime( as.Date(a,format="%Y-%m-%d"), as.Date(a[1],format="%Y-%m-%d") , units = c("days")) )+1

fallecidos <- bogota%>%filter(ESTADO=='Fallecido')
fallecidos <- fallecidos%>%count(FECHA_DE_INICIO_DE_SINTOMAS)
b <- fallecidos$FECHA_DE_INICIO_DE_SINTOMAS
diasFallecidos <- as.numeric(difftime( as.Date(b,format="%Y-%m-%d"), as.Date(a[1],format="%Y-%m-%d") , units = c("days")) )+1


infectBogota <- rep(0,max(dias))
recupBogota <- rep(0,max(dias)+10)
for (j in 1:length(dias)) {
  infectBogota[dias[j]] <- organizado2$n[j]
  recupBogota[dias[j]+10] <- organizado2$n[j]
}
fallecBogota <- rep(0,max(dias))
for (j in 1:length(diasFallecidos)) {
  fallecBogota[diasFallecidos[j]] <- fallecidos$n[j]
}

CFR <- rep(0,max(dias))
for (j in 1:max(dias)) {
  cumInfec <- cumsum(infectBogota)[j]
  cumFallec <- cumsum(fallecBogota)[j]
  if (cumInfec!=0) {
    CFR[j] <- cumFallec*100/cumInfec
  }
  
}

MF <- CFR/1.38
quantile(MF)



totalinfc <- cumsum(infectBogota)-cumsum(recupBogota)[1:length(infectBogota)]
totalrecu <- cumsum(recupBogota)[1:length(infectBogota)]

cases<-totalinfc
day<-0:(length(totalinfc)-1)
recover <- totalrecu
datosfinall <- data.frame(day,cases,recover) 
write_xlsx (datosfinall, paste(Path,"datosBogotaTotal.xlsx",sep = ""))
###############################


nombres <- c()
fechas <- c()
diaVacunacion <- c()
#saving first day for each localidad
for (i in 1:nrow(localidades)) {
  nombres <- c(nombres,localidades$LOCALIDAD_ASIS[i])
  fechas <- c(fechas, primerDia[[localidades[i,1]]] )
}

for (i in 1:length(nombres)) {
  diaVacunacion[i] <- as.numeric(difftime( as.Date("2021-02-17",format="%Y-%m-%d"), as.Date(fechas[i],format="%Y-%m-%d") , units = c("days")) )
}



datosPrimerosDias <- data.frame(nombres,fechas,diaVacunacion) 
write_xlsx (datosPrimerosDias, paste(Path,"FechasPrimerDia.xlsx",sep = ""))

primerDia[[localidades[1,1]]]



