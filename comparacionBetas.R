library(writexl)
library(lubridate)
library(readr)
library(readxl)
library(tidyverse)
library(ggplot2)
require(gridExtra)

##################################################################################################
Path <- "C:/Users/MALAGON/Desktop/Pandemia 2/"


populationdat <- read_excel( paste(Path,"PoblacionLocalidades.xlsx" ,sep = "") )
localidades <- populationdat$Localidad
DATABETALOC <- list()

FechasPrimerDia<-read_excel( paste(Path,"FechasPrimerDia.xlsx" ,sep = "") ) 
FechasPrimerDia<-FechasPrimerDia%>%filter(nombres!="Fuera de Bogotá")%>%filter(nombres!="Sin dato")
localidades <- FechasPrimerDia$nombres


for (i in 1:20 ) {
  DATABETALOC[[localidades[i]]] <- read_excel(sprintf( paste(Path,"UltimosBetas/beta%sVentana7.xlsx" ,sep = ""),localidades[i]))
}
DATABETALOCBogota <- read_excel(paste(Path,"UltimosBetas/betaBogotáVentana7.xlsx" ,sep = ""))


#MinSalud data
rMinisterio <- read.csv2(paste(Path,"Numero-de-Reproduccion-Efectivo-R(t) todos los casos.csv" ,sep = ""), header=FALSE)

view(rMinisterio)
#betasPrueba <- read_excel("C:/Users/MALAGON/Desktop/Pandemia/betas2KennedyVentana73Rs.xlsx")

#attach(mtcars)
#par(mfrow=c(1,1))
#detach()

fechasPrimerDia <- read_excel(paste(Path,"fechasPrimerDia.xlsx" ,sep = "") )
fechasCuarentena <- read_excel(paste(Path,"FechasCuarentenas.xlsx" ,sep = "") )



##########################################################################################

##########################################################################################





graficas <- list()
for (i in 1:(length(localidades)-1) ) {
  
  p <- ggplot()
  primerDiaFila <- fechasPrimerDia%>%filter(nombres==localidades[i])
  primerDia <- primerDiaFila$fechas
  minisRestringido <- rMinisterio%>%filter(V12==localidades[i])
  corte <- min(nrow(DATABETALOC[[localidades[i]]]),nrow(rMinisterio))
  minisRestringido <- minisRestringido[1:corte,]
  DATABETALOC[[localidades[i]]] <- DATABETALOC[[localidades[i]]][1:corte,]
  nrow(minisRestringido)
  nrow(DATABETALOC[[localidades[i]]])
  Rt <- as.numeric(gsub(",", ".", gsub("\\.", "", minisRestringido$V3)))
  Dia <-seq(as.Date(primerDia,format="%Y-%m-%d"), as.Date(primerDia,format="%Y-%m-%d")+(nrow(minisRestringido)-1), by="days")
  length(Dia)
  length(Rt)
  Rs <- DATABETALOC[[localidades[i]]]$prueba2.R
  length(Rs)
  dd <- data.frame(Dia,Rs)
  dd2 <-data.frame(Dia,Rt)
  p <- ggplot(data = dd, aes(x=Dia, y=Rs)) +
    geom_line(size=1.3,col="red") + 
    ylab("Rt")+xlab("")+ggtitle(localidades[i])
  
  p<-p+ geom_point(data = dd2,aes(x=Dia, y=Rt)) + 
    ylab("Rt")+xlab("")+geom_hline(yintercept=1, linetype="dashed", color = "blue")
  
  fechasRestringido <- fechasCuarentena%>%filter(Lugar=="Bogotá" | Lugar==localidades[i])
  
  for (j in 1:nrow(fechasRestringido)) {
    diferTime <- difftime( as.Date(fechasRestringido$FechaFinal[j],format="%Y-%m-%d"), as.Date(fechasRestringido$FechaInicio[j],format="%Y-%m-%d") , units = c("days"))
    for (k in 1:diferTime) {
      p <- p+geom_vline(xintercept=as.Date(fechasRestringido$FechaInicio[j],format="%Y-%m-%d")+k-1,
                        linetype=1, colour="blue",alpha = 0.1)
    }
  }
  graficas[[ localidades[i] ]] <- p
  print(p)
  
}

p <- ggplot()
primerDiaFila <- fechasPrimerDia%>%filter(nombres==localidades[1])
primerDia <- primerDiaFila$fechas
minisRestringido <- rMinisterio%>%filter(V12=="Bogotá D.C.")
corte <- min(nrow(DATABETALOCBogota),nrow(rMinisterio))
minisRestringido <- minisRestringido[1:corte,]
DATABETALOCBogota <- DATABETALOCBogota[1:corte,]
nrow(minisRestringido)
nrow(DATABETALOCBogota)
Rt <- as.numeric(gsub(",", ".", gsub("\\.", "", minisRestringido$V3)))
Dia <-seq(as.Date(primerDia,format="%Y-%m-%d"), as.Date(primerDia,format="%Y-%m-%d")+(nrow(minisRestringido)-1), by="days")
length(Dia)
length(Rt)
Rs <- DATABETALOCBogota$prueba2.R
length(Rs)
dd <- data.frame(Dia,Rs)
dd2 <-data.frame(Dia,Rt)
p <- ggplot(data = dd, aes(x=Dia, y=Rs)) +
  geom_line(size=1.3,col="red") + 
  ylab("Rt")+xlab("")+ggtitle("Bogotá")

p<-p+ geom_point(data = dd2,aes(x=Dia, y=Rt)) + 
  ylab("Rt")+xlab("")+geom_hline(yintercept=1, linetype="dashed", color = "blue")

fechasRestringido <- fechasCuarentena%>%filter(Lugar=="Bogotá" | Lugar=="Bogotá")

for (j in 1:nrow(fechasRestringido)) {
  diferTime <- difftime( as.Date(fechasRestringido$FechaFinal[j],format="%Y-%m-%d"), as.Date(fechasRestringido$FechaInicio[j],format="%Y-%m-%d") , units = c("days"))
  for (k in 1:diferTime) {
    p <- p+geom_vline(xintercept=as.Date(fechasRestringido$FechaInicio[j],format="%Y-%m-%d")+k-1,
                      linetype=1, colour="blue",alpha = 0.1)
  }
}
graficas[[ "Bogotá" ]] <- p
print(p)





grid.arrange(graficas[[localidades[3]]],graficas[[localidades[11]]])











maxColors <- length(localidades)
possibleColors <- colorRampPalette( brewer.pal( 9 , "Set1" ) )(maxColors)


p <- ggplot()
for (i in 1:length(localidades)) {
  primerDiaFila1 <- fechasPrimerDia%>%filter(nombres==localidades[i])
  primerDia1 <- primerDiaFila1$fechas
  Dia1 <-seq(as.Date(primerDia1,format="%Y-%m-%d"), as.Date(primerDia1,format="%Y-%m-%d")+(nrow(DATABETALOC[[localidades[i]]])-1), by="days")
  Bet1 <- DATABETALOC[[localidades[i]]]$prueba2.BETA
  dd1 <- data.frame(Dia1,Bet1)
  p <- p +geom_line(data = dd1, aes(x=Dia1, y=Bet1),size=0.5,color=possibleColors[i])
  }

p
    
 










   