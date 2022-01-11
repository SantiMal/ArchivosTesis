library(tidyverse)
library(deSolve)
library(readxl)
library(stats4)
library(bbmle)
library(writexl)

Path <- "C:/Users/MALAGON/Desktop/Pandemia 2/"

#Import fist date data
FechasPrimerDia<-read_excel( paste(Path,"FechasPrimerDia.xlsx" ,sep = "") ) 
localidades <- FechasPrimerDia$nombres

#Import localidades population and Pop have each localidad´s quantity
populationdat <- read_excel(paste(Path,"PoblacionLocalidades.xlsx" ,sep = "") )
##View(populationdat)
Pop <- list()
for (i in 1:nrow(populationdat)) {
  ee <-populationdat%>%filter(Localidad==localidades[i])
  Pop[[localidades[i]]] <- strtoi(ee$Población[1])
}


#uploading datosVacunacionColombia created in "tratamientoDatosVacunacion.R"
datosVacunacionColombia <-read_excel(paste(Path,"datosVacunacionColombia.xlsx" ,sep = "") ) 


View(datosVacunacionColombia)
#View(FechasPrimerDia)


for (i in 1:nrow(FechasPrimerDia)) {
  
  vac <- rep(0,FechasPrimerDia$diaVacunacion[i]-1)
  vac <-c(vac,datosVacunacionColombia$fullyBog*(Pop[[localidades[i]]]/7743955) )
  niv <- length(vac)
  day <- 1:(niv+300)
  vac <- c(vac,rep(0,300))
  cumvac <- cumsum(vac)
  daticos <- data.frame(day,vac,cumvac) 
  write_xlsx (daticos, sprintf(paste(Path,"VacunacionLocalidades/Vacunados%s.xlsx" ,sep = ""),localidades[i]))
}

#Same for Bogotá
dayBogota <- max(FechasPrimerDia$diaVacunacion)
vac <- rep(0,dayBogota-1)
vac <-c(vac,datosVacunacionColombia$fullyBog )
niv <- length(vac)
day <- 1:(niv+300)
vac <- c(vac,rep(0,300))
cumvac <- cumsum(vac)
daticos <- data.frame(day,vac,cumvac) 
write_xlsx (daticos, sprintf(paste(Path,"VacunacionLocalidades/Vacunados%s.xlsx" ,sep = ""),"Bogotá"))














