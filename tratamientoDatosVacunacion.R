library(writexl)
library(lubridate)
library(readr)
library(readxl)
library(tidyverse)
library(utf8)

#MF :D
porcen <- 1.38/(27665*100/(1455250))
1/porcen

Path <- "C:/Users/MALAGON/Desktop/Pandemia 2/"

vaciMinistertio <- read_excel(paste(Path,"Dosisaplicadas.xlsx",sep = ""))
vaciMinistertio <- vaciMinistertio%>%select(FECHAS,Bogotá)

vaccinations <- read.csv(paste(Path,"vaccinations.csv",sep = ""))
vaccinations <- vaccinations%>%filter( location=="Colombia")
nRowVacc <- nrow(vaccinations)

#as.Date(vaciMinistertio$FECHAS[nRowVacc],format="%Y-%m-%d")==as.Date(vaccinations$date[nRowVacc],format="%Y-%m-%d")
#vaciMinistertio$FECHAS[nRowVacc]


#function that generate a line and return ponts that lie in
generateLine <- function(n1,n2,interv, num){
  return( ((n2-n1)/interv)*num + n1 )
}


#Filling NA with generateLine in total
i <- 1
while ( i < nRowVacc ) {
  
  if (is.na(vaccinations$total_vaccinations[i])) {
    n1 <- vaccinations$total_vaccinations[i-1]
    n2 <- 0
    interv <- 1
    newi <- i
    for (j in (i+1):(i+20)) {
      interv <- interv + 1
      if (!is.na(vaccinations$total_vaccinations[j])){
        n2 <- vaccinations$total_vaccinations[j]
        newi <- j
        break
      }
    }
    for (j in i:(i+interv-2)) {
      vaccinations$total_vaccinations[j] <- generateLine(n1,n2,interv,j-i+1)
    }
    i <- newi
  }
  i <- i + 1
}

#Filling dailing_vaccination_raw
vaccinations$daily_vaccinations_raw[1] <- 18
for (j in 2:nRowVacc) {
  if (is.na(vaccinations$daily_vaccinations_raw[j])) {
    vaccinations$daily_vaccinations_raw[j] <- vaccinations$total_vaccinations[j]-vaccinations$total_vaccinations[j-1]
  }
}

View(vaccinations)


#Filling first fullyvaccinated
for (j in 1:24) {
  vaccinations$people_fully_vaccinated[j] <- 0
}

#Filling NA with generateLine in fully
i <- 1
while ( i < nRowVacc ) {
  
  if (is.na(vaccinations$people_fully_vaccinated[i])) {
    n1 <- vaccinations$people_fully_vaccinated[i-1]
    n2 <- 0
    interv <- 1
    newi <- i
    for (j in (i+1):(i+20)) {
      interv <- interv + 1
      if (!is.na(vaccinations$people_fully_vaccinated[j])){
        n2 <- vaccinations$people_fully_vaccinated[j]
        newi <- j
        break
      }
    }
    for (j in i:(i+interv-2)) {
      vaccinations$people_fully_vaccinated[j] <- generateLine(n1,n2,interv,j-i+1)
    }
    i <- newi
  }
  i <- i + 1
}

#write.csv(vaccinations,"C:/Users/MALAGON/Desktop/Pandemia 2/vaccinationsRelleno.csv", row.names = FALSE)

FECHAS <- vaccinations$date
daily_vaccinations_raw <- vaccinations$daily_vaccinations_raw
people_fully_vaccinated_raw <- c(0)
percentage <- c()
Bogotá <- vaciMinistertio$Bogotá[1:nRowVacc] 
fullyBog <- c()

for (j in 2:nRowVacc) {
  people_fully_vaccinated_raw[j] <- vaccinations$people_fully_vaccinated[j]-vaccinations$people_fully_vaccinated[j-1]
}
for (j in 1:nRowVacc) {
  percentage[j] <- people_fully_vaccinated_raw[j]/daily_vaccinations_raw[j]
  fullyBog[j] <- percentage[j]*Bogotá[j]
}


datosVacunacionColombia <- data.frame(FECHAS,Bogotá,daily_vaccinations_raw,people_fully_vaccinated_raw,percentage,fullyBog) 
write_xlsx (datosVacunacionColombia, paste(Path,"datosVacunacionColombia.xlsx",sep = ""))





  