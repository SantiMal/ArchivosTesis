library(tidyverse)
library(deSolve)
library(readxl)
library(stats4)
library(bbmle)
library(writexl)


Path <- "C:/Users/MALAGON/Desktop/Pandemia 2/"

#####################################################################################
#Charge LOCALIDADES dataset

FechasPrimerDia<-read_excel( paste(Path,"FechasPrimerDia.xlsx" ,sep = "") ) 
FechasPrimerDia<-FechasPrimerDia%>%filter(nombres!="Fuera de Bogotá")%>%filter(nombres!="Sin dato")
localidades <- FechasPrimerDia$nombres

############################################################################3
#Import localidades population and Pop have each localidad´s quantity
populationdat <- read_excel(paste(Path,"PoblacionLocalidades.xlsx" ,sep = "") )
##View(populationdat)
Pop <- list()
for (i in 1:nrow(populationdat)) {
  Pop[[populationdat$Localidad[i]]] <- strtoi(populationdat$Población[i])
}

##########################################################3
#DATALOC is a List with tables with cases and days for each localidad
#DATALOCVAC is a List with tables with vaccination for each localidad
#MAX is a List with max number of cases in each localidad
DATALOC <- list()
DATALOCVAC <- list()
MAX <- list()
for (i in 1:length(localidades) ) {
  DATALOC[[localidades[i]]] <- read_excel(sprintf( paste(Path,"Localidades/%s.xlsx" ,sep = ""),localidades[i]))
  DATALOCVAC[[localidades[i]]] <- read_excel(sprintf(paste(Path,"VacunacionLocalidades/Vacunados%s.xlsx" ,sep = ""),localidades[i]))
  MAX[[localidades[i]]] <- max(DATALOC[[localidades[i]]]$cases)
}
#DATALOCBogota is a table with infect cases and days for Bogotá
#DATAVaccination is a table with vaccination for Bogotá
#MAXBogota is max number of cases in Bogotá
DATALOCBogota <- read_excel(paste(Path,"datosBogotaTotal.xlsx" ,sep = ""))
DATAVaccination <- read_excel(paste(Path,"VacunacionLocalidades/VacunadosBogotá.xlsx" ,sep = ""))
MAXBogota <-max(DATALOCBogota$cases)
PopBogota <- 7743955

############################

#Writte function for later use in ode function (Bogotá default)
sir_equations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -beta * I * S-DATAVaccination$vac[floor(time+1)]/PopBogota
    dI <-  beta * I * S - (1/10)* I
    dR <-  (1/10) * I+DATAVaccination$vac[floor(time+1)]/PopBogota
    return(list(c(dS, dI, dR)))
  })
}

# This function sir_1 produce a table with the discrete diferential equation´s solution in times
sir_1 <- function(beta, S0, I0, R0, times) {
  require(deSolve) # for the "ode" function
  # the parameters values:
  parameters_values <- c(beta  = beta)
  # the initial values of variables:
  initial_values <- c(S = S0, I = I0, R = R0)
  # solving
  out <- ode(initial_values, times, sir_equations, parameters_values,method = "rk4")
  # returning the output:
  return(as.data.frame(out))
}

######################################################
#Define MF ()
MF <- 1
######################################################

#Función que pinta 1
model_fit <- function(beta, data, N , dataVac, ...) {
  I0 <- data$cases[1]*MF/N # initial number of infected (from data)
  R0 <- data$recover[1]/N
  times <- data$day   # time points (from data)
  
  # model's predictions:
  predictions <- sir_1(beta = beta,   # parameters
                       S0 = 1 - I0 - R0-dataVac/N, I0 = I0, R0 = R0+dataVac/N, # variables' intial values
                       times = times)                 # time points
  
  # plotting the observed prevalences:
  with(data, plot(day, cases*MF/N, ...))
  # adding the model-predicted prevalence:
  with(predictions, lines(time, I, col = "red"))
  
  lastReal <- data$cases[length(data$cases)-1]
  lastPrediction <- predictions$I[length(predictions$I)-1]
  return(lastPrediction-lastReal)
}

#Funcion que pinta 2

model_fit2 <- function(beta,sigma, data, N,dataVac, ...){
  
  I0 <- data$cases[1]*MF/N # initial number of infected (from data)
  R0 <- data$recover[1]*MF/N
  times <- data$day   # time points (from data)
  
  # model's predictions:
  best_predictions <- sir_1(beta = beta,   # parameters
                       S0 = 1 - I0 - R0-(dataVac/N), I0 = I0, R0 = R0+(dataVac/N), # variables' intial values
                       times = times)$I 
  # confidence interval of the best predictions:
  cl <- 0.95 # confidence level
  cl <- (1 - cl) / 2
  lwr <- qnorm(p = cl, mean = best_predictions, sd = sigma)
  upr <- qnorm(p = 1 - cl, mean = best_predictions, sd = sigma)
  # layout of the plot:
  plot(times, times, ylim = c(0, max(upr)), type = "n",
       xlab = "time (days)", ylab = "prevalence")
  # adding the predictions' confidence interval:
   # predictions start from the second data point
  polygon(c(times, rev(times)), c(upr, rev(lwr)),
          border = NA, col = adjustcolor("red", alpha.f = 0.1))
  # adding the model's best predictions:
  lines(times, best_predictions, col = "red")
  # adding the observed data:
  with(data, points(day, cases*MF/N, pch = 19, col = "red"))
  
  
  lastReal <- data$cases[length(data$cases)-1]
  lastPrediction <- best_predictions[length(best_predictions)-1]
  return(lastPrediction-lastReal)
}


#minus log-likelihood function
mLL <- function(beta, sigma, day, cases, recover, N ,dataVac) {
  beta <- exp(beta) # to make sure that the parameters are positive
  sigma <- exp(sigma)
  I0 <- cases[1]*MF/N # initial number of infectious
  R0 <- recover[1]*MF/N
  observations <- cases[-1]*MF/N # the fit is done on the other data points
  predictions <- sir_1(beta = beta,
                       S0 = 1 - I0 - R0-dataVac/N, I0 = I0, R0 = R0+dataVac/N, times = day)
  predictions <- predictions$I[-1] # removing the first point too
  # returning minus log-likelihood:
  -sum(dnorm(x = observations, mean = predictions, sd = sigma, log = TRUE))
}


################################################################################################################
#Function for produce the fist beta for iteration

produceBeta <- function(data){
  data <- data%>% 
    mutate(cases = ifelse(cases == 0, 0.5, cases))
  model <- lm(log(cases) ~ day, data = data)
  m <- coefficients(model)["day"][[1]] #slope of regretion 
  return(m)
}

MF <- 1
#Prueba Bogotá ###############
print("Bogotá con nelder")

DATALOCBogota2 <- DATALOCBogota[1:(nrow(DATALOCBogota)-15),]
m <- produceBeta(DATALOCBogota2)
betaInitial <- (10*m+1)*(1/10)
print("------------beta pendiente con regresión-------------")
print(betaInitial)
print("------------parametros-------------")

starting_param_val <- list(beta = betaInitial, sigma = 0.1)
yy <- mle2(minuslogl = mLL, start = lapply(starting_param_val, log),
           method = "CG", data = c(DATALOCBogota2, N = PopBogota,dataVac=DATAVaccination$cumvac[1]))
print( exp(coef(yy)) )

betaFit <-as.numeric(exp(coef(yy))[1])
error <- model_fit(beta = betaFit, DATALOCBogota2,N=PopBogota,dataVac=DATAVaccination$cumvac[1], pch = 10, col = "red", ylim = c(0, MAXBogota*1.83/PopBogota))
title(main = "Bogotá CG" )
DATAVaccination$vac[1]
############################################################################################################


DrawForWindowss <- function(DATALOC,localidad,vacunation,Pop,max,window,static=FALSE,salto=1){
  
  #Writte function for later use in ode function 
  sir_equations <- function(time, variables, parameters) {
    with(as.list(c(variables, parameters)), {
      dS <- -beta * I * S-vacunation$vac[floor(time+1)]/Pop
      dI <-  beta * I * S - (1/10)* I
      dR <-  (1/10) * I+vacunation$vac[floor(time+1)]/Pop
      return(list(c(dS, dI, dR)))
    })
  }
  
  wdow <- window-1
  estimatess <- list()
  betas <- c()
  sigmas <- c()
  Rs <- c()
  jump <- 1
  endJump <- TRUE
  iter <- 1
  mser <- 0
  
  while ( endJump ) {
    top <- jump + wdow
    if((jump + wdow)> nrow(DATALOC) ){
      top <- nrow(DATALOC)
      endJump <- FALSE
    }
    
    day <- DATALOC$day[jump:top]
    cases <- DATALOC$cases[jump:top]
    recover <- DATALOC$recover[jump:top]
    vacunadosCum <- vacunation$cumvac[jump:top]
    DATALOC2 <- data.frame(day,cases,recover)
    
    m <- produceBeta(DATALOC2)
    betaInitial <- (10*m+1)*(1/10)
    print(jump)
    print(top)
    starting_param_val <- list(beta = betaInitial, sigma = 0.1)
    
    try(
    estimatess[[iter]] <- mle2(minuslogl = mLL, start = lapply(starting_param_val, log),
                               method = "CG", data = c(DATALOC2, N = Pop, dataVac=vacunadosCum[1]))
    ,silent = TRUE)
    estimatess[[iter+1]] <- estimatess[[iter]]
    betaFit <-as.numeric(exp(coef(estimatess[[iter]]))[1])
    sigmaFit <- as.numeric(exp(coef(estimatess[[iter]]))[2])
    SFinal <- Pop-DATALOC2$cases[length(DATALOC2$cases)]-DATALOC2$recover[length(DATALOC2$cases)]
    Rs <- c(Rs,(betaFit*SFinal)/(Pop*(1/10)))
    betas <- c(betas,as.numeric(exp(coef(estimatess[[iter]]))[1]))
    sigmas <- c(sigmas,as.numeric(exp(coef(estimatess[[iter]]))[2]))
    
    cat(c(localidad),"iteración",iter)
    
    print("    ")
    print( exp(coef(estimatess[[iter]])) )
    
    error <- model_fit2( beta = betaFit,sigma = sigmaFit, DATALOC2, N=Pop, dataVac=vacunadosCum[1])
    title(main = sprintf("%s",localidad) )
    mser <- mser + error**2
    
    print("-------------------------------------------------------------")
    
    iter <- iter +1
    if(static==FALSE){
      jump <- top
    }else{
      jump <- jump +salto
    }
  }
  print("-------------------------------------------------------------")
  print("-------------------------------------------------------------")
  print("-------------------------------------------------------------")
  print("ERROR!!!")
  print("-------------------------------------------------------------")
  print("-------------------------------------------------------------")
  print("-------------------------------------------------------------")
  print(mser/iter)
  ERR <- mser/iter
  return( list(ESTIMATE=estimatess,BETA=betas,SIGMA=sigmas,R=Rs,ERROR=ERR) )
}
################################################################################
#Write and save Betas for each localidad an Bogotá
u <- 1
vent <-8
localidades[u]

prueba2 <- DrawForWindowss(DATALOC[[localidades[u]]][1:(nrow(DATALOC[[localidades[u]]])-15),],localidades[u],DATALOCVAC[[localidades[u]]],Pop[[localidades[u]]],MAX[[localidades[u]]],vent,static=TRUE)


stdError <- c()
for (i in 1:length(prueba2$ESTIMATE)) {
  stdError <- c(stdError,coef(summary(prueba2$ESTIMATE[[i]]))[, "Std. Error"][[1]])
}
stdError <-stdError[1:(length(stdError)-1)]
dataprov <- data.frame(prueba2$BETA,prueba2$SIGMA,prueba2$R,stdError )
write_xlsx (dataprov, sprintf(paste(Path,"UltimosBetas/beta%sVentana7.xlsx" ,sep = ""),localidades[u]))



for (i in 2:length(localidades)) {
  u <- i
  vent <-8
  localidades[u]
  
  prueba2 <- DrawForWindowss(DATALOC[[localidades[u]]][1:(nrow(DATALOC[[localidades[u]]])-15),],localidades[u],DATALOCVAC[[localidades[u]]],Pop[[localidades[u]]],MAX[[localidades[u]]],vent,static=TRUE)
  
  
  stdError <- c()
  for (i in 1:length(prueba2$ESTIMATE)) {
    stdError <- c(stdError,coef(summary(prueba2$ESTIMATE[[i]]))[, "Std. Error"][[1]])
  }
  stdError <-stdError[1:(length(stdError)-1)]
  dataprov <- data.frame(prueba2$BETA,prueba2$SIGMA,prueba2$R,stdError )
  write_xlsx (dataprov, sprintf(paste(Path,"UltimosBetas/beta%sVentana7.xlsx" ,sep = ""),localidades[u]))
  
}

#For Bogotá
vent <-8
prueba2 <- DrawForWindowss(DATALOCBogota[1:(nrow( DATALOCBogota )-15),],"Bogotá",DATAVaccination,PopBogota,MAXBogota,vent,static=TRUE)
stdError <- c()
for (i in 1:length(prueba2$ESTIMATE)) {
  stdError <- c(stdError,coef(summary(prueba2$ESTIMATE[[i]]))[, "Std. Error"][[1]])
}
stdError <-stdError[1:(length(stdError)-1)]
dataprov <- data.frame(prueba2$BETA,prueba2$SIGMA,prueba2$R,stdError )
write_xlsx (dataprov, sprintf(paste(Path,"UltimosBetas/beta%sVentana7.xlsx" ,sep = ""),"Bogotá"))








windlength <- c()
jump <- c()
er <- c()


for (i in 1:35) {
  if(5*i+10>max(INF[[ localidades[1] ]]$day)){break}
  for (j in 1:10) {
    if(j<5*i+10){
      prueba0 <- DrawForWindowss(INF[[ localidades[1] ]],localidades[1],Pop[[localidades[1]]],MAX[[localidades[1]]],5*i+10,static=TRUE,salto=j)
      windlength <- c(windlength,5*i+10)
      jump <- c(jump,j)
      er <- c(er,prueba0[[2]])
    }
  }
  
}


datosEngativa <- data.frame(windlength,jump,er)

#Se guardan y exportan los datos a excel 

write_xlsx (datosEngativa, sprintf("C:/Users/MALAGON/Desktop/Pandemia/Localidades/VentanasUsaquen sin gamma.xlsx"))




















