# Library

library(nortest)

library (moments)

library(stats)

library(forecast)

library(tidyr)

rm(list = ls())
#Cargar datos en formato csv

data1 <- read.csv("DATOS/RATIO FINAL 3-9-18.csv",header = TRUE, sep=";")

# Modificar datos

data1<- spread(data1,FINCA,RATIO)

##############################

# FINCAS

# GOLETA

farm <- data1$GOLETA
pars <- c(20,0,4)
ini  <- c(2009, 1)
semP <- 23
nomFinca <- "Goleta"

frCast <- function(dat, pars,semP,...) {forecast(arima(dat, order=pars ,...),h=semP)}

generar_pronostico <- function(farm,pars,ini,semP,nomFinca){

farm <- ts(farm, start =ini )
cat(1)
#armMod <- arima(farm, order=pars)
cat(2)
frCast <- frCast(farm,pars,semP = semP)
cat(3)
plot(frCast,main=nomFinca)


png(paste0('GRAFICOS/',nomFinca,'.png'),width = 800, height = 450)
plot(frCast,main=nomFinca)
dev.off()

write.csv(data.frame(prediccion=as.numeric(frCast$mean)),paste('PREDICCION/',nomFinca,'.csv'))
}

generar_pronostico(farm = data1$GOLETA,pars = c(20,0,4),ini = c(2009, 1),20,"Goleta")
