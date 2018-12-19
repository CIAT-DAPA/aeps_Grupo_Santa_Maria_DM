
#Proyeccion Embolses
#
#Jorge Montesino & Marco Romero
#
#29/8/18
#
#
# Limpieza de datos de poblacion y embolses historicos

#Cargamos paquetes

library(tidyr)
library(ggplot2)
library(xlsx)
# Cargamos la base de datos

Embolses<- read.csv("Datos/EmbolsesSemana30.csv", sep=";", dec=".", header=T,na.string = "#¡DIV/0!")
Poblacion <- read.csv("Datos/Poblaciones-clones.csv", sep = ";",header = T, dec = ".",na.string = "#¡DIV/0!")
IDFINCAS <- read.xlsx('Datos/ID fincas.xlsx',sheetName = 'ID',rowIndex = 1:421,colIndex = 1:26)

#################################################################


###############################################################
# Tranformaciones de tablas: Formato poblaciones

pob2 <- Poblacion[,c("FINCA","LOTE.1","VARIEDAD.CLON","Plantasxha.2018","Plantasxha.2017")] #Extraigo
#las variables que voy a trabajar en asoacion con el otro archivo

re_pob <- gather(pob2,anio,poblacion,-FINCA,-LOTE.1,-VARIEDAD.CLON) #

nueva_base_pob <- separate(re_pob,"anio",c("var","anio")) #separar las palabras

nueva_base_pob$anio <- as.numeric(nueva_base_pob$anio)

separate
####################################################################

Embolses$finca <- as.character(Embolses$finca)

nueva_base_pob$FINCA <- as.character(nueva_base_pob$FINCA)


Embolses$Lote <- as.character(Embolses$Lote)


nueva_base_pob$FINCA <- as.character(nueva_base_pob$FINCA)


Pob_Embolses <-
  merge(Embolses,nueva_base_pob,by.x=c("finca","Lote","anio"),by.y=c("FINCA","LOTE.1","anio"),
        all.x = T,all.y =F,sort=F)


save(Pob_Embolses,file = "RData/Pob_Embolses.RData")

write.csv(Pob_Embolses,"Datos/pob_embolses_definitivo.csv")

####################################################################



# Verificada