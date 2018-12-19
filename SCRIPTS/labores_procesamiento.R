

#Desmache
#
#Jorge Montesino & Marco Romero
#
#29/8/18
#
#
library(reshape2)
library(xlsx)

rm(list = ls())

Labores<-read.csv("Datos/Labores Relacion Adjunta.csv",sep=";")
head(Labores)

Labores1 <- Labores[grep("DESMACHE|FERTILIZACION",Labores$Labor),]

Labores1 <- droplevels(Labores1)

Labores1$Nuev_labor <- as.character(Labores1$Labor)

Labores1$Nuev_labor <- gsub(" +$","",Labores1$Nuev_labor) # Gsub es como un reemplazar datos

Labores1$Nuev_labor[grep("DESMACHE",Labores1$Labor)] <- "DESMACHE"

Labores_cantidad <- Labores1[c("Finca" ,   "Anio"  , "Semana","Lote_Canal","Nuev_labor","Cantidad")]

Labores_cantidad_Trans <- acast(Labores_cantidad,Finca+ Anio +Semana+ Lote_Canal~  Nuev_labor,sum)

Labores_cantidad_Conteo_Trans <- acast(Labores_cantidad,Finca+ Anio +Semana+ Lote_Canal~  Nuev_labor,length)


Labores_cantidad_Trans  <- data.frame(ID = row.names(Labores_cantidad_Trans),Labores_cantidad_Trans)


Labores_cantidad_Conteo_Trans <- data.frame(ID = row.names(Labores_cantidad_Conteo_Trans),Labores_cantidad_Conteo_Trans)


LaboresFinales <- merge(Labores_cantidad_Conteo_Trans,Labores_cantidad_Trans,by="ID",all.x=F,all.y = F)

names(LaboresFinales) <- gsub(".x","Conteo",names(LaboresFinales)) # Remplazar caracter por nuevo valor el gsub

names(LaboresFinales) <- gsub(".y","sum",names(LaboresFinales))

write.csv(LaboresFinales,"Datos/LaboresFinalesProcesadas.csv",row.names = F)

####################################################################################

# Verificada
