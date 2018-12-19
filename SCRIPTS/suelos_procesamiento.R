
# Analisis de suelo
rm(list = ls())

library(reshape2)
library(tidyr)

###############################
ASue <- read.csv("Datos/Analisis de suelo.csv", sep=";")

names(ASue) <- gsub("X..","",names(ASue))

ASue$FEANALISIS <- substring(ASue$FEANALISIS,7,10) 

ASue$FEANALISIS <- as.numeric(ASue$FEANALISIS ) # Tranforma la variable a numerica

names(ASue)[9] <- "ANIO" # Cambia el nombre de una variable de interes

sinRep <- ASue[c("FINCA", "Lotes", "UNIDAD.SUELO","ANIO","ARENA","Arcilla","LIMO","PH","MATERIA.ORGANCICA","DSTERA")]


sinRep <- unique(sinRep)

sinRep <- sinRep[sinRep$PH != "",]

sinRep <- sinRep[sinRep$DSTERA != "",]

sinRep <-
data.frame(ID=paste(sinRep$FINCA,sinRep$Lotes,sinRep$UNIDAD.SUELO,sinRep$ANIO,sep="_"),
     sinRep
)


# tail(sort(table(with(Rep,paste0(FINCA,Lotes,UNIDAD.SUELO,ANIO,ELEMENTO)))))

Rep <- ASue[c("FINCA", "Lotes", "UNIDAD.SUELO","ANIO","ELEMENTO"  ,  "CANTIDAD")]

Rep <- spread(Rep,ELEMENTO,CANTIDAD)

sort(table(with(Rep,paste0(FINCA,Lotes,UNIDAD.SUELO,ANIO))))


Rep1 <- cbind(Rep,ID=with(Rep,paste(FINCA,Lotes,UNIDAD.SUELO,ANIO,sep="_")))

Rep1 <- as.data.frame(Rep1)

write.csv( merge(sinRep,Rep1,by="ID",all=T,sort=F),"Datos/ConsolidadoSuelos.csv")

################################################################################3


