
library(tidyr)

# Consolidacion base de datos
rm(list = ls())

labores   <- read.csv("Datos/LaboresFinalesProcesadas.csv",sep=";")
poblacion <- read.csv("Datos/pob_embolses_definitivo.csv",sep=",")
anasuelos <- read.csv("Datos/ConsolidadoSuelos.csv")

# Labores

names(labores)
names(poblacion)


  
varSuelo <- 
c("ID","FINCA.x","Lotes.x","UNIDAD.SUELO.x","ANIO.x","ARENA","Arcilla","LIMO",
  "PH","MATERIA.ORGANCICA","DSTERA","Al","B","Ca","CICE","Cu","Fe",
  "K","Mg","Mn","P","S","Zn")

suelos <- anasuelos[varSuelo]

#


id <- labores["ID"]

id <- separate(id,1,c("FINCA","ANIO","SEMANA","LOTE"),sep="_")

ID_FIC_ANIO_SEM <- with(id,paste(FINCA,ANIO,LOTE,sep="_"))

ID_FIC_ANIO_SEM <-  gsub(" ", "", ID_FIC_ANIO_SEM, fixed = TRUE)

ID_FIC_LOT_ANI_SEM <- with(id,paste(FINCA,LOTE,ANIO,SEMANA,sep="_"))

ID_FIC_LOT_ANI_SEM <-  gsub(" ", "", ID_FIC_LOT_ANI_SEM, fixed = TRUE)

labores <- data.frame(ID_FIC_ANIO_SEM,ID_FIC_LOT_ANI_SEM,labores)

# Poblacion

ID_FIC_ANIO_SEM <- with(poblacion,paste(finca,anio,Lote,sep="_"))

ID_FIC_ANIO_SEM <-  gsub(" ", "", ID_FIC_ANIO_SEM, fixed = TRUE)

poblacion <- data.frame(ID_FIC_ANIO_SEM,poblacion)
ID_FIC_LOT_ANI_SEM <- with(poblacion,paste(finca,Lote,anio,Semana,sep="_"))

ID_FIC_LOT_ANI_SEM <- gsub(" ", "", ID_FIC_LOT_ANI_SEM, fixed = TRUE)

poblacion <- data.frame(ID_FIC_LOT_ANI_SEM,poblacion)

# Suelos

ID_FIC_ANIO_SEM <- with(suelos,paste(FINCA.x,ANIO.x,Lotes.x,sep="_"))

ID_FIC_ANIO_SEM <-  gsub(" ", "", ID_FIC_ANIO_SEM, fixed = TRUE)

suelos <- data.frame(ID_FIC_ANIO_SEM,suelos)

# Union de bases de datos

pobLab  <- 
merge(poblacion,labores,by.x = "ID_FIC_LOT_ANI_SEM",by.y = "ID_FIC_LOT_ANI_SEM",all.x = T,all.y = F)

pobLabSuel <- merge(suelos,pobLab,by.x="ID_FIC_ANIO_SEM",by.y="ID_FIC_ANIO_SEM.x",all.x = F,all.y = T)

write.csv(pobLabSuel,"base_datos_consolidad.csv")

###########################################################################33

# Verificada