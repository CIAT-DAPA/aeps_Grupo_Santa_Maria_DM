
# vecinos mas cercanos para asemejar nuevos lotes, con lotes anteriores
# Hugo Dorado
# 25-09-2018

library(caret)
library(tidyr)
library(ggplot2)

source('SCRIPTS/nearest_neighbor_FUN.R')

suelo_hist <- read.csv('DATOS/SUELO/ConsolidadoSuelos.csv',row.names = 2,na.strings = c('#N/A',"NA", ''))

suelo_nuev <- read.csv('DATOS/SUELO/Analisis quimicos_nueva finca_2018.csv',
                       sep=';',na.strings = c('#N/A',"NA", ''))

suelo_hist <- suelo_hist[,c(6:10,16:27)]

names(suelo_hist) <- c('Arena','Arcilla','Limo','pH','MO','Al','B','Ca','CICE',
                       'Cu','Fe','K','Mg','Mn','P','S','Zn')

suelo_nuev <- suelo_nuev[,c(3:ncol(suelo_nuev))]

names(suelo_nuev) <- c('Arena','Limo','Arcilla','pH','MO','S','P','K','Ca','Mg',
                       'Al','CICE','Cu','Zn','Fe','Mn','B')

suelo_nuev$Arena   <- suelo_nuev$Arena/100 
suelo_nuev$Arcilla <- suelo_nuev$Arcilla/100 
suelo_nuev$Limo    <- suelo_nuev$Limo/100 

# Ordenar variables

sortVars <- names(suelo_hist)

suelo_hist <- suelo_hist[complete.cases(suelo_hist),]

suelo_nuev <- suelo_nuev[names(suelo_hist)]

# Exploratory data analysis

suelo_hist_BX <- gather(suelo_hist,variable,value)

suelo_hist_BX$data <- 'Historico'

suelo_nuev_BX <- gather(suelo_nuev,variable,value)

suelo_nuev_BX$data <- 'Nuevo'

ggS <-
ggplot(
rbind(suelo_hist_BX,suelo_nuev_BX),aes(data,y=value)
)+geom_boxplot()+
  facet_wrap(~variable,scales='free')+ggtitle('Todos')

ggsave('REPORTS/soil_info.png',ggS)

# Normalizacion

normRG <- preProcess(suelo_hist,method = 'range')

norm_suelo_hist <- predict(normRG,suelo_hist)

norm_suelo_nuev <- predict(normRG,suelo_nuev)

distMat <- distance_matrix2(base=norm_suelo_hist,nuevo = norm_suelo_nuev)


# Vecinos mas cercano

vecinos_cercanos <- extract_neigthbors(suelo_hist,suelo_nuev,distMat,k=5)

head(vecinos_cercanos)

write.csv(vecinos_cercanos,'REPORTS/vecinos_mas_cercanos_suelo.csv',row.names = F)



