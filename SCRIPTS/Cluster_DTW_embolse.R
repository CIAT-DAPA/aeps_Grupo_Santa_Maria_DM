
# Analisis cluster de patrones de embolse
# Hugo Andres Dorado B
# 11-10-2018

library(plyr)
library(ggplot2)
library(dtw)
library(dtwclust)
library(plyr)

rm(list=ls())

source('SCRIPTS/funciones_cluster_temporal.R')

source('SCRIPTS/Cluster_DTW_embolse_FUN.R')

# Leer y alistar datos


embolse <- read.csv('DATOS/Embolse/Embolse_Cinta-semana30.csv')

embolse_lst <-
       split( embolse , paste(embolse$finca,embolse$anio,embolse$Lote,sep='_') )  


conteoFilas <- do.call(rbind,lapply(embolse_lst,dim))
summary(conteoFilas)

boxplot(conteoFilas[,1])

conteoFilas <- conteoFilas[order(conteoFilas[,1],decreasing = F),]

ct <- conteoFilas[conteoFilas[,1] > 51,]

emls_50_51 <- embolse_lst[row.names(ct)]

time_series <- lapply( emls_50_51,function(x){x['Embolse']} )

time_series <- lapply(time_series,ts)

# time_series <- time_series[1:30]

# lapply(time_series,function(x){plot(x,type='l')})

##----------------------------------------------------------------------------##
# -------------------Calcular matriz de distancia dtw---------------------------


distAllMatrix <- distDtwMV(time_series)

# save(distAllMatrix,file=here::here('DATOS','distAllMatrix_media_movil.RDATA'))

load(here::here('DATOS','distAllMatrix_media_movil.RDATA'))

# Cluster a (19 CLUSTER)

hClustEvents <- hirarCluster(distAllMatrix)

dfResults <- data.frame(Nam_time_series = names(time_series),
                        clust = hClustEvents)

spl_time_series <- split(time_series,hClustEvents)

dirSave  <- 'CLUSTER_19' 

if(!dir.exists(dirSave)){dir.create(dirSave)}

write.csv(dfResults,paste(dirSave,'/',dirSave,'.csv',sep=''),row.names = F)

tablResults <- ddply(dfResults,~clust,summarise,conteo = length(clust))

write.csv(tablResults,paste(dirSave,'/conteo_',dirSave,'.csv',sep=''),row.names = F)

graphics_cluster(ts= time_series,ts_per_cluster=spl_time_series,dirSave,limites_fijos=F)

# Cluster b (7 CLUSTER)

hClustEvents <- hirarCluster(distAllMatrix)

dfResults <- data.frame(Nam_time_series = names(time_series),
                        clust = hClustEvents)

spl_time_series <- split(time_series,hClustEvents)

dirSave  <- 'CLUSTER_7' 

if(!dir.exists(dirSave)){dir.create(dirSave)}

write.csv(dfResults,paste(dirSave,'/',dirSave,'.csv',sep=''),row.names = F)

tablResults <- ddply(dfResults,~clust,summarise,conteo = length(clust))

write.csv(tablResults,paste(dirSave,'/conteo_',dirSave,'.csv',sep=''),row.names = F)

graphics_cluster(ts= time_series,ts_per_cluster=spl_time_series,dirSave,limites_fijos=F)


# Cluster a (39 variables)

hClustEvents <- hirarCluster(distAllMatrix)

dfResults <- data.frame(Nam_time_series = names(time_series),
                        clust = hClustEvents)

spl_time_series <- split(time_series,hClustEvents)

dirSave  <- 'CLUSTER_39' 

if(!dir.exists(dirSave)){dir.create(dirSave)}

write.csv(dfResults,paste(dirSave,'/',dirSave,'.csv',sep=''),row.names = F)

tablResults <- ddply(dfResults,~clust,summarise,conteo = length(clust))

write.csv(tablResults,paste(dirSave,'/conteo_',dirSave,'.csv',sep=''),row.names = F)

graphics_cluster(ts= time_series,ts_per_cluster=spl_time_series,dirSave,limites_fijos=F)


# results report

################################################################################
################          Nomralize time series          #######################

normTime_series <- lapply(time_series,function(x){
    (x-min(x))/(max(x)-min(x))
  }
)


distAllMatrixNorm <- distDtwMV(normTime_series)

save(distAllMatrixNorm,file=here::here('DATOS','distAllMatrixNorm.RDATA'))



hClustEvents <- hirarCluster(distAllMatrixNorm)

dfResults <- data.frame(Nam_time_series = names(time_series),
                        clust = hClustEvents)

spl_time_series <- split(time_series,hClustEvents)

dirSave  <- 'CLUSTER_14_norm' 

if(!dir.exists(dirSave)){dir.create(dirSave)}

write.csv(dfResults,paste(dirSave,'/',dirSave,'.csv',sep=''),row.names = F)

tablResults <- ddply(dfResults,~clust,summarise,conteo = length(clust))

write.csv(tablResults,paste(dirSave,'/conteo_',dirSave,'.csv',sep=''),row.names = F)

graphics_cluster(ts= time_series,ts_per_cluster=spl_time_series,dirSave,limites_fijos=F)


################################################################################
############################# Cluster Reducido #################################

cluster_19 <- read.csv('CLUSTER_19/CLUSTER_19.csv')

table(cluster_19$clust)

time_series <- time_series[names(time_series) %in% as.character(cluster_19[cluster_19$clust %in% 1:3,][,1])]

distAllMatrix <- distDtwMV(time_series)

#save(distAllMatrix,file=here::here('DATOS','distAllMatrix_Filtrada.RDATA'))

load(here::here('DATOS','distAllMatrix_Filtrada.RDATA'))

# Cluster a (19 CLUSTER)

hClustEvents <- hirarCluster(distAllMatrix)

dfResults <- data.frame(Nam_time_series = names(time_series),
                        clust = hClustEvents)

spl_time_series <- split(time_series,hClustEvents)

dirSave  <- 'CLUSTER_19/CLUSTER_13' 

if(!dir.exists(dirSave)){dir.create(dirSave)}

write.csv(dfResults,paste(dirSave,'/','CLUSTER_13.csv',sep=''),row.names = F)

tablResults <- ddply(dfResults,~clust,summarise,conteo = length(clust))

write.csv(tablResults,paste(dirSave,'/conteo_','CLUSTER_13.csv.csv',sep=''),row.names = F)

graphics_cluster(ts= time_series,ts_per_cluster=spl_time_series,dirSave,limites_fijos=F)



