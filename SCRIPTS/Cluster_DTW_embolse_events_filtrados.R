
# Analisis cluster de patrones de embolse
# Hugo Andres Dorado B
# 11-10-2018


library(ggplot2)
library(dtw)
library(dtwclust)
library(plyr)
library(zoo)

rm(list=ls())

source('SCRIPTS/funciones_cluster_temporal.R')

source('SCRIPTS/Cluster_DTW_embolse_FUN.R')

# Leer y alistar datos


embolse <- read.csv('DATOS/consolidado_embolse_removiendo_interv.csv')

embolse <- embolse[!is.na(embolse$Embolse),] 

embolse_lst <-
       split( embolse , paste(embolse$finca,embolse$anio,embolse$Lote,sep='_') )  


conteoFilas <- do.call(rbind,lapply(embolse_lst,dim))
summary(conteoFilas)

boxplot(conteoFilas[,1])

conteoFilas <- conteoFilas[order(conteoFilas[,1],decreasing = F),]

ct <- conteoFilas[conteoFilas[,1] > 51,]

emls_50_51 <- embolse_lst[row.names(ct)]

emls_50_51 <- lapply(emls_50_51,
                     function(w){
                        w[order(w$Semana),]
                       })

time_series <- lapply( emls_50_51,function(x){x['Embolse']} )

time_series <- lapply( time_series,function(x){
  data.frame(Embolse =  rollmean(x$Embolse ,4) )
} 
)

time_series <- lapply(time_series,ts)

plot(time_series[[1]],las=2)

# time_series <- time_series[1:30]

# lapply(time_series,function(x){plot(x,type='l')})

##----------------------------------------------------------------------------##
# -------------------Calcular matriz de distancia dtw---------------------------


distAllMatrix <- distDtwMV(time_series)

 save(distAllMatrix,file=here::here('DATOS','distAllMatrix_media_movil_sin_interve.RDATA'))

 #load(here::here('DATOS','distAllMatrix_media_movil_sin_interve.RDATA'))

# Cluster a (25 CLUSTER)

hClustEvents <- hirarCluster(distAllMatrix)

dfResults <- data.frame(Nam_time_series = names(time_series),
                        clust = hClustEvents)

spl_time_series <- split(time_series,hClustEvents)

dirSave  <- 'CLUSTER_25_SIN_INTER' 

if(!dir.exists(dirSave)){dir.create(dirSave)}

write.csv(dfResults,paste(dirSave,'/',dirSave,'.csv',sep=''),row.names = F)

tablResults <- ddply(dfResults,~clust,summarise,conteo = length(clust))

write.csv(tablResults,paste(dirSave,'/conteo_',dirSave,'.csv',sep=''),row.names = F)

graphics_cluster(ts= time_series,ts_per_cluster=spl_time_series,dirSave,limites_fijos=F)

#------------------------------------------------------------------------------
#--------------- Indetificar los centroides de las curvas----------------------
library(gtools)
# Agrupar cada curva en una matriz

groupsTable <-
  lapply(spl_time_series,
         function(w){
           
           ltx <- lapply(w,function(q){
             g <- as.numeric(q[,1])
             names(g) <- 1:length(q[,1])
             g
           })
           
           do.call(smartbind,ltx)
           
         }
  )



centroids <- lapply(groupsTable,function(w){
  ts(data.frame(Embolse = apply(w,2,median,na.rm=T)))}
)


names(spl_time_series)
names(centroids)

centroids_obs <- sapply( names(spl_time_series) ,function(w){
  names(which.min(sapply(spl_time_series[[w]],function(x){dtw(centroids[[w]],x)$distance})))
}
)

centerPatterns <- lapply(time_series[centroids_obs],function(w){data.frame(week=1:length(w),Embolse = w[,1])})

names(centerPatterns) <- names(centroids_obs)

namCenPatt <- names(centerPatterns)

patterCluster <-
  do.call(rbind,
          lapply(seq(length(centerPatterns)),
                 function(g){dfa <- data.frame(Group=as.character(namCenPatt[g]),centerPatterns[[g]])
                 dfa$Group  <- as.character(dfa$Group )
                 dfa$week <- as.numeric(dfa$week)
                 dfa$Embolse <- as.numeric(dfa$Embolse)
                 dfa
                 } 
          )
  )         

patterCluster$Group <- as.numeric(patterCluster$Group)

tablResults$clust

mg_DS <- merge(patterCluster,tablResults,by.x= 'Group',by.y='clust',all.x = T ,all.y =F,sort=F)

mg_DS$Group_Count <- paste('G',mg_DS$Group,'_C',mg_DS$conteo,sep='')


mg_DS_1  <- mg_DS[mg_DS$conteo > 6,]

g1 <- ggplot(mg_DS,aes(x=week,y=Embolse))+geom_point(aes(colour=factor(Group_Count)),size = 0.3)+
  geom_line(aes(colour=factor(Group_Count)),size = 0.3)+theme_bw()


g2 <- ggplot(mg_DS,aes(x=week,y=Embolse))+geom_point(aes(colour=factor(Group_Count)))+
  geom_line(aes(colour=factor(Group_Count)))+facet_wrap(~Group_Count,scales = 'free')+theme_bw()

g3 <- ggplot(mg_DS_1,aes(x=week,y=Embolse))+geom_point(aes(colour=factor(Group_Count)),size = 0.3)+
  geom_line(aes(colour=factor(Group_Count)),size = 0.3)+theme_bw()

ggsave('CLUSTER_25_SIN_INTER/ghrap_lin_sin_interve.png',g1)
ggsave('CLUSTER_25_SIN_INTER/ghra_wrap_sin_interve.png',g2)
ggsave('CLUSTER_25_SIN_INTER/ghrap_lin_mas_5_sin_interve.png',g3)

write.csv(mg_DS_1,'CLUSTER_25_SIN_INTER/datos_grafico_centro_cluster_sin_interve.csv')

write.csv(mg_DS_1,'CLUSTER_22_MA/datos_grafico_centro_cluster_sin_interve.csv')




