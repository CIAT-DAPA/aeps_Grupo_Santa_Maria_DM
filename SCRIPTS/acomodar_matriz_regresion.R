
# Alistar base de datos para modelar

rm(list=ls())

clusterSeleccionados <- read.csv('CLUSTER_25_SIN_INTER/conteo_CLUSTER_25_SIN_INTER.csv')

clasificacion <- read.csv('CLUSTER_25_SIN_INTER/CLUSTER_25_SIN_INTER.csv')

select_clust <- clusterSeleccionados[clusterSeleccionados$conteo>20,]

clasificacion_filtrada <- clasificacion[clasificacion$clust %in% select_clust$clust,]

table(clasificacion_filtrada$clust)

consolidado_todas_variables <- read.csv('DATOS/consolidado_todas_variables_embolse.csv')

densidad_plantas <- ddply(consolidado_todas_variables[,1:6],~ finca + anio + Lote,summarise,PlXha = mean(PlXha))

suelos_consolidado <- read.csv('DATOS/suelos_consolidados.csv',row.names = 1)

practicas <- read.csv('DATOS/practicas_anual.csv',row.names = 1)


mg_dens_suelos <- merge(densidad_plantas,suelos_consolidado,by.x=c('finca' , 'Lote'),by.y=c('FINCA' ,'LOTE'),sort=F)

consolidado_inputs <- merge(mg_dens_suelos,practicas,by.x=c('finca', 'Lote', 'anio'),by.y =c('finca', 'Lote', 'anio'))

consolidado_inputs$Nam_time_series <- paste(consolidado_inputs$finca,consolidado_inputs$anio,consolidado_inputs$Lote,sep="_")

base_final <- merge(clasificacion_filtrada,consolidado_inputs,by='Nam_time_series',all.x=T,all.y=F)

cosolidado_clima_anio <- read.csv('DATOS/colidad_fina_clima_year.csv',row.names = 1)

cosolidado_clima_anio$anio <- year(as.Date(cosolidado_clima_anio$Date))

base_final <- merge(base_final,cosolidado_clima_anio,by = 'anio')

base_final <- base_final[,-c(1,4:5,22,26) ]

base_final$clust <- paste0('C',base_final$clust)

base_final <- base_final[complete.cases(base_final),]

summary(base_final)

table(base_final$clust)

write.csv(base_final,'DATOS/matriz_lista_regresion.csv',row.names=F)

