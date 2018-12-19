
# Procesamiento datos numero de racimos por HA
# Hugo Andres Dorado B.
# 12/04/2018

library(readxl)
library(stringi)
library(reshape2)
library(plyr)
library(tidyr)
library(lubridate)

rm(list=ls())

##----------------------------

# poblacion

poblacion <- read_xlsx('DATOS/POBLACION/Poblacion estimada 2012-2016.xlsx')

poblacion <- as.data.frame(poblacion)

head(poblacion)

poblacion$FINCA <- stri_replace_all_fixed(poblacion$FINCA, " ", "")

poblacion <-poblacion[c('FINCA','AÑO','LOTE','PlXha')]

#------------------------------


# suelo

suelos <- read.csv('DATOS/SUELO/ConsolidadoSuelos.csv',na.strings = c("#N/A",NA))

suelos <- suelos[c('FINCA.x', 'Lotes.x','ANIO.x','PH','MATERIA.ORGANCICA','DSTERA','Al','B','Ca','CICE','Cu','Fe','K','Mg','Mn','P','S','Zn')]

suelos$FINCA.x <- as.character(suelos$FINCA.x )

suelos$FINCA.x <- stri_replace_all_fixed(suelos$FINCA.x, " ", "")



names(suelos)[1:3] <- c('FINCA','LOTE','AÑO')

suelos_smr <- ddply(suelos,~FINCA+ LOTE,summarise,pH=mean(PH,na.rm = T),MO=mean(MATERIA.ORGANCICA,na.rm = T),Al=mean(Al,na.rm = T),
                B=mean(B,na.rm = T),Ca=mean(Ca,na.rm = T),CICE=mean(CICE,na.rm = T),Cu=mean(Cu,na.rm = T),Fe=mean(Fe,na.rm = T),K=mean(K,na.rm = T),
                Mg=mean(Mg,na.rm = T),Mn=mean(Mn,na.rm = T),P=mean(P,na.rm = T),S=mean(S,na.rm = T),Zn=mean(Zn,na.rm = T))


suelos_DSTERA <- ddply(suelos,~ FINCA+LOTE+DSTERA,summarise,DSTERA_CONT = length(DSTERA))

suelos_DSTERA$DSTERA <- as.character(suelos_DSTERA$DSTERA)

suelos_DSTERA <- suelos_DSTERA[!is.na(suelos_DSTERA$DSTERA),]
      
sp_suelos_DSTERA <- split(suelos_DSTERA,paste(suelos_DSTERA$FINCA,suelos_DSTERA$LOTE,sep='_'))

suelo_DSTERA_SMR <- do.call(rbind,lapply(sp_suelos_DSTERA,function(x){x[which.max(x$DSTERA_CONT),]}))

suelo_DSTERA_SMR <- suelo_DSTERA_SMR[,-4]

suelo_comp <- merge(suelos_smr,suelo_DSTERA_SMR,by=c('FINCA','LOTE' ),all.x =T ,all.y = T,sort=F)

 #-----------------------------

# Intervenciones historicas

interve_historicas <- read_xlsx('DATOS/INTERVENCIONES/Intervenciones historicas 2012-2018.xlsx')

interve_historicas$FINCA <- as.character(interve_historicas$FINCA )

interve_historicas$FINCA <- stri_replace_all_fixed(interve_historicas$FINCA , " ", "")

interve_historicas <- as.data.frame(interve_historicas)

head(interve_historicas)

table(interve_historicas$COMENTARIOS)

#-----------------------------

# Labores

labores <- read_xlsx('DATOS/Labores Fertilizacion Nomina.xlsx')

labores$Cantidad_porc <- labores$Cantidad/labores$HasLote*100

labores$Cantidad_porc[labores$Cantidad_porc>100] <- 100 # Curar area de labores por HA

labores <- as.data.frame(labores)

labores$Finca <- stri_replace_all_fixed(labores$Finca, " ", "")

labores$Finca <- gsub("2","DOS",labores$Finca)

labores$Finca <- gsub('GOLETACUNAS','GOLETA',labores$Finca)

labores$Finca <- gsub('SOLEDADCUNAS','SOLEDAD',labores$Finca)

labores <- labores[!(labores$Labor %in% c('EMPAQUE 13 KILOS','LIMPIA MADREVIEJA O LINDERO')),]

labores$Cantidad_porc[labores$Labor %in% "FERTILIZACION FOLIAR( BOMBA MOTOR)"] <- NA 

# Queremos mantener el conteo de labor pero no, sobre area de fertilizacion foliar

labores$Labor_resumida <- 'Fert_edafica'

# Voy aquí993386

labores$Labor_resumida[ labores$Labor %in% c("FERTILIZACION (FOLIAR)",
                                             "PROYECTO FERTILIZACION FOLIAR (BOMBA MANUAL)",
                                             "FERTILIZACION FOLIAR( BOMBA MOTOR)",
                                             "FERTILIZACION MEZCLA",
                                             "FERTILIZACION LIQUIDA (FOLIAR)",
                                             "FERTILIZACION LIQUIDA (FOLIAR)",
                                             "FERTILIZAC.UREA LIQUIDA RENOVA  (FOLIAR)")] <- "Fert_foliar"


table(labores$Unidad [labores$Labor_resumida== "Fert_edafica"])

# table(labores$Labor,labores$Labor_resumida)

fertilizacion <-
ddply(labores,~Finca+Lote+Anio+Labor_resumida,summarise,
      conteo_labor=length(Labor_resumida),
      area_labor=mean(Cantidad_porc))

ferti_conteo <- spread(fertilizacion[,-6],Labor_resumida,conteo_labor)

names(ferti_conteo)[4:5] <- c('Cont_Fert_edafica','Cont_Fert_foliar')

ferti_area_porc  <- spread(fertilizacion[,-5],Labor_resumida,area_labor)

ferti_area_porc <- ferti_area_porc[,-5]

names(ferti_area_porc)[4] <- 'Porc_area_fol'

fert_merge <- merge(ferti_conteo,ferti_area_porc,by=c('Finca', 'Lote', 'Anio'),all=T,sort=F)

# Fertilizaciones no registradas son dejadas como NA

fert_merge$Cont_Fert_foliar[is.na(fert_merge$Cont_Fert_foliar)] <- 0

fert_merge$Cont_Fert_edafica[is.na(fert_merge$Cont_Fert_edafica)] <- 0

fert_merge$Porc_area_fol[is.na(fert_merge$Porc_area_fol)] <- 0

summary(fert_merge)

#--------------------------

embolse <- read.csv('DATOS/Embolse/Embolse_Cinta-semana30_arregladas_areas.csv',stringsAsFactors = F)

embolse$finca <- stri_replace_all_fixed(embolse$finca, " ", "")

embolse$finca <- gsub('2','DOS',embolse$finca)

embolse$finca <- gsub('2','DOS',embolse$finca)

embolse$finca <- gsub('GOLETACUNAS','GOLETA',embolse$finca)

embolse$finca <- gsub('SOLEDADCUNAS','SOLEDAD',embolse$finca)

embolse <- embolse[c('finca', 'anio','Semana', 'Lote', 'Rac.Hect')]

consolidado_embolse <- merge(embolse,interve_historicas,by.x = c('finca', 'anio', 'Semana', 'Lote'),
      by.y = c('FINCA', 'ANIO', 'LOTE', 'Semana'), all.x=T,all.y=F,sort=F)

head(consolidado_embolse,20)

data.frame(a=sort(unique(embolse$finca)),b=sort(unique(interve_historicas$FINCA)))


# SOLEDADCUNAS,GOLETACUNAS,CUNAS2

#-------------------------

# Procesamiento de practicas

suelos <- suelos[!is.na(suelos$AÑO),]



cosolidado0 <- merge(embolse,poblacion,by.x=c('finca','anio','Lote'),by.y=c( 'FINCA','AÑO','LOTE'),
                     all.x = T,all.y = F,sort = F)

consolidado1 <- merge(cosolidado0,suelo_comp,by.x=c('finca','Lote'),by.y = c('FINCA','LOTE'),
                     all.x = T,all.y = F,sort = F) # se une suelos compactados

interve_historicas$Semana <- as.numeric(interve_historicas$Semana)

interve_historicas2 <- interve_historicas[!is.na(interve_historicas$Semana),]

consolidado2 <- merge(consolidado1,interve_historicas2,by.x = c('finca','anio', 'Lote',  'Semana'),
                      by.y = c('FINCA', 'ANIO', 'LOTE', 'Semana'),all.x = T,all.y =F ,sort=F)

fert_merge <- fert_merge[!is.na(fert_merge$Lote),]

consolidado3 <- merge(consolidado2,fert_merge,by.x=c( 'finca','anio','Lote'),by.y=c('Finca','Anio','Lote'),
                      all.x = T,all.y = F)

consolidado3$PlXha <- as.numeric(consolidado3$PlXha)

summary(consolidado3)


consolidado3$Cont_Fert_edafica[is.na(consolidado3$Cont_Fert_edafica)] <- 0
consolidado3$Cont_Fert_foliar[is.na(consolidado3$Cont_Fert_foliar)] <- 0
consolidado3$Porc_area_fol[is.na(consolidado3$Porc_area_fol)] <- 0

consolidado3$Cont_Fert_edafica[consolidado3$finca == 'BACOTA' & consolidado3$anio <= 2014 |consolidado3$finca == 'GOLETA'& consolidado3$anio <= 2015 | consolidado3$finca == 'SOLEDAD' & consolidado3$anio <= 2015    ] <- NA

consolidado3$Cont_Fert_foliar[consolidado3$finca == 'BACOTA' & consolidado3$anio <= 2014 |consolidado3$finca == 'GOLETA'& consolidado3$anio <= 2015 | consolidado3$finca == 'SOLEDAD' & consolidado3$anio <= 2015  ] <- NA

consolidado3$Porc_area_fol[consolidado3$finca == 'BACOTA' & consolidado3$anio <= 2014 |consolidado3$finca == 'GOLETA'& consolidado3$anio <= 2016 | consolidado3$finca == 'SOLEDAD' & consolidado3$anio <= 2015 | consolidado3$finca == 'CATAMARAN' & consolidado3$anio == 2014 | consolidado3$finca == 'CUNAS' & consolidado3$anio == 2012  ] <- NA


write.csv(consolidado3,'DATOS/consolidado_todas_variables_Rac.Hect.csv',row.names = F)

consolidado_todas_variables<-consolidado3

#----------------------------------------------------------------------------------------------------------------------------
#------------------------ Tratar de colocar en NA las semanas que quedan despues de la practica como removidas --------------


spl_consolidado_embolse <- split(consolidado_embolse,paste(consolidado_embolse$finca,consolidado_embolse$Lote,sep='_'))


#event <- consolidado_embolse[3810,]

con_practicas <- consolidado_embolse[consolidado_embolse$COMENTARIOS %in% c('Renovación','Subsolado','Siembra nueva','Desbacote'),]


replaceAnio <- function(event,wks=156){ # 156 semanas son 3 anios


  anio <- event$anio

  semana <- event$Semana

  newDate <- as.Date(paste0(anio,'-',semana,'-',01),'%Y-%W-%w')

  endDate <-  newDate+wks*7

  toRemove <- paste(event$finca,event$Lote,
    unique(paste(year(as.Date(newDate:endDate,"1970-01-01")),
               week(as.Date(newDate:endDate,"1970-01-01")),sep='-')),sep='_')
  
  toRemove
  
}

to_replace_NA <- do.call(c,lapply(seq(nrow(con_practicas)),function(w){replaceAnio(con_practicas[w,])}))

consolidado_embolse <- data.frame(consolidado_embolse,ID=with(consolidado_embolse,paste0(finca,'_',Lote,'_',anio,'-',Semana)))

consolidado_embolse$ID <- as.character(consolidado_embolse$ID )

head(consolidado_embolse)

consolidado_embolse$Rac.Hect[consolidado_embolse$ID %in% to_replace_NA] <- NA

write.csv(consolidado_embolse,'DATOS/consolidado_embolse_removiendo_interv_Rac.Hect.csv') # base de datos para clustering

## ---------------------Procesamiento info clima-----------------------------

# consolidado_todas_variables <- read.csv('DATOS/consolidado_todas_variables_embolse.csv')

consolidado_todas_variables[3141,4] <- 52 # La semana 53 no existe en R

consolidado_todas_variables$Semana[ consolidado_todas_variables$anio == 2015 & consolidado_todas_variables$Semana==53] <- 52

consolidado_todas_variables$Date <- as.Date(paste(consolidado_todas_variables$anio,
                                   consolidado_todas_variables$Semana,'2',
                                   sep='-'),'%Y-%W-%w')


# Procesar bases de viento

nameStat <- substring(list.files('DATOS/CLIMA/SERIES_CLIMA/',pattern = 'txt'),1,
                      nchar(list.files('DATOS/CLIMA/SERIES_CLIMA/',
                                       pattern = 'txt'))-4)

stat_dat <-
lapply(list.files('DATOS/CLIMA/SERIES_CLIMA/',pattern = 'txt',full.names = T),
       read.table,header=T)

stat_dat <- lapply(stat_dat,function(x){x$Date <- as.Date(x$Date);x})

names(stat_dat) <- nameStat

asing_finca_pluv <- read.csv('DATOS/Asignacion Finca Pluviometro.csv',stringsAsFactors =F)

##Realizar aqui procesamiento largo de clima

##
  
station_end <- stat_dat[['AptoLosCedros']]

consolidado_todas_variables$Date <- as.Date(consolidado_todas_variables$Date )



colidad_fina_clima <-
  do.call(rbind,lapply(1:nrow(consolidado_todas_variables),function(w){
    cat(w,'\t')

    event <- consolidado_todas_variables[w,]
  
    date_embolse <- event$Date

    dates_embolse <- data.frame(Date = as.Date((date_embolse-173):date_embolse,origin = "1970-01-01"))

    mg_dataset_embolse  <- merge(dates_embolse,station_end,by='Date')

    clim <- with(mg_dataset_embolse,{data.frame(AccP = sum(P),PropP10 = sum(P<=10)/length(P),TMax_max=max(TX),
                                        TMax_min=min(TX),Tmax_mean=mean(TX),Tmin_max=max(TM),
                                        Tmin_min=min(TM),Tmin_mean=mean(TM),
                                        AccBs = sum(BS),BS_mean=mean(BS),RH_mean=mean(RH),RH_Sd = sd(RH))})


    cbind(event,clim)
  }
  )
)

write.csv(colidad_fina_clima,'DATOS/clima_semana_variables.csv')

consolid_year <- data.frame(Date = as.Date(paste(unique(consolidado_todas_variables$anio),1,1,sep='-')))


colidad_fina_clima_year <-
  do.call(rbind,lapply(1:nrow(consolid_year),function(w){
    cat(w,'\t')
    
    event <- consolid_year[w,]
    
    date_embolse <- event
    
    dates_embolse <- data.frame(Date = as.Date((date_embolse-173):(date_embolse+173),origin = "1970-01-01"))
    
    mg_dataset_embolse  <- merge(dates_embolse,station_end,by='Date')
    
    clim <- with(mg_dataset_embolse,{data.frame(AccP = sum(P),PropP10 = sum(P<=10)/length(P),TMax_max=max(TX),
                                                TMax_min=min(TX),Tmax_mean=mean(TX),Tmin_max=max(TM),
                                                Tmin_min=min(TM),Tmin_mean=mean(TM),
                                                AccBs = sum(BS),BS_mean=mean(BS),RH_mean=mean(RH),RH_Sd = sd(RH))})
    
    
    cbind(Date=event,clim)
  }
  )
  )

colidad_fina_clima_year <- colidad_fina_clima_year[complete.cases(colidad_fina_clima_year),]

write.csv(colidad_fina_clima_year,'DATOS/colidad_fina_clima_year.csv')

#------------------

consolidado_todas_variables_fert <- consolidado_todas_variables[c('finca','anio','Lote')]
  
uniqe_consold <- unique(consolidado_todas_variables_fert)

uniqe_consold$Date <- as.Date(paste0(uniqe_consold$anio,"-","1","-",'1'))

data.frame(uniqe_consold)

event <- uniqe_consold[120,]

practicas_anio <- do.call(rbind,lapply(seq(nrow(uniqe_consold)),function(w){
  cat(w,'\n')
event <- uniqe_consold[w,]

event_Date <- event$Date

date_days <- data.frame(Date = as.Date((event_Date-173):(event_Date+173),origin = "1970-01-01"))

date_week <- unique(data.frame(finca=event$finca,anio=year(date_days$Date),Lote=event$Lote,Semana=week(date_days$Date)))

date_week$finca <- as.character(date_week$finca)

data_week_mg  <-
merge(date_week,
      consolidado_todas_variables[c( 'finca', 'anio', 'Lote' ,'Semana' ,
                                     'Cont_Fert_edafica', 'Cont_Fert_foliar', 
                                     'Porc_area_fol')],by=c('finca', 'anio', 'Lote' ,'Semana'),all.x = T,all.y=F)


with(data_week_mg , data.frame(Cont_Fert_edafica_mean = mean(Cont_Fert_edafica,na.rm=T), 
      Cont_Fert_foliar_mean= mean(Cont_Fert_foliar,na.rm=T),Porc_area_fol_mean= mean(Porc_area_fol,na.rm=T)))

}
))

practicas_anual <- cbind(uniqe_consold,practicas_anio)

write.csv(practicas_anual,"DATOS/practicas_anual.csv")


