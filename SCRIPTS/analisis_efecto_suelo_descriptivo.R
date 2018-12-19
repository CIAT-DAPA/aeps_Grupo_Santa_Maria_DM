
# Analisis independientes para comprender tendencia entre variables
# Hugo Andres Dorado B
# 12 - 12 -2018


library(readxl)
library(stringi)
library(reshape2)
library(plyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(gridExtra)

# Analisis de embolses 

rm(list=ls())

embolse <- read.csv('DATOS/Embolse/Embolse_Cinta-semana30_arregladas_areas.csv',
                    stringsAsFactors = F)

embolse$finca <- stri_replace_all_fixed(embolse$finca, " ", "")

embolse$finca <- gsub('2','DOS',embolse$finca)

embolse$finca <- gsub('2','DOS',embolse$finca)

embolse$finca <- gsub('GOLETACUNAS','GOLETA',embolse$finca)

embolse$finca <- gsub('SOLEDADCUNAS','SOLEDAD',embolse$finca)

embolse <- embolse[c('finca', 'anio','Semana', 'Lote', 'Rac.Hect')]


# Intervenciones historicas

interve_historicas <- read_xlsx('DATOS/INTERVENCIONES/Intervenciones historicas 2012-2018.xlsx')

interve_historicas$FINCA <- as.character(interve_historicas$FINCA )

interve_historicas$FINCA <- stri_replace_all_fixed(interve_historicas$FINCA , " ", "")

interve_historicas <- as.data.frame(interve_historicas)

head(interve_historicas)

table(interve_historicas$COMENTARIOS)

interve_historicas$Semana <- as.numeric(interve_historicas$Semana)

interve_historicas <- interve_historicas[!is.na(interve_historicas$Semana),] 

table(interve_historicas$COMENTARIOS)

summary(embolse);summary(interve_historicas)

consolidado_embolse <- merge(embolse,interve_historicas,by.x = c('finca', 
                                                                 'anio', 
                                                                 'Lote',
                                                                 'Semana'),
                             by.y = c('FINCA', 'ANIO', 'LOTE','Semana' ),
                             all.x=T,all.y=F,sort=F)


# Analisis de suelos


suelos <- read.csv('DATOS/SUELO/ConsolidadoSuelos.csv',
                   na.strings = c("#N/A",NA))

suelos <- suelos[c('FINCA.x', 'Lotes.x','ANIO.x','PH','MATERIA.ORGANCICA',
                   'DSTERA','Al','B','Ca','CICE','Cu','Fe','K','Mg','Mn','P',
                   'S','Zn')]

suelos$FINCA.x <- as.character(suelos$FINCA.x )

suelos$FINCA.x <- stri_replace_all_fixed(suelos$FINCA.x, " ", "")

names(suelos)[1:3] <- c('FINCA','LOTE','AÑO')

head(suelos)

unique(suelos$DSTERA)


consolidado_embolse <- merge(embolse,interve_historicas,by.x=c('finca','anio','Lote','Semana'),
                             by.y =c('FINCA','ANIO','LOTE','Semana') ,all.x = T,all.y = F)

# no existen muchos los lotes que fueron intervenidos despues de la intersección
# con inverenciones historicas

table(consolidado_embolse$COMENTARIOS)
table(interve_historicas$COMENTARIOS)

table(consolidado_embolse$anio)
table(interve_historicas$ANIO)

# --------------------------Poblacion estimada-------------------------------


poblacion <- read_xlsx('DATOS/POBLACION/Poblacion estimada 2012-2016.xlsx')

poblacion <- as.data.frame(poblacion)

head(poblacion)

poblacion$FINCA <- stri_replace_all_fixed(poblacion$FINCA, " ", "")

poblacion$PlXha <- as.numeric(poblacion$PlXha)

poblacion <-poblacion[c('FINCA','AÑO','LOTE','PlXha')]


#------------------------ Tratar de colocar en NA las semanas que quedan despues de la practica como removidas --------------


spl_consolidado_embolse <- split(consolidado_embolse,
                                 paste(consolidado_embolse$finca,
                                       consolidado_embolse$Lote,sep='_'))


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

summary(consolidado_embolse)

suelos <- suelos[!is.na(suelos$AÑO),]

# Merges


MediaRacHec <- ddply(consolidado_embolse,~finca + anio+ Lote,summarise,
      MediaRacHec = mean(Rac.Hect,na.rm = T))

consolidado_embolse_poblacion <- merge(MediaRacHec,
                                       poblacion,by.x=c('finca','anio','Lote'),
                                       by.y = c('FINCA','AÑO','LOTE'),all.x = T,
                                       all.y = F)

consolidado_embolse <- merge(consolidado_embolse,suelos,
                             by.x = c('finca', 'Lote',  'anio') ,
                             by.y=c('FINCA', 'LOTE',  'AÑO'),
                             all.x=T,all.y = F)

write.csv(consolidado_embolse,'DESCRIPTIVOS_PASO_PASO/consolidado_embolse_suelos.csv',row.names = F)

# Consolidado final

consolidado_embolse <- consolidado_embolse[,-6]

consolidado_embolse

head(consolidado_embolse)

consolidado_embolse <- consolidado_embolse[complete.cases(consolidado_embolse),]

summary(consolidado_embolse)

####---------------------Analisis exploratorio----------------------------#####

g1 <- ggplot(consolidado_embolse,aes(x= Rac.Hect ))+geom_histogram(binwidth = 5)+
  theme_bw()

g2 <- ggplot(consolidado_embolse,aes(x='Racim X HA',y= Rac.Hect ))+geom_boxplot()+
  theme_bw()

consolidado_embolse <- consolidado_embolse[consolidado_embolse$Rac.Hect < 300,]

racim <- grid.arrange(g1,g2,nrow=1,ncol=2)

ggsave('GRAFICOS/racimos.ha1.png',racim,width = 6 ,height =2.5 )

#racimos 2

g1 <- ggplot(consolidado_embolse,aes(x='Racim X HA',y= Rac.Hect ))+geom_boxplot()+
  theme_bw()

barfill <- "#4271AE"
barlines <- "#1F3552"

g2 <- ggplot(consolidado_embolse,aes(x= Rac.Hect ))+
  geom_histogram(colour = barlines, fill = barfill)+theme_bw()

racim <- grid.arrange(g1,g2,nrow=1,ncol=2)

ggsave('GRAFICOS/racimos.ha2.png',racim,width = 6 ,height =2.5 )

# pH

g1 <- ggplot(consolidado_embolse,aes(x=PH,y=Rac.Hect))+geom_point()+theme_bw()+geom_rug(sides="b")

g2 <- ggplot(consolidado_embolse,aes(y=PH))+geom_boxplot()+theme_bw()

pH <- grid.arrange(g1,g2,nrow=1,ncol=2)

ggsave('GRAFICOS/pH.png',pH,width = 6 ,height =3.5 )


# Materia Organica

g1 <- ggplot(consolidado_embolse,aes(x=MATERIA.ORGANCICA,y=Rac.Hect))+geom_point()+
  theme_bw()

g2 <- ggplot(consolidado_embolse,aes(y=MATERIA.ORGANCICA))+geom_boxplot()+theme_bw()

MO <- grid.arrange(g1,g2,nrow=1,ncol=2)

ggsave('GRAFICOS/MO.png',MO,width = 6 ,height =3.5 )

# Al ---------------------------------

g1 <- ggplot(consolidado_embolse,aes(x= Al,y=Rac.Hect))+geom_point()+
  theme_bw()

g2 <- ggplot(consolidado_embolse,aes(y= Al))+geom_boxplot()+theme_bw()

Al <- grid.arrange(g1,g2,nrow=1,ncol=2)

ggsave('GRAFICOS/Al.png',Al,width = 6  ,height =2.5 )

# B

g1 <- ggplot(consolidado_embolse,aes(x=B,y=Rac.Hect))+geom_point()+
  theme_bw()

g2 <- ggplot(consolidado_embolse,aes(y=B))+geom_boxplot()+theme_bw()

B <- grid.arrange(g1,g2,nrow=1,ncol=2)

ggsave('GRAFICOS/B.png',B,width = 6  ,height =2.5 )

# Ca

g1 <- ggplot(consolidado_embolse,aes(x=Ca,y=Rac.Hect))+geom_point()+
  theme_bw()

g2 <- ggplot(consolidado_embolse,aes(y=Ca))+geom_boxplot()+theme_bw()

Ca <- grid.arrange(g1,g2,nrow=1,ncol=2)

ggsave('GRAFICOS/Ca.png',Ca,width = 6  ,height =2.5 )

# CICE

g1 <- ggplot(consolidado_embolse,aes(x=CICE,y=Rac.Hect))+geom_point()+
  theme_bw()

g2 <- ggplot(consolidado_embolse,aes(y=CICE))+geom_boxplot()+theme_bw()

CICE <- grid.arrange(g1,g2,nrow=1,ncol=2)

ggsave('GRAFICOS/CICE.png',CICE,width = 6  ,height =2.5 )

# Cu

g1 <- ggplot(consolidado_embolse,aes(x=Cu,y=Rac.Hect))+geom_point()+
  theme_bw()

g2 <- ggplot(consolidado_embolse,aes(y=Cu))+geom_boxplot()+theme_bw()

Cu <- grid.arrange(g1,g2,nrow=1,ncol=2)

ggsave('GRAFICOS/Cu.png',Cu,width = 6  ,height =2.5 )

# Fe

g1 <- ggplot(consolidado_embolse,aes(x=Fe,y=Rac.Hect))+geom_point()+
  theme_bw()

g2 <- ggplot(consolidado_embolse,aes(y=Fe))+geom_boxplot()+theme_bw()

Fe <- grid.arrange(g1,g2,nrow=1,ncol=2)

ggsave('GRAFICOS/Fe.png',Fe,width = 6  ,height =2.5 )

# K

g1 <- ggplot(consolidado_embolse,aes(x=K,y=Rac.Hect))+geom_point()+
  theme_bw()

g2 <- ggplot(consolidado_embolse,aes(y=K))+geom_boxplot()+theme_bw()

K <- grid.arrange(g1,g2,nrow=1,ncol=2)

ggsave('GRAFICOS/K.png',K,width = 6  ,height =2.5 )

# Mg

g1 <- ggplot(consolidado_embolse,aes(x=Mg,y=Rac.Hect))+geom_point()+
  theme_bw()

g2 <- ggplot(consolidado_embolse,aes(y=Mg))+geom_boxplot()+theme_bw()

Mg <- grid.arrange(g1,g2,nrow=1,ncol=2)

ggsave('GRAFICOS/Mg.png',Mg,width = 6  ,height =2.5 )

# Mn

g1 <- ggplot(consolidado_embolse,aes(x=Mn,y=Rac.Hect))+geom_point()+
  theme_bw()

g2 <- ggplot(consolidado_embolse,aes(y=Mn))+geom_boxplot()+theme_bw()

Mn <- grid.arrange(g1,g2,nrow=1,ncol=2)

ggsave('GRAFICOS/Mn.png',Mn,width = 6  ,height =2.5 )

# P

g1 <- ggplot(consolidado_embolse,aes(x=P,y=Rac.Hect))+geom_point()+
  theme_bw()

g2 <- ggplot(consolidado_embolse,aes(y=P))+geom_boxplot()+theme_bw()

P <- grid.arrange(g1,g2,nrow=1,ncol=2)

ggsave('GRAFICOS/P.png',P,width = 6  ,height =2.5 )

# S

g1 <- ggplot(consolidado_embolse,aes(x=S,y=Rac.Hect))+geom_point()+
  theme_bw()

g2 <- ggplot(consolidado_embolse,aes(y=S))+geom_boxplot()+theme_bw()

S <- grid.arrange(g1,g2,nrow=1,ncol=2)

ggsave('GRAFICOS/S.png',S,width = 6  ,height =2.5 )

# Zn

g1 <- ggplot(consolidado_embolse,aes(x=Zn,y=Rac.Hect))+geom_point()+
  theme_bw()

g2 <- ggplot(consolidado_embolse,aes(y=Zn))+geom_boxplot()+theme_bw()

Zn <- grid.arrange(g1,g2,nrow=1,ncol=2)

ggsave('GRAFICOS/Zn.png',Zn,width = 6  ,height =2.5 )


################################################################################

consolidado_embolse_poblacion <- consolidado_embolse_poblacion[complete.cases(consolidado_embolse_poblacion),]


ge_pob <- ggplot(consolidado_embolse_poblacion,aes(x= PlXha,y= MediaRacHec))+geom_point(aes(colour=factor(anio)))+
geom_smooth(aes(colour=factor(anio)))+facet_grid(anio~.,scales = 'free')

ggsave('GRAFICOS/poblacion.png',ge_pob,width = 5  ,height =7 )


################################################################################

# Clima

clima_Datos <- read.csv('DATOS/clima_semana_variables.csv',row.names = 1)

clima_final <-
clima_Datos[c('finca', 'anio', 'Lote', 'Semana', 'Date', 'Rac.Hect','AccP',
              'PropP10', 'TMax_max', 'TMax_min', 'Tmax_mean', 
              'Tmin_max', 'Tmin_min', 'Tmin_mean', 'AccBs',  'BS_mean','RH_mean',
              'RH_Sd')]


clima_final <- clima_final[complete.cases(clima_final),]

nam <- names(clima_final)[7:18]

var <- nam[1]

for(var in nam){
  
  g1 <- ggplot(clima_final,aes(x=get(var),y=Rac.Hect))+geom_point()+geom_smooth()+theme_bw()

  g2 <- ggplot(clima_final,aes(x=var,y=get(var)))+geom_boxplot()+theme_bw()

  gr <- grid.arrange(g1,g2,nrow=1,ncol=2)

  ggsave(paste0('GRAFICOS/',var,'.png'),gr,width = 6  ,height =2.5)
}


###############################################################################
####               Analisis de intervenciones historicas              #####


interv_filtradas <- interve_historicas[interve_historicas$COMENTARIOS %in% c('Renovación','Subsolado','Siembra nueva','Desbacote'),]

embolse_intervenidos <- embolse[ paste(embolse$finca,embolse$Lote,sep='_') %in%  paste(interv_filtradas$FINCA,interv_filtradas$LOTE,sep='_'),]

embolse_final  <-
  merge(embolse_intervenidos,interv_filtradas,
      by.y = c('FINCA','ANIO','LOTE','Semana'),
      by.x = c('finca', 'anio', 'Lote','Semana') ,
     all.x = T,all.y = T,sort=F)

head(embolse_final)


embolse_tot <- with(embolse_final,{data.frame(embolse_final,
                               Fecha=as.Date(paste0(anio,'-',Semana,'-',01),
                                             '%Y-%W-%w'))})



spl_embolse <- split(embolse_tot,paste(embolse_tot$finca,  embolse_tot$Lote,sep='-'))



nam_spl_emb <- names(spl_embolse)

lapply(nam_spl_emb,function(nam){
  cat(nam)
  plt_exmp <- spl_embolse[[nam]]
  
  dates <- plt_exmp[!is.na(plt_exmp$COMENTARIOS),]$Fecha

  gg <-
    ggplot(plt_exmp,aes(x=Fecha,y=Rac.Hect))+geom_point(aes(colour=COMENTARIOS))+ geom_line(position = 'jitter')+
    geom_vline(xintercept = dates,linetype ='dashed')+geom_smooth()+theme_bw()
  
  ggsave(paste0('PATRONES_INTERVENCION/',nam,'.png'),gg,width = 7  ,height =2.5)
  }
)







