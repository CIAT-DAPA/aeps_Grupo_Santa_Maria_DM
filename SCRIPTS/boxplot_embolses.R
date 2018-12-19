library(plyr)
library(ggplot2)


embolse <- read.csv('DATOS/Embolse/Embolse_Cinta-semana30.csv')

embolse_lst <-
  split( embolse ,embolse$finca )  


namList <- names(embolse_lst)

# Crear una carpeta que se llama finca embolse

lapply(1:length(embolse_lst),function(x){
  
  g <- ggplot(embolse_lst[[x]],aes(x=factor(anio),y=Rac.Hect))+geom_boxplot()+
    facet_wrap(~finca,scales = 'free')
  
  ggsave(paste0('FINCA_EMBOLSE/',namList[x],'.png'),g,width = 6,height =3.5 )
  
})



promeEmbolse <- ddply(embolse,~finca+anio,summarise,promedioEmbolse=mean(Rac.Hect),
                      maxEmbolse=max(Rac.Hect),minEmbolse=min(Rac.Hect))



ggplot(promeEmbolse,aes(x=factor(anio),y=promedioEmbolse))+geom_boxplot()

ggplot(embolse,aes(x=factor(anio),y=Rac.Hect))+geom_boxplot()+facet_wrap(~finca,scales = 'free')


CUNAS2 <- subset(embolse, finca=='CUNAS 2')

ggplot(CUNAS2,aes(x=factor(anio),y=Rac.Hect))+geom_boxplot()+
  facet_wrap(~finca,scales = 'free')
