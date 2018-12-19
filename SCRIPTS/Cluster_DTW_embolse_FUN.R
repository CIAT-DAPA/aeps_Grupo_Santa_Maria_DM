

# Calcular maximos y minimos de toda la serie
graphics_cluster <- function(ts,ts_per_cluster,dirSave,limites_fijos){
   
  max_min <- do.call(rbind,
                     lapply(ts,function(x){
                       data.frame(mx=max(x),mn=min(x),len = length(x))}
                       )
                     )


  maxim <- max(max_min$mx)
  minm  <- min(max_min$mn)  
  leng  <- max(max_min$len)


  namSplTS <- names(ts_per_cluster)

  grap <- function(w){

    splt <- ts_per_cluster[[w]]
  
    max <- max(do.call(c,splt))
    min <- min(do.call(c,splt))
  
    nam <- namSplTS[w]
    png(paste0(dirSave,'/',paste('Group',nam),'.png'),height =390 ,width =635 )
    if(!limites_fijos){
      plot(1,col=0,ylim=c(min,max),xlim=c(1,leng),main=paste('Group',nam),ylab='embolse')
    #,ylim=c(minm,maxim),
    }else{plot(1,col=0,ylim=c(minm,maxim),xlim=c(1,leng),main=paste('Group',nam),ylab='embolse')}
      lapply(splt,function(x){lines(x)})
  
    dev.off()
  }

  lapply(seq(length(spl_time_series)),grap)
}
