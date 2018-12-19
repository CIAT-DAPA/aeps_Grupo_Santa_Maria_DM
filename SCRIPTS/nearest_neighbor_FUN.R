distancia <- function(x,y,...){
  dist(rbind(x,y),...)
}

distance_matrix2  <- function(base,nuevo,...){
  
  matDistance <-
    do.call(rbind,
            lapply(1:nrow(nuevo), function(u){
              vec <- sapply(1:nrow(base),function(m){distancia(nuevo[u,],base[m,],...)})
              names(vec) <- row.names(base)
              vec
            }
            )
    )
  
  row.names(matDistance) <- row.names(nuevo)
  
  matDistance
}


extract_neigthbors <- function(base_org,nuev_org,distMat_hist_org,k=5){
  
  todos_ds <- rbind(base_org,nuev_org)
  
  todos_ds$nam_lote <-row.names(todos_ds)
  
  names_nuev <- row.names(distMat_hist_org)
  
  do.call(rbind,
          lapply(1:length(names_nuev),function(i){
            
            s <- sort(distMat[i,])
            
            distancias <- s[1:k]
            
            lotes_cercanos <- names(distancias)
            
            result <- data.frame(lote_evaluado=names_nuev[i],distancias,ord_cercania = 1:k,lotes_cercanos)
            
            result <- rbind(data.frame(lote_evaluado=names_nuev[i],
                                       distancias=0,ord_cercania = 0,
                                       lotes_cercanos=names_nuev[i]),result)
            
            mg <- merge(result,todos_ds,by.x = 'lotes_cercanos' ,by.y = 'nam_lote',
                  all.x=T,all.y = F,sort=F)
            
            mg[c(2:1,3:ncol(mg))]
          }
          )
  )
}