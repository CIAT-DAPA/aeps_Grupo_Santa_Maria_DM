
# interpolacion por kriging in R
# Hugo Andres Dorado Betancourt
# 

# https://nceas.github.io/oss-lessons/spatial-data-gis-law/4-tues-spatial-analysis-in-r.html

rm(list=ls())

library(rgdal)
library(sp)
library(gstat)
library(dplyr) # for "glimpse"
library(ggplot2)
library(scales) # for "comma"
library(magrittr)
library(raster)
library(automap)
 
# Lectura de informacion

puntosMuestreo <- readOGR('DATOS/puntos_informacion.shp')

plot(puntosMuestreo)

poligono <- readOGR('DATOS/Corte_ejemplo.shp')

plot(poligono,add=T)

head(puntosMuestreo)

is(puntosMuestreo)

# Extraccion de variable

CE <- puntosMuestreo["CE"]

# Crear la grilla para llenar datos

cordinates <- CE@coords

# Resolucion en mts
resol  <- 30 #mts

grd <- expand.grid(x = seq(from = min(cordinates[,1]), to = max(cordinates[,1]),by = resol), 
                   y = seq(from = min(cordinates[,2]),to = max(cordinates[,2]), by = resol))  # 


# Asignar a la grilla 
class(grd)
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE


plot(CE,add =T )

crs(grd) <- crs(CE)

plot(grd, cex = 1.5, col = "grey") # mostrar grilla

# Realizar interpolacion IDW

idw_pow1 <- idw(formula = CE ~ 1,
                locations = CE,
                newdata = grd,
                idp = 1)


class(idw_pow1)

# Generar grafico

idw_pow1 <- raster(idw_pow1)

plot(idw_pow1)

idw_pow1 <- mask(idw_pow1,poligono)

plot(idw_pow1,
     col = terrain.colors(55))

# Convertir y salvar raster

writeRaster(idw_pow1,'Test_Interpolacion/rs_idw_pow1_dec.tif',"GTiff",overwrite=TRUE)

# Realizar interpolacion krigging

kriging_result = autoKrige(CE~1, CE, grd)

kriging_int <- raster(kriging_result$krige_output)

kriging_int <- mask(kriging_int,poligono)

plot(kriging_int)

writeRaster(kriging_int,'Test_Interpolacion/kriging_int.tif',"GTiff",overwrite=TRUE)

# Paso a paso

lzn.vgm <- variogram(CE~1, CE) # calculates sample variogram values 

lzn.fit <- fit.variogram(lzn.vgm, model=vgm(psill=max(lzn.vgm$gamma)*0.9,
                                            "Sph",range=1500, nugget = mean(lzn.vgm$gamma)/4) )#

plot(lzn.vgm, lzn.fit) 

lzn.kriged <- krige(CE ~ 1, CE, grd, model=lzn.fit)

lzn.kriged <- raster(lzn.kriged)

lzn.kriged <- mask(lzn.kriged,poligono)

plot(lzn.kriged)

