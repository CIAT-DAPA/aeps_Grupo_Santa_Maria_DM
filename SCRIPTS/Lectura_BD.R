
library(xlsx)
library(gdata)
library(tidyr)

#-------------------------------------------------------------------------------
# Lectura y procesamiento de datos de molienda

molienda_2006 <- read.xlsx('DATOS/Molienda PP 2006-2012.xls',sheetName = '2006',rowIndex =  5:36,colIndex = 2:26)

molienda_2006 <- molienda_2006[,seq(1,25,by=2)] # Este caso es especial quitamos las columnas que dicen semanas

molienda_2006 <- gather(molienda_2006,Mes,Molienda,-DIA) # Restructuramos la matriz en un formato lóngitudinal

molienda_2006 <- data.frame(Anio=2006,molienda_2006) # Agregamos una columna con el año

molienda_2007 <- read.xlsx('DATOS/Molienda PP 2006-2012.xls',sheetName = '2007',rowIndex =  5:36,colIndex = 1:13)

molienda_2007 <- gather(molienda_2007,Mes,Molienda,-DIA)

molienda_2007 <- data.frame(Anio=2007,molienda_2007)

molienda_2008 <- read.xlsx('DATOS/Molienda PP 2006-2012.xls',sheetName = '2008',rowIndex =  5:36,colIndex = 1:13)

molienda_2008 <- gather(molienda_2008,Mes,Molienda,-DIA)

molienda_2008 <- data.frame(Anio=2008,molienda_2008)

molienda_2009 <- read.xlsx('DATOS/Molienda PP 2006-2012.xls',sheetName = '2009',rowIndex =  5:36,colIndex = 1:13)

molienda_2009 <- gather(molienda_2009,Mes,Molienda,-DIA)

molienda_2009 <- data.frame(Anio=2009,molienda_2009)

molienda_2010 <- read.xlsx('DATOS/Molienda PP 2006-2012.xls',sheetName = 'PRE2010',rowIndex =  5:36,colIndex = 1:13)

molienda_2010 <- gather(molienda_2010,Mes,Molienda,-DIA)

molienda_2010 <- data.frame(Anio=2010,molienda_2010)

molienda_2011 <- read.xlsx('DATOS/Molienda PP 2006-2012.xls',sheetName = '2011',rowIndex =  11:42,colIndex = 1:13)

molienda_2011 <- gather(molienda_2011,Mes,Molienda,-DIA)

molienda_2011 <- data.frame(Anio=2011,molienda_2011)

# Unificacion final

data_Molienda <- rbind(molienda_2006,molienda_2007,molienda_2008,molienda_2009,molienda_2010,molienda_2011)

#-------------------------------------------------------------------------------
# Lectura utilizando el paquete xlsx

read.xlsx('DATOS/Embolse_Cinta-semana30.xlsx',sheetName = 'Datos')

embolse_cintaS30 <- read.csv('DATOS/Embolse_Cinta-semana30.csv')

#-------------------------------------------------------------------------------





