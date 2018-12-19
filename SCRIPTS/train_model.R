#Ejercicion Redes Neuronales en R
#Hugo Andres Dorado B.
#30 Mayo 2017


#install.packages("caret")
#install.packages("nnet")

rm(list=ls())

library(randomForest)

library(caret)

datos_mora <- read.csv("DATOS/matriz_lista_regresion.csv",row.names = 1)

#Particion

set.seed(123)

inTrain  <- createDataPartition(y=datos_mora$clust, p=0.7, list=F)

training <- datos_mora[inTrain,]

testing  <- datos_mora[-inTrain,]


#Entrenamiento

set.seed(123)

model <- train(y=training$clust,x=training[,-1],method = "rf",
                trControl = trainControl(method = "cv",number = 5),importance=T)


model

#Desempeño del modelo

pred_val <- predict(model,testing)

postResample(pred_val,testing$clust)

confusionMatrix(pred_val,testing$clust)

#Relevancia de variables

varImp <- varImp(model)

plot(varImp)



datos_mora_cuant <- datos_mora[,sapply(datos_mora,is.numeric)]

datos_mora_cuant$Cluster <- datos_mora$clust

namVars <- names(datos_mora_cuant)

for(i in 1:(length(namVars)-1))
{
  nm <- namVars[i]

png(paste0("Descriptivos/",nm,".png"),width = 680  ,height = 470 )
boxplot(datos_mora_cuant[,i]~datos_mora_cuant$Cluster,main=nm,ylab=nm)
dev.off()
}

barplot(table(datos_mora$DSTERA,datos_mora$clust),beside = TRUE,legend=unique(datos_mora$DSTERA),ylab = 'Frecuencia')
box()
