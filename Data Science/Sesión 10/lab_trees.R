rm(list=ls())

#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("tree")
#install.packages("MASS")
#install.packages("randomForest")

library("ISLR")
library("rpart")
library("rpart.plot")
library("tree")
library("MASS")
library("randomForest")

#setwd("G:/Mi unidad/TEC/clases/2021/dataScience/clase9/data/")


################################################################################
################################################################################
################################################################################

Carseats <- read.csv("Carseats.csv")

Carseats <- Carseats[complete.cases(Carseats),]
Carseats$High <- ifelse(Carseats$Sales <=8, "No", "Yes")
class(Carseats$High)

Carseats$High <- as.factor(Carseats$High)
Carseats$ShelveLoc <- as.factor(Carseats$ShelveLoc)
Carseats$Urban <- as.factor(Carseats$Urban)
Carseats$US <- as.factor(Carseats$US)
Carseats$High <- as.factor(Carseats$High)

tree.carseats = tree(High~.-Sales, Carseats)
summary(tree.carseats)

# Error de clasificación del 9%

plot(tree.carseats)
text(tree.carseats, pretty=0)

# El predictor más importante es shelve location

#Imprimir resultados como texto
tree.carseats

# Evaluar una muestra cruzada. 

sizeCars = nrow(Carseats)
train = sample(1:sizeCars, sizeCars/2)


Carseats.train <- Carseats[train,]
Carseats.test <- Carseats[-train,]

tree.carseats.train = tree(High~.-Sales, Carseats, subset=train)
summary(tree.carseats.train)

plot(tree.carseats.train)
text(tree.carseats.train, pretty=0)

# Usemos el modelo calibrado o ajustado con la base de entrenamiento, para predecir sobre
# la base de prueba. 
# Si mi modelo predice correctamente el mismo 91%, entonces no estoy haciendo overfitting. 

## Training y Testing
tree.predict.test <- predict(tree.carseats.train, Carseats.test, type="class")
summary(tree.predict.test)
table(tree.predict.test, Carseats$High[-train])

## Vamos a poder nuestro árbol 
cv.tree = cv.tree(tree.carseats.train, FUN=prune.misclass)
names(cv.tree)
cv.tree

par(mfrow=c(1,2))
plot(cv.tree$size,cv.tree$dev,type="b")
plot(cv.tree$k,cv.tree$dev,type="b")


## Prueba la poda con varios numero nodos y vuelve a utilizar tu base de pruebas
pruned9 <-  prune.misclass(tree.carseats.train, best=9)
plot(pruned9)
text(pruned9)

tree.predict.test <- predict(pruned9, Carseats.test, type="class")
summary(tree.predict.test)
table(tree.predict.test, Carseats.test$High)


################################################################################
boston <- read.csv("boston.csv")

## Crossvalidation
sizeBoston = nrow(Boston)
train = sample(1:sizeBoston, sizeBoston/2)
Boston[train,]

tree.boston <- tree(medv~., Boston, subset=train)
plot(tree.boston)
text(tree.boston, pretty=0)
summary(tree.boston)

yhat <- predict(tree.boston, Boston[-train,])
#sum(y-yhat) métrica para podar el árbol

## Calculo el error
yhat = predict(tree.boston, newdata=Boston[-train,])
boston.test=Boston[train]

# Bagging y Random Forest
# La misma función hace bagging y random forest.
# El bagging sería un caso especial del random forest, donde simplemente tu muestra es la misma.

#Bagging
bag.boston <- 
  randomForest(medv~., data=Boston, subset=train, ntree=50, sampsize=150, mtry=13) 
#para el bagging, se usan todas las variables (mtry = 13)

#Random Forest
bag.boston <- 
  randomForest(medv~., data=Boston, subset=train, ntree=50, sampsize=150, mtry=10)