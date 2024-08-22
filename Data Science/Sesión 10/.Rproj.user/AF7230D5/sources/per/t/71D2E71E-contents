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
pruned9 <-  prune.misclass(tree.carseats.train, best=8)
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
#sum(y-yhat)

## Calculo el error
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)


## Calcular el error con un OLS
ols.boston <- lm(medv~., data=Boston, subset=train)
summary(ols.boston)
# Probemos este modelo en el test set.
yhat.ols = predict(ols.boston,newdata=Boston[-train,])
plot(yhat.ols, boston.test)
abline(0,1)
mean((yhat.ols-boston.test)^2)


# Random Forest 
# La misma función hace bagging y randomo forest. 
# El bagging sería un caso especial del randomo forest, 

# Bagging 
bag.boston <- randomForest(medv~., data=Boston, subset=train, ntree=50, sampsize=150, mtry=13)

# Probemos este modelo en el test set.
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)


# Random Forest
rf.boston <- randomForest(medv~., data=Boston, subset=train, ntree=1000, sampsize=180, mtry=11)

yhat.rf = predict(rf.boston,newdata=Boston[-train,])
plot(yhat.rf, boston.test)
abline(0,1)
mean((yhat.rf-boston.test)^2)


# Veamos el error en el test set. 
importance(rf.boston)
varImpPlot(rf.boston)


