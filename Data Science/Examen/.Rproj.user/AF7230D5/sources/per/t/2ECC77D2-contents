#### Examen Final ####

#install.packages("readxl")
#install.packages("sqldf")
#install.packages("pscl")

# Depuración y Logit
library(readxl)
library(plyr)
library(dplyr)
library(stringr)
library(sqldf)
library(pscl)
library(ROCR)

# Random Forest
library(ISLR)
library(rpart)
library(rpart.plot)
library(tree)
library(MASS)
library(randomForest)

d0 <- read.csv("200524COVID19MEXICO.csv", stringsAsFactors = FALSE)
d0 <- d0[complete.cases(d0),]

#### Problema 1 ####

# Variables seleccionadas
## FECHA_DEF (DEFUNCION), SEXO, EDAD, HABLA_LENGUA_INDIG, UCI, CARDIOVASCULAR, 
## EPOC, OBESIDAD, DIABETES, TABAQUISMO, NEUMONIA.

d1 <- d0 %>% 
  na.exclude() %>% 
  filter(RESULTADO == 1) #Filtro el resultado = 1, osea que salieron positivos a la prueba
dim(d1)

d1$DEFUNCION <- ifelse(d1$FECHA_DEF!="9999-99-99",1,0)
d1$UCI <- ifelse(d1$UCI == 1,1,0)
d1$CARDIOVASCULAR <- ifelse(d1$CARDIOVASCULAR == 1,1,0)
d1$EPOC <- ifelse(d1$EPOC == 1, 1, 0)
d1$OBESIDAD <- ifelse(d1$OBESIDAD == 1, 1, 0)
d1$DIABETES <- ifelse(d1$DIABETES == 1, 1, 0)
d1$TABAQUISMO <- ifelse(d1$TABAQUISMO == 1, 1, 0)
d1$HABLA_LENGUA_INDIG <- ifelse(d1$HABLA_LENGUA_INDIG == 1, 1, 0)
d1$NEUMONIA <- ifelse(d1$NEUMONIA == 1, 1, 0)

d1_final <- d1 %>% 
  select(DEFUNCION, SEXO, EDAD, HABLA_LENGUA_INDIG, UCI, CARDIOVASCULAR, EPOC,
         OBESIDAD, DIABETES, TABAQUISMO, NEUMONIA) %>% 
  filter(SEXO != 99, 
         NEUMONIA != 97, NEUMONIA != 98, NEUMONIA != 99,
         HABLA_LENGUA_INDIG != 97, HABLA_LENGUA_INDIG != 98, HABLA_LENGUA_INDIG != 99,
         DIABETES != 97, DIABETES != 98, DIABETES != 99,
         EPOC != 97, EPOC != 98, EPOC != 99,
         CARDIOVASCULAR != 97, CARDIOVASCULAR != 98, CARDIOVASCULAR != 99,
         OBESIDAD != 97, OBESIDAD != 98, OBESIDAD != 99,
         TABAQUISMO != 97, TABAQUISMO != 98, TABAQUISMO != 99,
         UCI != 97, UCI != 98, UCI != 99)

#### Problema 2 ####

# Genero un vector con números aleatorios del tamaño de la mitad de mi base de datos
train_set <- sample(1 : nrow(d1_final), nrow(d1_final)/2)

d2_train <- d1_final[train_set,] # Base de entrenamiento
d2_test <- d1_final[-train_set,] # Base de prueba

#### Problema 3 ####
model_3 <- glm(DEFUNCION ~ UCI + CARDIOVASCULAR + EPOC + OBESIDAD + NEUMONIA +
                 DIABETES + TABAQUISMO + HABLA_LENGUA_INDIG + SEXO + EDAD, 
               data = d2_train, family=binomial(logit))
summary(model_3)

## Estimador de McFadden
pR2(model_3) # Da un resultado de 0.2727

## Utilizar nuestro modelo como un elemento de predicción.
p_3 <- predict(model_3, type="response")
summary(p_3)
plot(density(p_3))

#### Problema 4 ####

pr_4 <- prediction(p_3,d2_train$DEFUNCION)
summary(pr_4)
slotNames(pr_4)
pr_4@cutoffs

pr_4@predictions
pr_4@labels

prf_4 <- performance(pr_4, measure = "tpr", x.measure = "fpr")
plot(prf_4)
abline(a=0, b=1)

auc_4 <- performance(pr_4, measure = "auc")
auc_4 <- auc_4@y.values[[1]]
auc_4 # va de 0 a 1. 0.5 no hay relación, cercano a 1 es mucha dependencia.

acc.perf_4 = performance(pr_4, measure="acc")
plot(acc.perf_4)

ind = which.max(slot(acc.perf_4, "y.values")[[1]])
acc = slot(acc.perf_4, "y.values")[[1]][ind]
cutoff = slot(acc.perf_4, "x.values")[[1]][ind]
print(c(accuracy=acc, cutoff=cutoff))

## Doy mi threshold. El valor resultante fue de 0.6166516.
p2_3 <- ifelse(p_3>0.6166516,1,0)
table(p2_3)

#### Problema 5 ####

## Determino la probabilidad de deceso para cada observación en la base de prueba
logit.predict.test <- predict(model_3, d2_test, type = "response")
summary(logit.predict.test)
plot(density(logit.predict.test))

## Junto en una nueva base de datos comparativa mi predicción sobre la defunción de los pacientes 
## de la base de prueba y los datos reales de las defunciones de mi base de prueba
dfcomparative_5 <- data.frame(logit.predict.test, 
                       "Predicción de defunciones" = ifelse(logit.predict.test>0.6166516, 1, 0), 
                       "Defunciones" = d2_test$DEFUNCION)

## Hago una variable nueva en la base de datos para evaluar si las predicciones fueron reales o falsas.
dfcomparative_5$Eval_prediccion <- ifelse(dfcomparative_5$Predicción.de.defunciones == 
                                     dfcomparative_5$Defunciones,1,0)

porc.logit_5 <- sum(dfcomparative_5$Eval_prediccion)/nrow(dfcomparative_5)
porc.logit_5 

## El modelo predice correctamente el 89.58% de las observaciones de la base de prueba.

#### Problema 6 ####

## Hago nuevos objetos de las mismas bases de datos de entrenamiento y prueba
## para trabajar con árboles.
d6_train <- d2_train
d6_test <- d2_test

## Convierto mi variable dependiente en factor para obtener árboles de clasificación
d6_train$DEFUNCION <- ifelse(d6_train$DEFUNCION == 1, "Si", "No")
d6_train$DEFUNCION <- as.factor(d6_train$DEFUNCION)

d6_test$DEFUNCION <- ifelse(d6_test$DEFUNCION == 1, "Si", "No")
d6_test$DEFUNCION <- as.factor(d6_test$DEFUNCION)

## Bagging

bag.covid_6 <- randomForest(DEFUNCION~., 
                           data = d6_train,  
                           ntree = 500, 
                           mtry = 10, 
                           sampsize = 22988)
summary(bag.covid_6)
### ntree: número de árboles. Utilicé 500.
### mtry : número de variables a considerar. Utilicé las 10 variables disponibles para considerar bagging.
### sampsize: tamaño de la muestra que toma. Utilicé el 67% de las observaciones por muestra aleatoria.

## Random Forest

rf.covid_6 <- randomForest(DEFUNCION~., 
                           data = d6_train,  
                           ntree = 500, 
                           mtry = 7, 
                           sampsize = 22988)
summary(rf.covid_6)
### ntree: número de árboles. Utilicé 500.
### mtry : número de variables a considerar. Utilicé 7 de las 10 variables disponibles para Random Forest.
### sampsize: tamaño de la muestra que toma. Utilicé el 67% de las observaciones por muestra aleatoria.

#### Problema 7 ####

# Importancia de las variables
## Bagging
importance(bag.covid_6)
varImpPlot(bag.covid_6)

## Random Forest
importance(rf.covid_6)
varImpPlot(rf.covid_6)

# Las variable más importante para predecir si un paciente fallecerá, de acuerdo con 
# el bagging y el random forest, es su edad.

#### Problema 8 ####

#Uso el modelo para predecir sobre la base de prueba

## Bagging

bag.covid.test <- predict(bag.covid_6, 
                          d6_test, 
                          type = "class")
summary(bag.covid.test)
table(bag.covid.test, d6_test$DEFUNCION)

porc.bag_8 <- ((table(bag.covid.test, d6_test$DEFUNCION)[1,1] + 
                table(bag.covid.test, d6_test$DEFUNCION)[2,2])/
                nrow(d6_test))
porc.bag_8 # Bagging me genera un 88.82% de clasificaciones correctas.                

## Random Forest
rf.covid.test <- predict(rf.covid_6, 
                          d6_test, 
                          type = "class")
summary(rf.covid.test)
table(rf.covid.test, d6_test$DEFUNCION)

porc.rf_8 <- ((table(rf.covid.test, d6_test$DEFUNCION)[1,1] + 
                  table(rf.covid.test, d6_test$DEFUNCION)[2,2])/
                 nrow(d6_test))
porc.rf_8 # Random Forest me genera un 89.15% de clasificaciones correctas.

#### Problema 9 ####

# Logit me genera un 89.58% de clasificaciones correctas.
porc.logit_5
# Bagging me genera un 88.82% de clasificaciones correctas.
porc.bag_8
# Random Forest me genera un 89.15% de clasificaciones correctas.
porc.rf_8

# Las mejores predicciones las genera el modelo de logit.

#### Problema 10 ####

# Hago una nueva base de datos de prueba ajustando todas las edades a 70
# y dándole neumonía a todas las observaciones
d10 <- d2_test %>% 
  mutate(NEUMONIA = 1,
         EDAD = 70)

# Genero mi predicción
prediccion_10 <- predict(model_3, d10, type= "response")
summary(prediccion_10)
plot(density(prediccion_10))

# En promedio, un paciente de 70 años con neumonía tiene 37.98% de probabilidad de fallecer.
