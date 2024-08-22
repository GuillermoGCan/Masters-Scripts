rm(list=ls())

install.packages("mlogit")

library(mlogit)

#### Base de datos de trenes ####

train <- read.csv("train.csv", stringsAsFactors = FALSE)

# Base de datos de preferencias establecidas, se otorgan un par de alternativas en 10-15 ocasiones
# al tomador de decisión. Son viajes hipotéticos. Las alternativas tienen 3 atributos: 
# a) precio: en centavos de guilders
# b) tiempo: en minutos
# c) comfort: 0 business, 1 primera, 2 turista

# Vamos a convertir a euros, 1 guilder = 2.20371 euros. 

Tr <- mlogit.data(train, shape="wide", choice = "choice", varying = 4:11,
                  sep = "", id="id") 
#shape = wide se refiere a que la base de datos crece hacia la derecha,
  #no hacia abajo, conforme crecen las alternativas.
#choice establece la variable donde está la decisión.
#varying las columnas que varían van a partir de la 4.
#sep el separador (no se acuerda para qué es
#id es el id del tomador de decisión 

class(Tr)
head(Tr)

#mlogit hace la base de datos wide a long automáticamente

#conversión de unidades: minutos a horas y guilders a euros
Tr$price <- Tr$price/100*2.20371
Tr$time <- Tr$time/60

#Logit
ml <- mlogit(choice ~ price + time + change + comfort | -1, Tr) 

summary(ml)
# el signo del estimador nos dice si el aumento en una unidad en la variable 
#nos aumenta + o reduce - la utilidad

#Tenemos el atributo precio de las alternativas
#Tenemos los coeficientes

coef(ml)

coef(ml)[-1] #quito el coeficiente de precio (todos menos el primero)
coef(ml)[1] #coeficiente de precio (el primero)

# Si saco la proporción de las demás variables con respecto al precio, lo puedo
# medir en términos de disposición a pagar.
coef(ml)[-1]/coef(ml)[1]

# Están dispuestos a pagar 25.54 euros por una hora menos de viaje.
# Están dispuestos a pagar 4.8 euros por una conexión de cambio menos.
# Están dispuestos a pagar 14 euros por una categoría de mayor comfort.

# Modelamiento de la Demanda. 

#### Modelar demanda de pesca ####

fish <- read.csv("fishing.csv")
# boat es bote privado, mayor catch
# charter es bote compartido
# pier es en el muelle
# beach es en la playa, menor catch
# Income, característica del tomador de decisión

# Convertir al formato de mlogit:
d <- mlogit.data(fish, varying = c(2:9), shape="wide", choice = "mode")
head(d)

# Sin constantes específicas:
simple_model <- mlogit(mode ~ price + catch | 0, d)
summary(simple_model)

#Interpretación del log-likelihood (función de verosimilitud)
# Punto de referencia para comparar modelos. Haces una prueba de log likelihood
#Sacas diferencias entre los log-likelihood y ves la diferencia entre los grados de libertad
# más variables en relación con 

# Comparemos nuestro modelo contra la predicción simple
t <- data.frame(table(fish$mode))
colnames(t) <- c("mode", "Obs")
t$pct_observed <- t$Obs/nrow(fish)

t2 <- data.frame(apply(fitted(simple_model, outcome=FALSE), 2, mean))
colnames(t2) <- "pct_predicted"
t2$mode <- row.names(t2)

## Explicar la importancia de tener una constante específica a las alternativas.
const_esp <- mlogit(mode ~ price + catch, d, reflevel = "boat")
summary(const_esp)
# Se interpreta el signo con respecto a la alternativa de referencia.
# Interpretación de los signos de los estimadores: 
# "Independientemente del precio y del catch, la alternativa boat es más preferida a beach".

t3 <- data.frame(apply(fitted(const_esp, outcome=FALSE), 2, mean))
colnames(t3) <- "pct_predicted"
t3$mode <- row.names(t3)

# Agreguemos ingreso
caract_ingreso <- mlogit(mode ~ price + catch | income, d, reflevel = "boat")
summary(caract_ingreso)

# Interpretación de los signos:
# El ingreso de beach comparado con boat me genera menos utilidad sensibilidad en esa alternativa
# respecto a las otras.

## Cuando empiezo a comparar modelos utilizo el log likelihood para elegir el mejor
test <- lrtest(const_esp, caract_ingreso)
print(test)

# Agreguemos catch como una variable específica a las alternativas 
catch_var <- mlogit(mode ~ price | income | catch, d, reflevel = "boat")
summary(catch_var)

# Hay que diferenciar entre características del tomador de decisión
# y atributos de la alternativa (lo segundo es lo más importante que se busca, porque es lo manipulable)

test <- lrtest(caract_ingreso, catch_var)
print(test)

# ¿Cómo se reconfigura mi modelo si aumenta precio o catch de una demanda?

fittedv <- fitted(catch_var, outcome = FALSE)

# Ver demanda agregada
apply(fittedv, 2, mean)

# Utilicemos el modelo ajustado para simular cambios en demanda

X <- model.matrix(catch_var)
# Model matrix te saca del modelo todas las variables que se utilizaron
alt <- index(d)$alt
chid <- index(d)$chid
eXb <- as.numeric(exp(X %*% coef(catch_var)))
SeXb <- tapply(eXb, chid, sum)
P <- eXb/SeXb[chid]

# ordenar la matriz de probabilidades
P <- matrix(P, ncol = 4, byrow=TRUE)
head(P)
head(fittedv)
apply(P, 2, mean)

### Ahora vamos a lo más interesante. ¿Qué pasa si cambiamos el precio de pier?

# Estima elasticidades de demanda a cambios en el precio de pier:
newX <- function(d,X,catch_var, constante){
  t1 <- replicate(nrow(X), 1)
  for(i in 1:length(t1)){
    if (i %% 4 == 0){
      t1[i] <- constante
    }
  }
  X[,4] <- X[,4]*t1
  alt <- index(d)$alt
  chid <- index(d)$chid
  eXb <- as.numeric(exp(X %*% coef(catch_var)))
  SeXb <- tapply(eXb, chid, sum)
  P <- eXb/SeXb[chid]
  P <- matrix(P, ncol = 4, byrow = TRUE)
  head(P)
  return(apply(P,2,mean))
}

X <- model.matrix(catch_var)
newX(d,X,catch_var,1.20)

#### Covid-19 ####

#install.packages("readxl")
#install.packages("sqldf")
#install.packages("pscl")

library(readxl)
library(plyr)
library(dplyr)
library(stringr)
library(sqldf)
library(pscl)

d <- read.csv("200524COVID19MEXICO.csv", stringsAsFactors = FALSE)

d2 <- d[d$RESULTADO==1,] #Filtro el resultado = 1, osea que salieron positivos a la prueba
dim(d2)

d2$DEFUNCION <- ifelse(d2$FECHA_DEF!="9999-99-99",1,0)

d2$INTUBADO2 <- ifelse(d2$INTUBADO == 1,1,0)
d2$CARDIOVASCULAR <- ifelse(d2$CARDIOVASCULAR == 1,1,0)
d2$NEUMONIA <- ifelse(d2$NEUMONIA == 1, 1, 0)
d2$HIPERTENSION <- ifelse(d2$HIPERTENSION == 1, 1, 0)
d2$DIABETES <- ifelse(d2$DIABETES == 1, 1, 0)
d2$ASMA <- ifelse(d2$ASMA == 1, 1, 0)

table(d2$INTUBADO2, d2$DEFUNCION)

# Generalized Linear Model (GLM)

fit <- glm(DEFUNCION ~ INTUBADO2, data = d2, family=binomial(logit))
summary(fit)

#La magnitud de los coeficientes no tiene una interpretación directa,
#se tienen que convertir a probabilidades. 

pR2(fit) 
# Estimador McFadden me da una especie de R2

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds/(1+odds)
  return(prob)
}

# Log_deceso = intercept + b_intubado*clase(valor)
coef(fit)

intercept <- coef(fit)[1]
b_intubado <- coef(fit)[2]
x_intubado = 1

logit_deceso <- intercept + x_intubado*b_intubado

logit2prob(logit_deceso)

# Se puede sacar automáticamente con la función predict.
# Predice la probabilidad (response) de deceso cuando el paciente está intubado (INTUBADO2=1)
predict(fit,data.frame(INTUBADO2=1), type="response")

# Agregar más variables
fit2 <- glm(DEFUNCION ~ INTUBADO2 + ASMA + CARDIOVASCULAR + NEUMONIA + HIPERTENSION
            + DIABETES, data = d2, family=binomial(logit))
summary(fit2)

pR2(fit2) # a partir de 0.4 es bastante bueno

#Utilizar nuestro modelo como un elemento de predicción.
p <- predict(fit2, type="response")
summary(p)
plot(density(p))

# ¿Cómo fijar mi threshold a partir del cuál defino si hay o no hay defunciones?
p2 <- ifelse(p>0.5,1,0)
table(p2)

## AUC (Area Under the Curve) ROC (Receiver Operating Characteristic)

#install.packages("ROCR")
library(ROCR)

pr <- prediction(p,d2$DEFUNCION)
summary(pr)
slotNames(pr)
pr@cutoffs

pr@predictions
pr@labels

prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
abline(a=0, b=1)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc # va de 0 a 1. 0.5 no hay relación, cercano a 1 es mucha dependencia.

acc.perf = performance(pr, measure="acc")
plot(acc.perf)

ind = which.max(slot(acc.perf, "y.values")[[1]])
acc = slot(acc.perf, "y.values")[[1]][ind]
cutoff = slot(acc.perf, "x.values")[[1]][ind]
print(c(accuracy=acc, cutoff=cutoff))

# Doy mi threshold...
p2 <- ifelse(p>0.5443165,1,0)
table(p2)

# Para prevenir overfitting, hacemos cross validation.

al_tr <- sample(1:nrow(d2), nrow(d2)/2)

train <- d2[al_tr,]
test <- d2[!row.names(d2) %in% al_tr,]

