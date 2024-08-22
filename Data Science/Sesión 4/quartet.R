rm(list=ls())

library(readr)
d <- read_csv("C:/Users/Guillermo GC/Dropbox/TEC MTY MPE/2do Trimestre/Ciencia de datos/Sesión 4/quartet.csv")

plot(d$x1, d$y1) #Se plotean los puntos en el scatter plot.
abline(lm(d$y1 ~ d$x1)) #Se dibuja la línea que mejor se ajusta.
ols1 <- lm(y1 ~ x1, data = d) #lm estima los coeficientes de B0 y B1. 
summary(ols1) #resumen de los estimadores


plot(d$x2, d$y2)
abline(lm(d$y2 ~ d$x2))
ols2 <- lm(y2 ~ x2, data = d) #lm estima los coeficientes de B0 y B1. 
summary(ols2)

plot(d$x3, d$y3)
abline(lm(d$y3 ~ d$x3))
ols3 <- lm(y3 ~ x3, data = d) #lm estima los coeficientes de B0 y B1. 
summary(ols3)

plot(d$x4, d$y4)
abline(lm(d$y4 ~ d$x4))
ols4 <- lm(y4 ~ x4, data = d) #lm estima los coeficientes de B0 y B1. 
summary(ols4)

# Corrlación de Pearson entre las dos variables.

cor(d$x1, d$y1) 
cor(d$x2, d$y2)
cor(d$x3, d$y3)
cor(d$x4, d$y4)

ols1 <- lm(y1 ~ x1, data = d) #lm estima los coeficientes de B0 y B1. 
summary(ols1)


