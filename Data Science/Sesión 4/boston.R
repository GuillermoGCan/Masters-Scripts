library(readr)
boston <- read_csv("C:/Users/Guillermo GC/Dropbox/TEC MTY MPE/2do Trimestre/Ciencia de datos/Sesión 4/boston.csv")


### Variables en la base de datos:
#crim: per capita crime rate by town.
# zn: proportion of residential land zoned for lots over 25,000 sq.ft.
# indus: proportion of non-retail business acres per town.
# chas; Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
# nox: nitrogen oxides concentration (parts per 10 million).
#rm: average number of rooms per dwelling.
# age: proportion of owner-occupied units built prior to 1940.
# dis: weighted mean of distances to five Boston employment centres.
# rad: index of accessibility to radial highways.
# tax: full-value property-tax rate per \$10,000.
# ptratio: pupil-teacher ratio by town.
# black: 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.
# lstat: lower status of the population (percent).
# medv: median value of owner-occupied homes in \$1000s.

# Descriptivos de las variables
summary(boston)
cor(boston)

# Distribución de la variable dependiente
plot(density(boston$medv))

#
pairs(~medv+crim+indus+tax, data = boston)

#
fit <- lm(medv ~ log(crim) + zn + indus + chas + nox + rm + age + dis + rad +
            tax + ptratio + black + lstat, data = boston)
summary(fit)

# Interpretación: Por cada incremento de una unidad en el índice de criminalidad,
# tenemos una reducción en el valor de la propiedad de 415 USD.

confint(fit)

predict(fit)

y <- boston$medv
y_hat <- predict(fit)

y <- data.frame(y, y_hat)
y$u2 <- (y-y_hat)^2

residuals(fit)
