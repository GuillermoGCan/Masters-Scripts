rm(list=ls())

setwd("G:/Mi unidad/TEC/clases/2021/dataScience/Clase4")

d <- read.csv("quartet.csv")

plot(d$x1, d$y1)
abline(lm(d$y1 ~ d$x1))

plot(d$x2, d$y2)
plot(d$x3, d$y3)
plot(d$x4, d$y4)

ols1 <- lm(y1 ~ x1, data=d)
ols2 <- lm(y2 ~ x2, data=d)
ols3 <- lm(y3 ~ x3, data=d)
ols4 <- lm(y4 ~ x4, data=d)

summary(ols1)
summary(ols2)
summary(ols3)
summary(ols4)

plot(d$x1, d$y1)
abline(lm(d$y1 ~ d$x1))

ols1 <- lm(y3 ~ x3, data=d)
summary(ols3)

plot(d$x4, d$y4)
abline(lm(d$y4 ~ d$x4))


ols2 <- lm(y2 ~ x2, data=d)
summary(ols2)
plot(d$x2, d$y2)
abline(lm(d$y2 ~ d$x2))
