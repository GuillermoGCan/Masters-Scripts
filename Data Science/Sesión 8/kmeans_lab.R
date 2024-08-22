rm(list=ls())

# tienes tu data frame con variables numericas
# Ya estableciste el valor de K
# Pseudo-code:
# 1.- Elige k observaciones al azar y éstas son tus centroides
# 2.- Calcula las distancias al cuadrado de cada observación a cada centroide, toma el promedio de las variables
# 3.- Asigna cada observación al centroide más cercano
# 4. Recalcula los centroides de cada grupo, esto genera unos centroides nuevos
# 5.- Calcula el punto 2,3,4, repite hasta que la suma de los cuadrados within ya no disminuya
# Nota: within sum of squares es la sumatoria para todos los grupos k, de la diferencia
# de cada x de cada observación respecto al promedio del grupo
# O bien, asigna un número máximo de iteraciones. 


setwd("C:/Users/alva_/OneDrive/Escritorio/TEC/MAESTR�A/ENE-MAR 21/Ciencia de datos")

library(cluster)
library(factoextra)

df <- read.csv("USArrests.csv", row.names=1)

#EJERCICIO CLUSTRERS HERARQUICOS############
####################

#Eliminar valores nulos
df <- na.omit(df)


#Escalar valores en misma longitud
df <- scale(df)


#Calcular distancias
dist_mat <- dist(df, method = "euclidean")

head(dist_mat)

hclust_avg <- hclust(dist_mat, method = "average")
plot(hclust_avg)


##Marco mis clusts
rect.hclust(hclust_avg, k=4, border = 2:6)

##########################

#EJERCICIO CLUSTERS K-MEANS
# Standarizar o ree-escalar mis valores:
df <- scale(df)
distance <- get_dist(df)
fviz_dist(distance, gradient=list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# kmeans cluster
k2 <- kmeans(df, centers=40, iter.max = 100, nstart=5)
k2

# Elbow Diagram
k.max = 40 # Numero máximo de clusters

res <- sapply(1:k.max, 
       function(k){kmeans(df, k, nstart=5, iter.max=100)$tot.withinss})

plot(1:k.max, res, 
     type="b", pch=19, frame=FALSE,
     xlab="k clusters",
     ylab="Total within-cluster sum of squares")
     abline(v=4, lty=2)

k4 <- kmeans(df, centers=4, iter.max = 100, nstart=5)
k5 <- kmeans(df, centers=5, iter.max = 100, nstart=5)

df2 <- df
df2 <- data.frame(unlist(k5$cluster))
df2 <- cbind(df, df2)
