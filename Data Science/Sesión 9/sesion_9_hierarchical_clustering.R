rm(list=ls())
library(readr)

df <- read.csv("USArrests.csv", stringsAsFactors = FALSE, row.names = 1)

#Eliminar valores nulos
df <- na.omit(df)

#Escalar valores en misma magnitud
df <- scale(df)

#Vamos a calcular las distancias
dist_mat <- dist(df, method = "euclidean")
head(dist_mat)

hclust_avg <- hclust(dist_mat, method="average")
plot(hclust_avg)

plot(hclust_avg)
rect.hclust(hclust_avg, k=5, border = 2:6)

# Poda el árbol
hclust_