rm(list=ls())

library(factoextra)
library(gridExtra)

setwd("G:/Mi unidad/TEC/clases/2021/dataScience/clase8/kmeans/")

df <- read.csv("USArrests.csv", stringsAsFactors = FALSE, row.names = 1)

# Eliminar valores nulos
df <- na.omit(df)

# Escalar valores en misma magnitud
df <- scale(df)

# Vamos a calcular las distancias
dist_mat <- dist(df, method="euclidean")
head(dist_mat)
fviz_dist(dist_mat, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))



hclust_avg <- hclust(dist_mat, method="average")
plot(hclust_avg)

plot(hclust_avg)
rect.hclust(hclust_avg, k=5, border=2:6)

# Poda el árbol
hclust_def <- cutree(hclust_avg, k=5)
