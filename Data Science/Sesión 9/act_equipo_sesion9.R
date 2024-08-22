rm(list=ls())

library(readxl)
library(sf)
library(rgdal)
library(qgraph)
library(FactoMineR)
library(factoextra)
library(tidyverse)
library(dplyr)
library(corrplot)
library(cluster)

#### Componentes principales ####

df <- read.csv("delitosWide.csv", stringsAsFactors = FALSE, row.names = 1)

df_selec <- df %>% 
  select(desc_entidad,PARMAS, PUEC, PDELHOG, PDELUEC, TDELT, TINCID)

# Matriz de correlaciones
cor(df_selec[2:ncol(df_selec)],)
qgraph(cor(df_selec[2:ncol(df_selec)],))

# Calcular el PCA
ir.pca <- prcomp(df_selec[2:ncol(df_selec)], center= TRUE, scale = TRUE)

print(ir.pca)
summary(ir.pca)
plot(ir.pca, type="l")

d <- as.data.frame(predict(ir.pca, df_selec[]))
head(d)
dim(d)

# Exploremos los resultados por componente
var <- get_pca_var(ir.pca)
corrplot(var$cos2, is.corr=FALSE)

# llevo mi índice a los estados
df_selec$PC1 <- d$PC1
df_selec$PC2 <- d$PC2

#### Hierarchical Clustering ####

#Escalar valores en misma magnitud
df_selec2 <- df_selec %>% 
  select(PARMAS, PUEC, PDELHOG, PDELUEC, TDELT, TINCID) %>% 
  scale() %>% 
  as.data.frame()

row.names(df_selec2) <- df_selec$desc_entidad

#Vamos a calcular las distancias
dist_mat <- dist(df_selec2, method = "euclidean")
head(dist_mat)

hclust_avg <- hclust(dist_mat, method="average")
plot(hclust_avg)

plot(hclust_avg)
rect.hclust(hclust_avg, k=5, border = 2:6)

#podo el arbol
cut_avg <- cutree(hclust_avg, k = 5)

df_hclusters <- data.frame(cut_avg)

#### K Means ####

# Standarizar o ree-escalar mis valores:
distance <- get_dist(df_selec2)
fviz_dist(distance, gradient=list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# kmeans cluster
k2 <- kmeans(df_selec2, centers=5, iter.max = 100, nstart=1)
k2

# Elbow Diagram
k.max = 20 # Numero máximo de clusters

res <- sapply(1:k.max, 
              function(k){kmeans(df_selec2, k, nstart=1, iter.max=100)$tot.withinss})

plot(1:k.max, 
     res, 
     type="b", 
     pch=19, 
     frame=FALSE,
     xlab="k clusters",
     ylab="Total within-cluster sum of squares")

abline(v=5, lty=2)

k5 <- kmeans(df_selec2, centers=5, iter.max = 100, nstart=1)

df2 <- df_selec2
df_kmeans <- data.frame(unlist(k5$cluster))
df2 <- cbind(df_selec2, df2)

df_final <- df_kmeans
df_final$df_hclusters <- df_hclusters$cut_avg
