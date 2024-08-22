rm(list=ls())

#install.packages("jpeg")
#install.packages("gridExtra")
#install.packages("reshape2")

library(jpeg)
library(gridExtra)
library(reshape2)
library(ggplot2)

img <- readJPEG("egytp.jpg")

# Una imagen es un array de muchos números.
# Tenemos 3 badnas, rojo, verde y azul.
head(img)
nrow(img)

r <- img[,,1] #rojo
g <- img[,,2] #verde
b <- img[,,3] #azul

dim(img)
dim(r)
dim(g)
dim(b)

## Componentes principales para cada banda o color
cat.r.pca <- prcomp(r, center = FALSE)
cat.g.pca <- prcomp(g, center = FALSE)
cat.b.pca <- prcomp(b, center = FALSE)

names(cat.r.pca)

summary(cat.r.pca) # solo el primer componente principal me explica el 90.94% de la varianza
summary(cat.g.pca) # solo el primer componente principal me explica el 92.18% de la varianza
summary(cat.b.pca) # solo el primer componente principal me explica el 90.92% de la varianza


## Hago una lista que incluya mis tres objetos
rgb.pca <- list(cat.r.pca,cat.g.pca,cat.b.pca)

# Reconstruir con dos componentes principales
comp = 200
pca.img <- sapply(rgb.pca, function(j){
  compressed.img <- j$x [,1:comp] %*% t(j$rotation[,1:comp])
}, simplify="array")
writeJPEG(pca.img, "pc_200.jpg")
