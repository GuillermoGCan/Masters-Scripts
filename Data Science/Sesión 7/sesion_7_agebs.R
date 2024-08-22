rm(list=ls())

install.packages("qgraph")
install.packages("FactoMineR")
install.packages("FactoExtra")

library(sf) # más común para hacer análisis espacial en R
library(rgdal)
library(ggplot2)
library(qgraph)
library(FactoMineR)
library(factoextra)
library(corrplot)

agebs <- readOGR(dsn=".",layer = "agebs_zm",stringsAsFactors = FALSE)
plot(agebs)
table(agebs$GRADO_MARG)

# observemos los datos
ggplot(agebs@data, aes(x=VIV33_R, y=VIV36_R)) + 
  geom_point() + 
  ggtitle("% Computadoras vs. % Internet por AGEB/MTY, Censo INEGI 2010")

# se usa el @data al momento de trabajar con archivos espaciales para trabajar
# exclusivamente con los datos, no con la geometría.

cor(agebs@data$VIV33_R, y= agebs@data$VIV36_R)

vars <- c("CVEGEO", "GRADO_MARG", "ECO22_R", "ECO25_R", "VIV5_R", "VIV9_R",
          "VIV27_R", "VIV28_R", "VIV32_R", "VIV33_R", "VIV35_R", "VIV36_R")

agebs2 <- agebs[agebs@data$VIV27_R >= 0 & agebs@data$ECO22_R >= 0 & agebs@data$VIV32_R >= 0, ]
sel_data <- agebs2[, vars]
sel_data@data[sel_data@data < 0] <- NA
sel_data <- sel_data[complete.cases(sel_data@data),]

## Matriz de correlaciones
cor(sel_data@data[3:ncol(sel_data)],) #selecciona de columna 3 en delante porque 1 es clave del AGEB
                                      # y 2 es el grado de marginación (cualitativo)

qgraph(cor(sel_data@data[3:ncol(sel_data)],)) #despliega plot de asociación en variables de base de datos

# Calcular el PCA
ir.pca <- prcomp(sel_data@data[3:ncol(sel_data)], center= TRUE, scale = TRUE)
# scale asegura que todas las vars estén en la misma escala, que sean todas comparables

print(ir.pca)
summary(ir.pca)
plot(ir.pca, type="l") #plot de explicación de la varianza por PCA

d <- as.data.frame(predict(ir.pca, sel_data@data[]))
head(d)
dim(d)

# Exploremos los resultados por componente
var <- get_pca_var(ir.pca)
corrplot(var$cos2, is.corr=FALSE)

# llevo mi índice a las agebs
sel_data$PC1 <- d$PC1
