rm(list=ls())

library(sf)
library(rgdal)
library(qgraph)
library(corrplot)
library(ggplot2)
library(factoextra)

setwd("G:/Mi unidad/TEC/clases/2021/dataScience/clase7/")

agebs <- readOGR(dsn="./shapefiles", layer="agebs_zm", stringsAsFactors = FALSE)
plot(agebs)
table(agebs$GRADO_MARG)

# observemos los datos
ggplot(agebs@data, aes(x=VIV33_R, y=VIV36_R)) +
  geom_point() + ggtitle("% Computadoras vs. % Internet por AGEB/MTY, Censo INEGI 2010")

cor(agebs@data$VIV33_R, y=agebs@data$VIV36_R)
cor(agebs@data$VIV28_R, agebs@data$VIV36_R)

vars <- c("CVEGEO","GRADO_MARG","ECO22_R", "ECO25_R", "VIV5_R", "VIV9_R", 
          "VIV27_R", "VIV28_R", "VIV32_R", "VIV33_R", "VIV35_R", "VIV36_R")

agebs2 <- agebs[agebs$VIV27_R>=0 & agebs@data$ECO22_R>=0 & agebs@data$VIV32_R>=0,]
sel_data <- agebs2[, vars]
sel_data@data[sel_data@data < 0] <- NA

sel_data <- sel_data[complete.cases(sel_data@data),]

## Matriz de correlaciones
cor(sel_data@data[3:ncol(sel_data)],)
qgraph(cor(sel_data@data[3:ncol(sel_data)],))


# Calcular el PCA
ir.pca <- prcomp(sel_data@data[3:ncol(sel_data)], center=TRUE, scale=TRUE)
print(ir.pca)

summary(ir.pca)
plot(ir.pca, type="l")

d <- as.data.frame(predict(ir.pca, sel_data@data[3:ncol(sel_data)]))
head(d)
dim(d)

sel_data$PC1 <- d$PC1
# Exploremos los resultados por componente
var <- get_pca_var(ir.pca)
corrplot(var$cos2, is.corr=FALSE)

# llevo mi indice a las agebs
library("cartography")
choroLayer(spdf=sel_data, df=sel_data@data, var="PC1")

library(leaflet)
mi.paleta <- colorQuantile( palette= "magma", domain=sel_data@data$PC1, 
                           na.color="transparent" )

# un mapa sencillo
mapa_lf <- leaflet(sel_data) %>% 
  addTiles() %>% 
  setView(lat=25.6707731, lng=-100.309228, zoom=12) %>% 
  addPolygons( 
               stroke=TRUE,
               weight=0.3,
               fillOpacity=0.60, 
               fillColor = ~colorQuantile("YlGnBu", PC1)(PC1)
               ) %>% 
  addProviderTiles("Stamen.Toner", options=providerTileOptions(opacity=1))
mapa_lf


# un mapa con tooltips
etiquetasMTY <- paste( 
                "AGEB: ", sel_data@data$CVEGEO, "<br/>",
                "Indicador Ingreso: ", round(sel_data@data$PC1,2),  
                sep="") %>% 
  lapply(htmltools::HTML) # Hay que instalar la librería de htmltools


mapa_lf2 <- leaflet(sel_data) %>% 
  addTiles() %>% 
  setView(lat=25.6707731, lng=-100.309228, zoom=12) %>% 
  addPolygons( 
    stroke=TRUE,
    weight=0.3,
    fillOpacity=0.65, 
    label=etiquetasMTY,
    labelOptions = labelOptions(
      style=list("font-weight"="normal", padding="4px px"),
      textsize="11px",
      direction="auto"
    ),
    fillColor = ~mi.paleta(PC1)
  ) %>% 
  addProviderTiles("Stamen.Toner", options=providerTileOptions(opacity=1)) %>% 
  addLegend( 
    pal=mi.paleta, values=~PC1, opacity=0.65, 
    title="Indicador de Ingreso", 
    position="bottomleft",
    labFormat = function(type, cuts, p){
      paste0(c("Bajo", "Medio", "Alto", "Muy Alto"))
    }
    )
mapa_lf2

# Instalar y cargar dos librerías
library("htmlwidgets")
htmlwidgets::saveWidget(mapa_lf2, file="mapaClaseCD.html")
