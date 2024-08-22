rm(list=ls())
library(tidyverse)
library(dplyr)
library(readr)
library(rgdal)
library(ggplot2)
library(qgraph)
library(FactoMineR)
library(factoextra)
library(corrplot)

#setwd("Inserta aquí tu directorio de trabajo donde tengas los archivos descargados")


library(sf) # librería para cargar shapefiles

agebs <- read_sf(dsn=".", layer="19a", stringsAsFactors = FALSE)
head(agebs) ## Lee el archivo cartográfico

tabla <- read.csv("agebs_2021.csv", encoding ="UTF -8", na.strings = "*",
                  stringsAsFactors = FALSE) #Lee la tabla de resultados

## Genera un campo en comun con el shapefile para unir las dos bases de datos:

tabla$MUN <- ifelse(nchar(tabla$MUN)==1, paste("00", tabla$MUN, sep=""), 
                    ifelse(nchar(tabla$MUN)==2, paste("0", tabla$MUN, sep=""),
                           tabla$MUN))

tabla$LOC <- ifelse(nchar(tabla$LOC)==1, paste("000", tabla$LOC, sep=""), 
                    ifelse(nchar(tabla$LOC)==2, paste("00", tabla$LOC, sep=""),
                           ifelse(nchar(tabla$LOC)==3, paste("0", tabla$LOC, sep=""),
                           tabla$LOC)))

tabla$AGEB <- ifelse(nchar(tabla$AGEB)==1, paste("000", tabla$AGEB, sep=""), 
                    ifelse(nchar(tabla$AGEB)==2, paste("00", tabla$AGEB, sep=""),
                           ifelse(nchar(tabla$AGEB)==3, paste("0", tabla$AGEB, sep=""),
                                  tabla$AGEB)))
# Genera el campo CVEGEO
tabla$CVEGEO <- paste(tabla$ENTIDAD, tabla$MUN, tabla$LOC, tabla$AGEB, sep="")
head(tabla)


table(tabla$CVEGEO %in% agebs$CVEGEO)
table(agebs$CVEGEO %in% tabla$CVEGEO)

# Une los dos archivos. El objeto agebs_urbanas tiene la cartografia de agebs y todos los
# datos del censo del 2020.
agebs_urbanas <- merge(x=agebs, y=tabla, by="CVEGEO")
class(agebs_urbanas) # Revisa el tipo de objeto: sf es un objeto espacial con coordenadas

# Extrae la tabla a un data frame que no sea un objeto espacial
# Posiblemente tengas que instalar la librería dplyr.
df <- dplyr::select(as.data.frame(agebs_urbanas), -geometry)

# Creación y selección de variables

sel_data <- df %>% 
  filter(POBTOT != 0) %>%
  filter(NOM_MUN %in% c("Monterrey", "San Nicolás de los Garza", "San Pedro Garza García",
                        "Santa Catarina", "Guadalupe", "General Escobedo", "Apodaca",
                        "Juárez", "García", "Cadereyta Jiménez", "Pesquería", "El Carmen",
                        "General Zuazua", "Salinas Victoria", "Santiago")) %>% 
  mutate(PER_PSINDER = PSINDER/POBTOT, PER_POCUPADA = POCUPADA/POBTOT, PER_VPH_AUTOM = VPH_AUTOM/TVIVPARHAB,
         PER_VPH_INTER = VPH_INTER/TVIVPARHAB, PER_VPH_SPMVPI = VPH_SPMVPI/TVIVPARHAB, 
         PER_VPH_CVJ = VPH_CVJ/TVIVPARHAB, PER_VPH_CEL = VPH_CEL/TVIVPARHAB) %>% 
  select(CVEGEO,GRAPROES, PRO_OCUP_C,PER_PSINDER, PER_VPH_AUTOM,
         PER_VPH_INTER, PER_VPH_SPMVPI, PER_VPH_CVJ, PER_VPH_CEL) %>% 
  na.exclude()

# Matriz de correlaciones
cor(sel_data[2:ncol(sel_data)],)
qgraph(cor(sel_data[2:ncol(sel_data)],))

# Calcular el PCA
ir.pca <- prcomp(sel_data[2:ncol(sel_data)], center= TRUE, scale = TRUE)

print(ir.pca)
summary(ir.pca)
plot(ir.pca, type="l") #plot de explicación de la varianza por PCA

d <- as.data.frame(predict(ir.pca, sel_data[]))
head(d)
dim(d)

# Exploremos los resultados por componente
var <- get_pca_var(ir.pca)
corrplot(var$cos2, is.corr=FALSE)

# llevo mi índice a las agebs
sel_data$PC1 <- d$PC1

# Transformar a espacial
agebs_urbanas_2 <- merge(x=agebs, y=sel_data, by="CVEGEO")
agebs_urbanas_3 <- st_transform(agebs_urbanas_2, 4326)
class(agebs_urbanas_3)

mi.paleta <- colorNumeric( palette= "YlGnBu", domain=sel_data$PC1, 
                           na.color="transparent" )


