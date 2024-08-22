rm(list=ls())

install.packages("pacman")
install.packages("rvest")

#library("pacman")
library("rvest")

# Dar la direccion que quiero descargar:
url = "https://en.wikipedia.org/wiki/List_of_hospitals_in_Washington_(state)" 
xPath =  '//*[@id="mw-content-text"]/div/table[1]'

# Bajar HTML
mi.pagina <- read_html(x=url)

# identifica la ubicación de la tabla
mis.nodos <- html_nodes(x=mi.pagina, xpath=xPath)

# Descargar la tabla:
mi.tabla <- html_table(x=mis.nodos)
mi.tabla = as.data.frame(mi.tabla[[1]])
