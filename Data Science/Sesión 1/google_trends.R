rm(list=ls())

# Instalar librerías

# install.packages("gtrendsR")
install.packages("reshape2")
install.packages("ggplot2")

# Cargar librerias
library(gtrendsR) # esta es para utilizar google trends
library(reshape2) # esta es para modificar mi base de datos
library(ggplot2) # esta es para graficar las tendencias

# Definir los keywords de búsqueda en Google Trends
keywords = c("semáforo", "covid19", "vacuna")
keywords

# Definir el pais
country=c("MX")

# Definir la temporalidad
time=("2020-01-01 2020-12-05")

# Canal
channel=c("web")

# Hacer el query a google trends
covidTrends = gtrends(keyword=keywords, geo=country, gprop=channel, time = time)

time_trend = covidTrends$interest_over_time # Salvar mi base de datos como un objeto
head(time_trend) # despliega nada más las primeras 6 observaciones


# Vamos a graficar
plot = ggplot(data=time_trend, aes(x=date, y=hits, group=keyword, col=keyword))
plot + geom_line() + xlab("Fecha") + ylab("Relative Interest") + theme_bw() +
  theme(legend.title= element_blank(), legend.position="bottom", legend.text=element_text(size=12)) +
  ggtitle("Magnitud de Búsquedas en Google")

