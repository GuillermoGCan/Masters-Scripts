
##############################################################################################

# TAREA 1

##############################################################################################

#Asignación de directorio

setwd("E:/Proyectos 2022/Curso/Curso 2022/Clase 2")

#Instalar paquetes

install.packages("tidyverse")
install.packages("fpp3")
install.packages("forecast")
installed.packages("ggplot2")
install.packages("astsa")
install.packages("GGally")
install.packages("fma")
install.packages("expsmooth")
install.packages("tsibble")

# Ejercicio 1

# 1a.	Convierte la serie de tiempo de la siguiente tabla en "tsibble" en R. 

y <- tsibble(
  Year = 2015:2019,
  Observation = c(48, 36, 25, 30, 68),
  index = Year
)

# b.	Gráfica la serie. Puedes usar el siguiente código:
  
  autoplot(y) +
  ggtitle("Aprobación presidencial (% promedio en México, anual)") +
  ylab("%") +
  xlab("Año")

# c.	¿La serie sigue alguna tendencia, estacionalidad o ciclo?
  
# Ejercicio 2
  
# a.	Carga la base de datos en R 

tute1 <- readr::read_csv("tute1.csv")
View(tute1)
  
# b.	Transforma los datos en una serie de tiempo para R.

mytimeseries <- tute1 %>%
  mutate(Quarter = yearmonth(Quarter)) %>%
  as_tsibble(index = Quarter)

# c.	Gráfica las series de tiempo de las variables en la base de datos.
mytimeseries %>%
  pivot_longer(-Quarter) %>%
  ggplot(aes(x = Quarter, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y")

# d.	Revisa qué pasa cuando no incluyes facet_grid().
mytimeseries %>%
  pivot_longer(-Quarter) %>%
  ggplot(aes(x = Quarter, y = value, colour = name)) +
  geom_line()

# Ejercicio 3

# 3a.	Crea un training set que excluya los últimos cuatro años y los convierta en un test set.

library(tsibbledata)
View(hh_budget)

hh_budget %>%
  autoplot(Wealth)

hh_budget_train <-
  hh_budget %>%
  filter_index(. ~ '2012')

hh_budget_test <-
  hh_budget %>% 
  filter_index('2013' ~ .)

# b.	Ejecuta los métodos simples de pronóstico con el training set y pronostica para los correspondientes al test set.

hh_budget_models <-
  hh_budget_train %>% 
  model(
    mean = MEAN(Wealth),
    naive = NAIVE(Wealth),
    snaive = SNAIVE(Wealth),
    Drift= NAIVE(Wealth~drift())
  )

hh_budget_forecast <-
  hh_budget_models %>% 
  forecast(hh_budget_test)

View(hh_budget_forecast)


# c.	Calcula la exactitud de los métodos. ¿Cuál es el mejor?

hh_budget_fc <- hh_budget_models %>%
  forecast(hh_budget_test)
accuracy(hh_budget_fc, hh_budget)

accuracy(hh_budget_fc, hh_budget) %>%
  group_by(Country) %>% 
  slice(1)

# Ejercicio 4

# Establece si las siguientes frases son verdaderas o falsas.

# a.	El mejor indicador para evaluar la exactitud de un pronóstico es el MAPE. FALSO.
# b.	Los modelos explicativos predicen mejor las series de tiempo. FALSO.
# c.	Siempre debe escogerse el modelo con el mejor desempeño en el test set. VERDADERO.
# d.	Los modelos simples de pronóstico son los mejores, por su sencillez. FALSO.


##############################################################################################

# GRÁFICAS CON SERIES DE TIEMPO

##############################################################################################


#Visualización de gráficas básicas 

library(ggplot2)
ggplot(data=iris,
       aes(Sepal.Length, Petal.Length))

ggplot(iris, aes(Sepal.Length, Petal.Length)) +
  geom_point()

ggplot(iris, aes(Sepal.Length, Petal.Length, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(iris, aes(Sepal.Length, Petal.Length)) +
  geom_point(aes(color = Species)) # Notar diferencia! aes() aparece en geom_point()

ggplot(iris, aes(Sepal.Length, Petal.Length)) +
  geom_point(aes(color = Species))+
  geom_smooth(method="lm")

ggplot(iris, aes(Sepal.Length, Petal.Length)) +
  geom_point()

ggplot(iris, aes(Sepal.Length, Petal.Length, color='magenta')) +
  geom_point()

ggplot(iris, aes(Sepal.Length, Petal.Length)) +
  geom_point()+
  facet_wrap(~Species)

ggplot(iris, aes(Sepal.Length, Petal.Length)) +
  geom_point(aes(color=Petal.Width))+
  facet_wrap(~Species)

p3 <- ggplot(iris, aes(Species, Sepal.Length)) +
  geom_boxplot()
p3

library(fpp2)

autoplot(melsyd[,"Economy.Class"]) +
  ggtitle("Economy class passengers: Melbourne-Sydney") +
  xlab("Year") +
  ylab("Thousands")

autoplot(a10) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")

ggseasonplot(a10, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")

ggseasonplot(a10, polar=TRUE) +
  ylab("$ million") +
  ggtitle("Polar seasonal plot: antidiabetic drug sales")

autoplot(elecdemand[,c("Demand","Temperature")], facets=TRUE) +
  xlab("Year: 2014") + ylab("") +
  ggtitle("Half-hourly electricity demand: Victoria, Australia")

autoplot(visnights[,1:5], facets=TRUE) +
  ylab("Number of visitor nights each quarter (millions)")

GGally::ggpairs(as.data.frame(visnights[,1:5]))

aelec <- window(elec, start=1980)
autoplot(aelec) + xlab("Year") + ylab("GWh")

#Otras gráficas

library(tidyquant)
library(plotly)
tickers = c("GOOG", "AAPL", "AMZN", "FB", "NFLX", "MSFT")
for (i in tickers){
  getSymbols(i,
             from = "2018-01-01",
             to = "2019-12-31")}

x <- list(
  title = "date"
)
y <- list(
  title = "value"
)

stock <- data.frame(GOOG$GOOG.Adjusted,
                    AAPL$AAPL.Adjusted,
                    AMZN$AMZN.Adjusted,
                    FB$FB.Adjusted,
                    NFLX$NFLX.Adjusted,
                    MSFT$MSFT.Adjusted)
stock$GOOG.Adjusted <- stock$GOOG.Adjusted/stock$GOOG.Adjusted[1]
stock$AAPL.Adjusted <- stock$AAPL.Adjusted/stock$AAPL.Adjusted[1]
stock$AMZN.Adjusted <- stock$AMZN.Adjusted/stock$AMZN.Adjusted[1]
stock$FB.Adjusted <- stock$FB.Adjusted/stock$FB.Adjusted[1]
stock$NFLX.Adjusted <- stock$NFLX.Adjusted/stock$NFLX.Adjusted[1]
stock$MSFT.Adjusted <- stock$MSFT.Adjusted/stock$MSFT.Adjusted[1]
stock <- data.frame(stock,rownames(stock))
colnames(stock) <- append(tickers,'Dates')

ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE
)

fig1 <- plot_ly(stock, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
  add_trace(x = ~Dates, y = ~GOOG, name = 'GOOG')%>%
  layout(legend=list(title=list(text='company')), xaxis = ax, yaxis = list(range = c(0.5,2), title = 'value'))


fig2 <- plot_ly(stock, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
  add_trace(x = ~Dates, y = ~AAPL, name = 'AAPL')%>%
  layout(legend=list(title=list(text='company')), xaxis = ax, yaxis = list(range = c(0.5,2),title = '', showticklabels = FALSE))


fig3 <- plot_ly(stock, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
  add_trace(x = ~Dates, y = ~AMZN, name = 'AMZN')%>%
  layout(legend=list(title=list(text='company')), xaxis = ax, yaxis = list(range = c(0.5,2), title = 'value'))


fig4 <- plot_ly(stock, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
  add_trace(x = ~Dates, y = ~FB, name = 'FB')%>%
  layout(legend=list(title=list(text='company')), xaxis = ax, yaxis = list(range = c(0.5,2),title = '', showticklabels = FALSE))


fig5 <- plot_ly(stock, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
  add_trace(x = ~Dates, y = ~NFLX, name = 'NFLX')%>%
  layout(legend=list(title=list(text='company')), xaxis = list(title = 'Date'), yaxis = list(range = c(0.5,2), title = 'value'))


fig6 <- plot_ly(stock, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
  add_trace(x = ~Dates, y = ~MSFT, name = 'MSFT')%>%
  layout( legend=list(title=list(text='company')), yaxis = list(range = c(0.5,2) ,showticklabels = FALSE, title =''),  xaxis = list(title = 'Date'))


fig <- subplot(fig1, fig2, fig3, fig4, fig5, fig6,
               nrows = 3, titleY = TRUE, titleX = TRUE) %>% layout(
                 xaxis = list(zerolinecolor = '#ffff',
                              zerolinewidth = 2,
                              gridcolor = 'ffff'),
                 yaxis = list(zerolinecolor = '#ffff',
                              zerolinewidth = 2,
                              gridcolor = 'ffff'),
                 plot_bgcolor='#e5ecf6')
annotations = list(
  list(
    x = 0.225,
    y = 1.0,
    font = list(size = 10),
    text = "company=GOOG",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.775,
    y = 1,
    font = list(size = 10),
    text = "company=AAPL",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.225,
    y = 0.64,
    font = list(size = 10),
    text = "company=AMZN",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.775,
    y = 0.64,
    font = list(size = 10),
    text = "company=FB",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.225,
    y = 0.315,
    font = list(size = 10),
    text = "company=NFLX",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.775,
    y = 0.315,
    font = list(size = 10),
    text = "company=MSFT",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  )
)

fig <- fig %>%layout(annotations = annotations, width = 900)
options(warn = -1)
fig

##################################################################

library(tidyquant)
library(plotly)
tickers = c("GOOG", "AAPL", "AMZN", "FB", "NFLX", "MSFT")
for (i in tickers){
  getSymbols(i,
             from = "2018-01-01",
             to = "2019-12-31")}
stock <- data.frame(GOOG$GOOG.Adjusted,
                    AAPL$AAPL.Adjusted,
                    AMZN$AMZN.Adjusted,
                    FB$FB.Adjusted,
                    NFLX$NFLX.Adjusted,
                    MSFT$MSFT.Adjusted)
stock$GOOG.Adjusted <- stock$GOOG.Adjusted/stock$GOOG.Adjusted[1]
stock$AAPL.Adjusted <- stock$AAPL.Adjusted/stock$AAPL.Adjusted[1]
stock$AMZN.Adjusted <- stock$AMZN.Adjusted/stock$AMZN.Adjusted[1]
stock$FB.Adjusted <- stock$FB.Adjusted/stock$FB.Adjusted[1]
stock$NFLX.Adjusted <- stock$NFLX.Adjusted/stock$NFLX.Adjusted[1]
stock$MSFT.Adjusted <- stock$MSFT.Adjusted/stock$MSFT.Adjusted[1]
stock <- data.frame(stock,rownames(stock))
colnames(stock) <- append(tickers,'Dates')

fig <- plot_ly(stock, type = 'scatter', mode = 'lines')%>%
  add_trace(x = ~Dates, y = ~GOOG, name = 'GOOG')%>%
  add_trace(x = ~Dates, y = ~AAPL, name = 'AAPL')%>%
  add_trace(x = ~Dates, y = ~AMZN, name = 'AMZN')%>%
  add_trace(x = ~Dates, y = ~FB, name = 'FB')%>%
  add_trace(x = ~Dates, y = ~NFLX, name = 'NFLX')%>%
  add_trace(x = ~Dates, y = ~MSFT, name = 'MSFT')%>%
  layout(title = 'custom tick labels',legend=list(title=list(text='variable')),
         xaxis = list(dtick = "M1", tickformat="%b<br>%Y"), width = 1000)
options(warn = -1)
fig <- fig %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6')


fig


