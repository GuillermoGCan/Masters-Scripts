#### Generales ####

rm(list =ls())

# Descarga el archivo del censo 2020 por AGEB para
# Nuevo Leon
download.file (url = 
'https://www.inegi.org.mx/contenidos/programas/ccpv/2020/microdatos/ageb_manzana/RESAGEBURB_19_2020_csv.zip', 
destfile ='censo20_NL.zip', method ='curl')

# Descomprime el archivo . zip
zipF <- "censo20_NL.zip"
outDir <-"./unzipfolder20"
unzip (zipF , exdir = outDir )

# Lee el archivo que descomprimes .
d20 <- read.csv(
  paste(outDir, "/", "RESAGEBURB_19CSV20.csv", sep =""),
  encoding ="UTF-8", na.strings ="*",
  stringsAsFactors = FALSE )
head ( d20 )

#### Tidyverse y SQL ####

# Librerías
library(tidyverse)
library(dplyr)

# Función nueva
`%!in%` = Negate("%in%")

# Ejercicio 1a
d20_1a <- d20 %>% 
  filter(NOM_LOC %!in% c("Total de la entidad", "Total del municipio",
                                      "Total de la localidad urbana", "Total AGEB urbana"))

# otra forma de llegar al mismo resultado
d20_1a2 <- d20 %>% 
  filter(MZA != 0)

# Ejercicio 1b
d20_1b <- d20_1a %>% 
  filter(NOM_MUN %in% c("Monterrey", "San Nicolás de los Garza", "San Pedro Garza García",
                                           "Santa Catarina", "Guadalupe", "General Escobedo", "Apodaca",
                                           "Juárez", "García", "Cadereyta Jiménez", "Pesquería", "El Carmen",
                                           "General Zuazua", "Salinas Victoria", "Ciénega de Flores",
                                           "Santiago", "Hidalgo", "Abasolo")) 

d20_1b2 <- d20_1b %>% 
  select(GRAPROES,POBTOT,VIVPAR_HAB,VPH_INTER,PRO_OCUP_C,PSINDER)

# Ejercicio 1c

d20_1c <- d20_1b2 %>% 
  na.exclude() %>% 
  filter(POBTOT != 0)

# Ejercicio 1d

d20_1d <- d20_1b %>% 
  select(NOM_MUN, GRAPROES,POBTOT,VIVPAR_HAB,VPH_INTER,PRO_OCUP_C,PSINDER) %>% 
  na.exclude() %>% filter(POBTOT != 0) %>% 
  group_by(NOM_MUN) %>% 
  transmute(GRAPROES_NUM = as.numeric(GRAPROES)) %>% 
  filter(!is.na(GRAPROES_NUM)) %>% 
  summarise(MIN = min(GRAPROES_NUM), PROM = mean(GRAPROES_NUM), MAX = max(GRAPROES_NUM)) %>% 
  format.data.frame(digits=3)

#### Regresión Lineal ####

# Ejercicio 2a

#install.packages("scales")
library(scales)

d20_2a <- d20_1b %>% 
  select(NOM_MUN,GRAPROES,POBTOT,VIVPAR_HAB,VPH_INTER,PRO_OCUP_C,PSINDER) %>% 
  na.exclude() %>% 
  filter(POBTOT != 0) %>% 
  group_by(NOM_MUN) %>% 
  mutate(GRAPROES_NUM = as.numeric(GRAPROES),VPH_INTER_NUM = as.numeric(VPH_INTER),PSINDER_NUM = as.numeric(PSINDER)) %>% 
  transmute(GRAPROES_NUM, VPH_INTER_NUM, POBTOT, PSINDER_NUM) %>% 
  na.exclude() %>% 
  summarise(MIN_GRAPROES = min(GRAPROES_NUM), PROM_GRAPROES = mean(GRAPROES_NUM), MAX_GRAPROES = max(GRAPROES_NUM), 
            PROM_VPH_INTER = mean(VPH_INTER_NUM), POR_PSINDER_POBTOT = label_percent()(sum(PSINDER_NUM)/sum(POBTOT))) %>% 
  format.data.frame(digits=3)

# Ejercicio 2b

d20_2b <- d20_1a %>% 
  select(GRAPROES, VIVPAR_HAB, VPH_INTER, POBTOT) %>%
  na.exclude() %>% 
  filter(POBTOT != 0) %>% 
  select(GRAPROES, VIVPAR_HAB, VPH_INTER) %>% 
  transmute(GRAPROES_NUM = as.numeric(GRAPROES), VIVPAR_HAB_NUM = as.numeric(VIVPAR_HAB), 
            VPH_INTER_NUM = as.numeric(VPH_INTER)) %>% 
  na.exclude() %>% 
  filter(VIVPAR_HAB_NUM != 0) %>% 
  transmute(GRAPROES_NUM, PER_VPH_INTER = label_percent()(VPH_INTER_NUM/VIVPAR_HAB_NUM))

  #plot(d20_2b$GRAPROES_NUM,d20_2b$PER_VPH_INTER) DUDA!!!!!!


  


