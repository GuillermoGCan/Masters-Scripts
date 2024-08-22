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
  select(GRAPROES,POBTOT,TVIVPARHAB,VPH_INTER,PRO_OCUP_C,PSINDER)

# Ejercicio 1c

d20_1c <- d20_1b2 %>% 
  na.exclude() %>% 
  filter(POBTOT != 0)

# Ejercicio 1d

d20_1d <- d20_1b %>% 
  select(NOM_MUN, GRAPROES,POBTOT,TVIVPARHAB,VPH_INTER,PRO_OCUP_C,PSINDER) %>% 
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
  select(NOM_MUN,GRAPROES,POBTOT,TVIVPARHAB,VPH_INTER,PRO_OCUP_C,PSINDER) %>% 
  na.exclude() %>% 
  filter(POBTOT != 0) %>% 
  group_by(NOM_MUN) %>% 
  mutate(GRAPROES_NUM = as.numeric(GRAPROES),VPH_INTER_NUM = as.numeric(VPH_INTER),PSINDER_NUM = as.numeric(PSINDER), 
         TVIVPARHAB_NUM = as.numeric(TVIVPARHAB)) %>% 
  transmute(GRAPROES_NUM, VPH_INTER_NUM, POBTOT, PSINDER_NUM, TVIVPARHAB_NUM) %>% 
  na.exclude() %>% 
  summarise(MIN_GRAPROES = min(GRAPROES_NUM), 
            PROM_GRAPROES = mean(GRAPROES_NUM), 
            MAX_GRAPROES = max(GRAPROES_NUM), 
            PER_VPH_INTER = label_percent()(sum(VPH_INTER_NUM)/sum(TVIVPARHAB_NUM)), 
            PER_PSINDER = label_percent()(sum(PSINDER_NUM)/sum(POBTOT))) %>% 
  format.data.frame(digits=3)

# Ejercicio 2b

d20_2b <- d20_1a %>% 
  select(GRAPROES, TVIVPARHAB, VPH_INTER, POBTOT) %>%
  na.exclude() %>% 
  filter(POBTOT != 0) %>% 
  select(GRAPROES, TVIVPARHAB, VPH_INTER) %>% 
  transmute(GRAPROES_NUM = as.numeric(GRAPROES), TVIVPARHAB_NUM = as.numeric(TVIVPARHAB), 
            VPH_INTER_NUM = as.numeric(VPH_INTER)) %>% 
  na.exclude() %>% 
  transmute(GRAPROES_NUM, PER_VPH_INTER = 100*(VPH_INTER_NUM/TVIVPARHAB_NUM))

ols_2b <- lm(GRAPROES_NUM ~ PER_VPH_INTER, data = d20_2b)
cor(d20_2b$PER_VPH_INTER,d20_2b$GRAPROES_NUM) #grado de asociación entre variables

summary(ols_2b)

# Ejercicio 2c 

plot(d20_2b$PER_VPH_INTER,d20_2b$GRAPROES_NUM)
abline(lm(d20_2b$GRAPROES_NUM ~ d20_2b$PER_VPH_INTER),col="red")

# Ejercicio 2d

d20_2d <- d20_1a %>% 
  select(GRAPROES, TVIVPARHAB, VPH_INTER, POBTOT, PSINDER, PRO_OCUP_C) %>%
  na.exclude() %>% 
  filter(POBTOT != 0) %>% 
  transmute(POBTOT, GRAPROES_NUM = as.numeric(GRAPROES), TVIVPARHAB_NUM = as.numeric(TVIVPARHAB), 
            VPH_INTER_NUM = as.numeric(VPH_INTER), PSINDER_NUM = as.numeric(PSINDER),
            PRO_OCUP_C_NUM = as.numeric(PRO_OCUP_C)) %>% 
  na.exclude() %>% 
  transmute(GRAPROES_NUM, PER_VPH_INTER = 100*(VPH_INTER_NUM/TVIVPARHAB_NUM),
            PRO_OCUP_C_NUM, PER_PSINDER = 100*(PSINDER_NUM/POBTOT))

ols_2d <- lm(GRAPROES_NUM ~ 
               PER_VPH_INTER + PRO_OCUP_C_NUM + PER_PSINDER, data = d20_2d)
summary(ols_2d)

# Ejercicio 2e
# Sesgo de la variable omitida. Si hay sesgo de la variable omitida porque existen
# otras variables que se relacionan tanto con el Grado Promedio de Escolaridad como
# con el acceso a internet, los ocupantes por cuarto y el porcentaje de población sin
# afiliación a servicios de salud. Ejemplos serían los ingresos promedio per cápita
# de la población y la zona donde se encuentre ubicada esa manzana.


  


