## Ejercicio de SQL

rm(list=ls())

#install.packages("sqldf")
library(sqldf)

setwd("G:/Mi unidad/TEC/clases/2021/dataScience/Clase3")

admisiones <- read.csv("admisiones.csv")
alumnos <- read.csv("alumnos.csv")
campus <- read.csv("campus.csv")
clases <- read.csv("clases.csv")
cursos <- read.csv("cursos.csv")
edificios <- read.csv("edificios.csv")
profesores <- read.csv("profesores.csv")


sqldf("SELECT FNAME, LNAME, EDAD, SEXO
        FROM alumnos
        WHERE SEXO='1' AND EDAD<'40'")


sqldf("SELECT SEXO, COUNT() AS NUMERO_ALUMNOS
        FROM alumnos
        GROUP BY (SEXO)")


q2 <- sqldf("SELECT alumnos.*, clases.ID_CURSO, clases.CALIFICACION, cursos.NOMBRE 
            FROM alumnos
            INNER JOIN clases ON (clases.ID_ALUMNOS=alumnos.ID_ALUMNOS)
            INNER JOIN cursos ON (clases.ID_CURSO=cursos.ID_CURSO)")

library("tidyverse")

q3 <- alumnos %>% 
        inner_join(clases, by="ID_ALUMNOS") %>% 
        inner_join(cursos, by="ID_CURSO") %>% 
        select(FNAME:SEXO, ID_CURSO, CALIFICACION, NOMBRE)

