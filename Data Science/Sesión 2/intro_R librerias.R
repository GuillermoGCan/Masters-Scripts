rm(list=ls())

install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")

library(tm)
library(wordcloud)
library(RColorBrewer)

getwd()
setwd("C:/Users/Guillermo GC/Dropbox/TEC MTY MPE/2do Trimestre/Ciencia de datos/Sesión 2")

speech = "vision_futuro_mat_vesp.txt"

modi_txt = readLines(speech)
modi <- Corpus(VectorSource(modi_txt))

# Limpieza de datos
modi_data <- tm_map(modi,stripWhitespace)
modi_data <- tm_map(modi_data,tolower)
modi_data <- tm_map(modi_data,removeNumbers)
modi_data <- tm_map(modi_data,removePunctuation)
modi_data <- tm_map(modi_data,removeWords,stopwords("spanish"))


tdm_modi <- TermDocumentMatrix(modi_data)

TDM1 <- as.matrix(tdm_modi)
View(TDM1)
dim(TDM1)

v = sort(rowSums(TDM1),decreasing = TRUE)
v
summary(v)
v[v>10]

png("miprimernube.png", width=12, height=8, units="in", res=300)
wordcloud(modi_data, scale=c(5, 0.2), min.freq=3, max.words=200, 
          random.order=FALSE, rot.per=0.2, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()
