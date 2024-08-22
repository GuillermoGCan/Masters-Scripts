library(tm)

texto = "tarea1_1.txt"
vec_texto = readLines(texto)
lista_texto <- Corpus (VectorSource(vec_texto))

lista_texto <- tm_map(lista_texto, stripWhitespace)
lista_texto <- tm_map(lista_texto, removePunctuation)
lista_texto <- tm_map(lista_texto, tolower)
vec2_texto <- unlist(lista_texto)
list_palabras <- strsplit(vec2_texto," ")
vec_palabras <- unlist(list_palabras)


vec_preposicionesyarticulos <- c("a", "ante", "bajo", "cabe", "con", "contra", "de", 
                                 "desde", "durante", "en", "entre", "hacia", "hasta", 
                                 "mediante", "para", "por", "según", "sin", "so", "sobre", 
                                 "tras", "versus", "vía", "el", "la", "los", "las", "uno",
                                 "una", "unos", "unas", "del")

i <- is.character("")
j <- is.character("")
for (i in vec_palabras){
  for (j in vec_preposicionesyarticulos){
    if ( i == j){
      pos <- match(i,vec_palabras)
      vec_palabras <- vec_palabras[-pos]
    }
  }
}

vec_palabras_nuevo <- vec_palabras
vec_palabras <- unlist(list_palabras)


vocales <- c("a", "e", "i", "o", "u")
consonantes <- c("b", "c", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", 
                 "q", "r", "s", "t", "v", "w", "x", "y", "z", "ñ")
caracteres_vec <- nchar(vec_palabras_nuevo)
k <- is.numeric(0)
v <- is.numeric(1)
c <- is.numeric(1)
nvoccon <- is.numeric(0)
nvocvoc <- is.numeric(0)
notras <- is.numeric(0)
vec_voccon <- is.vector(0)
vec_vocvoc <- as.vector(0)
vec_otras <- as.vector(0)

for (k in 1:81){
  while (substring(!vec_palabras_nuevo[k],1,1) == vocales[v]) {
    v = v + 1
    if (v == 6){
      notras = notras + 1
      vec_otras [notras] = vec_palabras_nuevo [k]
      vocales[v] = substring(!vec_palabras_nuevo[k]
    }
  }
  while (substring(!vec_palabras_nuevo[k],caracteres_vec[k],caracteres_vec[k]) == consonantes[c]) {
    c = c + 1
    if (c == 23){
      nvocvoc = nvoccon + 1
      vec_vocvoc [nvocvoc] = vec_palabras_nuevo[k]
      consonantes[c] = substring(!vec_palabras_nuevo[k],caracteres_vec[k],caracteres_vec[k])
    }
  }
  nvoccon = n
}
  
lista_final <- list(vec_voccon, vec_vocvoc, vec_otras)
