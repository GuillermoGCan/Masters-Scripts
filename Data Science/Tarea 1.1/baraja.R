#### Código de baraja de póker ####

num <- c(1 ,2 ,3 ,4 ,5 ,6 ,7 ,8 ,9 ,10 ,11 ,12 ,13)
palos <- c(" trebol "," espadas "," corazones "," picas ")
baraja <- c()
for (i in num ){
  for (j in palos ){
    card <- ( paste (i, j, sep ="-"))
    baraja <- c( baraja , card )
  }
}

# Toma el código anterior como punto de partida y utiliza R para resolver las
#   siguientes tareas.

## a) Construye una función que tome una carta del mazo al azar.

alAzar <- function(input_baraja,input_numero_cartas){
  carta = sample(input_baraja,input_numero_cartas)
  return(carta)
}

alAzar(baraja,1)

## b) ¿Cuál es la probabilidad de sacar un as de espadas? Calcula la 
##      probabilidad de forma analítica y presenta tu resultado. Cálcula la 
##      probabilidad simulando que sacas 10,000 cartas al azar y
##      verificando cuántas de esas 10,000 son un as de espadas y presenta tu código.

prob_analitica <- 1/52 #probabilidad analítica de sacar un as de espadas

# Declaro mis variables iniciales
as_espadas <- is.numeric(0)
no_as_espadas <- is.numeric(0)
k <- is.numeric(0)

# Inicio ciclo de sacar 10,000 cartas al azar.
for (k in 1 : 10000){
  carta = alAzar(baraja,1)
  if(carta == "1- espadas "){
    as_espadas = as_espadas + 1
    }
  else{
    no_as_espadas = no_as_espadas + 1
  }
}

# Cálculo empírico de probabilidad de sacar un as de espadas con 10,000 experimentos
prob_as_espadas <- as_espadas/(as_espadas + no_as_espadas)

## c) Revuelve tus 52 cartas aleatoriamente y retira cuatro cartas al azar. 
##      Corre una simulación para calcular la probabilidad de que obtengas un  
##      J, Q, R del mismo palo. Presenta el resultado y el código para generarlo.

# Declaro mis variables iniciales
l <- is.numeric(0)
jqr <- is.numeric(0)
njqr <- is.numeric(0)
counter <- is.numeric(0)

for (l in 1 : 10000){
  baraja_revuelta <- sample(baraja) # aquí revuelvo mi baraja
  cuatroCartas <- alAzar(baraja_revuelta,4)
  cuatroCartas_list <- strsplit(cuatroCartas, "-")
  cuatroCartas_vec <- unlist(cuatroCartas_list)
  numero_vec <- as.numeric(c(cuatroCartas_vec [1], cuatroCartas_vec [3], 
                  cuatroCartas_vec [5], cuatroCartas_vec [7]))
  palos_vec <- as.character(c(cuatroCartas_vec [2], cuatroCartas_vec [4], 
                              cuatroCartas_vec [6], cuatroCartas_vec [8]))
  
  if ((palos_vec[1] == palos_vec[2]) & (palos_vec[1] == palos_vec[3]) & (palos_vec[1] == palos_vec[4])){
    for (m in numero_vec){
      if(m > 10){
        counter = counter + 1
      }
    }
      if (counter == 3){
        jqr = jqr + 1
        counter = 0
      } else {
        njqr = njqr + 1
        counter = 0
      }
  } else if ((palos_vec[1] == palos_vec[2]) & (palos_vec[1] == palos_vec[3])){
    if (sum(numero_vec[1],numero_vec[2],numero_vec[3]) == 36){
      jqr = jqr + 1
    } else {
      njqr = njqr + 1
    }
  } else if ((palos_vec[1] == palos_vec[2]) & (palos_vec[1] == palos_vec[4])){
    if (sum(numero_vec[1],numero_vec[2],numero_vec[4]) == 36){
      jqr = jqr + 1
    } else {
      njqr = njqr + 1
    }
  } else if ((palos_vec[1] == palos_vec[3]) & (palos_vec[1] == palos_vec[4])){
    if (sum(numero_vec[1],numero_vec[3],numero_vec[4]) == 36){
      jqr = jqr + 1
    } else {
      njqr = njqr + 1
    }
  } else if ((palos_vec[2] == palos_vec[3]) & (palos_vec[2] == palos_vec[4])){
    if (sum(numero_vec[2],numero_vec[3],numero_vec[4]) == 36){
      jqr = jqr + 1
    } else {
      njqr = njqr + 1
    }
  } else {
    njqr = njqr + 1
  }
}

prob_jqr <- jqr/(jqr + njqr)

