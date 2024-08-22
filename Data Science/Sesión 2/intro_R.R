#### Mi primer script en R (que no es el primero en realidad) ####

#### Variables ####
x=5
print (x)

y="mmm"
y

#### Vectores ####
v1 = c(4,8,10,11,20)
v1 <- c(4,8,10,11,20) #Usar = o <- es lo mismo, aunque este es más convencional
class(v1)

v2 <- c("a","b","c","d")

print(v2)
class(v2)

v1[1:3] #esto es el indexado, se usa para imprimir los datos que necesites en el vector
v2[2:4]

length(v1)
length(v2)

v3<- c(v1[1:3],length(v2))
v3

#### Matrices ####
m1 <- matrix(c(4,1,8,6,9,1),nrow=3,ncol=2)
print(m1)

m1[1,]
m1[,2]
m1[2,2]

#### Bases de datos ####

d <- data.frame(m1)
print(d)
row.names(d) <- c("a","b","c")
print(d)
colnames(d) <- c("col1","col2")
print(d)

d2 <- data.frame(COL1=c(1,2,3,4),COL2=c(5,6,7,8))
print(d2)

d$col1
d$col2
d[,"col1"] #son equivalentes las dos formas de acceder a las columnas
d[,"col2"]

d2$COL1
d2$COL2
d2[,"COL1"]
d2[,"COL2"]

d$col3 <- d$col1 * d$col2
print(d)

d2[,"COL3"] <- d2[,"COL1"] * d2[,"COL2"] #esto es equivalente al anterior, pero con otra sintaxis
print(d2)

#Agregar filtro
d
d[d$col1<=4,]

t

#### Ejercicio de práctica ####

d2 <- data.frame(COL1=c(1,2,3,4,20,40,1,4),COL2=c(4,5,6,7,3,100,3,1))
d2

## 1. Valor mínimo de la fila 1

min(d2[1,])

## 2. Obtener el promedio de la columna 1
mean(d2$COL1)

## 3. Crear una nueva columna que sea la suma de COL1 y COL2
d2[,"COL3"] <- d2[,"COL1"] + d2[,"COL2"]
print(d2)

#### Listas ####
l1 <- list(v1, v2, d, d2,y)
class(l1)
print(l1)

l1[[1]][1:3]


#### Funciones ####
#Supongamos que tenemos un triangulo y queremos calcular el área
# base*altura/2

base=5
altura=4

base*altura/2

miArea <- function(input_base,input_altura){
  area = input_base*input_altura/2
  return(area)
}

miArea(5,4)

#### Iteradores #### 

##Hay dos tipos, "for" y "while"
# Iterador "for"

for(i in 1:10){
  print (i)
}

for(i in c(2,5,10)){
  print (i)
}

for(i in l1){
  print (i)
}

# Iterador "while"

i=1
while (i<=10){
  print(i)
  i=i+1
}

#### Condicionales ####
## Función para convertir monedas a peso

currencyConverter <- function(dinero, moneda){
  if(moneda == "dolar"){
    pesos = dinero * 20
  }
  else if(moneda == "euro"){
    pesos = dinero * 26
  }
  else{
    pesos = "intenta otra moneda"
  }
  return(pesos)
}

d2$basemenos10 <- ifelse(d2$base<10,"menor de 10","mayor de 10") #falta revisar, marca error

#### Simulaciones o probabilidades ####
# Escribir una función que genere una moneda y la aviente aleatoriamente.
# Mete tu función en un iterador y lanza la moneda mil veces.
# Guarda tu resultado en un vector.
# c("H","T","H","T",...n=1000)


vec <- vector(mode="numeric")

for (i in 1:1000){ 
  tiro = runif(1,min=0,max=1)
  if (tiro <=0.5){
    resultado = "H"
  }
  else {
    resultado ="T"
  }
  vec[i]=resultado
}
print(vec)
table(vec)

#Opción del Profe

headTail <- function(){
  p<- runif(1)
  if(p<0.5){
    res= "H"
  }else{
    res="T"
  }
  return(res)
}

headTail()
n=1000
res <-c()

for (i in 1:n) {
  res <- c(res,headTail())
}

prop.table(table(res))

## Simulen que lanzan tres monedas de forma secuencial
# Calcula la probabilidad de que el primer lanzamiento será una cara y que en total
# obtengas más caras que cruces en los tres lanzamientos

n=1000
counter=0

for(i in 1:n){
  m1 <- headTail()
  m2 <- headTail()
  m3 <- headTail()
  
  if(m1=="H"){
    if(m2=="H" | m3=="H"){
      counter=counter+1
    } 
  }
}
counter/n