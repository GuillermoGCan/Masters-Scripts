#### Tutorial ####

#Paso1: cargar la librería "deSolve"empleando la función library
library("deSolve")

sars.epidemic <- function(t,state,parameters) {
  #La segunda línea indica la forma en la que se deben
  #arreglar los valores una vez que se integra numéricamente el modelo,
  with(as.list(c(state,parameters)),{
    #después de esta instrucción el modelo dinámico es especificado:
    
    #Endogenous auxiliary variables
    #The probability of contact with infected person
    # is equal to the ration of the stock of population infected to the Total Population
    Probability.of.Contact.with.Infected.Person<- population.infected.with.SARS/Total.Population #[1] dimmensionless
    #The susceptible contacts are equal to the stock of
    #population susceptible to SARS by the contact frequency
    Susceptible.Contacts <- population.susceptible.to.SARS*Contact.frequency #people/day
    #The contacts between infected and uninfected people
    #are determinedby the pool of susceptible contacts
    #and by the probability of contacting an infected person
    Contacts.between.Infected.and.Uninfected.People<- Susceptible.Contacts*
      Probability.of.Contact.with.Infected.Person #people/day
    
    #Flow variables
    #The infection rate is determined by the total number of contacts
    #between infected and uninfected people each day and
    #the probability that each such contact results in transmission
    #from the infected to uninfected person (denoted infectivity)
    Infection.Rate <- Contacts.between.Infected.and.Uninfected.People*Infectivity #People/day
    
    #La variable recovery rate se determina por el número de personas infectado dividido
    # entre la duración promedio de la infectividad.
    Recovery.Rate <- population.infected.with.SARS/Average.Duration.of.Infectivity #People/day
    
    #State (stock) variables
    
    #The Population Susceptible to SARS is the equal to the population susceptible
    #prior to the onset of the disease less all of those that have contracted it.
    #It is initialized to the Total Population,
    #which assumes that all individuals are initially susceptible
    #(no prior natural or vaccine-conferred immunity)
    dpopulation.susceptible.to.SARS <- (-1)*Infection.Rate #Stock units: People
    
    #The population infected with SARS is equal to the population
    #initially infected with the VIRUS plus those who have contracted it
    #minus the people who have recovered from it after a determined recovery time
    dpopulation.infected.with.SARS <- Infection.Rate - Recovery.Rate #Stock units: People
    
    #The population recovered from SARS is equal to the population 
    #initially recovered from the virus plus thos who have recovered from it.
    dpopulation.recovered.from.SARS <- Recovery.Rate #Stock units: People
    
    #la última línea indica que variables se imprimen como resultado de la integración
    #nota: es importante asegurarse que todas las variables de estado estén listadas
    # en esta línea
    list(c(dpopulation.susceptible.to.SARS,dpopulation.infected.with.SARS,
           dpopulation.recovered.from.SARS))
  })
}

#Paso 2:
# Especificación de parametros, condiciones iniciales,
# período de análisis y método de integración

#la siguiente línea crea un vector con los valores de los parámetros del modelo
parameters<- c(Infectivity = 0.1/2,    #[1] dimmensionless
               Contact.frequency=2,  # people/day
               Total.Population=350, # people
               Average.Duration.of.Infectivity = 14) #Days

#la siguiente línea crea un vector con los valores de las condiciones iniciales de cada variable de estado
InitialConditions <- c(population.susceptible.to.SARS = 350,
                       population.infected.with.SARS = 1,
                       population.recovered.from.SARS = 0)

#la siguiente línea crea un vector con la secuencia de tiempo para la cuál será simulado el modelo
times <- seq(0, #initial time, #days
             120, #end time, #days
             0.25) #time.step, #days

#como paso final elegimos un método de integración del modelo
intg.method <- c("rk4") #existen muchos métodos disponibles, en este ejemplo
#el método elegido es Runge-Kutta de Órden 4

# Resuelve numéricamente el modelo, corre el modelo

out <- ode(y = InitialConditions,
           times = times,
           func = sars.epidemic,
           parms = parameters,
           method = intg.method)

#Analiza gráficamente los resultados
plot(out,col = c("blue"), main = c("population.susceptible.to.SARS", "population.infected.with.SARS",
                                   "population.recovered.from.SARS"),
     mfrow = c(1,3))
