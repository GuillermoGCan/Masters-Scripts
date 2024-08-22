#====================================================================================
#Growth and collapase Case Study
#===================================================================================
library("deSolve")

parameters<-c(normal.birth.rate=0.35/100,
              regeneration.rate=1.2,
              carrying.capacity=7.5e6,
              minimum.regeneration.rate=1/100,
              rapid.resource.depletion.time=1,
              renewable.resource.consumption.per.capita=1,
              normal.lifetime=30,
              percent.consumers=0.90)

InitialConditions <- c(population = 1e6,
                       renewable.resources=5e6)

times <- seq(0, #initial time
             100, #end of simulation
             0.1)#time step

intg.method<-c("rk4")

growth.collapase.pruyt <- function(t, state, parameters) {
  with(as.list(c(state,parameters)), {
    #auxiliary endogenous variables
    consumer.population<-population*percent.consumers
    per.capita.renewable.resource.availability<-renewable.resources/population
    resource.availability.dependent.lifetime<-max(15,min(100,
                                                             normal.lifetime*
                                                             per.capita.renewable.resource.availability))
    minimum.regeneration<-carrying.capacity*minimum.regeneration.rate
    resource.dependent.regeneration<-regeneration.rate*
                                     renewable.resources*
                                     (renewable.resources/carrying.capacity)*
                                     (1-renewable.resources/carrying.capacity)
    #flow variables
     births.flow<-population*per.capita.renewable.resource.availability*normal.birth.rate
     deaths.flow<-consumer.population/resource.availability.dependent.lifetime
     regeneration<-minimum.regeneration+resource.dependent.regeneration
     resource.use<-min(population*renewable.resource.consumption.per.capita,
                       renewable.resources/rapid.resource.depletion.time)
    #state variables
     dpopulation<-births.flow-deaths.flow
     drenewable.resources<-regeneration-resource.use
    
     list(c(dpopulation,drenewable.resources),
          resource.use=resource.use,
          resource.availability.dependent.lifetime=resource.availability.dependent.lifetime)
  })
}

#Simulate model
out <- ode(y = InitialConditions,
           times = times,
           func = growth.collapase.pruyt,
           parms = parameters,
           method =intg.method )

head(out)

class(out)

out1<-data.frame(out)

#Plot behavior
plot(out, which=c("population","renewable.resources"),
     xlab = "time", ylab =c("people","renewable resource units"))

#======================================================================================================================================

#next we discuss how to plot different runs  and different variables
#Plot behavior
plot(out, which=c("population","renewable.resources",
                  "resource.availability.dependent.lifetime"),xlab = "time", ylab 
     =c("people","renewable resource units","lifetime"))


#ggplot2
library(ggplot2)
  ggplot(out1,aes(x=time, y=renewable.resources))+geom_line()

head(out)

