#==================================================================
#Run experiment
#================================================================
#======================================================================================
#Part 1: Load the model to your R session
#=======================================================================================
#Load required libraries for the ediam model
 library(deSolve)
 library(shiny)
 library(ggplot2)
 library(Rmisc)
 library(scales)
 library(extrafont)
 library(data.table)
 library(reshape2)
#Load the Ediam model to your R session
#specify directory where you have saved the model
  dir.model<-r"(C:\Users\L03054557\OneDrive\Edmundo-ITESM\2.Cursos Impartidos\Modelacion Avanzada\2022\Labs\Week 11\)"
#specify the model version you are using
  model.version<-"ediam_10_24_2016.r"
#load the model in your session
  source(paste(dir.model,model.version,sep=""))

#Define alternatives
#======================================================================
#Alternative 1
#======================================================================
policies.a1<-c(
#carbon tax
 tax.rate.N=0.60,
 tax.rate.S=0.60,
#Technology push in Advanced Region
 epsi.re.subsidy.N = 0.0, #Technology subsidy in Advanced Region
 s.re.subsidy.N = 0.0, #R&D subsidy in Advanced Region
#Technology push in Emerging Region
 epsi.re.subsidy.S = 0.0, #Technology subsidy in Emerging Region
 epsi.re.GFsubsidy.N = 0.00000000,#Technology subsidy in GCF
 s.re.subsidy.S = 0.0, #R&D subsidy in Emerging Region
 s.re.GFsubsidy.N = 0.00000000, #R&D subsidy in GCF
 policy.half.life= 0.00411079) #Policy half time

#======================================================================
# Alternative 2
#======================================================================
policies.a2<-c(
#carbon tax
  tax.rate.N=0.605709672,
  tax.rate.S=0.295041075,
#Technology push in Advanced Region
  epsi.re.subsidy.N = 0.191382026, #Technology subsidy in Advanced Region
  s.re.subsidy.N = 3.322859643, #R&D subsidy in Advanced Region
#Technology push in Emerging Region
  epsi.re.subsidy.S = 0.202098716, #Technology subsidy in Emerging Region
  epsi.re.GFsubsidy.N = 0.077498849,#Technology subsidy in GCF
  s.re.subsidy.S = 0.733830192, #R&D subsidy in Emerging Region
  s.re.GFsubsidy.N = 0.540613659, #R&D subsidy in GCF
  policy.half.life= 0.003541255) #Policy half time

#=============================================================================================
#
#=============================================================================================
library(lhs)
sample.size<-10
Xs<-c("epsilon","Gamma.re","Gamma.ce","Eta.re","Eta.ce","Nu.re","Nu.ce","Delta.S","Beta.Delta.Temp")
Exp.design<-data.frame(randomLHS(sample.size,length(Xs)))
colnames(Exp.design)<-Xs

####
Exp.design[,Xs[1]]<-qunif(Exp.design[,Xs[1]],3.0,10)
Exp.design[,Xs[2]]<-qunif(Exp.design[,Xs[2]],0.16,0.33)
Exp.design[,Xs[3]]<-qunif(Exp.design[,Xs[3]],0.16,0.33)
Exp.design[,Xs[4]]<-qunif(Exp.design[,Xs[4]],0.013,0.027)
Exp.design[,Xs[5]]<-qunif(Exp.design[,Xs[5]],0.013,0.027)
Exp.design[,Xs[6]]<-qunif(Exp.design[,Xs[6]],0.013,0.027)
Exp.design[,Xs[7]]<-qunif(Exp.design[,Xs[7]],0.013,0.027)
Exp.design[,Xs[8]]<-qunif(Exp.design[,Xs[8]],0.0006,0.002)
Exp.design[,Xs[9]]<-qunif(Exp.design[,Xs[9]],4.9,6.2)

summary(Exp.design)

#add run.id 
Exp.design$Run.ID<-1:nrow(Exp.design)


#=============================================================================================
#Define function to run ediam accross the experimental design
#=============================================================================================
ediam.experiment<-function(Exp.design,policies)
  {
   exp.out<-apply(Exp.design,1,function(x){
  #load parameters from Ex.design.table
      params<-c( CO2.Concentration.0 = 382.2461, #
          TimeStep = as.numeric(5.0),#
          EndTime = as.numeric(300), #
          alfa = as.numeric(0.33),
          epsilon = as.numeric(x["epsilon"]), #elasticity of subsitution
          Gamma.re = as.numeric(x["Gamma.re"]), #R&D returns SETs
          k.re = as.numeric(0),#
          Gamma.ce = as.numeric(x["Gamma.ce"]), #R&D returns FETs
          k.ce = as.numeric(0),#
          Eta.re = as.numeric(x["Eta.re"]), #Innovation propensity SETs
          Eta.ce = as.numeric(x["Eta.ce"]), #Innovation propensity FETs
          Nu.re = as.numeric(x["Nu.re"]),  #Transferability SETs
          Nu.ce = as.numeric(x["Nu.ce"]),  #Transferability FETs
          qsi = as.numeric(0.0100539),#
          Delta.S = as.numeric(x["Delta.S"]),
          Delta.Temp.Disaster = as.numeric(6.0),#
          Beta.Delta.Temp = as.numeric(x["Beta.Delta.Temp"]),
          CO2.base = as.numeric( 289.415),
          labor.growth_N = as.numeric(0),# Population growth Advanced Region
          labor.growth_S = as.numeric(0),# Population growth Emerging Region
          lambda.S = as.numeric(0.1443),
          sigma.utility = as.numeric(2.0),
          rho = as.numeric(0.008), #Discount rate
          Yre.0_N = as.numeric(45.55074),
          Yce.0_N = as.numeric(193.2),
          Yre.0_S = as.numeric(27.82166),
          Yce.0_S = as.numeric(257.5463),
          size.factor = as.numeric(4.0),#
          Run.ID = as.numeric(x["Run.ID"]));
  #run the model
  ediam(policies,params)
  })
  #Transform results into a data frame
  exp.out<-do.call("rbind",exp.out)
  return(exp.out)
  }

#=============================================================================================

#=============================================================================================
#Run the experiment for each policy
#=============================================================================================
 exp.out.a1<-ediam.experiment(Exp.design,policies.a1)
 exp.out.a2<-ediam.experiment(Exp.design,policies.a2)
