
##**********************************************
##   Scenario Simulation Function           ####
##**********************************************


Scenario.Simulation.function <- function(
  post_FuEm_FuUn_ratio,
  post_FuEm_FuUn_damp,
  post_EmUn_ratio,
  post_EmUn_UnEm_damp
){
  
  ## source multistate simulation function
  source("MultiState_Simulation_function.R")
  
  ##Initialise the parameters.
  endFur <- 100
  
  peri_EmFu_ratio <- 0.39
  peri_EmFu_FuEm_damp <- 40
  peri_EmUn_ratio <-0.041
  peri_EmUn_UnEm_damp <- 40
  
  ##Define the fidelity. This is the number of slices to divide each day into for the numerical solving of the differential equations.
  fidelity <- 1
  # not much difference with the initial fidelity parameter of 10
  
  ## This is the number of people in the population being simulated.
  ## In this case it represents the economically active population of the UK, aged 16+
  pop <- 34326606
  
  ## Run time parameters
  theStart <- as.Date("2020-06-22")
  
  ##Estimate the simluation starting parameters from the observed data.
  Em <- 23891075/pop
  Fu <- 9100000/pop
  Un <- 1335532/pop
  
  ##Create the init array for passing parameter values to the simulation.
  inits <- c(
    Em=Em,
    Fu=Fu,
    Un=Un
  )
  names(inits)<- c("Em","Fu","Un")
  
  ## Choose how long to project the simulations.
  simTime <- 600
  #simTime <- 1000
  
  ## Simulate
  bestFitResult <- MultiState.Simulation.function(inits,
                                                  simTime,
                                                  pop,
                                                  fidelity,
                                                  theStart,
                                                  endFur,
                                                  peri_EmFu_ratio,
                                                  peri_EmFu_FuEm_damp,
                                                  peri_EmUn_ratio,
                                                  peri_EmUn_UnEm_damp,
                                                  post_FuEm_FuUn_ratio,
                                                  post_FuEm_FuUn_damp,
                                                  post_EmUn_ratio,
                                                  post_EmUn_UnEm_damp
  )
  
  colnames(bestFitResult) <- c("Employed", "Furlough", "Unemployed")
  bestFitResult$date <- as.Date(rownames(bestFitResult),"%Y-%m-%d")
  bestFitResult$Unemployment_Rate <-  bestFitResult$Unemployed/pop*100
  bestFitResult$Employed <- bestFitResult$Employed/1000000
  bestFitResult$Furlough <- bestFitResult$Furlough/1000000
  bestFitResult$Unemployed <- bestFitResult$Unemployed/1000000
  
  return(bestFitResult)
  
}