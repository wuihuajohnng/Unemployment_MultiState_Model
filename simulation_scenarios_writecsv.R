##***************************************************************
##                                                           ****
##   Simulate fixed scenarios and write results to csv       ####
##                                                           ****
##***************************************************************

##***************************
##  ~~ 1.2 Functions     ####
##***************************

##****************************************************
##   ~~ 1.2.1 Scenario.Simulation.function        ####
##****************************************************

source("Scenario_Simulation_function.R")

##**********************************************
##   ~~ 1.2.2 ScenarioParameters Funcrion   ####
##**********************************************

ScenarioParameters.function <- function(
  scenario_option
){
  if (scenario_option == "scenario_1"){
    post_FuEm_FuUn_ratio <- 1.75
    post_FuEm_FuUn_damp <- 50
    post_EmUn_ratio <- 0.03
    post_EmUn_UnEm_damp <- 1000
    scenario <- "01. OBR Central"
    
  } else if (scenario_option == "scenario_2"){
    post_FuEm_FuUn_ratio <- 1
    post_FuEm_FuUn_damp <- 100
    post_EmUn_ratio <- 0.041
    post_EmUn_UnEm_damp <- 100
    scenario <- "02. OBR Upside"
    
  } else if (scenario_option == "scenario_3"){
    post_FuEm_FuUn_ratio <- 1.5
    post_FuEm_FuUn_damp <- 70
    post_EmUn_ratio <- 0.04
    post_EmUn_UnEm_damp <- 1000
    scenario <- "03. OBR Downside"
    
  } else if (scenario_option == "scenario_4"){
    post_FuEm_FuUn_ratio <- 3
    post_FuEm_FuUn_damp <- 50
    post_EmUn_ratio <- 0.041
    post_EmUn_UnEm_damp <- 300
    scenario <- "04. Bank of England (May)"
    
  } else if (scenario_option == "scenario_5"){
    post_FuEm_FuUn_ratio <- 5
    post_FuEm_FuUn_damp <- 50
    post_EmUn_ratio <- 0.041
    post_EmUn_UnEm_damp <- 500
    scenario <- "05. Bank of England (August)"
    
  } else if (scenario_option == "scenario_6"){
    post_FuEm_FuUn_ratio <- 6
    post_FuEm_FuUn_damp <- 50
    post_EmUn_ratio <- 0.053
    post_EmUn_UnEm_damp <- 1000
    scenario <- "06. Other forecasters' expectations - Central"
    
  } else if (scenario_option == "scenario_7"){
    post_FuEm_FuUn_ratio <- 6
    post_FuEm_FuUn_damp <- 600
    post_EmUn_ratio <- 0.045
    post_EmUn_UnEm_damp <- 400
    scenario <- "07. Other forecasters' expectations - Upside"
    
  } else if (scenario_option == "scenario_8"){
    post_FuEm_FuUn_ratio <- 2.5
    post_FuEm_FuUn_damp <- 50
    post_EmUn_ratio <- 0.07
    post_EmUn_UnEm_damp <- 400
    scenario <- "08. Other forecasters' expectations - Downside"
    
  }
  
  para_vec <- c(post_FuEm_FuUn_ratio, 
                post_FuEm_FuUn_damp,
                post_EmUn_ratio,
                post_EmUn_UnEm_damp,
                scenario)
  
  return(para_vec)
  
}  


##*******************************************************
##                                                   ****
##   Scenarios                                       ####
##                                                   ****
##*******************************************************

## initiate empty data frame
simTime <- 600
DF_sim_all <-  data.frame(matrix(ncol=7,nrow=0))
colnames(DF_sim_all) <-
  c("Employed","Furlough","Unemployed","date", "Unemployment_Rate", "scenario_number", "scenario")

## run all scenarios
for (i in 1:8){
  
  para_vec <- ScenarioParameters.function(scenario_option = paste0("scenario_",i))
  post_FuEm_FuUn_ratio <- as.numeric(para_vec[1])
  post_FuEm_FuUn_damp <- as.numeric(para_vec[2])
  post_EmUn_ratio <- as.numeric(para_vec[3])
  post_EmUn_UnEm_damp <- as.numeric(para_vec[4])
  scenario <- as.character(para_vec[5])
  
  DF_sim <- Scenario.Simulation.function(
    post_FuEm_FuUn_ratio,
    post_FuEm_FuUn_damp,
    post_EmUn_ratio,
    post_EmUn_UnEm_damp
  )
  
  DF_sim$scenario_number <- paste0("scenario_",i)
  DF_sim$scenario <- scenario
  
  DF_sim_all <- rbind(DF_sim_all, DF_sim)
  
}

##*******************************************************
##                                                   ****
##   Save CSV                                        ####
##                                                   ****
##*******************************************************

write.csv(DF_sim_all, "DF_simulation_scenarios.csv",row.names = FALSE)

