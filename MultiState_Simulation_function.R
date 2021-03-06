
##****************************************************
##    MultiState.Simulation.function              ####
##****************************************************

## The simulation function
## inits is the list of starting values for the compartments,
## in the order Em,Fu,Un.
## simTime is the number of days the simulation should run for.
## pop is the number of people in the population.
## fidelity is the number of slices a day is divided into for numerical estimation of teh integrals
## theStart is the date the simulation begins. [It needs to match the date of the first day of observed data.]
## endFur is the day into the simulation that the furlough scheme ends, and represents the switching date between the two sets of partial equations.
## pre params are those that relate to the period during furlough.
## post params are those that relate to the period after the furlough scheme ends.

MultiState.Simulation.function <- function(inits,
                                           simTime,
                                           pop,
                                           fidelity,
                                           theStart,
                                           endFur,
                                           pre_EmFu_ratio,
                                           pre_EmFu_FuEm_damp,
                                           pre_EmUn_ratio,
                                           pre_EmUn_UnEm_damp,
                                           post_FuEm_FuUn_ratio,
                                           post_FuEm_FuUn_damp,
                                           post_EmUn_ratio,
                                           post_EmUn_UnEm_damp
){
  ## Prepare the output data structure by copying the inits vector.
  results <-
    data.frame(matrix(
      rep(0,length(inits)*simTime),
      ncol=length(inits),
      nrow=simTime
    ))
  colnames(results) <-
    c("Em","Fu","Un")
  ## E is the employed fraction.
  results[1,"Em"]<-inits["Em"]
  ## F is the furloughed fraction.
  results[1,"Fu"]<-inits["Fu"]
  ## U is the unemployed fraction.
  results[1,"Un"]<-inits["Un"]
  
  ## Now step through one day at a time to our time horizon 'simTime'.
  #print(paste("simTime:",simTime))
  
  for (i in 1:(simTime-1)){
    
    ## Fetch the existing compartment values.
    Emz <-results[i, "Em"]
    Fuz <-results[i, "Fu"]
    Unz <-results[i, "Un"]
    
    ## Cycle through the (fidelity) slices of a day used to numerically estimate the integrals.
    
    for (j in 1:fidelity){
      ##Here detect whether we are pre or post furlough.
      if (i<endFur){
        ##Proceed with pre-furlough changes.
        ## First calculate compartment transfers.
        Em_Fu <- (Emz*(pre_EmFu_ratio/pre_EmFu_FuEm_damp)) / fidelity
        Em_Un <- (Emz*(pre_EmUn_ratio/pre_EmUn_UnEm_damp)) / fidelity
        Un_Em <- (Unz*(1/pre_EmUn_UnEm_damp)) / fidelity
        Fu_Em <- (Fuz*(1/pre_EmFu_FuEm_damp)) / fidelity
        Fu_Un <- 0
        Un_Fu <- 0
        ## Then combine these to calculate compartment changes.
        Emz <- Emz - Em_Un - Em_Fu + Fu_Em + Un_Em
        Fuz <- Fuz - Fu_Em - Fu_Un + Em_Fu + Un_Fu
        Unz <- Unz - Un_Em - Un_Fu + Em_Un + Fu_Un                               
      }
      else{
        ##proceed with post-furlough changes.
        ## First calculate compartment transfers.
        Em_Fu <- 0
        Em_Un <- (Emz*(post_EmUn_ratio/post_EmUn_UnEm_damp)) / fidelity
        Un_Em <- (Unz*(1/post_EmUn_UnEm_damp)) / fidelity
        Fu_Em <- (Fuz*(post_FuEm_FuUn_ratio/post_FuEm_FuUn_damp)) / fidelity
        Fu_Un <- (Fuz*(1/post_FuEm_FuUn_damp)) / fidelity
        Un_Fu <- 0
        ## Then combine these to calculate compartment changes.
        Emz <- Emz - Em_Un - Em_Fu + Fu_Em + Un_Em
        Fuz <- Fuz - Fu_Em - Fu_Un + Em_Fu + Un_Fu
        Unz <- Unz - Un_Em - Un_Fu + Em_Un + Fu_Un
        
      }
      
    }
    
    ## Place the new compartment values into the results object
    results[i+1,"Em"] <-Emz
    results[i+1,"Fu"] <-Fuz
    results[i+1,"Un"] <-Unz
  }
  ## Add the dates against the entries for each day.
  rownames(results) <-as.Date(0:(simTime-1),origin = theStart)
  
  ##The results are calculated as a proportion. Now calculate the number of people.
  results <- results*pop
  
  ##return the results.
  return(results)
}
