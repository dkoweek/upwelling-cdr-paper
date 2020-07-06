#Plankton growth models from:
#Galbraith, Eric D., and Adam C. Martiny. 
#"A simple nutrient-dependence mechanism for predicting the stoichiometry...
#...of marine ecosystems."  Proceedings of the National Academy of Sciences 
#112.27 (2015): 8199-8204.

N_C <- function(NO3) {
  
  nitrogen_carbon_ratio <- #per mil
    125 + 
    (
      (30 * NO3) /
        (0.32 + NO3)
    )
  
  nitrogen_carbon_ratio <- 
    (nitrogen_carbon_ratio / 1000)
  
  return(nitrogen_carbon_ratio)
  
}
  
  
P_C <- function(PO4) {
  
  phosphorus_carbon_ratio <- 
    4.8 + 
    7.3 * PO4
  
  phosphorus_carbon_ratio <- 
    (phosphorus_carbon_ratio / 1000)
  
  return(phosphorus_carbon_ratio)
  
}
  

N_P_expected <- function(NO3, PO4) {
  
  N_to_C <- 
    N_C(NO3)
  
  P_to_C <- 
    P_C(PO4)
  
  N_P <- 
    (N_to_C / P_to_C)
  
  return(N_P)
  
} 


plankton_biogeochem_model <- function(NO3, PO4) {
  
  if(is.na(NO3) | is.na(PO4)) {
    
    return(list(delta_DIC_bio = NA,
                delta_TA_bio = NA))
    
  } else {
    
    N_P_hat <-
      N_P_expected(NO3 = NO3,
                   PO4 = PO4)
    
    if ((NO3 / PO4) >= N_P_hat) {
      
      #P-limited
      P_C_ratio <-
        P_C(PO4)
      
      delta_DIC_bio <- 
        PO4 / 
        P_C_ratio
      
      delta_N_bio <- 
        PO4 * N_P_hat
      
      return(list(delta_DIC_bio = -delta_DIC_bio,
                  delta_TA_bio = delta_N_bio))
      
    } else {
      
      #N-limited
      N_C_ratio <-
        N_C(NO3)
      
      delta_DIC_bio <- 
        NO3 / 
        N_C_ratio
      
      return(list(delta_DIC_bio = -delta_DIC_bio,
                  delta_TA_bio = NO3))
      
    }
    
  }

}