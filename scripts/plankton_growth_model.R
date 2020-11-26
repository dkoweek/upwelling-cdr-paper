#----Galbraith_Martiny_2015----
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

galbraith_biogeochemical_model <- function(NO3, PO4) {
  
    
    N_P_hat <-
      N_P_expected(NO3 = NO3,
                   PO4 = PO4)
    
    P_limited <- 
      if_else((NO3 / PO4) >= N_P_hat, 1, 0) 
      
      #P-limited
      P_C_ratio <-
        P_C(PO4)
      
      delta_DIC_bio_P_limited <- 
        PO4 / 
        P_C_ratio
      
      delta_N_bio_P_limited <- 
        PO4 * N_P_hat
      
      
    N_limited <- 
      if_else((NO3 / PO4) >= N_P_hat, 0, 1)
        
      #N-limited
      N_C_ratio <-
        N_C(NO3)
      
      delta_DIC_bio_N_limited <- 
        NO3 / 
        N_C_ratio
      
      delta_N_bio_N_limited <- 
        NO3
    
    #Combine results    
    delta_DIC_bio <- 
      -1* (
        (P_limited * delta_DIC_bio_P_limited) + 
          (N_limited * delta_DIC_bio_N_limited)
      )
      
    delta_TA_bio <- 
      (
        (P_limited * delta_N_bio_P_limited) + 
          (N_limited * delta_N_bio_N_limited)
      )
      
    
      return(list(delta_DIC_bio = delta_DIC_bio,
                  delta_TA_bio = delta_TA_bio))
      
}

#----Garcia_et_al_2018----

garcia_df <- 
  read_csv(file = str_c(input_data_directory,
                                   "Garcia_et_al_T1.csv",
                                   sep = "/"),
                      col_types = 
                        cols(
                        Class = col_character(),
                        Species = col_character(),
                        Isolate = col_character(),
                        `Temp (C)` = col_double(),
                        `p (d-1)` = col_double(),
                        s.d. = col_double(),
                        `max (d-1)` = col_double(),
                        s.d._1 = col_double(),
                        `p: fmax` = col_double(),
                        `Cell volume` = col_character(),
                        s.d._2 = col_number(),
                        `C:N` = col_double(),
                        s.d._3 = col_double(),
                        `N:P` = col_double(),
                        s.d._4 = col_double(),
                        `C:P` = col_double(),
                        s.d._5 = col_double()
                        )
                      ) %>% 
  rename(C_N = `C:N`,
         N_P = `N:P`,
         C_P = `C:P`)  %>% 
  slice(-1)

#Arrange data frame by N:P ratio
N_P_ecdf <- 
  ecdf(garcia_df$N_P)

garcia_df <-
  garcia_df %>% 
  mutate(N_P_quantile = N_P_ecdf(N_P)) %>% 
  arrange(N_P_quantile)



garcia_biogeochemical_model <- function(NO3, PO4, percentile = 0.5) {
  
  #Grab the N_P ratio matching the input percentile
  data <- 
    garcia_df %>% 
    mutate(quantile_diff = abs(percentile - N_P_quantile)) %>% 
    slice(which.min(quantile_diff))
  
  N_P_hat <- 
    data %>%
    pull(N_P)
  
  #Calculate the DIC and TA changes from both N and P limitation
  P_limited <-
    if_else((NO3 / PO4) >= N_P_hat, 1, 0)
  
    #P-limited
    delta_DIC_bio_P_limited <- 
      data %>% 
      pull(C_P) %>% 
      `*`(PO4)
    
    delta_N_bio_P_limited <- 
      data %>% 
      pull(N_P) %>% 
      `*`(PO4)
  
  N_limited <- 
    if_else((NO3 / PO4) >= N_P_hat, 0, 1)
  
    #N-limited
    delta_DIC_bio_N_limited <- 
    data %>% 
    pull(C_N) %>% 
    `*`(NO3)
    
    delta_N_bio_N_limited <- 
    NO3
  
  #Combine N and P limitation calculations in vectorized fashion
  #Ultimate result is DIC or TA modification due to only N OR P limitation, not both
  delta_DIC_bio <- 
    -1* (
        (P_limited * delta_DIC_bio_P_limited) + 
          (N_limited * delta_DIC_bio_N_limited)
      )
    
  delta_TA_bio <- 
    (
        (P_limited * delta_N_bio_P_limited) + 
          (N_limited * delta_N_bio_N_limited)
    )
    
    
  return(list(delta_DIC_bio = delta_DIC_bio,
              delta_TA_bio = delta_TA_bio))
  
}


