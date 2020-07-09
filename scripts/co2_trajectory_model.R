#Functions for modeling evolution of mixed layer CO2 concentration

#Determine the initial mixed layer temperature, salinity, TA, and DIC
initialize_ML_concentrations <- function(data_set) {
  
  surf_field_0 <- 
    data_set %>% 
    group_by(lat, lon) %>% 
    filter(depth_m <= MLD) %>% 
    summarize(T_ML_0 = mean(T),
              S_ML_0 = mean(S),
              TA_ML_0 = mean(TA),
              DIC_ML_0 = mean(DIC),
              .groups = "keep") %>% 
    ungroup()
  
  data_set %>% 
    left_join(.,
              surf_field_0,
              by = c("lon", "lat")) %>% 
    return()
  
}

biological_uptake_potential <- function(data_set) {
  
  data_set %>% 
    #Calculate changes to DIC and TA due to biological productivity
    mutate(bio_uptake = map2(NO3, PO4, plankton_biogeochem_model)) %>% 
    mutate(delta_DIC_bio = map_dbl(bio_uptake, 1, pluck),
           delta_TA_bio = map_dbl(bio_uptake, 2, pluck)) %>% 
    mutate(DIC_pot = DIC + delta_DIC_bio,
           TA_pot = TA + delta_TA_bio) %>% 
    select(-bio_uptake) %>% 
    return()
  
  
}

pumping_fraction <- function(n_pipes = 1, Q_dot = 1, area, MLD) {
  
  #n _pipes= number of pipes per 1x1 grid cell
  #m_dot = flow rate per pipe (m^3/s)
  #area = grid cell area = f(latitude) (m^2)
  #MLD = mixed layer depth of grid cell (m)
  
  delta_time <- 3.154e7 #seconds/year
  
  f <- 
    (n_pipes * Q_dot) / 
    (area * MLD) *
    delta_time
  
  return(f)
  
  
}

mixed_layer_pumping_model <- function(n_years, n_pipes, Q_dot, data_set) {
  
  #Calculate mixing fraction and add pumping information to data frame
  data_set <- 
    data_set %>% 
    mutate(n_pipes = n_pipes,
           Q_dot = Q_dot,
           f = pumping_fraction(n_pipes = n_pipes,
                                Q_dot = Q_dot,
                                area = area,
                                MLD = MLD),
           n_years = n_years)
  
  #For each year, for each variable, run the mixing model
  for (i in 1:n_years) {
    for (j in c("T", "S", "TA", "DIC")) {
      
      
      variable_year_name <- 
        str_c(j,
              "_ML_",
              i,
              sep = "")
      
      variable_previous_year_name <- 
        str_c(j,
              "_ML_",
              i - 1,
              sep = "")
      
      #If DIC or TA, need to mix in bottom water concentration after biological drawdown
      if(j %in% c("T", "S")) {
        mix_in_variable_name <- j
      } else {
        mix_in_variable_name <- 
          str_c(j, "_pot", sep="")
      }
        
      
      data_set <- 
        data_set %>% 
        mutate(!!variable_year_name := 
                 ((f * !!as.name(mix_in_variable_name)) + 
                    ((1 - f) * !!as.name(variable_previous_year_name))))
      
    }
  }
  
  return(data_set) 
  
}


CO2_from_mixing_model <- function(data_set, n_years) {
  
  for (i in 0:n_years) {
    
    CO2_year_name <- 
      str_c("CO2_ML_",
            i,
            sep = "")
    
    data_set <- 
      data_set %>% 
      mutate(!!CO2_year_name :=
               carb(flag = 15,
                    var1 = !!as.name(str_c("TA_ML_",i,sep="")) / 1e6,
                    var2 = !!as.name(str_c("DIC_ML_",i,sep="")) / 1e6,
                    T = !!as.name(str_c("T_ML_",i,sep="")),
                    S = !!as.name(str_c("S_ML_",i,sep="")),
                    warn = "n") %>% 
               pull(CO2) %>% 
               "*" (1e6))
    
  }
  
  return(data_set)
  
}