#Model for mixed layer CO2 evolution

initialize_ML_CO2 <- function(data_set) {
  
  
  CO2_surf_field_0 <- 
    data_set %>% 
    group_by(lat, lon) %>% 
    filter(depth_m <= MLD) %>% 
    mutate(CO2 = carb(flag = 15,
                      var1 = TA/1e6,
                      var2 = DIC/1e6,
                      S = S,
                      T = T,
                      Pt = PO4/1e6,
                      warn = "n") %>% 
                  pull(CO2),
                  CO2 = CO2 * 1e6) %>%  #umol/kg
    summarize(CO2_ML_0 = mean(CO2),
              .groups = "keep") %>% 
    ungroup()
  
  data_set %>% 
    left_join(.,
              CO2_surf_field_0,
              by = c("lon", "lat")) %>% 
    return()
  


}

pumped_CO2_concentration <- function(data_set) {
  
  data_set %>% 
    #Calculate changes to DIC and TA
    mutate(bio_uptake = map2(NO3, PO4, plankton_biogeochem_model)) %>% 
    mutate(delta_DIC_bio = map_dbl(bio_uptake, 1, pluck),
           delta_TA_bio = map_dbl(bio_uptake, 2, pluck)) %>% 
    #Calculate pumped CO2 concentration after biological drawdown...
    #...assume adiabatic transfer
    mutate(CO2 = carb(flag = 15,
                      var1 = (TA + delta_TA_bio) / 1e6,
                      var2 = (DIC + delta_DIC_bio) / 1e6,
                      S = S,
                      T = T,
                      Pt = PO4 / 1e6,
                      warn = "n") %>%
             pull(CO2) %>%
             "*" (1e6)) %>% #umol/kg
    select(-bio_uptake) %>% 
    return()
  
  
}

pumping_fraction <- function(n_pipes = 1, Q_dot = 1, area, MLD) {
  
  #n _pipes= number of pipes per 1x1 grid cell
  #m_dot = flow rate per pipe (m^3/s)
  #rho = seawater density leaving pipe (kg/m^3)
  #area = grid cell area = f(latitude) (m^2)
  #MLD = mixed layer depth of grid cell (m)
  
  delta_time <- 3.154e7 #seconds/year
  
  f <- 
    (n_pipes * Q_dot) / 
    (area * MLD) *
    delta_time
  
  return(f)
  
  
}

CO2_trajectory <- function(n_years, data_set,...) {
  
  
  data_set <- 
    data_set %>% 
    mutate(f = pumping_fraction(area = area,
                                MLD = MLD))
  
  for (i in 1:n_years) {
    
     
    CO2_year_name <- 
      str_c("CO2_ML_",
            i,
            sep = "")
    
    CO2_previous_year_name <- 
      str_c("CO2_ML_",
            i - 1,
            sep = "")
    
    data_set <- 
      data_set %>% 
      mutate(!!CO2_year_name := ((f * CO2) + ((1 - f) * !!as.name(CO2_previous_year_name))))
    
  }
  
  return(data_set) 
  
}