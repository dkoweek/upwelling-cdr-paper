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
    select(-bio_uptake) %>% 
    return()
  
  
}

pumping_rate_constant <- function(n_pipes = 1, 
                                  rho_z,
                                  Q_dot = 1, 
                                  area, 
                                  MLD,
                                  T_ML,
                                  S_ML) {
  
  #Return the rate constant (time^-1) of pumping
  
  #n_pipes= number of pipes per 1x1 grid cell
  #rho_z = density of DOW at depth z when brought to the surface (kg/m^3) 
  #Q_dot = volumetric flow rate of pumping (m^3/s)
  #area = grid cell area = f(latitude) (m^2)
  #MLD = mixed layer depth of grid cell (m)
  #T_ML = mixed layer temperature (deg C)
  #S_ML = mixed layer salinity
  
  rho_ML <-
    swRho(salinity = S_ML,
          temperature = T_ML,
          pressure = 0)
  
  f <-
    (n_pipes * rho_z * Q_dot) / 
    (area * MLD * rho_ML)
  
  return(f)
  
  
}

air_sea_gas_flux <- function(k = 2.4,
                             pCO2_atm = 410,
                             MLD,
                             T_ML,
                             S_ML,
                             TA_ML,
                             DIC_ML) {
  
  #k = gas transfer velocity (m d^-1)
  #pCO2_atm = atmospheric partial pressure of CO2 (uatm)
  #T,S,TA,DIC_ML = variables in the mixed layer
  
  pCO2_sw <- 
    carb(flag = 15,
         var1 = TA_ML / 1e6,
         var2 = DIC_ML / 1e6,
         S = S_ML,
         T = T_ML,
         Patm = 1,
         P = 0,
         warn = "n") %>% 
    pull(pCO2)
  
  K_0 <- 
    K0(S = S_ML,
       T = T_ML,
       P = 0,
       warn = "n") %>% #mol/kg/atm= umol/kg/uatm
    as.numeric()
  
  J <-
    (k / MLD) * K_0 * (pCO2_atm - pCO2_sw)
  
  return(J)
    
  
}


mass_balances <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    
    f <- #rate constant for pumping (time^-1)
      pumping_rate_constant(n_pipes = n_pipes,
                            rho_z = rho_z,
                            Q_dot = Q_dot,
                            area = area,
                            MLD = MLD,
                            T_ML = T_ML,
                            S_ML = S_ML)
    
    #Air-sea gas_flux (umo/kg/time)
    J <- 
      air_sea_gas_flux(k = k,
                       pCO2_atm = pCO2_atm,
                       MLD = MLD,
                       T_ML = T_ML,
                       S_ML = S_ML,
                       TA_ML = TA_ML,
                       DIC_ML = DIC_ML)
    
    dT_ML <- 
      f * (T_z - T_ML)
    
    dS_ML <-
      f * (S_z - S_ML)
    
    dTA_ML <- 
      f * ((TA_z - TA_ML) + delta_TA_bio)
    
    dDIC_ML <- 
      (f * ((DIC_z - DIC_ML) + delta_DIC_bio)) + J
    
    
    return(list(c(dT_ML, dS_ML, dTA_ML, dDIC_ML), J = J))
    
  
  })
}






mixed_layer_pumping_model <- function(n_years, 
                                      n_pipes, 
                                      Q_dot, 
                                      k, 
                                      pCO2_atm, 
                                      data_set) {
  
  times <-
    seq(0,
        n_years * 365,
        length.out = 52 * n_years) #weekly resolution
  
  model_x_y_z <- list()

  for (i in 1:nrow(data_set)) {
    
      model_parameters <- 
        c(n_pipes = n_pipes,
          rho_z = data_set[["rho"]][i],
          Q_dot = Q_dot,
          area = data_set[["area"]][i],
          MLD = data_set[["MLD"]][i],
          k = k,
          pCO2_atm = pCO2_atm,
          delta_DIC_bio = data_set[["delta_TA_bio"]][i],
          delta_TA_bio = data_set[["delta_DIC_bio"]][i],
          T_z = data_set[["T"]][i],
          S_z = data_set[["S"]][i],
          TA_z = data_set[["TA"]][i],
          DIC_z = data_set[["DIC"]][i])
      
      ICs <-
        c(T_ML = data_set[["T_ML_0"]][i],
          S_ML = data_set[["S_ML_0"]][i],
          TA_ML = data_set[["TA_ML_0"]][i],
          DIC_ML = data_set[["DIC_ML_0"]][i])
      
      
      #Run the ODEs
      model_x_y_z[[i]] <-
        ode(y = ICs,
            times = times,
            func = mass_balances,
            parms = model_parameters) %>% 
        as.data.frame()
      
  }
  
 return(model_x_y_z) 
  
}  