#----Load_the_initialized_grid----
delta_CO2_grid_initial <-
  readRDS(file = str_c(working_data_directory,
                       "delta_CO2_grid_initial.RDS",
                       sep = "/")) %>% 
  filter(depth_m > MLD_max) #only calculate from depths deeper than the winter mixed layer

#----Custom_function_to_calculate_CO2_concentration_across_columns----
calculate_CO2_p <- function(data, model)  {
  output_name <- 
    str_c(model,"_CO2_p_xyzt",sep="")
  
  TA_input <- 
    str_c(model, "_TA_p", sep = "")
  
  DIC_input <- 
    str_c(model, "_DIC_p", sep = "")
  
  data %>% 
    mutate.(!!output_name := carb(flag = 15,
                                  var1 = !!as.name(TA_input) / 1e6,
                                  var2 = !!as.name(DIC_input) / 1e6,
                                  S = S,
                                  T = T,
                                  P = 0,
                                  warn = "n") %>% 
                        pull(CO2))
  
}

#----Calculate_biogeochemical_drawdown----

galbraith_delta_CO2_grid <- 
  delta_CO2_grid_initial %>% 
  mutate.(galbraith = map2.(NO3, PO4, galbraith_model)) %>% 
  #Maximum potential biogeochemical change
  mutate.(galbraith_dDIC_max = map_dbl.(galbraith, ~ pluck(.x,1)),
          galbraith_dTA_max = map_dbl.(galbraith, ~ pluck(.x,2))) %>% 
  #Add in light limitation
  mutate.(galbraith_dDIC = galbraith_dDIC_max * epsilon_micro,
          galbraith_dTA = galbraith_dTA_max * epsilon_micro) %>% 
  #Apply changes to DIC and TA and location x,y,z
  mutate.(galbraith_DIC_p = galbraith_dDIC + DIC,
          galbraith_TA_p = galbraith_dTA + TA) %>% 
  #Calculate CO2 concentration
  calculate_CO2_p(model = "galbraith") %>% 
  #Calculate CO2 gradient
  mutate.(galbraith_delta_CO2 = CO2_ML_xyt - galbraith_CO2_p_xyzt)

saveRDS(galbraith_delta_CO2_grid,
        str_c(working_data_directory,
              "/galbraith_delta_CO2_grid.RDS",
              sep = ""))
rm(galbraith_delta_CO2_grid)
  
garcia_delta_CO2_grid <- 
  delta_CO2_grid_initial %>% 
  mutate.(garcia = map2.(NO3, PO4, garcia_model, metric = "median")) %>% 
  #Maximum potential biogeochemical change
  mutate.(garcia_dDIC_max = map_dbl.(garcia, ~ pluck(.x,1)),
          garcia_dTA_max = map_dbl.(garcia, ~ pluck(.x,2))) %>% 
  #Add in light limitation
  mutate.(garcia_dDIC = garcia_dDIC_max * epsilon_micro,
          garcia_dTA = garcia_dTA_max * epsilon_micro) %>% 
  #Apply changes to DIC and TA and location x,y,z
  mutate.(garcia_DIC_p = garcia_dDIC + DIC,
          garcia_TA_p = garcia_dTA + TA) %>% 
  #Calculate CO2 concentration
  calculate_CO2_p(model = "garcia") %>% 
  #Calculate CO2 gradient
  mutate.(garcia_delta_CO2 = CO2_ML_xyt - garcia_CO2_p_xyzt)

saveRDS(garcia_delta_CO2_grid,
        str_c(working_data_directory,
              "/garcia_delta_CO2_grid.RDS",
              sep = ""))
rm(garcia_delta_CO2_grid)
  
redfield_delta_CO2_grid <- 
  delta_CO2_grid_initial %>% 
  mutate.(redfield = map2.(NO3, PO4, redfield_model)) %>% 
  #Maximum potential biogeochemical change
  mutate.(redfield_dDIC_max = map_dbl.(redfield, ~ pluck(.x,1)),
          redfield_dTA_max = map_dbl.(redfield, ~ pluck(.x,2))) %>% 
  #Add in light limitation
  mutate.(redfield_dDIC = redfield_dDIC_max * epsilon_micro,
          redfield_dTA = redfield_dTA_max * epsilon_micro) %>% 
  #Apply changes to DIC and TA and location x,y,z
  mutate.(redfield_DIC_p = redfield_dDIC + DIC,
          redfield_TA_p = redfield_dTA + TA) %>% 
  #Calculate CO2 concentration
  calculate_CO2_p(model = "redfield") %>% 
  #Calculate CO2 gradient
  mutate.(redfield_delta_CO2 = CO2_ML_xyt - redfield_CO2_p_xyzt)

saveRDS(redfield_delta_CO2_grid,
        str_c(working_data_directory,
              "/redfield_delta_CO2_grid.RDS",
              sep = ""))
rm(redfield_delta_CO2_grid)

atkinson_delta_CO2_grid <- 
  delta_CO2_grid_initial %>% 
  mutate.(atkinson = map2.(NO3, PO4, atkinson_model, metric = "median")) %>% 
  #Maximum potential biogeochemical change
  mutate.(atkinson_dDIC_max = map_dbl.(atkinson, ~ pluck(.x,1)),
          atkinson_dTA_max = map_dbl.(atkinson, ~ pluck(.x,2))) %>% 
  #Add in light limitation
  mutate.(atkinson_dDIC = atkinson_dDIC_max * epsilon_macro,
          atkinson_dTA = atkinson_dTA_max * epsilon_macro) %>% 
  #Apply changes to DIC and TA and location x,y,z
  mutate.(atkinson_DIC_p = atkinson_dDIC + DIC,
          atkinson_TA_p = atkinson_dTA + TA) %>% 
  #Calculate CO2 concentration
  calculate_CO2_p(model = "atkinson") %>% 
  #Calculate CO2 gradient
  mutate.(atkinson_delta_CO2 = CO2_ML_xyt - atkinson_CO2_p_xyzt)

saveRDS(atkinson_delta_CO2_grid,
        str_c(working_data_directory,
              "/atkinson_delta_CO2_grid.RDS",
              sep = ""))
rm(atkinson_delta_CO2_grid)