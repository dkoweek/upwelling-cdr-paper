#Calculate CO2 gradients for microalgae growth models with E_k that does not vary in time and space
#Already using a constant E_k value for macroalgae models so not necessary to run here

#----Load_the_initialized_grid----
upwelling_grid_initial <-
  readRDS(file = str_c(working_data_directory,
                       "upwelling_grid_initial.RDS",
                       sep = "/")) 

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
                                  T = theta,
                                  P = 0,
                                  warn = "n") %>% 
              pull(CO2))
  
}

#----Calculate_biogeochemical_drawdown----

#----Galbraith_and_Martiny_2015----
galbraith_delta_CO2_grid <-
  upwelling_grid_initial %>%
  mutate.(galbraith = map2.(NO3, PO4, galbraith_model)) %>%
  #Maximum potential biogeochemical change
  mutate.(galbraith_dDIC_max = map_dbl.(galbraith, ~ pluck(.x,1)),
          galbraith_dTA_max = map_dbl.(galbraith, ~ pluck(.x,2))) %>%
  #Add in light limitation
  mutate.(galbraith_dDIC = galbraith_dDIC_max * epsilon_micro_E_k_const,
          galbraith_dTA = galbraith_dTA_max * epsilon_micro_E_k_const) %>%
  #Apply changes to DIC and TA and location x,y,z
  mutate.(galbraith_DIC_p = galbraith_dDIC + DIC,
          galbraith_TA_p = galbraith_dTA + TA) %>%
  #Calculate CO2 concentration
  calculate_CO2_p(model = "galbraith") %>%
  #Calculate CO2 gradient
  mutate.(galbraith_delta_CO2 = CO2_ML_xyt - galbraith_CO2_p_xyzt)

saveRDS(galbraith_delta_CO2_grid,
        str_c(working_data_directory,
              "/galbraith_E_k_const_delta_CO2_grid.RDS",
              sep = ""))
rm(galbraith_delta_CO2_grid)

#----Garcia_et_al_2018----

#1st quartile N:P
garcia_q1_delta_CO2_grid <-
  upwelling_grid_initial %>%
  mutate.(garcia_q1 = map2.(NO3, PO4, garcia_model, metric = "q1")) %>%
  #Maximum potential biogeochemical change
  mutate.(garcia_q1_dDIC_max = map_dbl.(garcia_q1, ~ pluck(.x,1)),
          garcia_q1_dTA_max = map_dbl.(garcia_q1, ~ pluck(.x,2))) %>%
  #Add in light limitation
  mutate.(garcia_q1_dDIC = garcia_q1_dDIC_max * epsilon_micro_E_k_const,
          garcia_q1_dTA = garcia_q1_dTA_max * epsilon_micro_E_k_const) %>%
  #Apply changes to DIC and TA and location x,y,z
  mutate.(garcia_q1_DIC_p = garcia_q1_dDIC + DIC,
          garcia_q1_TA_p = garcia_q1_dTA + TA) %>%
  #Calculate CO2 concentration
  calculate_CO2_p(model = "garcia_q1") %>%
  #Calculate CO2 gradient
  mutate.(garcia_q1_delta_CO2 = CO2_ML_xyt - garcia_q1_CO2_p_xyzt)

saveRDS(garcia_q1_delta_CO2_grid,
        str_c(working_data_directory,
              "/garcia_q1_E_k_const_delta_CO2_grid.RDS",
              sep = ""))
rm(garcia_q1_delta_CO2_grid)

#3rd quartile N:P
garcia_q3_delta_CO2_grid <-
  upwelling_grid_initial %>%
  mutate.(garcia_q3 = map2.(NO3, PO4, garcia_model, metric = "q3")) %>%
  #Maximum potential biogeochemical change
  mutate.(garcia_q3_dDIC_max = map_dbl.(garcia_q3, ~ pluck(.x,1)),
          garcia_q3_dTA_max = map_dbl.(garcia_q3, ~ pluck(.x,2))) %>%
  #Add in light limitation
  mutate.(garcia_q3_dDIC = garcia_q3_dDIC_max * epsilon_micro_E_k_const,
          garcia_q3_dTA = garcia_q3_dTA_max * epsilon_micro_E_k_const) %>%
  #Apply changes to DIC and TA and location x,y,z
  mutate.(garcia_q3_DIC_p = garcia_q3_dDIC + DIC,
          garcia_q3_TA_p = garcia_q3_dTA + TA) %>%
  #Calculate CO2 concentration
  calculate_CO2_p(model = "garcia_q3") %>%
  #Calculate CO2 gradient
  mutate.(garcia_q3_delta_CO2 = CO2_ML_xyt - garcia_q3_CO2_p_xyzt)

saveRDS(garcia_q3_delta_CO2_grid,
        str_c(working_data_directory,
              "/garcia_q3_E_k_const_delta_CO2_grid.RDS",
              sep = ""))
rm(garcia_q3_delta_CO2_grid)

#----Redfield_et_al_1934----  
redfield_delta_CO2_grid <- 
  upwelling_grid_initial %>% 
  mutate.(redfield = map2.(NO3, PO4, redfield_model)) %>% 
  #Maximum potential biogeochemical change
  mutate.(redfield_dDIC_max = map_dbl.(redfield, ~ pluck(.x,1)),
          redfield_dTA_max = map_dbl.(redfield, ~ pluck(.x,2))) %>% 
  #Add in light limitation
  mutate.(redfield_dDIC = redfield_dDIC_max * epsilon_micro_E_k_const,
          redfield_dTA = redfield_dTA_max * epsilon_micro_E_k_const) %>% 
  #Apply changes to DIC and TA and location x,y,z
  mutate.(redfield_DIC_p = redfield_dDIC + DIC,
          redfield_TA_p = redfield_dTA + TA) %>% 
  #Calculate CO2 concentration
  calculate_CO2_p(model = "redfield") %>% 
  #Calculate CO2 gradient
  mutate.(redfield_delta_CO2 = CO2_ML_xyt - redfield_CO2_p_xyzt)

saveRDS(redfield_delta_CO2_grid,
        str_c(working_data_directory,
              "/redfield_E_k_const_delta_CO2_grid.RDS",
              sep = ""))
rm(redfield_delta_CO2_grid)

#----Redfield_P_limitation----
redfield_P_limited_delta_CO2_grid <- 
  upwelling_grid_initial %>% 
  mutate.(redfield_P_limited = map2.(NO3, PO4, redfield_P_limited_model)) %>% 
  #Maximum potential biogeochemical change
  mutate.(redfield_P_limited_dDIC_max = map_dbl.(redfield_P_limited, ~ pluck(.x,1)),
          redfield_P_limited_dTA_max = map_dbl.(redfield_P_limited, ~ pluck(.x,2))) %>% 
  #Add in light limitation
  mutate.(redfield_P_limited_dDIC = redfield_P_limited_dDIC_max * epsilon_micro_E_k_const,
          redfield_P_limited_dTA = redfield_P_limited_dTA_max * epsilon_micro_E_k_const) %>% 
  #Apply changes to DIC and TA and location x,y,z
  mutate.(redfield_P_limited_DIC_p = redfield_P_limited_dDIC + DIC,
          redfield_P_limited_TA_p = redfield_P_limited_dTA + TA) %>% 
  #Calculate CO2 concentration
  calculate_CO2_p(model = "redfield_P_limited") %>% 
  #Calculate CO2 gradient
  mutate.(redfield_P_limited_delta_CO2 = CO2_ML_xyt - redfield_P_limited_CO2_p_xyzt)

saveRDS(redfield_P_limited_delta_CO2_grid,
        str_c(working_data_directory,
              "/redfield_P_limited_E_k_const_delta_CO2_grid.RDS",
              sep = ""))
rm(redfield_P_limited_delta_CO2_grid)