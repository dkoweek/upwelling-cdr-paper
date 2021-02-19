#----Load_the_initialized_grid----
readRDS(file = str_c(working_data_directory,
                     "delta_CO2_grid_initial.RDS",
                     sep = "/"))

#----Split_the_grid_into_pieces_to_compute----
num_groups <- 10

grid_sections <- 
  delta_CO2_grid %>% 
  group_by((row_number()-1) %/% (n()/num_groups)) %>%
  nest %>% pull(data)

#----Set_up_parallel_processsing----
plan(multisession)

#----Calculate_biogeochemical_drawdown----
i <- 1 #chunk of grid to compute

grid_sections[[i]] <- 
  grid_sections[[i]] %>%
  #Run models
  mutate(galbraith = future_map2(NO3, PO4, galbraith_model),
         garcia = future_map2(NO3, PO4, garcia_model, metric = "median"),
         redfield = future_map2(NO3, PO4, redfield_model),
         atkinson = future_map2(NO3, PO4, atkinson_model, metric = "median")) %>% 
  #Maximum potential biogeochemical change
  mutate(
         galbraith_dDIC_max = future_map(galbraith, ~ pluck(.x,1)),
         galbraith_dTA_max = future_map(galbraith, ~ pluck(.x,2)),
         garcia_dDIC_max = future_map(garcia, ~ pluck(.x,1)),
         garcia_dTA_max = future_map(garcia, ~ pluck(.x,2)),
         redfield_dDIC_max = future_map(redfield, ~ pluck(.x,1)),
         redfield_dTA_max = future_map(redfield, ~ pluck(.x,2)),
         atkinson_dDIC_max = future_map(atkinson, ~ pluck(.x,1)),
         atkinson_dTA_max = future_map(atkinson, ~ pluck(.x,2)),
         ) %>% 
  #Add in light limitation
  mutate(
         galbraith_dDIC = future_map2(galbraith_dDIC_max, epsilon_micro,  ~ .x * .y),
         galbraith_dTA = future_map2(galbraith_dTA_max, epsilon_micro, ~ .x * .y),
         garcia_dDIC = future_map2(garcia_dDIC_max, epsilon_micro,  ~ .x * .y),
         garcia_dTA = future_map2(garcia_dTA_max, epsilon_micro, ~ .x * .y),
         redfield_dDIC = future_map2(redfield_dDIC_max, epsilon_micro,  ~ .x * .y),
         redfield_dTA = future_map2(redfield_dTA_max, epsilon_micro, ~ .x * .y),
         atkinson_dDIC = future_map2(atkinson_dDIC_max, epsilon_micro,  ~ .x * .y),
         atkinson_dTA = future_map2(atkinson_dTA_max, epsilon_micro, ~ .x * .y)
         ) %>% 
  #Apply changes to DIC and TA and location x,y,z
  mutate(
         galbraith_DIC_p = future_map2(galbraith_dDIC, DIC, ~ .x + .y),
         galbraith_TA_p = future_map2(galbraith_dTA, TA, ~ .x + .y),
         garcia_DIC_p = future_map2(garcia_dDIC, DIC, ~ .x + .y),
         garcia_TA_p = future_map2(garcia_dTA, TA, ~ .x + .y),
         redfield_DIC_p = future_map2(redfield_dDIC, DIC, ~ .x + .y),
         redfield_TA_p = future_map2(redfield_dTA, TA, ~ .x + .y),
         atkinson_DIC_p = future_map2(atkinson_dDIC, DIC, ~ .x + .y),
         atkinson_TA_p = future_map2(atkinson_dTA, TA, ~ .x + .y)
        ) %>% 
  #Drop variables to save memory
  select(-c(galbraith:atkinson_dTA)) %>% 
  #Calculate CO2 concentration
  mutate(galbraith_CO2_p_xyzt = future_pmap(list(galbraith_TA_p, galbraith_DIC_p, S, T),
                                     function(a,b,c,d) carb(flag = 15,
                                                            var1 = a / 1e6,
                                                            var2 = b / 1e6,
                                                            S = c,
                                                            T = d,
                                                            P = 0,
                                                            warn = "n") %>% 
                                                        pull(CO2)),
         garcia_CO2_p_xyzt = future_pmap(list(garcia_TA_p, garcia_DIC_p, S, T),
                                            function(a,b,c,d) carb(flag = 15,
                                                                   var1 = a / 1e6,
                                                                   var2 = b / 1e6,
                                                                   S = c,
                                                                   T = d,
                                                                   P = 0,
                                                                   warn = "n") %>% 
                                              pull(CO2)),
         redfield_CO2_p_xyzt = future_pmap(list(redfield_TA_p, redfield_DIC_p, S, T),
                                            function(a,b,c,d) carb(flag = 15,
                                                                   var1 = a / 1e6,
                                                                   var2 = b / 1e6,
                                                                   S = c,
                                                                   T = d,
                                                                   P = 0,
                                                                   warn = "n") %>% 
                                              pull(CO2)),
         atkinson_CO2_p_xyzt = future_pmap(list(atkinson_TA_p, atkinson_DIC_p, S, T),
                                            function(a,b,c,d) carb(flag = 15,
                                                                   var1 = a / 1e6,
                                                                   var2 = b / 1e6,
                                                                   S = c,
                                                                   T = d,
                                                                   P = 0,
                                                                   warn = "n") %>% 
                                              pull(CO2)))


#----Calculate_CO2_gradient----
grid_sections[[i]] <- 
  grid_sections[[i]] %>%
  mutate(galbraith_delta_CO2 = future_map2(CO2_ML_xyt, galbraith_CO2_p_xyzt, ~ .x - .y),
         garcia_delta_CO2 = future_map2(CO2_ML_xyt, garcia_CO2_p_xyzt, ~.x - .y),
         redfield_delta_CO2 = future_map2(CO2_ML_xyt, redfield_CO2_p_xyzt, ~.x - .y),
         atkinson_delta_CO2 = future_map2(CO2_ML_xyt, atkinson_CO2_p_xyzt, ~.x - .y)) %>% 
  select(c(lon, lat, month, depth_m, contains("delta_CO2")))

#----Save_output----
saveRDS(grid_sections[[i]],
        str_c(working_data_directory,
              "/delta_CO2_grid_computed_",
              str_pad(i, 2, pad = "0"),
              ".RDS",
              sep = ""))

#----Return_to_serial_processing----
plan(sequential)