#----Load_the_data_sets----

#Load the merged mixed layer data sets
source(here::here("scripts",
                  "merge_ML_data.R"))

#Load the GLODAP data set
source(here::here("scripts",
                  "load_GLODAP_data.R"))

#----Merge_ML_data_set_with_GLODAP----
delta_CO2_grid <- 
  inner_join(mixed_layer_data %>% 
               select(c(lon, lat, month, depth_MLD, CO2, epsilon_micro, epsilon_macro)), 
             GLODAP_data %>% 
               select(c(lon, lat, depth_m, T, S, NO3, PO4, DIC, TA)), 
             by = c("lon", "lat")) %>% 
  rename(CO2_ML_xyt = CO2) %>% 
  filter(depth_m > depth_MLD) 

#----Calculate_biogeochemical_drawdown----
delta_CO2_grid <-
  delta_CO2_grid %>% 
  #Run models
  mutate(galbraith = map2_dfr(NO3, PO4, galbraith_model),
         garcia = map2_dfr(NO3, PO4, garcia_model, metric = "median"),
         redfield = map2_dfr(NO3, PO4, redfield_model),
         atkinson = map2_dfr(NO3, PO4, atkinson_model, metric = "median")) %>% 
  #Maximum potential biogeochemical change
  mutate(across(galbraith:atkinson, ~ pluck(.x,1), .names = "{col}_dDIC_max"),
         across(galbraith:atkinson, ~ pluck(.x,2), .names = "{col}_dTA_max")) %>% 
  #Add in light limitation
  mutate(galbraith_dDIC = galbraith_dDIC_max * epsilon_micro,
         galbraith_dTA = galbraith_dTA_max * epsilon_micro,
         garcia_dDIC = garcia_dDIC_max * epsilon_micro,
         garcia_dTA = garcia_dTA_max * epsilon_micro,
         redfield_dDIC = redfield_dDIC_max * epsilon_micro,
         redfield_dTA = redfield_dTA_max * epsilon_micro,
         atkinson_dDIC = atkinson_dDIC_max * epsilon_macro,
         atkinson_dTA = atkinson_dTA_max * epsilon_macro) %>% 
  #Apply changes to DIC and TA and location x,y,z
  mutate(galbraith_DIC_p = galbraith_dDIC + DIC,
         galbraith_TA_p = galbraith_dTA + TA,
         garcia_DIC_p = garcia_dDIC + DIC,
         garcia_TA_p = garcia_dTA + TA,
         redfield_DIC_p = redfield_dDIC + DIC,
         redfield_TA_p = redfield_dTA + TA,
         atkinson_DIC_p = atkinson_dDIC + DIC,
         atkinson_TA_p = atkinson_dTA + TA) %>%  
  #Drop variables to save memory
  select(-c(galbraith:atkinson_dTA)) %>% 
  #Calculate CO2 concentration
  mutate(
    galbraith_CO2_p_xyzt = carb(
      flag = 15,
      var1 = galbraith_TA_p / 1e6,
      var2 = galbraith_DIC_p / 1e6,
      S = S,
      T = T,
      P = 0,
      warn = "n"
    ) %>% pull(CO2),
    garcia_CO2_p_xyzt = carb(
      flag = 15,
      var1 = garcia_TA_p / 1e6,
      var2 = garcia_DIC_p / 1e6,
      S = S,
      T = T,
      P = 0,
      warn = "n"
    ) %>% pull(CO2),
    redfield_CO2_p_xyzt = carb(
      flag = 15,
      var1 = redfield_TA_p / 1e6,
      var2 = redfield_DIC_p / 1e6,
      S = S,
      T = T,
      P = 0,
      warn = "n"
    ) %>% pull(CO2),
    atkinson_CO2_p_xyzt = carb(
      flag = 15,
      var1 = atkinson_TA_p / 1e6,
      var2 = atkinson_DIC_p / 1e6,
      S = S,
      T = T,
      P = 0,
      warn = "n"
    ) %>% pull(CO2))

#----Calculate_CO2_gradient----
delta_CO2_grid <- 
  delta_CO2_grid %>%
  mutate(galbraith_delta_CO2 = CO2_ML_xyt - galbraith_CO2_p_xyzt,
         garcia_delta_CO2 = CO2_ML_xyt - garcia_CO2_p_xyzt,
         redfield_delta_CO2 = CO2_ML_xyt - redfield_CO2_p_xyzt,
         atkinson_delta_CO2 = CO2_ML_xyt - atkinson_CO2_p_xyzt) %>% 
  select(c(lon, lat, month, depth_m, contains("delta_CO2")))

#----Save_output----
saveRDS(delta_CO2_grid,
        str_c(working_data_directory,
              "delta_CO2_grid.RDS",
              sep = "/"))