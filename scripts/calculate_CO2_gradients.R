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
               select(c(lon, lat, month, depth_MLD, CO2, epsilon)), 
             GLODAP_data %>% 
               select(c(lon, lat, depth_m, T, S, NO3, PO4, DIC, TA)), 
             by = c("lon", "lat")) %>% 
  rename(CO2_ML_xyt = CO2) %>% 
  filter(depth_m > depth_MLD) 

#----Calculate_biogeochemical_drawdown----
delta_CO2_grid <- 
  delta_CO2_grid %>% 
  mutate(
         #Maximum potential biogeochemical change
         delta_DIC_bio_max = galbraith_biogeochemical_model(NO3 = NO3,
                                                PO4 = PO4) %>% pluck(1),
         delta_TA_bio_max = galbraith_biogeochemical_model(NO3 = NO3,
                                               PO4 = PO4) %>% pluck(2),
         #Add in light limitation
         delta_DIC_bio = epsilon * delta_DIC_bio_max, 
         delta_TA_bio = epsilon * delta_TA_bio_max,
         #Apply change to DIC and TA at location x,y,z
         DIC_p = DIC + delta_DIC_bio, 
         TA_p = TA + delta_TA_bio) %>% 
  #Drop variables to save memory
  select(-c(contains("_DIC_"))) %>% 
  select(-c(contains("_TA_"))) %>% 
         mutate(CO2_p_xyzt = carb(flag = 15,
                    var1 = TA_p / 1e6,
                    var2 = DIC_p / 1e6,
                    S = S,
                    T = T,
                    P = 0,
                    warn = "n") %>% 
           pull(CO2)) %>% 
  mutate(CO2_p_xyzt = as.numeric(CO2_p_xyzt)) 


#----Calculate_CO2_gradient----
delta_CO2_grid <- 
  delta_CO2_grid %>% 
  mutate(delta_CO2 = CO2_ML_xyt - CO2_p_xyzt) %>% 
  select(c(lon, lat, month, depth_m, delta_CO2))

#----Save_output----
saveRDS(delta_CO2_grid,
        str_c(working_data_directory,
              "delta_CO2_grid.RDS",
              sep = "/"))