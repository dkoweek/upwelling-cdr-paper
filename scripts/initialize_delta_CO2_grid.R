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
               select(c(lon, lat, month, MLD, MLD_max, salinity, temperature, CO2, epsilon_micro, epsilon_macro)), 
             GLODAP_data %>% 
               select(c(lon, lat, depth_m, T, S, NO3, PO4, DIC, TA)), 
             by = c("lon", "lat")) %>% 
  rename(CO2_ML_xyt = CO2,
         S_ML_xyt = salinity,
         T_ML_xyt = temperature)

#----Save_the_grid----
saveRDS(delta_CO2_grid,
        str_c(working_data_directory,
              "delta_CO2_grid_initial.RDS",
              sep = "/"))