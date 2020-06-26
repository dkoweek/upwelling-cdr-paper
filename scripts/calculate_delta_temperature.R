#Load netCDF file
temperature_data <- 
  here::here("data", 
             "input_data",
             "GLODAPv2.2016b_MappedClimatologies", 
             "GLODAPv2.2016b.temperature.nc") %>% 
  tidync()

#Calculate tibble of temperature differences between ML and different depths
delta_T_data <- 
  delta_dataset(tidync_data = temperature_data,
                variable = "temperature",
                nickname = "T")

#Save data set
saveRDS(delta_T_data,
        here::here("data",
                   "working_data",
                   "delta_T.rds"))

#Clear memory
rm(delta_T_data)