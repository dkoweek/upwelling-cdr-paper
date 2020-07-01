#Load netCDF file
nitrate_data <- 
  here::here("data", 
             "input_data",
             "GLODAPv2.2016b_MappedClimatologies", 
             "GLODAPv2.2016b.NO3.nc") %>% 
  tidync()

#Calculate tibble of nitrate differences between ML and different depths
delta_N_data <- 
  delta_dataset(tidync_data = nitrate_data,
                variable = "NO3",
                nickname = "N") 

#Save data set
saveRDS(delta_N_data,
        here::here("data",
                   "working_data",
                   "delta_N.rds"))

#Clear memory
rm(delta_N_data)