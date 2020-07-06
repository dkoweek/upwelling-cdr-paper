#Load netCDF file
salinity_data <- 
  here::here("data", 
             "input_data",
             "GLODAPv2.2016b_MappedClimatologies", 
             "GLODAPv2.2016b.salinity.nc") %>% 
  tidync()

#Calculate tibble of salinity differences between ML and different depths
delta_S_data <- 
  delta_dataset(tidync_data = salinity_data,
                variable = "salinity",
                nickname = "S")

#Save data set
saveRDS(delta_S_data,
        here::here("data",
                   "working_data",
                   "delta_S.rds"))

#Clear memory
rm(delta_S_data)