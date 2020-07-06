#Load netCDF file
phosphate_data <- 
  here::here("data", 
             "input_data",
             "GLODAPv2.2016b_MappedClimatologies", 
             "GLODAPv2.2016b.PO4.nc") %>% 
  tidync()

#Calculate tibble of phosphate differences between ML and different depths
delta_P_data <- 
  delta_dataset(tidync_data = phosphate_data,
                variable = "PO4",
                nickname = "P")

#Save data set
saveRDS(delta_P_data,
        here::here("data",
                   "working_data",
                   "delta_P.rds"))

#Clear memory
rm(delta_P_data)