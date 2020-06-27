#Load netCDF file
TA_data <- 
  here::here("data", 
             "input_data",
             "GLODAPv2.2016b_MappedClimatologies", 
             "GLODAPv2.2016b.TAlk.nc") %>% 
  tidync()

#Calculate tibble of total alkalinity differences between ML and different depths
delta_TA_data <- 
  delta_dataset(tidync_data = TA_data,
                variable = "TAlk",
                nickname = "TA")

#Save data set
saveRDS(delta_TA_data,
        here::here("data",
                   "working_data",
                   "delta_TA.rds"))

#Clear memory
rm(delta_TA_data)