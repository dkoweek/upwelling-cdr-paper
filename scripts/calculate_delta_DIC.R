#Load netCDF file
DIC_data <- 
  here::here("data", 
             "input_data",
             "GLODAPv2.2016b_MappedClimatologies", 
             "GLODAPv2.2016b.TCO2.nc") %>% 
  tidync()

#Calculate tibble of DIC differences between ML and different depths
delta_DIC_data <- 
  delta_dataset(tidync_data = DIC_data,
                variable = "TCO2",
                nickname = "DIC")

#Save data set
saveRDS(delta_DIC_data,
        here::here("data",
                   "working_data",
                   "delta_DIC.rds"))

#Clear memory
rm(delta_DIC_data)