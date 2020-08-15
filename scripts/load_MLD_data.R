#Load netCDF file
mixed_layer_depth_data <- 
  str_c(input_data_directory,
        "Argo_mixedlayers_monthlyclim_12112019.nc",
        sep = "/") %>% 
  tidync()

#Choose which method to use to define this data set
method <- "algorithm"

if(method == "algorithm") {
  MLD_var = "mld_da_mean"
} else {
  MLD_var = "mld_dt_mean"
}

#Load monthly MLD data
MLD_data <- 
  mixed_layer_depth_data %>% 
  hyper_tibble(select_var = MLD_var) %>% 
  rename(lon = iLON,
         lat = iLAT,
         month = iMONTH,
         depth_MLD = mld_da_mean)

#Reset lat/lon to actual coordinates
MLD_data <- 
  MLD_data %>% 
  mutate(lat = lat - 90.5, #first lat bin spans 90S - 89S, centered at 89.5S
         lon = lon - 180.5) #first lon bin spans 180-179W, centered at 179.5W
