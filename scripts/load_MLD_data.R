#----Load_netCDF_file----
mixed_layer_depth_data <- 
  str_c(input_data_directory,
        "Argo_mixedlayers_monthlyclim_12112019.nc",
        sep = "/") %>% 
  tidync()

#----Choose_method----
method <- "algorithm"

if(method == "algorithm") {
  MLD_var = "mld_da_mean"
} else {
  MLD_var = "mld_dt_mean"
}

#----Load_monthly_MLD_data----
MLD_data <- 
  mixed_layer_depth_data %>% 
  hyper_tibble(select_var = MLD_var) %>% 
  rename(lon = iLON,
         lat = iLAT,
         month = iMONTH,
         MLD = mld_da_mean)

#----Interpolate_through_missing_values----
interpolate_MLD <- function(data) {
  
  #If only one observation, take that for the entire year
  if(nrow(data)==1) {
    
    return(tibble(month = seq(1,12),
                  MLD = data[["MLD"]],
                  MLD_max = data[["MLD"]]))
  
      
  } 
  #Otherwise, linearly interpolate through the values
  else {
    
    interpolated_MLD <- 
      approx(x = data[["month"]],
             y = data[["MLD"]],
             xout = seq(1,12),
             method = "linear",
             rule = 2)
    
    return(tibble(month = interpolated_MLD[["x"]],
                  MLD = interpolated_MLD[["y"]],
                  MLD_max = max(interpolated_MLD[["y"]])))
    
    
  }
  
}


MLD_data <- 
  MLD_data %>% 
  group_by(lon, lat) %>% 
  nest() %>% 
  mutate(data = map(data, interpolate_MLD)) %>% 
  unnest(data)
  

#----Adjust_lat_lon----  
#Reset lat/lon to actual coordinates
MLD_data <- 
  MLD_data %>% 
  mutate(lat = lat - 90.5, #first lat bin spans 90S - 89S, centered at 89.5S
         lon = lon - 180.5) #first lon bin spans 180-179W, centered at 179.5W
