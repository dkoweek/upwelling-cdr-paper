#Load netCDF file
mixed_layer_depth_data <- 
  here::here("data",
             "input_data",
             "Argo_mixedlayers_monthlyclim_12112019.nc") %>% 
  tidync()

#Choose which method to use to define this data set
method <- "algorithm"

if(method == "algorithm") {
  MLD_var = "mld_da_mean"
} else {
  MLD_var = "mld_dt_mean"
}


#Calculate annual mean MLD data from monthly-resolved MLD data
MLD_data <- 
  mixed_layer_depth_data %>% 
  hyper_tibble(select_var = MLD_var) %>% 
  rename(lon = iLON,
         lat = iLAT) %>% 
  group_by(lon, lat) %>% 
  summarise(MLD = mean(!!as.name(MLD_var)),
            months_MLD_data = n_distinct(iMONTH),
            .groups = "keep") %>% 
  ungroup() 

#Wrangle lat/lon data to match GLODAP data
MLD_data <- 
  MLD_data %>% 
  mutate(lat = lat - 90.5, #first lat bin spans 90S - 89S, centered at 89.5S
         lon = lon - 180.5) #first lon bin spans 180-179W, centered at 179.5W

#Save data set
saveRDS(MLD_data,
        here::here("data",
                   "working_data",
                   "MLD_data.rds"))

#Clear memory
rm(MLD_data)