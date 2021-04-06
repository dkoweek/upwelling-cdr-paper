#----Load_netCDF_file----
mixed_layer_depth_data <- 
  str_c(input_data_directory,
        "Argo_mixedlayers_monthlyclim_12112019.nc",
        sep = "/") %>% 
  tidync()

lon_adjustment <- -180.5  #first lon bin spans 180-179W, centered at 179.5

lat_adjustment <- -90.5 #first lat bin spans 90S - 89S, centered at 89.5S

#----Choose_MLD_method----
method <- "algorithm"

if(method == "algorithm") {
  MLD_var = "mld_da_median"
} else {
  MLD_var = "mld_dt_median"
}

#----Load_monthly_MLD_data----
MLD_data <- 
  mixed_layer_depth_data %>% 
  hyper_tibble(select_var = MLD_var) %>% 
  rename(lon = iLON,
         lat = iLAT,
         month = iMONTH,
         MLD = !!(MLD_var))


#----Identify_missing_values_each_month----
months_of_year <- seq(1,12)

lon_lat_pairs <- 
  MLD_data %>% 
  select(c(lon, lat)) %>% 
  distinct() %>% 
  expand_grid(., month = months_of_year)

MLD_data <- 
  left_join(lon_lat_pairs,
            MLD_data,
            by = c("lon", "lat", "month"))


#----Interpolate_through_missing_values----

MLD_data_by_month <- list()
for (n in months_of_year) { #For each month
  
  #For each month, identify the measured and missing values
  
  measured_values <- 
    MLD_data %>% 
    filter(month == n,
           !is.na(MLD))
  
  missing_values <- 
    MLD_data %>% 
    filter(month == n,
           is.na(MLD))
  
  for (i in 1:nrow(missing_values)) {
    
    #For each missing value, calculate the distance between the missing value and all measured values
    missing_lon <- 
      missing_values[[i, "lon"]]
      
    missing_lat <- 
      missing_values[[i, "lat"]]

    #Start by searching in a 1x1 box around the missing value
    MLD_NN <- c() #initialize vector of nearest neighbors
    k <- 0 #counter
    bounding_box_initial <- 1 #degrees
    while(length(MLD_NN) < 1) {
      
      #Expand bounding box radius as necessary
      bounding_box_degrees <- bounding_box_initial + k 
      
      #Set corner points of bounding box
      lat_min <- missing_lat - bounding_box_degrees
      
      lat_max <- missing_lat + bounding_box_degrees
      
      #Longitude bounds (more complicated by 0 deg meridian)
      if (missing_lon <= bounding_box_degrees) {
        
        meridian_cross <- TRUE
        
        lon_min <- 360 - (bounding_box_degrees - missing_lon)
        
        lon_max <- missing_lon + bounding_box_degrees
        
      } else if (360 - missing_lon <= bounding_box_degrees) {
        
        meridian_cross <- TRUE
        
        lon_min <- missing_lon - bounding_box_degrees
        
        lon_max <- bounding_box_degrees - (360 - missing_lon) 
      
      } else {
        
        meridian_cross <- FALSE
        
        lon_min <- missing_lon - bounding_box_degrees
        
        lon_max <- missing_lon + bounding_box_degrees
      }
      
      #Function for conditional filtering of longitude
      lon_filtering <- function(data, meridian_cross) {
        
        if (meridian_cross == TRUE) {
        
          data %>% 
            filter(lon >= lon_min | lon <= lon_max) %>% 
            return()
          
        } else if (meridian_cross == FALSE) {
          
          data %>% 
            filter(lon_min <= lon, lon <= lon_max) %>% 
            return()
          
        }
        
      }
      
      #Filter nearest neighbors based on location of missing value and window search size
      nearest_neighbors <- 
        measured_values %>% 
        lon_filtering(., meridian_cross) %>% 
        filter(lat_min <= lat, lat <= lat_max)
      
      if (nrow(nearest_neighbors) >=  1) {
        
        suppressMessages(
          
          MLD_NN <- 
            nearest_neighbors %>% 
            #Have to work on actual coordinate for `distm` function
            mutate(lat = lat + lat_adjustment,
                   lon = lon + lon_adjustment) %>%
            mutate(distance_to_missing_value = distm(
              bind_cols(lon, lat), 
              c(missing_lon + lon_adjustment, missing_lat + lat_adjustment),
              fun = distGeo
            )) %>% 
            slice_min(distance_to_missing_value) %>% 
            pull(MLD)
                        
          )
        
      } else {
      
        k <- k + 1 #counter to expand box size
    
      }
      
    }
    
    
    #Assign nearest neighbor MLD to missing value 
    #...(if more than one equally distant neighbor, take average)
    missing_values[i, "MLD"] <- 
      mean(MLD_NN) 
    
    
  }
  
  #Recombine measured and interpolated data for each month
  MLD_data_by_month[[n]] <- 
    bind_rows(measured_values,
              missing_values)
  
}

#Reconstitute data frame
MLD_data <- 
  plyr::ldply(MLD_data_by_month,
              data.frame) %>% 
  as_tibble()

#----Adjust_lat_lon----  
#Reset lat/lon to actual coordinates
MLD_data <- 
  MLD_data %>% 
  mutate(lat = lat + lat_adjustment,
         lon = lon + lon_adjustment) 