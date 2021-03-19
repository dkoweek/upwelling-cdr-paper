# Calculate annual CDR from the growth model results

# Generate list of model runs
AU_models <-
  list.files(path = working_data_directory,
             pattern = "_delta_CO2_grid.RDS")


CDR_all_list <- list()
CDR_max_list <- list()
for (i in 1:nrow(AU_models)) {
  
  model_name <- 
    str_extract(string = AU_models[i], 
                pattern = "^[a-z]+")
  
  model_in <- readRDS(file = str_c(working_data_directory,
                                   AU_models[i],
                                   sep="/"))
  
  delta_CO2_name <- 
    model_in %>% 
    select(contains("delta_CO2")) %>% 
    names() 
  
  #For each model, calculate the annual CDR for each latitude, longitude, depth combination
  CDR_all_list[[i]] <- 
    model_in %>% 
    group_by(lon, lat, depth_m) %>% 
    dplyr::summarize(CDR = sum(!!as.name(delta_CO2_name)),
                     .groups = "drop") %>%  
    mutate(model = model_name)
  
  #Save memory
  rm(model_in)
  
  #For each model, select the greatest CDR at each longitude/latitude
  CDR_max_list[[i]] <- 
    CDR_all_list[[i]] %>% 
    group_by(lon, lat) %>% 
    slice_max(CDR) %>% 
    ungroup()
  
}

#Combine the results from each model into a single data frame
#CDR from all depths
CDR_all <- 
  plyr::ldply(CDR_all_list,
              data.frame) %>% 
  as_tibble()

#Maximum CDR at a given latitude/longitude
CDR_max <-
  plyr::ldply(CDR_max_list,
              data.frame) %>% 
  as_tibble()