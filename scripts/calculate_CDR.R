# Generate list of model runs
AU_models <-
  list.files(path = working_data_directory,
             pattern = "_delta_CO2_grid.RDS")



CDR_all_list <- list()
CDR_max_list <- list()
for (i in c(1)) {
  
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

  CDR_all_list[[i]] <- 
    model_in %>% 
    group_by(lon, lat, depth_m) %>% 
    dplyr::summarize(CDR = sum(!!as.name(delta_CO2_name)),
                     .groups = "drop") %>%  
    mutate(model = model_name)
  
  #Save memory
  rm(model_in)
  
  CDR_max_list[[i]] <- 
    CDR_all_list[[i]] %>% 
    group_by(lon, lat) %>% 
    slice_max(CDR) %>% 
    ungroup()
  
}

#Combine lists into single data frames
CDR_all <- 
  plyr::ldply(CDR_all_list,
              data.frame) %>% 
  as_tibble()

CDR_max <-
  plyr::ldply(CDR_max_list,
              data.frame) %>% 
  as_tibble()