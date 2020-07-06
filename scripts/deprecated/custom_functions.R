mixed_layer_value <- function(data_list, MLD) {
  
  data <- 
    data_list %>% 
    as.data.frame()
  
  data_ML <- 
    data %>% 
    filter(depth_m <= MLD)
  
  if(nrow(data_ML) == 0) {
    
    return(NA)
    
  } else {
    
    mean_ML_value <- 
      data_ML %>% 
      select(-depth_m) %>% 
      pull() %>% 
      mean(., na.rm = TRUE)
    
    return(mean_ML_value)
    
  }
}

delta_value <- function(data_list, value_ML) {
  
  #If there is no ML data, return NA
  if(is.na(value_ML)==TRUE) {
    
    return(NA)
    #If there is ML data, calculate the delta between the ML value and each depth  
  } else {
    
    data <- 
      data_list %>% 
      as.data.frame() %>% 
      select(-depth_m) %>% 
      pull()
    
    delta_data <- 
      data - value_ML
    
    #Check that the length of the data equals the length of the delta data
    stopifnot({length(data)==length(delta_data)})
    
    return(delta_data)  
    
  } 
}

delta_dataset <- function(tidync_data, variable, nickname, ...) {
  
  #Define dynamic variable names
  ML_name <- 
    str_c(nickname,
          "ML",
          sep = "_")
  
  delta_name <- 
    str_c("delta",
          nickname,
          sep = "_")
  
  #Build dataset of delta values
  delta_ds <- 
    tidync_data %>% 
    hyper_tibble(select_var = variable) %>% 
    full_join(., depth_surfaces_matchup_table, by = "depth_surface") %>% 
    select(-depth_surface) %>% #drop unnecessary columns to save memory
    mutate(lon = case_when(lon >=180 ~ lon - 360,
                           TRUE ~ as.numeric(lon))) %>% #Adjust to E/W coordinates
    group_by(lat, lon) %>% 
    nest() %>% 
    inner_join(., MLD_data, by = c("lat", "lon")) %>% #Merge with MLD data
    select(-months_MLD_data) %>%  #only important for MLD diagnostics 
    mutate(!!ML_name := map2_dbl(data, MLD, mixed_layer_value),
           !!delta_name := map2(data, !!as.name(ML_name), delta_value)) %>%
    unnest(col = c(data, !!delta_name)) %>% 
    ungroup()
  
  return(delta_ds)
  
}


run_or_load <- function(data_directory, data_file, scripts_directory, script) {
  
  #Check if the data file is present
  setwd(data_directory)
  
  file_absent <- 
    !(file.exists(data_file))
  
  #Get modified timestamps for file, script, and functions
  file_mtime <- 
    file.mtime(data_file)
  
  setwd(scripts_directory)
  
  script_mtime <- 
    file.mtime(script)
  
  functions_mtime <-
    file.mtime("custom_functions.R")
  
  #If the file doesn't exist or the script mtime is newer than the file mtime...
  #...or the set of custom functions mtime is new than the file mtime...
  #....run the script and load the file. Otherwise, just load the file
  if(file_absent | difftime(script_mtime, file_mtime)  > 0 | difftime(functions_mtime, file_mtime)  > 0) {
    
    source(script)
    
    return(
      assign(str_split(data_file, ".rds", simplify = TRUE)[1],
             readRDS(str_c(data_directory, "/", data_file, sep = "")),
             envir = .GlobalEnv
      )
    )
    
  } else {
    
    return(
      assign(str_split(data_file, ".rds", simplify = TRUE)[1],
             readRDS(str_c(data_directory, "/", data_file, sep = "")),
             envir = .GlobalEnv
      )
    )
    
  }
  
  setwd(here::here())
  
}
