cell_grid_area <- function(lat) {
  
  earth_radius <- 6371e3 #m
  
  lat_lb <- (lat - 0.5) * (pi / 180)
  
  lat_ub <- (lat + 0.5) * (pi / 180)
  
  lon_diff <- 1 * (pi / 180) #Assume all cells 1x1 degree
  
  area <-
    (sin(lat_ub) -
       sin(lat_lb)) *
    lon_diff *
    earth_radius ^ 2
  
  return(area)

}


generate_run_parameters_table <- function(n_years, n_pipes = 1, Q_dot = 1) {
  
  run_table <- 
    expand.grid(n_pipes,
                Q_dot) %>% 
    rename(n_pipes = Var1,
           Q_dot = Var2) %>% 
    mutate(n_years = n_years,
           tag = NA) %>% 
    as_tibble()
  
  return(run_table)
    
  
}

save_model_run <- function(model_output, model_tag) {
  
  model_output_file <- 
    str_c("plankton_model_",
          model_tag,
          sep = "")
  
  saveRDS(model_output,
          file = str_c(output_directory,
                       "/",
                       model_output_file,
                       ".rds",
                       sep = ""))
  
  Sys.sleep(1) #Ensures that model cannot write two files in the same second
  
  
}