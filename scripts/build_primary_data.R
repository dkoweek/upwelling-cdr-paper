#Script to be used as a pseudo-Makefile that allows packages and custom...
#...functions to be shared across scripts



#----Initialize_workspace----
source(here::here("scripts", "initialize_workspace.R"))

#Load data set on mixed layer depth
source(here::here("scripts", "calculate_mixed_layer_depth.R"))

#----Load_temperature_data----
run_or_load(data_directory = here::here("data", "working_data"), 
            "delta_T.rds", 
            scripts_directory = here::here("scripts"), 
            "calculate_delta_temperature.R")

#----Load_salinity_data----
run_or_load(data_directory = here::here("data", "working_data"), 
            "delta_S.rds", 
            scripts_directory = here::here("scripts"), 
            "calculate_delta_salinity.R")