#Script to be used as a pseudo-Makefile that allows packages and custom...
#...functions to be shared across scripts



#----Initialize_workspace----
source(here::here("scripts", "initialize_workspace.R"))

#----Load_mixed_layer_depth_data----
run_or_load(data_directory = here::here("data", "working_data"), 
            "MLD_data.rds", 
            scripts_directory = here::here("scripts"), 
            "calculate_mixed_layer_depth.R")

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

#----Load_nitrate_data----
run_or_load(data_directory = here::here("data", "working_data"), 
            "delta_N.rds", 
            scripts_directory = here::here("scripts"), 
            "calculate_delta_nitrate.R")

#----Load_phosphate_data----
run_or_load(data_directory = here::here("data", "working_data"), 
            "delta_P.rds", 
            scripts_directory = here::here("scripts"), 
            "calculate_delta_phosphate.R")

#----Load_TA_data----
run_or_load(data_directory = here::here("data", "working_data"), 
            "delta_TA.rds", 
            scripts_directory = here::here("scripts"), 
            "calculate_delta_TA.R")

#----Load_DIC_data----
run_or_load(data_directory = here::here("data", "working_data"), 
            "delta_DIC.rds", 
            scripts_directory = here::here("scripts"), 
            "calculate_delta_DIC.R")

#----Merge_data_sets----

delta_data <- 
  delta_T %>% 
  