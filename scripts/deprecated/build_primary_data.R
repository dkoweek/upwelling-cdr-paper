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

#----Load_salinity_data----
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

grouping_columns <- 
  c("lat",
    "lon",
    "depth_m",
    "MLD")

delta_data <-
  delta_T %>%
  inner_join(.,
             delta_S,
             by = grouping_columns) %>% 
  inner_join(.,
             delta_N,
             by = grouping_columns) %>% 
  inner_join(.,
             delta_P,
             by = grouping_columns) %>% 
  inner_join(.,
             delta_TA,
             by = grouping_columns) %>% 
  inner_join(.,
             delta_DIC,
             by = grouping_columns)

#Remove data in the mixed layer
delta_data <- 
  delta_data %>% 
  filter(depth_m >= MLD)

#----Export_merged_data----

#Save data set
saveRDS(delta_data,
        here::here("data",
                   "working_data",
                   "delta_data.rds"))

#Clear memory
rm(delta_T,
   delta_S,
   delta_N,
   delta_P,
   delta_TA,
   delta_DIC,
   delta_data)
  