#----Load_necessary_packages----
pacman::p_load(tidyverse,
               viridis,
               here,
               seacarb,
               ncdf4,
               RNetCDF,
               tidync,
               deSolve)

#----Load_project_directories----
source(here::here("scripts",
                  "project_directories.R"))

#----Load_functions_and_data----

#Load custom functions for wrangling data
source(here::here("scripts",
                  "custom_functions.R"))

#Load plankton growth model
source(here::here("scripts",
                  "plankton_growth_model.R"))

#Load tables and parameters to be used through analysis
source(here::here("scripts",
                  "load_depth_surface_matchup_table.R"))

#----Print_session_info----
# Print session info
writeLines(capture.output(sessionInfo()),
           here::here("session_information.txt"))