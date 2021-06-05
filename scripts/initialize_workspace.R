#----Load_necessary_packages----
pacman::p_load(tidyverse,
               broom,
               lubridate,
               viridis,
               here,
               tidytable,
               seacarb,
               ncdf4,
               RNetCDF,
               tidync,
               deSolve,
               pangaear,
               sf,
               geosphere,
               readxl,
               cowplot,
               ggpubr,
               ggrepel)

#----Load_project_directories----
source(here::here("scripts",
                  "project_directories.R"))

#----Load_functions_and_data----
#Load plankton nutrient model
source(here::here("scripts",
                  "plankton_nutrient_model.R"))

#Load macroalgae nutrient model
source(here::here("scripts",
                  "macroalgae_nutrient_model.R"))

#Load tables and parameters to be used through analysis
source(here::here("scripts",
                  "load_depth_surface_matchup_table.R"))

#----Print_session_info----
# Print session info
writeLines(capture.output(sessionInfo()),
           here::here("session_information.txt"))