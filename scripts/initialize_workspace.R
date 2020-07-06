# Load necessary packages
pacman::p_load(tidyverse,
               viridis,
               here,
               seacarb,
               ncdf4,
               RNetCDF,
               tidync)



#Load custom functions for wrangling data
source(here::here("scripts",
                  "custom_functions.R"))

#Load plankton growth model
source(here::here("scripts",
                  "plankton_growth_model.R"))

#Load set of functions for calculating CO2 trajectory in the mixed layer
source(here::here("scripts",
                  "co2_trajectory_model.R"))

#Load tables and parameters to be used through analysis
source(here::here("scripts",
                  "load_depth_surface_matchup_table.R"))


# Print session info
writeLines(capture.output(sessionInfo()),
           here::here("session_information.txt"))