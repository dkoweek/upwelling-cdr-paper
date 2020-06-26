# Load necessary packages
pacman::p_load(tidyverse,
               viridis,
               here,
               seacarb,
               ncdf4,
               RNetCDF,
               tidync)



#Load custom functions
source(here::here("scripts",
                  "custom_functions.R"))

#Load tables and parameters to be used through analysis
source(here::here("scripts",
                  "load_depth_surface_matchup_table.R"))


# Print session info
writeLines(capture.output(sessionInfo()),
           here::here("session_information.txt"))