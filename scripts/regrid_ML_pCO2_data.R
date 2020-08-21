#Script to regrid ML pCO2 climatology data to 1deg x 1deg

#----Define_input_output_and_grid_files----
#Raw netCDF file
takahashi_climatology_raw <- 
  str_c(ML_climatology_directory,
        "TALK_TCO2_pCO2_GLOB_Grid_Dat.nc",
        sep = "/")


#New path and filename
takahashi_climatology_interpolated <- 
  str_c(ML_climatology_directory,
        "TALK_TCO2_pCO2_GLOB_Grid_Dat_regridded.nc",
        sep = "/")

gridfile <- 
  here::here("scripts",
             "gridfile.txt")

#----Construct_and_execute_interpolation----
#Nearest neighbor interpolation
regrid_command <- 
  str_c("cdo -remapnn,",
        gridfile,
        " ",
        takahashi_climatology_raw,
        " ",
        takahashi_climatology_interpolated,
        sep = "")

system(regrid_command)





