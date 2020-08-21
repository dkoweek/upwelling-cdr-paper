#Script to regrid MODIS PAR data to 1deg x 1deg

#----Define_input_output_and_grid_files----
#Raw directory
MODIS_PAR_raw_directory <- 
  str_c(MODIS_directory,
        "raw",
        sep = "/")

#Regridded directory
MODIS_PAR_regridded_directory <- 
  str_c(MODIS_directory,
        "regridded",
        sep = "/")

gridfile <- 
  here::here("scripts",
             "gridfile.txt")

#----Regrid_all_PAR_monthly_climatologies----
PAR_raw_files <- 
  list.files(MODIS_PAR_raw_directory)

#Need cdo and nco installed

setwd(MODIS_PAR_raw_directory)
for (i in PAR_raw_files) {
  
  #Drop "palette" variable
  drop_palette_command <- 
    str_c("ncks -v par ",
          i,
          " temp.nc")
  
  system(drop_palette_command)
  
  #Regrid to 1deg x 1deg
  base_name <- 
    str_split(i,
              ".nc",
              simplify = TRUE)
  
  regridded_name <- 
    str_c(base_name[1],
          "_regrid.nc",
          sep="")
  
  regrid_command <- 
    str_c("cdo -remapnn,",
          gridfile,
          " temp.nc ../regridded/",
          regridded_name,
          sep = "")
  
  system(regrid_command)
  system("rm temp.nc")
  
}
setwd(here())