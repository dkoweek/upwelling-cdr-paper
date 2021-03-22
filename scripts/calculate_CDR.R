# Calculate annual CDR from the growth model and mass flow results

#----Load_mass_flow_grid----
mass_flow_grid <- 
  readRDS(file = str_c(working_data_directory,
                       "mass_flow_grid.RDS",
                       sep="/"))

#----List_growth_models----
AU_models <-
  list.files(path = working_data_directory,
             pattern = "_delta_CO2_grid.RDS")

#----Calculate_CDR----

for (i in 1:length(AU_models)) {
  
  model_name <- 
    str_extract(string = AU_models[i], 
                pattern = "^[a-z]+")
  
  model_in <- 
    readRDS(file = str_c(working_data_directory,
                         AU_models[i],
                         sep="/"))
  
  delta_CO2_name <- 
    model_in %>% 
    select(contains("delta_CO2")) %>% 
    names() 
  
  #For each model, calculate the annual CDR for each latitude, longitude, depth combination
  
  #First define grouping variables to merge 'delta_CO2' and 'mass_flow' data sets
  grouping_variables <- 
    intersect(names(mass_flow_grid),
              names(model_in))
  
  CDR_df <- 
    #Merge mass flow and delta CO2 data sets
    inner_join.(model_in,
                mass_flow_grid,
                by = grouping_variables) %>% 
    #Calculate CDR at every xyzt combination (lon, lat, depth, month)
    mutate.(CDR_per_area = Q_max_xyzt * !!as.name(delta_CO2_name)) %>% #kg/m^2/month * mol/kg -> mol/m^2/month
    #Calculate annual by adding up the monthly CDR
    summarize.(CDR_annual = sum(CDR_per_area), #mol/m^2/year
               MLD_max = mean(MLD_max), #hack to carry 'MLD_max' and 'cell_area' into summarized data frame
               cell_area = mean(cell_area),
                            .by = c(lon,
                                    lat,
                                    depth_m)) %>% 
    mutate.(CDR_annual = CDR_annual * (44 / 1e6)) %>%  #metric ton CO2/m^2/year (molar mass = 44 g/mol * kg/1000g * metric ton/ 1000 kg)
    mutate.(model = model_name)

  #Save memory
  rm(model_in)
  
  saveRDS(CDR_df,
          str_c(working_data_directory,
                "/CDR_grid_",
                model_name,
                ".RDS",
                sep = ""))
  
}