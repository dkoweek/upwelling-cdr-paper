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

#----Define_constants----
CO2_MM <- 44 #grams/mol
g_per_ton <- 1e6 #g per metric ton


#----Calculate_CDR----

CDR_df <- list()
for (i in 1:length(AU_models)) {
  
  model_name <- 
    str_split(string = AU_models[i], 
              pattern = "_delta_")[[1]][1] 
    
  
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
  
  CDR_df[[i]] <- 
    #Merge mass flow and delta CO2 data sets
    inner_join.(model_in,
                mass_flow_grid,
                by = grouping_variables) %>% 
    #Calculate CDR at every xyzt combination (lon, lat, depth, month): fractional day in the photoperiod * kg/m^2/month * mol/kg -> mol/m^2/month 
    mutate.(CDR_per_area_lb = f_light * Q_xyz_lb * !!as.name(delta_CO2_name),
            CDR_per_area_ub = f_light * Q_xyz_ub * !!as.name(delta_CO2_name)) %>% 
    #Calculate annual by adding up the monthly CDR
    summarize.(CDR_annual_lb = sum(CDR_per_area_lb), #mol/m^2/year
               CDR_annual_ub = sum(CDR_per_area_ub),
               MLD_max = mean(MLD_max), #hack to carry 'MLD_max' and 'pipe_depth' into summarized data frame
               pipe_depth = mean(pipe_depth),
                            .by = c(lon,
                                    lat,
                                    depth_m)) %>% 
    mutate_across.(contains("CDR_annual"), ~ .x * (CO2_MM / g_per_ton)) %>% #metric ton CO2/m^2/year (molar mass = 44 g/mol * kg/1000g * metric ton/ 1000 kg)
    mutate.(model = model_name)

  #Save memory
  rm(model_in)
  
  
}

#Combine into a single data frame
CDR_grid <- 
  plyr::ldply(CDR_df,
              data.frame) %>% 
  as_tibble()


#----Save_output----
saveRDS(CDR_grid,
        str_c(working_data_directory,
              "CDR_grid.RDS",
              sep = "/"))