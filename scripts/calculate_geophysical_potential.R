#Calculate geophysical potential for CDR

#----Load_primary_data----
#'build_primary_data.R' sources 'initialize_workspace.R'
source(here::here("scripts", "build_primary_data.R"))

delta_data <- 
  readRDS(here::here("data",
                     "working_data",
                     "delta_data.rds"))

#----Apply_growth_models----
plankton_model <- 
  delta_data %>% 
  #Apply growth models to ML data and depth data
  mutate(bio_ML = map2(N_ML, P_ML, plankton_biogeochem_model),
         bio_z = map2(NO3, PO4, plankton_biogeochem_model)) %>%  
  #Pluck list elements
  mutate(delta_DIC_bio_ML = map_dbl(bio_ML, pluck, 1),
         delta_TA_bio_ML = map_dbl(bio_ML, pluck, 2),
         delta_DIC_bio_z = map_dbl(bio_z, pluck, 1),
         delta_TA_bio_z = map_dbl(bio_z, pluck, 2)) %>% 
  select(-c(bio_ML, bio_z)) %>% #save memory
  #calculate differences between gained production from new nutrients...
  #...and lost production from 
  mutate(delta_DIC_bio = delta_DIC_bio_z - delta_DIC_bio_ML, 
         delta_TA_bio = delta_TA_bio_z - delta_TA_bio_ML) %>% 
  #Calculate total change in DIC and TA from transport and changes in production
  mutate(delta_DIC = delta_DIC_cons + delta_DIC_bio,
         delta_TA = delta_TA_cons + delta_TA_bio)

rm(delta_data)


#----Light_limitation----



#----Calculate_CO2_deficit----
plankton_model <-
  plankton_model %>%
  mutate(T_midpt = (temperature + T_ML) / 2,
         S_midpt = (salinity + S_ML) / 2,
         TA_midpt = (TA + TA_ML) / 2,
         DIC_midpt = (DIC + DIC_ML) / 2) %>% 
  mutate(
         dCO2_dT = pmap_dbl(list(T_midpt, S_midpt, TA_midpt, DIC_midpt, lon, lat),
                            dCO2_dT),
         dCO2_dS = pmap_dbl(list(T_midpt, S_midpt, TA_midpt, DIC_midpt, lon, lat),
                            dCO2_dS),
         dCO2_dTA = pmap_dbl(list(T_midpt, S_midpt, TA_midpt, DIC_midpt, lon, lat),
                            dCO2_dTA),
         dCO2_dDIC = pmap_dbl(list(T_midpt, S_midpt, TA_midpt, DIC_midpt, lon, lat),
                            dCO2_dDIC)
         ) %>% 
  mutate(CO2_deficit = #mol CO2 /kg SW
                        (dCO2_dT * delta_T) + 
                        (dCO2_dS * delta_S) +
                        (dCO2_dTA * delta_TA) +
                        (dCO2_dDIC * delta_DIC)
         )


#----Export_results----
saveRDS(plankton_model,
        here::here("data",
                   "working_data",
                   "plankton_model.rds"))  