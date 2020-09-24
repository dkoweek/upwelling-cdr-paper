#----Load_data_sets----
#Temperature
temperature_file <- 
  str_c(GLODAP_directory,
        "/",
        "GLODAPv2.2016b.temperature.nc",
        sep = "") %>% 
  tidync() %>% 
  hyper_tibble(select_var = "temperature") %>% 
  rename(T = temperature)

#Salinity
salinity_file <- 
  str_c(GLODAP_directory,
        "/",
        "GLODAPv2.2016b.salinity.nc",
        sep = "") %>% 
  tidync() %>% 
  hyper_tibble(select_var = "salinity") %>% 
  rename(S = salinity)

#Nitrate
nitrate_file <- 
  str_c(GLODAP_directory,
        "/",
        "GLODAPv2.2016b.NO3.nc",
        sep = "") %>% 
  tidync() %>% 
  hyper_tibble(select_var = "NO3")

#Phosphate
phosphate_file <- 
  str_c(GLODAP_directory,
        "/",
        "GLODAPv2.2016b.PO4.nc",
        sep = "") %>% 
  tidync() %>% 
  hyper_tibble(select_var = "PO4")

#Dissolved Inorganic Carbon
DIC_file <- 
  str_c(GLODAP_directory,
        "/",
        "GLODAPv2.2016b.TCO2.nc",
        sep = "") %>% 
  tidync() %>% 
  hyper_tibble(select_var = "TCO2") %>% 
  rename(DIC = TCO2)

#Total Alkalinity
TA_file <- 
  str_c(GLODAP_directory,
        "/",
        "GLODAPv2.2016b.TAlk.nc",
        sep = "") %>% 
  tidync() %>% 
  hyper_tibble(select_var = "TAlk") %>% 
  rename(TA = TAlk)

#----Merge_data_sets----

grouping_columns <- 
  c("lon",
    "lat",
    "depth_surface")

GLODAP_data <- 
  temperature_file %>% 
  inner_join(.,
             salinity_file,
             by = grouping_columns) %>% 
  inner_join(.,
             nitrate_file,
             by = grouping_columns) %>% 
  inner_join(.,
             phosphate_file,
             by = grouping_columns) %>% 
  inner_join(.,
             DIC_file,
             by = grouping_columns) %>% 
  inner_join(.,
             TA_file,
             by = grouping_columns) %>% 
  full_join(., #Combine with depth surface matchup table
            depth_surfaces_matchup_table, 
            by = "depth_surface") %>% 
  select(-depth_surface) %>% #drop unnecessary columns to save memory
  mutate(lon = case_when(lon >=180 ~ lon - 360, #Adjust to E/W coordinates
                         TRUE ~ as.numeric(lon)))