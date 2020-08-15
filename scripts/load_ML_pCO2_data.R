#Load *regridded* Takahashi et al. mixed layer climatology
#Need to regrid original data (see 'regrid_takahashi_data.sh')

#----Open_as_tibble----
mixed_layer_chemistry_data_file <-
  str_c(input_data_directory,
        "TALK_TCO2_pCO2_GLOB_Grid_Dat_regridded.nc",
        sep = "/")

mixed_layer_chemistry_data <- 
  tidync(mixed_layer_chemistry_data_file) %>% 
  hyper_tibble()

#----Correct_time_data----
#time listed as days since 1-Jan-2000
time_units <-
  ncmeta::nc_atts(mixed_layer_chemistry_data_file, "time") %>% 
  tidyr::unnest(cols=c(value))

timescale <- 
  mixed_layer_chemistry_data %>% 
  select(time) %>% 
  unique %>% 
  mutate(month = row_number())

mixed_layer_chemistry_data <- 
  mixed_layer_chemistry_data %>% 
  left_join(., timescale, by = "time") %>%
  select(-time)