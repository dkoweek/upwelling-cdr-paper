#----Open_as_tibble----
mixed_layer_chemistry_data_file <-
  str_c(OceanSODA_ETHZ_directory,
        "OceanSODA-ETHZ_v2020a.nc",
        sep = "/")

mixed_layer_chemistry_data <- 
  tidync(mixed_layer_chemistry_data_file) %>% 
  hyper_tibble(select_var = c("pCO2", "salinity", "temperature"))

#----Acquire_month_data----
#time listed as days since 1-Jan-1985
time_units <-
  ncmeta::nc_atts(mixed_layer_chemistry_data_file, "time") %>% 
  tidyr::unnest(cols=c(value))

timescale <- 
  mixed_layer_chemistry_data %>% 
  select(time) %>%
  unique() %>% 
  mutate(date = ymd("1985-01-01") + time,
         month = month(date)
         ) %>% 
  select(-date)

mixed_layer_chemistry_data <- 
  mixed_layer_chemistry_data %>% 
  left_join(., timescale, by = "time") %>%
  select(-time)

#----Take_climatologial_mean_at_every_x_y_t_combination----
mixed_layer_chemistry_data <-
  mixed_layer_chemistry_data %>% 
  group_by(lon, lat, month) %>%
  dplyr::summarize(pCO2 = mean(pCO2, na.rm = TRUE),
                   salinity = mean(salinity, na.rm = TRUE),
                   temperature = mean(temperature, na.rm = TRUE),
                   .groups = "keep") %>% 
  ungroup()

#----Keep_locations_with_12_month_of_data----
mixed_layer_chemistry_data <- 
  mixed_layer_chemistry_data %>% 
  group_by(lon, lat) %>% 
  nest() %>% 
  mutate(n_months = map_dbl(data, nrow)) %>% 
  filter(n_months == 12) %>% 
  unnest(cols = data) %>% 
  ungroup() %>% 
  select(-n_months)

#----Calculate_CO2_concentration----
#Saves time to calculate ML CO2 before CO2 gradient calculations

mixed_layer_chemistry_data <- 
  mixed_layer_chemistry_data %>% 
  mutate(K_0 = K0(S = salinity,
                  T = temperature,
                  P = 0,
                  warn = "n"),
         CO2 = K_0 * (pCO2 / 1e6),
         CO2 = as.numeric(CO2))