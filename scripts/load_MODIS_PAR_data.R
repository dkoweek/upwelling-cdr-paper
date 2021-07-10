#----Match_file_to_month----
monthly_PAR_grids <- 
  list.files(str_c(MODIS_directory,
                   "regridded",
                   sep="/"))

month_index <-
  str_sub(monthly_PAR_grids,
          start = 6,
          end = 8) %>% 
  as.numeric() %>% 
  order()

#----Open_each_file_and_build_master_data_frame----
PAR_climatology_list <- list()
for (i in 1:length(monthly_PAR_grids)) {
  
  df <- 
    tidync(str_c(MODIS_directory,
                 "regridded",
                 monthly_PAR_grids[i],
                 sep="/")) %>% 
    hyper_tibble() %>% 
    mutate(month = as.integer(month_index[i]))
  
  PAR_climatology_list[[i]] <- df
  
}

#Reorder so January comes first
PAR_climatology_list <- 
  PAR_climatology_list[month_index]

#Build data frame from list
PAR_climatology <- 
  do.call(rbind.data.frame,
          PAR_climatology_list) %>% 
  as_tibble() %>% 
  rename(E_bar_diel_cycle = par)

#----Remove_grid_cells_without_12_months_of_data----
PAR_climatology <- 
  PAR_climatology %>% 
  group_by(lon, lat) %>% 
  nest() %>% 
  mutate(n_months = map_dbl(data, nrow)) %>% 
  filter(n_months == 12) %>% 
  unnest(cols = data) %>% 
  ungroup() %>% 
  select(-n_months)

#----Calculate_photoperiod_and_average_light_during_photoperiod_by_lat_and_date----
PAR_climatology <- 
  PAR_climatology %>% 
  mutate(days_month = map(month, ~ seq(1, days_in_month(.x)))) %>% 
  mutate(datestring = map2(month, days_month, ~ date(str_c("2021-",.x,"-",.y) ))) %>% 
  mutate(photoperiod = map2(lat, datestring, ~ daylength(.x, .y))) %>% 
  mutate(mean_photoperiod = map_dbl(photoperiod, mean)) %>% 
  mutate(f_light = mean_photoperiod / 24) %>% #Fraction of each day in the photoperiod
  mutate(E = E_bar_diel_cycle / f_light) %>%  #Average PAR during the photoperiod
  select(-c(days_month, photoperiod, datestring))
  
  