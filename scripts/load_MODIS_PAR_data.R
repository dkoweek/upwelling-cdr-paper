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
  rename(E = par)