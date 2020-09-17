#Load shapefile of Longhurst domains
BG_provinces_sf <- 
  str_c(BG_directory,
        "Longhurst_world_v4_2010.shp",
        sep="/") %>% 
  read_sf()

#Generate matchup table for merging with mixed layer data sets

BG_provinces_matchup_table <- 
  tibble(BG_provinces_sf) %>% 
  mutate(province_number = row_number(),
         domains = str_sub(ProvDescr,
                           start = 1,
                           end = 1)) %>% 
  mutate(domains = factor(domains)) %>% 
  select(-geometry)