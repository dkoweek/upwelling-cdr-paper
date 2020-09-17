# Load MAPPS database of global P-I measurements

#-----Load_MAPPS_data----

MAPPS_list <-
  pg_data(doi = "10.1594/PANGAEA.874087")

MAPPS_data <- 
  MAPPS_list[[1]][["data"]]

longhurst_domains <- #As listed in Table 1 of Bouman et al.
  #P=Polar, W=Westerlies, T=Trades, C=Coastal
  tibble(
    BG_province = c(seq(1,12), 15, 17, 18, 20, 21, 22, 
                    30, 33, 34, 37, 50, 51, 53, 58, 60, 
                    63, 64, 68, 69, seq(80, 83)),
    domains = c("P", "P", "P", "W", "W", "W", "T", "T", 
                "T", "T", "C", "C", "C", "T", "W", "C", 
                "C", "C", "T", "C", "C", "C", "P", "W", 
                "W", "W", "T", "T", "T", "C", "C", "W", 
                "W", "P", "P")
  )

#----Wrangle_MAPPS_data----
MAPPS_data <- 
  MAPPS_data %>% 
  rename(depth_m = `Depth water [m]`,
         E_k = `Ek [µmol/m**2/s]`,
         lon = Longitude,
         lat = Latitude,
         BG_province = `BG province`) %>% 
  mutate(ymd = lubridate::ymd(`Date/Time`),
         year = lubridate::year(ymd),
         month = as.integer(lubridate::month(ymd)),
         E_k = E_k * ((24 * 3600) / 1e6)) %>% #umol/m^2/s -> mol/m^2/d (to align with MODIS data) 
  group_by(BG_province) %>% 
  nest() %>% 
  left_join(.,
            longhurst_domains,
            by = "BG_province") %>% 
  mutate(domains =  factor(domains)) %>% 
  unnest(cols = c(data)) %>% 
  ungroup() %>% 
  mutate(hemisphere = case_when(lat < 0 ~ "S",
                                TRUE ~ "N"),
         season = case_when(month %in% c(3,4,5) ~ "MAM",
                            month %in% c(6,7,8) ~ "JJA",
                            month %in% c(9,10,11) ~ "SON",
                            TRUE ~ "DJF")) %>% 
  mutate(season = factor(season, levels = c("DJF",
                                            "MAM",
                                            "JJA",
                                            "SON")),
         hemisphere = factor(hemisphere, levels = c("S", "N"))) %>% 
  filter(depth_m < 20) #to ensure surface samples

#----Summarize_by_domain_hemisphere_season----
E_k_boxplots <- 
  MAPPS_data %>% 
  ggplot(aes(x = season,
             y = E_k)) +
  geom_boxplot() +
  facet_grid(domains ~ hemisphere,
             scales = "free_y")

#Take medians for each group
E_k_medians <- 
  MAPPS_data %>% 
  group_by(domains, hemisphere, season) %>% 
  summarize(E_k_median = median(E_k), 
            .groups = "keep")

#Interpolate for missing domain/seasons/hemisphere 
E_k_interpolated <- 
  E_k_medians %>% 
  filter(hemisphere == "S",
         domains %in% c("T", "C")) %>% 
  group_by(domains) %>% 
  summarize(E_k_median = mean(E_k_median),
            .groups = "keep") %>% 
  mutate(season = factor("DJF"),
         hemisphere = factor("S")) %>% 
  relocate(E_k_median, .after = season) %>% 
  relocate(hemisphere, .before = season) %>% 
  bind_rows(., (.)%>% mutate(season = factor("JJA"))) %>% 
  ungroup() %>% 
  slice(-3)
  


#Return interpolated results to summary table
E_k_medians <-
  bind_rows(E_k_medians,
            E_k_interpolated) %>% 
  arrange(domains,
          hemisphere,
          season)