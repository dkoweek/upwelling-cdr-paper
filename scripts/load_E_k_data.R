#-----Load_MAPPS_data----
# Load MAPPS database of global P-I measurements for phytoplankton communities
# Bouman, HA et al. (2018): Photosynthesis-irradiance parameters...
#...of marine phytoplankton: synthesis of a global data set. 
# Earth System Science Data, 10, 251-266

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
  summarize(E_k_median_micro = median(E_k), 
            .groups = "keep")

#Interpolate for missing domain/seasons/hemisphere 
E_k_interpolated <- 
  E_k_medians %>% 
  filter(hemisphere == "S",
         domains %in% c("T", "C")) %>% 
  group_by(domains) %>% 
  summarize(E_k_median_micro = mean(E_k_median_micro),
            .groups = "keep") %>% 
  mutate(season = factor("DJF"),
         hemisphere = factor("S")) %>% 
  relocate(E_k_median_micro, .after = season) %>% 
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

#----Add_E_k_for_macroalgae----
#The E_k from two studies focused on red, green, and brown macroalgae...
#...find a pretty consistent median E_k of 125 umol photons/m^2/s

#Johansson, G., and P. Snoeijs. "Macroalgal photosynthetic responses...
#to light in relation to thallus morphology and depth zonation."...
#Marine Ecology Progress Series 244 (2002): 63-72.
#(n=35 samples from the Baltic Sea area)

#Gómez, Iván, et al. "Patterns of photosynthesis in 18 species of...
#intertidal macroalgae from southern Chile." ...
#Marine Ecology Progress Series 270 (2004): 103-116.

E_k_median_macro <- 125 #umol/m2/s

E_k_medians <- 
  E_k_medians %>% 
  mutate(E_k_median_macro = E_k_median_macro * ((24 * 3600) / 1e6)) #umol/m^2/s -> mol/m^2/d (to align with MODIS data)