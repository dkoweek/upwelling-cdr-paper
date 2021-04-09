#----Merge_chemistry_MLD_PAR_data----
#Load OceanSODA-ETHZ surface layer pCO2 climatology
source(here::here("scripts",
                  "load_OceanSODA_ETHZ.R"))

#Load mixed layer depth climatology
source(here::here("scripts",
                  "load_MLD_data.R"))

#Load MODIS-Aqua PAR climatology
source(here::here("scripts",
                  "load_MODIS_PAR_data.R"))

#Join data sets
mixed_layer_data <-
  inner_join(mixed_layer_chemistry_data,
            MLD_data,
            by = c("lon",
                   "lat",
                   "month")) %>% 
  left_join(.,
            PAR_climatology,
            by = c("lon",
                   "lat",
                   "month"))
  

#----Assign_biogeographic_domain----

#Assign biogeochemical domain
source(here::here("scripts",
                  "load_phyto_biogeo_provinces.R"))

coords <- 
  mixed_layer_data %>% 
  select(c(lon, lat)) %>% 
  distinct() 

bg_domain_matchup <- 
  coords %>% 
  st_as_sf(coords = c('lon', 'lat'),
           crs = st_crs(BG_provinces_sf)) %>% 
  mutate(province_number = as.integer(st_within(geometry,
                                                BG_provinces_sf))) %>% 
  tibble() %>%
  select(province_number) %>% 
  bind_cols(coords) %>% 
  left_join(.,
            BG_provinces_matchup_table,
            by = "province_number")

mixed_layer_data <- 
  mixed_layer_data %>% 
  left_join(.,
            bg_domain_matchup,
            by = c("lon",
                   "lat"))

#----Assign_E_k_value----
source(here::here("scripts",
                  "load_E_k_data.R"))

mixed_layer_data <- 
  mixed_layer_data %>% 
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
  left_join(.,
            E_k_medians,
            by = c("domains",
                   "hemisphere",
                   "season"))

#----Calculate_monthly_light_limitation----
mixed_layer_data <- 
  mixed_layer_data %>% 
  mutate(epsilon_micro = tanh(E / E_k_median_micro),
         epsilon_macro = tanh(E / E_k_median_macro))