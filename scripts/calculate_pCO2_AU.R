#Load the GLODAP data set
source(here::here("scripts",
                  "load_GLODAP_data.R"))


#Calculate MAXIMUM changes in DIC and TA due to biological drawdown of DOW
GLODAP_data <- 
  GLODAP_data %>% 
  mutate(bio_uptake = map2(NO3, PO4, plankton_biogeochem_model)) %>% 
  mutate(delta_DIC_bio_max = map_dbl(bio_uptake, 1, pluck),
         delta_TA_bio_max = map_dbl(bio_uptake, 2, pluck)) %>%
  select(-bio_uptake)