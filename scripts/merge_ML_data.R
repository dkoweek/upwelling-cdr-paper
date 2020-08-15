#Load Takahashi et al. mixed layer chemistry climatology
source(here::here("scripts",
                  "load_ML_pCO2_data.R"))

#Load mixed layer depth climatology
source(here::here("scripts",
                  "load_MLD_data.R"))

#Join data sets (only keep rows for which pCO2 exists)
mixed_layer_data <-
  left_join(mixed_layer_chemistry_data,
            MLD_data,
            by = c("lon",
                   "lat",
                   "month"))