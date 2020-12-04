#Wrangle data from Atkinson and Smith 1983


atkinson_df <- 
  read_csv(file = str_c(input_data_directory,
                        "Atkinson_Smith_1983.csv",
                        sep = "/"),
           col_types = cols(
             Taxon = col_character(),
             Plant_part = col_character(),
             Phylum = col_character(),
             Locality = col_character(),
             C_percent = col_character(),
             C_N_P = col_character(),
             References = col_character()
             ),
           na = c("-", ""),
           ) %>% 
  mutate(C_N = str_extract(string = C_N_P,
                         pattern = "\\d+:\\d+"),
         C_P = str_extract(string = C_N,
                         pattern = "\\d+"),
         N_P = str_extract(string = C_N,
                         pattern = "\\d+$")) %>% 
  select(-C_N) %>% 
  mutate(C_P = as.numeric(C_P),
         N_P = as.numeric(N_P),
         C_N = C_P / N_P) %>% 
  filter(Phylum %in% c("P", "Ch"))

#Arrange data frame by N:P ratio
N_P_ecdf <- 
  ecdf(atkinson_df$N_P)

atkinson_df <-
  atkinson_df %>% 
  mutate(N_P_quantile = N_P_ecdf(N_P)) %>% 
  arrange(N_P_quantile)

atkinson_model <- function(NO3, PO4, percentile = 0.5) {
  
  #Grab the N_P ratio matching the input percentile
  data <- 
    atkinson_df %>% 
    mutate(quantile_diff = abs(percentile - N_P_quantile)) %>% 
    slice(which.min(quantile_diff))
  
  N_P_hat <- 
    data %>%
    pull(N_P)
  
  #Calculate the DIC and TA changes from both N and P limitation
  P_limited <-
    if_else((NO3 / PO4) >= N_P_hat, 1, 0)
  
  #P-limited
  delta_DIC_bio_P_limited <- 
    data %>% 
    pull(C_P) %>% 
    `*`(PO4)
  
  delta_N_bio_P_limited <- 
    data %>% 
    pull(N_P) %>% 
    `*`(PO4)
  
  N_limited <- 
    if_else((NO3 / PO4) >= N_P_hat, 0, 1)
  
  #N-limited
  delta_DIC_bio_N_limited <- 
    data %>% 
    pull(C_N) %>% 
    `*`(NO3)
  
  delta_N_bio_N_limited <- 
    NO3
  
  #Combine N and P limitation calculations in vectorized fashion
  #Ultimate result is DIC or TA modification due to only N OR P limitation, not both
  delta_DIC_bio <- 
    -1* (
      (P_limited * delta_DIC_bio_P_limited) + 
        (N_limited * delta_DIC_bio_N_limited)
    )
  
  delta_TA_bio <- 
    (
      (P_limited * delta_N_bio_P_limited) + 
        (N_limited * delta_N_bio_N_limited)
    )
  
  
  return(list(delta_DIC_bio = delta_DIC_bio,
              delta_TA_bio = delta_TA_bio))
  
}

