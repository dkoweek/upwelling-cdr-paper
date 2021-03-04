#Wrangle data from Atkinson and Smith 1983

#Read in data
atkinson_df_all <- 
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
         C_N = C_P / N_P)

#Select all of the macroalgal phyla
atkinson_macroalgae_df <- 
  atkinson_df_all %>% 
  filter(Phylum %in% c("P", "Ch", "R")) %>% 
  filter(N_P < 150)  #Remove outlier

#C:P vs. N:P
atkinson_macroalgae_regression <-
  lm(C_P ~ N_P, 
     data = atkinson_macroalgae_df)

#Summarize N:P and then calculate C:P at summary statistic values
atkinson_macroalgae_summary <- 
  summary(atkinson_macroalgae_df$N_P) %>% 
  broom::tidy() %>% 
  pivot_longer(cols = minimum:maximum,
               names_to = "statistic",
               values_to = "N_P") %>% 
  mutate(C_P = as.numeric(predict(object = atkinson_macroalgae_regression,
                       newdata = .)))


#Function for converting nutrient ratio model into DIC and TA uptake
atkinson_model <- function(NO3, PO4, metric = "median") {
  
  #Grab the N_P and C_P ratio matching the statistic
  N_P <- 
    atkinson_macroalgae_summary[[which(atkinson_macroalgae_summary[["statistic"]] == metric),"N_P"]]
  
  C_P <- 
    atkinson_macroalgae_summary[[which(atkinson_macroalgae_summary[["statistic"]] == metric),"C_P"]]
  
  #Calculate the DIC and TA changes from both N and P limitation
  P_limited <-
    if_else((NO3 / PO4) >= N_P, 1, 0)
  
  #P-limited
  delta_DIC_bio_P_limited <- 
    C_P * PO4
  
  delta_N_bio_P_limited <- 
    N_P * PO4
  
  N_limited <- 
    if_else((NO3 / PO4) > N_P, 0, 1)
  
  #N-limited
  delta_DIC_bio_N_limited <- 
    (C_P / N_P) * NO3
  
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

