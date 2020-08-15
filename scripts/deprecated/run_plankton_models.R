#----Load_packages_variables_and_functions----
source(here::here("scripts",
                  "initialize_workspace.R"))

#----Load_the_input_data----
source(here::here("scripts",
                  "load_input_data.R"))

#----Initialize_the_input_data-----
#Calculate mixed layer concentrations
data_0 <- 
  initialize_ML_concentrations(input_data)

#Filter the depth range
data_0 <- 
  data_0 %>% 
  filter(depth_m >= MLD & depth_m <= 500)

#Determine the potential biological drawdown
data_0 <-
  biological_uptake_potential(data_0)

#----Load_model_run_parameters----
model_runs <- 
  generate_run_parameters_table(n_years = 10,
                                n_pipes = c(1e2, 1e3, 1e4),
                                Q_dot = 1)

#----Run_the_pumping_model----

for (n in 1:nrow(model_runs)) {
  
  # plankton_AU_CDR_mixing_model <-
  #   mixed_layer_pumping_model(n_years = model_runs[["n_years"]][n],
  #                             n_pipes = model_runs[["n_pipes"]][n],
  #                             Q_dot = model_runs[["Q_dot"]][n],
  #                             data_set = test)
  # 
  # plankton_AU_CDR_ML_CO2 <- 
  #   CO2_from_mixing_model(data_set = plankton_AU_CDR_mixing_model,
  #                         n_years = model_runs[["n_years"]][n]) %>% 
  #   select(c(lon,lat,depth_m,area,contains("CO2")))
  

  finish_timestamp <- 
    format(Sys.time(), "%Y%m%d%H%M%S")
  
  model_runs[["run_start"]][n] <- finish_timestamp
  
  save_model_run(model_output = plankton_AU_CDR_ML_CO2,
                 model_tag = finish_timestamp)
  
  
}

