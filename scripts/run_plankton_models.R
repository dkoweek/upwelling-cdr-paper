#----Initialize_workspace----
source(here::here("scripts",
                  "initialize_workspace.R"))

#----Load_the_input_data----
source(here::here("scripts",
                  "load_input_data.R"))

#----Load_model_run_parameters----
model_runs <- 
  generate_run_parameters_table(n_years = 10,
                                n_pipes = c(1e2, 1e3, 1e4),
                                Q_dot = 1)

#----Initialize_the_model-----
#Determine the initial mixed layer concentrations
data_0 <- 
  initialize_ML_concentrations(input_data)

#Determine the potential biological drawdown
data_0 <-
  biological_uptake_potential(data_0)

#----Run_the_mixing_model----

for (n in 1:nrow(model_runs)) {
  
  plankton_AU_CDR_model <-
    mixed_layer_pumping_model(n_years = model_runs[["n_years"]][n],
                              n_pipes = model_runs[["n_pipes"]][n],
                              Q_dot = model_runs[["Q_dot"]][n],
                              data_set = test)
  
  finish_timestamp <- 
    format(Sys.time(), "%Y%m%d%H%M%S")
  
  model_runs[["run_start"]][n] <- finish_timestamp
  
  save_model_run(model_output = plankton_AU_CDR_model,
                 model_tag = finish_timestamp)
  
  
}

