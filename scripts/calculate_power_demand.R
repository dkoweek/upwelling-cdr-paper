#Calculate the power demand of upwelling

#----Load_scripts_and_functions----
#Load the GLODAP data set
source(here::here("scripts",
                  "load_GLODAP_data.R"))

#Function to calculate mean density between surface and depth z
mean_density <- function(data) {
  
  if (min(data$depth_m) != 0) {
    return(NA)
  }
  
  if (nrow(data) == 1) {
    return(NA)
    
  } else {
    depth_density_function <-
      approxfun(x = data$depth_m,
                y = data$rho)
    
  }

  rho_bar_z <- c()
  rho_bar_z[1] <- data$rho[1]

  for (i in 2:nrow(data)) {
    rho_bar_z[i] <-
      (1 / data$depth_m[i]) *
      (
        integrate(depth_density_function,
                lower = 0,
                upper = data$depth_m[i]) %>% 
         pluck("value")
      )
  }

  return(rho_bar_z)
}

#Function to calculate the power demand of upwelling
power_demand <- function(rho_z, r, v, rho_bar, d, f_D = 0.06, alpha = 0.8) {
  
  #Parameters
  g <- 9.81 #m/s^2
  #f_D = Darcy-Weisbach friction factor coefficient (upper bound of ~0.06 for turbulent flows)
  #alpha = pump efficiency (%)
  
  power_to_upwell <- #Watts
    (rho_z * pi * r ^ 2 * v) *
    (
      (
        (1-
           (rho_bar / rho_z)
        ) *
          (g * d)
      ) +
        (v^2 /2) +
      (
        (f_D * d * v^2) /
          (4 * r)
      )
    )
  
  power_to_upwell <- 
    power_to_upwell / alpha #pipe efficiency
  
  return(power_to_upwell)
  
  
}

#----Set_parameters----
#Upwelling pipe geometry
v <- 0.01 #upwelling velocity (m/s)

r <- 0.5 #pipe radius

Re <- (1025 * v * 2 * r) / 1e-3 #Reynolds number (all scenarios point to turbulent flow)

#----Calculate_power_demand----
power_demand_df <- 
  GLODAP_data %>% 
  mutate(rho = swRho(salinity = S,
                     temperature = T,
                     pressure = depth_m)) %>% 
  group_by(lon, lat) %>% 
  nest() %>% 
  mutate(rho_bar_z = map(data, mean_density)) %>% 
  unnest(cols = c(data, rho_bar_z)) %>% 
  ungroup() %>% 
  mutate(power_need = power_demand(rho_z = rho,
                                   r = r,
                                   v = v,
                                   rho_bar = rho_bar_z,
                                   d = depth_m),
         power_need = power_need / 1000 #W -> kW
         )
