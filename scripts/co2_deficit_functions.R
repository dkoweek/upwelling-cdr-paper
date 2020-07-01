dCO2_dT <- function(T, S, TA, DIC, lon, lat) {
  
  deriv <- 
    derivnum(varid = "t",
             flag = 15,
             var1 = TA / 1e6,
             var2 = DIC / 1e6,
             S = S,
             T = T,
             lon = lon,
             lat = lat,
             warn = "n") %>% 
    pull("CO2")
    
  return(deriv) #mol CO2/kg SW/deg C
  
}

dCO2_dS <- function(T, S, TA, DIC, lon, lat) {
  
  deriv <- 
    derivnum(varid = "s",
             flag = 15,
             var1 = TA / 1e6,
             var2 = DIC / 1e6,
             S = S,
             T = T,
             lon = lon,
             lat = lat,
             warn = "n") %>% 
    pull("CO2")
  
  return(deriv) #mol CO2/kg SW/psu
    
}

dCO2_dTA <- function(T, S, TA, DIC, lon, lat) {
  
  deriv <- 
    derivnum(varid = "var1",
             flag = 15,
             var1 = TA / 1e6,
             var2 = DIC / 1e6,
             S = S,
             T = T,
             lon = lon,
             lat = lat,
             warn = "n") %>% 
    pull("CO2")
  
  deriv <- 
    deriv / 1e6 #mol CO2/kg  SW/ mol TA/kg  SW-> mol CO2/kg SW / umol TA/kg SW
  
  return(deriv) 
  
}

dCO2_dDIC <- function(T, S, TA, DIC, lon, lat) {
  
  deriv <- 
    derivnum(varid = "var2",
             flag = 15,
             var1 = TA / 1e6,
             var2 = DIC / 1e6,
             S = S,
             T = T,
             lon = lon,
             lat = lat,
             warn = "n") %>% 
    pull("CO2")
  
  deriv <- 
    deriv / 1e6 #mol CO2/kg  SW/ mol DIC/kg  SW-> mol CO2/kg SW / umol DIC/kg SW
  
  return(deriv) 
  
}