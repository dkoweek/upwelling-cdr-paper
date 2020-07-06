cell_grid_area <- function(lat) {
  
  earth_radius <- 6371e3 #m
  
  lat_lb <- (lat - 0.5) * (pi / 180)
  
  lat_ub <- (lat + 0.5) * (pi / 180)
  
  lon_diff <- 1 * (pi / 180) #Assume all cells 1x1 degree
  
  area <-
    (sin(lat_ub) -
       sin(lat_lb)) *
    lon_diff *
    earth_radius ^ 2
  
  return(area)

}
