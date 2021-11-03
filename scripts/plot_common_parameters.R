#Set common parameters to be shared across plots here

models <- c("galbraith", "garcia_q1", "garcia_q3", "redfield", "redfield_P_limited", "atkinson_q1", "atkinson_med", "atkinson_q3")
model_titles <- c("Galbraith",
                  "Garcia Q1",
                  "Garcia Q3",
                  "Redfield ",
                  "Redfield P-limited",
                  "Atkinson Q1",
                  "Atkinson Median",
                  "Atkinson Q3")

m2_to_km2 <- 1e6 #m^2 per km^2

depth_category_labels <-
  c("<500 (m)",
    "All Depths")