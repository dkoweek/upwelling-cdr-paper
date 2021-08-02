#Set common parameters to be shared across plots here

models <- c("galbraith", "garcia_q1", "garcia_q3", "redfield", "redfield_P_limited", "atkinson_q1", "atkinson_med", "atkinson_q3")
model_titles <- c("Galbraith et al. (2015)",
                  "Garcia et al. (2018) Q1 N:P",
                  "Garcia et al. (2018) Q3 N:P",
                  "Redfield et al. (1934)",
                  "Redfield et al. (1934): P-limited",
                  "Atkinson & Smith (1983): Q1 N:P",
                  "Atkinson & Smith (1983): Median N:P",
                  "Atkinson & Smith (1983): Q3 N:P")

m2_to_km2 <- 1e6 #m^2 per km^2

depth_category_labels <-
  c("<500 (m)",
    "All Depths")