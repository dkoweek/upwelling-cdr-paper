depth_surfaces_matchup_table <- 
  read_csv(here::here("data",
                      "input_data",
                      "depth_surfaces_matchup_table.csv"),
           comment = "#",
           col_types = list(col_integer(),
                            col_double()))