depth_surfaces_matchup_table <- 
  read_csv(str_c(input_data_directory,
                 "/",
                 "depth_surfaces_matchup_table.csv",
                 sep = ""),
           comment = "#",
           col_types = list(col_integer(),
                            col_double()))