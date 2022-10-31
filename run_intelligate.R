# run_intelligate is a function to run Intelligate for identification of sub-population from flow cytometry data
run_intelligate <- function (fs_df){

  source("cluster.R")
  answer <- userQuery("\nBegin Intelligate clustering analysis?", default = "n")
  
  num_experiments <- length(unique(fs_df$names))
  filename_index <- tibble("Filename" = unique(fs_df$names)) %>% rownames_to_column("Experiment")
  
  if(answer == "y"){
    option <- userQuery("\nWhat would you like Intelligate to output?\n(1) Ellipses (useful for informing user gating strategy).\n(2) Classification (useful for downstream analyses of sub-population.)", default = "ellipses", allowed = c("1", "2"))
    if(option == "1"){
      cat("\nOK, initiating Intelligate clustering to output ellipses...")
      output <- map_dfr(.x = 1:num_experiments, .f = ~cluster(dataframe = fs_df, what = "ellipses", experiment_number = .x), .id = "Experiment") %>% left_join(filename_index, by = "Experiment")
    } else if (option == "2"){
      cat("\nOK, initiating Intelligate clustering to output classification...")
      output <- map_dfr(.x = 1:num_experiments, .f = ~cluster(dataframe = fs_df, what = "classification", experiment_number = .x), .id = "Experiment") %>% left_join(filename_index, by = "Experiment")
    }
    cat("Intelligate completed!")
  }
  if (answer == "n"){
    stop("Intelligate aborted.")
  }
  
  return(output)
}