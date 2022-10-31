# import_data is a function to import flow cytometry data from .fcs files, pre-process, and output a results in a dataframe
import_data <- function(filepath){
  
  cat("Importing .fcs files...")
  
  fs <- read.flowSet(path = filepath, pattern = ".fcs", alter.names = TRUE) # import .fcs files into a flowset
  
  # create dataframe from flowset:
  fs_df <- 
    flowSet2LongDf(fs) %>% 
    drop_na() %>% # remove events with any NA fluorescent measurements
    filter(if_all(where(is.numeric), is.finite)) %>% # remove events with any infinite fluorescent measurements
    filter(if_all(c(where(is.numeric), -Time), ~. > 0)) %>% # remove events with any negative fluorescent measurements
    mutate(across(c(where(is.numeric), -Time), ~log(., base = 10))) %>% # log-transform fluorescent measurements
    select(-Time, -acqDate)
  
  cat("\nImport completed.")
  
  return(fs_df)
}
