# query_path is a function to confirm the directory holding flow cytometry data in .fcs files
query_path <- function(){

  cat(sprintf("\nThe current working directory is: %s\n", here::here()))
  answer <- userQuery("Is this working directory correct?", default = "n")
  
  if (answer == "y"){
    cat("\nGreat!")
  }
  if (answer == "n"){
    new_wd <- readline(prompt = "Confirm the new working directory: ")
    setwd(new_wd)
    cat(sprintf("\nThe new working directory is: %s\n", here::here()))
  }
  
  folder <- readline(prompt = "Enter the name of the folder containing .fcs data for analysis:\n")
  filepath <-  paste(here::here(), folder, sep = "/")
  cat(sprintf("Intelligate will now import all .fcs data files from %s:\n", filepath))
  cat(list.files(path = filepath, pattern = ".fcs"), sep = "\n")
  answer <- userQuery("Is this correct?", default = "n")
  
  if (answer == "y"){
    print("Great!")
  }
  if (answer == "n"){
    folder <- readline(prompt = "Confirm the name of the folder containing .fcs data for analysis: ")
    filepath <-  paste(here::here(), folder, sep = "/")
    cat(sprintf("Intelligate will now import all .fcs data files from %s:\n", filepath))
    cat(list.files(path = filepath, pattern = ".fcs"), sep = "\n")
  }
  
  return(filepath)
}