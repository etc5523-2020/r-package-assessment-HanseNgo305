#' Gather all function to read csv in the server
#' 
#' @description This function is used to read all of the local csv files and assign to variables for later use
#' 
#' @importFrom utils read.csv
#' @export

readData <- function(name) {
  readData <- read.csv(system.file("extdata", paste0(name,".csv"), package = "vietnamcovid19"))
}

#' @export
## vietnamdaily
vietnam_daily <- readData("vn_by_province")

#' @export
# Second tab - prepare data
## read edges and nodes
patient_link <- readData("patient_link")

#' @export
patient_node <- readData("patient_details")