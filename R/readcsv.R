#' Gather all function to read csv in the server
#' 
#' @description This function is used to read all of the local csv files and assign to variables for later use
#' 
#' @importFrom utils read.csv
#' @importFrom dplyr mutate
#' @export

readData <- function(name) {
  file_name <- list.files("inst/extdata")
  
  stopifnot(
    paste0(name,".csv") %in% file_name == TRUE
  )
  readData <- read.csv(system.file("extdata", paste0(name,".csv"), package = "vietnamcovid19"))
}

#' @export
## vietnamdaily
vietnam_daily <- readData("vn_by_province")

#' @export
# Second tab - prepare data
## read edges and nodes
patient_link <- readData("patient_link") %>%
  mutate(date_announced = as.Date(date_announced, format = "%d-%b-%y"))

#' @export
patient_node <- readData("patient_details") %>%
  mutate(date_announced = as.Date(date_announced, format = "%d-%b-%y"))