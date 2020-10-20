#' Gather all function to read csv in the server
#' 
#' @description This function is used to read all of the local csv files and assign to variables for later use
#' 
#' @param name name of the csv file with out the ".csv". Cases and typos sensitive
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

#' Read the csv file for Vietnam cases by province
#' 
#' @description Return the dataset for Vietnam cases and deaths by province updated the latest by 10 Oct 2020
#' @export
## vietnamdaily
vietnam_daily <- readData("vn_by_province")

#' Read the csv file for the patient infection route in Vietnam, last updated by 10 Oct 2020
#' 
#' @description Return the dataset for patient infection route.
#' @export
# Second tab - prepare data
## read edges and nodes
patient_link <- readData("patient_link") %>%
  mutate(date_announced = as.Date(date_announced, format = "%d-%b-%y"))

#' Read the csv file for details of infected patients in Vietnam
#' 
#' @description Return the dataset for patient details.
#' 
#' @details 
#' The dataset would include details of each confirmed cases in Vietnam. Main information provided would include the patient id number,
#' age, address, treatment location, status, infected type, nationality, date announced and death day announced. Other variables would 
#' include the details of vessels that transported oversea patients back home and symtoms if available.
#' 
#' @source Vietnam's MOH at https://ncov.moh.gov.vn/web/guest/trang-chu
#' @export
patient_node <- readData("patient_details") %>%
  mutate(date_announced = as.Date(date_announced, format = "%d-%b-%y"))