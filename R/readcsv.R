#' Gather all function to read csv in the server
#' 
#' @description This function is used to read all of the local csv files and assign to variables for later use. All csv files are stored in folder extdata nested under the inst folder.
#' 
#' @param name name of the csv file without the ".csv". Cases and typos sensitive
#' 
#' @importFrom utils read.csv
#' @importFrom dplyr mutate
#' @export

#' @examples 
#' \dontrun{patient_link <- readData("patient_link")}


readData <- function(name) {
  file_name <- list.files("inst/extdata")
  stopifnot(
    paste0(name,".csv") %in% file_name == TRUE
  )
  readData <- read.csv(system.file("extdata", paste0(name,".csv"), package = "vietnamcovid19"))
}

#' Count of Vietnam cases and deaths by province
#' 
#' vietnam_daily returns the dataset for Vietnam cases and deaths by province. 
#' Updated the latest by 10 Oct 2020. Data was collected manually from the main page of Vietnam Ministry of Health.
#' 
#' The data contained the following variables:
#' \tabular{ll}{
#'  province: \tab The provinces that reported having confirmed cases. This variable does not contain all of the provinces in  Vietnam \cr
#'  cumulative_case: \tab The number of cummulative cases by 10 October 2020 \cr
#'  Active: \tab The number of active cases that remained by 10 October 2020 \cr
#'  Recovered: \tab The number of recovered cases by 10 October 2020 \cr
#'  Death: \tab The number of death by reported by 10 October 2020 \cr
#'  }
#'  
#' @source Vietnam's MOH at <https://ncov.moh.gov.vn>
#' 
#' @export
## vietnamdaily
vietnam_daily <- readData("vn_by_province")

#' Patient infection route in Vietnam
#' 
#' This dataset contained information of patients linkage as well as other information of the infected.
#' This list is not exhaustive as not all infected sources can be identified. Information is gathered based on 
#' the MOH's daily announcement of patients' travelling history which was last updated by 10 Oct 2020.
#' 
#' The dataset contained the following variables:
#' \tabular{ll}{
#'  from: \tab The patient id of source of infection \cr
#'  to: \tab The patient id of the infected  \cr
#'  dob: \tab The date of birth of the infected \cr
#'  age: \tab The age of the infected by 2020 \cr
#'  gender: \tab The gender of the infected \cr
#'  address: \tab living address of the infected  \cr
#'  treatment_location: \tab the treatment location of the infected. Could differ from the address as patients are treated focusly in some specialised medical centres.  \cr
#'  status: \tab either "Active", "Recovered", "Deceased" or "Other". Other indicated patients'death of other causes rather than of Covid-19.  \cr
#'  type: \tab type of infection. Either "foreign expert", from oversea", "health-care worker", "illegal immigrated", "quarantine upon entry" or "social setting'. Meaning of each type is noted in the app\cr
#'  nationality: \tab nationality of the infected \cr
#'  date_announced: \tab date announced of the confirmed case \cr
#'  month_announced: \tab month announced of the confirmed case  \cr
#'  death_announced: \tab date announced of the deceased \cr
#'  vessel_no: \tab The vessel that transported the infected into Vietnam; for cases coming back from oversea only. \cr
#'  flight_from: \tab The departed destination of the infected before coming into Vietnam; for cases coming back from oversea only. \cr
#'  note: \tab Additional note \cr
#'  }
#' 
#' @source Vietnam's MOH at <https://ncov.moh.gov.vn>
#' @export
# Second tab - prepare data
## read edges and nodes
patient_link <- readData("patient_link") %>%
  mutate(date_announced = as.Date(date_announced, format = "%d-%b-%y"))

#' Vietnam patients'details
#' 
#' This dataset contained details of all the confirmed cases recorded in Vietnam by 10 October 2020.
#' 
#' The dataset would include details of each confirmed cases in Vietnam. Main information provided would include the patient id number,
#' age, address, treatment location, status, infected type, nationality, date announced and death day announced. Other variables would 
#' include the details of vessels that transported oversea patients back home and symtoms if available.
#' 
#' @source Vietnam's MOH at https://ncov.moh.gov.vn/web/guest/trang-chu
#' 
#' @export
patient_node <- readData("patient_details") %>%
  mutate(date_announced = as.Date(date_announced, format = "%d-%b-%y"))