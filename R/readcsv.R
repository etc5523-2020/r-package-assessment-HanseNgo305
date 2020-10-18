#' Gather all function to read csv in the server
#' 
#' @export

readData <- function(name) {
  readData <- read.csv(system.file("extdata", paste0(name,".csv"), package = "vietnamcovid19"))
}

## vietnamdaily
vietnam_daily <- readData("vn_by_province")

# Second tab - prepare data
## read edges and nodes
patient_link <- readData("patient_link")
patient_node <- readData("patient_details") %>%
  mutate(date_announced = as.Date(date_announced, format = "%d-%b-%y"))

