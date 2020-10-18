#' Read data and clean up for number of cases, deaths by days
#' 
#' @export

data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")
raw_vietnam <- data %>%
  filter(countriesAndTerritories == "Vietnam") %>%
  select(dateRep, day, month, year, cases, deaths, popData2019) %>%
  arrange(year, month, day) %>%
  mutate(cumulative_case = cumsum(cases),
         cumulative_death = cumsum(deaths)) %>%
  mutate(dateRep = dmy(dateRep))