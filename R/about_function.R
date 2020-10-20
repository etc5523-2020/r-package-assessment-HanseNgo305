#' Return list of references used in the app
#' 
#' @description the ref function returns the list of R packages and external resources refered in the app. As both packages and external links are gathered into one data frame, duplication of values may occur.
#' 
#' 
#' @examples
#' ref
#' @export
ref <- data.frame("packages" = c("shiny","shinydashboard","shinydashboardPlus","tidyverse",
                                 "plotly","lubridate", "leaflet","rgdal", "tigris", 
                                 "geojsonio", "DT", "echarts4r"),
                  "link" = c("Wei, S. (2020).Covid-2019.live. Retrieved 9 October 2020, from https://covid-2019.live/en/.",
                             "Face Mask Man icon. UXWing. (2020). Retrieved 9 October 2020, from https://uxwing.com/face-mask-man-icon/", 
                             "Le, B. (2020). Vietnam Choropleth map. Raw.githubusercontent.com. Retrieved 9 October 2020, from https://raw.githubusercontent.com/lebinh/vietnam-choropleth-map/master/vn.json.",
                             "Sievert, C. (2020). 17 Server-side linking with shiny | Interactive web-based data visualization with R, plotly, and shiny. Plotly-r.com. Retrieved 9 October 2020, from https://plotly-r.com/linking-views-with-shiny.html#shiny-performance.",
                             "So ca nhiem COVID19 theo tinh thanh Viet Nam. Vi.wikipedia.org. (2020). Retrieved 9 October 2020, from https://vi.wikipedia.org/wiki/B%E1%BA%A3n_m%E1%BA%ABu:S%E1%BB%91_ca_nhi%E1%BB%85m_COVID-19_theo_t%E1%BB%89nh_th%C3%A0nh_Vi%E1%BB%87t_Nam.",
                             "TRANG TIN VE DICH BENH VIEM DUONG HO HAP CAP COVID-19 - BO Y Te(2020). Retrieved 9 October 2020, from https://ncov.moh.gov.vn/web/guest/trang-chu."
                  )
)

#' Create function to generate reference
#' 
#' @description This function provides the list of R packages and external resources added in the Reference section
#' 
#' @param type The type of references, either 'packages' or 'link'
#' 
#' @examples
#' library(shiny)
#' output_ref("link")
#' output_ref("packages")
#' @export
output_ref <- function(type) {
  vars <- c("link", "packages")

  stopifnot(
    nchar(type) > 0,
    length(type) > 0,
    type %in% vars == TRUE
  )
  
  apply(ref, 1, function(x) tags$li(tags$span(x[type])))
}
