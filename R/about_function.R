#' Create function to generate reference for libraries used
#' 
#' @export

# add further packages if needed
output_ref <- function() {
  ref <- data.frame("packages" = c("shiny","shinydashboard","shinydashboardPlus","tidyverse",
                                          "plotly","lubridate", "leaflet","rgdal", "tigris", 
                                          "geojsonio", "DT", "echarts4r")
  )
  
  apply(ref, 1, function(x) tags$li(tags$span(x['packages'])))
}

#' @export
# add further references if needed

external_ref <- function() {
  sources <- data.frame("link" = c("Wei, S. (2020). 🦠新型コロナウイルス感染速報. Covid-2019.live. Retrieved 9 October 2020, from https://covid-2019.live/en/.",
                                   "Face Mask Man icon. UXWing. (2020). Retrieved 9 October 2020, from https://uxwing.com/face-mask-man-icon/", 
                                   "Le, B. (2020). Vietnam Choropleth map. Raw.githubusercontent.com. Retrieved 9 October 2020, from https://raw.githubusercontent.com/lebinh/vietnam-choropleth-map/master/vn.json.",
                                   "Sievert, C. (2020). 17 Server-side linking with shiny | Interactive web-based data visualization with R, plotly, and shiny. Plotly-r.com. Retrieved 9 October 2020, from https://plotly-r.com/linking-views-with-shiny.html#shiny-performance.",
                                   "Số ca nhiễm COVID-19 theo tỉnh thành Việt Nam. Vi.wikipedia.org. (2020). Retrieved 9 October 2020, from https://vi.wikipedia.org/wiki/B%E1%BA%A3n_m%E1%BA%ABu:S%E1%BB%91_ca_nhi%E1%BB%85m_COVID-19_theo_t%E1%BB%89nh_th%C3%A0nh_Vi%E1%BB%87t_Nam.",
                                   "TRANG TIN VỀ DỊCH BỆNH VIÊM ĐƯỜNG HÔ HẤP CẤP COVID-19 - Bộ Y tế - Trang tin về dịch bệnh viêm đường hô hấp cấp COVID-19. Bộ Y tế - Trang tin về dịch bệnh viêm đường hô hấp cấp COVID-19. (2020). Retrieved 9 October 2020, from https://ncov.moh.gov.vn/web/guest/trang-chu."
  )
  )
  
  apply(sources, 1, function(x) tags$li(tags$span(x['link'])))
}
