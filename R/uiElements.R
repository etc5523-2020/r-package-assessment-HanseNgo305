#' UI Elements
#'
#'
#' @export

#### Bulletin Board ####

bulletinNote <- function() {
  column(5, 
         tags$h3("Vietnam case map by 6-Oct-2020"),
         tags$hr(),
         leafletOutput("vietnammap"), height = "800px",
         tags$br(),
         tags$h3("Observations and Notes"),
         tags$hr(),
         tags$h5("Note"),
         tags$p("Data used for this section was collected manually from official Governmental sources. 
                 However, they may not reflect the actual numbers by the time you read this report as data was not updated automatically.
                 The latest date of this report is 10 October 2020. Please proceed with that in mind."),
         tags$br(),
         tags$h5("Observation"),
         tags$div(tags$ul("From the bar graph we can see that Vietnam had three outbreaks from January: ",
                          tags$li(tags$span("One occured around March and April from Bach Mai hospital in Hanoi where a source of infection was determined to be from a 
                                  catering company named Truong Sinh who in charged of canteen services for the hospital. Another case of patient number 17 announced on 6 March 20 also put an end to a month of Covid - free in Vietnam.")),
                          tags$li(tags$span("Another minor outbreak occured in May when the Government dispatched flights to all over the world to rescue citizens stuck oversea. A hugh flood of international returns had pushed up the figures for a short time.")),
                          tags$li(tags$span("The last peak and the most severed occured in July where a case of illegal immigration from China entered Danang and turned its biggest hospital to one of a huge infection source. This was also when Vietnam started to record deaths as patients already had a complex history of medical conditions, being the patients of the said hospital.")),
         ))
  )
}

#' @export
##### About#####
aboutNote <- function() {
  library_used <- c("shiny", "shinydashboard","shinydashboardPlus","tidyverse","plotly",
                    "lubridate", "leaflet","rgdal", "tigris", "geojsonio", "DT", "echarts4r")
  column(7,
         tags$h4("About this app"),
         tags$hr(),
         tags$body("This app was created with an original purpose as an assignment to submit to a university class. 
                        However, I hope this app will be put into a greater use than what it was meant to be. 
                        Compared to other countries in the area, Vietnam lacked an effective channel to communicate with people.
                        The Government set up one main channel at the MOH website (link below) to keep people up to date. However, such source stopped at basic information only and lack informative analysis (Not to mention the apprearance was not exactly catchy).
                        Data was outdated, inconsistent across sources and was not available to download for any purposes, which posed as the biggest challenge when i created this app.
                        I hoped you who are reading this section will find the app helpful to get to know my country better and how we are courageously fighting the pandemic to leave no one behind.
                        "),
         tags$p("Please find the link to the Vietnamese Covis-19 information center here: "),
         tags$a("https://ncov.moh.gov.vn/web/guest/trang-chu"),
         tags$br(),
         tags$p("This app was created by Hanh Ngo, student id -31196101, student of the Master of Business Analytics program in Monash University.
                 This app was greatly inspired by the Covid - 19 bulletin board developed by Su Wei for the Japan Government (link provided in the References).
                 Data was not updated automatically so some information might be outdated by the time you are reading this report. Please read with caution."),
         tags$h4("References"),
         tags$hr(),
         tags$div(tags$ul(tags$strong("R packages used"),
           unique(output_ref("packages"))
         )),
         tags$div(tags$ul(tags$strong("External sources"),
                          unique(output_ref("link"))
         ))
  )
}
