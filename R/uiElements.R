#' UI Elements
#'
#'
#' @export

## Bulletin Board##

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