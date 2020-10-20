library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(plotly)
library(lubridate)
library(leaflet)
library(rgdal)
library(tigris)
library(geojsonio)
library(DT)
library(echarts4r)

#------- DATA PREPARATION ------------------------------------------------------------------------------------

# Read data and clean up for number of cases, deaths by days
data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")


# First tab - prepare data
## get json file
vietnam_map <- geojson_read("https://data.opendevelopmentmekong.net/dataset/999c96d8-fae0-4b82-9a2b-e481f6f50e12/resource/2818c2c5-e9c3-440b-a9b8-3029d7298065/download/diaphantinhenglish.geojson", what = "sp")

## merge json with data
vn_map_merged <- geo_join(vietnam_map, vietnam_daily, "Name", "province")

bins_vn <- c(0, 1, 10, 50, 100, 500, Inf)
pal_vn <- colorBin("YlOrRd", domain = vn_map_merged$cummulative_case, bins = bins_vn)
labels_vn <- sprintf(
  "<strong>%s</strong><br/>Total cases: %g<br/>Total Active: %g<br/>Death:%g",
  vn_map_merged$province, vn_map_merged$cummulative_case, vn_map_merged$Active,vn_map_merged$Death) %>% 
  lapply(htmltools::HTML)

# Third tab - prepare world map
## Prepare data for world map
world_data <- data %>%
    group_by(countriesAndTerritories) %>%
    arrange(year, month, day, .by_group = TRUE) %>%
    mutate(cumulative_case = cumsum(cases),
           cumulative_death = cumsum(deaths)) %>%
    mutate(dateRep = dmy(dateRep),
           "cases_per_100k" = round(cumulative_case/popData2019*100000,0),
           "deaths_per_100k" = round(cumulative_death/popData2019*100000,0),
           countriesAndTerritories = gsub("_", " ", countriesAndTerritories))

## Select only needed vars
map_dat2 <- world_data %>%
    filter(dateRep == max(dateRep)) %>%
    select(countryterritoryCode, cumulative_case, cumulative_death, cases_per_100k, deaths_per_100k)

## get json file
global_map <- geojson_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json", what = "sp")

## merge json with data
global_map_merged <- geo_join(global_map, map_dat2, "id", "countryterritoryCode")

## prepare bins, palette and labels for plot
bins <- c(0, 1, 10, 50, 100, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = global_map_merged$cases_per_100k, bins = bins)
labels <- sprintf(
    "<strong>%s</strong><br/>%g cases / 100k population<br/>total cases: %g",
    global_map_merged$countriesAndTerritories, global_map_merged$cases_per_100k, global_map_merged$cumulative_case) %>% lapply(htmltools::HTML)

## Prepare data for bar charts
### data for particular country
world_data_2 <- data %>%
    select(- continentExp, - geoId) %>%
    mutate(dateRep = dmy(dateRep)) %>%
    # group_by(dateRep) %>%
    arrange(dateRep, .by_group = TRUE) %>%
    mutate(cumulative_case = cumsum(cases),
           cumulative_death = cumsum(deaths)) %>%
    mutate(countriesAndTerritories = gsub("_", " ", countriesAndTerritories))

### data for all countries  
all_countries <- world_data_2 %>%
    group_by(dateRep) %>%
    summarise(cases = sum(cases),
              deaths = sum(deaths),
              population = sum(popData2019, na.rm = TRUE)) %>%
    mutate(cumulative_case = cumsum(cases),
           cumulative_death = cumsum(deaths))

### dt table
dt_table <- map_dat2 %>%
    arrange(desc(cumulative_case)) %>%
    rename("Country" = countriesAndTerritories,
           "Country code" = countryterritoryCode,
           "Cummulative cases" = cumulative_case,
           "Cumulative deaths" = cumulative_death,
           "Cases per 100k population" = cases_per_100k,
           "Deaths per 100k population" = deaths_per_100k)

#------- DEFINE UI ------------------------------------------------------------------------------------
ui <- navbarPage(inverse = TRUE, "The Vietnam COVID-19",

    # First tab - Bulletin Board
    tabPanel("Bulletin Board",
        fluidPage(
          fluidRow(
          infoBoxOutput("totalcases"),
          infoBoxOutput("totalrecovered"),
          infoBoxOutput("totaldeath")
        ),
                  fluidRow(
                    bulletinNote(),
                    column(7, 
                           tags$h3("Daily new cases in Vietnam"),
                           tags$hr(),
                    wellPanel(
                      tags$em("Explore the patients detail for each day by clicking in the date of your choice in the below graphs. A table will unfold underneath to tell you the details of confirmed cases that were announced on such day."),
                      tags$hr(),
                      plotlyOutput("Dailybarchart", height = 200),
                      tags$hr(),
                      tags$h3("Patient Information"),
                      dataTableOutput("detailtable")
                      )
                    )
                  )
        )
    ),
    
    # Second tab - Infection Route
    tabPanel("Infection Route",
        fluidPage(fluidRow(
            column(6,
                   tags$h3("Patient Infected Route Map"),
                   tags$hr(),
                   selectInput("month",
                               "Select month from the dropdown list",
                               choices = unique(patient_node$month_announced),
                               selected = "September"),
                   echarts4rOutput("network", height = "800px")
                   ),
            column(width = 6,
                   uiOutput("ProfileSearchBox"),
                   uiOutput("profile"),
              tags$h3("Note and user guidance"),
             wellPanel(
                      tags$h5(strong("Note")),
                      tags$hr(),
                      tags$div(tags$ul(
                       tags$li(tags$span("This chart showed the interaction between confirmed cases. Information might not be sufficient as patient's travelling history may not be recalled and recorded properly.")),
                       tags$li(tags$span("The chart's legend did not show consistent colour for each status by months. This was defined as a bug of the function provided and cannot be fixed temporarily.")),
                       tags$li(tags$span("Relation implies the interaction between patients, but not necessarily mean the source of infection.")),
                      )),
                      tags$h5(strong("User guidance")),
                      tags$hr(),
                      tags$p("Choose the month you want to view from the dropdown list. You can check the patients profile for this month by clicking directly on the nodes or navigate in the Patient profile search box."))
        ))
        )
    ),
    
    # Third tab - World Situation
    tabPanel("World Situation",
        fluidPage(sidebarLayout(position = "right",
                  sidebarPanel(
                      wellPanel(selectInput("country",
                                      "Select country from the dropdown list",
                                      choices = c("Global", sort(unique(world_data_2$countriesAndTerritories))),
                                      selected = "Global"),
                                selectInput("cases",
                                            "Select variable output",
                                            choices = c("Daily cases and deaths", "Cummulative cases and deaths"),
                                            selected = "Daily cases and deaths"),
                                plotlyOutput("global")),
                      wellPanel(tags$h4(strong("How to use")),
                                br(),
                                tags$p("The map coloured countries by the number of cases per 100,000 population. Hover your mouse over each country for additional info of the total cases.
                                       You can choose to see the chart for either daily counts or accumulated counts for your chosen country. Pick your choice from the two dropdown lists above the bar chart. You can also manouver your way over the table beneath the map to search from a particular country of interest and its detailed numbers by making use of the Search box."))
                  ),
                  mainPanel(
                      wellPanel(
                          tags$h4(strong("The world situation")),
                          tags$hr(),
                          tags$em("Hover over the map to explore the most updated cases for each country"),
                          br(),
                          leafletOutput("worldmap"), height = 400),
                      wellPanel(
                          tags$h4(strong("Details of each country")),
                          dataTableOutput("ranktable"))
                  )
                                )
        )
    ),
    
    # Fourth tab - About
    tabPanel("About",
        fluidPage(fluidRow(
          column(5, tags$img(src = "vietnam-stamp.jpg")), aboutNote())
)))

#------- SERVER FROM HERE ---------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # BULLENTIN BOARD - Page 1
  ## Value box
   output$totalcases <- renderInfoBox({
     total_case <- patient_node %>% nrow()
     
     infoBox("Total ", paste0(total_case ," confirmed cases"),
              subtitle = ("691 cases in social setting"),
              icon = icon("procedures"), color = "red", fill = TRUE)
   })
   
   count <- patient_node %>%
     group_by(status) %>%
     count()
  
   output$totalrecovered <- renderInfoBox({
     infoBox("Recovered ", paste0(count[4,2]," cases"),
              subtitle = paste0(count[1,2], " cases stil active"),
              icon = icon("user-shield"), color = "green", fill = TRUE)
     
   })
  
   output$totaldeath <- renderInfoBox({
     infoBox("Total ", paste0(count[2,2]," deaths"),
              subtitle = ("3 cases died of other causes"),
              icon = icon("bible"), color = "navy", fill = TRUE)
   })
   
   ## Vietnam map
   output$vietnammap <- renderLeaflet({
     leaflet(vn_map_merged) %>%
       #setView(lng = 10, lat = 50, zoom = 2) %>%
       addProviderTiles("MapBox", options = providerTileOptions(
         id = "mapbox.light", accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
       # addPolygons()
       addPolygons(fillColor = ~pal(cummulative_case),
                   weight = 1,
                   opacity = 1,
                   color = "white",
                   dashArray = "3",
                   fillOpacity = 0.7,
                   highlight = highlightOptions(
                     weight = 5,
                     color = "#666",
                     dashArray = "",
                     fillOpacity = 0.7,
                     bringToFront = TRUE),
                   label = labels_vn,
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px",
                     direction = "auto")) %>%
       addLegend(pal = pal_vn, values = ~cummulative_case, opacity = 0.7, title = NULL, position = "bottomright")
   })
   
   ## Daily bar chart
   output$Dailybarchart <- renderPlotly({
     p4 <- patient_node %>%
       count(date_announced) %>%
       rename("cases" = n) %>%
       arrange(date_announced) %>%
       ggplot(aes(x = date_announced, y = cases)) +
       geom_histogram(stat = "identity",fill = "#f03b20") +
       theme_minimal() +
       theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank())
     
     ggplotly(p4, source = "dateclick")

   })
   
   SelectedBar <- reactiveVal()
   
   observeEvent(event_data("plotly_click", source = "dateclick"), {
     SelectedBar(as.Date(event_data("plotly_click", source = "dateclick")$x,
                         origin="1970-01-01"))
   }
   )

   output$detailtable <- renderDataTable({
     if (is.null(SelectedBar())) return(NULL)
     
     patient_node %>%
       filter(date_announced %in% SelectedBar()) %>%
       arrange(desc(id_no)) %>%
       select(to, gender, treatment_location, status, nationality, type, date_announced) %>%
       rename("patientid" = to) %>%
       datatable(filter = "top",
                 options = list(pageLength = 7))
   })
   
   
    # NETWORK - Page 2
    cluster <- reactive({
        patientLink <- patient_link %>%
            filter(month_announced %in% input$month)
        patientDetail <- patient_node %>%
            filter(month_announced %in% input$month) %>%
            group_by(status)
        return(list(node = patientDetail, edge = patientLink))
    })
    
    output$network <- renderEcharts4r({
    ## Define nodes and edges
        node <- cluster()$node
        # status is currently factor, need to convert to character to show legend
        node$status <- as.character(node$status)
        edge <- cluster()$edge
        
    ## Draw chart
        e_charts() %>%
          e_graph(#layout = "force",
                  roam = T,
                    draggable = T,
                    symbolKeepAspect = T,
                    focusNodeAdjacency = T
                ) %>%
          # nodes randomly assigned colour for category when change input, cannnot fix this yet
          e_graph_nodes(node, names = to, value = value, size = size, category = status,
                    symbol = NULL, legend = TRUE) %>%
          e_graph_edges(edge, target = to, source = from) %>%
          e_labels(
            formatter = htmlwidgets::JS(
              paste0(
                'function(params) {
                if (params.value) {
                const text = params.value.split("|")
                const id = text[0]
                const status = text[5] == "Deceased" ? "{death|†}" : ""
                return(`${id}`)
                }
              }
                '
              )
            ),
            rich = list(
                borderColor = "auto",
                borderWidth = 2,
                borderRadius = 2,
                padding = 3,
                fontSize = 6
              ),
          ) %>%
          e_tooltip(formatter = htmlwidgets::JS(
                    paste0(
                        'function(params) {
                        if (params.value) {
                        const text = params.value.split("|")
                        return(`
                        ',
                        "Patient ID: ", "${text[0]}<br>",
                        "Type: ","${text[6]}<br>",
                        "Age: ", "${text[1]}<br>",
                        "Gender: ", "${text[2]}<br>",
                        "Nationality: ", "${text[7]}<br>",
                        "Status: ", "${text[5]}`)
                        }
                      }
  "
                    )
                )) %>%
          # e_modularity(modularity = TRUE) %>%
          e_legend_select(name = "Active",  btn = NULL) %>% 
          ## e_add("itemStyle", color) %>%
          e_title(
                    text = paste0("Total cases: ", nrow(node)),
                    subtext = paste0(
                        "Month: ", input$month
                    )
                ) 
    })
    
    ## Create search box
    
    output$ProfileSearchBox <- renderUI({
      node <- cluster()$node
      if (!is.null(node) && nrow(cluster()$node) > 0) {
        choicesLabel <-
          paste0(node$to, "（age ", node$age," - ",node$gender, "）")
        choices <- node$to
        names(choices) <- choicesLabel
        selectizeInput(
          label = tagList(icon("search"), "Patient profile search"),
          choices = choices,
          inputId = "searchProfile"
        )
      }
    })
    
    ## When click on the nodes, subsequent patient infos will be shown
    observeEvent(input$network_clicked_data, {
      updateSelectizeInput(
        session = session,
        inputId = "searchProfile",
        selected = input$network_clicked_data$name
      )
    })
    
    output$profile <- renderUI({
      if (!is.null(input$searchProfile) &&
          !is.null(cluster()$node) && nrow(cluster()$node) > 0) {
        # When search in search box
        patientInfo <-
          cluster()$node %>% filter(to == input$searchProfile)
        
        if (length(patientInfo$value) > 0) {
          patientInfo$value <- as.character(patientInfo$value)
          profile <- unlist(strsplit(patientInfo$value, "\\|")[[1]])
          
          age <- ifelse(profile[2] != "", profile[2], "unknown")
          type <-
            ifelse(profile[7] != "", profile[7], "unknown")
          treatment_loc <-
            ifelse(profile[5] != "", profile[5], "unknown")
          nationality <- ifelse(profile[8] != "", profile[8], "unknown")
          gender <- tagList(icon("venus-mars"), profile[4])
          if (profile[3] == "male") {
            gender <- tagList(icon("mars"), "Male")
          } else if (profile[3] == "female") {
            gender <- tagList(icon("venus"), "Female")
          }
        }
        # Create badge next to gender
        statusBadge <- ""
        if (profile[6] == "Active") {
          statusBadge <- dashboardLabel("Active", status = "warning")
        } else if (profile[6] == "Recovered") {
          statusBadge <- dashboardLabel("Recovered", status = "success")
        } else if (profile[6] == "Deceased") {
          statusBadge <- dashboardLabel("Deceased", status = "primary")
        } else {
          statusBadge <- dashboardLabel(profile[6], status = "info")
        }
        
        boxPlus(
          title = tagList(icon("id-card"), "Patient Information"),
          width = 12,
          closable = F,
          boxProfile(
            title = profile[1], 
            src = ifelse(profile[3] == 'male', 'man.png', 'woman.png'),
            subtitle = tagList(gender, statusBadge),
            boxProfileItemList(
              bordered = TRUE,
              boxProfileItem(
                title = tagList(icon("user-clock"), "Age"),
                description = age
              ),
              boxProfileItem(
                title = tagList(icon("bullhorn"), "Type"),
                description = type
              ),
              boxProfileItem(
                title = tagList(icon("user-tie"), "Nationality"),
                description = nationality
              ),
              boxProfileItem(
                title = tagList(icon("home"), "Treatment location"),
                description = treatment_loc
              ),
            )
          ),
          footer = tagList(
            tags$b(icon("procedures"), "Symtomps"),
            tags$p(tags$small(HTML(profile[14]))),
            tags$hr(),
            tags$b(icon("walking"), "Infected category"),
            tags$div(tags$ul(
              tags$li(tags$span("Quarantine upon entry - confirmed cases from patients who are quarantined in Government appointed location right upon entry into the country.")),
              tags$li(tags$span("Social settings - confirmed cases in the society. Sources may not be tracked down")),
              tags$li(tags$span("Foreign expert - foreign exports who were approved to fly into the country.")),
              tags$li(tags$span("Health-care worker - confirmed cases of health-care workers")),
              tags$li(tags$span("Illegal immigrated - Illegal immigration into the country")),
              tags$li(tags$span("From oversea - cases coming from people who entered Vietnam from overseas. This was when the Government did not enact the decision to impose compulsory monitored quarantine onto foreign entrance."))
            )),
            tags$hr()
          )
        )
      }
    })
    
    
    # WORLD MAP STATS- Page 3
    output$worldmap <- renderLeaflet({
        leaflet(global_map_merged) %>%
            setView(lng = 10, lat = 50, zoom = 2) %>%
            addProviderTiles("MapBox", options = providerTileOptions(
                id = "mapbox.light", accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
            # addPolygons()
            addPolygons(fillColor = ~pal(cases_per_100k),
                        weight = 1,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.7,
                        highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE),
                        label = labels,
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto")) %>%
            addLegend(pal = pal, values = ~cases_per_100k, opacity = 0.7, title = NULL, position = "bottomright")
    })
    
    # ggplot in side panels - Page 3
    country <- reactive({
        if(input$country == "Global")
            all_countries
        else
            world_data_2 %>%
            filter(countriesAndTerritories %in% input$country)
    })
    
    text<- reactive({
        if(input$country == "Global")
            paste("Global update")
        else
            paste0(input$country," update")
    })
    case <- reactive({
        if(input$cases == "Daily cases and deaths")
            country()$cases
        else
            country()$cumulative_case
    })
    death <- reactive({
        if(input$cases == "Daily cases and deaths")
            country()$deaths
        else
            country()$cumulative_death
    })

    output$global <- renderPlotly({
        country <- country()
        global_bar <- country %>%
            ggplot(aes(x = dateRep)) +
            geom_bar(aes(y = case()/1000), stat = "identity", fill = "#f03b20") +
            geom_line(aes(y = death()/1000)) +
            theme_minimal() +
            xlab("") +
            ylab("Count (in thousans)") +
            scale_y_continuous(labels = scales::comma) +
            ggtitle(paste(input$country, "-", input$cases))
        ggplotly(global_bar)
    })
    
    ## datatable underneath map - Page 3   
    output$ranktable <- renderDataTable({
        datatable(dt_table, filter = "top", options = list(pageLength = 10)) %>%
            formatCurrency(3:5,currency = "", interval = 3, mark = ",")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
