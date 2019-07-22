library(shiny)
library(tidyverse)
library(lubridate)
library(speakr)
library(shinydashboard)
soil_sensor <- create_channel(823293,"UDZ0R0ZMYFN7ETCB",TRUE)

db_sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      "Status", tabName = "status", icon = icon("bell")
    ),
    menuItem(
      "Time series", tabName = "timeseries", icon = icon("chart-line")
    )
  )
)
db_header <-  dashboardHeader(
  title = "Soil Moisture")
db_body <- dashboardBody(
  tabItems(
    tabItem("status",
            fluidRow(
              uiOutput("status_boxes")
            )),
    tabItem("timeseries",
            fluidRow(
              column(width = 9,
                     box(
                       plotOutput("graph")
                     )
              ),
              column(width = 3,
                     box(width = NULL, status = "warning",
                         sliderInput("days", "Days", 1, 14,3),
                         actionButton("refresh", "Refresh now")
                     )
              )
            )

    )
  )
)

ui <- dashboardPage(db_header, db_sidebar, db_body)
# ---------
server <- function(input, output) {
  # demo channels
  channel_list <- reactive({
    l <- list() %>%
      append(list(create_channel("9"))) %>%
      append(list(create_channel("9", "E52AWRAV1RSXQQJW", TRUE)))
    l %>% purrr::map(
      ~ .x %>%
        purrr::list_modify(
          read_settings(.x)
        ) %>%
        flatten()
    )
  })
  # fetching data ----
  get_latest <- function () {
    read_data(channel_list()[[1]], list( results = 1))
  }
  latest <- reactive({
    invalidateLater(1800)
    get_latest()
  })
  observeEvent(latest,{
    # status tab ----
    battery <- function(x, max = 1000){
      load <- c("empty", "quarter", "half", "three-quarters", "full")
      index <- 1 + (x*4)/max
      icon(paste0("battery-", load[index]))
    }
    time_print <- function(x){
      sprintf(
        "%02d/%02d %02d:%02d",
        lubridate::day(x),
        lubridate::month(x),
        lubridate::hour(x),
        lubridate::minute(x)
      )
    }
    output$status_boxes <- renderUI({
      tagList(
        valueBox(time_print(latest()$feeds$`created-at`), "Last Update", icon = icon("clock")),
        valueBox(latest()$feeds$`Basil moisture`, "Basil", icon = battery(latest()$feeds$`Basil moisture`)),
        valueBox(latest()$feeds$Temperature, "Temperature", icon = icon("thermometer-quarter"))

      )
    })
  })
  # fetching data ----
  hist_data <- reactive({
    input$refresh
    invalidateLater(1800000)
    feeds <- list()
    plus <- 0
    while(length(feeds) == 0 ){
      feeds <- read_data(soil_sensor, list( days = input$days + plus)) %>%
        magrittr::extract2("feeds")
      plus <- plus + 1
    }
    feeds
  })
  observeEvent(hist_data,{
    # timeseries tab ----
    output$graph <- renderPlot(
      hist_data() %>%
        gather("Sensor", "Value", -(1:2)) %>%
        rename("Timestamp" = "created-at") %>%
        ggplot() +
        aes(x = Timestamp,
            y = Value) +
        geom_line()+
        facet_wrap(~Sensor,
                   ncol = 1,
                   scales = "free_y")
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
