app_demo <- function() {
  library(shiny)
  library(tidyverse)
  library(lubridate)
  library(shinydashboard)
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
    title = "Shiny App Demo")
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
                         plotOutput("graph"),
                         width = NULL
                       )
                ),
                column(width = 3,
                       box(width = NULL, status = "warning",
                           sliderInput("days", "Days", 1, 14,3),
                           selectInput("timescale", "Resolution",
                                       choices = list(
                                         "hourly" = 60,
                                         "daily" = "daily",
                                         "full" = 0
                                       )
                           ),
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
    # latest boxes
    boxes <- reactive({
      invalidateLater(2000)
      renderUI({
        latest_boxes(channel_list()[[1]],
                     formatting = c(as.character,
                                    identity,
                                    round,
                                    round)
        )
      })
    })
    observeEvent(boxes, output$status_boxes <- boxes())

    # make plot with slider ----
    hist_data <- eventReactive(
      input$refresh,
      {
        feeds <- read_data(channel_list()[[1]], list( days = input$days, timescale = input$timescale)) %>%
          extract_feeds_df()
      }
    )
    output$graph <- renderPlot(demo_plot(hist_data()))
  }

  # Run the application
  shinyApp(ui = ui, server = server)
}
