#' Title
#'
#' @return
#' @export
#'
#' @examples
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
      ),
      menuItem("Channels", tabName = "channels", icon = icon("database"))
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

      ),
      tabItem("channels",
              fluidRow(
                box(
                  textInput("chan_id", "Channel ID"),
                  textInput("chan_key", "Channel Key"),
                  actionButton("chan_submit", "Submit"),
                  title = "Add a new channel"
                ),
                box(
                  uiOutput("select_active_chan"),
                  title = "Select active Channel"
                )
              ))
    )
  )

  ui <- dashboardPage(db_header, db_sidebar, db_body)
  # ---------
  server <- function(input, output) {
    values <- reactiveValues(
      channel_list = list() %>%
        append(list(create_channel(9))) %>%
        purrr::map(
          ~ .x %>%
            purrr::list_modify(
              read_settings(.x)
            ) %>%
            flatten()
        ),
      active_channel = 1
    )
    observeEvent(input$chan_submit, {
      if(input$chan_key != "") {
        channel <- create_channel(input$chan_id, input$chan_key)
      } else channel <- create_channel(input$chan_id)
      values$channel_list <-
        values$channel_list %>%
        append(list(channel)) %>%
        purrr::map(
          ~ .x %>%
            purrr::list_modify(
              read_data(.x, list(results = 0))
            ) %>%
            flatten()
        )
    })
    output$select_active_chan <- renderUI({
      radioButtons("active_channel", "",
                   choices = map(values$channel_list, "name"),
                   selected = values$channel_list[[1]]$name
                   )
    })
    active_channel <- reactive({
      values$channel_list[[values$active_channel]]
    })
    observeEvent(input$active_channel, {
      values$active_channel <-
        match(input$active_channel,
            map_chr(values$channel_list, "name"))
    })
    # latest boxes
    boxes <- reactive({
      invalidateLater(2000)
      renderUI({
        latest_boxes(active_channel(),
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
        feeds <- read_data(active_channel(), list( days = input$days, timescale = input$timescale)) %>%
          extract_feeds_df()
      }
    )
    output$graph <- renderPlot(demo_plot(hist_data()))
  }

  # Run the application
  shinyApp(ui = ui, server = server)
}
