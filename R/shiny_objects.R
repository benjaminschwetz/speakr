# input: channel information
# output: tag list with value boxes for all the latest infos
#' Title
#'
#' @param channel A thingspeak channel
#' @param icons
#' @param colors
#' @param widths
#' @param hrefs
#' @param formatting
#'
#' @return
#' @export
#'
#' @examples
latest_boxes <- function(channel, formatting = NULL, icons = NULL, colors = "aqua", widths = 4, hrefs = NULL) {
  nulls <- function(n) map(1:n, function(...) {NULL})
  data <- read_data(channel, list( results = 1)) %>%
    purrr::pluck("feeds")
  n <- length(data)
  stopifnot(is.null(formatting) | length(formatting) == n)
  l <- list(data,
            names(data),
            if(is.null(icons)) nulls(n) else map(icons, shiny::icon),
            colors,
            widths,
            if(is.null(hrefs)) nulls(n) else hrefs
  )
  if(length(formatting) == n) {
    l <- l %>% purrr::modify_in(1,
                     ~ map2(.x, formatting,
                            ~ .y(.x)
                            )
                     )
  }
  shiny::tagList(
    purrr::pmap(
      l,
      ~ shinydashboard::valueBox(
        value = ..1,
        subtitle = ..2,
        icon = ..3,
        color = ..4,
        width = ..3
      )
    )
  )
}

#' Generate a battery icon depending on input
#'
#' @param x
#' @param max
#'
#' @return
#' @export
#'
#' @examples
battery_icon <- function(x, max = 1000){
  load <- c("empty", "quarter", "half", "three-quarters", "full")
  index <- 1 + (x*4)/max
  icon(paste0("battery-", load[index]))
}
