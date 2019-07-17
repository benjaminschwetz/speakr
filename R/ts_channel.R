create_channel <- function(
  id,
  key = NA_character_,
  private = FALSE
){
  x <- structure(
    list(
      id = id,
      key = key,
      private = private
    ),
    class = "ts channel")
  x
}

.make_ts_path <- function(channel,
                         type,
                         last = FALSE,
                         field = NA_integer_,
                         form = "xml"){
  type_string <- dplyr::case_when(
    type == "fields" ~ paste(type, field, sep="/"),
    TRUE ~ type
  )
  return_str <-
    paste("channels",
         channel$id,
         type_string,
         sep = "/")
  if(last) return_str <- paste(return_str, "last", sep = "/")
  return_str <- paste0(return_str, ".", form)
  return_str
}

.make_ts_query <- function(channel,
                          ...){
  if(channel$private){
    l<- list(
      api_key = channel$key
    )} else l <- list()
  l <-  append(l, ...)
  l
}

.compose_ts_url <- function(
  channel,
  request_type,
  last = FALSE,
  field = NA_integer_,
  parameters = list()
){
  x <- list(
    scheme = "https",
    hostname= "api.thingspeak.com",
    port = NULL,
    path = .make_ts_path(channel, request_type, last, field),
    params = NULL,
    fragment = NULL,
    query = .make_ts_query(channel, parameters),
    username = NULL,
    password = NULL
  )
  attr(x, "class") <- c("ts url", "url")
  x
}

