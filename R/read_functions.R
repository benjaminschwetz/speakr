read_data <- function(channel, parameters = list()) {
  .compose_ts_url(
    channel = channel,
    request_type = "feeds",
    parameters = parameters
  ) %>%
    httr::build_url() %>%
    httr::GET() %>%
    .parse_response()
}

read_last_data <- function(channel, parameters = list()) {
  .compose_ts_url(
    channel = channel,
    request_type = "feeds",
    last = TRUE,
    parameters = parameters
  ) %>%
    httr::build_url() %>%
    httr::GET() %>%
    .parse_response()
}

read_status <- function(channel, parameters = list()) {
  .compose_ts_url(
    channel = channel,
    request_type = "status",
    parameters = parameters
  ) %>%
    httr::build_url() %>%
    httr::GET() %>%
    .parse_response()
}

read_last_status <- function(channel, parameters = list()) {
  .compose_ts_url(
    channel = channel,
    request_type = "status",
    last = TRUE,
    parameters = parameters
  ) %>%
    httr::build_url() %>%
    httr::GET() %>%
    .parse_response()
}

read_field <- function(channel, field, parameters = list()) {
  .compose_ts_url(
    channel = channel,
    request_type = "fields",
    field = field,
    parameters = parameters
  ) %>%
    httr::build_url() %>%
    httr::GET() %>%
    .parse_response()
}

read_last_field <- function(channel, field, parameters = list()) {
  .compose_ts_url(
    channel = channel,
    request_type = "fields",
    last = TRUE,
    field = field,
    parameters = parameters
  ) %>%
    httr::build_url() %>%
    httr::GET() %>%
    .parse_response()
}
