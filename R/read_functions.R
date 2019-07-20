.build_n_get <- purrr::as_mapper(~ .x %>%
                                httr::build_url() %>%
                                httr::GET()
                              )

#' Title
#'
#' @param channel
#' @param parameters
#'
#' @return
#' @export
#'
#' @examples
read_data <- function(channel, parameters = list()) {
  .compose_ts_url(
    channel = channel,
    request_type = "feeds",
    parameters = parameters
  ) %>%
    .build_n_get() %>%
    .parse_response()
}

#' Title
#'
#' @param channel
#' @param parameters
#'
#' @return
#' @export
#'
#' @examples
read_status <- function(channel, parameters = list()) {
  .compose_ts_url(
    channel = channel,
    request_type = "status",
    parameters = parameters
  ) %>%
    .build_n_get %>%
    .parse_response()
}


#' Title
#'
#' @param channel
#' @param field
#' @param parameters
#'
#' @return
#' @export
#'
#' @examples
read_field <- function(channel, field, parameters = list()) {
  .compose_ts_url(
    channel = channel,
    request_type = "fields",
    field = field,
    parameters = parameters
  ) %>%
    .build_n_get() %>%
    .parse_response()
}
