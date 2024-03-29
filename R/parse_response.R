.parse_response <- function(response) {
  if(response$status_code != 200){
    stop(
      sprintf("Response code is %d", response$status_code)
    )
  }
  l <- response %>%
    httr::content() %>%
    xml2::as_list() %>%
    purrr::flatten()
  df_element <- l %>%
    names() %>%
    stringr::str_extract("feed(s)*|tags") %>%
    na.omit()
  l <- l %>%
    purrr::modify_at(dplyr::vars(-df_element),
                     ~ .x %>%
                       purrr::flatten_chr() %>%
                       readr::parse_guess()
    )
  if(!is.null(l[[df_element]]) &
     length(l[[df_element]]) > 0){
    l <- l %>%
      purrr::modify_in(df_element,
                       ~purrr::discard(
                         .x,
                         ~ .x %>%
                           purrr::map_lgl(~ length(.x) == 0) %>%
                           any()
                       )
      ) %>%
      purrr::modify_in(df_element,
                       ~ purrr::modify_depth(.x, 2, purrr::flatten_chr)) %>%
      purrr::modify_in(df_element,
                       ~ purrr::map_df(.x, ~ dplyr::bind_rows(.x)) %>%
                         dplyr::mutate_all(readr::parse_guess))
    #renaming the fields
    if(stringr::str_detect(df_element,"feed(s)*")){
      fields <- l[stringr::str_detect(names(l), "field")] %>%
        unlist
      columns <- names(fields) %>%
        magrittr::set_names(fields) %>%
        magrittr::extract(. %in% colnames(l[[df_element]]))
      l <- l %>% purrr::modify_in( df_element,
                                   ~.x %>% dplyr::rename(!!!columns)
      )
    }
  }
  l
}
#' Title
#'
#' @param data_response
#'
#' @return
#' @export
#'
#' @examples
extract_feeds_df <- function(data_response) {
  n <- data_response %>%
    names() %>%
    stringr::str_detect("field\\d+") %>%
    which %>%
    length()
  df <- data_response %>%
    magrittr::extract2("feeds")
  attr(df, "n_fields") <- n
  df
}
