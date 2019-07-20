.parse_response <- function(response) {
  l <- response %>%
    httr::content() %>%
    xml2::as_list() %>%
    purrr::flatten() %>%
    purrr::modify_if(~length(.x) == 1,
                     ~ .x %>%
                       purrr::flatten_chr() %>%
                       readr::parse_guess()
                       )
  df_element <- l %>%
    names() %>%
    stringr::str_extract("feed(s)*") %>%
    na.omit()
  if(!is.null(l[[df_element]])){
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
    fields <- l[stringr::str_detect(names(l), "field")] %>%
      unlist
    columns <- names(fields) %>%
      magrittr::set_names(fields) %>%
      magrittr::extract(. %in% colnames(l[[df_element]]))
    l <- l %>% purrr::modify_in( df_element,
                                 ~.x %>% dplyr::rename(!!!columns)
    )
  }
  l
}
