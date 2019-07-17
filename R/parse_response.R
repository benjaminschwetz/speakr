.parse_response <- function(response) {
  l <- response %>%
    httr::content() %>%
    xml2::as_list() %>%
    purrr::flatten() %>%
    purrr::modify_if(~length(.x) == 1,
                     ~ .x %>%
                       purrr::flatten_chr() %>%
                       readr::parse_guess()
                       )  %>%
    purrr::modify_in("feeds",
                     ~purrr::discard(
                       .x,
                       ~ .x %>%
                         purrr::map_lgl(~ length(.x) == 0) %>%
                         any()
                     )
    ) %>%
    purrr::modify_in("feeds",
                     ~ purrr::modify_depth(.x, 2, purrr::flatten_chr)) %>%
    purrr::modify_in("feeds",
                     ~ purrr::map_df(.x, ~ dplyr::bind_rows(.x)) %>%
                       dplyr::mutate_all(readr::parse_guess))
  #renaming the fields
  fields <- l[stringr::str_detect(names(l), "field")] %>%
    unlist
  columns <- names(fields) %>%
    magrittr::set_names(fields) %>%
    magrittr::extract(. %in% colnames(l$feeds))
  l <- l %>% purrr::modify_in( "feeds",
                               ~.x %>% dplyr::rename(!!!columns)
  )
  l
}
