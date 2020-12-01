# TODO: thank https://github.com/libscie/retractcheck/blob/master/R/utils.R
doi_regex <- "\\b10\\.\\d{4,9}/[-._;()/:a-z0-9]+\\b"

resolve_shortdoi <- function(short_doi, polite_bow, csv_path) {
  on_publisher <- get_page(short_doi, polite_bow)

  # bypass Elsevier's JS-redirection..
  meta_nodes <- on_publisher %>%
    rvest::html_nodes('meta')
  meta_http <- meta_nodes %>%
    rvest::html_attr('http-equiv')
  redirect_idx <- meta_http %>%
    stringr::str_which(stringr::regex("refresh",
                                      ignore_case = TRUE)) %>%
    head(1)

  if (length(redirect_idx) > 0) {
    redir <- meta_nodes %>%
      purrr::pluck(redirect_idx) %>%
      rvest::html_attr('content') %>%
      url_decode() %>%
      stringr::str_extract("http(.*)(?=[&?])")

    # TODO: might fail on exotic Elsevier's redirection url
    # TODO: recover from possible xml2::read_html() failure
    on_publisher <- httr::GET(redir) %>%
      xml2::read_html()

    meta_nodes <- on_publisher %>%
      rvest::html_nodes('meta')
  }

  meta_names <- meta_nodes %>%
    rvest::html_attr('name')
  doi_idx <- meta_names %>%
    stringr::str_which(stringr::regex("doi|dc.identifier",
                                      ignore_case = TRUE)) %>%
    head(1)
  doi <- meta_nodes %>%
    rvest::html_attr('content') %>%
    purrr::pluck(doi_idx, .default = NA_character_)
  res <- tibble::tibble(short_doi, doi)
  return_df(res, csv_path)
}


get_cr_data <- function(doi, csv_path, verbose = TRUE) {
  url <-
    sprintf("https://api.crossref.org/works/%s", doi)
  resp <-
    httr::GET(url, httr::add_headers("user-agent" = settings$agent))
  res <- tibble::tibble(
    doi,
    cr_title = NA_character_,
    cr_container_title = NA_character_,
    cr_short_container_title = NA_character_,
    cr_volume = NA_character_,
    cr_issue = NA_character_,
    cr_page = NA_character_,
    cr_year = NA_character_
  )
  if (verbose)
    roger_that(url, "Trying")
  resp_is_success <- !httr::http_error(resp)
  resp_is_json <- httr::http_type(resp) == "application/json"
  if (resp_is_success & resp_is_json) {
    json <- jsonlite::fromJSON(httr::content(resp, "text"), flatten = TRUE)
    res <- res %>%
      dplyr::mutate(
        cr_title = purrr::pluck(json,
                                "message",
                                "title",
                                1,
                                .default = NA_character_),
        cr_container_title = purrr::pluck(json,
                                          "message",
                                          "container-title",
                                          1,
                                          .default = NA_character_),
        cr_short_container_title = purrr::pluck(json,
                                                "message",
                                                "short-container-title",
                                                1,
                                                .default = NA_character_),
        cr_volume = purrr::pluck(json,
                                 "message",
                                 "volume",
                                 1,
                                 .default = NA_character_),
        cr_issue = purrr::pluck(json,
                                "message",
                                "issue",
                                1,
                                .default = NA_character_),
        cr_page = purrr::pluck(json,
                               "message",
                               "page",
                               1,
                               .default = NA_character_),
        cr_year = purrr::pluck(json,
                               "message",
                               "created",
                               "date-time",
                               1,
                               .default = NA_character_)
      )
  }
  return_df(res, csv_path)
}

get_cr_batch <- function(dois) {
  cr_list <- rcrossref::cr_cn(dois,
                              "citeproc-json",
                              .progress = "text")
  return(cr_list)
}
