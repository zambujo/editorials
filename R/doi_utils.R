
resolve_shortdoi <- function(short_doi, polite_bow, csv_path) {
  on_publisher <- get_page(short_doi, polite_bow)

  # bypass Elsevier's JS-redirection..
  meta_nodes <- on_publisher %>%
    html_nodes('meta')
  meta_http <- meta_nodes %>%
    html_attr('http-equiv')
  redirect_idx <- meta_http %>%
    str_which(regex("refresh", ignore_case = TRUE)) %>%
    head(1)

  if (length(redirect_idx) > 0) {
    redir <- meta_nodes %>%
      pluck(redirect_idx) %>%
      html_attr('content') %>%
      url_decode() %>%
      str_extract("http(.*)(?=[&?])")

    # TODO: might fail on exotic Elsevier's redirection url
    # TODO: recover from possible read_html() failure
    on_publisher <- httr::GET(redir) %>%
      read_html()

    meta_nodes <- on_publisher %>%
      html_nodes('meta')
  }

  meta_names <- meta_nodes %>%
    html_attr('name')
  doi_idx <- meta_names %>%
    str_which(regex("doi|dc.identifier", ignore_case = TRUE)) %>%
    head(1)
  doi <- meta_nodes %>%
    html_attr('content') %>%
    pluck(doi_idx, .default = NA_character_)
  res <- tibble(short_doi, doi)
  return_df(res, csv_path)
}


get_cr_data <- function(doi, csv_path, verbose = TRUE) {
  agent <- settings$agent
  url <-
    sprintf("https://api.crossref.org/v1/works/%s", url_encode(doi))
  resp <- GET(url, add_headers("user-agent" = settings$agent))
  res <- tibble(
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
  if (http_type(resp) == "application/json" & !http_error(resp)) {
    json <- fromJSON(content(resp, "text"), flatten = TRUE)
    res <- res %>%
      mutate(
        cr_title = pluck(json,
                         "message",
                         "title",
                         1,
                         .default = NA_character_),
        cr_container_title = pluck(json,
                                   "message",
                                   "container-title",
                                   1,
                                   .default = NA_character_),
        cr_short_container_title = pluck(json,
                                         "message",
                                         "short-container-title",
                                         1,
                                         .default = NA_character_),
        cr_volume = pluck(json,
                          "message",
                          "volume",
                          1,
                          .default = NA_character_),
        cr_issue = pluck(json,
                         "message",
                         "issue",
                         1,
                         .default = NA_character_),
        cr_page = pluck(json,
                        "message",
                        "page",
                        1,
                        .default = NA_character_),
        cr_year = pluck(json,
                        "message",
                        "created",
                        "date-time",
                        1,
                        .default = NA_character_)
      )
  }
  return_df(res, csv_path)
}
