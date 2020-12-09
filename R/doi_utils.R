# TODO: thank https://github.com/libscie/retractcheck/blob/master/R/utils.R
doi_regex <- "\\b10\\.\\d{4,9}/[-._;()/:[:alnum:]]+\\b"

resolve_shortdoi <- function(short_doi, polite_bow, csv_path) {
  on_publisher <- get_page(short_doi, polite_bow)

  # bypass Elsevier's JS-redirection..
  meta_nodes <- on_publisher %>%
    rvest::html_nodes("meta")
  meta_http <- meta_nodes %>%
    rvest::html_attr("http-equiv")
  redirect_idx <- meta_http %>%
    stringr::str_which(stringr::regex("refresh",
                                      ignore_case = TRUE)) %>%
    head(1)

  if (length(redirect_idx) > 0) {
    redir <- meta_nodes %>%
      purrr::pluck(redirect_idx) %>%
      rvest::html_attr("content") %>%
      url_decode() %>%
      stringr::str_extract("http(.*)(?=[&?])")

    # TODO: might fail on exotic Elsevier's redirection url
    # TODO: recover from possible xml2::read_html() failure
    on_publisher <- httr::GET(redir) %>%
      xml2::read_html()

    meta_nodes <- on_publisher %>%
      rvest::html_nodes("meta")
  }

  meta_names <- meta_nodes %>%
    rvest::html_attr("name")
  doi_idx <- meta_names %>%
    stringr::str_which(stringr::regex("doi|dc.identifier",
                                      ignore_case = TRUE)) %>%
    head(1)
  doi <- meta_nodes %>%
    rvest::html_attr("content") %>%
    purrr::pluck(doi_idx, .default = NA_character_)
  res <- tibble::tibble(short_doi, doi)
  return_df(res, csv_path)
}

get_cr_raw <- function(doi, polite_bow) {
  doi_mailto <- sprintf("works/%s?mailto=%s", doi, settings$mailto)
  resp <- get_page(doi_mailto, polite_bow, accept = "json")
  if (purrr::pluck(resp, "status") == "ok") {
    resp <- purrr::pluck(resp, "message")
  }
  else {
    resp <- NA
  }
  return(resp)
}

crossref_basic <- function(doi,
                           cr_json,
                           csv_path,
                           yml_file = here::here("inst", "crossref.yml")) {
    if (!file.exists(yml_file))
      stop("Crossref definition configuration YAML file could not be found.")
    cr_yml <- yaml::read_yaml(yml_file)

    if (!is.na(head(cr_json, 1))) {
      cr_yml <- cr_yml %>%
        purrr::map( ~ append(., list(1))) # pluck --force 1st element
      cr_fields <- purrr::map(cr_yml, function(x)
        purrr::pluck(cr_json, !!!x, .default = NA_character_))
    } else {
      cr_fields <- purrr::map(cr_yml, function(x)
        NA_character_)
    }
    dplyr::tibble(doi) %>%
      dplyr::bind_cols(dplyr::as_tibble(cr_fields)) %>%
      return_df(csv_path)
  }

crossref_funders <- function(doi, cr_json, csv_path) {
  funder <- NA_character_
  if (!is.na(head(cr_json, 1))) {
    funder <- purrr::pluck(cr_json, "funder", .default = NA_character_)
    if (!is.na(head(funder, 1))) {
      funder <- purrr::map_chr(funder, "name")
    }
  }
  dplyr::tibble(doi, funder) %>%
    return_df(csv_path)
}
