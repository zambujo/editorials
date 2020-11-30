#' Retrieve selected data from a Nature journal URL
#'
#' @param addr Relative path to page.
#' @param polite_bow An HTTP polite::bow session object.
#' @param csv_path Path to CSV file.
#'
#' @return A tibble with scraped data, if `csv_path` is missing.
#' @family nature scraping function
#' @name get_nature
#' @examples
#' \dontrun{
#' get_nature_volumes(bow("https://www.nature.com"))
#' }
#'
NULL

#' @rdname get_nature
#' @export
get_nature_volumes <- function(addr,
                               polite_bow,
                               csv_path) {
  if (missing(addr))
    addr <- "nature/volumes"
  volumes_html <- get_page(addr, polite_bow)
  volumes <- rvest::html_nodes(volumes_html, "#volume-decade-list")
  res <- tibble::tibble(
    volume_key = volumes %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href"),
    date = volumes %>%
      rvest::html_nodes("time") %>%
      rvest::html_text()
  )
  # cast time
  res <- dplyr::mutate(res,
                       year = as.numeric(stringr::str_extract(date, "\\d{4}")))
  return_df(res, csv_path)
}

#' @rdname get_nature
#' @export
get_nature_issues <- function(addr, polite_bow, csv_path) {
  issues_html <- get_page(addr, polite_bow)
  issues <- rvest::html_nodes(issues_html, ".flex-box-item")
  res <- tibble::tibble(
    issue_key = issues %>%
      rvest::html_attr("href"),
    issue_date = issues %>%
      rvest::html_nodes(".text-gray") %>%
      rvest::html_text()
  )
  return_df(res, csv_path)
}

#' @rdname get_nature
#' @export
get_nature_contents <- function(addr, polite_bow, csv_path) {
  toc_html <- get_page(addr, polite_bow)

  contents <- toc_html %>%
    rvest::html_nodes("#ThisWeek-content")
  if (length(contents) == 0) {
    contents <- toc_html %>%
      rvest::html_nodes("#ResearchHighlights-section")
  }

  res <- tibble::tibble(
    issue_key = addr,
    article_key = contents %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href") %>%
      stringr::str_subset("^/articles/"),
    contents_labels = contents %>%
      rvest::html_nodes(".mb4 span:nth-child(1)") %>%
      rvest::html_text()
  )

  if (nrow(res) > 0)
    return_df(res, csv_path)
}

#' @rdname get_nature
#' @export
get_nature_articles <- function(addr, polite_bow, csv_path) {
  article_html <- get_page(addr, polite_bow)

  article_title <- article_html %>%
    rvest::html_node(".c-article-title, .article-item__title") %>%
    rvest::html_text()

  article_subject <- article_html %>%
    rvest::html_node(".c-article-title__super, .article-item__subject") %>%
    rvest::html_text()

  doi_txt <- NA_character_
  doi_link <- NA_character_

  if (stringr::str_to_lower(article_title) == "research highlights") {
    # early volumes -----------------------------------------
    coda <- c("journal club",
              "rights and permissions",
              "about this article",
              "comments")
    article_title <- article_html %>%
      rvest::html_nodes(".c-article-section__title") %>%
      html_text
    stop_at <-
      which(stringr::str_to_lower(article_title) %in% coda) %>% min()
    ## stop short of journal club
    article_title <- head(article_title, stop_at - 1)

    external_refs <- article_html %>%
      rvest::html_nodes(".c-article-section__content")

    link_list <- external_refs %>%
      purrr::map(html_nodes, css = "a") %>%
      head(length(article_title)) %>%
      purrr::map(html_attr, name = "href")

    txt_list <- external_refs %>%
      purrr::map(html_node, css = "p") %>%
      head(length(article_title)) %>%
      purrr::map(html_text) %>%
      stringr::str_squish()

    df_refs <- tibble::tibble(ttl = article_title,
                              txt = txt_list,
                              a = link_list) %>%
      unnest(a, keep_empty = TRUE)

    article_title <- dplyr::pull(df_refs, ttl)
    doi_txt <- dplyr::pull(df_refs, txt)
    doi_link <- dplyr::pull(df_refs, a)
  } else {
    # later volumes -----------------------------------------
    external_refs <- article_html %>%
      rvest::html_nodes(".c-article-section__content a, .serif")

    doi_idx <- external_refs %>%
      rvest::html_attr("href") %>%
      stringr::str_detect("doi[.]org")

    if (any(doi_idx)) {
      doi_txt <- external_refs %>%
        rvest::html_text() %>%
        subset(doi_idx) %>%
        head(1) %>% ## ! leap of faith
        stringr::str_squish()
      doi_link <- external_refs %>%
        rvest::html_attr("href") %>%
        subset(doi_idx) %>%
        head(1) ## ! leap of faith
    }
  }

  if (is.na(article_subject)) {
    # try to parse subject from title
    subject_title <- stringr::str_split(article_title, ": ", n = 2)
    article_subject <- purrr::map_chr(subject_title, head, 1)
    article_title <- purrr::map_chr(subject_title, tail, 1)
    if (identical(article_subject, article_title))
      article_subject <- NA_character_
  }

  res <- tibble::tibble(
    article_key = addr,
    title = article_title,
    topic = article_subject,
    citation = doi_txt,
    doi = doi_link
  )

  return_df(res, csv_path)
}
