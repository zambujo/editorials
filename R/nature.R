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
  volumes <- html_nodes(volumes_html, "#volume-decade-list")
  res <- tibble(
    volume_key = volumes %>%
      html_nodes("a") %>%
      html_attr("href"),
    date = volumes %>%
      html_nodes("time") %>%
      html_text()
  )
  # cast time
  res <- mutate(res, year = as.numeric(str_extract(date, "\\d{4}")))
  return_df(res, csv_path)
}

#' @rdname get_nature
#' @export
get_nature_issues <- function(addr, polite_bow, csv_path) {
  issues_html <- get_page(addr, polite_bow)
  issues <- html_nodes(issues_html, ".flex-box-item")
  res <- tibble(
    issue_key = issues %>%
      html_attr("href"),
    issue_date = issues %>%
      html_nodes(".text-gray") %>%
      html_text()
  )
  return_df(res, csv_path)
}

#' @rdname get_nature
#' @export
get_nature_contents <- function(addr, polite_bow, csv_path) {
  toc_html <- get_page(addr, polite_bow)

  contents <- toc_html %>%
    html_nodes("#ThisWeek-content")
  if (length(contents) == 0) {
    contents <- toc_html %>%
      html_nodes("#ResearchHighlights-section")
  }

  res <- tibble(
    issue_key = addr,
    article_key = contents %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      str_subset("^/articles/"),
    contents_labels = contents %>%
      html_nodes(".mb4 span:nth-child(1)") %>%
      html_text()
  )

  if (nrow(res) > 0)
    return_df(res, csv_path)
}

#' @rdname get_nature
#' @export
get_nature_articles <- function(addr, polite_bow, csv_path) {
  article_html <- get_page(addr, polite_bow)

  article_title <- article_html %>%
    html_node(".c-article-title, .article-item__title") %>%
    html_text()

  article_subject <- article_html %>%
    html_node(".article-item__subject") %>%
    html_text()

  if (is.na(article_subject))
    article_subject <- NA_character_

  doi_txt <- NA_character_
  doi_link <- NA_character_

  if (str_to_lower(article_title) == "research highlights") {
    # early volumes -----------------------------------------
    coda <- c("journal club",
              "rights and permissions",
              "about this article",
              "comments")
    article_title <- article_html %>%
      html_nodes(".c-article-section__title") %>%
      html_text
    stop_at <- which(str_to_lower(article_title) %in% coda) %>% min()
    ## stop short of journal club
    article_title <- head(article_title, stop_at - 1)

    external_refs <- article_html %>%
      html_nodes(".c-article-section__content")

    link_list <- external_refs %>%
      map(html_nodes, css = "a") %>%
      head(length(article_title)) %>%
      map(html_attr, name = "href")

    txt_list <- external_refs %>%
      map(html_node, css = "p") %>%
      head(length(article_title)) %>%
      map(html_text) %>%
      str_squish()

    df_refs <- tibble(ttl = article_title,
                      txt = txt_list,
                      a = link_list) %>%
      unnest(a, keep_empty = TRUE)

    article_title <- pull(df_refs, ttl)
    doi_txt <- pull(df_refs, txt)
    doi_link <- pull(df_refs, a)
  } else {
    # later volumes -----------------------------------------
    external_refs <- article_html %>%
      html_nodes(".c-article-section__content a, .serif")

    doi_idx <- external_refs %>%
      html_attr("href") %>%
      str_detect("doi[.]org")

    if (any(doi_idx)) {
      doi_txt <- external_refs %>%
        html_text() %>%
        subset(doi_idx) %>%
        head(1) %>% ## ! leap of faith
        str_squish()
      doi_link <- external_refs %>%
        html_attr("href") %>%
        subset(doi_idx) %>%
        head(1) ## ! leap of faith
    }
  }

  res <- tibble(
    article_key = addr,
    title = article_title,
    topic = article_subject,
    citation = doi_txt,
    doi = doi_link
  )

  return_df(res, csv_path)
}
