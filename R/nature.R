#' Retrieve the URL of the listed volumes of the journal
#'
#' @param polite_bow An HTTP polite::bow session object.
#'
#' @return A tibble with the URL, the date, and the corresponding
#' year of publication of each volume.
#'
#' @export
#' @examples
#' \dontrun{
#' nature_volumes(bow("https://www.nature.com"))
#' }
#'
nature_volumes <- function(polite_bow) {
  if (missing(polite_bow))
    stop("Missing polite::bow() session")

  session <- nod(bow = polite_bow,
                 path = "nature/volumes")
  volumes <- scrape(session) %>%
    html_nodes("#volume-decade-list")

  tibble(
    volume_key = volumes %>% html_nodes("a") %>% html_attr("href"),
    date = volumes %>% html_nodes("time") %>% html_text()
  ) %>%
    mutate(year = str_extract(date, "\\d{4}"),
           year = as.integer(year))
}


nature_issues <- function(volume, polite_bow) {
  glue("Parsing {volume} ...") %>% message()

  session <- nod(bow = polite_bow, path = volume)

  issues <- scrape(session) %>%
    html_nodes(".flex-box-item")

  tibble(
    issue_key = issues %>% html_attr("href"),
    issue_date = issues %>% html_nodes(".text-gray") %>% html_text()
  )
}

nature_contents <- function(issue, file_path, polite_bow) {
  glue("Parsing {issue} ...") %>% message()

  session <- nod(bow = polite_bow, path = issue)
  session_html <- scrape(session)

  contents <- session_html %>%
    html_nodes("#ThisWeek-content")
  if (length(contents) == 0) {
    contents <- session_html %>%
      html_nodes("#ResearchHighlights-section")
  }

  res <- tibble(
    issue_key = issue,
    article_key = contents %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      str_subset("^/articles/"),
    contents_labels = contents %>%
      html_nodes(".mb4 span:nth-child(1)") %>%
      html_text()
  )

  if (nrow(res) > 0) {
    if (missing(file_path)) {
      return(res)
    } else {
      write_csv(res, file_path, append = file.exists(file_path))
    }
  }
}

nature_articles <- function(article, file_path, polite_bow) {
  glue("Parsing {article} ...") %>% message()

  session <- nod(bow = polite_bow, path = article)
  session_html <- scrape(session)

  article_title <- session_html %>%
    html_node(".c-article-title, .article-item__title") %>%
    html_text()

  article_subject <- session_html %>%
    html_node(".article-item__subject") %>%
    html_text()

  if (is.na(article_subject))
    article_subject <- NA_character_

  doi_txt <- NA_character_
  doi_link <- NA_character_

  if (str_to_lower(article_title) == "research highlights") {
    # early volumes -----------------------------------------
    coda <- c("Journal club",
              "Rights and permissions",
              "About this article",
              "Comments")
    article_title <- session_html %>%
      html_nodes(".c-article-section__title") %>%
      html_text
    stop_at <- which(article_title %in% coda) %>% min()
    ## stop short of journal club
    article_title <- head(article_title, stop_at - 1)

    external_refs <- session_html %>%
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
    external_refs <- session_html %>%
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
    article_key = article,
    title = article_title,
    topic = article_subject,
    citation = doi_txt,
    doi = doi_link
  )

  if (missing(file_path)) {
    return(res)
  } else {
    write_csv(res, file_path, append = file.exists(file_path))
  }
}
