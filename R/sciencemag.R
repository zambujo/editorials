get_sciencemag_issues <- function(year, file_path, polite_bow) {
  if (missing(polite_bow))
    stop("Missing polite::bow() session. Try to be polite. Please.")

  url_year <- str_c("content/by/year/", year)
  glue("Parsing /{url_year} ...") %>% message()
  session <- nod(bow = polite_bow,
                 path = url_year)
  res <- tibble(
    issue_year = as.integer(year),
    issue_key = scrape(session) %>%
      html_nodes(".issue-month-detail .highwire-cite-linked-title") %>%
      html_attr("href")
  )

  if (missing(file_path)) {
    return(res)
  }
  else {
    write_csv(res, file_path, append = file.exists(file_path))
  }

}

get_sciencemag_articles <-
  function(twil_url, file_path, polite_bow) {
    glue("Parsing {twil_url} ...") %>% message()
    session <- nod(bow = polite_bow,
                   path = twil_url)
    twil_html <- scrape(session)

    ref_title <- twil_html %>%
      html_nodes("#content-block-markup h1") %>%
      html_text()

    ref_editor <- twil_html %>%
      html_nodes(".name") %>%
      html_text()

    ref_topics <- twil_html %>%
      html_nodes(".compilation-overline") %>%
      html_text() %>%
      str_to_title()

    twil_reference <- twil_html %>%
      html_nodes("ol .compilation")

    ref_url <- twil_reference %>%
      html_attr("xml:base")

    p_ids <- twil_reference %>%
      html_nodes("p") %>%
      html_attr("id")
    p_idx <- p_ids %>%
      str_sub(1, 16) %>%
      table() %>%
      cumsum()
    p_path <- glue("#{p_ids[p_idx]}") %>%
      paste(collapse = ", ")
    ref_paper <- twil_reference %>%
      html_nodes(p_path) %>%
      html_text()

    if (length(ref_editor) != length(ref_title))
      ref_editor = rep(NA_character_, length(ref_title))

    res <- tibble(
      highlight_key = ref_url,
      title = ref_title,
      editor = ref_editor,
      topic = ref_topics,
      ref = ref_paper
    ) %>%
      mutate(editorial_key = twil_url) %>%
      select(editorial_key, everything())

    if (missing(file_path)) {
      return(res)
    }
    else {
      write_csv(res, file_path, append = file.exists(file_path))
    }
  }
