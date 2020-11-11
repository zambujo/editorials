parse_year_issues <- function(year, polite_bow) {
  glue("Parsing /content/by/year/{year} ...") %>% message()
  session <- nod(bow = polite_bow,
                 path = paste0("content/by/year/", year))
  scrape(session) %>%
    html_nodes(".issue-month-detail .highwire-cite-linked-title") %>%
    html_attr("href")
}

parse_twil <- function(twil_url, polite_bow) {
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

  tibble(
    title = ref_title,
    editor = ref_editor,
    topic = ref_topics,
    url = ref_url,
    ref = ref_paper
  )
}
