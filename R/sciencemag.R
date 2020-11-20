#' Retrieve selected data from a Science Magazine URL
#'
#' @param addr Relative path to page.
#' @param polite_bow An HTTP polite::bow session object.
#' @param csv_path Path to CSV file.
#'
#' @return A tibble with scraped data, if `csv_path` is missing.
#' @family sciencemag scraping function
#' @name get_sciencemag
#' @examples
#' \dontrun{
#' get_sciencemag_issues(2020, bow("https://www.nature.com"))
#' }
#'
NULL

#' @rdname get_sciencemag
#' @export
get_sciencemag_issues <- function(year, polite_bow, csv_path) {
  addr <- str_c("content/by/year/", year)
  issues_html <- get_page(addr, polite_bow)
  res <- tibble(
    issue_year = as.numeric(year),
    issue_key = issues_html %>%
      html_nodes(".issue-month-detail .highwire-cite-linked-title") %>%
      html_attr("href")
  )

  return_df(res, csv_path)
}


#' @rdname get_sciencemag
#' @export
get_sciencemag_articles <- function(addr,
                                    polite_bow,
                                    csv_path) {
  twil_html <- get_page(addr, polite_bow)

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

  return_df(res, csv_path)
}
