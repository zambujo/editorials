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
  addr <- stringr::str_c("content/by/year/", year)
  issues_html <- get_page(addr, polite_bow)
  res <- tibble::tibble(
    issue_year = as.numeric(year),
    issue_key = issues_html %>%
      rvest::html_nodes(".issue-month-detail .highwire-cite-linked-title") %>%
      rvest::html_attr("href")
  )

  return_df(res, csv_path)
}


#' @rdname get_sciencemag
#' @export
get_sciencemag_articles <- function(addr,
                                    polite_bow,
                                    csv_path) {
  validate_ref <- function(x) {
    x_wc <-  purrr::map_int(stringr::str_split(x, " "), length)
    if (x_wc > 25 | stringr::str_detect(x, "^[↵]")) {
      x <- NA_character_
    }
    x
  }

  twil_html <- get_page(addr, polite_bow)

  ref_title <- twil_html %>%
    rvest::html_nodes("#content-block-markup h1") %>%
    rvest::html_text()

  ref_editor <- twil_html %>%
    rvest::html_nodes(".name") %>%
    rvest::html_text()

  ref_topics <- twil_html %>%
    rvest::html_nodes(".compilation-overline") %>%
    rvest::html_text() %>%
    stringr::str_to_title()

  twil_reference <- twil_html %>%
    rvest::html_nodes("ol .compilation")

  ref_url <- twil_reference %>%
    rvest::html_attr("xml:base")

  # TODO: https://github.com/zambujo/editorials/issues/17
  p_ids <- twil_reference %>%
    rvest::html_nodes("p") %>%
    rvest::html_attr("id")
  p_wc <- twil_reference %>%
    rvest::html_nodes("p") %>%
    rvest::html_text() %>%
    stringr::str_split(" ") %>%
    purrr::map_int(length)
  # expecting summaries w/ more than 60 words
  p_idx <- 1 + which(p_wc > 60)
  p_path <- glue("#{p_ids[p_idx]}") %>%
    paste(collapse = ", ")
  ref_paper <- twil_reference %>%
    rvest::html_nodes(p_path) %>%
    rvest::html_text() %>%
    purrr::map_chr(validate_ref) %>%
    na.omit() %>%
    as.character()

  if (length(ref_editor) != length(ref_title))
    ref_editor = rep(NA_character_, length(ref_title))

  if (length(ref_paper) != length(ref_title))
    ref_paper = rep(NA_character_, length(ref_title))

  res <- tibble::tibble(
    highlight_key = ref_url,
    title = ref_title,
    editor = ref_editor,
    topic = ref_topics,
    ref = ref_paper
  ) %>%
    dplyr::mutate(editorial_key = addr) %>%
    dplyr::select(editorial_key, dplyr::everything())

  return_df(res, csv_path)
}
