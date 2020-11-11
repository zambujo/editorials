library("here")
source(here("R", "packages.R"))

# logic -------------------------------------------------------------------

parse_year_issues <- function(year,
                              base_url = "https://science.sciencemag.org") {
  current_url <- glue("{base_url}/content/by/year/{year}")
  glue("Parsing {current_url} ...") %>% message()

  url_list <- current_url %>%
    read_html() %>%
    html_nodes(".issue-month-detail .highwire-cite-linked-title") %>%
    html_attr("href")

  glue("{base_url}{url_list}")
}

parse_twil <- function(twil_url) {
  glue("Parsing {twil_url} ...") %>% message()

  twil_html <- twil_url %>% read_html()

  ref_topics <- twil_html %>%
    html_nodes(".compilation-overline") %>%
    html_text() %>%
    str_to_title()
  ref_editor <- twil_html %>%
    html_nodes(".name") %>%
    html_text()

  twil_reference <- twil_html %>%
    html_nodes("ol .compilation")

  ref_url <- twil_reference %>%
    html_attr("xml:base")
  ref_paper <- twil_reference %>%
    html_nodes("p") %>%
    html_text() %>%
    # assumes that ref ends with ex:  "... (2008)."
    str_subset("[[:space:]][(][12][0-9]{3}[)][.]$")
  # note that ref_paper can contain multiple papers separated by ";"

  tibble(editor = ref_editor,
         topic = ref_topics,
         internal_url = ref_url,
         external_ref = ref_paper)
}


# main --------------------------------------------------------------------

# httr::set_config(httr::user_agent("me@example.com; +https://example.com/info.html"))
url_root = "https://science.sciencemag.org"

archive_links <- 2005:2020 %>%
  sample(1) %>%
  map(parse_year_issues) %>%
  flatten_chr()

(twil_links <- glue("{archive_links}/twil"))

sample_result <- twill_links %>%
  sample(3) %>%
  map_df(parse_twil)

sample_result
