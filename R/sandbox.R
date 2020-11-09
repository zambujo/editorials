library(tidyverse)
library(here)
library(glue)
library(rvest)
library(usethis)
library(conflicted)
conflict_prefer("pluck", "purrr", "rvest")
conflict_prefer("guess_encoding", "rvest", "readr")


# YEAR_RANGE <- 2005:2021
url_root <- "https://science.sciencemag.org"

# issues ------------------------------------------------------------------



parse_year_issues <- function(year, base_url = url_root) {
  current_url <- glue("{base_url}/content/by/year/{year}")
  glue("Parsing {current_url} ...") %>%
    ui_info()
  current_url %>%
    read_html() %>%
    html_nodes(".issue-month-detail .highwire-cite-linked-title") %>%
    html_attr("href")
}

parse_twil <- function(twil_url) {
  glue("Parsing {twil_url} ...") %>%
    ui_info()

  twil_html <- twil_url %>%
    read_html()

  ref_topics <- twil_html %>%
    html_nodes(".compilation-overline") %>%
    html_text()

  twil_reference <- twil_html %>%
    html_nodes("ol .compilation")

  ref_url <- twil_reference %>%
    html_attr("xml:base")

  ref_paper <- twil_reference %>%
    html_text("p") %>%
    str_split(" — ") %>%
    map_chr(tail, 1) %>%
    str_squish()

  tibble(
    topic = ref_topics,
    internal_url = ref_url,
    external_ref = ref_paper
  )
}


# main --------------------------------------------------------------------

## Testing

archive_links <- 2007:2008 %>%
  map(parse_year_issues) %>%
  flatten_chr()

twil_url <- glue("{url_root}{archive_links}/twil")

sample_result <- twil_url %>%
  sample(2) %>%
  map_df(parse_twil)

