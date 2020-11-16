library("here")
source(here("R", "packages.R"))

TIME_PERIOD <- 2005:2020

# science magazine --------------------------------------------------------
if (FALSE) {
  source(here("R", "sciencemag.R"))

  session_bow <- bow(url = "https://science.sciencemag.org",
                     user_agent = "Zambujo <https://github.com/zambujo/editorials>",
                     force = TRUE)

  # get issues
  archive_links <- TIME_PERIOD %>%
    map(parse_year_issues, polite_bow = session_bow) %>%
    flatten_chr()

  # get editorial highlights
  glue("{archive_links}/twil") %>%
    walk(
      parse_twil,
      file_path = here("data-raw", "sciencemag-update.csv"),
      polite_bow = session_bow
    )

  highlights <-
    read_csv(here("data-raw", "sciencemag-highlights.csv"))

  highlights <- highlights %>%
    mutate(ref = str_split(ref, "(?<=[[:space:]][(][12][0-9]{3}[)][;])")) %>%
    unnest(ref) %>%
    mutate(
      ref = str_squish(ref),
      ref = str_remove(ref, "[;.]$"),
      doi = str_extract(ref, "\\b10[.][[:digit:]]{4,9}[/][[:graph:]]+\\b")
    )

  write_csv(highlights, here("data", "sciencemag.csv"))
}

# nature ------------------------------------------------------------------
if (TRUE) {
  source(here("R", "nature.R"))

  session_bow <- bow(url = "https://www.nature.com",
                     user_agent = "Martins <https://github.com/zambujo>",
                     force = TRUE)

  # volumes
  nature_volumes <- parse_volumes(session_bow) %>%
    filter(year %in% TIME_PERIOD)

  # issues
  nature_issues <- nature_volumes %>% pull(volume_key) %>%
    map(parse_volume_issues, polite_bow = session_bow) %>%
    bind_rows()

  write_csv(nature_issues, here("data-raw", "nature-issues.csv"))

  # contents
  nature_contents <-
    pull(nature_issues, issue_key) %>%
    map(parse_issue_thisweek, polite_bow = session_bow) %>%
    bind_rows() %>%
    filter(contents_labels == "Research Highlights")
  write_csv(nature_contents, here("data-raw", "nature-contents.csv"))

  # highlights
  pull(nature_contents, article_key) %>%
    walk(
      parse_article,
      file_path = here("data-raw", "nature-highlights.csv"),
      polite_bow = session_bow
    )
}
