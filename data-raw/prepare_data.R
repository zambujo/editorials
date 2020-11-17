library("here")
source(here("R", "packages.R"))


# science mag -------------------------------------------------------------

science_issues <-
  read_csv(here("data-raw", "sciencemag-issues.csv"))

science_highlights <-
  read_csv(here("data-raw", "sciencemag-highlights.csv")) %>%
  ## un-nest or "explode" multiple references
  mutate(ref = str_split(ref, "(?<=[[:space:]][(][12][0-9]{3}[)][;])")) %>%
  unnest(ref) %>%
  mutate(
    ref = str_squish(ref),
    ref = str_remove(ref, "[;.]$"),
    doi = str_extract(ref, "\\b10[.][[:digit:]]{4,9}[/][[:graph:]]+\\b"),
    journal = "science",
    issue_key = str_remove(editorial_key, "/twil"),
    url_rel = str_remove(highlight_key, "[.]full[.]html"),
    url = str_c("https://science.sciencemag.org/content", url_rel)
  ) %>%
  ## add year of the editorial highlight
  left_join(science_issues, by = "issue_key") %>%
  select(
    journal,
    hl_year = issue_year,
    hl_topic = topic,
    hl_title = title,
    hl_url = url,
    citation = ref,
    resource = doi
  )


# nature ------------------------------------------------------------------

nature_issues <-
  read_csv(here("data-raw", "nature-issues.csv")) %>%
  mutate(year = str_extract(issue_date, "\\d{4}"),
         year = as.numeric(year))

nature_contents <-
  read_csv(here("data-raw", "nature-contents.csv"))

nature_highlights <-
  read_csv(here("data-raw", "nature-highlights.csv")) %>%
  mutate(
    journal = "nature",
    doi = str_remove(doi, "https[:]//doi.org/"),
    url = str_c("https://www.nature.com/", article_key)
  ) %>%
  left_join(nature_contents, by = "article_key") %>%
  left_join(nature_issues, by = "issue_key") %>%
  select(
    journal,
    hl_year = year,
    hl_topic = topic,
    hl_title = title,
    hl_url = url,
    citation,
    resource = doi
  )


# combine & export --------------------------------------------------------

bind_rows(science_highlights,
          nature_highlights) %>%
  arrange(hl_url) %>%
  write_csv(here("data", "research_highlights.csv"))
