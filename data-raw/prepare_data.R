library("here")
source(here("R", "common.R"))
source(here("R", "doi_utils.R"))

# sciencemag:

science_issues <-
  read_csv(here("data-raw", "sciencemag-issues.csv"))

# TODO: https://github.com/zambujo/editorials/issues/17
science_highlights <-
  read_csv(here("data-raw", "sciencemag-highlights.csv")) %>%
  ## un-nest or "explode" multiple references
  mutate(ref = str_split(ref, "(?<=[[:space:]][(][12][0-9]{3}[)][;])")) %>%
  unnest(ref) %>%
  mutate(
    ref = str_squish(ref),
    ref = str_remove(ref, "[;.]$"),
    doi = str_extract(ref, doi_regex),
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


# nature:

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
    url = str_c("https://www.nature.com", article_key)
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


# science + nature: combine & export

bind_rows(science_highlights,
          nature_highlights) %>%
  arrange(hl_url) %>%
  write_csv(here("data", "research_highlights.csv"))

highlights <- read_csv(here("data", "research_highlights.csv"))

# df <- highlights %>%
#   mutate(
#     citation = str_remove(citation, "doi[:][[:space:]]?"),
#     cit_journal = str_extract(citation, "([A-Z][A-Za-z.[:space:]]+)+"),
#     cit_issue = str_extract(citation, "[0-9]+[,][[:space:]][0-9–]+"),
#     cit_year = str_extract(citation, "[(]\\d{4}[)]"),
#     cit_journal = str_squish(cit_journal)
#   )

# DOIs --------------------------------------------------------------------

dois <- highlights %>%
  distinct(resource) %>%
  drop_na() %>%
  filter(str_detect(resource, doi_regex)) %>%
  pull() %>%
  str_to_lower() %>%
  str_trim()

walk(dois,
     get_cr_data,
     csv_path = here("data-raw", "full_dois.csv"))

short_dois <- highlights %>%
  distinct(resource) %>%
  drop_na() %>%
  filter(!str_detect(resource, doi_regex)) %>%
  pull() %>%
  str_to_lower() %>%
  str_trim() %>%
  str_split("/") %>%
  map(tail, n = 1) %>%
  flatten_chr()

session_bow <- bow(url = "http://doi.org",
                   user_agent = settings$agent)

walk(
  short_dois,
  resolve_shortdoi,
  polite_bow = session_bow,
  csv_path = here("data-raw", "short_dois.csv")
)


# cr metadata for resolved shortDOIs --------------------------------------

resolved_dois <- read_csv(here("data-raw", "short_dois.csv")) %>%
  drop_na() %>%
  mutate(doi = str_remove(doi, "^doi:"),
         doi = str_to_lower(doi),
         doi = str_trim(doi))

resolved_dois %>%
  write_csv(here("data", "shortdoi.csv"))


# combine data and export -------------------------------------------------

resolved_dois %>%
  pull(doi) %>%
  walk(get_cr_data, csv_path = here("data-raw", "full_dois.csv"))

full_dois <- read_csv(here("data-raw", "full_dois.csv")) %>%
  distinct(doi, .keep_all = TRUE) %>%
  arrange(cr_year) %>%
  mutate(cr_year = str_sub(cr_year, 1, 4)) %>%
  write_csv(here("data", "crossref.csv"))
