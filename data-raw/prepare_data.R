source(here::here("R", "common.R"))
source(here::here("R", "doi_utils.R"))


# sciencemag:

science_issues <-
  readr::read_csv(here::here("data-raw", "sciencemag-issues.csv"))

# TODO: https://github.com/zambujo/editorials/issues/17
science_highlights <-
  readr::read_csv(here::here("data-raw", "sciencemag-highlights.csv")) %>%
  ## un-nest or "explode" multiple references
  dplyr::mutate(ref = stringr::str_split(ref, "(?<=[[:space:]][(][12][0-9]{3}[)][;])")) %>%
  tidyr::unnest(ref) %>%
  dplyr::mutate(
    ref = stringr::str_squish(ref),
    ref = stringr::str_remove(ref, "[;.]$"),
    doi = stringr::str_extract(ref, doi_regex),
    journal = "science",
    issue_key = stringr::str_remove(editorial_key, "/twil"),
    url_rel = stringr::str_remove(highlight_key, "[.]full[.]html"),
    url = stringr::str_c("https://science.sciencemag.org/content", url_rel)
  ) %>%
  ## add year of the editorial highlight
  dplyr::left_join(science_issues, by = "issue_key") %>%
  dplyr::select(
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
  readr::read_csv(here::here("data-raw", "nature-issues.csv")) %>%
  dplyr::mutate(year = stringr::str_extract(issue_date, "\\d{4}"),
                year = as.numeric(year))

nature_contents <-
  readr::read_csv(here::here("data-raw", "nature-contents.csv"))

nature_highlights <-
  readr::read_csv(here::here("data-raw", "nature-highlights.csv")) %>%
  dplyr::mutate(
    journal = "nature",
    doi = stringr::str_remove(doi, "https[:]//doi.org/"),
    url = stringr::str_c("https://www.nature.com", article_key)
  ) %>%
  dplyr::left_join(nature_contents, by = "article_key") %>%
  dplyr::left_join(nature_issues, by = "issue_key") %>%
  dplyr::select(
    journal,
    hl_year = year,
    hl_topic = topic,
    hl_title = title,
    hl_url = url,
    citation,
    resource = doi
  )


# science + nature: combine & export

dplyr::bind_rows(science_highlights,
                 nature_highlights) %>%
  dplyr::arrange(hl_url) %>%
  readr::write_csv(here::here("data", "research_highlights.csv"))

highlights <-
  readr::read_csv(here::here("data", "research_highlights.csv"))

# TODO: https://github.com/zambujo/editorials/issues/16
df <- highlights %>%
  dplyr::mutate(
    citation = stringr::str_remove(citation, "doi[:][[:space:]]?"),
    cit_journal = stringr::str_extract(citation, "([A-Z][A-Za-z.[:space:]]+)+"),
    cit_issue = stringr::str_extract(citation, "[0-9]+[,][[:space:]][0-9–]+"),
    cit_year = stringr::str_extract(citation, "[(]\\d{4}[)]"),
    cit_journal = stringr::str_squish(cit_journal)
  )

# DOIs --------------------------------------------------------------------


highlights <- highlights %>%
  dplyr::mutate(
    doi = stringr::str_extract(resource,
                               stringr::regex(doi_regex, ignore_case = TRUE)),
    doi = stringr::str_to_lower(doi),
    doi = stringr::str_trim(doi)
  )

full_dois <-
  readr::read_csv(here::here("data-raw", "full_dois.csv"))

dois <- highlights %>%
  tidyr::drop_na(doi) %>%
  dplyr::distinct(doi) %>%
  dplyr::anti_join(full_dois, by = c("doi")) %>%
  dplyr::pull()

purrr::walk(dois,
            get_cr_data,
            csv_path = here::here("data-raw", "new_full_dois.csv"))

short_dois <-
  highlights %>%
  dplyr::distinct(resource) %>%
  tidyr::drop_na() %>%
  dplyr::filter(stringr::str_detect(resource, "doi[.]org")) %>%
  dplyr::filter(!stringr::str_detect(resource, doi_regex)) %>%
  dplyr::mutate(
    resource = stringr::str_to_lower(resource),
    resource = stringr::str_trim(resource),
    resource = stringr::str_split(resource, "/"),
    resource = purrr::map(resource, tail, n = 1),
    resource = purrr::flatten_chr(resource)
  ) %>%
  dplyr::pull()

session_bow <- polite::bow(url = "http://doi.org",
                           user_agent = settings$agent)

purrr::walk(
  short_dois,
  resolve_shortdoi,
  polite_bow = session_bow,
  csv_path = here::here("data-raw", "short_dois.csv")
)


# cr metadata for resolved shortDOIs --------------------------------------

resolved_dois <-
  readr::read_csv(here::here("data-raw", "short_dois.csv")) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(
    doi = stringr::str_remove(doi, "^doi:"),
    doi = stringr::str_to_lower(doi),
    doi = stringr::str_trim(doi)
  )

resolved_dois %>%
  readr::write_csv(here::here("data", "shortdoi.csv"))


# combine data and export -------------------------------------------------

resolved_dois %>%
  dplyr::pull(doi) %>%
  purrr::walk(get_cr_data, csv_path = here::here("data-raw", "full_dois.csv"))

readr::read_csv(here::here("data-raw", "full_dois.csv")) %>%
  dplyr::distinct(doi, .keep_all = TRUE) %>%
  dplyr::arrange(cr_year) %>%
  dplyr::mutate(
    cr_title = stringr::str_squish(cr_title),
    cr_year = stringr::str_sub(cr_year, 1, 4)
  ) %>%
  readr::write_csv(here::here("data", "crossref.csv"))
