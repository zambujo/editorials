library("here")
source(here::here("R", "common.R"))
source(here::here("R", "nature.R"))
source(here::here("R", "sciencemag.R"))

##  initial collection :
time_period <- seq(settings$start_year, settings$end_year)

# science magazine --------------------------------------------------------
session_bow <- polite::bow(url = "https://science.sciencemag.org",
                           user_agent = settings$agent)
# issues ...
time_period %>%
  purrr::walk(
    get_sciencemag_issues,
    polite_bow = session_bow,
    csv_path = here::here("data-raw", "sciencemag-issues.csv")
  )

# articles ...
readr::read_csv(here::here("data-raw", "sciencemag-issues.csv")) %>%
  mutate(archive_links = str_c(issue_key, "/twil")) %>%
  dplyr::pull(archive_links) %>%
  purrr::walk(
    get_sciencemag_articles,
    polite_bow = session_bow,
    csv_path = here::here("data-raw", "sciencemag-highlights.csv")
  )

# nature ------------------------------------------------------------------
session_bow <- polite::bow(url = "https://www.nature.com",
                           user_agent = settings$agent)
# volumes ...
nature_volumes <- get_nature_volumes(polite_bow = session_bow) %>%
  dplyr::filter(year %in% time_period)
# issues ...
nature_volumes %>%
  dplyr::pull(volume_key) %>%
  purrr::map(
    get_nature_issues,
    polite_bow = session_bow,
    csv_path = here::here("data-raw", "nature-issues.csv")
  )

# contents ...
readr::read_csv(here::here("data-raw", "nature-issues.csv")) %>%
  dplyr::pull(issue_key) %>%
  purrr::walk(
    get_nature_contents,
    polite_bow = session_bow,
    csv_path = here::here("data-raw", "nature-contents.csv")
  )

# articles ...
readr::read_csv(here::here("data-raw", "nature-contents.csv")) %>%
  dplyr::filter(str_detect(contents_labels, "Research Highlight")) %>%
  dplyr::pull(article_key) %>%
  purrr::walk(
    get_nature_articles,
    polite_bow = session_bow,
    csv_path = here::here("data-raw", "nature-highlights.csv")
  )
