library("here")
source(here("R", "common.R"))
source(here("R", "nature.R"))
source(here("R", "sciencemag.R"))

##  initial collection :
time_period <- 2005:2020

# science magazine --------------------------------------------------------
session_bow <- bow(url = "https://science.sciencemag.org",
                   user_agent = glue("<{agent}/>"))
# issues ...
time_period %>%
  walk(
    get_sciencemag_issues,
    polite_bow = session_bow,
    csv_path = here("data-raw", "sciencemag-issues.csv")
  )

# articles ...
read_csv(here("data-raw", "sciencemag-issues.csv")) %>%
  mutate(archive_links = str_c(issue_key, "/twil")) %>%
  pull(archive_links) %>%
  walk(
    get_sciencemag_articles,
    polite_bow = session_bow,
    csv_path = here("data-raw", "sciencemag-highlights.csv")
  )

# nature ------------------------------------------------------------------
session_bow <- bow(url = "https://www.nature.com",
                   user_agent = glue("<{agent}/>"))
# volumes ...
nature_volumes <- get_nature_volumes(polite_bow = session_bow) %>%
  filter(year %in% time_period)
# issues ...
nature_volumes %>%
  pull(volume_key) %>%
  map(
    get_nature_issues,
    polite_bow = session_bow,
    csv_path = here("data-raw", "nature-issues.csv")
  )

# contents ...
read_csv(here("data-raw", "nature-issues.csv")) %>%
  pull(issue_key) %>%
  walk(
    get_nature_contents,
    polite_bow = session_bow,
    csv_path = here("data-raw", "nature-contents.csv")
  )

# articles ...
read_csv(here("data-raw", "nature-contents.csv")) %>%
  filter(str_detect(contents_labels, "Research Highlight")) %>%
  pull(article_key) %>%
  walk(
    get_nature_articles,
    polite_bow = session_bow,
    csv_path = here("data-raw", "nature-highlights.csv")
  )
