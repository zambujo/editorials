library("here")
source(here("R", "packages.R"))
source(here("R", "sciencemag.R"))
source(here("R", "nature.R"))

agent <- as_tibble(read.dcf(here("DESCRIPTION"))) %>% pull(URL)

##  initial collection :
time_period <- 2005:2020

# science magazine --------------------------------------------------------
session_bow <- bow(url = "https://science.sciencemag.org",
                   user_agent = glue("<{agent}/>"))
# issues ...
time_period %>%
  walk(get_sciencemag_issues,
         file_path = here("data-raw", "sciencemag-issues.csv"),
         polite_bow = session_bow)

# articles ...
read_csv(here("data-raw", "sciencemag-issues.csv")) %>%
  mutate(archive_links = str_c(issue_key, "/twil")) %>%
  pull(archive_links) %>%
  walk(
    get_sciencemag_articles,
    file_path = here("data-raw", "sciencemag-highlights.csv"),
    polite_bow = session_bow
  )

# nature ------------------------------------------------------------------
session_bow <- bow(url = "https://www.nature.com",
                   user_agent = glue("<{agent}/>"))
# volumes ...
nature_volumes <- get_nature_volumes(session_bow) %>%
  filter(year %in% time_period)
# issues ...
nature_volumes %>%
  pull(volume_key) %>%
  map(
    get_nature_issues,
    file_path = here("data-raw", "nature-issues.csv"),
    polite_bow = session_bow
  )
# contents ...
read_csv(here("data-raw", "nature-issues.csv")) %>%
  pull(issue_key) %>%
  walk(
    get_nature_contents,
    file_path = here("data-raw", "nature-contents.csv"),
    polite_bow = session_bow
  )
# articles ...
read_csv(here("data-raw", "nature-contents.csv")) %>%
  filter(str_detect(contents_labels, "Research Highlight")) %>%
  pull(article_key) %>%
  walk(
    get_nature_articles,
    file_path = here("data-raw", "nature-highlights.csv"),
    polite_bow = session_bow
  )
