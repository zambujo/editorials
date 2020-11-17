library("here")
source(here("R", "packages.R"))
source(here("R", "sciencemag.R"))
source(here("R", "nature.R"))

TIME_PERIOD <- 2005:2020

# science magazine --------------------------------------------------------
session_bow <- bow(url = "https://science.sciencemag.org",
                   user_agent = "Zambujo <https://github.com/zambujo/editorials>",
                   force = TRUE)

# issues
archive_links <- TIME_PERIOD %>%
  map(parse_year_issues, polite_bow = session_bow) %>%
  flatten_chr()

# articles
glue("{archive_links}/twil") %>%
  walk(
    parse_twil,
    file_path = here("data-raw", "sciencemag-highlights.csv"),
    polite_bow = session_bow
  )

# tidy up
highlights <-
  read_csv(here("data-raw", "sciencemag-highlights.csv"))

highlights %>%
  mutate(ref = str_split(ref, "(?<=[[:space:]][(][12][0-9]{3}[)][;])")) %>%
  unnest(ref) %>%
  mutate(
    ref = str_squish(ref),
    ref = str_remove(ref, "[;.]$"),
    doi = str_extract(ref, "\\b10[.][[:digit:]]{4,9}[/][[:graph:]]+\\b")
  ) %>%
  write_csv(here("data", "sciencemag.csv"))

# nature ------------------------------------------------------------------
session_bow <- bow(url = "https://www.nature.com",
                   user_agent = "Martins <https://github.com/zambujo>",
                   force = TRUE)

# volumes ...
nature_volumes <- nature_volumes(session_bow) %>%
  filter(year %in% TIME_PERIOD)

# issues ...
nature_issues <- nature_volumes %>% pull(volume_key) %>%
  map(nature_issues, polite_bow = session_bow) %>%
  bind_rows()

write_csv(nature_issues, here("data-raw", "nature-issues.csv"))

# contents ...
read_csv(here("data-raw", "nature-issues.csv")) %>%
  pull(issue_key) %>%
  walk(
    nature_contents,
    file_path = here("data-raw", "nature-contents.csv"),
    polite_bow = session_bow
  )

# articles ...
read_csv(here("data-raw", "nature-contents.csv")) %>%
  filter(str_detect(contents_labels, "Research Highlight")) %>%
  pull(article_key) %>%
  walk(
    nature_articles,
    file_path = here("data-raw", "nature-highlights.csv"),
    polite_bow = session_bow
  )
