library("here")
source(here("R", "packages.R"))


# science magazine --------------------------------------------------------
source(here("R", "sciencemag.R"))

session_bow <- bow(url = "https://science.sciencemag.org",
                  user_agent = "Zambujo <https://github.com/zambujo/editorials>",
                  force = TRUE)

# get issues
archive_links <- 2005:2020 %>%
  map(parse_year_issues, polite_bow = session_bow) %>%
  flatten_chr()

# get editorial highlights
glue("{archive_links}/twil") %>%
  walk(parse_twil,
       file_path = here("data-raw", "sciencemag-highlights.csv"),
       polite_bow = session_bow)


highlights <- read_csv(here("data-raw", "sciencemag-highlights.csv"))

highlights <- highlights %>%
    mutate(ref = str_split(ref, "(?<=[[:space:]][(][12][0-9]{3}[)][;])")) %>%
    unnest(ref) %>%
    mutate(
      ref = str_squish(ref),
      ref = str_remove(ref, "[;.]$"),
      doi = str_extract(ref, "\\b10[.][[:digit:]]{4,9}[/][[:graph:]]+\\b"))

write_csv(highlights, here("data", "sciencemag.csv"))


