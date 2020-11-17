library("here")
source(here("R", "packages.R"))

science_highlights <-
  read_csv(here("data-raw", "sciencemag-highlights.csv"))

science_highlights %>%
  mutate(ref = str_split(ref, "(?<=[[:space:]][(][12][0-9]{3}[)][;])")) %>%
  unnest(ref) %>%
  mutate(
    ref = str_squish(ref),
    ref = str_remove(ref, "[;.]$"),
    doi = str_extract(ref, "\\b10[.][[:digit:]]{4,9}[/][[:graph:]]+\\b")
  ) %>%
  write_csv(here("data", "sciencemag.csv"))
