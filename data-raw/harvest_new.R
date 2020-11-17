library("here")
source(here("R", "packages.R"))
source(here("R", "sciencemag.R"))
source(here("R", "nature.R"))

agent <- as_tibble(read.dcf(here("DESCRIPTION"))) %>% pull(URL)

# science magazine --------------------------------------------------------
session_bow <- bow(url = "https://science.sciencemag.org",
                   user_agent = glue("<{agent}/>"))

# TODO: check what has been harvested
# TODO: check what else is there to harvest
# TODO: add the new harvest to the existing

# nature ------------------------------------------------------------------
session_bow <- bow(url = "https://www.nature.com",
                   user_agent = glue("<{agent}/>"))

# TODO: check what has been harvested
# TODO: check what else is there to harvest
# TODO: add the new harvest to the existing

