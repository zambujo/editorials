library(tidyverse)
library(glue)
library(rvest)
library(httr)
library(urltools)
library(jsonlite)
library(polite)
library(conflicted)
conflict_prefer("pluck", "purrr")
conflict_prefer("guess_encoding", "readr")
conflict_prefer("filter", "dplyr")


settings <- config::get()
doi_regex <- "\\b10[.][[:digit:]]{4,9}[/][[:graph:]]+\\b"

roger_that <- function(x, msg = "Parsing") {
  glue("{msg} {x} ...") %>%
    message()
}


get_page <- function(addr, obj_polite, verbose = TRUE) {
  if (missing(obj_polite))
    stop("Missing polite::bow() session. Try to be polite. Please.")
  if (verbose)
    roger_that(addr)

  session <- nod(bow = obj_polite, path = addr)
  scrape(session)
}


return_df <- function(x, file_csv, verbose = TRUE) {
  if (missing(x))
    stop("Requires a data frame or tibble to write to disk.")
  if (missing(file_csv)) {
    return(x)
  } else {
    write_csv(x, file_csv, append = file.exists(file_csv))
    if (verbose)
      roger_that(file_csv, msg = "Data added to")
  }
}
