library(tidyverse)
library(glue)
library(rvest)
library(polite)
library(conflicted)
conflict_prefer("pluck", "purrr")
conflict_prefer("guess_encoding", "readr")
conflict_prefer("filter", "dplyr")


return_df <- function(x, file_csv) {
  if (missing(x))
    stop("Requires a data frame or tibble to write to disk.")
  if (missing(file_csv)) {
    return(x)
  } else {
    write_csv(x, file_csv, append = file.exists(file_csv))
  }
}

get_page <- function(addr, obj_polite) {
  if (missing(obj_polite))
    stop("Missing polite::bow() session. Try to be polite. Please.")

  session <- nod(bow = obj_polite, path = addr)
  scrape(session)
}
