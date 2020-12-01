settings <- config::get()
`%>%` <- magrittr::`%>%` # export the pipe operator
time_period <- seq(settings$start_year, settings$end_year)

roger_that <- function(x, msg = "Parsing") {
  glue::glue("{msg} {x} ...") %>%
    message()
}

get_page <- function(addr,
                     obj_polite,
                     accept = "html",
                     verbose = TRUE) {
  if (missing(obj_polite))
    stop("Missing polite::bow() session. Try to be polite. Please.")
  if (verbose)
    roger_that(addr)

  session <- polite::nod(bow = obj_polite, path = addr)
  return(polite::scrape(session, accept = accept, verbose = verbose))
}

return_df <- function(x, file_csv, verbose = TRUE) {
  if (missing(x))
    stop("Requires a data frame or tibble to write to disk.")
  if (missing(file_csv)) {
    return(x)
  } else {
    readr::write_csv(x, file_csv, append = file.exists(file_csv))
    if (verbose)
      roger_that(file_csv, msg = "Data added to")
  }
}
