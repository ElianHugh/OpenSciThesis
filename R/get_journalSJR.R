##' .. content for \description{} (no empty lines) ..
##' .. content for \details{} ..
##'
##' @title
##' @param
##' @return
##' @author
##' @export

get_journalSJR <- function() {
  journalSJR <- readr::read_delim(file_in("data/scimagojr 2019.csv"),
    ";",
    escape_double = FALSE,
    col_types = cols(
      `Citable Docs. (3years)` = col_skip(),
      `Cites / Doc. (2years)` = col_skip(),
      Country = col_skip(),
      Coverage = col_skip(),
      `H index` = col_skip(),
      Publisher = col_skip(),
      `Ref. / Doc.` = col_skip(),
      `SJR Best Quartile` = col_skip(),
      `Total Cites (3years)` = col_skip(),
      `Total Docs. (2019)` = col_skip(),
      `Total Docs. (3years)` = col_skip(),
      `Total Refs.` = col_skip()
    ),
    trim_ws = TRUE
  )
  colnames(journalSJR)[5] <- "ISSN"
  return(journalSJR)
}