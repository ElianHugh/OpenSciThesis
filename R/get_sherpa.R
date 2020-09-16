##' .. content for \description{} (no empty lines) ..
##' .. content for \details{} ..
##'
##' @title
##' @param
##' @return
##' @author
##' @exportwb

get_sherpa <- function() {
      sherpa <- read_csv(file_in("data/Sherpa.csv"),
            col_types = cols(X1 = col_skip())
      )
       
      return(sherpa)
}