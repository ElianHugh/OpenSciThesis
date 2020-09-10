##' .. content for \description{} (no empty lines) ..
##' .. content for \details{} ..
##'
##' @title
##' @param
##' @return
##' @author
##' @export

get_key <- function() {
  
  key <- read_file('data/api-key.txt') 

  return(key)
}