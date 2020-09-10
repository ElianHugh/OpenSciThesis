#' title
#' @param param
#' @return return
#' @author author
#' @export
describe_journal <- function(x) {
  x %<>%
    group_by(Discipline) %>%
    tally()
}