# Returns a simple grouped N
describe_journal <- function(x) {
  x %<>%
    group_by(Discipline) %>%
    tally()
}