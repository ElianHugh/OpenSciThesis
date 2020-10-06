describe_discipline <- function(x) {
  x %<>%
    select(ParticipantNumber, Discipline, Barrier)
  #x <- x[complete.cases(x), ]
  x %<>%
    distinct(ParticipantNumber, .keep_all = TRUE)

  # Count N by discipline
  x %<>%
    group_by(Discipline) %>%
    tally()

  return(x)
}