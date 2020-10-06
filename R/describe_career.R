describe_career <- function(y) {
  y %<>%
    select(ParticipantNumber, CareerLevel, Barrier)
  #y <- y[complete.cases(y), ]
  y %<>%
    distinct(ParticipantNumber, .keep_all = TRUE)
  y %<>%
    group_by(CareerLevel) %>%
    tally()
  return(y)
}