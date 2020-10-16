describe_discipline <- function(barrierAnalysis) {
totalN <- barrierAnalysis %>%
  distinct(ParticipantNumber) %>%
  tally()
table <- barrierAnalysis %>%
  group_by(Discipline) %>%
  distinct(ParticipantNumber) %>%
  tally()
table %<>%
  group_by(Discipline) %>%
  mutate(Percentage = unlist(round(((n / totalN) * 100), digits = 2)))
apa_table(table,
  caption   = "Descriptive Statistics for Disciplines",
  escape    = TRUE,
  longtable = TRUE
)
}