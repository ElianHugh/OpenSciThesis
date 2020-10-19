describe_career <- function(barrierAnalysis) {
totalN <- barrierAnalysis %>%
  distinct(ParticipantNumber) %>%
  tally()
table  <- barrierAnalysis %>%
  # dplyr::filter(!is.na(Barrier)) %>%
  group_by(CareerLevel) %>%
  distinct(ParticipantNumber) %>%
  tally()
table %<>%
  group_by(CareerLevel) %>%
  mutate(Percentage = unlist(round(((n / totalN) * 100), digits = 2)))
# table <- barrierAnalysis %>%
#   dplyr::filter(!is.na(Barrier)) %>%
#   dplyr::filter(CareerLevel == "Unlisted") %>%
#   dplyr::filter(Discipline != "Other") %>%
#   group_by(CareerLevel, Barrier) %>%
#   tally() %>%
#   pivot_wider(names_from = CareerLevel, values_from = n)
apa_table(table,
  caption   = "Descriptive Statistics for Career Levels",
  escape    = TRUE,
  longtable = TRUE
)
}