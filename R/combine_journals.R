#' title
#' @param param
#' @return return
#' @author author
#' @export
combine_journals <- function(combinedCite, journalPolicies) {

combinedRow <- combinedCite %>%
  ungroup() %>%
  distinct(Title) %>%
  tally()
journalRow <- journalPolicies %>%
  distinct(Title) %>%
  tally()

message("Number of unique journals found in combinedCite: ", combinedRow)
message("Number of unique journals found in journalPolicies: ", journalRow)
  journalPolicies %<>%
    select(-ISSN) %>%
    distinct(Title, .keep_all = TRUE) %>%
     mutate_at(vars(Submitted:Accepted), ~ case_when(
        . == TRUE ~ 1,
        . == FALSE ~ 0,
        . == NA ~ 999,
        TRUE ~ 999
    ))
 
x <- combinedCite %>%
  ungroup()

df <- left_join(x, journalPolicies)
y <- df[is.na(df$Submitted), ] %>%
  select(
    Title,
    MatchTitle,
    ISSN,
    Publisher,
    DataTransparency,
    AnalysisTransparency,
    MaterialsTransparency,
    DesignAnalysis,
    Preregistration,
    Replication,
    AnalysisPreReg,
    RegRepPubBias,
    DataTransparency,
    DataCitation,
    Badges
  )

journalPolicies %<>%
  rename(MatchTitle = Title)
df2 <- left_join(y, journalPolicies, by = "MatchTitle")
df <- coalesce_join(df, df2, by = "Title")
leftover <- df[is.na(df$Submitted), ]
df <- anti_join(df, leftover, by = "Title")

message("Final number of journals : ", nrow(df %>% ungroup() %>% distinct(Title)))

return(df)

}

