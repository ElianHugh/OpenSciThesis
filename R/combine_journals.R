#' title
#' @param param
#' @return return
#' @author author
#' @export
combine_journals <- function(combinedCite, journalPolicies) {
  journalPolicies %<>%
    select(-ISSN) %>%
    distinct(Title, .keep_all = TRUE) %>%
     mutate_at(vars(Submitted:Accepted), ~ case_when(
        . == TRUE ~ 1,
        . == FALSE ~ 0,
        . == NA ~ 999,
        TRUE ~ 999
    ))
 

temp <- left_join(combinedCite, journalPolicies)
y <- anti_join(journalPolicies, temp)
x <- temp[is.na(temp$Submitted), ] %>%
  ungroup() %>%
  select(Title, MatchTitle)

}
