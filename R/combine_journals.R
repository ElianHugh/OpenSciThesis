#' title
#' @param param
#' @return return
#' @author author
#' @export
combine_journals <- function(masterList, topFactor, journalPolicy) {

# Load internal function
coalesce_by_column <- function(df) {
  return(dplyr::coalesce(!!!as.list(df)))
}

# Final table joins
a <- left_join(masterList, journalPolicy)
a <- a[!is.na(a$ISSN), ]
combinedTableSJR <- left_join(a, topFactor, by = "Title")

combinedTableSJR %<>%
  group_by(Title) %>%
  summarise_all(coalesce_by_column)
combinedTableSJR[c(4, 6, 8, 10, 12)] <- list(NULL)

# Rename cols
combinedTableSJR %<>%
  rename(
    Submitted = `submitted == "submitted"`,
    Accepted = `accepted == "accepted"`,
    # ! CHECK WHY THIS IS MISSING
    # Published = `published == "published"`, 
    DataTransparency = `Data transparency score`,
    DesignAnalysis = `Design & analysis reporting guidelines score`,
    Preregistration = `Study preregistration score`,
    Replication = `Replication score`,
    Materials = `Materials transparency score`,
    AnalysisPreReg = `Analysis plan preregistration score`,
    RegRepPubBias = `Registered reports & publication bias score`
  )

# Quirk that converts logical to integer by multiplying by 1
combinedTableSJR %<>%
  mutate_at(
    # vars(c(Submitted, Published, Accepted)),
    vars(c(Submitted,Accepted)),
    (function(x) x*1)
  )

# Sum the columns 
# ! ADD PUBLISHED TO THIS LIST!
combinedTableSJR %<>%
  mutate(`Open Science Score` = select(., c(Submitted, Accepted, DataTransparency, DesignAnalysis, Preregistration, Replication, Materials, AnalysisPreReg, RegRepPubBias)) %>%
  rowSums(na.rm = TRUE)
  )

combinedTableSJR %<>%
    group_by(Discipline) %>%
    arrange(desc('Open Science Score', .by_group = TRUE)) %>%
    slice_head(n=5) 

return(combinedTableSJR)
}