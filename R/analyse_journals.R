#' title
#' @param param
#' @return return
#' @author author
#' @export
analyse_journals <- function(journalSJR, topFactor) {

categoryVector <- c(
  "Agricultural and Biological Sciences",
  "Arts and Humanities",
  "Biochemistry, Genetics and Molecular Biology",
  "Business, Management and Accounting",
  "Chemical Engineering",
  "Chemistry",
  "Computer Science",
  "Decision Science",
  "Dentistry",
  "Earth and Planetary Science",
  "Economics, Econometrics and Finance",
  "Energy",
  "Engineering",
  "Environmental Science",
  "Health Professions",
  "Immunology and Microbiology",
  "Materials Science",
  "Mathematics",
  "Medicine",
  "Multidisciplinary",
  "Neuroscience",
  "Nursing",
  "Pharmacology, Toxicology and Pharmaceutics",
  "Physics and Astronomy",
  "Psychology",
  "Social Sciences",
  "Veterinary"
)

# use the categoryVector to iterate over journalSJR, creating a list of vectors. Then, convert the list to a data frame
topJournals <- sapply(categoryVector, function(i) {
  journalSJR %>%
    select(Title, ISSN, Type, Categories, SJR) %>%
    dplyr::filter(Type == "journal") %>%
    dplyr::filter(grepl(i, Categories)) %>%
    top_n(20, SJR) %>%
    select(ISSN)
})

topJournals <- as.data.frame(do.call(rbind, topJournals))
for (i in 1:20) {
  names(topJournals)[i] <- i
}

colnames(topJournals) <-
  paste("Journal", colnames(topJournals), sep = " ")
rownames(topJournals) <- gsub("\\..*", "", rownames(topJournals))
topJournals <- rownames_to_column(topJournals)
colnames(topJournals)[1] <- 'Discipline'

# Convert top journals to list, then dataframe, for parsing purposes

a <- journalSJR %>% select(Categories, ISSN, Title)
b <- topJournals %>%
  select(-1)
b <- data.frame(c = unlist(b, use.names = FALSE))
colnames(b) <- "ISSN"

masterList <- b %>%
  left_join(a) %>%
  na.omit()
topJournals <- pivot_longer(topJournals, 2:21)
colnames(topJournals)[3] <- "ISSN"
masterList <- left_join(masterList, topJournals)
# Join dataframes together

masterList <- separate(data = masterList, col = ISSN, into = c("ISSN", "ISSN2"), sep = ",")
masterList <- pivot_longer(masterList, c("ISSN", "ISSN2"), names_repair = "unique")

issn <- masterList %>% pull(6)
issn <- gsub("[[:space:]]", "", issn)
issn <- gsub("^(.{4})(.*)$", "\\1-\\2", issn)
masterList[6] <- issn


masterList[5] <- NULL
colnames(masterList)[5] <- "ISSN"

return(masterList)
}