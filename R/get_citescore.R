# Import the Scopus dataset from its file
get_citescore <- function() {
  citeScore <- read_csv(file_in("data/CiteScore.csv"))
    return(citeScore)
}
