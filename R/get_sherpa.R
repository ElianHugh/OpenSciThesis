# Import Sherpa data from its file
get_sherpa <- function(save_sherpa) {
      sherpa <- read_csv(file_in("data/Sherpa.csv"),
            col_types = cols(X1 = col_skip())
      )
return(sherpa)
}