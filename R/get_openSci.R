# Import the Swinburne Open Science Survey data
get_openSci <- function() {
      openSci <- read_csv(file_in("data/OSF_data.csv"),
            col_types = cols(X1 = col_skip())
      )
      return(openSci)
}