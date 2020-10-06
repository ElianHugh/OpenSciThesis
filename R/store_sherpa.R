# Write the fetched Sherpa information to a data file.
# returns false if there is an issue in the workflow
store_sherpa <- function(df){
    write.csv(df, file = "data/Sherpa.csv", fileEncoding = "UTF-8")
    if((file.exists('data/Sherpa.csv')) == TRUE) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}