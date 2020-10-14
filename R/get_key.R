# Import the API key for Sherpa
get_key <- function() {
    key <- read_file(file_in("data/api-key.txt"))
    return(key)
}
