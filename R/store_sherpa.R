store_sherpa <- function(df){
    write.csv(df, file = "data/Sherpa.csv", fileEncoding = "UTF-8")
<<<<<<< HEAD
<<<<<<< HEAD
    
    # if((file.exists('data/Sherpa.csv')) == TRUE){
    #     return(TRUE)
    # } else {
    #     return(FALSE)
    # }
=======
=======
>>>>>>> e38eb6831e738e34df8c6c9efc6fc146e5854ab9
    if((file.exists('data/Sherpa.csv')) == TRUE) {
        return(TRUE)
    } else {
        return(FALSE)
    }
>>>>>>> e38eb6831e738e34df8c6c9efc6fc146e5854ab9
}