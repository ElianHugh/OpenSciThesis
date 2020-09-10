##' .. content for \description{} (no empty lines) ..
##' .. content for \details{} ..
##'
##' @title
##' @param
##' @return
##' @author
##' @export

get_topFactor <- function() {
    topFactor <- readr::read_csv(file_in("data/top-factor.csv"),
        col_types = cols(
            `Analysis code transparency justification` = col_skip(),
            `Analysis plan preregistration justification` = col_skip(),
            `Author guideline url` = col_skip(),
            `Data citation justification` = col_skip(),
            `Data citation score` = col_skip(),
            `Data transparency justification` = col_skip(),
            `Design and analysis guidelines justification` = col_skip(),
            `Materials transparency justification` = col_skip(),
            `Open science badges justification` = col_skip(),
            `Open science badges score` = col_skip(),
            Publisher = col_skip(),
            `Registered reports justification` = col_skip(),
            `Replication justification` = col_skip(),
            Society = col_skip(),
            `Study preregistration justification` = col_skip(),
            Total = col_skip()
        ),
        trim_ws = TRUE
    )

    colnames(topFactor)[2] <- "ISSN"
    colnames(topFactor)[1] <- "Title"
    issn <- topFactor %>% pull(2)
    issn <- gsub("-", "", issn)
    issn <- gsub("[[:space:]]", "", issn)
    topFactor[2] <- issn

    return(topFactor)
}