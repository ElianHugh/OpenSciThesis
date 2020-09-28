##' .. content for \description{} (no empty lines) ..
##' .. content for \details{} ..
##'
##' @title
##' @param
##' @return
##' @author
##' @export

get_metadata <- function() {
  meta <- read_excel(file_in("data/OS_Data_Metadata.xlsx")) %>%
    dplyr::filter(grepl(
      paste("OSBarriersList",
        collapse = "|"
      ),
      OldVariable
    )) %>%
    select(OldVariable, NewVariable, QuestionText)

  return(meta)
}