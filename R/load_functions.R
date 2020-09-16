# https://alistaire.rbind.io/blog/coalescing-joins/
# Cool function that acts like coalesce from SQL (with dplyr join)
coalesce_join <- function(x, y, 
                          by = NULL, suffix = c(".x", ".y"), 
                          join = dplyr::full_join, ...) {
    joined <- join(x, y, by = by, suffix = suffix, ...)
    # names of desired output
    cols <- union(names(x), names(y))
    
    to_coalesce <- names(joined)[!names(joined) %in% cols]
    suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
    # remove suffixes and deduplicate
    to_coalesce <- unique(substr(
        to_coalesce, 
        1, 
        nchar(to_coalesce) - nchar(suffix_used)
    ))
    
    coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
        joined[[paste0(.x, suffix[1])]], 
        joined[[paste0(.x, suffix[2])]]
    ))
    names(coalesced) <- to_coalesce
    
    dplyr::bind_cols(joined, coalesced)[cols]
}


# My little function
# Returns false if both strings have no space
checkmatch <- function(x, y) {
    if ((str_detect(x, "\\s")) && (str_detect(y, "\\s"))) {
        return(TRUE)
    } else {
        if (!(str_detect(x, "\\s")) && !(str_detect(y, "\\s"))) {
            return(FALSE)
        } else {
            return(TRUE)
        }
    }
}


