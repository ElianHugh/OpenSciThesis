# Returns a dataframe that calculates the changes in
# the number of cases. Used in the flow graph.
describe_flow <- function(topFactor, combinedCite, citeScore, combinedPolicies) {

    topfacN <- topFactor %>%
        select(Title)    %>%
        distinct()       %>%
        tally()          %>%
        as.numeric()

    citeN <- citeScore %>%
        select(Title)  %>%
        distinct()     %>%
        tally()        %>%
        as.numeric()

    combinedN <- combinedCite %>%
        ungroup()             %>%
        select(Title)         %>%
        distinct()            %>%
        tally()               %>%
        as.numeric()

    differenceN <- topfacN - combinedN

    filteredN <- combinedCite            %>%
        ungroup()                        %>%
        dplyr::filter(Top10Perc == TRUE) %>%
        select(Title)                    %>%
        distinct()                       %>%
        tally()                          %>%
        as.numeric()

    finalN <- combinedPolicies %>%
        ungroup() %>%
        dplyr::filter(Top10Perc == TRUE) %>%
        select(Title) %>%
        distinct() %>%
        tally() %>%
        as.numeric()

    sherpadiffN <- filteredN - finalN

    flowDf <- as_tibble(as.list(c(
        "topfacN"     = topfacN,
        "citeN"       = citeN, 
        "combinedN"   = combinedN, 
        "differenceN" = differenceN, 
        "filteredN"   = filteredN, 
        "finalN"      = finalN, 
        "sherpadiffN" = sherpadiffN
    )))
}