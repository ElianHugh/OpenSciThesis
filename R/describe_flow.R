describe_flow <- function(topFactor, combinedCite, citeScore) {
topfacN <- length(unique(topFactor$Title))
citeN <- length(unique(citeScore$Title))
combinedN <- length(unique(combinedCite$Title))
differenceN <- topfacN - combinedN
filteredN <- combinedCite %>%
    dplyr::filter(Top10Perc == TRUE)
filteredN <- length(unique(filteredN$Title))
flowDf <- as_tibble(as.list(c('topfacN' = topfacN, 'citeN' = citeN, 'combinedN' = combinedN, 'differenceN' = differenceN, 'filteredN' = filteredN))) 
}