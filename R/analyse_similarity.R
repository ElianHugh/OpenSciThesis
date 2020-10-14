analyse_similarity <- function(aggregatePolicies, citeScore) {
    # A is our priors for mean and sd
    A <- citeScore %>%
        filter(`Top 10% (CiteScore Percentile)` == TRUE) %>%
        dplyr::select(CiteScore)
    priors <- list(
        muM = mean(A$CiteScore),
        muSD = sd(A$CiteScore),
        sigmaMode = ,
        sigmaSD =    
    )

    B <- aggregatePolicies %>%
        filter(Top10Perc == TRUE) %>%
        dplyr::select(Title, CiteScore)
    
    # Random sample of A - B with length B
    AnB <- citeScore %>%
        filter(`Top 10% (CiteScore Percentile)` == TRUE) %>%
        distinct(Title, .keep_all = TRUE) %>%
        anti_join(B, by = "Title") %>%
        select(Title, CiteScore) %>%
        sample_n(nrow(B))


    BESTout <- BESTmcmc(
        y1 = B$CiteScore,
        y2 = AnB$CiteScore,
        priors = priors,
        parallel = TRUE
    )
    return(BESTout)
}