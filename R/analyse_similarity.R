analyse_similarity <- function(aggregatePolicies, citeScore) {
    bootstrap <- function(x) {
        out <- sample(x$CiteScore, size = nrow(var2), replace = T)
        tibble(
            Mean = mean(out),
            SD = sd(out)
        )
    }

    var <- citeScore %>%
        filter(`Top 10% (CiteScore Percentile)` == TRUE) %>%
        distinct(Title, .keep_all = TRUE) %>%
        dplyr::select(CiteScore)

    var2 <- aggregatePolicies %>%
        filter(Top10Perc == TRUE) %>%
        distinct(Title, .keep_all = TRUE) %>%
        select(CiteScore)

    set.seed(1234)
    iterations <- 1000
    boot.out <- tibble(Mean = numeric(), SD = numeric())
    for (i in 1:iterations) {
        df <- bootstrap(var)
        boot.out %<>%
            add_row(df)
    }

    boot.SE <- sd(boot.out$Mean)
    boot.m.CI <- bca(boot.out$Mean)
    boot.sd.CI <- bca(boot.out$SD)

    boot.table <- tibble(
        Low = boot.sd.CI[1],
        High = boot.sd.CI[2],
        Statistic = "SD",
        Sample = "Bootstrap"
    ) %>%
        add_case(
            Low = boot.m.CI[1],
            High = boot.m.CI[2],
            Statistic = "Mean",
            Sample = "Bootstrap"
        )
    return(boot.table)
}