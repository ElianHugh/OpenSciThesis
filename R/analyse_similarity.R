analyse_similarity <- function(aggregatePolicies, citeScore) {
    bootstrap <- function(x) {
        out <- sample(x$CiteScore, size = nrow(var2), replace = T)
        tibble(
            Mean = mean(out),
            SD = sd(out)
        )
    }

    var <- citeScore                                     %>%
        filter(`Top 10% (CiteScore Percentile)` == TRUE) %>%
        distinct(Title, .keep_all = TRUE)                %>%
        dplyr::select(CiteScore)

    var2 <- aggregatePolicies             %>%
        filter(Top10Perc == TRUE)         %>%
        distinct(Title, .keep_all = TRUE) %>%
        select(CiteScore)

    set.seed(1234)
    iterations <- 100000

    pb <- new_bar(iterations)
    cores <- detectCores()
    cl <- snow::makeCluster(cores, type = "SOCK")
    registerDoSNOW(cl)
    opts <- list(progress = (function(n) setTxtProgressBar(pb, n)))

    boot.out <- foreach(i = 1:iterations,
    .options.snow         = opts,
    .errorhandling        = "remove",
    .combine              = 'rbind',
    .packages             = "tidyverse") %dopar% {
         x <- bootstrap(var)
         return(x)
    }

    close(pb)

    return(boot.out)
}