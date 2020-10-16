describe_similarity <- function(boot.out) {
    
        boot.SE <- sd(boot.out$Mean)
        boot.SD <- mean(boot.out$SD)
        boot.M <- mean(boot.out$Mean)
        boot.m.CI <- bca(boot.out$Mean)
        boot.sd.CI <- bca(boot.out$SD)

        boot.table <- tibble(
            Value = round(boot.SD, digits = 2),
            Low = round(boot.sd.CI[1], digits = 2),
            High = round(boot.sd.CI[2], digits = 2),
            Statistic = "SD",
            Sample = "Bootstrap"
        ) %>%
            add_case(
                Value = round(boot.M, digits = 2),
                Low = round(boot.m.CI[1], digits = 2),
                High = round(boot.m.CI[2], digits = 2),
                Statistic = "Mean",
                Sample = "Bootstrap"
            )

        return(boot.table)
}