graph_similarity <- function(aggregatePolicies, boot.out) {
    
    var <- aggregatePolicies                     %>%
        dplyr::filter(Top10Perc == TRUE)         %>%
        dplyr::distinct(Title, .keep_all = TRUE) %>%
        dplyr::select(CiteScore)

    jMean     <- mean(var$CiteScore)
    dens      <- density(boot.out$Mean)
    df        <- data.frame(x = dens$x, y = dens$y)
    probs     <- c(0, 0.25, 0.5, 0.75, 1)
    quantiles <- quantile(boot.out$Mean, prob = probs)
    loc       <- df$y[which(abs(df$x - jMean) == min(abs(df$x - jMean)))]

    df$quant <- factor(findInterval(df$x, quantiles))
    ggplot(df, aes(x, y)) +
        geom_line() +
        geom_segment(size = 1, color = "red", aes(x = jMean, xend = jMean, y =.04, yend = loc)) +
        geom_ribbon(aes(ymin = 0, ymax = y, fill=quant)) + 
        #scale_x_continuous(breaks=quantiles, expand = c(0, 0)) + 
        scale_fill_brewer(guide="none") +
        scale_y_continuous(expand = c(0, 0)) +
        theme_apa(base_size = 11) +
        theme(
            text = element_text(size=20),
            plot.caption = element_text(hjust = 0),
            plot.title.position = "plot",
            plot.caption.position = "plot"
        ) + 
        xlab("Cite Score Means") +
        ylab("Density") +
        annotate("text", x= jMean + 0.9, y = loc + .05, label="Journal sample mean", size = 6) +
        annotate("text", x= 14 + 0.9, y = .4, label = "Iterations = 100,000", size = 6)
}