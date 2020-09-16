graph_journalbar <- function(df) {


# Top10 Journals
graphdf <- df %>%
  # distinct(Title, .keep_all = TRUE) %>%
  dplyr::filter(GroupedDisc != "Other") %>%
    # dplyr::filter(n > 10) %>%
    mutate(discN = paste0(GroupedDisc, ", n = ", n)) %>%
    pivot_longer(DataCitation:Badges, names_to = "Open Science Practice", values_to = "value") %>%
    ggplot(aes(x=`Open Science Practice`,y=value))  +
    stat_summary(
      fun.min = min,
      fun.max = max,
      fun = mean,
      geom = "bar"
    ) + facet_wrap(~discN) + coord_flip() + ggtitle("Percentage of Open Science Practice Policies for Top 10% Ranked Journals") +
    scale_y_continuous(labels = scales::percent)
return(graphdf)
}