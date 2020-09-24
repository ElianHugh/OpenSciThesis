graph_journalbar <- function(df) {
  graphdf <- df %>%
    dplyr::filter(GroupedDisc != "Other") %>%
    pivot_longer(DataCitation:Badges, names_to = "Open Science Practice", values_to = "value") %>%
    ungroup() %>%
    group_by(`Open Science Practice`, GroupedDisc) %>%
    dplyr::filter(!is.na(value)) %>%
    summarise(
      p.est = sum(value / length(value)),
      variance = (p.est * (1 - p.est)) / length(value),
      std.dev = sqrt(variance),
      discN = paste0(GroupedDisc, ", n = ", n)
    ) %>%
    distinct()


  graph <- graphdf %>%
    ggplot(aes(
      x = `Open Science Practice`,
      y = p.est, fill = discN
    )) +
    geom_col() +
    facet_grid(. ~ discN, labeller = labeller(discN = label_wrap_gen(10))) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 1), labels = c(",", "100%")) +
    geom_errorbar(width = .5, aes(y = p.est, ymin = p.est - std.dev, ymax = p.est + std.dev)) +
    ggtitle("Percentage of Open Science Practice Implementation for Top 10% Ranked Journals")

  return(graph)
}