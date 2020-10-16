# Plot journal open science policies by
# the number of journals that implement the practice.
# Grouped by discipline.
graph_journalbar <- function(graphdf) {

  pal <- c(
    "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499",
    "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"
  )
  pal2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  graph <- graphdf %>%
    ggplot(aes(
      x = `Open Science Practice`,
      y = p.est, fill = discN
    )) +
    geom_col() +
    facet_grid(. ~ discN, labeller = labeller(discN = label_wrap_gen(10))) +
    coord_flip() +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, .5, 1),
      labels = c(",", "50%", "100%"),
      expand = c(0, 0)
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    geom_errorbar(width = .5, aes(
      y = p.est,
      ymin = p.est - std.dev,
      ymax = p.est + std.dev
    )) +
    ggtitle("Open Science Practice Implementation for TOP Factor Journals") +
    theme_apa() +
    theme(legend.position = "none", text = element_text(size=20)) +
    scale_fill_manual(values = c(pal, pal2)) +
    ylab("Percentage of journals that implement practice")

  ggdraw(graph) +
    draw_label("Open Access", x = 0.05, y = 0.75, angle = 90)
}