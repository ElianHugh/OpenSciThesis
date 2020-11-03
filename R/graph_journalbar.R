# Plot journal open science policies by
# the number of journals that implement the practice.
# Grouped by discipline.
graph_journalbar <- function(graphdf) {

  pal   <- c(
    "#332288", "#AA4499", "#DDCC77", "#999933", "#44AA99", "#117733", "#CC6677",
    "#88CCEE"
  )

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
    theme_apa() +
    theme(
      legend.position = "none", 
      text = element_text(size=20), 
      plot.title=element_text(
        size = rel(1), 
        face = "italic"),
      plot.caption = element_text(hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    ) +
    scale_fill_manual(values = pal) +
    ylab("Percentage of journals that implement policy")

  ggdraw(graph) +
    draw_label("Open Access", x = 0.05, y = 0.75, angle = 90)
}