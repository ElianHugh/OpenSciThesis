graph_citeridge <- function(df) {

  # TODO either include open access in the calculated score, or explain why not

  df %<>%
    dplyr::filter(Top10Perc == TRUE)

  totalN <- df %>%
    ungroup() %>%
    distinct(Title, .keep_all = TRUE) %>%
    count()
  totalN <- sum(totalN$n)

  pal <- wes_palette("Darjeeling1")

  citeGraph <- df %>%
    distinct(Title, .keep_all = TRUE) %>%
    mutate(ScoreGrade = factor(ScoreGrade,
      levels = c(
        "Very High",
        "High",
        "Medium",
        "Low",
        "None"
      )
    )) %>%
    ggplot(aes(
      y = ScoreGrade,
      x = CiteScore,
      fill = factor(stat(quantile))
    )) +
    stat_density_ridges(
      jittered_points = TRUE,
      position = "raincloud",
      alpha = 0.5,
      scale = 1.5,
      geom = "density_ridges_gradient",
      calc_ecdf = TRUE,
      quantiles = 4,
      quantile_lines = TRUE
    ) +
    ggtitle(paste0("N = ", totalN)) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    theme_ridges(grid = FALSE, center_axis_labels = TRUE) +
    theme_apa() +
    scale_fill_manual(values = pal, name = "Quartiles") +
    theme(axis.ticks = element_blank())

  citeGraph
}