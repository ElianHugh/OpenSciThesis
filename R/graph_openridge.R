graph_openridge <- function(df) {
  df %<>%
    dplyr::filter(Top10Perc == TRUE)
  totalN <- df %>%
    distinct(Title, .keep_all = TRUE) %>%
    count()
  totalN <- sum(totalN$n)

  openGraph <- df %>%
    distinct(Title, .keep_all = TRUE) %>%
    pivot_longer(DataCitation:Badges, names_to = "Open Science Practice", values_to = "value") %>%
    ggplot(aes(y = `Open Science Practice`, x = value, fill = factor(stat(quantile)))) +
    ggtitle(paste0("N = ", totalN)) +
    stat_density_ridges(
      jittered_points = TRUE, position = "raincloud",
      alpha = 0.5, scale = 1, geom = "density_ridges_gradient", calc_ecdf = TRUE,
      quantiles = 4, quantile_lines = TRUE
    ) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_viridis_d(name = "Quartiles") +
    theme_ridges(grid = FALSE, center_axis_labels = TRUE)
  return(openGraph)
}

