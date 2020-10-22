graph_citeridge <- function(df) {

  df %<>%
    dplyr::filter(Top10Perc == TRUE)

  # Comparisons for p-values
  out <- pairwise.wilcox.test(x = df$OSS, g = df$ScoreGrade,
    p.adjust.method = "BH", exact = TRUE
  ) %>%
  broom::tidy()


  totalN <- df                        %>%
    ungroup()                         %>%
    distinct(Title, .keep_all = TRUE) %>%
    count()
  totalN <- sum(totalN$n)

  df %<>%
    group_by(ScoreGrade) %>%
    mutate(N = paste0(ScoreGrade, ", n = ", n()))

  pal <- c(
    "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499",
    "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"
  )
  
   title <- sprintf(
     "Distribution of Cite Score with Open Science Implementation \n (n = %d)",
     totalN
   )

  citeGraph <- df %>%
    ggplot(aes(
      y = fct_reorder(paste0(ScoreGrade, " (", ScoreMin, " - ", ScoreMax, "),", "\n n = (", ScoreGradeN, ")"), ScoreMax, .desc = TRUE),
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
    ggtitle(title) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) + 
      theme_ridges(grid = FALSE, center_axis_labels = TRUE) +
      theme_apa() +
      ylab("Open Science Implementation") +
      scale_fill_manual(values = pal) +
      theme(axis.ticks = element_blank(), text = element_text(size = 20), legend.position = "none")

  citeGraph
}