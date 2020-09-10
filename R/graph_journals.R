#' title
#' @param df
#' @return graph
#' @author Elian
#' @export
graph_journals <- function(df) {
  categoryVec <- separate_rows(df, Categories, sep = "; ", convert = FALSE) %>%
    select(Categories) %>%
    unique()
  
  graphDf <- separate(data = df, col = Categories, sep = "; ", into = c("1", "2", "3", "4", "5", "6", "7"))
  graphDf <- pivot_longer(graphDf, 2:8, values_drop_na = TRUE, values_to = "Categories")
  print(graphDf)
  graphDf <- select(graphDf, -c("Registered reports & publication bias justification", "name", "Design & analysis reporting guidelines justification", "ISSN.x"))

  graphDf <- tidyr::gather(
    data = graphDf,
    key = "Open Science Practice",
    value = "value",
    Submitted:RegRepPubBias
  )

  graphDf %<>%
    mutate(
      value = case_when(
        value == 0 ~ 0,
        value == 1 ~ 1,
        value == 2 ~ 1,
      )
    )

  graphDf <- graphDf[!is.na(graphDf$Discipline), ]
  print(graphDf)

  # Calculate total N per group
  x <- graphDf %>%
    group_by(`Open Science Practice`, Discipline) %>%
    count()
  # Find total N per group where value == 1
  y <- graphDf %>%
    dplyr::filter(value == 1) %>%
    group_by(`Open Science Practice`, Discipline) %>%
    count(name = "GroupN")
  # X - Y = Proportion
  z <- full_join(x, y) %>%
    mutate(proportion = 100 * (GroupN / n))
  #
  total_n <- length(unique(graphDf$Title))
  title <- sprintf("Discipline Open Science Practices \n (n = %d)", total_n)
  
  p <- ggplot(
    data = graphDf,
    aes(
      x = `Open Science Practice`,
      y = value,
      fill = Discipline
    )
  ) +
    stat_summary(
      fun.min = min,
      fun.max = max,
      fun = mean,
      geom = "bar"
    ) +
    ggtitle(title) +
    theme_apa(base_size = 11) +
    scale_fill_discrete(name = "Discipline") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    facet_wrap(~Discipline,
                        scales="free_x")+
    coord_flip() +
    annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf) +
    annotate("segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf)
  return(p)
}


    

