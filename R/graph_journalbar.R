graph_journalbar <- function(df) {
  graphdf <- df %>%
    dplyr::filter(Top10Perc == TRUE) %>%
    # dplyr::filter(GroupedDisc != "Other") %>%
    pivot_longer(
      cols = c(
        DataCitation,
        DataTransparency,
        AnalysisTransparency,
        MaterialsTransparency,
        DesignAnalysis,
        Preregistration,
        AnalysisPreReg,
        Replication,
        RegRepPubBias,
        Badges,
        Submitted,
        Published,
        Accepted
      ),
      names_to = "Open Science Practice",
      values_to = "value"
    ) %>%
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

  graphdf$`Open Science Practice` <- factor(graphdf$`Open Science Practice`,
    levels = c(
      "DataCitation",
      "DataTransparency",
      "AnalysisTransparency",
      "MaterialsTransparency",
      "DesignAnalysis",
      "Preregistration",
      "AnalysisPreReg",
      "Replication",
      "RegRepPubBias",
      "Badges",
      "Submitted",
      "Published",
      "Accepted"
    )
  )

<<<<<<< HEAD
<<<<<<< HEAD
# Top10 Journals
graphdf <- df %>%
  # distinct(Title, .keep_all = TRUE) %>%
  dplyr::filter(GroupedDisc != "Other") %>%
    # dplyr::filter(n > 10) %>%
    mutate(discN = paste0(GroupedDisc, ", n = ", n)) %>%
    pivot_longer(DataCitation:Badges,
    names_to = "Open Science Practice",
    values_to = "value") %>%
    ggplot(aes(x = `Open Science Practice`, y = value)) +
      stat_summary(
        fun.min = min,
        fun.max = max,
        fun = mean,
        geom = "bar"
      ) +
      facet_wrap(~discN) +
    coord_flip() +
    ggtitle("Percentage of Open Science Practice Policies for Top 10% Ranked Journals") +
    scale_y_continuous(labels = scales::percent)
return(graphdf)
=======
=======
>>>>>>> e38eb6831e738e34df8c6c9efc6fc146e5854ab9
  pal <- wes_palette("Darjeeling1")
  pal2 <- wes_palette("Darjeeling2")

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
    theme(legend.position = "none") +
    scale_fill_manual(values = c(pal, pal2)) +
    ylab("Percentage of journals that implement practice")

  ggdraw(graph) +
    draw_label("Open Access", x = 0.05, y = 0.75, angle = 90)
<<<<<<< HEAD
>>>>>>> e38eb6831e738e34df8c6c9efc6fc146e5854ab9
=======
>>>>>>> e38eb6831e738e34df8c6c9efc6fc146e5854ab9
}