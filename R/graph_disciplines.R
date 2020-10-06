# Plot the relative proportion of responses,
# grouped by discipline, for each item.
# Then, plot the distribution of responses,
# grouped by discipline.
graph_disciplines <- function(df, statsDiscipline, meta) {
  total_n <- statsDiscipline %>%
    dplyr::filter(Discipline != "Other") %>%
    select(n) %>%
    sum()

  title <- sprintf(
    "Perceived Institutional Barriers to Open Science by Discipline (n = %d)",
    total_n
  )

  pal <- wes_palette("Darjeeling1")
  pal2 <- wes_palette("Darjeeling2")

  x <- df %>%
    dplyr::filter(!is.na(Barrier)) %>%
    group_by(Discipline, Barrier) %>%
    tally()

  z <- df %>%
    dplyr::filter(!is.na(Discipline)) %>%
    group_by(Discipline) %>%
    distinct(ParticipantNumber) %>%
    tally(name = "TotalN")

  c <- df %>%
    dplyr::filter(!is.na(Barrier)) %>%
    group_by(Barrier) %>%
    tally(name = "BarN")

  v <- df %>%
    dplyr::filter(Discipline != "Other") %>%
    group_by(Discipline) %>%
    tally(name = "DiscN")

  y <- full_join(x, z) %>%
    mutate(Prop = (n / TotalN) * 100) %>%
    dplyr::filter(Discipline != "Other")

  graphDf <- full_join(y, c) %>%
    full_join(v)

  graphDiscBarr <-
    ggplot(
      graphDf,
      aes(
        x = fct_reorder(Barrier, BarN, .desc = TRUE),
        y = Prop,
        fill = fct_reorder(Discipline, DiscN, .desc = TRUE)
      )
    ) +
    geom_col(position = "fill") +
    coord_flip() +
    xlab("Barriers") +
    ylab("Proportionate Responses") +
    theme_apa(base_size = 11) +
    theme(text = element_text(size = 20)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(name = "Discipline", values = c(pal, pal2))

  freqDf <- df %>%
    full_join(c) %>%
    full_join(v)

  x <- freqDf %>%
    dplyr::filter(!is.na(Barrier)) %>%
    ggplot(aes(x = fct_reorder(Barrier, BarN, .desc = TRUE), fill = fct_reorder(Discipline, DiscN, .desc = TRUE))) +
    geom_bar() +
    coord_flip() +
    ylab("Frequency") +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_discrete(expand = c(0, 0)) +
    theme_apa(base_size = 11) +
    theme(
      text = element_text(size = 20),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    scale_fill_manual(name = "Discipline", values = c(pal, pal2))

  figure <- ggarrange(graphDiscBarr, x, common.legend = TRUE, legend = "bottom", align = "h")
  annotate_figure(figure, top = text_grob(title, face = "bold", size = 20))
}