# Plot the relative proportion of responses,
# grouped by discipline, for each item.
# Then, plot the distribution of responses,
# grouped by discipline.
graph_disciplines <- function(df, meta) {

  participants <- df %>%
    select(ParticipantNumber, Discipline, Barrier)  %>%
    distinct(ParticipantNumber, .keep_all = TRUE)  %>% 
    group_by(Discipline) %>%
    tally()

  total_n <- participants %>%
    dplyr::filter(Discipline != "Other") %>%
    select(n) %>%
    sum()

  title <- sprintf(
    "Perceived Institutional Barriers to Open Science by Discipline (n = %d)",
    total_n
  )

  pal <- c(
    "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499",
    "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"
  )
  pal2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

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