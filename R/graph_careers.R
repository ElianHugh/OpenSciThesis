# Graphs a comparison between academic
# and HDR student survey responses
graph_careers <- function(statsCareer, barrierAnalysis, openSci) {
  a <- openSci %>%
    select(ParticipantNumber, CareerLevel) %>%
    dplyr::filter(CareerLevel != "Unlisted") %>%
    dplyr::filter(CareerLevel != "prefer not to answer")
  a <- left_join(a, barrierAnalysis)
  a$Barrier <- fct_infreq(a$Barrier)
  a$CareerLevel <- fct_infreq(a$CareerLevel)

  n_HDR <- statsCareer %>%
    dplyr::filter(CareerLevel == "HDR Student") %>%
    select(n) %>%
    as.numeric()

  n_Academic <- statsCareer %>%
    dplyr::filter(CareerLevel == "Academic") %>%
    select(n) %>%
    as.numeric()

  studentPlot <- na.omit(a) %>%
    group_by(CareerLevel, Barrier) %>%
    dplyr::filter(CareerLevel == "HDR Student") %>%
    summarise(Freq = n()) %>%
    mutate(Perc = (Freq / n_HDR) * 100)

  academicPlot <- na.omit(a) %>%
    group_by(CareerLevel, Barrier) %>%
    dplyr::filter(CareerLevel == "Academic") %>%
    summarise(Freq = n()) %>%
    mutate(Perc = (Freq / n_Academic) * 100)

  ## FIX THIS
  df.Plotting <- full_join(studentPlot, academicPlot) %>%
    mutate(
      Perc = if_else(CareerLevel == "Academic", -Perc, Perc)
    )
    
  tempDf <- na.omit(df.Plotting) %>%
    dplyr::filter(CareerLevel == "Academic") %>%
    arrange(desc(Perc))
  order <- tempDf$Barrier

  # Graph barriers by career level (grouped)
  # IMPORTANT to omit NAs, as otherwise will inflate responses
  total_n <- statsCareer %>%
                  dplyr::filter(CareerLevel == "Academic" |
                         CareerLevel == "HDR Student") %>%
                         select(n) %>%
                           sum()
  title <- sprintf(
    "Perceived Barriers to Open Science by Career Level\n (n = %d)",
    total_n
  )

  df.Plotting$Barrier <- fct_reorder(df.Plotting$Barrier,
    df.Plotting$Perc,
    .desc = FALSE
  )

  pal <- wes_palette("Darjeeling1")

  graphCBar <- ggplot(
    (df.Plotting),
    aes(
      y = Perc,
      x = Barrier,
      label = round(Perc),
      fill = CareerLevel,
      colour = CareerLevel
    )
  ) +
    geom_segment(aes(
      x = Barrier,
      y = 0,
      xend = Barrier,
      yend = Perc
    ),
    color = "grey50",
    size = 0.75
    ) +
    geom_point(size = 3) +
    geom_hline(
      yintercept = 0,
      linetype = "dotted"
    ) +
    scale_x_discrete(limits = order) +
    scale_y_continuous(
      limits = c(-100, 100),
      labels = c(100, 50, 0, 50, 100)
    ) +
    coord_flip() +
    ggtitle(title) +
    ylab("Percentage") +
    theme_apa() +
    theme(text = element_text(size=20)) +
    scale_colour_manual(values = pal) 
  graphCBar
}