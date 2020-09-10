##' .. content for \description{} (no empty lines) ..
##' .. content for \details{} ..
##'
##' @title
##' @param
##' @return
##' @author
##' @export

graph_careers <- function(df) {
  loadd(statsCareer)
  loadd(barrierAnalysis)
  loadd(openSci)

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
    mutate(Perc = if_else(CareerLevel == "Academic", -Perc, Perc))
  tempDf <- na.omit(df.Plotting) %>%
    dplyr::filter(CareerLevel == "HDR Student") %>%
    arrange(Perc)
  order <- tempDf$Barrier

  # Graph barriers by career level (grouped)
  # IMPORTANT to omit NAs, as otherwise will inflate responses
  total_n <- sum(statsCareer$n)
  title <- sprintf("Perceived Barriers to Open Science by Career Level\n (n = %d)", total_n)

  graphCBar <- ggplot(na.omit(df.Plotting), aes(y = Perc, x = Barrier, label = Barrier, fill = CareerLevel, colour = CareerLevel)) +
    ggtitle(title) +
    geom_segment(aes(x = Barrier, y = 0, xend = Barrier, yend = Perc), color = "grey50", size = 0.75) +
    geom_point(size = 3) +
    coord_flip() +
    scale_x_discrete(limits = order) +
    ylim(-100, 100)
  graphCBar
  return(graphCBar)
}