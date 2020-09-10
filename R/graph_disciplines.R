##' .. content for \description{} (no empty lines) ..
##' .. content for \details{} ..
##'
##' @title
##' @param
##' @return
##' @author
##' @export

graph_disciplines <- function(df) {
  loadd(statsDiscipline)
  loadd(meta)

  total_n <- sum(statsDiscipline$n)
  title <- sprintf("Perceived Barriers to Open Science by Discipline\n (n = %d)", total_n)

  graphDiscBarr <-
    ggplot(
      data = na.omit(df),
      aes(
        x = fct_infreq(Barrier),
        fill = fct_infreq(Discipline)
      )
    ) +
    geom_bar() +
    coord_flip() +
    xlab("Barriers") +
    scale_fill_discrete(name = "Discipline") +
    ggtitle(title) +
    theme_apa(base_size = 11) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 100))
  graphDiscBarr
  return(graphDiscBarr)
}