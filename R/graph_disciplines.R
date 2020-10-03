##' .. content for \description{} (no empty lines) ..
##' .. content for \details{} ..
##'
##' @title
##' @param
##' @return
##' @author
##' @export

graph_disciplines <- function(df, statsDiscipline, meta) {

  total_n <- sum(statsDiscipline$n)
  title <- sprintf(
    "Perceived Institutional Barriers to Open Science by Discipline\n (n = %d)",
    total_n
  )
 pal <- wes_palette("Darjeeling1")
 pal2 <- wes_palette("Darjeeling2")

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
    ggtitle(title) +
    theme_apa(base_size = 11) +
    theme(text = element_text(size = 20)) + 
    scale_fill_manual(name = "Discipline", values = c(pal, pal2)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 100))
  
  graphDiscBarr
}