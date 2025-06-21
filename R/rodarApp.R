#' Rodar o aplicativo Shiny
#'
#' Esta função inicia o aplicativo Shiny embutido no pacote.
#'
#' @import shiny
#' @import tm
#' @import wordcloud
#' @import RColorBrewer
#' @import syuzhet
#' @import ggplot2
#' @import stringi
#' @import textstem
#' @import SnowballC

#' @export
rodarMeuApp <- function() {
  shiny::runApp(shiny::shinyApp(ui = ui, server = server))
}
