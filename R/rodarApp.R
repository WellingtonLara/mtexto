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
#' @import udpipe

#' @export
rodarMeuApp <- function() {
  shiny::runApp(shiny::shinyApp(ui = ui, server = server))
}

#' Carregar modelo UDPipe
#'
#' @return modelo UDPipe carregado
#' @import udpipe
#' @export
load_udpipe_model <- function() {
  model_path <- system.file("extdata", "portuguese-ud-2.5-191206.udpipe", package = "mtexto")
  if (model_path == "") {
    stop("Modelo UDPipe não encontrado. Coloque o arquivo .udpipe em inst/extdata/")
  }
  udpipe::udpipe_load_model(model_path)
}
