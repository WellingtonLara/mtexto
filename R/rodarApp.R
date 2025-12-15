#' Rodar o aplicativo Shiny
#' @export
rodarMeuApp <- function() {
  shiny::runApp(
    shiny::shinyApp(ui = ui, server = server)
  )
}

