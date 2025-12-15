' Servidor do aplicativo Shiny
#'
#' @import shiny
#' @export
server <- function(input, output, session) {

  model <- load_udpipe_model()

  # ===========================
  # NUVEM DE PALAVRAS
  # ===========================

  resultado_wc <- eventReactive(input$process, {
    req(input$file)

    texto <- read_text_file(input$file$datapath)

    preprocess <- input$preprocess_options
    if (is.null(preprocess)) preprocess <- character(0)

    processar_wordcloud(
      texto = texto,
      preprocess = preprocess,
      model = model,
      n_words = input$num_words
    )
  })

  output$wordcloud_img <- renderImage({
    req(resultado_wc())

    tmp <- tempfile(fileext = ".png")
    plot_wordcloud_png(tmp, resultado_wc(), input$num_words)

    list(
      src = tmp,
      contentType = "image/png",
      width = 800,
      height = 600
    )
  }, deleteFile = TRUE)

  output$download_wordcloud <- downloadHandler(
    filename = function() "nuvem_de_palavras.png",
    content = function(file) {
      req(resultado_wc())
      plot_wordcloud_png(file, resultado_wc(), input$num_words)
    }
  )

  # ===========================
  # ANÁLISE DE SENTIMENTOS
  # ===========================

  analise_sent <- eventReactive(input$analyze, {
    req(input$file_sentiment)

    texto <- read_text_file(input$file_sentiment$datapath)
    analisar_sentimentos(texto)
  })

  output$sentiment_plot <- renderPlot({
    req(analise_sent())
    plot_emocoes(analise_sent())
  })

  output$positive_negative_plot <- renderPlot({
    req(analise_sent())
    plot_posneg(analise_sent())
  })

  output$download_sentiment <- downloadHandler(
    filename = function() {
      if (input$select_graph == "all") "emocoes.png"
      else "sentimentos.png"
    },
    content = function(file) {
      req(analise_sent())
      salvar_grafico_sentimento(file, analise_sent(), input$select_graph)
    }
  )
}
