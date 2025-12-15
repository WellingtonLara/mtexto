#' Interface do aplicativo Shiny
#'
#' @import shiny
#' @export
ui <- shiny::fluidPage(
  titlePanel("Aplicativo Shiny para Mineração de Texto"),

  tabsetPanel(
    tabPanel(
      "Nuvem de Palavras",
      sidebarLayout(
        sidebarPanel(
          fileInput("file", "Carregar arquivo (.txt ou .pdf)",
                    accept = c(".txt", ".pdf")),

          checkboxGroupInput(
            "preprocess_options", "Opções de Pré-Processamento:",
            choices = list(
              "Converter para minúsculas" = "lowercase",
              "Remover pontuação" = "punctuation",
              "Remover números" = "numbers",
              "Remover stopwords (português)" = "stopwords",
              "Remover espaços extras" = "whitespace",
              "Remover acentos" = "accents",
              "Lematização" = "lemmatization",
              "Stematização" = "stemming"
            ),
            selected = c("lowercase", "punctuation", "stopwords")
          ),

          sliderInput("num_words", "Número de Palavras na Nuvem:",
                      min = 10, max = 500, value = 100, step = 10),

          actionButton("process", "Gerar Nuvem de Palavras"),
          downloadButton("download_wordcloud", "Download da Nuvem")
        ),
        mainPanel(
          imageOutput("wordcloud_img", height = "600px")
        )
      )
    ),

    tabPanel(
      "Análise de Sentimentos",
      sidebarLayout(
        sidebarPanel(
          fileInput("file_sentiment", "Carregar arquivo (.txt ou .pdf)",
                    accept = c(".txt", ".pdf")),
          actionButton("analyze", "Analisar Sentimentos"),

          radioButtons(
            "select_graph", "Selecionar gráfico para download:",
            choices = list(
              "Emoções" = "all",
              "Sentimentos" = "positive_negative"
            )
          ),

          downloadButton("download_sentiment", "Download do Gráfico")
        ),
        mainPanel(
          plotOutput("sentiment_plot"),
          plotOutput("positive_negative_plot")
        )
      )
    )
  )
)
