
ui <- fluidPage(
  titlePanel("Apicativo Shiny para Mineração de Texto"),

  tabsetPanel(
    tabPanel("Nuvem de Palavras",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Carregar arquivo (.txt)", accept = c(".txt")),
                 checkboxGroupInput("preprocess_options", "Opções de Pré-Processamento:",
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
                                    selected = c("lowercase", "punctuation", "stopwords")),
                 sliderInput("num_words", "Número de Palavras na Nuvem:", min = 10, max = 500, value = 100, step = 10),
                 actionButton("process", "Gerar Nuvem de Palavras"),
                 downloadButton("download_wordcloud", "Download da Nuvem de Palavras")
               ),
               mainPanel(
                 plotOutput("wordcloud_plot")
               )
             )
    ),
    tabPanel("Análise de Sentimentos",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file_sentiment", "Carregar arquivo (.txt)", accept = c(".txt")),
                 actionButton("analyze", "Analisar Sentimentos"),
                 radioButtons("select_graph", "Selecionar gráfico para download:",
                              choices = list("Todos os Sentimentos" = "all",
                                             "Positivo e Negativo" = "positive_negative")),
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


server <- function(input, output, session) {

  ### Função para Nuvem de Palavras
  observeEvent(input$process, {
    req(input$file)
    texto <- reactive({
      readLines(input$file$datapath, encoding = "UTF-8")
    })

    texto_processado <- reactive({
      corpus <- Corpus(VectorSource(texto()))

      if ("lemmatization" %in% input$preprocess_options) {
        lemmatized_text <- lemmatize_strings(texto())
        corpus <- Corpus(VectorSource(lemmatized_text))
      }

      if ("stemming" %in% input$preprocess_options) {
        stemmed_text <- sapply(texto(), function(x) paste(wordStem(unlist(strsplit(x, " ")), language = "portuguese"), collapse = " "))
        corpus <- Corpus(VectorSource(stemmed_text))
      }

      if ("lowercase" %in% input$preprocess_options) {
        corpus <- tm_map(corpus, content_transformer(tolower))
      }
      if ("punctuation" %in% input$preprocess_options) {
        corpus <- tm_map(corpus, removePunctuation)
      }
      if ("numbers" %in% input$preprocess_options) {
        corpus <- tm_map(corpus, removeNumbers)
      }
      if ("stopwords" %in% input$preprocess_options) {
        corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))
      }
      if ("whitespace" %in% input$preprocess_options) {
        corpus <- tm_map(corpus, stripWhitespace)
      }
      if ("accents" %in% input$preprocess_options) {
        corpus <- tm_map(corpus, content_transformer(function(x) stri_trans_general(x, "Latin-ASCII")))
      }

      dtm <- TermDocumentMatrix(corpus)
      matrix <- as.matrix(dtm)
      freq <- sort(rowSums(matrix), decreasing = TRUE)
      data.frame(word = names(freq), freq = freq)
    })


    output$wordcloud_plot <- renderPlot({
      df <- head(texto_processado(), input$num_words)
      wordcloud(
        words = df$word, freq = df$freq,
        min.freq = min(df$freq),
        max.words = input$num_words,
        colors = brewer.pal(8, "Dark2"),
        scale = c(5, 0.5),
        random.order = FALSE,
        rot.per = 0.1,
        random.color = FALSE
      )
    })

    output$download_wordcloud <- downloadHandler(
      filename = function() { "nuvem_de_palavras.png" },
      content = function(file) {
        png(file, width = 800, height = 600)
        df <- head(texto_processado(), input$num_words)
        wordcloud(
          words = df$word, freq = df$freq,
          min.freq = min(df$freq),
          max.words = input$num_words,
          colors = brewer.pal(8, "Dark2"),
          scale = c(5, 0.5),
          random.order = FALSE,
          rot.per = 0.1,
          random.color = FALSE
        )
        dev.off()
      }
    )
  })

  ### Função para Análise de Sentimentos
  observeEvent(input$analyze, {
    req(input$file_sentiment)
    texto_sentiment <- reactive({
      readLines(input$file_sentiment$datapath, encoding = "UTF-8")
    })

    sentiment_analysis <- reactive({
      texto <- paste(texto_sentiment(), collapse = " ")
      sent_scores <- get_nrc_sentiment(texto, language = "portuguese")
      sent_summary <- colSums(sent_scores)
      data.frame(Sentiment = names(sent_summary), Score = as.numeric(sent_summary))
    })

    # Traduzindo os sentimentos
    traduzir_sentimentos <- function(sentiment) {
      translation <- c(
        "anger" = "Raiva",
        "anticipation" = "Antecipação",
        "disgust" = "Nojo",
        "fear" = "Medo",
        "joy" = "Alegria",
        "sadness" = "Tristeza",
        "surprise" = "Surpresa",
        "trust" = "Confiança",
        "positive" = "Positivo",
        "negative" = "Negativo"
      )
      return(translation[sentiment])
    }

    output$sentiment_plot <- renderPlot({
      req(sentiment_analysis())
      analysis <- sentiment_analysis()
      analysis$Sentiment <- sapply(analysis$Sentiment, traduzir_sentimentos)
      ggplot(analysis, aes(x = Sentiment, y = Score, fill = Sentiment)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = "Análise de Sentimentos", x = "Sentimentos", y = "Pontuação")
    })

    output$positive_negative_plot <- renderPlot({
      req(sentiment_analysis())
      analysis <- sentiment_analysis()
      positive_negative <- analysis[analysis$Sentiment %in% c("positive", "negative"), ]
      positive_negative$Sentiment <- sapply(positive_negative$Sentiment, traduzir_sentimentos)
      ggplot(positive_negative, aes(x = Sentiment, y = Score, fill = Sentiment)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = "Análise Positivo/Negativo", x = "Sentimentos", y = "Pontuação")
    })

    output$download_sentiment <- downloadHandler(
      filename = function() {
        if (input$select_graph == "all") {
          "analise_sentimentos_todos.png"
        } else {
          "analise_positivo_negativo.png"
        }
      },
      content = function(file) {
        png(file)
        if (input$select_graph == "all") {
          analysis <- sentiment_analysis()
          analysis$Sentiment <- sapply(analysis$Sentiment, traduzir_sentimentos)
          print(
            ggplot(analysis, aes(x = Sentiment, y = Score, fill = Sentiment)) +
              geom_bar(stat = "identity") +
              theme_minimal() +
              labs(title = "Análise de Sentimentos", x = "Sentimentos", y = "Pontuação") +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))  # ← rotação dos rótulos
          )
        } else {
          analysis <- sentiment_analysis()
          positive_negative <- analysis[analysis$Sentiment %in% c("positive", "negative"), ]
          positive_negative$Sentiment <- sapply(positive_negative$Sentiment, traduzir_sentimentos)
          print(
            ggplot(positive_negative, aes(x = Sentiment, y = Score, fill = Sentiment)) +
              geom_bar(stat = "identity") +
              theme_minimal() +
              labs(title = "Análise Positivo/Negativo", x = "Sentimentos", y = "Pontuação")
          )
        }
        dev.off()
      }
    )
  })
}

