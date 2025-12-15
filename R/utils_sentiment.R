#' @importFrom ggplot2 margin
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual
#' @importFrom ggplot2 scale_y_continuous theme_classic labs theme element_text
#' @export
analisar_sentimentos <- function(texto) {
  texto <- paste(texto, collapse = " ")
  scores <- syuzhet::get_nrc_sentiment(texto)
  colSums(scores)
}

# ===========================
# FUNÇÃO AUXILIAR: tradução
# ===========================

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
  translation[sentiment]
}

# ===========================
# GRÁFICO DE EMOÇÕES (IGUAL AO ORIGINAL)
# ===========================

#' @export
plot_emocoes <- function(sentimentos) {

  analysis <- data.frame(
    Sentiment = names(sentimentos),
    Score = as.numeric(sentimentos)
  )

  emotions_only <- analysis[!analysis$Sentiment %in% c("positive", "negative"), ]
  emotions_only$Sentiment <- sapply(emotions_only$Sentiment, traduzir_sentimentos)

  polaridade <- c(
    "Antecipação" = "negativo",
    "Nojo" = "negativo",
    "Medo" = "negativo",
    "Raiva" = "negativo",
    "Tristeza" = "negativo",
    "Alegria" = "positivo",
    "Confiança" = "positivo",
    "Surpresa" = "positivo"
  )

  ordem <- c(
    "Antecipação", "Nojo", "Medo", "Raiva",
    "Tristeza", "Alegria", "Confiança", "Surpresa"
  )

  cores <- c("positivo" = "blue", "negativo" = "red")

  emotions_only$Polaridade <- polaridade[emotions_only$Sentiment]
  emotions_only$Sentiment <- factor(emotions_only$Sentiment, levels = ordem)

  ggplot(
    emotions_only,
    aes(x = Sentiment, y = Score, fill = Polaridade)
  ) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = cores) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    theme_classic() +
    labs(x = "Emoções", y = "Pontuação") +
    theme(
      axis.text = element_text(size = 14, colour = "black"),
      axis.title = element_text(size = 14, colour = "black"),
      legend.position = "bottom",
      legend.title = element_text(size = 14, margin = margin(r = 8), colour = "black"),
      legend.text = element_text(size = 14, colour = "black")
    )
}

# ===========================
# POSITIVO vs NEGATIVO (IGUAL AO ORIGINAL)
# ===========================

#' @export
plot_posneg <- function(sentimentos) {

  analysis <- data.frame(
    Sentiment = names(sentimentos),
    Score = as.numeric(sentimentos)
  )

  posneg <- analysis[analysis$Sentiment %in% c("positive", "negative"), ]
  posneg$Sentiment <- sapply(posneg$Sentiment, traduzir_sentimentos)

  ggplot(
    posneg,
    aes(x = Sentiment, y = Score, fill = Sentiment)
  ) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("Positivo" = "blue", "Negativo" = "red")) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    theme_classic() +
    labs(x = "Sentimentos", y = "Pontuação") +
    theme(
      axis.text = element_text(size = 14, colour = "black"),
      axis.title = element_text(size = 14, colour = "black"),
      legend.position = "none"
    )
}

# ===========================
# SALVAR GRÁFICO (SEM ALTERAR VISUAL)
# ===========================

#' @export
salvar_grafico_sentimento <- function(file, sentimentos, tipo) {

  png(file, width = 900, height = 600, res = 120)

  if (tipo == "all") {
    print(plot_emocoes(sentimentos))
  } else {
    print(plot_posneg(sentimentos))
  }

  dev.off()
}
