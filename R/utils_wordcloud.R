#' @importFrom SnowballC wordStem
#' @import tm
#' @importFrom wordcloud wordcloud
#' @importFrom RColorBrewer brewer.pal
#' @export
processar_wordcloud <- function(texto, preprocess, model, n_words) {
  corpus <- Corpus(VectorSource(texto))

  if ("lemmatization" %in% preprocess) {
    texto <- lemmatize_udpipe(paste(texto, collapse = " "), model)
    corpus <- Corpus(VectorSource(texto))
  }

  if ("stemming" %in% preprocess) {
    texto <- sapply(texto, function(x) {
      paste(wordStem(unlist(strsplit(x, " ")), "portuguese"), collapse = " ")
    })
    corpus <- Corpus(VectorSource(texto))
  }

  if ("lowercase" %in% preprocess)
    corpus <- tm_map(corpus, content_transformer(tolower))
  if ("punctuation" %in% preprocess)
    corpus <- tm_map(corpus, removePunctuation)
  if ("numbers" %in% preprocess)
    corpus <- tm_map(corpus, removeNumbers)
  if ("stopwords" %in% preprocess)
    corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))
  if ("whitespace" %in% preprocess)
    corpus <- tm_map(corpus, stripWhitespace)
  if ("accents" %in% preprocess)
    corpus <- tm_map(corpus, content_transformer(
      function(x) stringi::stri_trans_general(x, "Latin-ASCII")
    ))

  dtm <- TermDocumentMatrix(corpus)
  freq <- sort(rowSums(as.matrix(dtm)), decreasing = TRUE)

  data.frame(word = names(freq), freq = as.numeric(freq))
}

#' @export
plot_wordcloud_png <- function(file, df, n_words,
                               w = 800, h = 600, res = 120) {
  png(file, width = w, height = h, res = res)
  par(mar = c(0,0,0,0))
  wordcloud(df$word, df$freq,
            max.words = n_words,
            colors = brewer.pal(8, "Dark2"))
  dev.off()
}
