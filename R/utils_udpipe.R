load_udpipe_model <- function() {
  model_path <- system.file("extdata", "portuguese-bosque-ud-2.5-191206.udpipe", package = "mtexto")
  if (model_path == "") {
    stop("Modelo UDPipe não encontrado. Coloque o arquivo .udpipe em inst/extdata/")
  }
  udpipe::udpipe_load_model(model_path)
}


lemmatize_udpipe <- function(text, model) {
  x <- udpipe::udpipe_annotate(model, x = text)
  x <- as.data.frame(x)
  # Aqui pegamos o lema, se existir, senão usa a palavra original
  lemmatized <- ifelse(is.na(x$lemma), x$token, x$lemma)
  # Recompondo o texto lematizado como string
  paste(lemmatized, collapse = " ")
}
