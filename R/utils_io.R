#' @export
read_text_file <- function(path) {
  ext <- tolower(tools::file_ext(path))

  if (ext == "txt") {
    readLines(path, encoding = "UTF-8", warn = FALSE)
  } else if (ext == "pdf") {
    pdftools::pdf_text(path)
  } else {
    stop("Formato não suportado")
  }
}
