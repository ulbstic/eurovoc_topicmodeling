require(SnowballC) # stemmer
require(parallel)  # parallel processing
require(tau)       # tokenise function

stem_text<- function(text, language = 'porter', mc.cores = 1) {
  # stem each word in a block of text
  stem_string <- function(str, language) {
    str <- tokenize(x = str)
    str <- wordStem(str, language = language)
    str <- paste(str, collapse = "")
    return(str)
  }
  
  # stem each text block in turn
  x <- mclapply(X = text, FUN = stem_string, language, mc.cores = mc.cores)
  
  # return stemed text blocks
  return(unlist(x))
}
