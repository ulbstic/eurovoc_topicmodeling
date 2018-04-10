#' Get key collision fingerprints
#'
#' Given a character vector as input, get the key collision fingerprint for
#' each element. 
#'
#' Operations in order :
#'
#'-remove leading and trailing whitespace
#'-change all characters to their lowercase representation
#'-remove all punctuation and control characters
#'-split the string into whitespace-separated tokens
#'-sort the tokens and remove duplicates
#'-join the tokens back together
#'-normalize extended western characters to their ASCII representation (for example "gödel" → "godel")
#'
#'For more details on key collision, see
#' \url{https://github.com/OpenRefine/OpenRefine/wiki/Clustering-In-Depth}.
#'
#' @param vect Character vector.
#'
#'
#' @return Key collision value of the input string.
#' @importFrom dplyr "%>%"
#'
#' @examples \dontrun{
#' get_fingerprint("Tom's Sports Equipment, Inc.")
#' [1] "equipment inc sports toms"
#' }

get_fingerprint <- function(vect) {
  stopifnot(is.character(vect))
  
  
  vect %>%
    tolower %>%
    {gsub("[[:punct:]]", "", .)} %>%
    trimws %>%
    {gsub("\\s{2,}", " ", .)} %>%
    strsplit(., " ", fixed = TRUE) %>%
    lapply(., sort) %>%
    lapply(., unique) %>%
    vapply(., function(x) paste(x, collapse = " "), character(1)) %>%
    iconv(., to = "ASCII//TRANSLIT") %>%
    sapply(., function(x) if (x == "" || is.na(x)) {NA} else {x},
           USE.NAMES = FALSE)
  
}