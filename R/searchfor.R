
#' Search for pattern 
#'
#' @param text corpus
#' @param pattern  search for
#'
#' @return string
#' @export
#' @importFrom utils person
searchfor <- function(text, pattern = NULL) {
  if (is.null(pattern)) pattern <- .searchPatterns()
  finds <- setNames(vector("list", length(pattern)), pattern)
  for (i in seq_along(pattern)) {
    finds[[i]] <-   sort(unique(grep(pattern[i], unlist(strsplit(text, "\\s+")), value = TRUE)))
  }
  finds
}


.searchPatterns <- function() {
  unlist(strsplit(
  "ise
  ize
  ising
  izing
  lyz
  lys
  vour
  vor
  elle
  ele
  eli
  elli
  vior
  viour
  ", "\\s+"))
}