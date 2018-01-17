
#' Search for pattern 
#'
#' @param text corpus
#' @param pattern  search for
#'
#' @return string
#' @export
#' @importFrom utils person
searchfor <- function(text, pattern = NULL, drop = TRUE) {
  if (is.null(pattern)) pattern <- .searchPatterns()
  finds <- setNames(vector("list", length(pattern)), pattern)
  for (i in seq_along(pattern)) {
    finds[[i]] <-   sort(unique(grep(pattern[i], unlist(strsplit(text, "\\s+")), value = TRUE)))
  }
  if (drop) {
    finds <- finds[lengths(finds) > 0L]
    if (length(finds) < 1) warning("no candidate patterns found")
   }
  finds
}


## TODO
## https://en.wikipedia.org/wiki/American_and_British_English_spelling_differences
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
  ae
  oe
  center
  centre
  centri
  colour
  color
  bor
  bour
  ", "\\s+"))
}
