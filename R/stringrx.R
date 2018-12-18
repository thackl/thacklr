#' Keep strings uniquely matching a pattern, or find positions.
#'
#' These are wrappers for `stringr::str_subset` and `stringr::str_which` that
#' facilitate all-vs-all matching of multiple strings against multiple patterns,
#' and require matches to be unique across patterns.
#'
#' @param strings a vector of characters
#' @param patterns a vector of patterns
#' @param require_match for each pattern or throw an error
#' @export
str_which_unique <- function(strings, patterns, require_match=FALSE){
  w <- map(patterns, function(p){
    m <- str_which(strings, p)
    if(require_match && length(m)<1)
      stop("No matches for:", p)
    m
  }) %>% simplify

  if(any(duplicated(w))){
    stop("Duplicated matches")
    TODO("which patterns create which duplicated matches")
  }
  w
}
#' @rdname str_which_unique
#' @export
str_subset_unique <- function(strings, patterns, require_match=FALSE){
  strings[str_which_unique(strings, patterns, require_match)]
}
