#' Create a vector from unquoted words.
#'
#' Similar to perls `qw()`, however, in R spaces between args in function call
#' always cause an error, so `qw(foo bar)` wouldn't work. Workaround is either a
#' single string split at spaces, or unquoted elements, separated by commas.
#'
#' Took inspiration from
#' \href{https://stackoverflow.com/questions/520810/does-r-have-quote-like-operators-like-perls-qw}{stackoverflow/qw}
#' and \href{https://github.com/jebyrnes/multifunc/blob/master/R/qw.R}{github/Jarrett Byrnes}
#' 
#' @param x A single string of elements to be split at whitespace chars.
#' @return A vector of quoted words.
#' @export
#' @examples
#' qw("foo bar") # with a strsplit
#' qc(foo, bar) # or unquoted, but with commas
qw <- function(x) unlist(strsplit(x, "[[:space:]]+"))

#' @rdname qw
#' @param ... Unquated words, separated by comma.
#' @export
qc <- function(...) sapply(match.call()[-1], deparse)
