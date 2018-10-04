#' @export
#' @importFrom magrittr %>%
magrittr::`%>%`

#' @export
#' @importFrom magrittr %<>%
magrittr::`%<>%`

#' @export
`%||%` <- function(x, y){
  if(is.null(x)) y else x
}

#' @export
`%&&%` <- function(x, y) {
  if (is.null(x)) NULL else y
}

#' @export
if_null_else <- function(cond, true, false){
  if(is.null(cond)) true else false
}
