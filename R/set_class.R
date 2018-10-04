#' set class
#'
#' Set class with a parenthetical function.
#'
#' From https://github.com/mkearney/tfse.
#' 
#' @param x Object to assign new class to.
#' @param value Class value to assign to x
#' @return Object x as class value.
#' @export
set_class <- function(x, value) `class<-`(x, value)

#' add class
#'
#' Add class with a parenthetical function.
#'
#' From https://github.com/mkearney/tfse.
#' 
#' @inheritParams set_class
#' @export
#' @rdname set_class
add_class <- function(x, value) {
  set_class(x, unique(c(value, class(x))))
}
