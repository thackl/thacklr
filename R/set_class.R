#' set class
#'
#' Set class of an object. Optionally append or prepend to exiting class
#' attributes.
#'
#' @param x Object to assign new class to.
#' @param class Class value to assign to x
#' @return Object x as class value.
#' @export
set_class <- function(x, class, add=c("overwrite", "prepend", "append")){
  add = match.arg(add)
  class <- switch(add,
                  overwrite = class,
                  prepend = unique(c(class, class(x))),
                  append = unique(c(class(x), class)))
  `class<-`(x, class)
}
