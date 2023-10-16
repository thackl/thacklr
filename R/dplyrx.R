#' Index by keys preserving order
#'
#' Add indices by key columns while preserving order according to the first occurence
#'
#' @import tidyverse
#' @param keys in \code{\link[dplyr]{select}} semantics
#' @export
#' @examples
#' tibble(x=c(1,1,1,2), y=c("B", "A", "B", "B"), z="foo") %>%
#'   index_by(x,y)
index_by <- function(.data, ...){
  keys <- select(.data, ...) %>% apply(1,paste0, collapse="_")

  if(length(keys) == 0) stop("no keys to index by found\n")

  i <- "i"
  while(i %in% names(.data)) i <- paste0(i,"i")
  
  lvls <- unique(keys)
  .data[[i]] <- match(keys, lvls)
  .data
}

#' Split by key preserving order
#'
#' Split by key column while preserving order according to the first
#' occurence. R base split converts keys to factors, changing default order to
#' alphanumeric.
#'
#' @param key variable to split by
#' @export
#' @examples
#' tibble(x=c(1,1,1,2), y=c("B", "A", "B", "B"), z="foo") %>%
#'   split_by(x)
split_by <- function(.data, key){
  keys <- pull(.data, !!enquo(key))

  if(length(keys) == 0) stop("no keys to index by found\n")

  l <- split(.data, keys)
  l[unique(keys)]
}
