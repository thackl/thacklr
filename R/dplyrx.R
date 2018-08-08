#' Index by keys
#'
#' Add indices by key columns while preserving order according to the first occurence
#'
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
