#' @export
session_items <- function(x = sessionInfo()){
  if(inherits(x, "sessionInfo"))
    x <- capture.output(x)
  x %>% str_split("\\s+") %>% unlist %>% discard(str_detect, "^\\[")
}

#' Compare sessionInfo()s
#'
#' Compare a sessionInfo string for example from a github issue to the current
#' session and show the differences. Just copy the query session info from the
#' issue post into the clipboard and call `session_info_diff` directly.
#' 
#' @export
session_info_diff <- function(x=readLines("clipboard"), y=sessionInfo()){

  x <- session_items(x)
  y <- session_items(y)
  dx <- setdiff(x,y)
  x <- tibble(x=intersect(x, dx)) %>% mutate(item=str_remove(x, "_[-.\\d]+$"))
  dy <- setdiff(y,x)
  y <- tibble(y=intersect(y, dy)) %>% mutate(item=str_remove(y, "_[-.\\d]+$"))
  left_join(x, y) %>% relocate(item) %>% arrange(item)
}
