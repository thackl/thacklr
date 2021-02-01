#' @export
session_items <- function(x = sessionInfo()){
  if(inherits(x, "sessionInfo"))
    x <- capture.output(x)
  x %>% str_split("\\s+") %>% unlist %>% discard(str_detect, "^\\[") %>%
    discard(~.x == "")
}

#' Compare sessionInfo()s
#'
#' Compare a sessionInfo string for example from a github issue to the current
#' session and show the differences. Just copy the query session info from the
#' issue post into the clipboard and call `session_info_diff` directly.
#'
#' @export
session_info_diff <- function(x=readLines("clipboard"), y=sessionInfo()){
  y <- sessionInfo()
  x <- si
  x <- session_items(x)
  y <- session_items(y)
  dx <- setdiff(x,y)
  dy <- setdiff(y,x)
  x <- tibble(x=dx) %>% mutate(item=str_remove(x, "_[-.\\d]+$"))
  y <- tibble(y=dy) %>% mutate(item=str_remove(y, "_[-.\\d]+$"))
  full_join(x, y) %>% relocate(item) %>% arrange(item) %>%
    print(n=50)
}
