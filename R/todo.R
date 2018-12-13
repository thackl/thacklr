#' Add TODO statements. See `enable/distable_TODO` for control of when to print
#' them.
#'
#' @param desc of what to do
#' @export
TODO <- function(desc, file=stderr()){
  msg <- paste0('# TODO: ', desc)
  if(.thacklr$todo)
    write(msg, file=file)
  else
    invisible(msg);
}

#' Enable/Disable TODO() messages
#'
#' `TODO("msg")` are suppressed by default. `enable_todos()` enables them for
#' following code. `deactivate_todos()` disables them again.
#' @export
enable_TODO <- function(){
  .thacklr$todo <- TRUE
}
#' @rdname enable_TODO
#' @export
disable_TODO <- function(){
  .thacklr$todo <- FALSE
}
