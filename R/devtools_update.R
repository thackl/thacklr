#' Document and reload dev package
#'
#' @param pkg package to update, defaults to `here::here()`
#' @export
pkg_update <- function(pgk=here::here()){
    devtools::document(pkg)
    devtools::load_all(pkg, export_all=FALSE)
}
