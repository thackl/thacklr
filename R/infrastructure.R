#' Mount MIT remote drives
#'
#' Mount MIT remote drives (/nobackup1) hosted on engaging using my zsh-alias
#' 
#' @export
#' @examples
#' mount_mit()
mount_mit <- function(){
  system("zsh -c '. ~/.alias; mount-mit'")
}
