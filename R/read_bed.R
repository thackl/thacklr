#' Read a .bed file
#'
#' @inheritParams readr::read_tsv
#' @importFrom readr read_tsv
#' @export
#' @return tibble
read_bed <- function (file, col_names = c("seq_id", "start", "end", "name", "score", "strand")){
  bed <- read_tsv(file, comment = "#", col_names=FALSE)
  i <- seq_len(min(ncol(bed), length(col_names)))
  names(bed)[i] <- col_names[i]
  print(glimpse(bed))
  bed
}
