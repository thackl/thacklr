#' Read a BLAST .tsv files
#'
#' Read a BLAST output in `-outfmt 6/7` format (o6/o7). If BLAST was run
#' with custom column specifiers, those have to be passed to the call.
#'
#' @inheritParams readr::read_tsv
#' @importFrom readr read_tsv
#' @param col_spec BLAST custom column specifiers
#' @export
#' @return tibble
read_blast <- function (file, col_spec="std"){
  col_std <- 'qaccver saccver pident length mismatch gapopen qstart qend sstart send evalue bitscore'
  if(col_spec == "std") col_spec <- col_std
  col_names <- strsplit(col_std, " ", TRUE)[[1]]
  read_tsv(file, col_names = col_names, comment = "#")
}
