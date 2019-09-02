#' Read a .gff file
#'
#' Uses `rtracklayer::import`.
#'
#' @param gff_file to read
#' @param sources only return features from these sources
#' @param types only return features of these types, e.g. gene, CDS, ...
#' @export
#' @return tibble
read_gff <- function(gff_file, sources=NULL, types=NULL){
  if (!requireNamespace("rtracklayer", quietly = TRUE)) {
    stop("Reading .gffs requires package 'rtracklayer' to be installed.",
         call. = FALSE)
  }

  gff <- as_tibble(rtracklayer::import(gff_file)) %>%
    mutate_if(is.factor, as.character)
  if(!is.null(sources)) gff <- filter(gff, `source` %in% sources)
  if(!is.null(types)) gff <- filter(gff, type %in% types)
  rlang::inform("Features read:")
  print(count(gff, `source`, type))
  gff
}
