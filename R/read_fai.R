#' Read fasta index
#'
#' Read seq_ids and lengths from a samtools/seqkit faidx file or from a fasta
#' directly (requires seqkit in path). Seqkit is preferred because it allows
#' reading the full header including seq description.
#'
#' Note, only .fai connections can be pipes, .fa have to be plain text files.
#'
#' @inheritParams readr::read_tsv
#' @importFrom readr read_tsv
#' @param file .fai/.seqkit.fai (plain/pipe), .fa (plain)
#' @param col_names custom column names
#' @param keep_index do not ignore index columns with length offsets etc.
#' @export
#' @return tibble
#' @examples
#' read_fai("foo.fa.fai")  # samtools style index, no description
#' read_fai("foo.fa.seqkit.fai") # seqkit style with description
#' # custom col_names and keep index info
#' read_fai("foo.fa.fai", keep_index=T, col_names=c("contig_id", "contig_desc", "len"))
read_fai <- function (file, col_names=c("seq_id", "seq_desc", "length"), keep_index = FALSE){
  peek <- readChar(file, 1)
  if(peek == ">"){
    rlang::inform("Detected fasta file, running `seqkit faidx -f` to get index")
    system(paste("seqkit faidx -f", file))
    file <- paste0(file,".seqkit.fai")
  }
  col_types <- if(keep_index) "cnnnn" else "cn---";
  df <- read_tsv(file, col_types=col_types, col_names=F) %>%
    separate(1, into=c("seq_id", "seq_desc"), fill="right", sep=" ", extra="merge")
  if(length(col_names) > ncol(df)){
    rlang::warn("Too many col_names, ignoring extra ones")
    col_names <- col_names[seq_along(df)]
  }
  colnames(df)[seq_along(col_names)] <- col_names
  df
}
