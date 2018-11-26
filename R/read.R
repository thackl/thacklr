#' read a .paf file (minimap/minimap2).
#'
#' Only the first 12 canonical columns. Ignores tagged extra fields.
#'
#' From the minimap2 manual
#' 
#' +----+--------+---------------------------------------------------------+
#' |Col |  Type  |                       Description                       |
#' +----+--------+---------------------------------------------------------+
#' |  1 | string | Query sequence name                                     |
#' |  2 |  int   | Query sequence length                                   |
#' |  3 |  int   | Query start coordinate (0-based)                        |
#' |  4 |  int   | Query end coordinate (0-based)                          |
#' |  5 |  char  | ‘+’ if query/target on the same strand; ‘-’ if opposite |
#' |  6 | string | Target sequence name                                    |
#' |  7 |  int   | Target sequence length                                  |
#' |  8 |  int   | Target start coordinate on the original strand          |
#' |  9 |  int   | Target end coordinate on the original strand            |
#' | 10 |  int   | Number of matching bases in the mapping                 |
#' | 11 |  int   | Number bases, including gaps, in the mapping            |
#' | 12 |  int   | Mapping quality (0-255 with 255 for missing)            |
#' +----+--------+---------------------------------------------------------+
#'
#' +----+------+-------------------------------------------------------+
#' |Tag | Type |                      Description                      |
#' +----+------+-------------------------------------------------------+
#' | tp |  A   | Type of aln: P/primary, S/secondary and I,i/inversion |
#' | cm |  i   | Number of minimizers on the chain                     |
#' | s1 |  i   | Chaining score                                        |
#' | s2 |  i   | Chaining score of the best secondary chain            |
#' | NM |  i   | Total number of mismatches and gaps in the alignment  |
#' | MD |  Z   | To generate the ref sequence in the alignment         |
#' | AS |  i   | DP alignment score                                    |
#' | ms |  i   | DP score of the max scoring segment in the alignment  |
#' | nn |  i   | Number of ambiguous bases in the alignment            |
#' | ts |  A   | Transcript strand (splice mode only)                  |
#' | cg |  Z   | CIGAR string (only in PAF)                            |
#' | cs |  Z   | Difference string                                     |
#' | dv |  f   | Approximate per-base sequence divergence              |
#' +----+------+-------------------------------------------------------+
#'
#' @inheritParams readr::read_tsv
#' @importFrom readr read_tsv
#' @export
#' @return tibble
read_paf <- function(file, ...){
  col_names <- c(
    "query_name", "query_length", "query_start", "query_end",
    "strand",
    "target_name", "target_length", "target_start", "target_end",
    "map_match", "map_length", "map_quality", "tags");

  # dirty hack to compress tags into one space-sep column
  read_tsv(
    pipe(str_glue("bash -c \"paste <(cut -f1-12 {file}) <(cut -f13- {file} | tr '\t' ' ')\"")),
    col_names=col_names, ...)
}

