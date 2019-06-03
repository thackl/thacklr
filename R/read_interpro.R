#' Read InterProScan .tsv files.
#'
#' Columns are:
#'   1. protein_id - Protein Accession (e.g. P51587)
#'   2. protein_md5 - Sequence MD5 digest (e.g. 14086411a2cdf1c4cba63020e1622579)
#'   3. protein_length - Sequence Length (e.g. 3418)
#'   4. analysis - Analysis (e.g. Pfam / PRINTS / Gene3D)
#'   5. signature_id - Signature Accession (e.g. PF09103 / G3DSA:2.40.50.140)
#'   6. signature_desc - Signature Description (e.g. BRCA2 repeat profile)
#'   7. start - Start location
#'   8. end - Stop location
#'   9. score - is the e-value (or score) of the match reported by member database method (e.g. 3.1E-52)
#'   10. status - is the status of the match (T: true)
#'   11. date - is the date of the run
#'   12. interpro_id - (InterPro annotations - accession (e.g. IPR002093) - optional column; only displayed if -iprlookup option is switched on)
#'   13. interpro_desc - (InterPro annotations - description (e.g. BRCA2 repeat) - optional column; only displayed if -iprlookup option is switched on)
#'   14. go_terms - (GO annotations (e.g. GO:0005515) - optional column; only displayed if --goterms option is switched on)
#'   15. pathway_terms - (Pathways annotations (e.g. REACT_71) - optional column; only displayed if --pathways option is switched on)
#'
#' Because `readr::read_tsv` expects a fixed number of columns, but interpro columsn 12-15 are optional, warnings like 15 cols expected, 11 cols seen should be ignored.
#'
#' @source https://github.com/ebi-pf-team/interproscan/wiki/OutputFormats
#' @inheritParams readr::read_tsv
#' @importFrom readr read_tsv
#' @param max_tags maximum number of optional fields to include
#' @export
#' @return tibble
read_interpro <- function (file){
  col_names <- c("protein_id", "protein_md5", "protein_length", "analysis",
                 "signature_id", "signature_desc", "start", "end", "score",
                 "status", "date", "interpro_id", "interpro_desc", 
                 "go_terms", "pathway_terms")
  col_types <- "cciccciidl?cccc"
  read_tsv(file, col_names = col_names, col_types = col_types)
}
