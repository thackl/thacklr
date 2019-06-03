#' Read EggNOG-mapper annotation .tsv files.
#'
#' @inheritParams readr::read_tsv
#' @importFrom readr read_tsv
#' @export
#' @return tibble
read_eggnog <- function (file){
  col_names <- c("query_name", "seed_eggNOG_ortholog", "seed_ortholog_evalue", 
                 "seed_ortholog_score", "predicted_gene_name", "GO_terms", "KEGG_KOs", 
                 "BiGG_reactions", "Annotation_tax_scope", "OGs", "bestOG_evalue_score", 
                 "COG_cat", "eggNOG_annot")
  
  read_tsv(file, col_names=col_names, comment="#")
}
