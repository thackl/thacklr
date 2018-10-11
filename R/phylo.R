#' Most Recent Common Ancestor
#'
#' Get the most recent common ancester for a set of tips. Wraps `ape::getMRCA`
#' and also works if only a subset of tips is actually present in the tree
#'
#' @param tree phylo object
#' @param tips vector of tips (or nodes)
#' @examples
#' # "t6" is not present in tree
#' get_mrca(ape::rtree(5), tips=c("t4","t5","t6"))
get_mrca <- function(tree, tips){
  tips_tree <- tips[tips %in% tree$tip.label] # MRCA needs tips present
  if(length(tips_tree) == 0)
    stop("none of the tips found in tree")
  ape::getMRCA(tree, tips_tree)
}
