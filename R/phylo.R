#' Remove missing tips
#'
#' Compare a set of tip labels to a tree, and only keep tips actually
#' present. Through and error if no tips are present.
#'
#' @param tips vector of tip labels
#' @param tree phylo object
#' @export
clean_tips <- function(tips, tree){
  tips <- tips[tips %in% tree$tip.label]
  if(length(tips_tree) == 0)
    stop("none of the tips found in tree")
  tips
}

#' Get child nodes
#'
#' Get the immediate descendants for a given node.
#'
#' @param tree phylo object
#' @param node parent node
#' @export
get_children <- function(tree, node){
    # current node is parent
    tree$edge[which(tree$edge[,1]==node),2]
}

#' Prune very long branches
#'
#' Prune tip branches that are `f` times longer than the `q`th quantile of all
#' branch lengths in the tree.
#'
#' @param tree phylo object
#' @param q quantile of all branch lengths
#' @param f multiplicator for threshold
#' @export
prune_long_tip_branches <- function(tree, q=.95, f=5){
  threshold <- quantile(tree$edge.length, q)*f
  tips <- tree$edge[tree$edge.length > threshold, 2]
  tips <- tips[tips <= Ntip(tree)] # only tips
  if(length(tips) > 0){
    write("pruning", stdout())
    tree <- drop.tip(tree, tips)
  }
  tree
}
