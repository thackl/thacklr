#' Remove Missing Tips
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

#' Get Child Nodes
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

#' Flip Descendant Clades
#'
#' Flip two clades descending from `node` in a ggtree. Throw an error for
#' ambigious cases.
#' @param tree_view tree view
#' @param node node
#' @return ggtree object
#' @export
flip_descendants <- function(tree_view=NULL, node){
  tree_view %<>% ggtree:::get_tree_view()
  df <- tree_view$data
  kids <- df$node[df$parent==node]
  if(length(kids) < 2){
    stop("Node doesn't have 2 descendent clades, nothing to flip")
  }
  if(length(kids) >2){
    stop(paste0("Node has more than two descendant clades (", kids,
                "). Explicitly specify the pair you want to flip."))
  }
  ggtree::flip(tree_view=tree_view, node1=kids[1], node2=kids[2])
} 
