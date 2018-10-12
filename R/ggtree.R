#' Continious y-scale with fixed expansion space
#'
#' Overwrites the default expand for continuous scales
#'
#' @inheritParams ggplot2::scale_y_continuous
#' @export
scale_y_tree <- function(expand=expand_scale(0, 0.6), ...){
    scale_y_continuous(expand=expand, ...)
}

#' Get tree-based y-coordinates
#'
#' Matches a ggtree and a tibble by shared labels, and returns the respective
#' y-coordinates for the data.
#'
#' @param ggtree a ggtree plot object
#' @param data a tibble with a `label` column
#' @export
tree_y <-  function(ggtree, data){
  if(!inherits(ggtree, "ggtree"))
    stop("not a ggtree object")
  left_join(select(data, label), select(ggtree$data, label, y)) %>%
    pull(y)
}

#' Get the range of the ggtree y-axis data
#'
#' @param ggtree a ggtree plot object
#' @export
tree_ylim <- function(ggtree){
  if(!inherits(ggtree, "ggtree"))
    stop("not a ggtree object")
  range(ggtree$data$y)
}

#' Create a ggplot next to a ggtree
#'
#' `ggtreeplot()` wraps `ggplot()`, and initializes a new plot in the same
#' way. It takes a `ggtree()` object as additional argument. It aligns the plot
#' data by a shared label column to the tips in the tree and fixes inconsistent
#' limits and plot expansion space.
#'
#' @param ggtree a ggtree plot object
#' @param flip if the final plot will be flipped
#' @param expand_limits Vector of range expansion constants used to add some
#' padding around the data, to ensure that they are placed some distance away
#' from the axes. Use the convenience function `expand_scale()` to generate the
#' values for the `expand` argument. The defaults are to expand the scale by 0.6
#' units on each side for discrete variables. In contrast to the `expand`
#' argument of the the scales functions, this expansion space is directly added
#' to the limits, thus all data within the limit+expansions space is plotted.
#' @inheritParams ggplot2::ggplot
#'
#' @source https://thackl.github.io/ggtree-composite-plots
#' @export
ggtreeplot <- function(ggtree, data = NULL, mapping = aes(), flip=FALSE,
     expand_limits=expand_scale(0,.6), ...){

  if(!inherits(ggtree, "ggtree"))
    stop("not a ggtree object")

  # match the tree limits
  limits <- tree_ylim(ggtree)
  limits[1] <- limits[1] + (limits[1] * expand_limits[1]) - expand_limits[2]
  limits[2] <- limits[2] + (limits[2] * expand_limits[3]) + expand_limits[4]

  if(flip){
    mapping <- modifyList(aes_(x=~x), mapping)
    data <- mutate(data, x=tree_y(ggtree, data))
    gg <- ggplot(data=data, mapping = mapping, ...) +
      scale_x_continuous(limits=limits, expand=c(0,0))
  }else{
    mapping <- modifyList(aes_(y=~y), mapping)
    data <- mutate(data, y=tree_y(ggtree, data))
    gg <- ggplot(data=data, mapping = mapping, ...) +
      scale_y_continuous(limits=limits, expand=c(0,0))
  }
  gg
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
