% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phylo.R
\name{prune_long_tip_branches}
\alias{prune_long_tip_branches}
\title{Prune very long branches}
\usage{
prune_long_tip_branches(tree, q = 0.95, f = 5)
}
\arguments{
\item{tree}{phylo object}

\item{q}{quantile of all branch lengths}

\item{f}{multiplicator for threshold}
}
\description{
Prune tip branches that are `f` times longer than the `q`th quantile of all
branch lengths in the tree.
}
